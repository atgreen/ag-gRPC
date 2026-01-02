;;;; connection.lisp - HTTP/2 connection management

(in-package #:ag-http2)

;;;; ========================================================================
;;;; Conditions
;;;; ========================================================================

(define-condition http2-error (error)
  ((message :initarg :message :reader http2-error-message)
   (error-code :initarg :error-code :initform +error-internal-error+
               :reader http2-error-code))
  (:report (lambda (c s)
             (format s "HTTP/2 error (~A): ~A"
                     (error-code-name (http2-error-code c))
                     (http2-error-message c)))))

(define-condition http2-connection-error (http2-error)
  ()
  (:documentation "Error that affects the entire connection"))

(define-condition http2-stream-error (http2-error)
  ((stream-id :initarg :stream-id :reader http2-stream-error-stream-id))
  (:documentation "Error that affects a single stream"))

(define-condition http2-frame-error (http2-error)
  ()
  (:documentation "Error during frame parsing (incomplete or malformed frame)"))

;;;; ========================================================================
;;;; HTTP/2 Connection
;;;; ========================================================================

(defclass http2-connection ()
  ((socket :initarg :socket :accessor connection-socket
           :documentation "Underlying socket connection")
   (stream :initarg :stream :accessor connection-stream
           :documentation "Binary stream for reading/writing")
   (tls-p :initarg :tls-p :accessor connection-tls-p :initform nil
          :documentation "T if connection is using TLS")
   (client-p :initarg :client-p :accessor connection-client-p
             :documentation "T if this is a client connection")
   (hpack-encoder :initform (make-hpack-encoder) :accessor connection-hpack-encoder)
   (hpack-decoder :initform (make-hpack-decoder) :accessor connection-hpack-decoder)
   (multiplexer :accessor connection-multiplexer
                :documentation "Stream multiplexer")
   (local-settings :initform (copy-alist *default-settings*)
                   :accessor connection-local-settings)
   (remote-settings :initform (copy-alist *default-settings*)
                    :accessor connection-remote-settings)
   (local-window :initform (make-flow-control-window)
                 :accessor connection-local-window)
   (remote-window :initform (make-flow-control-window)
                  :accessor connection-remote-window)
   (state :initform :connecting :accessor connection-state
          :documentation "Connection state: :connecting, :open, :closing, :closed")
   (last-stream-id :initform 0 :accessor connection-last-stream-id)
   (pending-header-block :initform nil :accessor connection-pending-header-block
                         :documentation "Buffered header block fragments awaiting CONTINUATION")
   (pending-header-stream-id :initform nil :accessor connection-pending-header-stream-id
                              :documentation "Stream ID for pending header block")
   (pending-header-end-stream :initform nil :accessor connection-pending-header-end-stream
                               :documentation "END_STREAM flag from initial HEADERS frame"))
  (:documentation "HTTP/2 connection"))

(defun make-client-connection (host port &key tls (verify nil))
  "Create a new HTTP/2 client connection.
   If TLS is true, wrap the connection with TLS encryption.
   VERIFY controls certificate verification (default nil)."
  (let* ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8)))
         (raw-stream (usocket:socket-stream socket))
         (stream (if tls
                     (wrap-stream-with-tls raw-stream host :verify verify)
                     raw-stream))
         (conn (make-instance 'http2-connection
                              :socket socket
                              :stream stream
                              :tls-p tls
                              :client-p t)))
    (setf (connection-multiplexer conn) (make-stream-multiplexer :client-p t))
    (connection-handshake conn)
    conn))

(defun make-server-connection (socket &key tls certificate key password)
  "Create a new HTTP/2 server connection from an accepted socket.
If TLS is true, wrap the stream with TLS using the provided certificate and key."
  (let* ((raw-stream (usocket:socket-stream socket))
         (stream (if tls
                     (wrap-server-stream-with-tls raw-stream certificate key
                                                   :password password)
                     raw-stream))
         (conn (make-instance 'http2-connection
                              :socket socket
                              :stream stream
                              :tls-p tls
                              :client-p nil)))
    (setf (connection-multiplexer conn) (make-stream-multiplexer :client-p nil))
    conn))

;;;; ========================================================================
;;;; Connection Handshake
;;;; ========================================================================

(defun connection-handshake (conn)
  "Perform the HTTP/2 connection handshake (client-side)"
  (let ((stream (connection-stream conn)))
    (when (connection-client-p conn)
      ;; Client sends preface
      (write-sequence +connection-preface+ stream)
      ;; Send initial SETTINGS
      (write-frame (make-settings-frame :settings (connection-local-settings conn))
                   stream)
      (force-output stream)
      ;; Read server's SETTINGS
      (let ((frame (read-frame stream)))
        (when (typep frame 'settings-frame)
          (apply-remote-settings conn (settings-frame-settings frame))
          ;; Send SETTINGS ACK
          (write-frame (make-settings-frame :ack t) stream)
          (force-output stream))))
    (setf (connection-state conn) :open)))

(defun server-connection-handshake (conn)
  "Perform the HTTP/2 connection handshake (server-side).
Reads and validates client preface, exchanges SETTINGS frames."
  (let ((stream (connection-stream conn)))
    ;; Read and validate the client connection preface (24 bytes)
    (let ((preface (make-array 24 :element-type '(unsigned-byte 8))))
      (let ((bytes-read (read-sequence preface stream)))
        (unless (= bytes-read 24)
          (error 'http2-connection-error
                 :message "Incomplete connection preface"
                 :error-code +error-protocol-error+)))
      (unless (equalp preface +connection-preface+)
        (error 'http2-connection-error
               :message "Invalid connection preface"
               :error-code +error-protocol-error+)))
    ;; Read client's initial SETTINGS frame
    (let ((frame (read-frame stream)))
      (unless (typep frame 'settings-frame)
        (error 'http2-connection-error
               :message "Expected SETTINGS frame after preface"
               :error-code +error-protocol-error+))
      (when (settings-frame-ack-p frame)
        (error 'http2-connection-error
               :message "First SETTINGS frame must not be ACK"
               :error-code +error-protocol-error+))
      (apply-remote-settings conn (settings-frame-settings frame)))
    ;; Send our SETTINGS
    (write-frame (make-settings-frame :settings (connection-local-settings conn))
                 stream)
    ;; Send SETTINGS ACK for client's settings
    (write-frame (make-settings-frame :ack t) stream)
    (force-output stream)
    ;; Mark connection as open
    (setf (connection-state conn) :open)))

(defun apply-remote-settings (conn settings)
  "Apply settings received from the remote peer"
  (dolist (setting settings)
    (let ((id (car setting))
          (value (cdr setting)))
      (setf (cdr (assoc id (connection-remote-settings conn))) value)
      ;; Handle specific settings
      (case id
        (#.+settings-initial-window-size+
         ;; Update window sizes for all streams
         (maphash (lambda (id stream)
                    (declare (ignore id))
                    (setf (stream-remote-window stream) value))
                  (multiplexer-streams (connection-multiplexer conn))))
        (#.+settings-header-table-size+
         (setf (dynamic-table-max-size
                (encoder-dynamic-table (connection-hpack-encoder conn)))
               value))))))

;;;; ========================================================================
;;;; Sending Data
;;;; ========================================================================

(defun connection-send-headers (conn stream-id headers &key end-stream)
  "Send headers on a stream"
  (let* ((encoder (connection-hpack-encoder conn))
         (header-block (hpack-encode encoder headers))
         (frame (make-headers-frame stream-id header-block
                                    :end-stream end-stream
                                    :end-headers t)))
    (write-frame frame (connection-stream conn))
    (force-output (connection-stream conn))
    (let ((stream (multiplexer-get-stream (connection-multiplexer conn) stream-id)))
      (stream-transition stream :send-headers)
      (when end-stream
        (stream-transition stream :send-end-stream)))))

(defun connection-send-data (conn stream-id data &key end-stream)
  "Send data on a stream.
Respects MAX_FRAME_SIZE and flow control windows, fragmenting if needed."
  (let* ((max-frame-size (or (cdr (assoc +settings-max-frame-size+
                                          (connection-remote-settings conn)))
                             16384))  ; Default per RFC 7540
         (h2-stream (multiplexer-get-stream (connection-multiplexer conn) stream-id))
         (data-length (length data))
         (offset 0))
    ;; Send data in chunks respecting max frame size and flow control
    (loop while (< offset data-length)
          do (let* ((remaining (- data-length offset))
                    ;; Respect max frame size
                    (chunk-size (min remaining max-frame-size))
                    ;; Respect connection flow control window
                    (chunk-size (min chunk-size (window-size (connection-remote-window conn))))
                    ;; Respect stream flow control window
                    (chunk-size (min chunk-size (stream-remote-window h2-stream))))
               (if (zerop chunk-size)
                   ;; Flow control blocked - wait for WINDOW_UPDATE
                   (connection-read-frame conn)
                   ;; Send a chunk
                   (let* ((chunk (subseq data offset (+ offset chunk-size)))
                          (is-last (and end-stream (= (+ offset chunk-size) data-length)))
                          (frame (make-data-frame stream-id chunk :end-stream is-last)))
                     ;; Consume from flow control windows
                     (window-consume (connection-remote-window conn) chunk-size)
                     (decf (stream-remote-window h2-stream) chunk-size)
                     ;; Send the frame
                     (write-frame frame (connection-stream conn))
                     (force-output (connection-stream conn))
                     (incf offset chunk-size)
                     (when is-last
                       (stream-transition h2-stream :send-end-stream))))))
    ;; Handle case where data was empty but end-stream is requested
    (when (and end-stream (zerop data-length))
      (let ((frame (make-data-frame stream-id data :end-stream t)))
        (write-frame frame (connection-stream conn))
        (force-output (connection-stream conn))
        (stream-transition h2-stream :send-end-stream)))))

(defun connection-send-rst-stream (conn stream-id error-code)
  "Send a RST_STREAM frame to immediately terminate a stream.
ERROR-CODE is an HTTP/2 error code (e.g., +error-cancel+ for client cancellation)."
  (let ((frame (make-rst-stream-frame stream-id error-code)))
    (write-frame frame (connection-stream conn))
    (force-output (connection-stream conn))
    ;; Close the stream locally
    (multiplexer-close-stream (connection-multiplexer conn) stream-id)))

;;;; ========================================================================
;;;; Receiving Frames
;;;; ========================================================================

(defun connection-read-frame (conn)
  "Read and process a frame from the connection"
  (let ((frame (read-frame (connection-stream conn))))
    (when frame
      (process-frame conn frame))
    frame))

(defun process-frame (conn frame)
  "Process a received frame"
  (typecase frame
    (settings-frame
     (unless (plusp (logand (frame-flags frame) +flag-ack+))
       ;; Not an ACK, apply settings and send ACK
       (apply-remote-settings conn (settings-frame-settings frame))
       (write-frame (make-settings-frame :ack t) (connection-stream conn))
       (force-output (connection-stream conn))))
    (ping-frame
     (unless (plusp (logand (frame-flags frame) +flag-ack+))
       ;; Not an ACK, send response
       (write-frame (make-ping-frame (ping-frame-opaque-data frame) :ack t)
                    (connection-stream conn))
       (force-output (connection-stream conn))))
    (goaway-frame
     (setf (connection-state conn) :closing))
    (window-update-frame
     (let ((increment (window-update-frame-window-size-increment frame))
           (stream-id (frame-stream-id frame)))
       (if (zerop stream-id)
           (window-increment (connection-remote-window conn) increment)
           (let ((stream (multiplexer-get-stream (connection-multiplexer conn) stream-id)))
             (incf (stream-remote-window stream) increment)))))
    (headers-frame
     (let* ((stream-id (frame-stream-id frame))
            (stream (multiplexer-get-stream (connection-multiplexer conn) stream-id))
            (end-headers-p (plusp (logand (frame-flags frame) +flag-end-headers+)))
            (end-stream-p (plusp (logand (frame-flags frame) +flag-end-stream+))))
              ;; Reject HEADERS if we're already expecting CONTINUATION
       (when (connection-pending-header-block conn)
         (error 'http2-connection-error
                :message "Received HEADERS while awaiting CONTINUATION"
                :error-code +error-protocol-error+))
       (if end-headers-p
           ;; Complete header block - decode immediately
           (let* ((decoder (connection-hpack-decoder conn))
                  (header-block (extract-header-block frame)))
             (format *error-output* "~&  header-block-len=~A first-10: ~{~2,'0X ~}~%"
                     (length header-block) (coerce (subseq header-block 0 (min 10 (length header-block))) 'list))
             (let ((headers (hpack-decode decoder header-block)))
               (stream-transition stream :recv-headers)
               (if (stream-headers stream)
                   (setf (stream-trailers stream) headers)
                   (setf (stream-headers stream) headers))
               (when end-stream-p
                 (stream-transition stream :recv-end-stream))))
           ;; Incomplete - buffer and wait for CONTINUATION
           (progn
             (setf (connection-pending-header-block conn)
                   (extract-header-block frame))
             (setf (connection-pending-header-stream-id conn) stream-id)
             (setf (connection-pending-header-end-stream conn) end-stream-p)))))
    (continuation-frame
     (let* ((stream-id (frame-stream-id frame))
            (pending-block (connection-pending-header-block conn))
            (end-headers-p (plusp (logand (frame-flags frame) +flag-end-headers+))))
       ;; Verify stream-id matches pending block
       (unless (and pending-block
                    (= stream-id (connection-pending-header-stream-id conn)))
         (error 'http2-connection-error
                :message "Unexpected CONTINUATION frame"
                :error-code +error-protocol-error+))
       ;; Append to pending block
       (let ((new-block (make-array (+ (length pending-block)
                                        (length (frame-payload frame)))
                                     :element-type '(unsigned-byte 8))))
         (replace new-block pending-block)
         (replace new-block (frame-payload frame) :start1 (length pending-block))
         (if end-headers-p
             ;; Complete - decode and clear pending
             (let* ((stream (multiplexer-get-stream (connection-multiplexer conn) stream-id))
                    (decoder (connection-hpack-decoder conn))
                    (headers (hpack-decode decoder new-block))
                    (end-stream-p (connection-pending-header-end-stream conn)))
               (stream-transition stream :recv-headers)
               (if (stream-headers stream)
                   (setf (stream-trailers stream) headers)
                   (setf (stream-headers stream) headers))
               ;; Apply END_STREAM from original HEADERS frame
               (when end-stream-p
                 (stream-transition stream :recv-end-stream))
               ;; Clear pending state
               (setf (connection-pending-header-block conn) nil)
               (setf (connection-pending-header-stream-id conn) nil)
               (setf (connection-pending-header-end-stream conn) nil))
             ;; Still incomplete - update pending block
             (setf (connection-pending-header-block conn) new-block)))))
    (data-frame
     (let* ((stream-id (frame-stream-id frame))
            (stream (multiplexer-get-stream (connection-multiplexer conn) stream-id))
            (data-length (length (frame-payload frame))))
       (stream-append-data stream (frame-payload frame))
       ;; Update local flow control windows and send WINDOW_UPDATE
       (when (plusp data-length)
         ;; Consume from local windows
         (decf (window-size (connection-local-window conn)) data-length)
         (decf (stream-local-window stream) data-length)
         ;; Send WINDOW_UPDATE to replenish connection window
         (write-frame (make-window-update-frame 0 data-length) (connection-stream conn))
         ;; Send WINDOW_UPDATE to replenish stream window
         (write-frame (make-window-update-frame stream-id data-length) (connection-stream conn))
         (force-output (connection-stream conn))
         ;; Restore local windows
         (incf (window-size (connection-local-window conn)) data-length)
         (incf (stream-local-window stream) data-length))
       (when (plusp (logand (frame-flags frame) +flag-end-stream+))
         (stream-transition stream :recv-end-stream))))
    (rst-stream-frame
     (let* ((stream-id (frame-stream-id frame))
            (error-code (rst-stream-frame-error-code frame))
            (stream (multiplexer-get-stream (connection-multiplexer conn) stream-id)))
       ;; Store the error code before closing the stream
       (when stream
         (setf (stream-rst-stream-error stream) error-code))
       (multiplexer-close-stream (connection-multiplexer conn) stream-id)))))

;;;; ========================================================================
;;;; Connection Lifecycle
;;;; ========================================================================

(defun connection-close (conn &key (error-code +error-no-error+) debug-data)
  "Close the HTTP/2 connection gracefully"
  (when (eq (connection-state conn) :open)
    (setf (connection-state conn) :closing)
    (write-frame (make-goaway-frame (connection-last-stream-id conn)
                                    error-code
                                    debug-data)
                 (connection-stream conn))
    (force-output (connection-stream conn)))
  (setf (connection-state conn) :closed)
  (usocket:socket-close (connection-socket conn)))

(defun connection-new-stream (conn)
  "Create a new stream on this connection"
  (let ((stream (multiplexer-new-stream (connection-multiplexer conn))))
    (setf (connection-last-stream-id conn) (stream-id stream))
    stream))
