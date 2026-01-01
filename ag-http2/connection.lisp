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
   (last-stream-id :initform 0 :accessor connection-last-stream-id))
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

(defun make-server-connection (socket)
  "Create a new HTTP/2 server connection from an accepted socket"
  (let* ((stream (usocket:socket-stream socket))
         (conn (make-instance 'http2-connection
                              :socket socket
                              :stream stream
                              :client-p nil)))
    (setf (connection-multiplexer conn) (make-stream-multiplexer :client-p nil))
    conn))

;;;; ========================================================================
;;;; Connection Handshake
;;;; ========================================================================

(defun connection-handshake (conn)
  "Perform the HTTP/2 connection handshake"
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
  "Send data on a stream"
  (let ((frame (make-data-frame stream-id data :end-stream end-stream)))
    (write-frame frame (connection-stream conn))
    (force-output (connection-stream conn))
    (when end-stream
      (let ((stream (multiplexer-get-stream (connection-multiplexer conn) stream-id)))
        (stream-transition stream :send-end-stream)))))

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
            (decoder (connection-hpack-decoder conn))
            (headers (hpack-decode decoder (frame-payload frame))))
       (stream-transition stream :recv-headers)
       (setf (stream-headers stream) headers)
       (when (plusp (logand (frame-flags frame) +flag-end-stream+))
         (stream-transition stream :recv-end-stream))))
    (data-frame
     (let* ((stream-id (frame-stream-id frame))
            (stream (multiplexer-get-stream (connection-multiplexer conn) stream-id)))
       (stream-append-data stream (frame-payload frame))
       (when (plusp (logand (frame-flags frame) +flag-end-stream+))
         (stream-transition stream :recv-end-stream))))
    (rst-stream-frame
     (let ((stream-id (frame-stream-id frame)))
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
