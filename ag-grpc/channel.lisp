;;;; channel.lisp - gRPC channel (client connection)

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; gRPC Channel
;;;;
;;;; A channel represents a connection to a gRPC server.
;;;; It manages the underlying HTTP/2 connection and provides
;;;; methods for making RPC calls.
;;;; ========================================================================

(defclass grpc-channel ()
  ((host :initarg :host :accessor channel-host
         :documentation "Server hostname")
   (port :initarg :port :accessor channel-port
         :documentation "Server port")
   (tls :initarg :tls :accessor channel-tls
        :initform nil
        :documentation "Use TLS encryption")
   (tls-verify :initarg :tls-verify :accessor channel-tls-verify
               :initform nil
               :documentation "Verify TLS certificates")
   (connection :initarg :connection :accessor channel-connection
               :initform nil
               :documentation "HTTP/2 connection")
   (default-timeout :initarg :default-timeout :accessor channel-default-timeout
                    :initform 30
                    :documentation "Default timeout in seconds")
   (metadata :initarg :metadata :accessor channel-metadata
             :initform nil
             :documentation "Default metadata for all calls"))
  (:documentation "gRPC client channel"))

(defun make-channel (host port &key (connect t) (timeout 30 timeout-supplied-p) metadata tls (tls-verify nil))
  "Create a new gRPC channel to a server.
If CONNECT is true (default), immediately establish the connection.
TIMEOUT - Default timeout in seconds. Pass NIL to disable default timeout.
If TLS is true, use TLS encryption (requires cl+ssl).
If TLS-VERIFY is true, verify server certificates."
  (let ((channel (make-instance 'grpc-channel
                                :host host
                                :port port
                                :tls tls
                                :tls-verify tls-verify
                                :default-timeout (if timeout-supplied-p timeout 30)
                                :metadata metadata)))
    (when connect
      (channel-connect channel))
    channel))

(defun make-secure-channel (host port &key (connect t) timeout metadata (verify nil))
  "Create a new gRPC channel with TLS encryption.
Convenience function equivalent to (make-channel ... :tls t)."
  (make-channel host port
                :connect connect
                :timeout timeout
                :metadata metadata
                :tls t
                :tls-verify verify))

(defun channel-connect (channel)
  "Establish the connection for this channel"
  (unless (channel-connection channel)
    (setf (channel-connection channel)
          (ag-http2:make-client-connection
           (channel-host channel)
           (channel-port channel)
           :tls (channel-tls channel)
           :verify (channel-tls-verify channel)))))

(defun channel-connected-p (channel)
  "Return T if the channel has an active connection"
  (and (channel-connection channel)
       (eq (ag-http2:connection-state (channel-connection channel)) :open)))

(defun channel-close (channel)
  "Close the channel and its connection"
  (when (channel-connection channel)
    (ag-http2:connection-close (channel-connection channel))
    (setf (channel-connection channel) nil)))

;;;; ========================================================================
;;;; Channel Configuration
;;;; ========================================================================

(defun channel-set-default-metadata (channel metadata)
  "Set default metadata to be sent with every call"
  (setf (channel-metadata channel) metadata))

(defun channel-set-default-timeout (channel seconds)
  "Set the default timeout for calls"
  (setf (channel-default-timeout channel) seconds))

;;;; ========================================================================
;;;; Low-Level Stream Operations
;;;; ========================================================================

(defun channel-new-stream (channel)
  "Create a new HTTP/2 stream for an RPC call"
  (ag-http2:connection-new-stream (channel-connection channel)))

(defun channel-send-headers (channel stream-id method &key metadata timeout end-stream)
  "Send request headers for an RPC call"
  (let* ((conn (channel-connection channel))
         (authority (format nil "~A:~A" (channel-host channel) (channel-port channel)))
         (headers (make-request-headers method
                                        :authority authority
                                        :timeout (or timeout (channel-default-timeout channel))
                                        :metadata metadata
                                        :tls (channel-tls channel))))
    (ag-http2:connection-send-headers conn stream-id headers :end-stream end-stream)))

(defun channel-tls-p (channel)
  "Return T if the channel is using TLS encryption"
  (channel-tls channel))

(defun channel-send-message (channel stream-id message &key end-stream)
  "Send a message on a stream"
  (let ((frame (encode-grpc-message message)))
    (ag-http2:connection-send-data (channel-connection channel)
                                   stream-id
                                   frame
                                   :end-stream end-stream)))

(defun channel-receive-headers (channel stream-id)
  "Receive response headers from a stream"
  (let* ((conn (channel-connection channel))
         (stream (ag-http2:multiplexer-get-stream
                  (ag-http2:connection-multiplexer conn)
                  stream-id)))
    ;; Read frames until we get headers
    (loop while (null (ag-http2:stream-headers stream))
          do (ag-http2:connection-read-frame conn))
    (ag-http2:stream-headers stream)))

(defun channel-receive-message (channel stream-id)
  "Receive a message from a stream.
Handles messages that span multiple HTTP/2 DATA frames."
  (let* ((conn (channel-connection channel))
         (stream (ag-http2:multiplexer-get-stream
                  (ag-http2:connection-multiplexer conn)
                  stream-id)))
    ;; Keep reading until we have a complete gRPC message or stream closes
    (loop
      ;; Try to decode from current buffer (non-destructively peek first)
      (let* ((buffer (ag-http2:stream-data-buffer stream))
             (result (decode-grpc-message buffer 0)))
        (when result
          ;; Got a complete message - consume exactly what we decoded
          (multiple-value-bind (data compressed consumed)
              (decode-grpc-message buffer 0)
            (declare (ignore compressed))
            ;; Remove consumed bytes from buffer
            (let ((remaining (subseq buffer consumed)))
              (setf (fill-pointer buffer) 0)
              (loop for byte across remaining
                    do (vector-push-extend byte buffer)))
            (return data))))
      ;; Need more data - check if stream can receive more
      (unless (ag-http2:stream-can-recv-p stream)
        ;; Stream closed without complete message
        (return nil))
      ;; Read another frame
      (ag-http2:connection-read-frame conn))))

(defun channel-receive-trailers (channel stream-id)
  "Receive trailers from a stream (contains gRPC status)"
  (let* ((conn (channel-connection channel))
         (stream (ag-http2:multiplexer-get-stream
                  (ag-http2:connection-multiplexer conn)
                  stream-id)))
    ;; Read until stream is closed
    (loop while (ag-http2:stream-can-recv-p stream)
          do (ag-http2:connection-read-frame conn))
    (ag-http2:stream-trailers stream)))

(defun channel-stream-has-extra-data-p (channel stream-id)
  "Check if the stream's data buffer has unconsumed data.
For unary calls, this indicates multiple messages were sent which is an error."
  (let* ((conn (channel-connection channel))
         (stream (ag-http2:multiplexer-get-stream
                  (ag-http2:connection-multiplexer conn)
                  stream-id)))
    (plusp (length (ag-http2:stream-data-buffer stream)))))

(defun channel-cancel-stream (channel stream-id)
  "Cancel an in-progress RPC call by sending RST_STREAM with CANCEL error code.
This immediately terminates the stream without waiting for a response."
  (ag-http2:connection-send-rst-stream
   (channel-connection channel)
   stream-id
   ag-http2:+error-cancel+))
