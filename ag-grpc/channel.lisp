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

(defun make-channel (host port &key (connect t) timeout metadata)
  "Create a new gRPC channel to a server.
If CONNECT is true (default), immediately establish the connection."
  (let ((channel (make-instance 'grpc-channel
                                :host host
                                :port port
                                :default-timeout (or timeout 30)
                                :metadata metadata)))
    (when connect
      (channel-connect channel))
    channel))

(defun channel-connect (channel)
  "Establish the connection for this channel"
  (unless (channel-connection channel)
    (setf (channel-connection channel)
          (ag-http2:make-client-connection
           (channel-host channel)
           (channel-port channel)))))

(defun channel-connected-p (channel)
  "Return T if the channel has an active connection"
  (and (channel-connection channel)
       (eq (ag-http2::connection-state (channel-connection channel)) :open)))

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
  (ag-http2::connection-new-stream (channel-connection channel)))

(defun channel-send-headers (channel stream-id method &key metadata timeout end-stream)
  "Send request headers for an RPC call"
  (let* ((conn (channel-connection channel))
         (authority (format nil "~A:~A" (channel-host channel) (channel-port channel)))
         (headers (make-request-headers method
                                        :authority authority
                                        :timeout (or timeout (channel-default-timeout channel))
                                        :metadata metadata)))
    (ag-http2:connection-send-headers conn stream-id headers :end-stream end-stream)))

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
         (stream (ag-http2::multiplexer-get-stream
                  (ag-http2::connection-multiplexer conn)
                  stream-id)))
    ;; Read frames until we get headers
    (loop while (null (ag-http2::stream-headers stream))
          do (ag-http2:connection-read-frame conn))
    (ag-http2::stream-headers stream)))

(defun channel-receive-message (channel stream-id)
  "Receive a message from a stream"
  (let* ((conn (channel-connection channel))
         (stream (ag-http2::multiplexer-get-stream
                  (ag-http2::connection-multiplexer conn)
                  stream-id)))
    ;; Read frames until we have data and stream is half-closed or closed
    (loop while (and (zerop (length (ag-http2::stream-data-buffer stream)))
                     (ag-http2::stream-can-recv-p stream))
          do (ag-http2:connection-read-frame conn))
    (let ((data (ag-http2::stream-consume-data stream)))
      (when (plusp (length data))
        (decode-grpc-message data)))))

(defun channel-receive-trailers (channel stream-id)
  "Receive trailers from a stream (contains gRPC status)"
  (let* ((conn (channel-connection channel))
         (stream (ag-http2::multiplexer-get-stream
                  (ag-http2::connection-multiplexer conn)
                  stream-id)))
    ;; Read until stream is closed
    (loop while (ag-http2::stream-can-recv-p stream)
          do (ag-http2:connection-read-frame conn))
    (ag-http2::stream-trailers stream)))
