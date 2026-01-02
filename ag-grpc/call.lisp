;;;; call.lisp - gRPC call implementation

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; gRPC Call Object
;;;; ========================================================================

(defclass grpc-call ()
  ((channel :initarg :channel :accessor call-channel
            :documentation "The channel this call was made on")
   (rpc-method :initarg :method :accessor call-rpc-method
               :documentation "The RPC method path (e.g., /package.Service/Method)")
   (stream-id :initarg :stream-id :accessor call-stream-id
              :documentation "HTTP/2 stream ID")
   (request-metadata :initarg :request-metadata :accessor call-request-metadata
                     :initform nil
                     :documentation "Metadata sent with request")
   (response-headers :initform nil :accessor call-response-headers
                     :documentation "Response headers received")
   (response-trailers :initform nil :accessor call-response-trailers
                      :documentation "Response trailers received")
   (response :initform nil :accessor call-response
             :documentation "Deserialized response message")
   (status :initform nil :accessor call-status
           :documentation "gRPC status code")
   (status-message :initform nil :accessor call-status-message
                   :documentation "gRPC status message"))
  (:documentation "Represents a gRPC call"))

;;;; ========================================================================
;;;; Unary RPC
;;;; ========================================================================

(defun call-unary (channel method request &key metadata timeout response-type)
  "Make a unary RPC call.
CHANNEL - The gRPC channel to use
METHOD - The method path (e.g., \"/helloworld.Greeter/SayHello\")
REQUEST - The request message (a proto-message or byte vector)
METADATA - Optional call metadata
TIMEOUT - Optional timeout in seconds (overrides channel default)
RESPONSE-TYPE - Optional response type symbol for deserialization

Returns a grpc-call object. Check (call-status call) for success.
The response is available via (call-response call)."
  (ensure-connected channel)
  (let* ((stream (channel-new-stream channel))
         (stream-id (ag-http2:stream-id stream))
         (call (make-instance 'grpc-call
                              :channel channel
                              :method method
                              :stream-id stream-id
                              :request-metadata metadata)))
    ;; Send request headers
    (channel-send-headers channel stream-id method
                          :metadata metadata
                          :timeout timeout)
    ;; Send request message with END_STREAM
    (let ((request-bytes (if (typep request 'vector)
                             request
                             (ag-proto:serialize-to-bytes request))))
      (channel-send-message channel stream-id request-bytes :end-stream t))
    ;; Receive response headers
    (let ((raw-headers (channel-receive-headers channel stream-id)))
      (setf (call-response-headers call) raw-headers))
    ;; Check initial response status
    (let ((status-header (assoc :status (call-response-headers call))))
      (unless (and status-header (string= (cdr status-header) "200"))
        (setf (call-status call) +grpc-status-unknown+)
        (return-from call-unary call)))
    ;; Check for "trailers-only" mode - gRPC status in headers (error without body)
    (let ((grpc-status-header (assoc "grpc-status" (call-response-headers call)
                                     :test #'string-equal)))
      (when grpc-status-header
        ;; Trailers-only response - status is in headers
        (setf (call-status call) (parse-integer (cdr grpc-status-header)))
        (let ((message-header (assoc "grpc-message" (call-response-headers call)
                                     :test #'string-equal)))
          (when message-header
            (setf (call-status-message call)
                  (percent-decode (cdr message-header)))))
        ;; Signal error if not OK
        (unless (grpc-status-ok-p (call-status call))
          (error 'grpc-status-error
                 :code (call-status call)
                 :message (call-status-message call)
                 :headers (call-response-headers call)
                 :trailers (call-response-headers call)))  ; trailers-only: trailers are in headers
        (return-from call-unary call)))
    ;; Receive response message
    (let ((response-data (channel-receive-message channel stream-id)))
      (when response-data
        (setf (call-response call)
              (if response-type
                  (ag-proto:deserialize-from-bytes response-type response-data)
                  response-data))))
    ;; Receive trailers (contains gRPC status)
    (let ((raw-trailers (channel-receive-trailers channel stream-id)))
      (setf (call-response-trailers call) raw-trailers))
    ;; Extract status from trailers
    (let ((status-trailer (assoc "grpc-status" (call-response-trailers call)
                                 :test #'string-equal)))
      (setf (call-status call)
            (if status-trailer
                (parse-integer (cdr status-trailer))
                +grpc-status-ok+)))
    (let ((message-trailer (assoc "grpc-message" (call-response-trailers call)
                                  :test #'string-equal)))
      (when message-trailer
        (setf (call-status-message call)
              (percent-decode (cdr message-trailer)))))
    ;; Signal error if not OK
    (unless (grpc-status-ok-p (call-status call))
      (error 'grpc-status-error
             :code (call-status call)
             :message (call-status-message call)
             :headers (call-response-headers call)
             :trailers (call-response-trailers call)))
    call))

(defun ensure-connected (channel)
  "Ensure the channel is connected, connecting if necessary"
  (unless (channel-connected-p channel)
    (channel-connect channel)))

;;;; ========================================================================
;;;; Convenience Functions
;;;; ========================================================================

(defun call-ok-p (call)
  "Return T if the call completed successfully"
  (and (call-status call)
       (grpc-status-ok-p (call-status call))))

(defun call-metadata (call &optional key)
  "Get call response metadata. If KEY is provided, return just that value."
  (if key
      (cdr (assoc key (call-response-headers call) :test #'string-equal))
      (call-response-headers call)))

(defun call-trailing-metadata (call &optional key)
  "Get call trailing metadata. If KEY is provided, return just that value."
  (if key
      (cdr (assoc key (call-response-trailers call) :test #'string-equal))
      (call-response-trailers call)))

;;;; ========================================================================
;;;; Simple Call Interface
;;;; ========================================================================

(defun grpc-call (channel method request &key metadata timeout response-type)
  "Simple interface for making a unary gRPC call.
Returns the response message directly, or signals an error on failure."
  (let ((call (call-unary channel method request
                          :metadata metadata
                          :timeout timeout
                          :response-type response-type)))
    (call-response call)))

;;;; ========================================================================
;;;; Method Path Utilities
;;;; ========================================================================

(defun make-method-path (service method)
  "Create a gRPC method path from service and method names.
Example: (make-method-path \"helloworld.Greeter\" \"SayHello\")
         => \"/helloworld.Greeter/SayHello\""
  (format nil "/~A/~A" service method))

(defun parse-method-path (path)
  "Parse a gRPC method path into (values service method)"
  (let* ((trimmed (string-left-trim "/" path))
         (slash-pos (position #\/ trimmed)))
    (if slash-pos
        (values (subseq trimmed 0 slash-pos)
                (subseq trimmed (1+ slash-pos)))
        (values trimmed nil))))

;;;; ========================================================================
;;;; Server Streaming RPC
;;;; ========================================================================

(defclass grpc-server-stream ()
  ((call :initarg :call :accessor stream-call
         :documentation "The underlying gRPC call")
   (response-type :initarg :response-type :accessor stream-response-type
                  :initform nil
                  :documentation "Response type for deserialization")
   (buffer :initform (make-array 0 :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0)
           :accessor stream-buffer
           :documentation "Buffer for partial messages")
   (finished-p :initform nil :accessor stream-finished-p
               :documentation "True when stream is complete")
   (status :initform nil :accessor stream-status
           :documentation "Final gRPC status (set when stream ends)"))
  (:documentation "Represents a server streaming response"))

(defun call-server-streaming (channel method request &key metadata timeout response-type)
  "Initiate a server streaming RPC call.
CHANNEL - The gRPC channel to use
METHOD - The method path (e.g., \"/helloworld.Greeter/ListFeatures\")
REQUEST - The request message (a proto-message or byte vector)
METADATA - Optional call metadata
TIMEOUT - Optional timeout in seconds (overrides channel default)
RESPONSE-TYPE - Response type symbol for deserialization

Returns a grpc-server-stream object. Use stream-read-message to read responses."
  (ensure-connected channel)
  (let* ((stream (channel-new-stream channel))
         (stream-id (ag-http2:stream-id stream))
         (call (make-instance 'grpc-call
                              :channel channel
                              :method method
                              :stream-id stream-id
                              :request-metadata metadata)))
    ;; Send request headers
    (channel-send-headers channel stream-id method
                          :metadata metadata
                          :timeout timeout)
    ;; Send request message with END_STREAM (client is done sending)
    (let ((request-bytes (if (typep request 'vector)
                             request
                             (ag-proto:serialize-to-bytes request))))
      (channel-send-message channel stream-id request-bytes :end-stream t))
    ;; Receive response headers
    (let ((raw-headers (channel-receive-headers channel stream-id)))
      (setf (call-response-headers call) raw-headers))
    ;; Check initial response status
    (let ((status-header (assoc :status (call-response-headers call))))
      (unless (and status-header (string= (cdr status-header) "200"))
        (error 'grpc-status-error
               :code +grpc-status-unknown+
               :message "Server returned non-200 status")))
    ;; Return the stream object
    (make-instance 'grpc-server-stream
                   :call call
                   :response-type response-type)))

(defgeneric stream-read-message (stream)
  (:documentation "Read the next message from a streaming RPC.
Returns the deserialized message, or NIL if the stream is finished.
When the stream ends, also sets stream-status."))

(defmethod stream-read-message ((server-stream grpc-server-stream))
  "Read the next message from a server stream."
  (when (stream-finished-p server-stream)
    (return-from stream-read-message nil))
  (let* ((call (stream-call server-stream))
         (channel (call-channel call))
         (stream-id (call-stream-id call))
         (conn (channel-connection channel))
         (h2-stream (ag-http2:multiplexer-get-stream
                     (ag-http2:connection-multiplexer conn)
                     stream-id))
         (buffer (stream-buffer server-stream)))
    ;; Try to decode a message from the buffer first
    (multiple-value-bind (data compressed consumed)
        (decode-grpc-message buffer 0)
      (declare (ignore compressed))
      (when data
        ;; Got a complete message from buffer
        (let ((remaining (subseq buffer consumed)))
          (setf (fill-pointer buffer) 0)
          (loop for byte across remaining
                do (vector-push-extend byte buffer)))
        (return-from stream-read-message
          (if (stream-response-type server-stream)
              (ag-proto:deserialize-from-bytes (stream-response-type server-stream) data)
              data))))
    ;; Need to read more data
    (loop
      ;; Check if stream can still receive
      (unless (ag-http2:stream-can-recv-p h2-stream)
        ;; Stream is done, extract final status from trailers
        (let* ((trailers (ag-http2:stream-trailers h2-stream)))
          (setf (call-response-trailers call) trailers)
          (let ((status-trailer (assoc "grpc-status" trailers :test #'string-equal)))
            (setf (stream-status server-stream)
                  (if status-trailer
                      (parse-integer (cdr status-trailer))
                      +grpc-status-ok+))
            (setf (call-status call) (stream-status server-stream)))
          ;; Check for any remaining data in buffer
          (multiple-value-bind (data compressed consumed)
              (decode-grpc-message buffer 0)
            (declare (ignore compressed consumed))
            (when data
              (return-from stream-read-message
                (if (stream-response-type server-stream)
                    (ag-proto:deserialize-from-bytes (stream-response-type server-stream) data)
                    data))))
          ;; Mark stream as finished
          (setf (stream-finished-p server-stream) t)
          ;; Signal error if not OK
          (unless (grpc-status-ok-p (stream-status server-stream))
            (error 'grpc-status-error
                   :code (stream-status server-stream)
                   :message (call-status-message call)))
          (return-from stream-read-message nil)))
      ;; Read a frame
      (ag-http2:connection-read-frame conn)
      ;; Copy any new data to our buffer
      (let ((new-data (ag-http2:stream-consume-data h2-stream)))
        (loop for byte across new-data
              do (vector-push-extend byte buffer)))
      ;; Try to decode a message
      (multiple-value-bind (data compressed consumed)
          (decode-grpc-message buffer 0)
        (declare (ignore compressed))
        (when data
          ;; Got a complete message
          (let ((remaining (subseq buffer consumed)))
            (setf (fill-pointer buffer) 0)
            (loop for byte across remaining
                  do (vector-push-extend byte buffer)))
          (return-from stream-read-message
            (if (stream-response-type server-stream)
                (ag-proto:deserialize-from-bytes (stream-response-type server-stream) data)
                data)))))))

(defmacro do-stream-messages ((var server-stream &optional result) &body body)
  "Iterate over all messages in a server stream.
VAR is bound to each message in turn.
Returns RESULT (default NIL) when the stream is exhausted.

Example:
  (do-stream-messages (msg stream)
    (format t \"Got: ~A~%\" msg))"
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,server-stream))
       (loop for ,var = (stream-read-message ,stream-var)
             while ,var
             do (progn ,@body)
             finally (return ,result)))))

(defun stream-collect-all (server-stream)
  "Read all messages from a server stream and return them as a list."
  (let ((messages nil))
    (do-stream-messages (msg server-stream (nreverse messages))
      (push msg messages))))

;;;; ========================================================================
;;;; Client Streaming RPC
;;;; ========================================================================

(defclass grpc-client-stream ()
  ((call :initarg :call :accessor stream-call
         :documentation "The underlying gRPC call")
   (channel :initarg :channel :accessor client-stream-channel
            :documentation "The gRPC channel")
   (stream-id :initarg :stream-id :accessor client-stream-id
              :documentation "HTTP/2 stream ID")
   (response-type :initarg :response-type :accessor client-stream-response-type
                  :initform nil
                  :documentation "Response type for deserialization")
   (closed-p :initform nil :accessor client-stream-closed-p
             :documentation "True when client has finished sending"))
  (:documentation "Represents a client streaming request"))

(defun call-client-streaming (channel method &key metadata timeout response-type)
  "Initiate a client streaming RPC call.
CHANNEL - The gRPC channel to use
METHOD - The method path (e.g., \"/package.Service/RecordRoute\")
METADATA - Optional call metadata
TIMEOUT - Optional timeout in seconds (overrides channel default)
RESPONSE-TYPE - Response type symbol for deserialization

Returns a grpc-client-stream object. Use stream-send to send messages,
then stream-close-and-recv to finish and get the response."
  (ensure-connected channel)
  (let* ((stream (channel-new-stream channel))
         (stream-id (ag-http2:stream-id stream))
         (call (make-instance 'grpc-call
                              :channel channel
                              :method method
                              :stream-id stream-id
                              :request-metadata metadata)))
    ;; Send request headers (NOT end-stream, we're going to send data)
    (channel-send-headers channel stream-id method
                          :metadata metadata
                          :timeout timeout
                          :end-stream nil)
    ;; Return the client stream object
    (make-instance 'grpc-client-stream
                   :call call
                   :channel channel
                   :stream-id stream-id
                   :response-type response-type)))

(defgeneric stream-send (stream message)
  (:documentation "Send a message on a streaming RPC.
MESSAGE can be a proto-message or a byte vector.
Returns the stream for chaining."))

(defmethod stream-send ((client-stream grpc-client-stream) message)
  "Send a message on a client stream."
  (when (client-stream-closed-p client-stream)
    (error "Cannot send on closed client stream"))
  (let ((message-bytes (if (typep message 'vector)
                           message
                           (ag-proto:serialize-to-bytes message))))
    (channel-send-message (client-stream-channel client-stream)
                          (client-stream-id client-stream)
                          message-bytes
                          :end-stream nil))
  client-stream)

(defun stream-close-and-recv (client-stream)
  "Close the client stream and receive the server's response.
Signals END_STREAM to the server, then waits for the response.
Returns (values response status) where response is the deserialized message."
  (when (client-stream-closed-p client-stream)
    (error "Client stream already closed"))
  (setf (client-stream-closed-p client-stream) t)
  (let* ((channel (client-stream-channel client-stream))
         (stream-id (client-stream-id client-stream))
         (call (stream-call client-stream))
         (conn (channel-connection channel)))
    ;; Send empty DATA frame with END_STREAM to signal we're done sending
    (ag-http2:connection-send-data conn stream-id
                                   (make-array 0 :element-type '(unsigned-byte 8))
                                   :end-stream t)
    ;; Receive response headers
    (let ((raw-headers (channel-receive-headers channel stream-id)))
      (setf (call-response-headers call) raw-headers))
    ;; Check initial response status
    (let ((status-header (assoc :status (call-response-headers call))))
      (unless (and status-header (string= (cdr status-header) "200"))
        (setf (call-status call) +grpc-status-unknown+)
        (return-from stream-close-and-recv
          (values nil (call-status call)))))
    ;; Receive response message
    (let ((response-data (channel-receive-message channel stream-id)))
      (when response-data
        (setf (call-response call)
              (if (client-stream-response-type client-stream)
                  (ag-proto:deserialize-from-bytes
                   (client-stream-response-type client-stream) response-data)
                  response-data))))
    ;; Receive trailers (contains gRPC status)
    (let ((raw-trailers (channel-receive-trailers channel stream-id)))
      (setf (call-response-trailers call) raw-trailers))
    ;; Extract status from trailers
    (let ((status-trailer (assoc "grpc-status" (call-response-trailers call)
                                 :test #'string-equal)))
      (setf (call-status call)
            (if status-trailer
                (parse-integer (cdr status-trailer))
                +grpc-status-ok+)))
    (let ((message-trailer (assoc "grpc-message" (call-response-trailers call)
                                  :test #'string-equal)))
      (when message-trailer
        (setf (call-status-message call)
              (percent-decode (cdr message-trailer)))))
    ;; Signal error if not OK
    (unless (grpc-status-ok-p (call-status call))
      (error 'grpc-status-error
             :code (call-status call)
             :message (call-status-message call)))
    (values (call-response call) (call-status call))))

(defmacro with-client-stream ((var channel method &key metadata timeout response-type) &body body)
  "Execute BODY with VAR bound to a client stream, ensuring proper cleanup.
After BODY executes, automatically calls stream-close-and-recv.
Returns (values response status).

Example:
  (with-client-stream (stream channel \"/pkg.Svc/Record\" :response-type 'summary)
    (stream-send stream point1)
    (stream-send stream point2)
    (stream-send stream point3))"
  `(let ((,var (call-client-streaming ,channel ,method
                                       :metadata ,metadata
                                       :timeout ,timeout
                                       :response-type ,response-type)))
     ,@body
     (stream-close-and-recv ,var)))

;;;; ========================================================================
;;;; Bidirectional Streaming RPC
;;;; ========================================================================

(defclass grpc-bidi-stream ()
  ((call :initarg :call :accessor stream-call
         :documentation "The underlying gRPC call")
   (channel :initarg :channel :accessor bidi-stream-channel
            :documentation "The gRPC channel")
   (stream-id :initarg :stream-id :accessor bidi-stream-id
              :documentation "HTTP/2 stream ID")
   (response-type :initarg :response-type :accessor bidi-stream-response-type
                  :initform nil
                  :documentation "Response type for deserialization")
   (buffer :initform (make-array 0 :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0)
           :accessor bidi-stream-buffer
           :documentation "Buffer for partial messages")
   (send-closed-p :initform nil :accessor bidi-stream-send-closed-p
                  :documentation "True when client has finished sending")
   (recv-finished-p :initform nil :accessor bidi-stream-recv-finished-p
                    :documentation "True when server stream is complete")
   (status :initform nil :accessor stream-status
           :documentation "Final gRPC status (set when stream ends)"))
  (:documentation "Represents a bidirectional streaming RPC"))

(defun call-bidirectional-streaming (channel method &key metadata timeout response-type)
  "Initiate a bidirectional streaming RPC call.
CHANNEL - The gRPC channel to use
METHOD - The method path (e.g., \"/package.Service/Chat\")
METADATA - Optional call metadata
TIMEOUT - Optional timeout in seconds (overrides channel default)
RESPONSE-TYPE - Response type symbol for deserialization

Returns a grpc-bidi-stream object. Use stream-send to send messages,
stream-read-message to receive messages, and stream-close-send when done sending."
  (ensure-connected channel)
  (let* ((stream (channel-new-stream channel))
         (stream-id (ag-http2:stream-id stream))
         (call (make-instance 'grpc-call
                              :channel channel
                              :method method
                              :stream-id stream-id
                              :request-metadata metadata)))
    ;; Send request headers (NOT end-stream, we're going to send data)
    (channel-send-headers channel stream-id method
                          :metadata metadata
                          :timeout timeout
                          :end-stream nil)
    ;; Return the bidi stream object
    (make-instance 'grpc-bidi-stream
                   :call call
                   :channel channel
                   :stream-id stream-id
                   :response-type response-type)))

(defmethod stream-send ((bidi-stream grpc-bidi-stream) message)
  "Send a message on a bidirectional stream.
MESSAGE can be a proto-message or a byte vector.
Returns the bidi-stream for chaining."
  (when (bidi-stream-send-closed-p bidi-stream)
    (error "Cannot send on closed stream"))
  (let ((message-bytes (if (typep message 'vector)
                           message
                           (ag-proto:serialize-to-bytes message))))
    (channel-send-message (bidi-stream-channel bidi-stream)
                          (bidi-stream-id bidi-stream)
                          message-bytes
                          :end-stream nil))
  bidi-stream)

(defun stream-close-send (bidi-stream)
  "Close the send side of a bidirectional stream.
After this, no more messages can be sent, but messages can still be received.
Returns the bidi-stream."
  (when (bidi-stream-send-closed-p bidi-stream)
    (return-from stream-close-send bidi-stream))
  (setf (bidi-stream-send-closed-p bidi-stream) t)
  (let* ((channel (bidi-stream-channel bidi-stream))
         (stream-id (bidi-stream-id bidi-stream))
         (conn (channel-connection channel)))
    ;; Send empty DATA frame with END_STREAM to signal we're done sending
    (ag-http2:connection-send-data conn stream-id
                                   (make-array 0 :element-type '(unsigned-byte 8))
                                   :end-stream t))
  bidi-stream)

(defmethod stream-read-message ((bidi-stream grpc-bidi-stream))
  "Read the next message from a bidirectional stream.
Returns the deserialized message, or NIL if the stream is finished.
When the stream ends, also sets stream-status."
  (when (bidi-stream-recv-finished-p bidi-stream)
    (return-from stream-read-message nil))
  (let* ((call (stream-call bidi-stream))
         (channel (bidi-stream-channel bidi-stream))
         (stream-id (bidi-stream-id bidi-stream))
         (conn (channel-connection channel))
         (h2-stream (ag-http2:multiplexer-get-stream
                     (ag-http2:connection-multiplexer conn)
                     stream-id))
         (buffer (bidi-stream-buffer bidi-stream)))
    ;; First time reading: check for response headers
    (unless (call-response-headers call)
      (let ((raw-headers (channel-receive-headers channel stream-id)))
        (setf (call-response-headers call) raw-headers))
      ;; Check initial response status
      (let ((status-header (assoc :status (call-response-headers call))))
        (unless (and status-header (string= (cdr status-header) "200"))
          (setf (stream-status bidi-stream) +grpc-status-unknown+)
          (setf (bidi-stream-recv-finished-p bidi-stream) t)
          (return-from stream-read-message nil))))
    ;; Try to decode a message from the buffer first
    (multiple-value-bind (data compressed consumed)
        (decode-grpc-message buffer 0)
      (declare (ignore compressed))
      (when data
        ;; Got a complete message from buffer
        (let ((remaining (subseq buffer consumed)))
          (setf (fill-pointer buffer) 0)
          (loop for byte across remaining
                do (vector-push-extend byte buffer)))
        (return-from stream-read-message
          (if (bidi-stream-response-type bidi-stream)
              (ag-proto:deserialize-from-bytes (bidi-stream-response-type bidi-stream) data)
              data))))
    ;; Need to read more data
    (loop
      ;; Check if stream can still receive
      (unless (ag-http2:stream-can-recv-p h2-stream)
        ;; Stream is done, extract final status from trailers
        (let* ((trailers (ag-http2:stream-trailers h2-stream)))
          (setf (call-response-trailers call) trailers)
          (let ((status-trailer (assoc "grpc-status" trailers :test #'string-equal)))
            (setf (stream-status bidi-stream)
                  (if status-trailer
                      (parse-integer (cdr status-trailer))
                      +grpc-status-ok+))
            (setf (call-status call) (stream-status bidi-stream)))
          ;; Check for any remaining data in buffer
          (multiple-value-bind (data compressed consumed)
              (decode-grpc-message buffer 0)
            (declare (ignore compressed consumed))
            (when data
              (return-from stream-read-message
                (if (bidi-stream-response-type bidi-stream)
                    (ag-proto:deserialize-from-bytes (bidi-stream-response-type bidi-stream) data)
                    data))))
          ;; Mark stream as finished
          (setf (bidi-stream-recv-finished-p bidi-stream) t)
          ;; Signal error if not OK
          (unless (grpc-status-ok-p (stream-status bidi-stream))
            (error 'grpc-status-error
                   :code (stream-status bidi-stream)
                   :message (call-status-message call)))
          (return-from stream-read-message nil)))
      ;; Read a frame
      (ag-http2:connection-read-frame conn)
      ;; Copy any new data to our buffer
      (let ((new-data (ag-http2:stream-consume-data h2-stream)))
        (loop for byte across new-data
              do (vector-push-extend byte buffer)))
      ;; Try to decode a message
      (multiple-value-bind (data compressed consumed)
          (decode-grpc-message buffer 0)
        (declare (ignore compressed))
        (when data
          ;; Got a complete message
          (let ((remaining (subseq buffer consumed)))
            (setf (fill-pointer buffer) 0)
            (loop for byte across remaining
                  do (vector-push-extend byte buffer)))
          (return-from stream-read-message
            (if (bidi-stream-response-type bidi-stream)
                (ag-proto:deserialize-from-bytes (bidi-stream-response-type bidi-stream) data)
                data)))))))

(defmacro do-bidi-recv ((var bidi-stream &optional result) &body body)
  "Iterate over all received messages in a bidirectional stream.
VAR is bound to each message in turn.
Returns RESULT (default NIL) when the stream is exhausted.

Example:
  (do-bidi-recv (msg stream)
    (format t \"Got: ~A~%\" msg))"
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,bidi-stream))
       (loop for ,var = (stream-read-message ,stream-var)
             while ,var
             do (progn ,@body)
             finally (return ,result)))))

;;;; ========================================================================
;;;; Convenience Macros
;;;; ========================================================================

(defmacro with-channel ((var host port &rest args &key connect timeout metadata tls tls-verify) &body body)
  "Execute BODY with VAR bound to a gRPC channel, ensuring proper cleanup.
The channel is automatically closed when BODY exits (normally or via error).

Example:
  (with-channel (ch \"localhost\" 50051)
    (call-unary ch \"/pkg.Svc/Method\" request))"
  (declare (ignore connect timeout metadata tls tls-verify))
  `(let ((,var (make-channel ,host ,port ,@args)))
     (unwind-protect
          (progn ,@body)
       (channel-close ,var))))

(defmacro with-call ((var channel method request &rest args
                      &key metadata timeout response-type) &body body)
  "Execute BODY with VAR bound to a completed unary call result.
Provides convenient access to call results with automatic error handling.

Example:
  (with-call (call ch \"/pkg.Svc/Method\" request :response-type 'response)
    (format t \"Response: ~A~%\" (call-response call)))"
  (declare (ignore metadata timeout response-type))
  `(let ((,var (call-unary ,channel ,method ,request ,@args)))
     ,@body))

(defmacro with-bidi-stream ((var channel method &rest args
                             &key metadata timeout response-type) &body body)
  "Execute BODY with VAR bound to a bidirectional stream.
Automatically closes the send side when BODY exits.

Example:
  (with-bidi-stream (stream ch \"/pkg.Svc/Chat\" :response-type 'msg)
    (stream-send stream msg1)
    (stream-send stream msg2)
    (stream-close-send stream)
    (do-bidi-recv (reply stream)
      (process reply)))"
  (declare (ignore metadata timeout response-type))
  `(let ((,var (call-bidirectional-streaming ,channel ,method ,@args)))
     (unwind-protect
          (progn ,@body)
       (unless (bidi-stream-send-closed-p ,var)
         (stream-close-send ,var)))))

(defmacro with-server-stream ((var channel method request &rest args
                               &key metadata timeout response-type) &body body)
  "Execute BODY with VAR bound to a server streaming response.
Provides convenient iteration over server stream responses.

Example:
  (with-server-stream (stream ch \"/pkg.Svc/List\" request :response-type 'item)
    (do-stream-messages (item stream)
      (process item)))"
  (declare (ignore metadata timeout response-type))
  `(let ((,var (call-server-streaming ,channel ,method ,request ,@args)))
     ,@body))
