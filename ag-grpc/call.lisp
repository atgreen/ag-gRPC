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
    (setf (call-response-headers call)
          (channel-receive-headers channel stream-id))
    ;; Check initial response status
    (let ((status-header (assoc :status (call-response-headers call))))
      (unless (and status-header (string= (cdr status-header) "200"))
        (setf (call-status call) +grpc-status-unknown+)
        (return-from call-unary call)))
    ;; Receive response message
    (let ((response-data (channel-receive-message channel stream-id)))
      (when response-data
        (setf (call-response call)
              (if response-type
                  (ag-proto:deserialize-from-bytes response-type response-data)
                  response-data))))
    ;; Receive trailers (contains gRPC status)
    (setf (call-response-trailers call)
          (channel-receive-trailers channel stream-id))
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
