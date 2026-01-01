;;;; response.lisp - Higher-level response objects with lazy/iterable semantics

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; gRPC Response Object
;;;;
;;;; Provides a unified interface for accessing RPC results with:
;;;; - Lazy evaluation (values computed on demand)
;;;; - Iteration support for streaming responses
;;;; - Consistent metadata access
;;;; ========================================================================

(defclass grpc-response ()
  ((call :initarg :call :reader response-call
         :documentation "Underlying gRPC call")
   (message :initarg :message :accessor response-message
            :initform nil
            :documentation "Response message (for unary calls)")
   (status :initarg :status :reader response-status
           :initform nil
           :documentation "gRPC status code")
   (status-message :initarg :status-message :reader response-status-message
                   :initform nil
                   :documentation "gRPC status message")
   (headers-metadata :initform nil :accessor response-headers-metadata
                     :documentation "Cached headers as grpc-metadata")
   (trailers-metadata :initform nil :accessor response-trailers-metadata
                      :documentation "Cached trailers as grpc-metadata"))
  (:documentation "Unified response object for gRPC calls"))

(defun make-response-from-call (call &optional message)
  "Create a response object from a completed call"
  (make-instance 'grpc-response
                 :call call
                 :message (or message (call-response call))
                 :status (call-status call)
                 :status-message (call-status-message call)))

(defmethod response-ok-p ((response grpc-response))
  "Return T if the response indicates success"
  (grpc-status-ok-p (response-status response)))

(defmethod response-headers ((response grpc-response))
  "Get response headers as grpc-metadata object (lazy conversion)"
  (or (response-headers-metadata response)
      (setf (response-headers-metadata response)
            (alist-to-metadata (call-response-headers (response-call response))))))

(defmethod response-trailers ((response grpc-response))
  "Get response trailers as grpc-metadata object (lazy conversion)"
  (or (response-trailers-metadata response)
      (setf (response-trailers-metadata response)
            (alist-to-metadata (call-response-trailers (response-call response))))))

(defmethod response-header ((response grpc-response) key)
  "Get a specific header value"
  (metadata-get (response-headers response) key))

(defmethod response-trailer ((response grpc-response) key)
  "Get a specific trailer value"
  (metadata-get (response-trailers response) key))

(defmethod print-object ((resp grpc-response) stream)
  "Print response in a readable format"
  (print-unreadable-object (resp stream :type t)
    (format stream "status=~A~@[ msg=~A~]"
            (grpc-status-name (response-status resp))
            (response-status-message resp))))

;;;; ========================================================================
;;;; Streaming Response Iterator
;;;;
;;;; Provides iteration protocol for streaming responses
;;;; ========================================================================

(defclass grpc-stream-iterator ()
  ((stream :initarg :stream :reader iterator-stream
           :documentation "Underlying gRPC stream (server or bidi)")
   (current :initform nil :accessor iterator-current
            :documentation "Current message")
   (exhausted-p :initform nil :accessor iterator-exhausted-p
                :documentation "T when no more messages")
   (response-type :initarg :response-type :reader iterator-response-type
                  :documentation "Response type for deserialization"))
  (:documentation "Iterator for streaming gRPC responses"))

(defun make-stream-iterator (stream)
  "Create an iterator from a gRPC server or bidi stream"
  (make-instance 'grpc-stream-iterator
                 :stream stream
                 :response-type (typecase stream
                                  (grpc-server-stream (stream-response-type stream))
                                  (grpc-bidi-stream (bidi-stream-response-type stream)))))

(defmethod iterator-next ((iter grpc-stream-iterator))
  "Get the next message, returns (values message present-p)"
  (when (iterator-exhausted-p iter)
    (return-from iterator-next (values nil nil)))
  (let ((msg (stream-read-message (iterator-stream iter))))
    (if msg
        (progn
          (setf (iterator-current iter) msg)
          (values msg t))
        (progn
          (setf (iterator-exhausted-p iter) t)
          (values nil nil)))))

(defmethod iterator-peek ((iter grpc-stream-iterator))
  "Peek at the current message without advancing"
  (iterator-current iter))

(defmethod iterator-done-p ((iter grpc-stream-iterator))
  "Return T if iterator is exhausted"
  (iterator-exhausted-p iter))

(defmethod iterator-status ((iter grpc-stream-iterator))
  "Get the final status after iteration completes"
  (stream-status (iterator-stream iter)))

;;;; ========================================================================
;;;; Streaming Response Collector
;;;;
;;;; Collects all messages from a stream with various output formats
;;;; ========================================================================

(defun collect-stream-messages (stream &key (limit nil) (transform #'identity))
  "Collect messages from a stream into a list.
LIMIT - Maximum number of messages to collect (nil = unlimited)
TRANSFORM - Function to apply to each message before collecting"
  (let ((messages nil)
        (count 0))
    (loop for msg = (stream-read-message stream)
          while (and msg (or (null limit) (< count limit)))
          do (push (funcall transform msg) messages)
             (incf count))
    (nreverse messages)))

(defun map-stream-messages (function stream)
  "Apply FUNCTION to each message from STREAM.
Returns the final status when complete."
  (loop for msg = (stream-read-message stream)
        while msg
        do (funcall function msg))
  (stream-status stream))

(defun reduce-stream-messages (function stream initial-value)
  "Reduce messages from STREAM using FUNCTION.
FUNCTION takes (accumulator message) and returns new accumulator."
  (loop with acc = initial-value
        for msg = (stream-read-message stream)
        while msg
        do (setf acc (funcall function acc msg))
        finally (return (values acc (stream-status stream)))))

(defun find-in-stream (predicate stream)
  "Find the first message matching PREDICATE.
Returns (values message found-p) or (values nil nil) if not found."
  (loop for msg = (stream-read-message stream)
        while msg
        when (funcall predicate msg)
          do (return (values msg t))
        finally (return (values nil nil))))

;;;; ========================================================================
;;;; Convenience Functions
;;;; ========================================================================

(defun response-value (response &optional default)
  "Get the response message, or DEFAULT if nil"
  (or (response-message response) default))

(defun check-response (response)
  "Signal an error if response is not OK, otherwise return the message"
  (unless (response-ok-p response)
    (error 'grpc-status-error
           :code (response-status response)
           :message (response-status-message response)))
  (response-message response))
