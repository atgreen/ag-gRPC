;;;; interceptor.lisp - Server interceptor/middleware system
;;;;
;;;; Interceptors provide hooks for cross-cutting concerns like
;;;; logging, authentication, metrics, and tracing.

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; Interceptor Protocol
;;;; ========================================================================

(defclass server-interceptor ()
  ()
  (:documentation "Base class for server-side interceptors.
Subclass and implement the generic functions to intercept RPC calls."))

(defgeneric interceptor-call-start (interceptor ctx handler-info)
  (:documentation "Called before handler dispatch.
CTX is the grpc-call-context.
HANDLER-INFO is a plist with :method-path, :handler-type (:unary, :server-streaming, etc.).
Can modify CTX or signal grpc-status-error to short-circuit.
Returns a context object passed to interceptor-call-end (for timing, etc.).")
  (:method ((interceptor server-interceptor) ctx handler-info)
    (declare (ignore ctx handler-info))
    nil))

(defgeneric interceptor-call-end (interceptor ctx handler-info call-context response error)
  (:documentation "Called after handler returns (or errors).
CTX is the grpc-call-context.
HANDLER-INFO is the same plist passed to call-start.
CALL-CONTEXT is what interceptor-call-start returned.
RESPONSE is the handler's return value (or NIL on error).
ERROR is the condition if handler signaled, or NIL on success.
Can modify or wrap the response. Return the (possibly modified) response.")
  (:method ((interceptor server-interceptor) ctx handler-info call-context response error)
    (declare (ignore ctx handler-info call-context error))
    response))

(defgeneric interceptor-recv-message (interceptor ctx message)
  (:documentation "Called for each message received in streaming RPCs.
MESSAGE is the deserialized message.
Return the (possibly modified) message, or signal error to abort.")
  (:method ((interceptor server-interceptor) ctx message)
    (declare (ignore ctx))
    message))

(defgeneric interceptor-send-message (interceptor ctx message)
  (:documentation "Called for each message sent in streaming RPCs.
MESSAGE is the message about to be serialized and sent.
Return the (possibly modified) message, or signal error to abort.")
  (:method ((interceptor server-interceptor) ctx message)
    (declare (ignore ctx))
    message))

;;;; ========================================================================
;;;; Interceptor Chain Execution
;;;; ========================================================================

(defun run-interceptors-call-start (interceptors ctx handler-info)
  "Run all interceptors' call-start methods. Returns list of call-contexts."
  (loop for interceptor in interceptors
        collect (interceptor-call-start interceptor ctx handler-info)))

(defun run-interceptors-call-end (interceptors ctx handler-info call-contexts response error)
  "Run all interceptors' call-end methods in reverse order.
Returns the final (possibly modified) response."
  (let ((result response))
    (loop for interceptor in (reverse interceptors)
          for call-ctx in (reverse call-contexts)
          do (setf result (interceptor-call-end interceptor ctx handler-info
                                                 call-ctx result error)))
    result))

(defun run-interceptors-recv-message (interceptors ctx message)
  "Run all interceptors' recv-message methods.
Returns the final (possibly modified) message."
  (let ((result message))
    (dolist (interceptor interceptors result)
      (setf result (interceptor-recv-message interceptor ctx result)))))

(defun run-interceptors-send-message (interceptors ctx message)
  "Run all interceptors' send-message methods.
Returns the final (possibly modified) message."
  (let ((result message))
    (dolist (interceptor interceptors result)
      (setf result (interceptor-send-message interceptor ctx result)))))

;;;; ========================================================================
;;;; Built-in Interceptors
;;;; ========================================================================

;;; Logging Interceptor

(defclass logging-interceptor (server-interceptor)
  ((stream :initarg :stream :initform *standard-output* :reader logging-stream)
   (log-requests :initarg :log-requests :initform t :reader log-requests-p)
   (log-responses :initarg :log-responses :initform t :reader log-responses-p)
   (log-errors :initarg :log-errors :initform t :reader log-errors-p))
  (:documentation "Logs RPC calls to a stream."))

(defun make-logging-interceptor (&key (stream *standard-output*)
                                       (log-requests t)
                                       (log-responses t)
                                       (log-errors t))
  "Create a logging interceptor."
  (make-instance 'logging-interceptor
                 :stream stream
                 :log-requests log-requests
                 :log-responses log-responses
                 :log-errors log-errors))

(defmethod interceptor-call-start ((interceptor logging-interceptor) ctx handler-info)
  (when (log-requests-p interceptor)
    (format (logging-stream interceptor)
            "[~A] RPC Start: ~A from ~A~%"
            (get-universal-time)
            (getf handler-info :method-path)
            (context-peer-address ctx)))
  (get-internal-real-time))

(defmethod interceptor-call-end ((interceptor logging-interceptor) ctx handler-info
                                  call-context response error)
  (declare (ignore ctx))
  (let ((elapsed-ms (/ (- (get-internal-real-time) call-context)
                       (/ internal-time-units-per-second 1000.0))))
    (cond
      ((and error (log-errors-p interceptor))
       (format (logging-stream interceptor)
               "[~A] RPC Error: ~A - ~A (~,2Fms)~%"
               (get-universal-time)
               (getf handler-info :method-path)
               error
               elapsed-ms))
      ((log-responses-p interceptor)
       (format (logging-stream interceptor)
               "[~A] RPC Complete: ~A (~,2Fms)~%"
               (get-universal-time)
               (getf handler-info :method-path)
               elapsed-ms))))
  response)

;;; Timing/Metrics Interceptor

(defclass metrics-interceptor (server-interceptor)
  ((call-counts :initform (make-hash-table :test 'equal) :reader metrics-call-counts)
   (call-durations :initform (make-hash-table :test 'equal) :reader metrics-call-durations)
   (error-counts :initform (make-hash-table :test 'equal) :reader metrics-error-counts))
  (:documentation "Collects metrics about RPC calls."))

(defun make-metrics-interceptor ()
  "Create a metrics interceptor."
  (make-instance 'metrics-interceptor))

(defmethod interceptor-call-start ((interceptor metrics-interceptor) ctx handler-info)
  (declare (ignore ctx))
  (let ((method (getf handler-info :method-path)))
    (incf (gethash method (metrics-call-counts interceptor) 0)))
  (get-internal-real-time))

(defmethod interceptor-call-end ((interceptor metrics-interceptor) ctx handler-info
                                  call-context response error)
  (declare (ignore ctx))
  (let* ((method (getf handler-info :method-path))
         (elapsed-ms (/ (- (get-internal-real-time) call-context)
                        (/ internal-time-units-per-second 1000.0))))
    (push elapsed-ms (gethash method (metrics-call-durations interceptor) nil))
    (when error
      (incf (gethash method (metrics-error-counts interceptor) 0))))
  response)

(defun metrics-get-stats (interceptor method)
  "Get statistics for a method. Returns (calls avg-duration-ms error-count)."
  (let* ((durations (gethash method (metrics-call-durations interceptor)))
         (count (gethash method (metrics-call-counts interceptor) 0))
         (errors (gethash method (metrics-error-counts interceptor) 0))
         (avg (if durations
                  (/ (reduce #'+ durations) (length durations))
                  0)))
    (values count avg errors)))

;;;; ========================================================================
;;;; Client-Side Interceptors
;;;; ========================================================================

(defclass client-interceptor ()
  ()
  (:documentation "Base class for client-side interceptors.
Subclass and implement the generic functions to intercept outgoing RPC calls."))

(defgeneric client-interceptor-call-start (interceptor call-info)
  (:documentation "Called before an RPC call is made.
CALL-INFO is a plist with :method, :channel, :metadata, :timeout.
Can modify metadata or signal an error to abort.
Returns a context object passed to client-interceptor-call-end.")
  (:method ((interceptor client-interceptor) call-info)
    (declare (ignore call-info))
    nil))

(defgeneric client-interceptor-call-end (interceptor call-info call-context response error)
  (:documentation "Called after an RPC call completes (or errors).
CALL-INFO is the same plist passed to call-start.
CALL-CONTEXT is what client-interceptor-call-start returned.
RESPONSE is the response message (or NIL on error).
ERROR is the condition if call failed, or NIL on success.
Can modify or wrap the response. Return the (possibly modified) response.")
  (:method ((interceptor client-interceptor) call-info call-context response error)
    (declare (ignore call-info call-context error))
    response))

(defgeneric client-interceptor-send-message (interceptor call-info message)
  (:documentation "Called for each message sent in streaming RPCs.
MESSAGE is the message about to be serialized and sent.
Return the (possibly modified) message, or signal error to abort.")
  (:method ((interceptor client-interceptor) call-info message)
    (declare (ignore call-info))
    message))

(defgeneric client-interceptor-recv-message (interceptor call-info message)
  (:documentation "Called for each message received in streaming RPCs.
MESSAGE is the deserialized message.
Return the (possibly modified) message, or signal error to abort.")
  (:method ((interceptor client-interceptor) call-info message)
    (declare (ignore call-info))
    message))

;;;; ========================================================================
;;;; Client Interceptor Chain Execution
;;;; ========================================================================

(defun run-client-interceptors-call-start (interceptors call-info)
  "Run all client interceptors' call-start methods. Returns list of call-contexts."
  (loop for interceptor in interceptors
        collect (client-interceptor-call-start interceptor call-info)))

(defun run-client-interceptors-call-end (interceptors call-info call-contexts response error)
  "Run all client interceptors' call-end methods in reverse order.
Returns the final (possibly modified) response."
  (let ((result response))
    (loop for interceptor in (reverse interceptors)
          for call-ctx in (reverse call-contexts)
          do (setf result (client-interceptor-call-end interceptor call-info
                                                        call-ctx result error)))
    result))

(defun run-client-interceptors-send-message (interceptors call-info message)
  "Run all client interceptors' send-message methods.
Returns the final (possibly modified) message."
  (let ((result message))
    (dolist (interceptor interceptors result)
      (setf result (client-interceptor-send-message interceptor call-info result)))))

(defun run-client-interceptors-recv-message (interceptors call-info message)
  "Run all client interceptors' recv-message methods.
Returns the final (possibly modified) message."
  (let ((result message))
    (dolist (interceptor interceptors result)
      (setf result (client-interceptor-recv-message interceptor call-info result)))))

;;;; ========================================================================
;;;; Built-in Client Interceptors
;;;; ========================================================================

;;; Client Logging Interceptor

(defclass client-logging-interceptor (client-interceptor)
  ((stream :initarg :stream :initform *standard-output* :reader client-logging-stream)
   (log-requests :initarg :log-requests :initform t :reader client-log-requests-p)
   (log-responses :initarg :log-responses :initform t :reader client-log-responses-p)
   (log-errors :initarg :log-errors :initform t :reader client-log-errors-p))
  (:documentation "Logs outgoing RPC calls to a stream."))

(defun make-client-logging-interceptor (&key (stream *standard-output*)
                                              (log-requests t)
                                              (log-responses t)
                                              (log-errors t))
  "Create a client logging interceptor."
  (make-instance 'client-logging-interceptor
                 :stream stream
                 :log-requests log-requests
                 :log-responses log-responses
                 :log-errors log-errors))

(defmethod client-interceptor-call-start ((interceptor client-logging-interceptor) call-info)
  (when (client-log-requests-p interceptor)
    (format (client-logging-stream interceptor)
            "[~A] RPC Call: ~A~%"
            (get-universal-time)
            (getf call-info :method)))
  (get-internal-real-time))

(defmethod client-interceptor-call-end ((interceptor client-logging-interceptor) call-info
                                         call-context response error)
  (let ((elapsed-ms (/ (- (get-internal-real-time) call-context)
                       (/ internal-time-units-per-second 1000.0))))
    (cond
      ((and error (client-log-errors-p interceptor))
       (format (client-logging-stream interceptor)
               "[~A] RPC Error: ~A - ~A (~,2Fms)~%"
               (get-universal-time)
               (getf call-info :method)
               error
               elapsed-ms))
      ((client-log-responses-p interceptor)
       (format (client-logging-stream interceptor)
               "[~A] RPC Complete: ~A (~,2Fms)~%"
               (get-universal-time)
               (getf call-info :method)
               elapsed-ms))))
  response)

;;; Client Metrics Interceptor

(defclass client-metrics-interceptor (client-interceptor)
  ((call-counts :initform (make-hash-table :test 'equal) :reader client-metrics-call-counts)
   (call-durations :initform (make-hash-table :test 'equal) :reader client-metrics-call-durations)
   (error-counts :initform (make-hash-table :test 'equal) :reader client-metrics-error-counts))
  (:documentation "Collects metrics about outgoing RPC calls."))

(defun make-client-metrics-interceptor ()
  "Create a client metrics interceptor."
  (make-instance 'client-metrics-interceptor))

(defmethod client-interceptor-call-start ((interceptor client-metrics-interceptor) call-info)
  (let ((method (getf call-info :method)))
    (incf (gethash method (client-metrics-call-counts interceptor) 0)))
  (get-internal-real-time))

(defmethod client-interceptor-call-end ((interceptor client-metrics-interceptor) call-info
                                         call-context response error)
  (let* ((method (getf call-info :method))
         (elapsed-ms (/ (- (get-internal-real-time) call-context)
                        (/ internal-time-units-per-second 1000.0))))
    (push elapsed-ms (gethash method (client-metrics-call-durations interceptor) nil))
    (when error
      (incf (gethash method (client-metrics-error-counts interceptor) 0))))
  response)

(defun client-metrics-get-stats (interceptor method)
  "Get client statistics for a method. Returns (calls avg-duration-ms error-count)."
  (let* ((durations (gethash method (client-metrics-call-durations interceptor)))
         (count (gethash method (client-metrics-call-counts interceptor) 0))
         (errors (gethash method (client-metrics-error-counts interceptor) 0))
         (avg (if durations
                  (/ (reduce #'+ durations) (length durations))
                  0)))
    (values count avg errors)))

;;; Metadata Injection Interceptor

(defclass metadata-interceptor (client-interceptor)
  ((metadata-fn :initarg :metadata-fn :accessor metadata-interceptor-fn
                :documentation "Function that returns metadata alist to merge"))
  (:documentation "Injects metadata into outgoing calls.
Useful for auth tokens, request IDs, tracing headers, etc."))

(defun make-metadata-interceptor (metadata-fn)
  "Create a metadata interceptor.
METADATA-FN is a function of no arguments that returns an alist of metadata
to merge into each outgoing call."
  (make-instance 'metadata-interceptor :metadata-fn metadata-fn))

(defmethod client-interceptor-call-start ((interceptor metadata-interceptor) call-info)
  (declare (ignore call-info))
  ;; The actual metadata injection happens in the channel code
  ;; This interceptor just provides the metadata function
  nil)
