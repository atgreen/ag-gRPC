;;;; async.lisp - Asynchronous/callback API for non-blocking calls
;;;;
;;;; Provides futures/promises for gRPC calls that don't block the caller.

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; Future/Promise Implementation
;;;; ========================================================================

(defclass grpc-future ()
  ((state :initform :pending :accessor future-state
          :documentation "State: :pending, :fulfilled, :rejected")
   (result :initform nil :accessor future-result
           :documentation "Result value when fulfilled")
   (error :initform nil :accessor future-error
          :documentation "Error when rejected")
   (lock :initform (bt:make-lock "future") :accessor future-lock
         :documentation "Lock for thread-safe access")
   (condition-var :initform (bt:make-condition-variable :name "future-cv")
                  :accessor future-condition-var
                  :documentation "Condition variable for waiting")
   (callbacks :initform nil :accessor future-callbacks
              :documentation "List of (on-success on-error) callbacks")
   (call :initarg :call :accessor future-call
         :initform nil
         :documentation "The underlying gRPC call (for cancellation)"))
  (:documentation "A future representing an asynchronous gRPC call result."))

(defun make-grpc-future ()
  "Create a new pending future."
  (make-instance 'grpc-future))

(defun future-pending-p (future)
  "Return T if the future is still pending."
  (eq (future-state future) :pending))

(defun future-fulfilled-p (future)
  "Return T if the future was fulfilled successfully."
  (eq (future-state future) :fulfilled))

(defun future-rejected-p (future)
  "Return T if the future was rejected with an error."
  (eq (future-state future) :rejected))

(defun future-done-p (future)
  "Return T if the future is complete (fulfilled or rejected)."
  (not (future-pending-p future)))

(defun future-fulfill (future result)
  "Fulfill the future with a result."
  (bt:with-lock-held ((future-lock future))
    (when (future-pending-p future)
      (setf (future-result future) result)
      (setf (future-state future) :fulfilled)
      (bt:condition-notify (future-condition-var future))
      ;; Run success callbacks
      (dolist (cb (future-callbacks future))
        (when (car cb)
          (handler-case
              (funcall (car cb) result)
            (error () nil)))))))

(defun future-reject (future error)
  "Reject the future with an error."
  (bt:with-lock-held ((future-lock future))
    (when (future-pending-p future)
      (setf (future-error future) error)
      (setf (future-state future) :rejected)
      (bt:condition-notify (future-condition-var future))
      ;; Run error callbacks
      (dolist (cb (future-callbacks future))
        (when (cadr cb)
          (handler-case
              (funcall (cadr cb) error)
            (error () nil)))))))

;;;; ========================================================================
;;;; Waiting and Callbacks
;;;; ========================================================================

(defun future-wait (future &key (timeout nil))
  "Wait for the future to complete.
TIMEOUT: Maximum seconds to wait (nil = wait forever)
Returns T if future completed, NIL if timeout expired."
  (bt:with-lock-held ((future-lock future))
    (loop while (future-pending-p future)
          do (if timeout
                 (unless (bt:condition-wait (future-condition-var future)
                                            (future-lock future)
                                            :timeout timeout)
                   (return-from future-wait nil))
                 (bt:condition-wait (future-condition-var future)
                                    (future-lock future))))
    t))

(defun future-get (future &key (timeout nil))
  "Get the result of the future, waiting if necessary.
TIMEOUT: Maximum seconds to wait
Returns the result value.
Signals the error if the future was rejected.
Signals error if timeout expires."
  (unless (future-wait future :timeout timeout)
    (error "Future timeout"))
  (if (future-fulfilled-p future)
      (future-result future)
      (error (future-error future))))

(defun future-then (future on-success &optional on-error)
  "Add callbacks to run when the future completes.
ON-SUCCESS: Function called with result on success
ON-ERROR: Function called with error on rejection
Returns a new future that resolves with the callback's return value."
  (let ((next-future (make-grpc-future)))
    (flet ((handle-success (result)
             (handler-case
                 (let ((new-result (if on-success
                                       (funcall on-success result)
                                       result)))
                   (future-fulfill next-future new-result))
               (error (e)
                 (future-reject next-future e))))
           (handle-error (error)
             (if on-error
                 (handler-case
                     (let ((result (funcall on-error error)))
                       (future-fulfill next-future result))
                   (error (e)
                     (future-reject next-future e)))
                 (future-reject next-future error))))
      (bt:with-lock-held ((future-lock future))
        (case (future-state future)
          (:pending
           (push (list #'handle-success #'handle-error) (future-callbacks future)))
          (:fulfilled
           (handle-success (future-result future)))
          (:rejected
           (handle-error (future-error future))))))
    next-future))

(defun future-catch (future on-error)
  "Add an error handler to the future.
ON-ERROR: Function called with error on rejection
Returns a new future."
  (future-then future nil on-error))

(defun future-finally (future callback)
  "Add a callback that runs regardless of outcome.
CALLBACK: Function called with no arguments
Returns a new future with the original result/error."
  (future-then future
               (lambda (result)
                 (funcall callback)
                 result)
               (lambda (error)
                 (funcall callback)
                 (error error))))

;;;; ========================================================================
;;;; Cancellation
;;;; ========================================================================

(defun future-cancel (future)
  "Cancel the underlying RPC call if possible.
Returns T if cancellation was attempted, NIL if future already complete."
  (bt:with-lock-held ((future-lock future))
    (when (and (future-pending-p future) (future-call future))
      (let ((call (future-call future)))
        (handler-case
            (let ((channel (call-channel call))
                  (stream-id (call-stream-id call)))
              (channel-cancel-stream channel stream-id)
              (future-reject future
                             (make-condition 'grpc-status-error
                                             :code +grpc-status-cancelled+
                                             :message "Cancelled by client"))
              t)
          (error () nil))))))

;;;; ========================================================================
;;;; Async Call Functions
;;;; ========================================================================

(defun call-unary-async (channel method request &key metadata timeout response-type
                                                      on-success on-error)
  "Make an asynchronous unary RPC call.
Returns a grpc-future immediately.
ON-SUCCESS: Optional callback (response) on success
ON-ERROR: Optional callback (error) on failure

Example:
  (let ((future (call-unary-async channel method request
                                   :response-type 'myresponse
                                   :on-success (lambda (r) (process r))
                                   :on-error (lambda (e) (log-error e)))))
    ;; Do other work...
    (future-get future))  ; Block when needed"
  (let ((future (make-grpc-future)))
    ;; Add user callbacks if provided
    (when (or on-success on-error)
      (push (list on-success on-error) (future-callbacks future)))
    ;; Launch call in background thread
    (bt:make-thread
     (lambda ()
       (handler-case
           (let ((call (call-unary channel method request
                                   :metadata metadata
                                   :timeout timeout
                                   :response-type response-type)))
             (setf (future-call future) call)
             (future-fulfill future (call-response call)))
         (error (e)
           (future-reject future e))))
     :name (format nil "async-~A" method))
    future))

(defun call-server-stream-async (channel method request &key metadata timeout response-type
                                                              on-message on-complete on-error)
  "Make an asynchronous server streaming RPC call.
ON-MESSAGE: Callback (message) for each received message
ON-COMPLETE: Callback () when stream completes
ON-ERROR: Callback (error) on failure
Returns a grpc-future that resolves when stream completes."
  (let ((future (make-grpc-future)))
    (bt:make-thread
     (lambda ()
       (handler-case
           (let ((stream (call-server-stream channel method request
                                             :metadata metadata
                                             :timeout timeout
                                             :response-type response-type)))
             (loop for msg = (stream-receive-message stream)
                   while msg
                   do (when on-message
                        (handler-case
                            (funcall on-message msg)
                          (error () nil))))
             (when on-complete
               (handler-case (funcall on-complete) (error () nil)))
             (future-fulfill future (stream-call-status stream)))
         (error (e)
           (when on-error
             (handler-case (funcall on-error e) (error () nil)))
           (future-reject future e))))
     :name (format nil "async-stream-~A" method))
    future))

;;;; ========================================================================
;;;; Combinators
;;;; ========================================================================

(defun future-all (futures)
  "Wait for all futures to complete.
Returns a future that resolves with a list of all results,
or rejects with the first error."
  (let ((result-future (make-grpc-future))
        (results (make-array (length futures) :initial-element nil))
        (completed 0)
        (lock (bt:make-lock "future-all")))
    (if (null futures)
        (future-fulfill result-future nil)
        (loop for future in futures
              for i from 0
              do (future-then future
                              (lambda (result)
                                (bt:with-lock-held (lock)
                                  (setf (aref results i) result)
                                  (incf completed)
                                  (when (= completed (length futures))
                                    (future-fulfill result-future
                                                    (coerce results 'list)))))
                              (lambda (error)
                                (bt:with-lock-held (lock)
                                  (when (future-pending-p result-future)
                                    (future-reject result-future error)))))))
    result-future))

(defun future-race (futures)
  "Return the first future to complete.
Returns a future that resolves/rejects with the first completed future."
  (let ((result-future (make-grpc-future)))
    (dolist (future futures)
      (future-then future
                   (lambda (result)
                     (bt:with-lock-held ((future-lock result-future))
                       (when (future-pending-p result-future)
                         (future-fulfill result-future result))))
                   (lambda (error)
                     (bt:with-lock-held ((future-lock result-future))
                       (when (future-pending-p result-future)
                         (future-reject result-future error))))))
    result-future))

(defun future-any (futures)
  "Return the first successful future.
Returns a future that resolves with the first success,
or rejects with the last error if all fail."
  (let ((result-future (make-grpc-future))
        (errors nil)
        (lock (bt:make-lock "future-any")))
    (dolist (future futures)
      (future-then future
                   (lambda (result)
                     (bt:with-lock-held (lock)
                       (when (future-pending-p result-future)
                         (future-fulfill result-future result))))
                   (lambda (error)
                     (bt:with-lock-held (lock)
                       (push error errors)
                       (when (and (future-pending-p result-future)
                                  (= (length errors) (length futures)))
                         (future-reject result-future (car errors)))))))
    result-future))
