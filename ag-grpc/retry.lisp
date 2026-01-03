;;;; retry.lisp - Retry policies for gRPC calls
;;;;
;;;; Provides automatic retry with exponential backoff for transient failures.

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; Retry Policy Configuration
;;;; ========================================================================

(defclass retry-policy ()
  ((max-attempts :initarg :max-attempts :accessor retry-max-attempts
                 :initform 3
                 :documentation "Maximum number of attempts (including first)")
   (initial-backoff :initarg :initial-backoff :accessor retry-initial-backoff
                    :initform 0.1
                    :documentation "Initial backoff in seconds")
   (max-backoff :initarg :max-backoff :accessor retry-max-backoff
                :initform 10.0
                :documentation "Maximum backoff in seconds")
   (backoff-multiplier :initarg :backoff-multiplier :accessor retry-backoff-multiplier
                       :initform 2.0
                       :documentation "Multiplier for exponential backoff")
   (retryable-status-codes :initarg :retryable-status-codes
                           :accessor retry-retryable-status-codes
                           :initform (list +grpc-status-unavailable+
                                           +grpc-status-resource-exhausted+
                                           +grpc-status-aborted+)
                           :documentation "gRPC status codes that trigger retry")
   (jitter :initarg :jitter :accessor retry-jitter
           :initform 0.2
           :documentation "Random jitter factor (0-1) added to backoff"))
  (:documentation "Configuration for retry behavior."))

(defun make-retry-policy (&key (max-attempts 3)
                               (initial-backoff 0.1)
                               (max-backoff 10.0)
                               (backoff-multiplier 2.0)
                               (retryable-status-codes nil retryable-supplied-p)
                               (jitter 0.2))
  "Create a retry policy.
MAX-ATTEMPTS: Maximum number of attempts (default 3)
INITIAL-BACKOFF: Initial backoff in seconds (default 0.1)
MAX-BACKOFF: Maximum backoff in seconds (default 10.0)
BACKOFF-MULTIPLIER: Multiplier for exponential backoff (default 2.0)
RETRYABLE-STATUS-CODES: List of status codes to retry (default: UNAVAILABLE, RESOURCE_EXHAUSTED, ABORTED)
JITTER: Random jitter factor 0-1 (default 0.2)"
  (make-instance 'retry-policy
                 :max-attempts max-attempts
                 :initial-backoff initial-backoff
                 :max-backoff max-backoff
                 :backoff-multiplier backoff-multiplier
                 :retryable-status-codes (if retryable-supplied-p
                                             retryable-status-codes
                                             (list +grpc-status-unavailable+
                                                   +grpc-status-resource-exhausted+
                                                   +grpc-status-aborted+))
                 :jitter jitter))

(defun compute-backoff (policy attempt)
  "Compute backoff delay for given attempt number (0-based)."
  (let* ((base (* (retry-initial-backoff policy)
                  (expt (retry-backoff-multiplier policy) attempt)))
         (capped (min base (retry-max-backoff policy)))
         (jitter-amount (* capped (retry-jitter policy) (random 1.0))))
    (+ capped jitter-amount)))

(defun retryable-error-p (policy error)
  "Check if the error should trigger a retry."
  (and (typep error 'grpc-status-error)
       (member (grpc-status-error-code error)
               (retry-retryable-status-codes policy))))

;;;; ========================================================================
;;;; Retry Wrapper Functions
;;;; ========================================================================

(defun call-with-retry (thunk policy &key on-retry)
  "Call THUNK with retry according to POLICY.
THUNK is a function of no arguments that makes the RPC call.
ON-RETRY is an optional function called before each retry with (attempt error backoff).
Returns the result of THUNK on success.
Signals the last error if all retries are exhausted."
  (let ((last-error nil)
        (max-attempts (retry-max-attempts policy)))
    (dotimes (attempt max-attempts)
      (handler-case
          (return-from call-with-retry (funcall thunk))
        (grpc-status-error (e)
          (setf last-error e)
          (unless (and (< (1+ attempt) max-attempts)
                       (retryable-error-p policy e))
            ;; Not retryable or last attempt - re-signal
            (error e))
          ;; Compute backoff and wait
          (let ((backoff (compute-backoff policy attempt)))
            (when on-retry
              (funcall on-retry attempt e backoff))
            (sleep backoff)))))
    ;; Should not reach here, but just in case
    (when last-error
      (error last-error))))

(defmacro with-retry ((policy &key on-retry) &body body)
  "Execute BODY with automatic retry according to POLICY.
ON-RETRY is an optional function called before each retry.

Example:
  (with-retry ((make-retry-policy :max-attempts 5))
    (call-unary channel method request))"
  `(call-with-retry (lambda () ,@body)
                    ,policy
                    :on-retry ,on-retry))

;;;; ========================================================================
;;;; Retry-Enabled Call Functions
;;;; ========================================================================

(defun call-unary-with-retry (channel method request &key metadata timeout response-type
                                                          (retry-policy nil))
  "Make a unary RPC call with automatic retry.
All parameters are the same as call-unary, plus:
RETRY-POLICY: A retry-policy object (or NIL to disable retry)

Returns a grpc-call object on success."
  (if retry-policy
      (call-with-retry
       (lambda ()
         (call-unary channel method request
                     :metadata metadata
                     :timeout timeout
                     :response-type response-type))
       retry-policy)
      (call-unary channel method request
                  :metadata metadata
                  :timeout timeout
                  :response-type response-type)))

;;;; ========================================================================
;;;; Channel-Level Retry Policy
;;;; ========================================================================

(defun channel-set-retry-policy (channel policy)
  "Set a default retry policy for the channel.
All calls made through this channel will use this policy unless overridden."
  ;; Store in channel metadata for now
  ;; This will be used by convenience wrappers
  (setf (getf (channel-metadata channel) :retry-policy) policy)
  channel)

(defun channel-get-retry-policy (channel)
  "Get the default retry policy for the channel, or NIL if none set."
  (getf (channel-metadata channel) :retry-policy))
