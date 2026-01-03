;;;; circuit-breaker.lisp - Circuit breaker pattern for fault tolerance
;;;;
;;;; Prevents cascade failures by detecting repeated failures and
;;;; temporarily stopping requests to unhealthy services.

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; Circuit Breaker States
;;;; ========================================================================

(deftype circuit-state ()
  "Possible states for a circuit breaker."
  '(member :closed :open :half-open))

;;;; ========================================================================
;;;; Circuit Breaker Configuration
;;;; ========================================================================

(defclass circuit-breaker ()
  ((name :initarg :name :accessor breaker-name
         :initform "default"
         :documentation "Name for logging/identification")
   (state :initform :closed :accessor breaker-state
          :documentation "Current state: :closed, :open, or :half-open")
   (failure-threshold :initarg :failure-threshold :accessor breaker-failure-threshold
                      :initform 5
                      :documentation "Number of failures before opening")
   (success-threshold :initarg :success-threshold :accessor breaker-success-threshold
                      :initform 2
                      :documentation "Successes needed in half-open to close")
   (timeout :initarg :timeout :accessor breaker-timeout
            :initform 30
            :documentation "Seconds to wait before trying half-open")
   (failure-count :initform 0 :accessor breaker-failure-count
                  :documentation "Current consecutive failure count")
   (success-count :initform 0 :accessor breaker-success-count
                  :documentation "Current consecutive success count in half-open")
   (last-failure-time :initform 0 :accessor breaker-last-failure-time
                      :documentation "Time of last failure (for timeout)")
   (lock :initform (bt:make-lock "circuit-breaker") :accessor breaker-lock
         :documentation "Lock for thread-safe state changes")
   (on-state-change :initarg :on-state-change :accessor breaker-on-state-change
                    :initform nil
                    :documentation "Callback (old-state new-state) on state changes")
   (tripped-error-codes :initarg :tripped-error-codes :accessor breaker-tripped-error-codes
                        :initform nil
                        :documentation "gRPC status codes that count as failures (nil = all errors)"))
  (:documentation "Circuit breaker for protecting against cascade failures."))

(defun make-circuit-breaker (&key (name "default")
                                   (failure-threshold 5)
                                   (success-threshold 2)
                                   (timeout 30)
                                   on-state-change
                                   tripped-error-codes)
  "Create a circuit breaker.
NAME: Identifier for logging
FAILURE-THRESHOLD: Failures before opening (default 5)
SUCCESS-THRESHOLD: Successes in half-open to close (default 2)
TIMEOUT: Seconds before trying half-open (default 30)
ON-STATE-CHANGE: Callback function (old-state new-state)
TRIPPED-ERROR-CODES: List of gRPC codes that count as failures (nil = all)"
  (make-instance 'circuit-breaker
                 :name name
                 :failure-threshold failure-threshold
                 :success-threshold success-threshold
                 :timeout timeout
                 :on-state-change on-state-change
                 :tripped-error-codes tripped-error-codes))

;;;; ========================================================================
;;;; State Transitions
;;;; ========================================================================

(defun breaker-change-state (breaker new-state)
  "Change breaker state and invoke callback if present."
  (let ((old-state (breaker-state breaker)))
    (unless (eq old-state new-state)
      (setf (breaker-state breaker) new-state)
      (when (breaker-on-state-change breaker)
        (funcall (breaker-on-state-change breaker) old-state new-state)))))

(defun breaker-should-trip-p (breaker error)
  "Check if this error should count toward tripping the breaker."
  (and (typep error 'grpc-status-error)
       (let ((codes (breaker-tripped-error-codes breaker)))
         (or (null codes)  ; nil means all errors count
             (member (grpc-status-error-code error) codes)))))

(defun breaker-record-success (breaker)
  "Record a successful call."
  (bt:with-lock-held ((breaker-lock breaker))
    (case (breaker-state breaker)
      (:closed
       ;; Reset failure count on success
       (setf (breaker-failure-count breaker) 0))
      (:half-open
       ;; Count successes toward closing
       (incf (breaker-success-count breaker))
       (when (>= (breaker-success-count breaker)
                 (breaker-success-threshold breaker))
         ;; Enough successes, close the circuit
         (setf (breaker-failure-count breaker) 0)
         (setf (breaker-success-count breaker) 0)
         (breaker-change-state breaker :closed))))))

(defun breaker-record-failure (breaker error)
  "Record a failed call."
  (when (breaker-should-trip-p breaker error)
    (bt:with-lock-held ((breaker-lock breaker))
      (case (breaker-state breaker)
        (:closed
         (incf (breaker-failure-count breaker))
         (setf (breaker-last-failure-time breaker) (get-universal-time))
         (when (>= (breaker-failure-count breaker)
                   (breaker-failure-threshold breaker))
           ;; Too many failures, open the circuit
           (breaker-change-state breaker :open)))
        (:half-open
         ;; Any failure in half-open goes back to open
         (setf (breaker-success-count breaker) 0)
         (setf (breaker-last-failure-time breaker) (get-universal-time))
         (breaker-change-state breaker :open))))))

(defun breaker-allow-request-p (breaker)
  "Check if a request should be allowed through.
Returns T if allowed, NIL if circuit is open."
  (bt:with-lock-held ((breaker-lock breaker))
    (case (breaker-state breaker)
      (:closed t)
      (:open
       ;; Check if timeout has passed
       (let ((elapsed (- (get-universal-time) (breaker-last-failure-time breaker))))
         (if (>= elapsed (breaker-timeout breaker))
             (progn
               ;; Transition to half-open, allow one request
               (breaker-change-state breaker :half-open)
               (setf (breaker-success-count breaker) 0)
               t)
             nil)))
      (:half-open t))))  ; Allow requests in half-open to test

;;;; ========================================================================
;;;; Circuit Breaker Error
;;;; ========================================================================

(define-condition circuit-open-error (grpc-status-error)
  ((breaker-name :initarg :breaker-name :reader circuit-open-breaker-name))
  (:documentation "Signaled when circuit breaker is open.")
  (:default-initargs
   :code +grpc-status-unavailable+
   :message "Circuit breaker is open"))

;;;; ========================================================================
;;;; Call Wrapper
;;;; ========================================================================

(defun call-with-circuit-breaker (breaker thunk)
  "Execute THUNK with circuit breaker protection.
If circuit is open, signals circuit-open-error.
Records success/failure and updates breaker state."
  (unless (breaker-allow-request-p breaker)
    (error 'circuit-open-error
           :breaker-name (breaker-name breaker)))
  (handler-case
      (prog1
          (funcall thunk)
        (breaker-record-success breaker))
    (grpc-status-error (e)
      (breaker-record-failure breaker e)
      (error e))
    (error (e)
      ;; Non-gRPC errors also count as failures
      (breaker-record-failure breaker
                              (make-condition 'grpc-status-error
                                              :code +grpc-status-unknown+
                                              :message (princ-to-string e)))
      (error e))))

(defmacro with-circuit-breaker ((breaker) &body body)
  "Execute BODY with circuit breaker protection.

Example:
  (with-circuit-breaker (my-breaker)
    (call-unary channel method request))"
  `(call-with-circuit-breaker ,breaker (lambda () ,@body)))

;;;; ========================================================================
;;;; Breaker Statistics
;;;; ========================================================================

(defun breaker-stats (breaker)
  "Get current circuit breaker statistics.
Returns (values state failure-count success-count time-until-retry)."
  (bt:with-lock-held ((breaker-lock breaker))
    (let ((time-until-retry
            (if (eq (breaker-state breaker) :open)
                (max 0 (- (breaker-timeout breaker)
                          (- (get-universal-time)
                             (breaker-last-failure-time breaker))))
                0)))
      (values (breaker-state breaker)
              (breaker-failure-count breaker)
              (breaker-success-count breaker)
              time-until-retry))))

(defun breaker-reset (breaker)
  "Manually reset the circuit breaker to closed state."
  (bt:with-lock-held ((breaker-lock breaker))
    (setf (breaker-failure-count breaker) 0)
    (setf (breaker-success-count breaker) 0)
    (breaker-change-state breaker :closed)))

(defun breaker-force-open (breaker)
  "Manually force the circuit breaker to open state."
  (bt:with-lock-held ((breaker-lock breaker))
    (setf (breaker-last-failure-time breaker) (get-universal-time))
    (breaker-change-state breaker :open)))
