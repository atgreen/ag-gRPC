;;;; hedge.lisp - Hedged requests for latency reduction
;;;;
;;;; Send the same request to multiple backends simultaneously,
;;;; use the first successful response, cancel the rest.

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; Hedged Request Configuration
;;;; ========================================================================

(defclass hedge-policy ()
  ((max-attempts :initarg :max-attempts :accessor hedge-max-attempts
                 :initform 2
                 :documentation "Maximum number of hedged requests")
   (delay :initarg :delay :accessor hedge-delay
          :initform 0.1
          :documentation "Seconds to wait before sending next hedge")
   (non-fatal-codes :initarg :non-fatal-codes :accessor hedge-non-fatal-codes
                    :initform (list +grpc-status-unavailable+
                                    +grpc-status-resource-exhausted+)
                    :documentation "Status codes that allow hedging to continue"))
  (:documentation "Configuration for hedged request behavior."))

(defun make-hedge-policy (&key (max-attempts 2) (delay 0.1) non-fatal-codes)
  "Create a hedge policy.
MAX-ATTEMPTS: Maximum concurrent requests (default 2)
DELAY: Seconds to wait before each subsequent hedge (default 0.1)
NON-FATAL-CODES: Codes that don't stop hedging (default: UNAVAILABLE, RESOURCE_EXHAUSTED)"
  (make-instance 'hedge-policy
                 :max-attempts max-attempts
                 :delay delay
                 :non-fatal-codes (or non-fatal-codes
                                      (list +grpc-status-unavailable+
                                            +grpc-status-resource-exhausted+))))

;;;; ========================================================================
;;;; Hedged Call Implementation
;;;; ========================================================================

(defun hedge-fatal-error-p (policy error)
  "Check if an error is fatal and should stop hedging."
  (and (typep error 'grpc-status-error)
       (not (member (grpc-status-error-code error)
                    (hedge-non-fatal-codes policy)))))

(defun call-with-hedging (policy channels-or-balancer method request
                          &key metadata timeout response-type)
  "Execute a hedged unary call across multiple channels.
POLICY: Hedge policy controlling behavior
CHANNELS-OR-BALANCER: List of channels or a load balancer
METHOD: RPC method path
REQUEST: Request message
Returns the first successful response.
Signals the last error if all attempts fail."
  (let* ((channels (etypecase channels-or-balancer
                     (list channels-or-balancer)
                     (load-balancer
                      ;; Get multiple channels from balancer
                      (loop repeat (hedge-max-attempts policy)
                            collect (balancer-get-channel channels-or-balancer)))))
         (num-channels (min (length channels) (hedge-max-attempts policy)))
         (result-lock (bt:make-lock "hedge-result"))
         (result nil)
         (result-ready nil)
         (errors nil)
         (threads nil))
    ;; Launch hedged requests
    (dotimes (i num-channels)
      (let ((channel (nth i channels))
            (attempt i))
        ;; Wait before sending hedge (except first)
        (when (plusp attempt)
          (sleep (* attempt (hedge-delay policy))))
        ;; Check if we already have a result
        (bt:with-lock-held (result-lock)
          (when result-ready
            (return)))
        ;; Launch request in thread
        (push
         (bt:make-thread
          (lambda ()
            (handler-case
                (let ((call (call-unary channel method request
                                        :metadata metadata
                                        :timeout timeout
                                        :response-type response-type)))
                  ;; Got a successful response
                  (bt:with-lock-held (result-lock)
                    (unless result-ready
                      (setf result call)
                      (setf result-ready t))))
              (grpc-status-error (e)
                (bt:with-lock-held (result-lock)
                  (push e errors)
                  ;; If fatal error, stop hedging
                  (when (hedge-fatal-error-p policy e)
                    (setf result-ready t))))
              (error (e)
                (bt:with-lock-held (result-lock)
                  (push e errors)))))
          :name (format nil "hedge-~A-~A" method attempt))
         threads)))
    ;; Wait for all threads or until we have a result
    (dolist (thread (reverse threads))
      (bt:with-lock-held (result-lock)
        (when result-ready
          (return)))
      (bt:join-thread thread))
    ;; Return result or signal error
    (cond
      (result result)
      (errors (error (car errors)))
      (t (error 'grpc-status-error
                :code +grpc-status-unavailable+
                :message "All hedged requests failed")))))

(defmacro with-hedging ((policy channels-or-balancer) &body call-form)
  "Execute a hedged call.

Example:
  (with-hedging ((make-hedge-policy :max-attempts 3) channels)
    (call-unary _ method request :response-type 'response))

The _ placeholder will be replaced with each channel."
  (let ((policy-var (gensym "POLICY"))
        (channels-var (gensym "CHANNELS")))
    `(let ((,policy-var ,policy)
           (,channels-var ,channels-or-balancer))
       ;; Extract call parameters from the form
       (call-with-hedging ,policy-var ,channels-var
                          ,@(cdr call-form)))))

;;;; ========================================================================
;;;; Simplified Hedged Call
;;;; ========================================================================

(defun call-unary-hedged (channels method request &key metadata timeout response-type
                                                       (max-attempts 2) (delay 0.1))
  "Make a hedged unary call across multiple channels.
CHANNELS: List of channels to use
MAX-ATTEMPTS: Maximum concurrent requests (default 2)
DELAY: Seconds between hedge attempts (default 0.1)
Other parameters same as call-unary."
  (call-with-hedging (make-hedge-policy :max-attempts max-attempts :delay delay)
                     channels method request
                     :metadata metadata
                     :timeout timeout
                     :response-type response-type))
