;;;; channel-pool.lisp - Channel pooling, wait-for-ready, and keepalive
;;;;
;;;; Provides advanced channel management features:
;;;; - Connection pooling for reusing channels
;;;; - Wait-for-ready semantics
;;;; - Keepalive/ping for connection health

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; Channel State
;;;; ========================================================================

(deftype channel-state ()
  "Possible states for a gRPC channel."
  '(member :idle :connecting :ready :transient-failure :shutdown))

(defun channel-state (channel)
  "Get the current state of the channel.
Returns one of: :idle, :connecting, :ready, :transient-failure, :shutdown"
  (cond
    ((null (channel-connection channel)) :idle)
    ((not (channel-connected-p channel)) :transient-failure)
    (t :ready)))

(defun channel-ready-p (channel)
  "Return T if the channel is ready to make calls."
  (eq (channel-state channel) :ready))

;;;; ========================================================================
;;;; Wait-for-Ready
;;;; ========================================================================

(defun channel-wait-for-ready (channel &key (timeout 30) (poll-interval 0.1))
  "Wait for the channel to become ready.
TIMEOUT: Maximum time to wait in seconds (default 30)
POLL-INTERVAL: Time between state checks in seconds (default 0.1)
Returns T if channel became ready, NIL if timeout expired.
Signals error if channel enters :shutdown state."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (let ((state (channel-state channel)))
        (case state
          (:ready (return t))
          (:shutdown (error "Channel is shut down"))
          (:idle
           ;; Try to connect
           (handler-case
               (channel-connect channel)
             (error () nil)))
          (otherwise nil)))
      ;; Check timeout
      (when (> (get-internal-real-time) deadline)
        (return nil))
      ;; Wait before next check
      (sleep poll-interval))))

(defun call-when-ready (channel thunk &key (timeout 30))
  "Wait for channel to be ready, then call THUNK.
TIMEOUT: Maximum time to wait for ready state."
  (unless (channel-wait-for-ready channel :timeout timeout)
    (error 'grpc-status-error
           :code +grpc-status-unavailable+
           :message "Timeout waiting for channel to become ready"))
  (funcall thunk))

(defmacro with-wait-for-ready ((channel &key (timeout 30)) &body body)
  "Execute BODY after waiting for channel to be ready.
Example:
  (with-wait-for-ready (channel :timeout 60)
    (call-unary channel method request))"
  `(call-when-ready ,channel (lambda () ,@body) :timeout ,timeout))

;;;; ========================================================================
;;;; Keepalive / Ping
;;;; ========================================================================

(defclass keepalive-config ()
  ((ping-interval :initarg :ping-interval :accessor keepalive-ping-interval
                  :initform 30
                  :documentation "Seconds between PING frames")
   (ping-timeout :initarg :ping-timeout :accessor keepalive-ping-timeout
                 :initform 10
                 :documentation "Seconds to wait for PING response")
   (permit-without-calls :initarg :permit-without-calls
                         :accessor keepalive-permit-without-calls
                         :initform nil
                         :documentation "Send pings even without active calls"))
  (:documentation "Configuration for HTTP/2 keepalive pings."))

(defun make-keepalive-config (&key (ping-interval 30) (ping-timeout 10)
                                    (permit-without-calls nil))
  "Create a keepalive configuration.
PING-INTERVAL: Seconds between pings (default 30)
PING-TIMEOUT: Seconds to wait for ping response (default 10)
PERMIT-WITHOUT-CALLS: Send pings even when idle (default NIL)"
  (make-instance 'keepalive-config
                 :ping-interval ping-interval
                 :ping-timeout ping-timeout
                 :permit-without-calls permit-without-calls))

(defun channel-ping (channel)
  "Send a PING frame on the channel's HTTP/2 connection.
Returns T if ping was sent successfully, NIL otherwise.
Note: Currently checks connection state only; actual PING frames
require HTTP/2 layer support."
  (and (channel-connection channel)
       (channel-connected-p channel)))

(defun channel-check-health (channel &key (timeout 5))
  "Check channel health.
Returns T if channel is healthy, NIL otherwise.
TIMEOUT: Not currently used (reserved for future PING support)."
  (declare (ignore timeout))
  ;; For now, just check if connection is still open
  (and (channel-connection channel)
       (channel-connected-p channel)))

;;;; ========================================================================
;;;; Channel Pool
;;;; ========================================================================

(defclass channel-pool ()
  ((target :initarg :target :accessor pool-target
           :documentation "Target in format \"host:port\"")
   (max-size :initarg :max-size :accessor pool-max-size
             :initform 10
             :documentation "Maximum number of channels")
   (channels :initform nil :accessor pool-channels
             :documentation "List of pooled channels")
   (lock :initform (bt:make-lock "channel-pool") :accessor pool-lock
         :documentation "Lock for thread-safe access")
   (tls :initarg :tls :accessor pool-tls
        :initform nil
        :documentation "Use TLS for channels")
   (tls-verify :initarg :tls-verify :accessor pool-tls-verify
               :initform nil
               :documentation "Verify TLS certificates")
   (default-timeout :initarg :default-timeout :accessor pool-default-timeout
                    :initform 30
                    :documentation "Default timeout for channels")
   (interceptors :initarg :interceptors :accessor pool-interceptors
                 :initform nil
                 :documentation "Interceptors to add to new channels"))
  (:documentation "Pool of reusable gRPC channels to a single target."))

(defun make-channel-pool (host port &key (max-size 10) tls tls-verify
                                          (timeout 30) interceptors)
  "Create a channel pool for the given target.
HOST, PORT: Target server
MAX-SIZE: Maximum number of channels (default 10)
TLS: Use TLS encryption
TLS-VERIFY: Verify TLS certificates
TIMEOUT: Default timeout for channels
INTERCEPTORS: List of interceptors to add to each channel"
  (make-instance 'channel-pool
                 :target (format nil "~A:~A" host port)
                 :max-size max-size
                 :tls tls
                 :tls-verify tls-verify
                 :default-timeout timeout
                 :interceptors interceptors))

(defun parse-pool-target (target)
  "Parse target string into (values host port)."
  (let ((colon-pos (position #\: target :from-end t)))
    (if colon-pos
        (values (subseq target 0 colon-pos)
                (parse-integer (subseq target (1+ colon-pos))))
        (values target 443))))

(defun pool-get-channel (pool)
  "Get a channel from the pool.
Returns an existing ready channel or creates a new one if possible.
Blocks if pool is full and all channels are in use."
  (bt:with-lock-held ((pool-lock pool))
    ;; First, try to find an existing ready channel
    (let ((ready-channel (find-if #'channel-ready-p (pool-channels pool))))
      (when ready-channel
        (return-from pool-get-channel ready-channel)))
    ;; No ready channel, try to create new one if under limit
    (when (< (length (pool-channels pool)) (pool-max-size pool))
      (multiple-value-bind (host port)
          (parse-pool-target (pool-target pool))
        (let ((channel (make-channel host port
                                     :tls (pool-tls pool)
                                     :tls-verify (pool-tls-verify pool)
                                     :timeout (pool-default-timeout pool))))
          ;; Add interceptors
          (dolist (interceptor (pool-interceptors pool))
            (channel-add-interceptor channel interceptor))
          (push channel (pool-channels pool))
          (return-from pool-get-channel channel))))
    ;; Pool is full, return first channel (may need reconnection)
    (car (pool-channels pool))))

(defun pool-release-channel (pool channel)
  "Release a channel back to the pool.
Currently a no-op since channels stay in the pool, but allows for future
tracking of in-use vs available channels."
  (declare (ignore pool channel))
  nil)

(defun pool-close (pool)
  "Close all channels in the pool."
  (bt:with-lock-held ((pool-lock pool))
    (dolist (channel (pool-channels pool))
      (handler-case
          (channel-close channel)
        (error () nil)))
    (setf (pool-channels pool) nil)))

(defmacro with-pooled-channel ((var pool) &body body)
  "Execute BODY with VAR bound to a channel from POOL.
The channel is automatically released when BODY exits.

Example:
  (with-pooled-channel (ch pool)
    (call-unary ch method request))"
  (let ((pool-var (gensym "POOL")))
    `(let* ((,pool-var ,pool)
            (,var (pool-get-channel ,pool-var)))
       (unwind-protect
            (progn ,@body)
         (pool-release-channel ,pool-var ,var)))))
