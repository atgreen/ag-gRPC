;;;; balancer.lisp - Load balancing and service discovery
;;;;
;;;; Provides:
;;;; - Load balancing policies (round-robin, pick-first)
;;;; - DNS-based service discovery
;;;; - Multi-endpoint channel management

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; Service Discovery - DNS Resolver
;;;; ========================================================================

(defclass dns-resolver ()
  ((refresh-interval :initarg :refresh-interval :accessor resolver-refresh-interval
                     :initform 30
                     :documentation "Seconds between DNS refresh")
   (last-refresh :initform 0 :accessor resolver-last-refresh
                 :documentation "Time of last refresh"))
  (:documentation "DNS-based service discovery resolver."))

(defun make-dns-resolver (&key (refresh-interval 30))
  "Create a DNS resolver for service discovery."
  (make-instance 'dns-resolver :refresh-interval refresh-interval))

(defun resolve-hostname (hostname)
  "Resolve a hostname to a list of IP addresses.
Returns a list of address strings."
  ;; Use usocket's host resolution
  (handler-case
      (let ((addr (usocket:get-host-by-name hostname)))
        (if (listp addr)
            (mapcar #'usocket:vector-quad-to-dotted-quad addr)
            (list (usocket:vector-quad-to-dotted-quad addr))))
    (error ()
      ;; If resolution fails, return the hostname itself
      ;; (it might be an IP address already)
      (list hostname))))

(defun resolver-get-addresses (resolver hostname)
  "Get addresses for hostname, refreshing if needed."
  (declare (ignore resolver))  ; Simple implementation for now
  (resolve-hostname hostname))

;;;; ========================================================================
;;;; Load Balancer Policies
;;;; ========================================================================

(defclass load-balancer ()
  ((endpoints :initarg :endpoints :accessor balancer-endpoints
              :initform nil
              :documentation "List of (host . port) endpoints")
   (channels :initform (make-hash-table :test 'equal) :accessor balancer-channels
             :documentation "Map of endpoint -> channel")
   (lock :initform (bt:make-lock "load-balancer") :accessor balancer-lock
         :documentation "Lock for thread-safe access")
   (tls :initarg :tls :accessor balancer-tls
        :initform nil
        :documentation "Use TLS for channels")
   (tls-verify :initarg :tls-verify :accessor balancer-tls-verify
               :initform nil
               :documentation "Verify TLS certificates")
   (default-timeout :initarg :default-timeout :accessor balancer-default-timeout
                    :initform 30
                    :documentation "Default timeout for channels"))
  (:documentation "Base class for load balancers."))

(defgeneric balancer-pick (balancer)
  (:documentation "Pick the next endpoint according to the load balancing policy.
Returns (values host port) or NIL if no endpoints available."))

(defgeneric balancer-report-success (balancer endpoint)
  (:documentation "Report a successful call to an endpoint.")
  (:method ((balancer load-balancer) endpoint)
    (declare (ignore endpoint))
    nil))

(defgeneric balancer-report-failure (balancer endpoint error)
  (:documentation "Report a failed call to an endpoint.")
  (:method ((balancer load-balancer) endpoint error)
    (declare (ignore endpoint error))
    nil))

;;; Pick-First Policy

(defclass pick-first-balancer (load-balancer)
  ((current-index :initform 0 :accessor balancer-current-index
                  :documentation "Index of current preferred endpoint"))
  (:documentation "Pick-first load balancer.
Always uses the first available endpoint, failing over to the next on error."))

(defun make-pick-first-balancer (endpoints &key tls tls-verify (timeout 30))
  "Create a pick-first load balancer.
ENDPOINTS: List of (host . port) cons cells"
  (make-instance 'pick-first-balancer
                 :endpoints endpoints
                 :tls tls
                 :tls-verify tls-verify
                 :default-timeout timeout))

(defmethod balancer-pick ((balancer pick-first-balancer))
  (bt:with-lock-held ((balancer-lock balancer))
    (let ((endpoints (balancer-endpoints balancer))
          (idx (balancer-current-index balancer)))
      (when endpoints
        (let ((endpoint (nth (mod idx (length endpoints)) endpoints)))
          (values (car endpoint) (cdr endpoint)))))))

(defmethod balancer-report-failure ((balancer pick-first-balancer) endpoint error)
  (declare (ignore error))
  (bt:with-lock-held ((balancer-lock balancer))
    (let ((endpoints (balancer-endpoints balancer)))
      ;; Move to next endpoint if current one failed
      (when (and endpoints
                 (equal endpoint (nth (balancer-current-index balancer) endpoints)))
        (incf (balancer-current-index balancer))))))

;;; Round-Robin Policy

(defclass round-robin-balancer (load-balancer)
  ((counter :initform 0 :accessor balancer-counter
            :documentation "Request counter for round-robin"))
  (:documentation "Round-robin load balancer.
Distributes requests evenly across all endpoints."))

(defun make-round-robin-balancer (endpoints &key tls tls-verify (timeout 30))
  "Create a round-robin load balancer.
ENDPOINTS: List of (host . port) cons cells"
  (make-instance 'round-robin-balancer
                 :endpoints endpoints
                 :tls tls
                 :tls-verify tls-verify
                 :default-timeout timeout))

(defmethod balancer-pick ((balancer round-robin-balancer))
  (bt:with-lock-held ((balancer-lock balancer))
    (let ((endpoints (balancer-endpoints balancer)))
      (when endpoints
        (let* ((idx (mod (balancer-counter balancer) (length endpoints)))
               (endpoint (nth idx endpoints)))
          (incf (balancer-counter balancer))
          (values (car endpoint) (cdr endpoint)))))))

;;;; ========================================================================
;;;; Load-Balanced Channel
;;;; ========================================================================

(defun balancer-get-channel (balancer)
  "Get a channel from the load balancer.
Creates a new channel if needed."
  (multiple-value-bind (host port)
      (balancer-pick balancer)
    (unless host
      (error 'grpc-status-error
             :code +grpc-status-unavailable+
             :message "No endpoints available"))
    (let ((key (cons host port)))
      (bt:with-lock-held ((balancer-lock balancer))
        (or (gethash key (balancer-channels balancer))
            (let ((channel (make-channel host port
                                         :tls (balancer-tls balancer)
                                         :tls-verify (balancer-tls-verify balancer)
                                         :timeout (balancer-default-timeout balancer))))
              (setf (gethash key (balancer-channels balancer)) channel)
              channel))))))

(defun balancer-close-all (balancer)
  "Close all channels in the load balancer."
  (bt:with-lock-held ((balancer-lock balancer))
    (maphash (lambda (key channel)
               (declare (ignore key))
               (handler-case
                   (channel-close channel)
                 (error () nil)))
             (balancer-channels balancer))
    (clrhash (balancer-channels balancer))))

(defun balancer-add-endpoint (balancer host port)
  "Add an endpoint to the load balancer."
  (bt:with-lock-held ((balancer-lock balancer))
    (pushnew (cons host port) (balancer-endpoints balancer) :test #'equal)))

(defun balancer-remove-endpoint (balancer host port)
  "Remove an endpoint from the load balancer."
  (let ((key (cons host port)))
    (bt:with-lock-held ((balancer-lock balancer))
      (setf (balancer-endpoints balancer)
            (remove key (balancer-endpoints balancer) :test #'equal))
      ;; Close and remove the channel for this endpoint
      (let ((channel (gethash key (balancer-channels balancer))))
        (when channel
          (handler-case (channel-close channel) (error () nil))
          (remhash key (balancer-channels balancer)))))))

(defmacro with-balanced-channel ((var balancer) &body body)
  "Execute BODY with VAR bound to a channel from BALANCER.
Reports success/failure back to the balancer.

Example:
  (with-balanced-channel (ch balancer)
    (call-unary ch method request))"
  (let ((balancer-var (gensym "BALANCER"))
        (endpoint-var (gensym "ENDPOINT"))
        (error-var (gensym "ERROR")))
    `(let* ((,balancer-var ,balancer)
            (,var (balancer-get-channel ,balancer-var))
            (,endpoint-var (cons (channel-host ,var) (channel-port ,var)))
            (,error-var nil))
       (handler-case
           (prog1
               (progn ,@body)
             (balancer-report-success ,balancer-var ,endpoint-var))
         (error (e)
           (setf ,error-var e)
           (balancer-report-failure ,balancer-var ,endpoint-var e)
           (error e))))))

;;;; ========================================================================
;;;; DNS-Aware Load Balancer
;;;; ========================================================================

(defclass dns-balancer (round-robin-balancer)
  ((hostname :initarg :hostname :accessor dns-balancer-hostname
             :documentation "DNS hostname to resolve")
   (port :initarg :port :accessor dns-balancer-port
         :documentation "Port number")
   (resolver :initarg :resolver :accessor dns-balancer-resolver
             :initform (make-dns-resolver)
             :documentation "DNS resolver"))
  (:documentation "Load balancer that discovers endpoints via DNS."))

(defun make-dns-balancer (hostname port &key tls tls-verify (timeout 30)
                                             (refresh-interval 30))
  "Create a DNS-based load balancer.
HOSTNAME: DNS name to resolve
PORT: Port number for all resolved addresses
REFRESH-INTERVAL: Seconds between DNS refreshes"
  (let ((balancer (make-instance 'dns-balancer
                                 :hostname hostname
                                 :port port
                                 :tls tls
                                 :tls-verify tls-verify
                                 :default-timeout timeout
                                 :resolver (make-dns-resolver
                                            :refresh-interval refresh-interval))))
    ;; Initial resolution
    (dns-balancer-refresh balancer)
    balancer))

(defun dns-balancer-refresh (balancer)
  "Refresh endpoints from DNS."
  (let* ((hostname (dns-balancer-hostname balancer))
         (port (dns-balancer-port balancer))
         (addresses (resolver-get-addresses (dns-balancer-resolver balancer) hostname)))
    (bt:with-lock-held ((balancer-lock balancer))
      (setf (balancer-endpoints balancer)
            (mapcar (lambda (addr) (cons addr port)) addresses)))))

(defmethod balancer-pick :before ((balancer dns-balancer))
  ;; Check if we should refresh DNS
  (let* ((resolver (dns-balancer-resolver balancer))
         (now (get-universal-time))
         (elapsed (- now (resolver-last-refresh resolver))))
    (when (>= elapsed (resolver-refresh-interval resolver))
      (dns-balancer-refresh balancer)
      (setf (resolver-last-refresh resolver) now))))
