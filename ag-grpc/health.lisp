;;;; health.lisp - gRPC Health Checking Protocol implementation
;;;;
;;;; Implements the standard grpc.health.v1.Health service for
;;;; load balancer integration and service health monitoring.
;;;; See: https://github.com/grpc/grpc/blob/master/doc/health-checking.md

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; Health Check Protocol Messages (from health.proto)
;;;; ========================================================================

(defclass health-check-request (ag-proto:proto-message)
  ((service :initarg :service :accessor health-request-service :initform "" :type string))
  (:documentation "Health check request message. Empty service name means overall server health."))

(defmethod ag-proto:serialize-to-bytes ((obj health-check-request))
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (let ((value (slot-value obj 'service)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 1 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    buffer))

(defmethod ag-proto:deserialize-from-bytes ((type (eql 'health-check-request)) data)
  (let ((obj (make-instance 'health-check-request))
        (buffer (cons data 0)))
    (loop while (< (cdr buffer) (length data))
          do (let* ((tag (ag-proto::read-varint buffer))
                    (field-number (ash tag -3))
                    (wire-type (logand tag 7)))
               (case field-number
                 (1 (setf (slot-value obj 'service)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (otherwise (ag-proto::skip-field buffer wire-type)))))
    obj))

;;; Serving status enum
(defconstant +health-unknown+ 0 "Unknown health status")
(defconstant +health-serving+ 1 "Service is healthy and serving")
(defconstant +health-not-serving+ 2 "Service is not serving")
(defconstant +health-service-unknown+ 3 "Requested service is not known")

(defclass health-check-response (ag-proto:proto-message)
  ((status :initarg :status :accessor health-response-status :initform +health-unknown+ :type integer))
  (:documentation "Health check response message with serving status."))

(defmethod ag-proto:serialize-to-bytes ((obj health-check-response))
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (let ((value (slot-value obj 'status)))
      (when (and value (not (eql value 0)))
        (ag-proto::write-field-tag 1 0 buffer)
        (ag-proto::write-varint value buffer)))
    buffer))

(defmethod ag-proto:deserialize-from-bytes ((type (eql 'health-check-response)) data)
  (let ((obj (make-instance 'health-check-response))
        (buffer (cons data 0)))
    (loop while (< (cdr buffer) (length data))
          do (let* ((tag (ag-proto::read-varint buffer))
                    (field-number (ash tag -3))
                    (wire-type (logand tag 7)))
               (case field-number
                 (1 (setf (slot-value obj 'status) (ag-proto::read-varint buffer)))
                 (otherwise (ag-proto::skip-field buffer wire-type)))))
    obj))

;;;; ========================================================================
;;;; Health Service Implementation
;;;; ========================================================================

(defclass health-service ()
  ((status-map :initform (make-hash-table :test 'equal)
               :accessor health-status-map
               :documentation "Map of service name to serving status")
   (watchers :initform nil
             :accessor health-watchers
             :documentation "List of (service-name . stream) for active Watch calls")
   (lock :initform (bt:make-lock "health-service")
         :accessor health-lock
         :documentation "Lock for thread-safe access"))
  (:documentation "Health checking service implementation.
Use with server-enable-health-checking to add to a gRPC server."))

(defun make-health-service ()
  "Create a new health service instance."
  (make-instance 'health-service))

(defun health-set-status (health-service service-name status)
  "Set the serving status for a service.
SERVICE-NAME: String name of the service (empty string for overall server health)
STATUS: One of +health-serving+, +health-not-serving+, +health-unknown+"
  (bt:with-lock-held ((health-lock health-service))
    (setf (gethash service-name (health-status-map health-service)) status)
    ;; Notify watchers
    (dolist (watcher (health-watchers health-service))
      (when (equal (car watcher) service-name)
        (let ((stream (cdr watcher)))
          ;; Try to send update, remove watcher if stream is closed
          (handler-case
              (stream-send stream (make-instance 'health-check-response :status status))
            (error () nil)))))))

(defun health-get-status (health-service service-name)
  "Get the serving status for a service.
Returns +health-service-unknown+ if service is not registered."
  (bt:with-lock-held ((health-lock health-service))
    (gethash service-name (health-status-map health-service) +health-service-unknown+)))

(defun health-clear-status (health-service service-name)
  "Remove the status for a service."
  (bt:with-lock-held ((health-lock health-service))
    (remhash service-name (health-status-map health-service))))

;;;; ========================================================================
;;;; Health Service Handlers
;;;; ========================================================================

(defun make-health-check-handler (health-service)
  "Create the Check RPC handler for this health service."
  (lambda (request ctx)
    (declare (ignore ctx))
    (let* ((service-name (health-request-service request))
           (status (health-get-status health-service service-name)))
      (make-instance 'health-check-response :status status))))

(defun make-health-watch-handler (health-service)
  "Create the Watch RPC handler for this health service."
  (lambda (request ctx stream)
    (let* ((service-name (health-request-service request))
           (current-status (health-get-status health-service service-name))
           (watcher (cons service-name stream)))
      ;; Send immediate response with current status
      (stream-send stream (make-instance 'health-check-response :status current-status))
      ;; Register as watcher for future updates
      (bt:with-lock-held ((health-lock health-service))
        (push watcher (health-watchers health-service)))
      ;; Keep stream open until client cancels or server shuts down
      ;; The handler blocks here; the stream is managed by the caller
      (unwind-protect
           (loop
             (sleep 1)
             (when (context-check-cancelled ctx)
               (return)))
        ;; Remove watcher on exit
        (bt:with-lock-held ((health-lock health-service))
          (setf (health-watchers health-service)
                (remove watcher (health-watchers health-service) :test #'eq)))))))

;;;; ========================================================================
;;;; Server Integration
;;;; ========================================================================

(defun server-enable-health-checking (server &key (initial-status +health-serving+))
  "Enable the standard gRPC health checking service on this server.
Creates a health-service and registers the Check and Watch handlers.
Returns the health-service instance for status management.

INITIAL-STATUS: Initial status for the empty service name (server health).
                Defaults to +health-serving+."
  (let ((health (make-health-service)))
    ;; Set initial server status
    (health-set-status health "" initial-status)
    ;; Register handlers
    (server-register-handler server
                             "/grpc.health.v1.Health/Check"
                             (make-health-check-handler health)
                             :unary
                             'health-check-request)
    (server-register-handler server
                             "/grpc.health.v1.Health/Watch"
                             (make-health-watch-handler health)
                             :server-streaming
                             'health-check-request)
    health))
