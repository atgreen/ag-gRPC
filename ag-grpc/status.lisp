;;;; status.lisp - gRPC status codes

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; Version
;;;; ========================================================================

(version-string:define-version-parameter +version+ :ag-grpc)

;;;; ========================================================================
;;;; gRPC Status Codes
;;;; https://grpc.github.io/grpc/core/md_doc_statuscodes.html
;;;; ========================================================================

(defconstant +grpc-status-ok+ 0
  "Not an error; returned on success.")

(defconstant +grpc-status-cancelled+ 1
  "The operation was cancelled, typically by the caller.")

(defconstant +grpc-status-unknown+ 2
  "Unknown error. For example, this error may be returned when a Status
value received from another address space belongs to an error space that
is not known in this address space.")

(defconstant +grpc-status-invalid-argument+ 3
  "The client specified an invalid argument.")

(defconstant +grpc-status-deadline-exceeded+ 4
  "The deadline expired before the operation could complete.")

(defconstant +grpc-status-not-found+ 5
  "Some requested entity (e.g., file or directory) was not found.")

(defconstant +grpc-status-already-exists+ 6
  "The entity that a client attempted to create already exists.")

(defconstant +grpc-status-permission-denied+ 7
  "The caller does not have permission to execute the specified operation.")

(defconstant +grpc-status-resource-exhausted+ 8
  "Some resource has been exhausted, perhaps a per-user quota.")

(defconstant +grpc-status-failed-precondition+ 9
  "The operation was rejected because the system is not in a state
required for the operation's execution.")

(defconstant +grpc-status-aborted+ 10
  "The operation was aborted, typically due to a concurrency issue
such as a sequencer check failure or transaction abort.")

(defconstant +grpc-status-out-of-range+ 11
  "The operation was attempted past the valid range.")

(defconstant +grpc-status-unimplemented+ 12
  "The operation is not implemented or is not supported/enabled
in this service.")

(defconstant +grpc-status-internal+ 13
  "Internal errors. This means that some invariants expected by the
underlying system have been broken.")

(defconstant +grpc-status-unavailable+ 14
  "The service is currently unavailable. This is most likely a
transient condition.")

(defconstant +grpc-status-data-loss+ 15
  "Unrecoverable data loss or corruption.")

(defconstant +grpc-status-unauthenticated+ 16
  "The request does not have valid authentication credentials
for the operation.")

(defun grpc-status-name (code)
  "Return the string name for a gRPC status code"
  (case code
    (#.+grpc-status-ok+ "OK")
    (#.+grpc-status-cancelled+ "CANCELLED")
    (#.+grpc-status-unknown+ "UNKNOWN")
    (#.+grpc-status-invalid-argument+ "INVALID_ARGUMENT")
    (#.+grpc-status-deadline-exceeded+ "DEADLINE_EXCEEDED")
    (#.+grpc-status-not-found+ "NOT_FOUND")
    (#.+grpc-status-already-exists+ "ALREADY_EXISTS")
    (#.+grpc-status-permission-denied+ "PERMISSION_DENIED")
    (#.+grpc-status-resource-exhausted+ "RESOURCE_EXHAUSTED")
    (#.+grpc-status-failed-precondition+ "FAILED_PRECONDITION")
    (#.+grpc-status-aborted+ "ABORTED")
    (#.+grpc-status-out-of-range+ "OUT_OF_RANGE")
    (#.+grpc-status-unimplemented+ "UNIMPLEMENTED")
    (#.+grpc-status-internal+ "INTERNAL")
    (#.+grpc-status-unavailable+ "UNAVAILABLE")
    (#.+grpc-status-data-loss+ "DATA_LOSS")
    (#.+grpc-status-unauthenticated+ "UNAUTHENTICATED")
    (t (format nil "UNKNOWN_STATUS_~A" code))))

(defun grpc-status-ok-p (code)
  "Return T if the status code indicates success"
  (= code +grpc-status-ok+))

;;;; ========================================================================
;;;; Status Error Condition
;;;; ========================================================================

(define-condition grpc-status-error (error)
  ((code :initarg :code :reader grpc-status-error-code
         :documentation "gRPC status code")
   (message :initarg :message :reader grpc-status-error-message
            :initform nil
            :documentation "Error message")
   (details :initarg :details :reader grpc-status-error-details
            :initform nil
            :documentation "Additional error details")
   (headers :initarg :headers :reader grpc-status-error-headers
            :initform nil
            :documentation "Response headers")
   (trailers :initarg :trailers :reader grpc-status-error-trailers
             :initform nil
             :documentation "Response trailers"))
  (:report (lambda (c s)
             (format s "gRPC error ~A (~A)~@[: ~A~]"
                     (grpc-status-error-code c)
                     (grpc-status-name (grpc-status-error-code c))
                     (grpc-status-error-message c)))))

(define-condition grpc-error (error)
  ((message :initarg :message :reader grpc-error-message))
  (:report (lambda (c s)
             (format s "gRPC error: ~A" (grpc-error-message c)))))
