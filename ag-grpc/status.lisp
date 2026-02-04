;;;; status.lisp - gRPC status codes

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; Version
;;;; ========================================================================

(version-string:define-version-parameter +version+ :ag-grpc)

;;;; ========================================================================
;;;; Status Functions
;;;; ========================================================================

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

(defun http-status-to-grpc-status (http-status)
  "Map an HTTP status code to the corresponding gRPC status code.
Per gRPC spec: https://github.com/grpc/grpc/blob/master/doc/http-grpc-status-mapping.md
Only specific codes have defined mappings; all others map to UNKNOWN."
  (case http-status
    (400 +grpc-status-internal+)         ; Bad Request
    (401 +grpc-status-unauthenticated+)  ; Unauthorized
    (403 +grpc-status-permission-denied+) ; Forbidden
    (404 +grpc-status-unimplemented+)    ; Not Found
    (429 +grpc-status-unavailable+)      ; Too Many Requests
    (502 +grpc-status-unavailable+)      ; Bad Gateway
    (503 +grpc-status-unavailable+)      ; Service Unavailable
    (504 +grpc-status-unavailable+)      ; Gateway Timeout
    (t +grpc-status-unknown+)))

(defun rst-stream-error-to-grpc-status (error-code)
  "Map an HTTP/2 RST_STREAM error code to a gRPC status code.
Per gRPC spec, when RST_STREAM is received without grpc-status:
  - CANCEL (8) maps to CANCELLED
  - REFUSED_STREAM (7) maps to UNAVAILABLE
  - Other errors map to INTERNAL"
  (case error-code
    (#.ag-http2:+error-cancel+ +grpc-status-cancelled+)
    (#.ag-http2:+error-refused-stream+ +grpc-status-unavailable+)
    (#.ag-http2:+error-enhance-your-calm+ +grpc-status-resource-exhausted+)
    (#.ag-http2:+error-inadequate-security+ +grpc-status-permission-denied+)
    (t +grpc-status-internal+)))

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
