;;;; context-values.lisp - cl-context integration for request-scoped values

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; Context Keys
;;;;
;;;; These define standard context keys for common request-scoped values.
;;;; Context values are immutable and propagate automatically through the
;;;; call stack via cl-context:*current-context*.
;;;; ========================================================================

(cl-context:define-context-key +grpc-request-id+
  "Unique request ID for tracing")

(cl-context:define-context-key +grpc-trace-context+
  "Distributed tracing context (trace-id, span-id)")

(cl-context:define-context-key +grpc-auth-token+
  "Authentication token from metadata")

(cl-context:define-context-key +grpc-peer-address+
  "Remote peer address")

;;;; ========================================================================
;;;; Context Value Accessors
;;;; ========================================================================

(defun grpc-context-value (key &optional default)
  "Get a value from the current gRPC context.

  Accesses cl-context:*current-context*. Outside of server handlers
  or client calls, this will use the background context.

  See also: context-metadata (for accessing request headers)"
  (cl-context:value cl-context:*current-context* key default))

(defun enrich-context-from-metadata (ctx headers peer-address)
  "Add request-scoped values to context from headers.

  Returns a new context with values added. Use this when creating
  contexts for incoming requests."
  (let ((enriched ctx))
    ;; Add peer address
    (setf enriched (cl-context:with-value enriched +grpc-peer-address+
                                          peer-address))

    ;; Extract trace context from headers (if present)
    (let ((trace-id (cdr (assoc "x-trace-id" headers :test #'string-equal))))
      (when trace-id
        (setf enriched (cl-context:with-value enriched +grpc-trace-context+
                                              trace-id))))

    ;; Extract auth token (if present)
    (let ((auth (cdr (assoc "authorization" headers :test #'string-equal))))
      (when auth
        (setf enriched (cl-context:with-value enriched +grpc-auth-token+
                                              auth))))

    enriched))
