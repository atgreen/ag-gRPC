;;;; context-values.lisp - Context values stubs (cl-cancel doesn't support context values)

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; Context Values - DEPRECATED
;;;;
;;;; cl-cancel does not support context values. The Common Lisp idiom
;;;; is to use dynamic variables (*special-variables*) for request-scoped
;;;; data instead of Go-style context values.
;;;;
;;;; These stubs are provided for backward compatibility but do nothing.
;;;; If you need request-scoped values, use dynamic variables:
;;;;
;;;;   (defvar *grpc-request-id* nil)
;;;;   (defvar *grpc-trace-context* nil)
;;;;   (defvar *grpc-auth-token* nil)
;;;;   (defvar *grpc-peer-address* nil)
;;;;
;;;; Then bind them in your handlers:
;;;;
;;;;   (let ((*grpc-request-id* (generate-request-id))
;;;;         (*grpc-peer-address* peer-addr))
;;;;     ...)
;;;;
;;;; ========================================================================

;; Stub macro - does nothing
(defmacro define-context-key (name &optional documentation)
  "DEPRECATED: cl-cancel doesn't support context values. Use dynamic variables instead."
  (declare (ignore documentation))
  `(defconstant ,name nil))

;; Define stub context keys
(define-context-key +grpc-request-id+
  "Unique request ID for tracing (DEPRECATED - use dynamic variables)")

(define-context-key +grpc-trace-context+
  "Distributed tracing context (DEPRECATED - use dynamic variables)")

(define-context-key +grpc-auth-token+
  "Authentication token (DEPRECATED - use dynamic variables)")

(define-context-key +grpc-peer-address+
  "Remote peer address (DEPRECATED - use dynamic variables)")

;;;; ========================================================================
;;;; Context Value Accessors (Stubs)
;;;; ========================================================================

(defun grpc-context-value (key &optional default)
  "DEPRECATED: cl-cancel doesn't support context values. Use dynamic variables instead.

  Always returns DEFAULT."
  (declare (ignore key))
  default)

(defun enrich-context-from-metadata (ctx headers peer-address)
  "DEPRECATED: cl-cancel doesn't support context values.

  Returns CTX unchanged. To store request metadata, use dynamic variables."
  (declare (ignore headers peer-address))
  ctx)
