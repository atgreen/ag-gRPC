;;;; tls.lisp - Optional TLS support for ag-http2
;;;;
;;;; This module provides optional TLS/SSL support using cl+ssl.
;;;; If cl+ssl is not available, TLS connections will signal an error.

(in-package #:ag-http2)

;;;; ========================================================================
;;;; TLS Availability Detection
;;;; ========================================================================

(defvar *tls-available* nil
  "T if cl+ssl is available and loaded")

(defun tls-available-p ()
  "Check if TLS support is available"
  *tls-available*)

(defun ensure-tls-available ()
  "Signal an error if TLS is not available"
  (unless *tls-available*
    (error "TLS requested but cl+ssl is not available. ~
            Install cl+ssl and OpenSSL to enable TLS support.")))

;;;; ========================================================================
;;;; TLS Stream Wrapping
;;;; ========================================================================

(defvar *make-ssl-client-stream* nil
  "Function to wrap a stream with TLS (set when cl+ssl is loaded)")

(defun wrap-stream-with-tls (stream hostname &key verify)
  "Wrap a socket stream with TLS encryption.
   HOSTNAME is used for SNI (Server Name Indication).
   VERIFY controls certificate verification (default nil for now)."
  (ensure-tls-available)
  (funcall *make-ssl-client-stream* stream
           :hostname hostname
           :verify verify))

;;;; ========================================================================
;;;; TLS Initialization
;;;; ========================================================================

(defun try-load-tls ()
  "Attempt to load cl+ssl if available"
  (handler-case
      (progn
        (asdf:load-system :cl+ssl :verbose nil)
        (setf *tls-available* t)
        (setf *make-ssl-client-stream*
              (fdefinition (find-symbol "MAKE-SSL-CLIENT-STREAM" :cl+ssl)))
        t)
    (error ()
      (setf *tls-available* nil)
      nil)))

;; Try to load TLS support at load time, but don't fail if unavailable
(ignore-errors (try-load-tls))
