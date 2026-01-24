;;;; tls.lisp - Optional TLS support for ag-http2
;;;;
;;;; This module provides optional TLS/SSL support using pure-tls.
;;;; If pure-tls is not available, TLS connections will signal an error.

(in-package #:ag-http2)

;;;; ========================================================================
;;;; TLS Availability Detection
;;;; ========================================================================

(defvar *tls-available* nil
  "T if pure-tls is available and loaded")

(defun tls-available-p ()
  "Check if TLS support is available"
  *tls-available*)

(defun ensure-tls-available ()
  "Signal an error if TLS is not available"
  (unless *tls-available*
    (error "TLS requested but pure-tls is not available. ~
            Install pure-tls to enable TLS support.")))

;;;; ========================================================================
;;;; TLS Stream Wrapping
;;;; ========================================================================

(defvar *make-tls-client-stream* nil
  "Function to wrap a stream with TLS (set when pure-tls is loaded)")

(defvar *make-tls-server-stream* nil
  "Function to create TLS server stream (set when pure-tls is loaded)")

(defun wrap-stream-with-tls (stream hostname &key verify client-certificate client-key)
  "Wrap a socket stream with TLS encryption.
   STREAM is the underlying TCP stream.
   HOSTNAME is used for SNI (Server Name Indication).
   VERIFY controls certificate verification (T for full verification).
   CLIENT-CERTIFICATE - Path to client certificate for mTLS.
   CLIENT-KEY - Path to client private key for mTLS."
  (ensure-tls-available)
  (apply *make-tls-client-stream* stream
         :hostname hostname
         :verify (if verify
                     (symbol-value (find-symbol "+VERIFY-PEER+" :pure-tls))
                     (symbol-value (find-symbol "+VERIFY-NONE+" :pure-tls)))
         :alpn-protocols '("h2")
         (append
          (when client-certificate (list :client-certificate client-certificate))
          (when client-key (list :client-key client-key)))))

(defun wrap-server-stream-with-tls (stream certificate key &key password verify trust-store)
  "Wrap a server socket stream with TLS encryption.
STREAM - the underlying TCP stream
CERTIFICATE - path to PEM certificate file
KEY - path to PEM private key file
PASSWORD - optional password for encrypted key (currently unused by pure-tls)
VERIFY - client certificate verification mode (T to require client certs)
TRUST-STORE - path to CA certificate for verifying client certificates"
  (declare (ignore password))
  (ensure-tls-available)
  (apply *make-tls-server-stream* stream
         :certificate certificate
         :key key
         :alpn-protocols '("h2")
         (append
          (when verify
            (list :verify (symbol-value (find-symbol "+VERIFY-REQUIRED+" :pure-tls))))
          (when trust-store
            (list :trust-store trust-store)))))

;;;; ========================================================================
;;;; TLS Initialization
;;;; ========================================================================

(defun try-load-tls ()
  "Attempt to load pure-tls if available"
  (handler-case
      (progn
        (asdf:load-system :pure-tls :verbose nil)
        (setf *tls-available* t)
        (setf *make-tls-client-stream*
              (fdefinition (find-symbol "MAKE-TLS-CLIENT-STREAM" :pure-tls)))
        (setf *make-tls-server-stream*
              (fdefinition (find-symbol "MAKE-TLS-SERVER-STREAM" :pure-tls)))
        t)
    (error ()
      (setf *tls-available* nil)
      nil)))

;; Try to load TLS support at load time, but don't fail if unavailable
(ignore-errors (try-load-tls))
