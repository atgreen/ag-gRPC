;;;; package.lisp - Package definition for ag-grpc

(defpackage #:ag-grpc
  (:use #:cl)
  (:export
   ;; Version
   #:+version+

   ;; Status codes
   #:+grpc-status-ok+
   #:+grpc-status-cancelled+
   #:+grpc-status-unknown+
   #:+grpc-status-invalid-argument+
   #:+grpc-status-deadline-exceeded+
   #:+grpc-status-not-found+
   #:+grpc-status-already-exists+
   #:+grpc-status-permission-denied+
   #:+grpc-status-resource-exhausted+
   #:+grpc-status-failed-precondition+
   #:+grpc-status-aborted+
   #:+grpc-status-out-of-range+
   #:+grpc-status-unimplemented+
   #:+grpc-status-internal+
   #:+grpc-status-unavailable+
   #:+grpc-status-data-loss+
   #:+grpc-status-unauthenticated+
   #:grpc-status-name

   ;; Framing
   #:encode-grpc-message
   #:decode-grpc-message

   ;; Metadata
   #:grpc-metadata
   #:metadata-get
   #:metadata-set
   #:metadata-add

   ;; Channel
   #:grpc-channel
   #:make-channel
   #:make-secure-channel
   #:channel-close
   #:channel-tls-p

   ;; Call
   #:grpc-call
   #:call-unary
   #:call-response
   #:call-status
   #:call-rpc-method
   #:call-request-metadata

   ;; Conditions
   #:grpc-error
   #:grpc-status-error))
