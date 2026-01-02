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
   #:make-grpc-metadata
   #:metadata-get
   #:metadata-get-all
   #:metadata-set
   #:metadata-add
   #:metadata-remove
   #:metadata-clear
   #:metadata-keys
   #:metadata-count
   #:metadata-empty-p
   #:metadata-copy
   #:metadata-merge
   #:metadata-to-alist
   #:alist-to-metadata

   ;; Channel
   #:grpc-channel
   #:make-channel
   #:make-secure-channel
   #:channel-close
   #:channel-tls-p

   ;; Call (unary)
   #:grpc-call
   #:call-unary
   #:call-response
   #:call-status
   #:call-rpc-method
   #:call-request-metadata

   ;; Server streaming
   #:grpc-server-stream
   #:call-server-streaming
   #:stream-read-message
   #:stream-finished-p
   #:stream-status
   #:stream-collect-all
   #:do-stream-messages

   ;; Client streaming
   #:grpc-client-stream
   #:call-client-streaming
   #:stream-send
   #:stream-close-and-recv
   #:client-stream-closed-p
   #:with-client-stream

   ;; Bidirectional streaming
   #:grpc-bidi-stream
   #:call-bidirectional-streaming
   #:stream-close-send
   #:bidi-stream-send-closed-p
   #:bidi-stream-recv-finished-p
   #:do-bidi-recv

   ;; Convenience macros
   #:with-channel
   #:with-call
   #:with-client-stream
   #:with-bidi-stream
   #:with-server-stream

   ;; Response objects
   #:grpc-response
   #:make-response-from-call
   #:response-ok-p
   #:response-message
   #:response-status
   #:response-status-message
   #:response-headers
   #:response-trailers
   #:response-header
   #:response-trailer
   #:response-value
   #:check-response

   ;; Stream iterators
   #:grpc-stream-iterator
   #:make-stream-iterator
   #:iterator-next
   #:iterator-peek
   #:iterator-done-p
   #:iterator-status

   ;; Stream collectors
   #:collect-stream-messages
   #:map-stream-messages
   #:reduce-stream-messages
   #:find-in-stream

   ;; Conditions
   #:grpc-error
   #:grpc-status-error
   #:grpc-status-error-code
   #:grpc-status-error-message
   #:grpc-status-error-details

   ;; Server
   #:grpc-server
   #:make-grpc-server
   #:server-start
   #:server-stop
   #:server-state
   #:server-register-handler
   #:with-grpc-server

   ;; Call context
   #:grpc-call-context
   #:context-metadata
   #:context-peer-address
   #:context-deadline
   #:context-set-response-metadata
   #:context-set-trailing-metadata

   ;; Server streams
   #:grpc-server-call-stream
   #:stream-recv
   #:do-stream-recv))
