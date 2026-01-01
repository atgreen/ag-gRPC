;;;; package.lisp - Package definition for ag-http2

(defpackage #:ag-http2
  (:use #:cl)
  (:export
   ;; HPACK
   #:hpack-encoder
   #:hpack-decoder
   #:hpack-encode
   #:hpack-decode
   #:make-hpack-encoder
   #:make-hpack-decoder

   ;; Frame types
   #:frame
   #:data-frame
   #:headers-frame
   #:priority-frame
   #:rst-stream-frame
   #:settings-frame
   #:push-promise-frame
   #:ping-frame
   #:goaway-frame
   #:window-update-frame
   #:continuation-frame

   ;; Frame operations
   #:read-frame
   #:write-frame
   #:frame-type
   #:frame-flags
   #:frame-stream-id
   #:frame-payload

   ;; Settings
   #:+settings-header-table-size+
   #:+settings-enable-push+
   #:+settings-max-concurrent-streams+
   #:+settings-initial-window-size+
   #:+settings-max-frame-size+
   #:+settings-max-header-list-size+

   ;; Frame flags
   #:+flag-end-stream+
   #:+flag-end-headers+
   #:+flag-ack+

   ;; Stream states
   #:http2-stream
   #:stream-state
   #:stream-id

   ;; Connection
   #:http2-connection
   #:make-client-connection
   #:make-server-connection
   #:connection-send-headers
   #:connection-send-data
   #:connection-read-frame
   #:connection-close

   ;; Conditions
   #:http2-error
   #:http2-connection-error
   #:http2-stream-error
   #:http2-frame-error

   ;; Connection accessors
   #:connection-state
   #:connection-multiplexer
   #:connection-new-stream
   #:connection-client-p

   ;; Server connection
   #:server-connection-handshake

   ;; Stream accessors
   #:stream-headers
   #:stream-trailers
   #:stream-data-buffer
   #:stream-can-send-p
   #:stream-can-recv-p
   #:stream-consume-data

   ;; Multiplexer
   #:multiplexer-get-stream
   #:multiplexer-new-stream

   ;; TLS
   #:tls-available-p
   #:try-load-tls))
