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
   #:http2-stream-error))
