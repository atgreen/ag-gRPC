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
   #:channel-cancel-stream

   ;; Call (unary)
   #:grpc-call
   #:call-unary
   #:call-response
   #:call-status
   #:call-rpc-method
   #:call-request-metadata

   ;; Server streaming
   #:grpc-server-stream
   #:call-server-stream
   #:stream-receive-message
   #:stream-receive-headers
   #:stream-finish
   #:stream-call-status
   #:stream-call-status-message
   #:stream-call-response-headers
   #:stream-call-response-trailers
   #:stream-finished-p
   #:stream-status-ok-p

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
   #:stream-read-message
   #:stream-close-send
   #:stream-status
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
   #:grpc-error-message
   #:grpc-status-error
   #:grpc-status-error-code
   #:grpc-status-error-message
   #:grpc-status-error-details
   #:grpc-status-error-headers
   #:grpc-status-error-trailers

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
   #:context-cancelled-p
   #:context-check-cancelled
   #:context-set-response-metadata
   #:context-set-trailing-metadata

   ;; Server streams
   #:grpc-server-call-stream
   #:stream-recv
   #:do-stream-recv

   ;; Interceptors
   #:server-interceptor
   #:interceptor-call-start
   #:interceptor-call-end
   #:interceptor-recv-message
   #:interceptor-send-message
   #:server-add-interceptor
   #:server-interceptors
   ;; Built-in server interceptors
   #:logging-interceptor
   #:make-logging-interceptor
   #:metrics-interceptor
   #:make-metrics-interceptor
   #:metrics-get-stats

   ;; Client interceptors
   #:client-interceptor
   #:client-interceptor-call-start
   #:client-interceptor-call-end
   #:client-interceptor-send-message
   #:client-interceptor-recv-message
   #:channel-add-interceptor
   #:channel-interceptors
   ;; Built-in client interceptors
   #:client-logging-interceptor
   #:make-client-logging-interceptor
   #:client-metrics-interceptor
   #:make-client-metrics-interceptor
   #:client-metrics-get-stats
   #:metadata-interceptor
   #:make-metadata-interceptor

   ;; Retry policies
   #:retry-policy
   #:make-retry-policy
   #:retry-max-attempts
   #:retry-initial-backoff
   #:retry-max-backoff
   #:retry-backoff-multiplier
   #:retry-retryable-status-codes
   #:call-with-retry
   #:with-retry
   #:call-unary-with-retry
   #:channel-set-retry-policy
   #:channel-get-retry-policy

   ;; Channel state and wait-for-ready
   #:channel-state
   #:channel-ready-p
   #:channel-wait-for-ready
   #:call-when-ready
   #:with-wait-for-ready

   ;; Keepalive
   #:keepalive-config
   #:make-keepalive-config
   #:keepalive-ping-interval
   #:keepalive-ping-timeout
   #:keepalive-permit-without-calls
   #:channel-ping
   #:channel-check-health

   ;; Channel pool
   #:channel-pool
   #:make-channel-pool
   #:pool-get-channel
   #:pool-release-channel
   #:pool-close
   #:with-pooled-channel

   ;; Load balancing
   #:load-balancer
   #:balancer-pick
   #:balancer-report-success
   #:balancer-report-failure
   #:balancer-endpoints
   #:balancer-get-channel
   #:balancer-close-all
   #:balancer-add-endpoint
   #:balancer-remove-endpoint
   #:with-balanced-channel
   ;; Pick-first policy
   #:pick-first-balancer
   #:make-pick-first-balancer
   ;; Round-robin policy
   #:round-robin-balancer
   #:make-round-robin-balancer
   ;; DNS-based discovery
   #:dns-resolver
   #:make-dns-resolver
   #:resolve-hostname
   #:dns-balancer
   #:make-dns-balancer
   #:dns-balancer-refresh

   ;; Health checking
   #:health-check-request
   #:health-request-service
   #:health-check-response
   #:health-response-status
   #:+health-unknown+
   #:+health-serving+
   #:+health-not-serving+
   #:+health-service-unknown+
   #:health-service
   #:make-health-service
   #:health-set-status
   #:health-get-status
   #:health-clear-status
   #:server-enable-health-checking

   ;; Reflection service
   #:reflection-service
   #:make-reflection-service
   #:server-enable-reflection

   ;; Rich error details (google.rpc.Status)
   #:rpc-status
   #:rpc-status-code
   #:rpc-status-message
   #:rpc-status-details
   #:proto-any
   #:any-type-url
   #:any-value
   #:make-any
   #:error-info
   #:make-error-info
   #:error-info-reason
   #:error-info-domain
   #:error-info-metadata
   #:retry-info
   #:make-retry-info
   #:debug-info
   #:make-debug-info
   #:make-rich-status-error
   #:extract-status-details

   ;; Async/Future API
   #:grpc-future
   #:make-grpc-future
   #:future-pending-p
   #:future-fulfilled-p
   #:future-rejected-p
   #:future-done-p
   #:future-fulfill
   #:future-reject
   #:future-wait
   #:future-get
   #:future-then
   #:future-catch
   #:future-finally
   #:future-cancel
   #:future-result
   #:future-error
   #:future-state
   #:call-unary-async
   #:call-server-stream-async
   #:future-all
   #:future-race
   #:future-any

   ;; Circuit breaker
   #:circuit-breaker
   #:make-circuit-breaker
   #:breaker-name
   #:breaker-state
   #:breaker-failure-threshold
   #:breaker-success-threshold
   #:breaker-timeout
   #:breaker-allow-request-p
   #:breaker-record-success
   #:breaker-record-failure
   #:breaker-stats
   #:breaker-reset
   #:breaker-force-open
   #:call-with-circuit-breaker
   #:with-circuit-breaker
   #:circuit-open-error
   #:circuit-open-breaker-name

   ;; Hedged requests
   #:hedge-policy
   #:make-hedge-policy
   #:hedge-max-attempts
   #:hedge-delay
   #:hedge-non-fatal-codes
   #:call-with-hedging
   #:with-hedging
   #:call-unary-hedged

   ;; OpenTelemetry / Tracing
   #:opentelemetry-available-p
   #:try-load-opentelemetry
   #:grpc-telemetry-config
   #:make-telemetry-config
   #:telemetry-service-name
   #:telemetry-endpoint
   #:telemetry-sample-rate
   #:telemetry-record-request-p
   #:telemetry-record-response-p
   #:telemetry-propagate-context-p
   #:tracing-interceptor
   #:make-tracing-interceptor
   #:client-tracing-interceptor
   #:make-client-tracing-interceptor
   #:enable-server-tracing
   #:enable-channel-tracing
   #:extract-trace-context
   #:inject-trace-context
   #:generate-trace-id
   #:generate-span-id

   ;; gRPC-Web
   #:+grpc-web-content-type+
   #:+grpc-web-text-content-type+
   #:grpc-web-handler
   #:make-grpc-web-handler
   #:grpc-web-request-p
   #:grpc-web-text-request-p
   #:grpc-web-process-request
   #:server-enable-grpc-web
   #:grpc-web-channel
   #:make-grpc-web-channel
   #:grpc-web-call-unary
   #:grpc-web-frame-message
   #:grpc-web-parse-frame
   #:grpc-web-encode-trailers
   #:grpc-web-parse-trailers
   #:base64-encode
   #:base64-decode))
