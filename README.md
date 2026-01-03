# ag-gRPC

A pure Common Lisp implementation of gRPC, Protocol Buffers, and HTTP/2.

## Overview

ag-gRPC provides a complete gRPC stack (client and server) written entirely in portable Common Lisp. It includes:

- **ag-proto** - Protocol Buffers (Proto3) implementation with .proto file parsing and code generation
- **ag-http2** - HTTP/2 protocol implementation (RFC 7540) with HPACK header compression (RFC 7541)
- **ag-grpc** - gRPC protocol implementation for client and server

## Status

ag-gRPC is tested against the [ConnectRPC conformance suite](https://github.com/connectrpc/conformance), achieving **100% pass rate** on supported features.

### Conformance Test Results

| Category | Tests | Status |
|----------|-------|--------|
| Basic RPC | 20 | ✅ Pass |
| Client Cancellation | 40 | ✅ Pass |
| Deadline Propagation | 16 | ✅ Pass |
| Duplicate Metadata | 16 | ✅ Pass |
| Errors | 88 | ✅ Pass |
| HTTP to RPC Mapping | 8 | ✅ Pass |
| Max Message Size | 48 | ✅ Pass |
| Request Headers | 32 | ✅ Pass |
| Response Headers | 32 | ✅ Pass |
| Timeouts | 16 | ✅ Pass |
| Trailers-Only | 24 | ✅ Pass |
| Unimplemented | 16 | ✅ Pass |
| Unicode | 3 | ✅ Pass |
| **Total** | **359** | **100%** |

### Feature Support Matrix

| Feature | Client | Server |
|---------|--------|--------|
| Unary RPC | ✅ | ✅ |
| Client Streaming | ✅ | ✅ |
| Server Streaming | ✅ | ✅ |
| Bidirectional Streaming | ✅ | ✅ |
| Metadata | ✅ | ✅ |
| Deadlines/Timeouts | ✅ | ✅ |
| Cancellation | ✅ | ✅ |
| Interceptors | ✅ | ✅ |
| Health Checking | — | ✅ |
| Reflection | — | ✅ |
| TLS (h2) | ✅ | ✅ |
| Plaintext (h2c) | ✅ | ✅ |
| Compression (gzip) | ✅ | ✅ |
| Retry Policies | ✅ | — |
| Load Balancing | ✅ | — |
| Channel Pooling | ✅ | — |
| Wait-for-Ready | ✅ | — |
| Rich Error Details | ✅ | ✅ |
| Async/Futures | ✅ | — |
| Circuit Breaker | ✅ | — |
| Hedged Requests | ✅ | — |
| OpenTelemetry | ✅ | ✅ |
| gRPC-Web | ✅ | ✅ |

## Features

- Pure Common Lisp - minimal foreign dependencies
- **Idiomatic Lisp API** with convenience macros (`with-channel`, `with-call`, `with-bidi-stream`)
- Proto3 wire format encoding/decoding
- .proto file parser with code generation to CLOS classes
- **Generated client stubs** - type-safe RPC methods from service definitions
- Full HPACK implementation including Huffman coding
- HTTP/2 client with stream multiplexing and flow control
- **Full streaming support**: unary, server streaming, client streaming, and bidirectional streaming
- **Stream collectors**: `collect-stream-messages`, `map-stream-messages`, `reduce-stream-messages`
- **Message compression**: gzip compression support (via salza2/chipz)
- Gray stream integration for composable I/O
- Optional TLS/SSL support (via cl+ssl)
- **gRPC Server**: handler registration, request context, streaming support
- **Interceptors**: client and server middleware for logging, auth, metrics
- **Health checking**: standard grpc.health.v1.Health service
- **Server reflection**: grpc.reflection.v1alpha for service discovery
- **Retry policies**: automatic retry with exponential backoff
- **Load balancing**: round-robin and pick-first policies with DNS discovery
- **Channel pooling**: connection reuse and wait-for-ready semantics
- **Rich error details**: google.rpc.Status with ErrorInfo, RetryInfo, DebugInfo
- **Async/Futures API**: non-blocking calls with futures, combinators (all, race, any)
- **Circuit breaker**: fault tolerance pattern for cascade failure prevention
- **Hedged requests**: send to multiple backends, use first response
- **OpenTelemetry**: distributed tracing with W3C trace context propagation
- **gRPC-Web**: browser client support with base64 and binary modes
- Interoperability tested against Go gRPC servers

## Installation

ag-gRPC uses [ocicl](https://github.com/ocicl/ocicl) for dependency management:

```bash
cd ag-gRPC
ocicl install
```

Or load via ASDF after adding to your source registry:

```lisp
(asdf:load-system :ag-grpc)
```

## Idiomatic Lisp API

ag-gRPC provides a clean, idiomatic Common Lisp API with convenience macros for resource management:

### Convenience Macros

```lisp
;; Automatic channel cleanup
(ag-grpc:with-channel (ch "localhost" 50051)
  (ag-grpc:with-call (call ch "/hello.Greeter/SayHello" request
                       :response-type 'helloreply)
    (format t "Response: ~A~%" (ag-grpc:call-response call))))

;; Server streaming with automatic iteration
(ag-grpc:with-channel (ch "localhost" 50051)
  (ag-grpc:with-server-stream (stream ch "/hello.Greeter/ListFeatures" request
                                :response-type 'feature)
    (ag-grpc:do-stream-messages (feature stream)
      (process-feature feature))))

;; Bidirectional streaming with automatic cleanup
(ag-grpc:with-channel (ch "localhost" 50051)
  (ag-grpc:with-bidi-stream (stream ch "/hello.Greeter/Chat"
                              :response-type 'chatmessage)
    (ag-grpc:stream-send stream msg1)
    (ag-grpc:stream-send stream msg2)
    (ag-grpc:stream-close-send stream)
    (ag-grpc:do-bidi-recv (reply stream)
      (handle-reply reply))))
```

### Metadata API

```lisp
;; Create metadata
(defvar *md* (ag-grpc:make-grpc-metadata))
(ag-grpc:metadata-set *md* "authorization" "Bearer token123")
(ag-grpc:metadata-add *md* "x-custom-header" "value")

;; Query metadata
(ag-grpc:metadata-get *md* "authorization")  ; => "Bearer token123"
(ag-grpc:metadata-keys *md*)                 ; => ("authorization" "x-custom-header")
(ag-grpc:metadata-count *md*)                ; => 2

;; Immutable-style operations
(defvar *md2* (ag-grpc:metadata-copy *md*))
(defvar *merged* (ag-grpc:metadata-merge *md* *other-md*))

;; Use with RPC calls
(ag-grpc:call-unary channel method request :metadata *md*)
```

### Stream Collectors

```lisp
;; Collect all messages into a list
(ag-grpc:collect-stream-messages stream)

;; Collect with limit and transform
(ag-grpc:collect-stream-messages stream :limit 10 :transform #'extract-id)

;; Map over stream messages
(ag-grpc:map-stream-messages #'process-message stream)

;; Reduce stream messages
(ag-grpc:reduce-stream-messages #'+ stream 0)  ; Sum all values

;; Find first matching message
(ag-grpc:find-in-stream #'important-p stream)
```

### Response Objects

```lisp
;; Create response from call
(let ((response (ag-grpc:make-response-from-call call)))
  (when (ag-grpc:response-ok-p response)
    (process (ag-grpc:response-message response)))
  ;; Lazy metadata access (converted to grpc-metadata on demand)
  (ag-grpc:response-header response "x-request-id")
  (ag-grpc:response-trailer response "grpc-status"))
```

## Quick Start

### Define a service (hello.proto)

```protobuf
syntax = "proto3";

package hello;

service Greeter {
  rpc SayHello (HelloRequest) returns (HelloReply);
}

message HelloRequest {
  string name = 1;
}

message HelloReply {
  string message = 1;
}
```

### Generate Lisp code

```lisp
(ag-proto:compile-proto-file "hello.proto" :load t)
```

Or use the CLI tool:

```bash
./ag-protoc -o hello.lisp hello.proto
```

### Make a gRPC call (using generated stubs)

```lisp
;; Create a channel to the server
(defvar *channel* (ag-grpc:make-channel "localhost" 50051))

;; Create a client stub
(defvar *greeter* (make-greeter-stub *channel*))

;; Create a request and make the call
(defvar *request* (make-instance 'hellorequest :name "World"))

;; The stub method returns (values response status call)
(multiple-value-bind (response status)
    (greeter-say-hello *greeter* *request*)
  (format t "Status: ~A~%" status)
  (format t "Message: ~A~%" (message response)))
;; Status: 0
;; Message: Hello World

;; Clean up
(ag-grpc:channel-close *channel*)
```

### Make a gRPC call (low-level API)

```lisp
;; Create a channel to the server
(defvar *channel* (ag-grpc:make-channel "localhost" 50051))

;; Create a request
(defvar *request* (make-instance 'hellorequest :name "World"))

;; Make the RPC call directly
(defvar *call* (ag-grpc:call-unary *channel*
                                    "/hello.Greeter/SayHello"
                                    *request*
                                    :response-type 'helloreply))

;; Get the response
(message (ag-grpc:call-response *call*))
;; => "Hello World"

;; Check status
(ag-grpc:call-status *call*)
;; => 0 (OK)

;; Clean up
(ag-grpc:channel-close *channel*)
```

## Server Streaming

ag-gRPC supports server streaming RPCs where the server sends multiple responses:

```lisp
;; Using generated stubs (recommended)
(defvar *stream* (greeter-say-hello-stream *stub* request))

;; Iterate over all messages
(ag-grpc:do-stream-messages (reply *stream*)
  (format t "Got: ~A~%" (message reply)))

;; Check final status
(ag-grpc:stream-status *stream*)
;; => 0 (OK)
```

### Alternative streaming APIs

```lisp
;; Read messages one at a time
(loop for msg = (ag-grpc:stream-read-message stream)
      while msg
      do (process msg))

;; Collect all messages into a list
(defvar *all-replies* (ag-grpc:stream-collect-all stream))

;; Low-level API
(defvar *stream* (ag-grpc:call-server-streaming channel
                                                 "/hello.Greeter/ListFeatures"
                                                 request
                                                 :response-type 'feature))
```

## Client Streaming

ag-gRPC supports client streaming RPCs where the client sends multiple requests and receives a single response:

```lisp
;; Using generated stubs (recommended)
(defvar *stream* (greeter-collect-hellos *stub*))

;; Send multiple messages
(ag-grpc:stream-send *stream* request1)
(ag-grpc:stream-send *stream* request2)
(ag-grpc:stream-send *stream* request3)

;; Close and get the response
(multiple-value-bind (response status)
    (ag-grpc:stream-close-and-recv *stream*)
  (format t "Status: ~A~%" status)
  (format t "Response: ~A~%" response))
```

### Using the with-client-stream macro

```lisp
;; Automatic cleanup with macro
(multiple-value-bind (response status)
    (ag-grpc:with-client-stream (stream channel "/pkg.Svc/Collect"
                                  :response-type 'summary)
      (ag-grpc:stream-send stream point1)
      (ag-grpc:stream-send stream point2)
      (ag-grpc:stream-send stream point3))
  (process-response response))
```

### Low-level client streaming API

```lisp
;; Direct channel access
(defvar *stream* (ag-grpc:call-client-streaming channel
                                                 "/hello.Greeter/CollectHellos"
                                                 :response-type 'hellosummary))

;; Send messages
(ag-grpc:stream-send *stream* (make-instance 'hellorequest :name "Alice"))
(ag-grpc:stream-send *stream* (make-instance 'hellorequest :name "Bob"))

;; Finish and get response
(ag-grpc:stream-close-and-recv *stream*)
```

## Bidirectional Streaming

ag-gRPC supports bidirectional streaming where both client and server can send messages concurrently:

```lisp
;; Using generated stubs (recommended)
(defvar *stream* (greeter-chat *stub*))

;; Send and receive can be interleaved
(ag-grpc:stream-send *stream* request1)
(let ((reply (ag-grpc:stream-read-message *stream*)))
  (process reply))

(ag-grpc:stream-send *stream* request2)
(let ((reply (ag-grpc:stream-read-message *stream*)))
  (process reply))

;; Close send side when done sending
(ag-grpc:stream-close-send *stream*)

;; Continue receiving remaining messages
(ag-grpc:do-bidi-recv (msg *stream*)
  (format t "Got: ~A~%" msg))

;; Check final status
(ag-grpc:stream-status *stream*)
```

### Low-level bidirectional streaming API

```lisp
;; Direct channel access
(defvar *stream* (ag-grpc:call-bidirectional-streaming channel
                                                        "/hello.Greeter/Chat"
                                                        :response-type 'helloreply))

;; Send messages
(ag-grpc:stream-send *stream* (make-instance 'hellorequest :name "Alice"))

;; Read responses (can interleave with sends)
(ag-grpc:stream-read-message *stream*)

;; Signal end of client messages
(ag-grpc:stream-close-send *stream*)

;; Drain remaining server messages
(loop for msg = (ag-grpc:stream-read-message *stream*)
      while msg
      do (process msg))
```

## gRPC Server

ag-gRPC includes full server-side support for hosting gRPC services:

### Basic Server Setup

```lisp
;; Define a handler function
(defun handle-say-hello (request ctx)
  "Handler for SayHello RPC"
  (make-instance 'hello-reply
    :message (format nil "Hello, ~A!" (name request))))

;; Create and start server
(defvar *server* (ag-grpc:make-grpc-server 50051))

;; Register handler
(ag-grpc:server-register-handler *server* "/hello.Greeter/SayHello"
                                  #'handle-say-hello
                                  :request-type 'hello-request
                                  :response-type 'hello-reply)

;; Start server (blocks)
(ag-grpc:server-start *server*)
```

### Using with-grpc-server

```lisp
;; Automatic cleanup with macro
(ag-grpc:with-grpc-server (server 50051)
  (ag-grpc:server-register-handler server "/hello.Greeter/SayHello"
                                    #'handle-say-hello
                                    :request-type 'hello-request
                                    :response-type 'hello-reply)
  (ag-grpc:server-start server))
```

### Server Streaming Handler

```lisp
(defun handle-list-features (request ctx stream)
  "Server streaming: send multiple responses"
  (dolist (feature (find-features-in-area request))
    (ag-grpc:stream-send stream feature)))

(ag-grpc:server-register-handler server "/route.RouteGuide/ListFeatures"
                                  #'handle-list-features
                                  :request-type 'rectangle
                                  :response-type 'feature
                                  :server-streaming t)
```

### Client Streaming Handler

```lisp
(defun handle-record-route (ctx stream)
  "Client streaming: receive multiple requests, return single response"
  (let ((points nil))
    (ag-grpc:do-stream-recv (point stream)
      (push point points))
    (make-instance 'route-summary
      :point-count (length points))))

(ag-grpc:server-register-handler server "/route.RouteGuide/RecordRoute"
                                  #'handle-record-route
                                  :request-type 'point
                                  :response-type 'route-summary
                                  :client-streaming t)
```

### Bidirectional Streaming Handler

```lisp
(defun handle-route-chat (ctx stream)
  "Bidi streaming: interleave send and receive"
  (ag-grpc:do-stream-recv (note stream)
    ;; Echo back with additional info
    (ag-grpc:stream-send stream
      (make-instance 'route-note
        :message (format nil "Got: ~A" (message note))))))

(ag-grpc:server-register-handler server "/route.RouteGuide/RouteChat"
                                  #'handle-route-chat
                                  :request-type 'route-note
                                  :response-type 'route-note
                                  :client-streaming t
                                  :server-streaming t)
```

### Accessing Request Context

```lisp
(defun handle-authenticated-rpc (request ctx)
  ;; Access request metadata
  (let ((auth-token (ag-grpc:context-metadata ctx "authorization")))
    (unless (valid-token-p auth-token)
      (error 'ag-grpc:grpc-status-error
             :code ag-grpc:+grpc-status-unauthenticated+
             :message "Invalid token")))
  ;; Set response metadata
  (ag-grpc:context-set-trailing-metadata ctx
    (ag-grpc:make-grpc-metadata '(("x-request-id" . "12345"))))
  ;; Return response
  (make-response request))
```

### Detecting Client Cancellation

Server handlers can detect when clients cancel RPCs:

```lisp
(defun handle-long-operation (request ctx)
  "Handler that checks for cancellation during long operations"
  (loop for i from 1 to 1000
        do (progn
             ;; Check if client cancelled
             (when (ag-grpc:context-check-cancelled ctx)
               (return-from handle-long-operation nil))
             ;; Do work
             (perform-step i)))
  (make-response))
```

## Interceptors (Middleware)

ag-gRPC supports server-side interceptors for cross-cutting concerns like logging, authentication, and metrics:

### Using Built-in Interceptors

```lisp
;; Add logging interceptor
(ag-grpc:server-add-interceptor server
  (ag-grpc:make-logging-interceptor :stream *standard-output*))

;; Add metrics interceptor
(defvar *metrics* (ag-grpc:make-metrics-interceptor))
(ag-grpc:server-add-interceptor server *metrics*)

;; Query metrics later
(multiple-value-bind (calls avg-ms errors)
    (ag-grpc:metrics-get-stats *metrics* "/hello.Greeter/SayHello")
  (format t "Calls: ~A, Avg: ~,2Fms, Errors: ~A~%" calls avg-ms errors))
```

### Creating Custom Interceptors

```lisp
(defclass auth-interceptor (ag-grpc:server-interceptor)
  ((required-token :initarg :token :reader required-token)))

(defmethod ag-grpc:interceptor-call-start ((i auth-interceptor) ctx handler-info)
  "Check authentication before handler runs"
  (let ((token (ag-grpc:metadata-get (ag-grpc:context-metadata ctx) "authorization")))
    (unless (equal token (required-token i))
      (error 'ag-grpc:grpc-status-error
             :code ag-grpc:+grpc-status-unauthenticated+
             :message "Invalid or missing token")))
  ;; Return start time for timing in call-end
  (get-internal-real-time))

(defmethod ag-grpc:interceptor-call-end ((i auth-interceptor) ctx handler-info
                                          call-context response error)
  "Log after handler completes"
  (let ((elapsed-ms (/ (- (get-internal-real-time) call-context)
                       (/ internal-time-units-per-second 1000.0))))
    (format t "~A completed in ~,2Fms~%"
            (getf handler-info :method-path) elapsed-ms))
  response)

;; Use custom interceptor
(ag-grpc:server-add-interceptor server
  (make-instance 'auth-interceptor :token "secret-token"))
```

### Interceptor Hooks

| Method | Called | Use Case |
|--------|--------|----------|
| `interceptor-call-start` | Before handler | Auth, logging, timing start |
| `interceptor-call-end` | After handler | Logging, metrics, response modification |
| `interceptor-recv-message` | Each received message | Validation, transformation |
| `interceptor-send-message` | Each sent message | Transformation, logging |

## Health Checking

ag-gRPC implements the standard [gRPC Health Checking Protocol](https://github.com/grpc/grpc/blob/master/doc/health-checking.md) for load balancer integration:

### Enabling Health Checking

```lisp
;; Enable health checking on server
(defvar *health* (ag-grpc:server-enable-health-checking server))

;; Server automatically responds to:
;; - /grpc.health.v1.Health/Check (unary)
;; - /grpc.health.v1.Health/Watch (server streaming)
```

### Managing Service Health

```lisp
;; Set status for a specific service
(ag-grpc:health-set-status *health* "my.service.Name" ag-grpc:+health-serving+)

;; Mark service as not serving
(ag-grpc:health-set-status *health* "my.service.Name" ag-grpc:+health-not-serving+)

;; Get current status
(ag-grpc:health-get-status *health* "my.service.Name")

;; Clear status (will return SERVICE_UNKNOWN)
(ag-grpc:health-clear-status *health* "my.service.Name")
```

### Health Status Constants

| Constant | Value | Meaning |
|----------|-------|---------|
| `+health-unknown+` | 0 | Status not set |
| `+health-serving+` | 1 | Healthy and serving |
| `+health-not-serving+` | 2 | Not accepting requests |
| `+health-service-unknown+` | 3 | Service not registered |

### Testing with grpc-health-probe

```bash
# Install grpc-health-probe
go install github.com/grpc-ecosystem/grpc-health-probe@latest

# Check overall server health
grpc-health-probe -addr=localhost:50051

# Check specific service
grpc-health-probe -addr=localhost:50051 -service=my.service.Name
```

## Retry Policies

ag-gRPC supports automatic retry with configurable backoff for transient failures:

```lisp
;; Create a retry policy
(defvar *retry* (ag-grpc:make-retry-policy
                  :max-attempts 5
                  :initial-backoff 0.1     ; 100ms
                  :max-backoff 10.0        ; 10 seconds
                  :backoff-multiplier 2.0))

;; Make a call with retry
(ag-grpc:call-unary-with-retry channel method request
                                :retry-policy *retry*)

;; Or use the macro
(ag-grpc:with-retry (*retry*)
  (ag-grpc:call-unary channel method request))
```

### Retryable Status Codes

By default, these status codes trigger retry:
- `UNAVAILABLE` - Server temporarily unavailable
- `RESOURCE_EXHAUSTED` - Rate limited
- `ABORTED` - Operation aborted

## Load Balancing

ag-gRPC supports client-side load balancing with multiple policies:

### Round-Robin

```lisp
;; Create a round-robin balancer with multiple endpoints
(defvar *balancer* (ag-grpc:make-round-robin-balancer
                     '(("server1.example.com" . 50051)
                       ("server2.example.com" . 50051)
                       ("server3.example.com" . 50051))
                     :tls t))

;; Get a channel and make calls
(ag-grpc:with-balanced-channel (ch *balancer*)
  (ag-grpc:call-unary ch method request))
```

### Pick-First (Failover)

```lisp
;; Pick-first uses the first available endpoint
(defvar *balancer* (ag-grpc:make-pick-first-balancer endpoints))
```

### DNS-Based Discovery

```lisp
;; Automatically discover endpoints via DNS
(defvar *balancer* (ag-grpc:make-dns-balancer "grpc.example.com" 50051
                                               :refresh-interval 30))
```

## Channel Pooling

Reuse connections across multiple operations:

```lisp
;; Create a channel pool
(defvar *pool* (ag-grpc:make-channel-pool "server.example.com" 50051
                                           :max-size 10
                                           :tls t))

;; Get a channel from the pool
(ag-grpc:with-pooled-channel (ch *pool*)
  (ag-grpc:call-unary ch method request))

;; Clean up
(ag-grpc:pool-close *pool*)
```

### Wait-for-Ready

Queue requests until channel becomes ready:

```lisp
;; Wait for channel to be ready before calling
(ag-grpc:with-wait-for-ready (channel :timeout 30)
  (ag-grpc:call-unary channel method request))
```

## Client Interceptors

Add middleware to outgoing calls for logging, authentication, metrics:

```lisp
;; Add logging interceptor to channel
(ag-grpc:channel-add-interceptor channel
  (ag-grpc:make-client-logging-interceptor))

;; Add metrics interceptor
(defvar *metrics* (ag-grpc:make-client-metrics-interceptor))
(ag-grpc:channel-add-interceptor channel *metrics*)

;; Query metrics
(multiple-value-bind (calls avg-ms errors)
    (ag-grpc:client-metrics-get-stats *metrics* "/hello.Greeter/SayHello")
  (format t "Calls: ~A, Avg: ~,2Fms, Errors: ~A~%" calls avg-ms errors))
```

### Custom Client Interceptor

```lisp
(defclass auth-interceptor (ag-grpc:client-interceptor)
  ((token :initarg :token :reader auth-token)))

(defmethod ag-grpc:client-interceptor-call-start ((i auth-interceptor) call-info)
  ;; Add auth token to metadata
  (format t "Calling ~A with auth~%" (getf call-info :method))
  nil)
```

## Server Reflection

Enable runtime service discovery for tools like grpcurl:

```lisp
;; Enable reflection on server
(ag-grpc:server-enable-reflection server)

;; Now tools can discover services:
;; grpcurl -plaintext localhost:50051 list
;; grpcurl -plaintext localhost:50051 describe my.Service
```

## Rich Error Details

Return structured error information beyond status codes:

```lisp
;; Signal error with details
(error (ag-grpc:make-rich-status-error
         ag-grpc:+grpc-status-invalid-argument+
         "Invalid email format"
         (ag-grpc:make-error-info "INVALID_FORMAT" "myapp.example.com"
                                   '(("field" . "email")
                                     ("expected" . "valid email address")))))

;; With retry information
(error (ag-grpc:make-rich-status-error
         ag-grpc:+grpc-status-resource-exhausted+
         "Rate limit exceeded"
         (ag-grpc:make-retry-info 30)))  ; retry after 30 seconds

;; Extract details from error
(handler-case
    (make-rpc-call)
  (ag-grpc:grpc-status-error (e)
    (let ((status (ag-grpc:extract-status-details e)))
      (when status
        (format t "Error: ~A~%" (ag-grpc:rpc-status-message status))))))
```

## Async/Futures API

Make non-blocking gRPC calls with futures for concurrent operations:

### Basic Async Calls

```lisp
;; Make an async unary call
(defvar *future* (ag-grpc:call-unary-async channel method request
                                            :response-type 'response))

;; Do other work while call is in progress...

;; Block and get result when ready
(defvar *response* (ag-grpc:future-get *future* :timeout 30))
```

### Callbacks

```lisp
;; Use callbacks for fully async processing
(ag-grpc:call-unary-async channel method request
                           :response-type 'response
                           :on-success (lambda (r) (process-response r))
                           :on-error (lambda (e) (log-error e)))

;; Chain operations with then
(ag-grpc:future-then future
                      (lambda (response) (extract-data response))
                      (lambda (error) (handle-error error)))

;; Error handling with catch
(ag-grpc:future-catch future
                       (lambda (e) (recover-from-error e)))

;; Finally - runs regardless of outcome
(ag-grpc:future-finally future
                         (lambda () (cleanup-resources)))
```

### Combinators

```lisp
;; Wait for all futures to complete
(defvar *all-results* (ag-grpc:future-get
                        (ag-grpc:future-all (list future1 future2 future3))))

;; Use first result (race)
(defvar *fastest* (ag-grpc:future-get
                    (ag-grpc:future-race (list future1 future2))))

;; Use first successful result
(defvar *first-success* (ag-grpc:future-get
                          (ag-grpc:future-any (list future1 future2 future3))))
```

### Cancellation

```lisp
;; Cancel a pending call
(when (ag-grpc:future-pending-p future)
  (ag-grpc:future-cancel future))
```

## Circuit Breaker

Prevent cascade failures by detecting repeated errors and temporarily stopping requests:

### Basic Usage

```lisp
;; Create a circuit breaker
(defvar *breaker* (ag-grpc:make-circuit-breaker
                    :name "payment-service"
                    :failure-threshold 5      ; open after 5 failures
                    :success-threshold 2      ; close after 2 successes
                    :timeout 30))             ; try again after 30 seconds

;; Use with RPC calls
(ag-grpc:with-circuit-breaker (*breaker*)
  (ag-grpc:call-unary channel method request))
```

### Circuit States

| State | Behavior |
|-------|----------|
| `:closed` | Normal operation, requests pass through |
| `:open` | Requests fail immediately with `circuit-open-error` |
| `:half-open` | Limited requests allowed to test recovery |

### Monitoring

```lisp
;; Get circuit breaker stats
(multiple-value-bind (state failures successes time-until-retry)
    (ag-grpc:breaker-stats *breaker*)
  (format t "State: ~A, Failures: ~A~%" state failures))

;; Manual control
(ag-grpc:breaker-reset *breaker*)       ; Force close
(ag-grpc:breaker-force-open *breaker*)  ; Force open

;; State change callbacks
(defvar *breaker* (ag-grpc:make-circuit-breaker
                    :on-state-change (lambda (old new)
                                       (log:warn "Circuit ~A -> ~A" old new))))
```

## Hedged Requests

Reduce latency by sending the same request to multiple backends:

```lisp
;; Simple hedged call
(ag-grpc:call-unary-hedged (list channel1 channel2 channel3)
                            method request
                            :response-type 'response
                            :max-attempts 3   ; use up to 3 channels
                            :delay 0.1)       ; wait 100ms between hedges

;; With explicit policy
(defvar *hedge-policy* (ag-grpc:make-hedge-policy
                         :max-attempts 3
                         :delay 0.05))  ; 50ms

(ag-grpc:call-with-hedging *hedge-policy*
                            channels
                            method request
                            :response-type 'response)

;; Use with load balancer
(ag-grpc:call-with-hedging policy balancer method request)
```

### How Hedging Works

1. Send request to first backend
2. After `:delay` seconds, if no response, send to second backend
3. Continue until `:max-attempts` reached or response received
4. Use first successful response, cancel others

### Non-Fatal Codes

By default, these status codes don't stop hedging:
- `UNAVAILABLE`
- `RESOURCE_EXHAUSTED`

## OpenTelemetry Tracing

Integrate with OpenTelemetry for distributed tracing:

### Server-Side Tracing

```lisp
;; Enable tracing on server
(ag-grpc:enable-server-tracing server
                                (ag-grpc:make-telemetry-config
                                  :service-name "my-grpc-service"
                                  :sample-rate 1.0))
```

### Client-Side Tracing

```lisp
;; Enable tracing on channel
(ag-grpc:enable-channel-tracing channel
                                 (ag-grpc:make-telemetry-config
                                   :service-name "my-grpc-client"))
```

### Configuration Options

```lisp
(ag-grpc:make-telemetry-config
  :service-name "my-service"           ; Service name in traces
  :endpoint "http://localhost:4318/v1/traces"  ; OTLP endpoint
  :sample-rate 0.1                     ; Sample 10% of requests
  :record-request t                    ; Include request data
  :record-response t                   ; Include response data
  :propagate-context t)                ; Propagate trace context
```

### Trace Context Propagation

W3C trace context is automatically propagated in gRPC metadata:

```lisp
;; Extract trace context from incoming metadata
(multiple-value-bind (trace-id span-id flags)
    (ag-grpc:extract-trace-context metadata)
  (format t "Trace: ~A, Span: ~A~%" trace-id span-id))

;; Inject trace context into outgoing metadata
(ag-grpc:inject-trace-context metadata trace-id span-id)

;; Generate new IDs
(defvar *trace-id* (ag-grpc:generate-trace-id))
(defvar *span-id* (ag-grpc:generate-span-id))
```

### Dependencies

OpenTelemetry integration uses `cl-opentelemetry` when available:

```lisp
;; Check availability
(ag-grpc:opentelemetry-available-p)  ; => T or NIL

;; Attempt to load
(ag-grpc:try-load-opentelemetry)
```

## gRPC-Web Support

Enable browser clients to call gRPC services:

### Server-Side Setup

```lisp
;; Enable gRPC-Web on your server
(defvar *web-handler* (ag-grpc:server-enable-grpc-web server
                                                       :allow-origins '("*")))

;; Process gRPC-Web requests from your HTTP server
(defun handle-grpc-web-request (http-request)
  (multiple-value-bind (body headers status)
      (ag-grpc:grpc-web-process-request *web-handler*
                                         (request-path http-request)
                                         (request-body http-request)
                                         (request-content-type http-request)
                                         (request-headers http-request))
    (make-http-response :status status :headers headers :body body)))
```

### Client-Side (Lisp→HTTP)

```lisp
;; Create a gRPC-Web channel for HTTP/1.1 endpoints
(defvar *web-channel* (ag-grpc:make-grpc-web-channel "http://api.example.com"
                                                      :text-mode t))  ; base64

;; Frame a request
(defvar *frame* (ag-grpc:grpc-web-frame-message serialized-request))

;; Parse response
(multiple-value-bind (message trailer-p offset)
    (ag-grpc:grpc-web-parse-frame response-bytes)
  (if trailer-p
      (ag-grpc:grpc-web-parse-trailers message)
      (deserialize message)))
```

### Content Types

| Content-Type | Format | Use Case |
|--------------|--------|----------|
| `application/grpc-web` | Binary | Standard binary frames |
| `application/grpc-web-text` | Base64 | Text-only transports |

### CORS Configuration

```lisp
(ag-grpc:make-grpc-web-handler server
                                :allow-origins '("https://app.example.com")
                                :expose-headers '("grpc-status" "grpc-message"
                                                  "x-custom-header"))
```

## TLS/SSL Support

ag-gRPC supports optional TLS encryption via [cl+ssl](https://github.com/cl-plus-ssl/cl-plus-ssl).

### Client TLS

```lisp
;; Create a secure channel
(defvar *channel* (ag-grpc:make-secure-channel "api.example.com" 443))

;; Or explicitly:
(defvar *channel* (ag-grpc:make-channel "api.example.com" 443 :tls t))

;; With certificate verification:
(defvar *channel* (ag-grpc:make-channel "api.example.com" 443
                                         :tls t
                                         :tls-verify t))
```

### Server TLS

```lisp
;; Create a TLS-enabled server with certificate and key
(defvar *server* (ag-grpc:make-grpc-server 50051
                   :tls t
                   :tls-certificate "/path/to/cert.pem"
                   :tls-key "/path/to/key.pem"))

;; Register handlers and start as usual
(ag-grpc:server-start *server*)
```

Generate a self-signed certificate for testing:

```bash
openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365 -nodes
```

### TLS Requirements

TLS support requires cl+ssl and OpenSSL:

**Linux/macOS:** OpenSSL is typically pre-installed.

**Windows:** Install OpenSSL from [slproweb.com](https://slproweb.com/products/Win32OpenSSL.html) and add the bin directory to your PATH.

### Checking TLS Availability

```lisp
;; Check if TLS is available
(ag-http2:tls-available-p)  ; => T or NIL

;; Explicitly load TLS support
(ag-http2:try-load-tls)
```

If TLS is requested but cl+ssl is not available, an error will be signaled.

## Systems

### ag-proto

Protocol Buffers implementation:

- Wire format encoding/decoding (varints, fixed types, length-delimited)
- Proto3 .proto file parser
- CLOS class generation with `serialize-to-bytes` and `deserialize-from-bytes` methods
- Support for all scalar types, nested messages, enums, and repeated fields
- **Client stub generation** from service definitions (e.g., `Greeter` → `greeter-stub` class with `greeter-say-hello` method)
- **Gray stream support** for composable serialization (`sequence-input-stream`, `sequence-output-stream`)

### ag-http2

HTTP/2 implementation:

- Connection management with preface and SETTINGS exchange
- HPACK header compression with Huffman coding
- Frame serialization/deserialization (HEADERS, DATA, SETTINGS, PING, GOAWAY, etc.)
- Stream state machine per RFC 7540
- Flow control windows

### ag-grpc

gRPC protocol:

- gRPC message framing (5-byte header)
- Metadata handling with CLOS wrapper (`grpc-metadata` class)
- Status codes per gRPC specification
- **Client**: Unary, server streaming, client streaming, and bidirectional streaming RPCs
- **Server**: Handler registration, request context, all streaming patterns
- Channel abstraction over HTTP/2 connections
- **Convenience macros** for lifecycle management (`with-channel`, `with-call`, `with-grpc-server`)
- **Stream collectors** for functional stream processing
- **Response objects** with lazy metadata conversion

## CLI Tool

`ag-protoc` generates Common Lisp code from .proto files:

```bash
# Print generated code to stdout
./ag-protoc --print example.proto

# Write to file
./ag-protoc -o example.lisp example.proto

# Generate to directory
./ag-protoc --output-dir generated/ *.proto

# Load into running Lisp
./ag-protoc -p my-package --load example.proto
```

Build the CLI:

```bash
make cli
```

## Testing

Run the test suite:

```bash
make test
```

Run interoperability tests against a Go gRPC server:

```bash
make interop
```

## Dependencies

**Required:**
- [usocket](https://github.com/usocket/usocket) - Portable socket library
- [trivial-utf-8](https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8) - UTF-8 encoding
- [ieee-floats](https://github.com/marijnh/ieee-floats) - IEEE 754 float encoding
- [trivial-gray-streams](https://github.com/trivial-gray-streams/trivial-gray-streams) - Gray stream support
- [bordeaux-threads](https://github.com/sionescu/bordeaux-threads) - Portable threading (for timeouts)
- [iparse](https://github.com/atgreen/iparse) - Parser combinator library
- [clingon](https://github.com/dnaeon/clingon) - CLI framework (for ag-protoc)
- [version-string](https://github.com/atgreen/cl-version-string) - Version string generation
- [chipz](https://github.com/froydnj/chipz) - Decompression library (for gzip)
- [salza2](https://github.com/xach/salza2) - Compression library (for gzip)

**Optional:**
- [cl+ssl](https://github.com/cl-plus-ssl/cl-plus-ssl) - TLS/SSL support (requires OpenSSL)

## Supported Implementations

Tested on:
- SBCL

Should work on other implementations supporting usocket.

## License

MIT License

## Author

Anthony Green <green@moxielogic.com>
