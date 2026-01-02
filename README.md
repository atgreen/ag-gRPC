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
| Unary RPC | 82 | ✅ Pass |
| Client Streaming | 23 | ✅ Pass |
| Server Streaming | 59 | ✅ Pass |
| Bidi Streaming (half-duplex) | 25 | ✅ Pass |
| **Total** | **189** | **100%** |

### Feature Support Matrix

| Feature | Client | Server |
|---------|--------|--------|
| Unary RPC | ✅ | ✅ |
| Client Streaming | ✅ | ✅ |
| Server Streaming | ✅ | ✅ |
| Bidirectional Streaming | ✅ | ✅ |
| Metadata | ✅ | ✅ |
| Deadlines/Timeouts | ✅ | ✅ |
| Cancellation | ✅ | — |
| TLS (h2) | ✅ | — |
| Plaintext (h2c) | ✅ | ✅ |
| Compression (gzip) | — | — |

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
- Gray stream integration for composable I/O
- Optional TLS/SSL support (via cl+ssl)
- **gRPC Server**: handler registration, request context, streaming support
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

## TLS/SSL Support

ag-gRPC supports optional TLS encryption via [cl+ssl](https://github.com/cl-plus-ssl/cl-plus-ssl).

### Using TLS

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

**Optional:**
- [cl+ssl](https://github.com/cl-plus-ssl/cl-plus-ssl) - TLS/SSL support (requires OpenSSL)

## Supported Implementations

Tested on:
- SBCL

Should work on other implementations supporting usocket.

## Limitations

Current limitations (contributions welcome!):

- No load balancing or service discovery
- No deadline propagation
- Server TLS not yet implemented

## License

MIT License

## Author

Anthony Green <green@moxielogic.com>
