# ag-gRPC

A pure Common Lisp implementation of gRPC, Protocol Buffers, and HTTP/2.

## Overview

ag-gRPC provides a complete gRPC client stack written entirely in portable Common Lisp. It includes:

- **ag-proto** - Protocol Buffers (Proto3) implementation with .proto file parsing and code generation
- **ag-http2** - HTTP/2 protocol implementation (RFC 7540) with HPACK header compression (RFC 7541)
- **ag-grpc** - gRPC protocol implementation for unary RPCs

## Features

- Pure Common Lisp - minimal foreign dependencies
- Proto3 wire format encoding/decoding
- .proto file parser with code generation to CLOS classes
- **Generated client stubs** - type-safe RPC methods from service definitions
- Full HPACK implementation including Huffman coding
- HTTP/2 client with stream multiplexing and flow control
- gRPC unary and **server streaming** RPC calls
- Optional TLS/SSL support (via cl+ssl)
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
- Metadata handling (headers and trailers)
- Status codes per gRPC specification
- Unary RPC calls
- **Server streaming RPC** with iterator/callback support
- Channel abstraction over HTTP/2 connections

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

- Client-side only (no server implementation yet)
- Server streaming only (no client streaming or bidirectional yet)
- No load balancing or service discovery
- No deadline propagation

## License

MIT License

## Author

Anthony Green <green@moxielogic.com>
