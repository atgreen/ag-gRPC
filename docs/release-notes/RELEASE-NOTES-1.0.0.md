# ag-gRPC 1.0.0 Release Notes

**Release Date:** January 2026

## Summary

Initial release of ag-gRPC, a pure Common Lisp implementation of gRPC, Protocol Buffers (Proto3), and HTTP/2 (RFC 7540).

## Features

### Protocol Buffers (ag-proto)

- Wire format encoding/decoding (varints, fixed types, length-delimited)
- Proto3 `.proto` file parser with full syntax support
- CLOS class generation with `serialize-to-bytes` and `deserialize-from-bytes` methods
- Support for all scalar types, nested messages, enums, and repeated fields
- Client stub generation from service definitions
- Gray stream support for composable serialization
- `ag-protoc` CLI tool for code generation

### HTTP/2 (ag-http2)

- Full HTTP/2 implementation per RFC 7540
- HPACK header compression with Huffman coding
- Stream multiplexing and flow control
- Optional TLS 1.3 support via pure-tls

### gRPC (ag-grpc)

- **Client features:**
  - All RPC types: unary, server streaming, client streaming, bidirectional streaming
  - Async API with futures for non-blocking calls
  - Retry policies with exponential backoff
  - Load balancing (round-robin, pick-first) with DNS discovery
  - Channel pooling and connection reuse
  - Circuit breaker pattern for fault tolerance
  - Request hedging for latency-sensitive calls
  - Client interceptors for middleware

- **Server features:**
  - Handler registration for all RPC types
  - Request context with metadata, deadlines, and cancellation
  - Server interceptors for logging, auth, metrics
  - Health checking (grpc.health.v1.Health)
  - Server reflection (grpc.reflection.v1alpha)
  - gRPC-Web support for browser clients

- **Common features:**
  - Message compression (gzip via salza2/chipz)
  - OpenTelemetry-compatible telemetry hooks
  - Full metadata support

## Conformance

- 359 conformance tests passing (100% pass rate)
- Tested against ConnectRPC conformance suite
- Interoperability verified with Go gRPC servers

## Installation

Using [ocicl](https://github.com/ocicl/ocicl):

```bash
ocicl install ag-grpc
```

## Quick Start

```lisp
;; Load the system
(asdf:load-system :ag-grpc)

;; Parse a .proto file and generate code
(ag-proto:compile-proto-file "hello.proto" :package :my-app)

;; Create a channel and make RPC calls
(let ((channel (ag-grpc:make-channel "localhost" 50051)))
  (unwind-protect
      (greeter-say-hello channel
                         (make-hello-request :name "World"))
    (ag-grpc:close-channel channel)))
```

## Supported Implementations

- SBCL (tested)
- Other implementations supporting usocket should work

## License

MIT License
