# ag-gRPC 1.2.0 Release Notes

**Release Date:** March 2026

## Summary

This release adds proto `map<K,V>` field support, channel keepalive, and several bug fixes for HTTP/2 flow control and streaming.

## New Features

### Protocol Buffers (ag-proto)
- **`map<K,V>` field support** - `ag-protoc` now generates CLOS slots, serialization, and deserialization for proto map fields. Maps are represented as alists and encoded per the proto wire format. All key types (`string`, `int32`, `int64`, `bool`, etc.) and arbitrary value types are supported. (#13)

### gRPC (ag-grpc)
- **Channel keepalive** - Channels can now be configured with keepalive parameters to send periodic HTTP/2 PINGs, detecting dead connections automatically. (#12)

## Bug Fixes

- Fix HTTP/2 flow control threading bug in `connection-send-data` (#10)
- Fix double data append and add `reader-thread-active-p` for single-threaded clients
- Fix unmatched close parenthesis in `server-handle-data`
- Fix missing `stream-cancel-context` for server streams (#7)

## Supported Implementations

- SBCL (tested)
- Other implementations supporting usocket should work
