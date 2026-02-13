# ag-gRPC 1.1.0 Release Notes

**Release Date:** February 2026

## Summary

This release brings mTLS support, improved concurrency and thread-safety, cancellation via cl-cancel, a new `--class-prefix` code generation option, CI infrastructure, and numerous bug fixes.

## Breaking Changes

- **cl-context replaced by cl-cancel:** The cancellation and timeout system has been migrated from `cl-context` to `cl-cancel`. Users relying on `cl-context` APIs for stream timeouts or cancellation must update to the `cl-cancel` equivalents.
- **Deprecated context values removed:** The context values feature deprecated in 1.0.0 has been removed.

## New Features

### Security
- **mTLS support** for both client and server connections

### Code Generation (ag-proto)
- **`--class-prefix` option** for `ag-protoc`, allowing generated CLOS class names to be prefixed for namespace isolation
- Message-specific accessor naming when using class prefix, avoiding symbol conflicts

### Server (ag-grpc)
- **Max concurrent streams enforcement** per connection
- **Stream cleanup** on connection close
- **Stream message buffer** for async streaming handlers
- **Async streaming handler multiplexing** fixes

### Concurrency
- **Thread-safe connection-local state** (Day 2 concurrency fix)
- **HTTP/2 cleanup callback** invocation improvements

### Infrastructure
- **GitHub Actions CI** testing workflow across Ubuntu, macOS, and Windows
- **Server configuration options** documented in README

## Bug Fixes

- Fix bidirectional streaming for server-initiated responses
- Fix thread-safety with connection-local state
- Fix multiplexing with async streaming handlers
- Fix accessor naming to be message-specific when using class prefix
- Fix `--class-prefix` not applying to RPC response types
- Fix codegen test failures with hyphenated class names
- Fix compilation errors in symbol exports and naming
- Fix type error in test (thanks @brown)
- Fix `grpc-web.lisp` missing babel dependency
- Fix `ag-http2` missing dependency on `trivial-utf-8`
- Fix thread-per-connection, trailers-only, and header ordering issues
- Update to latest iparse and handle metaobject format

## Dependency Changes

- Replaced `cl-context` with `cl-cancel` for cancellation and deadline tracking
- Added `with-stream-timeout` macro for stream-level timeout management

## Supported Implementations

- SBCL (tested)
- Other implementations supporting usocket should work
