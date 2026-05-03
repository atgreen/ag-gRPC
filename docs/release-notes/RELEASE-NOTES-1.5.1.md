# ag-gRPC 1.5.1

**Release date:** 2026-05-03

## Bug Fixes

- Fixed unqualified `ag-http2` symbols in `server-handle-data` that
  caused `The function AG-GRPC::STREAM-DATA-BUFFER is undefined` errors
  at runtime when processing DATA frames.  The symbols `stream-data-buffer`,
  `frame-flags`, and `+flag-end-stream+` are now correctly qualified with
  the `ag-http2:` package prefix.

- Fixed `server-enable-health-checking` passing positional arguments
  instead of keyword arguments to `server-register-handler`, which caused
  `Unknown &KEY argument: :UNARY` errors when enabling the standard
  gRPC health checking service.
