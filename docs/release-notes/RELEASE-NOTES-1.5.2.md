# ag-gRPC 1.5.2

**Release date:** 2026-05-04

## Bug Fixes

- Fixed unqualified `deserialize-from-bytes` calls in `server.lisp`
  and `call.lisp` that caused `The function AG-GRPC::DESERIALIZE-FROM-BYTES
  is undefined` errors at runtime when processing streaming DATA frames
  or receiving server-streaming responses.
