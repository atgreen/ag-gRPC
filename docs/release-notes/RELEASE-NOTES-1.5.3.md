# ag-gRPC 1.5.3

**Release date:** 2026-05-04

## Bug Fixes

- Fixed unqualified `release-lock` and `acquire-lock` calls in
  `ag-http2/connection.lisp` that caused `The function
  AG-HTTP2::RELEASE-LOCK is undefined` errors when HTTP/2 flow control
  activated during long-running gRPC calls.
