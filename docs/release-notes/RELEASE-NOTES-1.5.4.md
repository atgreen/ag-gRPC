# ag-gRPC 1.5.4

**Release date:** 2026-05-05

## Bug Fixes

- Fixed unqualified `condition-wait` call in `ag-http2/connection.lisp`
  that caused a type error (`BT2:LOCK is not of type SB-THREAD:MUTEX`)
  when HTTP/2 flow control activated during high-throughput streaming.
