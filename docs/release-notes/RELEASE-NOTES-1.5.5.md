# ag-gRPC 1.5.5

**Release date:** 2026-05-26

## Bug Fixes

- Fixed `read-frame` in `ag-http2/frames.lisp` returning `NIL` on a
  clean peer close. Every loop on top of `connection-read-frame`
  (`server-connection-loop`, and the client-side
  `channel-receive-headers` / `channel-receive-message` /
  `channel-receive-trailers`) treated the `NIL` return as "no frame
  yet, retry," busy-spinning a CPU and never exiting once the TCP
  socket gave EOF. `read-frame` now signals `END-OF-FILE` on a
  zero-byte read and `HTTP2-FRAME-ERROR` on a truncated 1–8 byte
  frame header. The server connection loop already handled
  `END-OF-FILE`; `call-unary` now also catches it and maps it to
  `grpc-status UNAVAILABLE` so client callers see a meaningful gRPC
  error and can retry on a fresh channel.
