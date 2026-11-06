# ag-gRPC 1.5.10

**Release date:** 2026-11-06

## Bug Fixes

Four defects in message compression and connection teardown, surfaced by a
long-running node-agent deployment.

- **Decompression no longer allocates the whole size cap for every message.**
  `bounded-inflate` allocated a `(1+ *max-decompressed-size*)` buffer - 16 MiB
  by default - for every compressed message, however small. On a busy
  bidirectional stream this is enormous garbage: roughly ten small messages a
  second churned over twenty gigabytes a minute, aging short-lived objects into
  old generations and helping run the heap out of memory. The gzip-bomb defence
  is unchanged, but the output buffer now starts near the compressed size and
  doubles as needed (clamped to the cap), so an ordinary message allocates
  roughly its real size while a bomb is still rejected the instant its output
  passes the cap. See **CL-SEC-2026-0223**.

- **Empty messages compress correctly.** salza2 compresses empty input into a
  gzip member whose ISIZE is `0x8000`, so an empty gzipped message decompressed
  to 32 KB of zeros. An empty protobuf message (a defaulted heartbeat, say) is
  common; `gzip-compress` and the `deflate` path now emit a valid 20-octet empty
  gzip member (and an empty raw-deflate stream) for empty input.

- **Compression output is collected by block.** The salza2 callback copied each
  output block one octet at a time through `vector-push-extend`; it now copies
  whole blocks with `replace` into one right-sized vector - markedly faster and
  far less allocation on the hot path (for an 85 KB message, 2.82 ms / 215 KB
  versus 4.17 ms / 730 KB).

- **An interrupted frame write is no longer resent on close.** SBCL resets an
  fd-stream's output buffer only after `write(2)` returns; a thread unwound by
  `sb-thread:interrupt-thread` in that window left frame bytes the peer had
  already received in the buffer, and `connection-close`'s farewell `GOAWAY`
  flush sent them a second time. A repeated DATA frame with END_STREAM crashed
  grpc-go servers before v1.85.0 (grpc/grpc-go#9533). `connection-close` now
  discards any unsent buffered output before writing `GOAWAY`.

## Tests

Added a regression guard for the flow-control lock's bordeaux-threads v2 API
usage (issue #17), round-trip tests covering the growing decompression buffer
and empty/non-empty gzip and deflate messages, and a test that a staged frame is
dropped rather than flushed on close. Full suite green (323 checks).

## Compatibility

No API changes.
