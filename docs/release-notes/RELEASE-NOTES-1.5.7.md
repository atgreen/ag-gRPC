# ag-gRPC 1.5.7

**Release date:** 2026-06-21

## Security Fixes

- Bounded inbound HTTP/2 resource consumption against remote,
  unauthenticated denial-of-service. Three changes in `ag-http2` /
  `ag-grpc`:

  - `read-frame` now enforces `SETTINGS_MAX_FRAME_SIZE` (RFC 7540 §4.2)
    *before* allocating the frame payload, rejecting an over-large frame
    with a `FRAME_SIZE_ERROR` connection error. Previously the 24-bit
    length field was trusted directly, so a 9-byte frame header could
    force an allocation of up to 16 MiB. See
    [CL-SEC-2026-0209](https://github.com/CL-SEC/cl-sec-advisories/blob/main/advisories/CL-SEC-2026-0209.yaml).

  - The HTTP/2 receive path now caps the buffered-but-unconsumed request
    data per stream at `ag-http2:*max-receive-buffer-size*` (default
    4 MiB) and fails the connection with `ENHANCE_YOUR_CALM` once the cap
    is exceeded. Previously a peer could stream `DATA` frames without
    `END_STREAM` (the receive window was replenished as data was
    accepted) and grow per-stream buffers without limit until the process
    exhausted memory. See
    [CL-SEC-2026-0208](https://github.com/CL-SEC/cl-sec-advisories/blob/main/advisories/CL-SEC-2026-0208.yaml).

  - `percent-decode` (used to decode the peer-supplied `grpc-message`
    trailer) now tolerates malformed `%`-escapes — a truncated or
    non-hexadecimal escape is passed through literally instead of
    signalling. Previously a malicious or buggy server could abort the
    client's in-flight response handling with an uncaught error. See
    [CL-SEC-2026-0210](https://github.com/CL-SEC/cl-sec-advisories/blob/main/advisories/CL-SEC-2026-0210.yaml).

  No public API was removed. New: `ag-http2:*max-receive-buffer-size*`
  (default 4 MiB; bind to `nil` to disable the cap) and an optional
  `max-frame-size` argument to `read-frame`.

## Bug Fixes

- **Server:** tolerate an HTTP/2 connection preface that arrives split
  across multiple reads — e.g. a reverse proxy such as Caddy doing h2c
  upstream, or ordinary TCP segmentation — instead of rejecting the
  connection as having an incomplete preface.

- **Client interoperability with nghttp2 / Caddy:** unknown `SETTINGS`
  identifiers (e.g. `NO_RFC7540_PRIORITIES`, `ENABLE_CONNECT_PROTOCOL`)
  are now ignored per RFC 7540 §6.5.2 rather than raising an error, and
  the flow-control lock is acquired/released through the matching
  `bordeaux-threads-2` API.
