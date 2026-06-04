# ag-gRPC 1.5.6

**Release date:** 2026-06-04

## Security Fixes

- Hardened HPACK decoding and CONTINUATION accumulation against the
  HTTP/2 Bomb class of denial-of-service attack publicly disclosed as
  CVE-2026-49975 (Apache httpd, nginx, Envoy, Microsoft IIS, Cloudflare
  Pingora). See [CL-SEC-2026-0205](https://github.com/atgreen/CLSEC/blob/main/advisories/CL-SEC-2026-0205.yaml).

  Three changes in `ag-http2`:

  - `hpack-decode` now enforces the locally-advertised
    `SETTINGS_MAX_HEADER_LIST_SIZE` (default 8192). The decoder
    accumulates `name + value + 32` per header (RFC 7541 §4.1) and
    signals a connection-level `COMPRESSION_ERROR` if the running
    total exceeds the limit, instead of materializing the full
    decoded header list first.
  - `hpack-decode` now clamps peer-issued HPACK dynamic-table-size
    updates to the locally-advertised `SETTINGS_HEADER_TABLE_SIZE`
    (RFC 7541 §6.3). An update above the limit is a
    `COMPRESSION_ERROR`.
  - `process-frame` now bounds the encoded header block size that may
    accumulate across `HEADERS` + chained `CONTINUATION` frames at
    `max(16384, 2 × SETTINGS_MAX_HEADER_LIST_SIZE)`. Exceeding that
    triggers an `ENHANCE_YOUR_CALM` connection error, mitigating the
    CONTINUATION-flood half of the attack and bounding the per-stream
    memory footprint while a header block is being assembled.

  The defaults trip well below memory-exhaustion territory; servers
  that need to accept very large header lists can raise their
  `SETTINGS_MAX_HEADER_LIST_SIZE`, which is propagated to the decoder
  automatically when the connection is constructed.

  No public API was removed. `make-hpack-decoder` gained an optional
  `:max-header-list-size` keyword (default 8192; pass `nil` to opt
  out).
