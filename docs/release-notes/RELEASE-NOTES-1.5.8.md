# ag-gRPC 1.5.8

**Release date:** 2026-08-07

## Security Fixes

- Bounded gRPC message decompression against a remote, unauthenticated
  gzip/deflate-bomb denial-of-service. `ag-grpc:*max-decompressed-size*`
  (added in 1.5.0) was previously checked *after* the payload had been
  fully inflated, so a small compressed message (~1000:1 expansion) could
  still force a multi-gigabyte transient allocation before the limit was
  applied. Decompression is now bounded *during* inflation: a new
  `bounded-inflate` helper drives `chipz` into a fixed
  `*max-decompressed-size*`-sized buffer and rejects an over-limit stream
  with a `grpc-error` before that memory is committed. Both the `gzip`
  and `deflate` paths, on request and response decoding, are covered. See
  [CL-SEC-2026-0212](https://github.com/CL-SEC/cl-sec-advisories/blob/main/advisories/CL-SEC-2026-0212.yaml)
  (completing
  [CL-SEC-2026-0204](https://github.com/CL-SEC/cl-sec-advisories/blob/main/advisories/CL-SEC-2026-0204.yaml)).

  No public API changed; `ag-grpc:*max-decompressed-size*` retains its
  16 MiB default (bind to `nil` to disable the bound).

## Bug Fixes

- **HTTP/2 HPACK:** adding a header whose size exceeds the dynamic-table
  maximum now empties the table and adds nothing, per RFC 7541 §4.4,
  instead of faulting. Previously such an entry (reachable from
  peer-supplied headers) drove the eviction loop to dereference an empty
  table and abort the connection with an array-index error. See
  [CL-SEC-2026-0213](https://github.com/CL-SEC/cl-sec-advisories/blob/main/advisories/CL-SEC-2026-0213.yaml).
