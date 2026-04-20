# ag-gRPC 1.5.0

**Release date:** 2026-04-20

## Security Fixes

### CL-SEC-2026-0203 — Unbounded recursion in nested message deserialization (HIGH)

Generated `deserialize-from-bytes` methods recursed into nested message
fields without any depth limit.  A crafted protobuf payload with deeply
nested embedded messages could exhaust the call stack (CWE-674).

**Fix:** A new variable `ag-proto:*max-recursion-depth*` (default: 100,
matching the protobuf spec) is checked before each recursive
deserialization call.  Exceeding the limit signals `wire-format-error`.

```lisp
(setf ag-proto:*max-recursion-depth* 50)   ; stricter
(setf ag-proto:*max-recursion-depth* nil)  ; disable (not recommended)
```

### CL-SEC-2026-0204 — No decompressed size limit in gRPC message decompression (HIGH)

The gzip and deflate decompression paths in `framing.lisp` expanded the
full payload into memory without checking the output size.  A small
compressed message could decompress to gigabytes, exhausting memory
(CWE-409, "gzip bomb").

**Fix:** A new variable `ag-grpc:*max-decompressed-size*` (default:
16 MiB) checks the decompressed output size before returning it.

```lisp
(setf ag-grpc:*max-decompressed-size* (* 64 1024 1024))  ; 64 MiB
(setf ag-grpc:*max-decompressed-size* nil)                ; disable (not recommended)
```

## New Features

- **UTF-8 validation for string fields.** Set `ag-proto:*validate-utf8*`
  to `t` to reject invalid UTF-8 in protobuf string fields during
  deserialization (spec-compliant strictness).  Off by default for
  backwards compatibility.

- **Zero-copy bytes field decoding.** Set `ag-proto:*zero-copy-bytes*`
  to `t` to have `:bytes` fields decoded as displaced arrays sharing
  storage with the receive buffer, eliminating allocation and copying on
  the decode path.  Off by default because displaced arrays differ from
  simple arrays in type and ownership semantics.

- **Timeout header overflow protection.**  `parse-grpc-timeout` now
  validates the 1-8 digit spec limit and caps the result at
  `*max-grpc-timeout-seconds*` (default: 1 year) to prevent numeric
  overflow when added to the current time.

- **Service registration helpers.**  Code generation now emits a
  `REGISTER-<SERVICE>-SERVICE` function per service that bulk-registers
  all RPC handlers with a server in a single call.

## Bug Fixes

- Restored `ag-grpc:check-response` as a deprecated alias for
  `ensure-response` to avoid breaking existing callers.
