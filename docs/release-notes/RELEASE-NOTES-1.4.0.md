# ag-gRPC 1.4.0

**Release date:** 2026-04-04

## Security Fixes

### CL-SEC-2026-0197 — Unbounded allocation from crafted length-delimited fields (HIGH)

The protobuf wire format decoder allocated byte vectors based on a
varint-decoded length read from the input stream.  A malicious message
could specify an enormous length (up to 2^63) for a string, bytes, or
embedded message field, causing the decoder to attempt allocating
gigabytes of memory before reading any payload data (CWE-770).

Additionally, `decode-field-tag-from-stream` lacked the varint overflow
check present in the other varint decoders, allowing a stream of
continuous high-bit-set bytes to cause unbounded loop iterations.

**Fix:**

- A new variable `ag-proto:*max-message-size*` (default: 64 MB) limits
  the size of any single length-delimited field.  Exceeding the limit
  signals `wire-format-error` before allocation.
- `decode-field-tag-from-stream` now rejects varints longer than 10
  bytes, matching `decode-varint` and `decode-varint-from-stream`.

**Breaking change:** Messages containing a string, bytes, or embedded
message field larger than 64 MB will now be rejected.  To raise or
remove the limit:

```lisp
;; Raise to 256 MB
(setf ag-proto:*max-message-size* (* 256 1024 1024))

;; Disable the check entirely (not recommended)
(setf ag-proto:*max-message-size* nil)
```

## Bug Fixes

- Fixed codegen emitting the removed symbol `CALL-SERVER-STREAMING`
  instead of `CALL-SERVER-STREAM` for server-streaming RPC stubs.
  Users who generated stubs containing server-streaming methods with
  ag-proto 1.3.x should regenerate them.
- Fixed interop and conformance tests to use the current streaming API.
