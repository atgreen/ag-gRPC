# ag-gRPC 1.3.0

**Release date:** 2026-03-31

## Security Fixes

### CLSEC-2026-0003 — TLS certificate verification disabled by default (HIGH)

`make-channel` with `:tls t` and `make-secure-channel` both defaulted
`:tls-verify` / `:verify` to NIL, meaning TLS connections did not
validate server certificates.

**Fix:** Both now default to T.  Callers who need to disable
verification (e.g., for testing with self-signed certs) can pass
`:tls-verify nil` or `:verify nil` explicitly.

**Breaking change:** Code that relied on the previous insecure default
will now fail if the server certificate is invalid.  This is
intentional -- the previous behavior was a security vulnerability.

## Acknowledgments

Security issue identified by the CLSEC (Common Lisp Security
Initiative) automated audit.
