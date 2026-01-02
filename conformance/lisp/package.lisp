;;;; package.lisp - Package definition for conformance protos

(defpackage #:conformance-proto
  (:use #:cl)
  ;; Shadow CL and SBCL symbols that conflict with proto message/field names
  ;; Note: ERROR is no longer shadowed since the class is now PROTO-ERROR
  (:shadow #:method #:struct)
  (:export
   ;; struct.lisp (google.protobuf)
   #:+null-value-null-value+
   #:struct
   #:value
   #:list-value

   ;; empty.lisp (google.protobuf)
   #:empty

   ;; any.lisp (google.protobuf)
   #:any

   ;; config.lisp
   #:+http-version-http-version-unspecified+
   #:+http-version-http-version-1+
   #:+http-version-http-version-2+
   #:+http-version-http-version-3+
   #:+protocol-protocol-unspecified+
   #:+protocol-protocol-connect+
   #:+protocol-protocol-grpc+
   #:+protocol-protocol-grpc-web+
   #:+codec-codec-unspecified+
   #:+codec-codec-proto+
   #:+codec-codec-json+
   #:+codec-codec-text+
   #:+compression-compression-unspecified+
   #:+compression-compression-identity+
   #:+compression-compression-gzip+
   #:+compression-compression-br+
   #:+compression-compression-zstd+
   #:+compression-compression-deflate+
   #:+compression-compression-snappy+
   #:+stream-type-stream-type-unspecified+
   #:+stream-type-stream-type-unary+
   #:+stream-type-stream-type-client-stream+
   #:+stream-type-stream-type-server-stream+
   #:+stream-type-stream-type-half-duplex-bidi-stream+
   #:+stream-type-stream-type-full-duplex-bidi-stream+
   #:+code-code-unspecified+
   #:+code-code-canceled+
   #:+code-code-unknown+
   #:+code-code-invalid-argument+
   #:+code-code-deadline-exceeded+
   #:+code-code-not-found+
   #:+code-code-already-exists+
   #:+code-code-permission-denied+
   #:+code-code-resource-exhausted+
   #:+code-code-failed-precondition+
   #:+code-code-aborted+
   #:+code-code-out-of-range+
   #:+code-code-unimplemented+
   #:+code-code-internal+
   #:+code-code-unavailable+
   #:+code-code-data-loss+
   #:+code-code-unauthenticated+
   #:config
   #:features
   #:config-case
   #:tls-creds

   ;; service.lisp
   #:unary-response-definition
   #:stream-response-definition
   #:unary-request
   #:unary-response
   #:idempotent-unary-request
   #:idempotent-unary-response
   #:server-stream-request
   #:server-stream-response
   #:client-stream-request
   #:client-stream-response
   #:bidi-stream-request
   #:bidi-stream-response
   #:unimplemented-request
   #:unimplemented-response
   #:conformance-payload
   #:proto-error  ; renamed from error to avoid CL:ERROR conflict
   #:header
   #:raw-http-request
   #:message-contents
   #:stream-contents
   #:raw-http-response

   ;; client_compat.lisp
   #:client-compat-request
   #:client-compat-response
   #:client-response-result
   #:client-error-result
   #:wire-details

   ;; Renamed accessors for shadowed symbols
   #:proto-error
   #:proto-method))
