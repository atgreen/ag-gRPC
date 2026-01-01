;;;; package.lisp - Package definition for ag-proto

(defpackage #:ag-proto
  (:use #:cl)
  (:export
   ;; Wire format encoding/decoding
   #:encode-varint
   #:decode-varint
   #:encode-varint-to-stream
   #:decode-varint-from-stream
   #:encode-fixed32
   #:decode-fixed32
   #:encode-fixed32-to-stream
   #:decode-fixed32-from-stream
   #:encode-fixed64
   #:decode-fixed64
   #:encode-fixed64-to-stream
   #:decode-fixed64-from-stream
   #:zigzag-encode
   #:zigzag-decode
   #:encode-length-delimited
   #:decode-length-delimited
   #:encode-length-delimited-to-stream
   #:decode-length-delimited-from-stream

   ;; Signed fixed encoding
   #:encode-sfixed32
   #:decode-sfixed32
   #:encode-sfixed64
   #:decode-sfixed64

   ;; Float encoding
   #:encode-float32
   #:decode-float32
   #:encode-float64
   #:decode-float64

   ;; String encoding
   #:encode-string
   #:decode-string
   #:encode-string-to-stream
   #:decode-string-from-stream

   ;; Field tag encoding
   #:make-field-tag
   #:parse-field-tag
   #:encode-field-tag
   #:decode-field-tag
   #:+wire-type-varint+
   #:+wire-type-fixed64+
   #:+wire-type-length-delimited+
   #:+wire-type-fixed32+

   ;; Descriptor classes
   #:proto-file-descriptor
   #:proto-message-descriptor
   #:proto-field-descriptor
   #:proto-enum-descriptor
   #:proto-enum-value-descriptor
   #:proto-service-descriptor
   #:proto-method-descriptor

   ;; File descriptor accessors
   #:proto-file-name
   #:proto-file-package
   #:proto-file-syntax
   #:proto-file-imports
   #:proto-file-messages
   #:proto-file-enums
   #:proto-file-services
   #:proto-file-options

   ;; Message descriptor accessors
   #:proto-message-name
   #:proto-message-full-name
   #:proto-message-fields
   #:proto-message-nested-messages
   #:proto-message-nested-enums
   #:proto-message-oneofs
   #:proto-message-options

   ;; Field descriptor accessors
   #:proto-field-name
   #:proto-field-number
   #:proto-field-type
   #:proto-field-type-name
   #:proto-field-label
   #:proto-field-default-value
   #:proto-field-options

   ;; Enum descriptor accessors
   #:proto-enum-name
   #:proto-enum-full-name
   #:proto-enum-values
   #:proto-enum-options

   ;; Enum value descriptor accessors
   #:proto-enum-value-name
   #:proto-enum-value-number
   #:proto-enum-value-options

   ;; Service descriptor accessors
   #:proto-service-name
   #:proto-service-full-name
   #:proto-service-methods
   #:proto-service-options

   ;; Method descriptor accessors
   #:proto-method-name
   #:proto-method-input-type
   #:proto-method-output-type
   #:proto-method-client-streaming
   #:proto-method-server-streaming
   #:proto-method-options

   ;; Parser
   #:parse-proto-file
   #:parse-proto-string

   ;; Code generation
   #:generate-lisp-code
   #:compile-proto-file
   #:compile-proto-string

   ;; Runtime
   #:proto-message
   #:serialize-to-stream
   #:serialize-to-bytes
   #:deserialize-from-stream
   #:deserialize-from-bytes

   ;; Conditions
   #:proto-error
   #:wire-format-error
   #:proto-parse-error))
