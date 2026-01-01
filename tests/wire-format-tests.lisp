;;;; wire-format-tests.lisp - Tests for Protocol Buffers wire format

(in-package #:ag-grpc-tests)

(in-suite wire-format-tests)

;;;; ========================================================================
;;;; Varint Tests
;;;; ========================================================================

(test varint-encode-zero
  "Encoding zero should produce a single zero byte"
  (is (equal '(0) (ag-proto:encode-varint 0))))

(test varint-encode-small
  "Encoding values < 128 should produce single byte"
  (is (equal '(1) (ag-proto:encode-varint 1)))
  (is (equal '(127) (ag-proto:encode-varint 127))))

(test varint-encode-two-bytes
  "Encoding 128-16383 should produce two bytes"
  (is (equal '(#x80 #x01) (ag-proto:encode-varint 128)))
  (is (equal '(#xac #x02) (ag-proto:encode-varint 300))))

(test varint-encode-large
  "Encoding larger values"
  ;; 150 = 0x96 0x01
  (is (equal '(#x96 #x01) (ag-proto:encode-varint 150)))
  ;; 16384 = 0x80 0x80 0x01
  (is (equal '(#x80 #x80 #x01) (ag-proto:encode-varint 16384))))

(test varint-roundtrip
  "Varint encode/decode roundtrip"
  (dolist (value (list 0 1 127 128 255 256 300 16383 16384 65535
                       1000000 (expt 2 31) (expt 2 63)))
    (let ((encoded (ag-proto:encode-varint value)))
      (is (= value (ag-proto:decode-varint encoded))
          "Roundtrip failed for ~A" value))))

(test varint-decode-bytes-consumed
  "Decode-varint should return correct bytes consumed"
  (multiple-value-bind (val consumed) (ag-proto:decode-varint '(#x96 #x01))
    (is (= 150 val))
    (is (= 2 consumed)))
  (multiple-value-bind (val consumed) (ag-proto:decode-varint '(#x80 #x80 #x01))
    (is (= 16384 val))
    (is (= 3 consumed))))

;;;; ========================================================================
;;;; Zigzag Tests
;;;; ========================================================================

(test zigzag-encode
  "Zigzag encoding maps signed to unsigned"
  (is (= 0 (ag-proto:zigzag-encode 0)))
  (is (= 1 (ag-proto:zigzag-encode -1)))
  (is (= 2 (ag-proto:zigzag-encode 1)))
  (is (= 3 (ag-proto:zigzag-encode -2)))
  (is (= 4 (ag-proto:zigzag-encode 2)))
  (is (= 4294967294 (ag-proto:zigzag-encode 2147483647)))
  (is (= 4294967295 (ag-proto:zigzag-encode -2147483648))))

(test zigzag-decode
  "Zigzag decoding maps unsigned to signed"
  (is (= 0 (ag-proto:zigzag-decode 0)))
  (is (= -1 (ag-proto:zigzag-decode 1)))
  (is (= 1 (ag-proto:zigzag-decode 2)))
  (is (= -2 (ag-proto:zigzag-decode 3)))
  (is (= 2 (ag-proto:zigzag-decode 4))))

(test zigzag-roundtrip
  "Zigzag encode/decode roundtrip"
  (dolist (value '(0 1 -1 2 -2 127 -128 32767 -32768
                   2147483647 -2147483648))
    (is (= value (ag-proto:zigzag-decode (ag-proto:zigzag-encode value)))
        "Zigzag roundtrip failed for ~A" value)))

;;;; ========================================================================
;;;; Fixed32 Tests
;;;; ========================================================================

(test fixed32-encode
  "Fixed32 encoding produces little-endian bytes"
  (is (equal '(0 0 0 0) (ag-proto:encode-fixed32 0)))
  (is (equal '(1 0 0 0) (ag-proto:encode-fixed32 1)))
  (is (equal '(#xff #xff #xff #xff) (ag-proto:encode-fixed32 #xffffffff)))
  (is (equal '(#x78 #x56 #x34 #x12) (ag-proto:encode-fixed32 #x12345678))))

(test fixed32-roundtrip
  "Fixed32 encode/decode roundtrip"
  (dolist (value '(0 1 255 256 65535 16777215 #xffffffff #x12345678))
    (let ((encoded (ag-proto:encode-fixed32 value)))
      (is (= value (ag-proto:decode-fixed32 encoded))
          "Fixed32 roundtrip failed for ~A" value))))

;;;; ========================================================================
;;;; Fixed64 Tests
;;;; ========================================================================

(test fixed64-encode
  "Fixed64 encoding produces little-endian bytes"
  (is (equal '(0 0 0 0 0 0 0 0) (ag-proto:encode-fixed64 0)))
  (is (equal '(1 0 0 0 0 0 0 0) (ag-proto:encode-fixed64 1)))
  (is (equal '(#xef #xcd #xab #x90 #x78 #x56 #x34 #x12)
             (ag-proto:encode-fixed64 #x1234567890abcdef))))

(test fixed64-roundtrip
  "Fixed64 encode/decode roundtrip"
  (dolist (value '(0 1 #xffffffff #x100000000 #xffffffffffffffff #x1234567890abcdef))
    (let ((encoded (ag-proto:encode-fixed64 value)))
      (is (= value (ag-proto:decode-fixed64 encoded))
          "Fixed64 roundtrip failed for ~A" value))))

;;;; ========================================================================
;;;; Signed Fixed Tests
;;;; ========================================================================

(test sfixed32-roundtrip
  "Sfixed32 encode/decode roundtrip with signed values"
  (dolist (value '(0 1 -1 127 -128 32767 -32768 2147483647 -2147483648))
    (let ((encoded (ag-proto:encode-sfixed32 value)))
      (is (= value (ag-proto:decode-sfixed32 encoded))
          "Sfixed32 roundtrip failed for ~A" value))))

(test sfixed64-roundtrip
  "Sfixed64 encode/decode roundtrip with signed values"
  (dolist (value '(0 1 -1 2147483647 -2147483648
                   9223372036854775807 -9223372036854775808))
    (let ((encoded (ag-proto:encode-sfixed64 value)))
      (is (= value (ag-proto:decode-sfixed64 encoded))
          "Sfixed64 roundtrip failed for ~A" value))))

;;;; ========================================================================
;;;; Float Tests
;;;; ========================================================================

(test float32-roundtrip
  "Float32 encode/decode roundtrip"
  (dolist (value (list 0.0f0 1.0f0 -1.0f0 3.14159f0 -3.14159f0
                       1.0e10 1.0e-10))
    (let* ((encoded (ag-proto:encode-float32 value))
           (decoded (ag-proto:decode-float32 encoded)))
      (is (< (abs (- value decoded)) 1e-5)
          "Float32 roundtrip failed for ~A, got ~A" value decoded))))

(test float64-roundtrip
  "Float64 encode/decode roundtrip"
  (dolist (value '(0.0d0 1.0d0 -1.0d0 3.141592653589793d0
                   1.0d100 1.0d-100))
    (let* ((encoded (ag-proto:encode-float64 value))
           (decoded (ag-proto:decode-float64 encoded)))
      (is (< (abs (- value decoded)) 1d-10)
          "Float64 roundtrip failed for ~A, got ~A" value decoded))))

;;;; ========================================================================
;;;; Length-Delimited Tests
;;;; ========================================================================

(test length-delimited-encode
  "Length-delimited encoding prepends length"
  (is (equal '(0) (ag-proto:encode-length-delimited #())))
  (is (equal '(3 1 2 3) (ag-proto:encode-length-delimited #(1 2 3))))
  ;; 128 bytes should have 2-byte length prefix
  (let* ((data (make-array 128 :initial-element 42))
         (encoded (ag-proto:encode-length-delimited data)))
    (is (= (+ 2 128) (length encoded)))))

(test length-delimited-roundtrip
  "Length-delimited encode/decode roundtrip"
  (dolist (data (list #() #(1 2 3) (make-array 100 :initial-element 0)
                      (make-array 300 :initial-element 255)))
    (let* ((encoded (coerce (ag-proto:encode-length-delimited data) 'vector))
           (decoded (ag-proto:decode-length-delimited encoded)))
      (is (equalp (coerce data 'vector) decoded)
          "Length-delimited roundtrip failed"))))

;;;; ========================================================================
;;;; String Tests
;;;; ========================================================================

(test string-encode-ascii
  "String encoding with ASCII"
  (let ((encoded (ag-proto:encode-string "hello")))
    (is (= 6 (length encoded)))  ; 1 byte length + 5 bytes data
    (is (= 5 (first encoded)))))

(test string-roundtrip
  "String encode/decode roundtrip"
  (dolist (s '("" "hello" "Hello, World!" "αβγδ" "日本語"))
    (let ((encoded (coerce (ag-proto:encode-string s) 'vector)))
      (is (string= s (ag-proto:decode-string encoded))
          "String roundtrip failed for ~S" s))))

(test utf8-multibyte
  "UTF-8 encoding handles multibyte characters"
  (let* ((s "α")  ; Greek alpha, U+03B1
         (utf8 (ag-proto::string-to-utf8 s)))
    (is (= 2 (length utf8)))  ; Should be 2 bytes
    (is (= #xce (aref utf8 0)))
    (is (= #xb1 (aref utf8 1)))))

;;;; ========================================================================
;;;; Field Tag Tests
;;;; ========================================================================

(test make-field-tag
  "Field tag creation"
  (is (= #x08 (ag-proto:make-field-tag 1 ag-proto:+wire-type-varint+)))
  (is (= #x12 (ag-proto:make-field-tag 2 ag-proto:+wire-type-length-delimited+)))
  (is (= #x1d (ag-proto:make-field-tag 3 ag-proto:+wire-type-fixed32+))))

(test parse-field-tag
  "Field tag parsing"
  (multiple-value-bind (field wire) (ag-proto:parse-field-tag #x08)
    (is (= 1 field))
    (is (= ag-proto:+wire-type-varint+ wire)))
  (multiple-value-bind (field wire) (ag-proto:parse-field-tag #x12)
    (is (= 2 field))
    (is (= ag-proto:+wire-type-length-delimited+ wire))))

(test field-tag-roundtrip
  "Field tag encode/decode roundtrip"
  (dolist (field-num '(1 2 15 16 100 536870911))
    (dolist (wire-type (list ag-proto:+wire-type-varint+
                             ag-proto:+wire-type-fixed64+
                             ag-proto:+wire-type-length-delimited+
                             ag-proto:+wire-type-fixed32+))
      (let ((tag (ag-proto:make-field-tag field-num wire-type)))
        (multiple-value-bind (decoded-field decoded-wire) (ag-proto:parse-field-tag tag)
          (is (= field-num decoded-field))
          (is (= wire-type decoded-wire)))))))
