;;;; wire-format.lisp - Protocol Buffers wire format encoding/decoding
;;;;
;;;; Implements the Protocol Buffers binary wire format as specified in:
;;;; https://protobuf.dev/programming-guides/encoding/

(in-package #:ag-proto)

;;; Wire types (3 bits)
(defconstant +wire-type-varint+ 0
  "Varint: int32, int64, uint32, uint64, sint32, sint64, bool, enum")
(defconstant +wire-type-fixed64+ 1
  "64-bit: fixed64, sfixed64, double")
(defconstant +wire-type-length-delimited+ 2
  "Length-delimited: string, bytes, embedded messages, packed repeated fields")
(defconstant +wire-type-start-group+ 3
  "Start group (deprecated)")
(defconstant +wire-type-end-group+ 4
  "End group (deprecated)")
(defconstant +wire-type-fixed32+ 5
  "32-bit: fixed32, sfixed32, float")

;;;; ========================================================================
;;;; Conditions
;;;; ========================================================================

(define-condition wire-format-error (error)
  ((message :initarg :message :reader wire-format-error-message))
  (:report (lambda (condition stream)
             (format stream "Wire format error: ~A"
                     (wire-format-error-message condition)))))

;;;; ========================================================================
;;;; Varint Encoding/Decoding (LEB128)
;;;; ========================================================================

(defun encode-varint (value)
  "Encode an unsigned integer as a varint (LEB128).
Returns a list of bytes."
  (declare (type (integer 0) value))
  (if (zerop value)
      (list 0)
      (loop for v = value then (ash v -7)
            while (plusp v)
            collect (if (> v 127)
                        (logior (logand v #x7f) #x80)
                        (logand v #x7f)))))

(defun encode-varint-to-stream (value stream)
  "Encode an unsigned integer as a varint directly to a binary stream."
  (declare (type (integer 0) value))
  (if (zerop value)
      (write-byte 0 stream)
      (loop for v = value then (ash v -7)
            while (plusp v)
            do (write-byte (if (> v 127)
                               (logior (logand v #x7f) #x80)
                               (logand v #x7f))
                           stream))))

(defun decode-varint (bytes &optional (start 0))
  "Decode a varint from a sequence of bytes.
Returns (values decoded-value bytes-consumed)."
  (loop with result = 0
        with shift = 0
        for i from start
        for byte = (elt bytes i)
        do (setf result (logior result (ash (logand byte #x7f) shift)))
           (incf shift 7)
        when (zerop (logand byte #x80))
          return (values result (1+ (- i start)))
        when (> shift 63)
          do (error 'wire-format-error
                    :message "Varint too long (> 10 bytes)")))

(defun decode-varint-from-stream (stream)
  "Decode a varint from a binary stream.
Returns the decoded value."
  (loop with result = 0
        with shift = 0
        for byte = (read-byte stream nil nil)
        when (null byte)
          do (error 'wire-format-error :message "Unexpected end of stream reading varint")
        do (setf result (logior result (ash (logand byte #x7f) shift)))
           (incf shift 7)
        when (zerop (logand byte #x80))
          return result
        when (> shift 63)
          do (error 'wire-format-error
                    :message "Varint too long (> 10 bytes)")))

;;;; ========================================================================
;;;; Zigzag Encoding (for signed integers)
;;;; ========================================================================

(defun zigzag-encode (value)
  "Encode a signed integer using zigzag encoding.
Maps negative numbers to positive: 0 -> 0, -1 -> 1, 1 -> 2, -2 -> 3, etc."
  (declare (type integer value))
  (if (minusp value)
      (1- (* -2 value))
      (* 2 value)))

(defun zigzag-decode (value)
  "Decode a zigzag-encoded value back to a signed integer."
  (declare (type (integer 0) value))
  (if (oddp value)
      (- (ash (1+ value) -1))
      (ash value -1)))

;;;; ========================================================================
;;;; Fixed-Width Encoding (Little-Endian)
;;;; ========================================================================

(defun encode-fixed32 (value)
  "Encode a 32-bit value as 4 bytes in little-endian order.
Returns a list of 4 bytes."
  (declare (type (unsigned-byte 32) value))
  (list (logand value #xff)
        (logand (ash value -8) #xff)
        (logand (ash value -16) #xff)
        (logand (ash value -24) #xff)))

(defun encode-fixed32-to-stream (value stream)
  "Encode a 32-bit value to a stream in little-endian order."
  (declare (type (unsigned-byte 32) value))
  (write-byte (logand value #xff) stream)
  (write-byte (logand (ash value -8) #xff) stream)
  (write-byte (logand (ash value -16) #xff) stream)
  (write-byte (logand (ash value -24) #xff) stream))

(defun decode-fixed32 (bytes &optional (start 0))
  "Decode a 32-bit little-endian value from bytes.
Returns (values decoded-value 4)."
  (values (logior (elt bytes start)
                  (ash (elt bytes (+ start 1)) 8)
                  (ash (elt bytes (+ start 2)) 16)
                  (ash (elt bytes (+ start 3)) 24))
          4))

(defun decode-fixed32-from-stream (stream)
  "Decode a 32-bit little-endian value from a stream."
  (logior (read-byte stream)
          (ash (read-byte stream) 8)
          (ash (read-byte stream) 16)
          (ash (read-byte stream) 24)))

(defun encode-fixed64 (value)
  "Encode a 64-bit value as 8 bytes in little-endian order.
Returns a list of 8 bytes."
  (declare (type (unsigned-byte 64) value))
  (list (logand value #xff)
        (logand (ash value -8) #xff)
        (logand (ash value -16) #xff)
        (logand (ash value -24) #xff)
        (logand (ash value -32) #xff)
        (logand (ash value -40) #xff)
        (logand (ash value -48) #xff)
        (logand (ash value -56) #xff)))

(defun encode-fixed64-to-stream (value stream)
  "Encode a 64-bit value to a stream in little-endian order."
  (declare (type (unsigned-byte 64) value))
  (write-byte (logand value #xff) stream)
  (write-byte (logand (ash value -8) #xff) stream)
  (write-byte (logand (ash value -16) #xff) stream)
  (write-byte (logand (ash value -24) #xff) stream)
  (write-byte (logand (ash value -32) #xff) stream)
  (write-byte (logand (ash value -40) #xff) stream)
  (write-byte (logand (ash value -48) #xff) stream)
  (write-byte (logand (ash value -56) #xff) stream))

(defun decode-fixed64 (bytes &optional (start 0))
  "Decode a 64-bit little-endian value from bytes.
Returns (values decoded-value 8)."
  (values (logior (elt bytes start)
                  (ash (elt bytes (+ start 1)) 8)
                  (ash (elt bytes (+ start 2)) 16)
                  (ash (elt bytes (+ start 3)) 24)
                  (ash (elt bytes (+ start 4)) 32)
                  (ash (elt bytes (+ start 5)) 40)
                  (ash (elt bytes (+ start 6)) 48)
                  (ash (elt bytes (+ start 7)) 56))
          8))

(defun decode-fixed64-from-stream (stream)
  "Decode a 64-bit little-endian value from a stream."
  (logior (read-byte stream)
          (ash (read-byte stream) 8)
          (ash (read-byte stream) 16)
          (ash (read-byte stream) 24)
          (ash (read-byte stream) 32)
          (ash (read-byte stream) 40)
          (ash (read-byte stream) 48)
          (ash (read-byte stream) 56)))

;;;; ========================================================================
;;;; Signed Fixed-Width (for sfixed32, sfixed64)
;;;; ========================================================================

(defun encode-sfixed32 (value)
  "Encode a signed 32-bit value as 4 bytes in little-endian order."
  (declare (type (signed-byte 32) value))
  (encode-fixed32 (logand value #xffffffff)))

(defun decode-sfixed32 (bytes &optional (start 0))
  "Decode a signed 32-bit little-endian value from bytes."
  (multiple-value-bind (uval consumed) (decode-fixed32 bytes start)
    (values (if (logbitp 31 uval)
                (- uval #x100000000)
                uval)
            consumed)))

(defun encode-sfixed64 (value)
  "Encode a signed 64-bit value as 8 bytes in little-endian order."
  (declare (type (signed-byte 64) value))
  (encode-fixed64 (logand value #xffffffffffffffff)))

(defun decode-sfixed64 (bytes &optional (start 0))
  "Decode a signed 64-bit little-endian value from bytes."
  (multiple-value-bind (uval consumed) (decode-fixed64 bytes start)
    (values (if (logbitp 63 uval)
                (- uval #x10000000000000000)
                uval)
            consumed)))

;;;; ========================================================================
;;;; Floating-Point Encoding
;;;; ========================================================================

(defun encode-float32 (value)
  "Encode a single-precision float as 4 bytes.
Note: This uses implementation-specific float representation."
  (declare (type single-float value))
  (encode-fixed32 (float-to-ieee754-32 value)))

(defun decode-float32 (bytes &optional (start 0))
  "Decode a single-precision float from 4 bytes."
  (multiple-value-bind (bits consumed) (decode-fixed32 bytes start)
    (values (ieee754-32-to-float bits) consumed)))

(defun encode-float64 (value)
  "Encode a double-precision float as 8 bytes."
  (declare (type double-float value))
  (encode-fixed64 (float-to-ieee754-64 value)))

(defun decode-float64 (bytes &optional (start 0))
  "Decode a double-precision float from 8 bytes."
  (multiple-value-bind (bits consumed) (decode-fixed64 bytes start)
    (values (ieee754-64-to-float bits) consumed)))

;;; IEEE 754 conversion helpers (using ieee-floats library)

(defun float-to-ieee754-32 (f)
  "Convert a single-float to its IEEE 754 32-bit representation."
  (ieee-floats:encode-float32 f))

(defun ieee754-32-to-float (bits)
  "Convert IEEE 754 32-bit representation to a single-float."
  (ieee-floats:decode-float32 bits))

(defun float-to-ieee754-64 (f)
  "Convert a double-float to its IEEE 754 64-bit representation."
  (ieee-floats:encode-float64 f))

(defun ieee754-64-to-float (bits)
  "Convert IEEE 754 64-bit representation to a double-float."
  (ieee-floats:decode-float64 bits))

;;;; ========================================================================
;;;; Length-Delimited Encoding
;;;; ========================================================================

(defun encode-length-delimited (bytes)
  "Encode a sequence of bytes with a length prefix.
Returns a list of bytes (length varint followed by data)."
  (let ((len (length bytes)))
    (append (encode-varint len) (coerce bytes 'list))))

(defun encode-length-delimited-to-stream (bytes stream)
  "Encode a sequence of bytes with a length prefix to a stream."
  (encode-varint-to-stream (length bytes) stream)
  (write-sequence bytes stream))

(defun decode-length-delimited (bytes &optional (start 0))
  "Decode a length-delimited field from bytes.
Returns (values data-bytes total-bytes-consumed)."
  (multiple-value-bind (length varint-size) (decode-varint bytes start)
    (let* ((data-start (+ start varint-size))
           (data-end (+ data-start length))
           (data (subseq bytes data-start data-end)))
      (values data (+ varint-size length)))))

(defun decode-length-delimited-from-stream (stream)
  "Decode a length-delimited field from a stream.
Returns the data as a byte vector."
  (let* ((length (decode-varint-from-stream stream))
         (data (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence data stream)
    data))

;;;; ========================================================================
;;;; String Encoding (UTF-8)
;;;; ========================================================================

(defun encode-string (string)
  "Encode a string as UTF-8 bytes with length prefix."
  (encode-length-delimited (string-to-utf8 string)))

(defun encode-string-to-stream (string stream)
  "Encode a string as UTF-8 bytes with length prefix to a stream."
  (encode-length-delimited-to-stream (string-to-utf8 string) stream))

(defun decode-string (bytes &optional (start 0))
  "Decode a length-delimited UTF-8 string from bytes.
Returns (values string bytes-consumed)."
  (multiple-value-bind (data consumed) (decode-length-delimited bytes start)
    (values (utf8-to-string data) consumed)))

(defun decode-string-from-stream (stream)
  "Decode a length-delimited UTF-8 string from a stream."
  (utf8-to-string (decode-length-delimited-from-stream stream)))

;;; UTF-8 helpers (using trivial-utf-8 library)

(defun string-to-utf8 (string)
  "Convert a string to a vector of UTF-8 bytes."
  (trivial-utf-8:string-to-utf-8-bytes string))

(defun utf8-to-string (bytes)
  "Convert a vector of UTF-8 bytes to a string."
  (trivial-utf-8:utf-8-bytes-to-string bytes))

;;;; ========================================================================
;;;; Field Tag Encoding
;;;; ========================================================================

(defun make-field-tag (field-number wire-type)
  "Create a field tag from field number and wire type.
Tag = (field-number << 3) | wire-type"
  (declare (type (integer 1) field-number)
           (type (integer 0 7) wire-type))
  (logior (ash field-number 3) wire-type))

(defun parse-field-tag (tag)
  "Parse a field tag into field number and wire type.
Returns (values field-number wire-type)."
  (declare (type (integer 0) tag))
  (values (ash tag -3) (logand tag #x07)))

(defun encode-field-tag (field-number wire-type)
  "Encode a field tag as a varint."
  (encode-varint (make-field-tag field-number wire-type)))

(defun decode-field-tag (bytes &optional (start 0))
  "Decode a field tag from bytes.
Returns (values field-number wire-type bytes-consumed)."
  (multiple-value-bind (tag consumed) (decode-varint bytes start)
    (multiple-value-bind (field-number wire-type) (parse-field-tag tag)
      (values field-number wire-type consumed))))

(defun decode-field-tag-from-stream (stream)
  "Decode a field tag from a stream.
Returns (values field-number wire-type) or NIL on EOF."
  (let ((first-byte (read-byte stream nil nil)))
    (when first-byte
      ;; Put back the byte and decode the full varint
      (let ((tag (if (zerop (logand first-byte #x80))
                     first-byte
                     (let ((result first-byte)
                           (shift 7))
                       (loop for byte = (read-byte stream)
                             do (setf result (logior result (ash (logand byte #x7f) shift)))
                                (incf shift 7)
                             until (zerop (logand byte #x80)))
                       result))))
        (parse-field-tag tag)))))
