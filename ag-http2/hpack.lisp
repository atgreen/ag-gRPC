;;;; hpack.lisp - HPACK header compression (RFC 7541)

(in-package #:ag-http2)

;;;; ========================================================================
;;;; HPACK Constants
;;;; ========================================================================

;;; Static table (RFC 7541 Appendix A)
(defparameter *hpack-static-table*
  #((:authority . "")
    (:method . "GET")
    (:method . "POST")
    (:path . "/")
    (:path . "/index.html")
    (:scheme . "http")
    (:scheme . "https")
    (:status . "200")
    (:status . "204")
    (:status . "206")
    (:status . "304")
    (:status . "400")
    (:status . "404")
    (:status . "500")
    ("accept-charset" . "")
    ("accept-encoding" . "gzip, deflate")
    ("accept-language" . "")
    ("accept-ranges" . "")
    ("accept" . "")
    ("access-control-allow-origin" . "")
    ("age" . "")
    ("allow" . "")
    ("authorization" . "")
    ("cache-control" . "")
    ("content-disposition" . "")
    ("content-encoding" . "")
    ("content-language" . "")
    ("content-length" . "")
    ("content-location" . "")
    ("content-range" . "")
    ("content-type" . "")
    ("cookie" . "")
    ("date" . "")
    ("etag" . "")
    ("expect" . "")
    ("expires" . "")
    ("from" . "")
    ("host" . "")
    ("if-match" . "")
    ("if-modified-since" . "")
    ("if-none-match" . "")
    ("if-range" . "")
    ("if-unmodified-since" . "")
    ("last-modified" . "")
    ("link" . "")
    ("location" . "")
    ("max-forwards" . "")
    ("proxy-authenticate" . "")
    ("proxy-authorization" . "")
    ("range" . "")
    ("referer" . "")
    ("refresh" . "")
    ("retry-after" . "")
    ("server" . "")
    ("set-cookie" . "")
    ("strict-transport-security" . "")
    ("transfer-encoding" . "")
    ("user-agent" . "")
    ("vary" . "")
    ("via" . "")
    ("www-authenticate" . ""))
  "HPACK static table entries (1-indexed, index 0 is invalid)")

;;;; ========================================================================
;;;; Huffman Encoding (RFC 7541 Appendix B)
;;;; ========================================================================

;;; HPACK Huffman table - each entry is (code . code-length-in-bits)
;;; Organized for decoding - we build a lookup tree

(defparameter *hpack-huffman-codes*
  ;; (code bits) for each byte 0-255 plus EOS (256)
  ;; From RFC 7541 Appendix B - verified against spec
  #((#x1ff8 13)      ; 0
    (#x7fffd8 23)    ; 1
    (#xfffffe2 28)   ; 2
    (#xfffffe3 28)   ; 3
    (#xfffffe4 28)   ; 4
    (#xfffffe5 28)   ; 5
    (#xfffffe6 28)   ; 6
    (#xfffffe7 28)   ; 7
    (#xfffffe8 28)   ; 8
    (#xffffea 24)    ; 9
    (#x3ffffffc 30)  ; 10
    (#xfffffe9 28)   ; 11
    (#xfffffea 28)   ; 12
    (#x3ffffffd 30)  ; 13
    (#xfffffeb 28)   ; 14
    (#xfffffec 28)   ; 15
    (#xfffffed 28)   ; 16
    (#xfffffee 28)   ; 17
    (#xfffffef 28)   ; 18
    (#xffffff0 28)   ; 19
    (#xffffff1 28)   ; 20
    (#xffffff2 28)   ; 21
    (#x3ffffffe 30)  ; 22
    (#xffffff3 28)   ; 23
    (#xffffff4 28)   ; 24
    (#xffffff5 28)   ; 25
    (#xffffff6 28)   ; 26
    (#xffffff7 28)   ; 27
    (#xffffff8 28)   ; 28
    (#xffffff9 28)   ; 29
    (#xffffffa 28)   ; 30
    (#xffffffb 28)   ; 31
    (#x14 6)         ; 32 (space)
    (#x3f8 10)       ; 33 !
    (#x3f9 10)       ; 34 "
    (#xffa 12)       ; 35 #
    (#x1ff9 13)      ; 36 $
    (#x15 6)         ; 37 %
    (#xf8 8)         ; 38 &
    (#x7fa 11)       ; 39 '
    (#x3fa 10)       ; 40 (
    (#x3fb 10)       ; 41 )
    (#xf9 8)         ; 42 *
    (#x7fb 11)       ; 43 +
    (#xfa 8)         ; 44 ,
    (#x16 6)         ; 45 -
    (#x17 6)         ; 46 .
    (#x18 6)         ; 47 /
    (#x0 5)          ; 48 0
    (#x1 5)          ; 49 1
    (#x2 5)          ; 50 2
    (#x19 6)         ; 51 3
    (#x1a 6)         ; 52 4
    (#x1b 6)         ; 53 5
    (#x1c 6)         ; 54 6
    (#x1d 6)         ; 55 7
    (#x1e 6)         ; 56 8
    (#x1f 6)         ; 57 9
    (#x5c 7)         ; 58 :
    (#xfb 8)         ; 59 ;
    (#x7ffc 15)      ; 60 <
    (#x20 6)         ; 61 =
    (#xffb 12)       ; 62 >
    (#x3fc 10)       ; 63 ?
    (#x1ffa 13)      ; 64 @
    (#x21 6)         ; 65 A
    (#x5d 7)         ; 66 B
    (#x5e 7)         ; 67 C
    (#x5f 7)         ; 68 D
    (#x60 7)         ; 69 E
    (#x61 7)         ; 70 F
    (#x62 7)         ; 71 G
    (#x63 7)         ; 72 H
    (#x64 7)         ; 73 I
    (#x65 7)         ; 74 J
    (#x66 7)         ; 75 K
    (#x67 7)         ; 76 L
    (#x68 7)         ; 77 M
    (#x69 7)         ; 78 N
    (#x6a 7)         ; 79 O
    (#x6b 7)         ; 80 P
    (#x6c 7)         ; 81 Q
    (#x6d 7)         ; 82 R
    (#x6e 7)         ; 83 S
    (#x6f 7)         ; 84 T
    (#x70 7)         ; 85 U
    (#x71 7)         ; 86 V
    (#x72 7)         ; 87 W
    (#xfc 8)         ; 88 X
    (#x73 7)         ; 89 Y
    (#xfd 8)         ; 90 Z
    (#x1ffb 13)      ; 91 [
    (#x7fff0 19)     ; 92 \
    (#x1ffc 13)      ; 93 ]
    (#x3ffc 14)      ; 94 ^
    (#x22 6)         ; 95 _
    (#x7ffd 15)      ; 96 `
    (#x3 5)          ; 97 a
    (#x23 6)         ; 98 b
    (#x4 5)          ; 99 c
    (#x24 6)         ; 100 d
    (#x5 5)          ; 101 e
    (#x25 6)         ; 102 f
    (#x26 6)         ; 103 g
    (#x27 6)         ; 104 h
    (#x6 5)          ; 105 i
    (#x74 7)         ; 106 j
    (#x75 7)         ; 107 k
    (#x28 6)         ; 108 l
    (#x29 6)         ; 109 m
    (#x2a 6)         ; 110 n
    (#x7 5)          ; 111 o
    (#x2b 6)         ; 112 p
    (#x76 7)         ; 113 q
    (#x2c 6)         ; 114 r
    (#x8 5)          ; 115 s
    (#x9 5)          ; 116 t
    (#x2d 6)         ; 117 u
    (#x77 7)         ; 118 v
    (#x78 7)         ; 119 w
    (#x79 7)         ; 120 x
    (#x7a 7)         ; 121 y
    (#x7b 7)         ; 122 z
    (#x7ffe 15)      ; 123 {
    (#x7fc 11)       ; 124 |
    (#x3ffd 14)      ; 125 }
    (#x1ffd 13)      ; 126 ~
    (#xffffffc 28)   ; 127 DEL
    (#xfffe6 20)     ; 128
    (#x3fffd2 22)    ; 129
    (#xfffe7 20)     ; 130
    (#xfffe8 20)     ; 131
    (#x3fffd3 22)    ; 132
    (#x3fffd4 22)    ; 133
    (#x3fffd5 22)    ; 134
    (#x7fffd9 23)    ; 135
    (#x3fffd6 22)    ; 136
    (#x7fffda 23)    ; 137
    (#x7fffdb 23)    ; 138
    (#x7fffdc 23)    ; 139
    (#x7fffdd 23)    ; 140
    (#x7fffde 23)    ; 141
    (#xffffeb 24)    ; 142
    (#x7fffdf 23)    ; 143
    (#xffffec 24)    ; 144
    (#xffffed 24)    ; 145
    (#x3fffd7 22)    ; 146
    (#x7fffe0 23)    ; 147
    (#xffffee 24)    ; 148
    (#x7fffe1 23)    ; 149
    (#x7fffe2 23)    ; 150
    (#x7fffe3 23)    ; 151
    (#x7fffe4 23)    ; 152
    (#x1fffdc 21)    ; 153
    (#x3fffd8 22)    ; 154
    (#x7fffe5 23)    ; 155
    (#x3fffd9 22)    ; 156
    (#x7fffe6 23)    ; 157
    (#x7fffe7 23)    ; 158
    (#xffffef 24)    ; 159
    (#x3fffda 22)    ; 160
    (#x1fffdd 21)    ; 161
    (#xfffe9 20)     ; 162
    (#x3fffdb 22)    ; 163
    (#x3fffdc 22)    ; 164
    (#x7fffe8 23)    ; 165
    (#x7fffe9 23)    ; 166
    (#x1fffde 21)    ; 167
    (#x7fffea 23)    ; 168
    (#x3fffdd 22)    ; 169
    (#x3fffde 22)    ; 170
    (#xfffff0 24)    ; 171
    (#x1fffdf 21)    ; 172
    (#x3fffdf 22)    ; 173
    (#x7fffeb 23)    ; 174
    (#x7fffec 23)    ; 175
    (#x1fffe0 21)    ; 176
    (#x1fffe1 21)    ; 177
    (#x3fffe0 22)    ; 178
    (#x1fffe2 21)    ; 179
    (#x7fffed 23)    ; 180
    (#x3fffe1 22)    ; 181
    (#x7fffee 23)    ; 182
    (#x7fffef 23)    ; 183
    (#xfffea 20)     ; 184
    (#x3fffe2 22)    ; 185
    (#x3fffe3 22)    ; 186
    (#x3fffe4 22)    ; 187
    (#x7ffff0 23)    ; 188
    (#x3fffe5 22)    ; 189
    (#x3fffe6 22)    ; 190
    (#x7ffff1 23)    ; 191
    (#x3ffffe0 26)   ; 192
    (#x3ffffe1 26)   ; 193
    (#xfffeb 20)     ; 194
    (#x7fff1 19)     ; 195
    (#x3fffe7 22)    ; 196
    (#x7ffff2 23)    ; 197
    (#x3fffe8 22)    ; 198
    (#x1ffffec 25)   ; 199
    (#x3ffffe2 26)   ; 200
    (#x3ffffe3 26)   ; 201
    (#x3ffffe4 26)   ; 202
    (#x7ffffde 27)   ; 203
    (#x7ffffdf 27)   ; 204
    (#x3ffffe5 26)   ; 205
    (#xfffff1 24)    ; 206
    (#x1ffffed 25)   ; 207
    (#x7fff2 19)     ; 208
    (#x1fffe3 21)    ; 209
    (#x3ffffe6 26)   ; 210
    (#x7ffffe0 27)   ; 211
    (#x7ffffe1 27)   ; 212
    (#x3ffffe7 26)   ; 213
    (#x7ffffe2 27)   ; 214
    (#xfffff2 24)    ; 215
    (#x1fffe4 21)    ; 216
    (#x1fffe5 21)    ; 217
    (#x3ffffe8 26)   ; 218
    (#x3ffffe9 26)   ; 219
    (#xffffffd 28)   ; 220
    (#x7ffffe3 27)   ; 221
    (#x7ffffe4 27)   ; 222
    (#x7ffffe5 27)   ; 223
    (#xfffec 20)     ; 224
    (#xfffff3 24)    ; 225
    (#xfffed 20)     ; 226
    (#x1fffe6 21)    ; 227
    (#x3fffe9 22)    ; 228
    (#x1fffe7 21)    ; 229
    (#x1fffe8 21)    ; 230
    (#x7ffff3 23)    ; 231
    (#x3fffea 22)    ; 232
    (#x3fffeb 22)    ; 233
    (#x1ffffee 25)   ; 234
    (#x1ffffef 25)   ; 235
    (#xfffff4 24)    ; 236
    (#xfffff5 24)    ; 237
    (#x3ffffea 26)   ; 238
    (#x7ffff4 23)    ; 239
    (#x3ffffeb 26)   ; 240
    (#x7ffffe6 27)   ; 241
    (#x3ffffec 26)   ; 242
    (#x3ffffed 26)   ; 243
    (#x7ffffe7 27)   ; 244
    (#x7ffffe8 27)   ; 245
    (#x7ffffe9 27)   ; 246
    (#x7ffffea 27)   ; 247
    (#x7ffffeb 27)   ; 248
    (#xffffffe 28)   ; 249
    (#x7ffffec 27)   ; 250
    (#x7ffffed 27)   ; 251
    (#x7ffffee 27)   ; 252
    (#x7ffffef 27)   ; 253
    (#x7fffff0 27)   ; 254
    (#x3ffffee 26)   ; 255
    (#x3fffffff 30)) ; 256 = EOS
  "HPACK Huffman code table from RFC 7541 Appendix B")

(defparameter *huffman-decode-tree* nil
  "Huffman decoding tree (built lazily, reset when codes change)")

(defun build-huffman-decode-tree ()
  "Build a tree structure for Huffman decoding"
  (let ((tree (cons nil nil)))  ; Root node
    (loop for symbol from 0 to 256
          for entry across *hpack-huffman-codes*
          for code = (first entry)
          for bits = (second entry)
          do (let ((node tree))
               ;; Walk down the tree, creating nodes as needed
               (loop for bit-pos from (1- bits) downto 0
                     for bit = (logand 1 (ash code (- bit-pos)))
                     do (if (zerop bit)
                            (progn
                              (unless (car node)
                                (setf (car node) (cons nil nil)))
                              (setf node (car node)))
                            (progn
                              (unless (cdr node)
                                (setf (cdr node) (cons nil nil)))
                              (setf node (cdr node)))))
               ;; Store symbol at leaf
               (setf (car node) symbol)))
    tree))

(defun get-huffman-decode-tree ()
  "Get or build the Huffman decode tree"
  (or *huffman-decode-tree*
      (setf *huffman-decode-tree* (build-huffman-decode-tree))))

(defun huffman-decode (bytes)
  "Decode Huffman-encoded bytes to a string"
  (let ((tree (get-huffman-decode-tree))
        (result (make-array 64 :element-type '(unsigned-byte 8)
                            :fill-pointer 0 :adjustable t))
        (node (get-huffman-decode-tree)))
    (loop for byte across bytes
          do (loop for bit-pos from 7 downto 0
                   for bit = (logand 1 (ash byte (- bit-pos)))
                   do (setf node (if (zerop bit) (car node) (cdr node)))
                   when (and node (atom (car node)))
                   do (let ((symbol (car node)))
                        (when (< symbol 256)  ; Not EOS
                          (vector-push-extend symbol result))
                        (setf node tree))))
    (ag-proto::utf8-to-string result)))

;;;; ========================================================================
;;;; Dynamic Table
;;;; ========================================================================

(defclass hpack-dynamic-table ()
  ((entries :initform (make-array 0 :adjustable t :fill-pointer 0)
            :accessor dynamic-table-entries)
   (size :initform 0 :accessor dynamic-table-size)
   (max-size :initarg :max-size :initform 4096 :accessor dynamic-table-max-size))
  (:documentation "HPACK dynamic table"))

(defun make-dynamic-table (&key (max-size 4096))
  "Create a new dynamic table"
  (make-instance 'hpack-dynamic-table :max-size max-size))

(defun dynamic-table-add (table name value)
  "Add a header to the dynamic table"
  (let ((entry-size (+ 32 (length name) (length value))))
    ;; Evict entries if needed
    (loop while (> (+ (dynamic-table-size table) entry-size)
                   (dynamic-table-max-size table))
          do (dynamic-table-evict table))
    ;; Add new entry at the front
    (vector-push-extend (cons name value) (dynamic-table-entries table))
    (incf (dynamic-table-size table) entry-size)))

(defun dynamic-table-evict (table)
  "Evict the oldest entry from the dynamic table"
  (let* ((entries (dynamic-table-entries table))
         (entry (aref entries 0))
         (entry-size (+ 32 (length (car entry)) (length (cdr entry)))))
    (decf (dynamic-table-size table) entry-size)
    ;; Shift entries down
    (loop for i from 0 below (1- (fill-pointer entries))
          do (setf (aref entries i) (aref entries (1+ i))))
    (decf (fill-pointer entries))))

;;;; ========================================================================
;;;; Encoder/Decoder Classes
;;;; ========================================================================

(defclass hpack-encoder ()
  ((dynamic-table :initform (make-dynamic-table) :accessor encoder-dynamic-table)
   (huffman :initarg :huffman :initform nil :accessor encoder-huffman-p))
  (:documentation "HPACK encoder"))

(defclass hpack-decoder ()
  ((dynamic-table :initform (make-dynamic-table) :accessor decoder-dynamic-table))
  (:documentation "HPACK decoder"))

(defun make-hpack-encoder (&key (huffman nil) (table-size 4096))
  "Create a new HPACK encoder"
  (make-instance 'hpack-encoder
                 :huffman huffman))

(defun make-hpack-decoder (&key (table-size 4096))
  "Create a new HPACK decoder"
  (make-instance 'hpack-decoder))

;;;; ========================================================================
;;;; Integer Encoding (RFC 7541 Section 5.1)
;;;; ========================================================================

(defun hpack-encode-integer (value prefix-bits output)
  "Encode an integer with the given prefix size"
  (hpack-encode-integer-with-prefix value prefix-bits 0 output))

(defun hpack-encode-integer-with-prefix (value prefix-bits prefix-byte output)
  "Encode an integer with the given prefix size and leading prefix bits."
  (let ((max-prefix (1- (ash 1 prefix-bits))))
    (cond
      ((< value max-prefix)
       (vector-push-extend (logior prefix-byte value) output))
      (t
       (vector-push-extend (logior prefix-byte max-prefix) output)
       (decf value max-prefix)
       (loop while (>= value 128)
             do (vector-push-extend (logior 128 (logand value 127)) output)
                (setf value (ash value -7)))
       (vector-push-extend value output)))))

(defun hpack-decode-integer (prefix-bits input pos)
  "Decode an integer with the given prefix size. Returns (values int new-pos)"
  (let* ((input-len (length input))
         (max-prefix (1- (ash 1 prefix-bits)))
         (byte (aref input pos))
         (value (logand byte max-prefix)))
    (if (< value max-prefix)
        (values value (1+ pos))
        (let ((m 0))
          (loop for p from (1+ pos)
                for b = (if (>= p input-len)
                            (error "HPACK integer extends beyond input bounds")
                            (aref input p))
                do (incf value (ash (logand b 127) m))
                   (incf m 7)
                when (zerop (logand b 128))
                  return (values value (1+ p)))))))

;;;; ========================================================================
;;;; String Encoding (RFC 7541 Section 5.2)
;;;; ========================================================================

(defun hpack-encode-string (string output &key huffman)
  "Encode a string (with or without Huffman coding)"
  (let* ((bytes (ag-proto::string-to-utf8 string))
         (len (length bytes)))
    ;; For now, always use literal (no Huffman)
    (hpack-encode-integer len 7 output)
    (loop for byte across bytes
          do (vector-push-extend byte output))))

(defun hpack-decode-string (input pos)
  "Decode a string. Returns (values string new-pos)"
  (let ((huffman-p (logbitp 7 (aref input pos))))
    (multiple-value-bind (len new-pos)
        (hpack-decode-integer 7 input pos)
      (let ((end-pos (+ new-pos len)))
        (when (> end-pos (length input))
          (error "HPACK string length ~D exceeds input bounds (pos=~D, input-length=~D, byte-at-pos=~D, huffman=~A)"
                 len new-pos (length input) (aref input pos) huffman-p))
        (let ((bytes (subseq input new-pos end-pos)))
          (values (if huffman-p
                      (huffman-decode bytes)
                      (ag-proto::utf8-to-string bytes))
                  end-pos))))))

;;;; ========================================================================
;;;; Header Encoding/Decoding
;;;; ========================================================================

(defun hpack-encode (encoder headers)
  "Encode a list of headers. Returns a byte vector."
  (let ((output (make-array 256 :element-type '(unsigned-byte 8)
                                :fill-pointer 0 :adjustable t)))
    (dolist (header headers)
      (let* ((name (car header))
             (value (cdr header))
             (name-string (if (keywordp name)
                              (format nil ":~A" (string-downcase (symbol-name name)))
                              (string-downcase name)))
             (value-string (if (stringp value) value (format nil "~A" value)))
             (exact-index (hpack-static-index name-string value-string))
             (name-index (or exact-index (hpack-static-name-index name-string))))
        (cond
          (exact-index
           ;; Indexed Header Field Representation (1xxxxxxx)
           (hpack-encode-integer-with-prefix exact-index 7 #x80 output))
          (name-index
           ;; Literal Header Field without Indexing, indexed name (0000xxxx)
           (hpack-encode-integer-with-prefix name-index 4 #x00 output)
           (hpack-encode-string value-string output))
          (t
           ;; Literal Header Field without Indexing, new name (0000 0000)
           (vector-push-extend 0 output)
           (hpack-encode-string name-string output)
           (hpack-encode-string value-string output)))))
    output))

(defun hpack-static-index (name value)
  "Return the static table index for an exact header match, or NIL."
  (loop for i from 1 to (length *hpack-static-table*)
        for (n . v) = (aref *hpack-static-table* (1- i))
        for n-string = (if (keywordp n)
                           (format nil ":~A" (string-downcase (symbol-name n)))
                           (string-downcase n))
        when (and (string= n-string name)
                  (string= v value))
          return i))

(defun hpack-static-name-index (name)
  "Return the static table index for a header name, or NIL."
  (loop for i from 1 to (length *hpack-static-table*)
        for (n . v) = (aref *hpack-static-table* (1- i))
        for n-string = (if (keywordp n)
                           (format nil ":~A" (string-downcase (symbol-name n)))
                           (string-downcase n))
        when (string= n-string name)
          return i))

(defun hpack-decode (decoder bytes)
  "Decode a header block. Returns a list of (name . value) pairs."
  (let ((headers nil)
        (pos 0)
        (len (length bytes)))
    (loop while (< pos len)
          do (let ((byte (aref bytes pos)))
               (cond
                 ;; Indexed header field (1xxxxxxx)
                 ((logbitp 7 byte)
                  (multiple-value-bind (index new-pos)
                      (hpack-decode-integer 7 bytes pos)
                    (push (hpack-get-indexed decoder index) headers)
                    (setf pos new-pos)))
                 ;; Literal header field with incremental indexing (01xxxxxx)
                 ((= (logand byte #xc0) #x40)
                  (multiple-value-bind (name value new-pos)
                      (hpack-decode-literal bytes pos 6)
                    (push (cons name value) headers)
                    (dynamic-table-add (decoder-dynamic-table decoder) name value)
                    (setf pos new-pos)))
                 ;; Dynamic table size update (001xxxxx)
                 ((= (logand byte #xe0) #x20)
                  (multiple-value-bind (new-size new-pos)
                      (hpack-decode-integer 5 bytes pos)
                    ;; Update the dynamic table max size
                    (setf (dynamic-table-max-size (decoder-dynamic-table decoder)) new-size)
                    ;; Evict entries if needed
                    (let ((table (decoder-dynamic-table decoder)))
                      (loop while (> (dynamic-table-size table) new-size)
                            do (dynamic-table-evict table)))
                    (setf pos new-pos)))
                 ;; Literal without indexing (0000xxxx) or never indexed (0001xxxx)
                 (t
                  (multiple-value-bind (name value new-pos)
                      (hpack-decode-literal bytes pos 4)
                    (push (cons name value) headers)
                    (setf pos new-pos))))))
    (nreverse headers)))

(defun hpack-decode-literal (bytes pos prefix-bits)
  "Decode a literal header field. Returns (values name value new-pos)"
  (let ((index-bits (logand (aref bytes pos) (1- (ash 1 prefix-bits)))))
    (multiple-value-bind (name new-pos)
        (if (zerop index-bits)
            (hpack-decode-string bytes (1+ pos))
            (values (car (hpack-get-indexed nil index-bits)) (1+ pos)))
      (multiple-value-bind (value final-pos)
          (hpack-decode-string bytes new-pos)
        (values name value final-pos)))))

(defun hpack-get-indexed (decoder index)
  "Get a header from the static or dynamic table by index"
  (cond
    ((<= index (length *hpack-static-table*))
     (aref *hpack-static-table* (1- index)))
    (decoder
     (let* ((dyn-table (decoder-dynamic-table decoder))
            (entries (dynamic-table-entries dyn-table))
            (dyn-index (- index (length *hpack-static-table*) 1)))
       (aref entries (- (length entries) dyn-index 1))))
    (t
     (error "Invalid HPACK index: ~A" index))))
