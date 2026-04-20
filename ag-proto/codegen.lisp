;;;; codegen.lisp - Generate CLOS classes from proto descriptors

(in-package #:ag-proto)

;;;; ========================================================================
;;;; Code Generation
;;;;
;;;; Generates CLOS class definitions from proto-message-descriptor objects.
;;;; Each proto message becomes a CLOS class with:
;;;; - Slots for each field
;;;; - serialize-to-stream method
;;;; - deserialize-from-stream method
;;;; ========================================================================

;;; Special variable to track known enum types during code generation
(defvar *known-enum-types* nil
  "Set of enum type names known during code generation.
Used to determine if a field type should be serialized as varint (enum) or
length-delimited (message).")

(defvar *class-prefix* nil
  "Optional prefix to add to generated class and accessor names.
For example, setting this to \"PROTO-\" will generate class names like PROTO-FOO-BAR.")

(defun enum-type-p (type-name)
  "Return T if TYPE-NAME refers to a known enum type."
  (and *known-enum-types*
       (stringp type-name)
       (gethash type-name *known-enum-types*)))

(defun lisp-name (name &optional suffix)
  "Convert a proto name to a Lisp symbol name.
   FooBar becomes FOO-BAR, TLSCreds becomes TLS-CREDS, with optional SUFFIX appended."
  (with-output-to-string (s)
    (loop for i from 0 below (length name)
          for char = (char name i)
          for prev-char = (if (> i 0) (char name (1- i)) nil)
          for next-char = (if (< (1+ i) (length name)) (char name (1+ i)) nil)
          do (cond
               ;; Insert hyphen before uppercase if previous was lowercase
               ((and prev-char
                     (upper-case-p char)
                     (lower-case-p prev-char))
                (write-char #\- s)
                (write-char (char-upcase char) s))
               ;; Insert hyphen before uppercase that starts a new word
               ;; (uppercase followed by lowercase, after another uppercase)
               ;; e.g., TLSCreds -> TLS-CREDS at 'C' (prev=S upper, curr=C upper, next=r lower)
               ((and prev-char
                     (upper-case-p prev-char)
                     (upper-case-p char)
                     next-char
                     (lower-case-p next-char))
                (write-char #\- s)
                (write-char (char-upcase char) s))
               ;; Convert underscore to hyphen
               ((char= char #\_)
                (write-char #\- s))
               ;; Normal character
               (t
                (write-char (char-upcase char) s))))
    (when suffix
      (write-string suffix s))))

(defun prefixed-lisp-name (name &optional suffix)
  "Convert a proto name to a prefixed Lisp symbol name using *class-prefix*."
  (if *class-prefix*
      (concatenate 'string *class-prefix* (lisp-name name suffix))
      (lisp-name name suffix)))

;;; Type mapping

(defun proto-type-to-lisp-type (proto-type)
  "Convert a proto type keyword to a Lisp type specifier"
  (case proto-type
    (:double 'double-float)
    (:float 'single-float)
    ((:int32 :sint32 :sfixed32) '(signed-byte 32))
    ((:int64 :sint64 :sfixed64) '(signed-byte 64))
    ((:uint32 :fixed32) '(unsigned-byte 32))
    ((:uint64 :fixed64) '(unsigned-byte 64))
    (:bool 'boolean)
    (:string 'string)
    (:bytes '(vector (unsigned-byte 8)))
    (otherwise t)))  ; Message types become t

(defun proto3-default-value (type)
  "Return the Proto3 default value for a type"
  (case type
    (:double 0.0d0)
    (:float 0.0f0)
    ((:int32 :int64 :uint32 :uint64 :sint32 :sint64
      :fixed32 :fixed64 :sfixed32 :sfixed64 :enum) 0)
    (:bool nil)
    (:string "")
    (:bytes '(make-array 0 :element-type '(unsigned-byte 8)))
    (otherwise nil)))  ; Message types default to nil

(defparameter *cl-reserved-names*
  '(;; Types and special values
    "NUMBER" "VALUES" "TYPE" "CLASS" "FUNCTION" "STREAM" "STRING"
    "LIST" "SEQUENCE" "ARRAY" "VECTOR" "SYMBOL" "PACKAGE" "CONS"
    "FLOAT" "INTEGER" "RATIO" "COMPLEX" "CHARACTER" "PATHNAME" "HASH-TABLE"
    "T" "NIL"
    ;; Common CL functions that would conflict with (SETF name)
    "COUNT" "LENGTH" "POSITION" "MEMBER" "FIND" "REMOVE" "DELETE" "SORT"
    "MAP" "REDUCE" "APPEND" "REVERSE" "SEARCH" "SUBSTITUTE" "REPLACE"
    "FIRST" "SECOND" "THIRD" "REST" "LAST" "NTH" "ELT" "AREF"
    "CAR" "CDR" "PUSH" "POP" "ERROR" "WARN" "FORMAT" "PRINT" "READ" "WRITE"
    "OPEN" "CLOSE" "TIME" "SLEEP" "RANDOM"
    ;; Other CL symbols that cause package lock violations as slot names
    "METHOD" "STRUCT")
  "CL symbols that should not be used as accessor names (to avoid package lock violations)")

(defun safe-accessor-name (name package &optional message-name)
  "Generate a safe accessor name, prefixing if it conflicts with CL symbols or if *CLASS-PREFIX* is set.
If MESSAGE-NAME is provided and *CLASS-PREFIX* is set, generates message-specific accessor names
like PROTO-MESSAGE-FIELD instead of just PROTO-FIELD to avoid naming collisions."
  (let* ((upname (string-upcase (substitute #\- #\_ name)))
         (prefix (or *class-prefix*
                     (when (member upname *cl-reserved-names* :test #'string=)
                       "PROTO-")))
         (final-name (if (and prefix message-name)
                         ;; When both prefix and message name are provided, create message-specific accessor
                         (concatenate 'string prefix (lisp-name message-name) "-" upname)
                         (if prefix
                             (concatenate 'string prefix upname)
                             upname))))
    (intern final-name package)))

(defun safe-class-name (name package)
  "Generate a safe class name, prefixing if it conflicts with CL symbols or if *CLASS-PREFIX* is set.
NAME should already be in LISP-NAME format (e.g., FOO-BAR).
If PACKAGE is nil, interns in the current package (*PACKAGE*)."
  (let* ((target-package (or package *package*))
         (prefix (or *class-prefix*
                     (when (member name *cl-reserved-names* :test #'string=)
                       "PROTO-")))
         (final-name (if prefix
                         (concatenate 'string prefix name)
                         name)))
    (intern final-name target-package)))

(defun field-name-to-slot-name (name)
  "Convert a field name to a slot name symbol, prefixing with PROTO- if it conflicts with CL"
  (let ((upname (string-upcase (substitute #\- #\_ name))))
    (if (member upname *cl-reserved-names* :test #'string=)
        (intern (concatenate 'string "PROTO-" upname))
        (intern upname))))

(defun field-name-to-keyword (name)
  "Convert a field name to a keyword symbol"
  (intern (string-upcase (substitute #\- #\_ name)) :keyword))

;;; Slot generation

(defun map-field-p (field)
  "Return T if FIELD is a map field."
  (and (proto-field-map-key-type field)
       (proto-field-map-value-type field)))

(defun generate-slot-definition (field &optional package message-name)
  "Generate a slot definition from a field descriptor.
MESSAGE-NAME is the proto message name, used to create message-specific accessor names when *CLASS-PREFIX* is set."
  (let* ((name (proto-field-name field))
         (slot-name (field-name-to-slot-name name))
         (accessor-name (if package
                            (safe-accessor-name name package message-name)
                            slot-name))
         (type (proto-field-type field))
         (lisp-type (proto-type-to-lisp-type type))
         (default (proto3-default-value type))
         (repeated-p (eql (proto-field-label field) :repeated))
         (map-p (map-field-p field)))
    `(,slot-name
      :initarg ,(field-name-to-keyword name)
      :accessor ,accessor-name
      :initform ,(if (or repeated-p map-p) 'nil default)
      :type ,(if (or repeated-p map-p) 'list lisp-type))))

;;; Serialization code generation

(defun generate-map-entry-serializer (key-var value-var key-type value-type buffer-var)
  "Generate code to serialize a map entry (key=1, value=2) into a sub-buffer."
  (let ((key-wire-type (proto-type-wire-type key-type))
        (value-wire-type (if (enum-type-p value-type)
                             +wire-type-varint+
                             (proto-type-wire-type value-type)))
        (value-serialize-type (if (enum-type-p value-type) :enum value-type)))
    `(let ((entry-buffer (make-array 16 :element-type '(unsigned-byte 8)
                                        :fill-pointer 0 :adjustable t)))
       ;; Serialize key as field 1
       (write-field-tag 1 ,key-wire-type entry-buffer)
       ,(generate-value-serializer key-var key-type 'entry-buffer)
       ;; Serialize value as field 2
       (write-field-tag 2 ,value-wire-type entry-buffer)
       ,(generate-value-serializer value-var value-serialize-type 'entry-buffer)
       ;; Write the entry as length-delimited to the main buffer
       (write-length-delimited entry-buffer ,buffer-var))))

(defun generate-field-serializer (field &optional oneofs)
  "Generate serialization code for a field.
ONEOFS is the list of oneof descriptors for the message."
  (let* ((name (proto-field-name field))
         (slot-name (field-name-to-slot-name name))
         (field-num (proto-field-number field))
         (type (proto-field-type field))
         ;; Check if this is an enum type (stored as string)
         (is-enum (enum-type-p type))
         ;; Use varint wire type for enums, otherwise use standard wire type
         (wire-type (if is-enum +wire-type-varint+ (proto-type-wire-type type)))
         ;; Use :enum for serialization if this is an enum type
         (serialize-type (if is-enum :enum type))
         (repeated-p (eql (proto-field-label field) :repeated))
         (map-p (map-field-p field))
         (oneof-index (proto-field-oneof-index field)))
    (cond
      ;; Map field - serialize each alist entry as a length-delimited map entry
      (map-p
       (let ((key-type (proto-field-map-key-type field))
             (value-type (proto-field-map-value-type field)))
         `(dolist (entry (slot-value obj ',slot-name))
            (write-field-tag ,field-num ,+wire-type-length-delimited+ buffer)
            ,(generate-map-entry-serializer '(car entry) '(cdr entry)
                                            key-type value-type 'buffer))))
      ;; Repeated field - serialize each element
      (repeated-p
       `(dolist (elem (slot-value obj ',slot-name))
          (write-field-tag ,field-num ,wire-type buffer)
          ,(generate-value-serializer 'elem serialize-type 'buffer)))
      ;; Singular field - only serialize if non-default
      (t
       (let ((default (if is-enum 0 (proto3-default-value type))))
         (if oneof-index
             ;; Oneof field - only serialize if this variant is active
             (let* ((oneof (when oneofs (nth oneof-index oneofs)))
                    (oneof-name (when oneof (proto-oneof-name oneof)))
                    (case-slot (when oneof-name
                                 (intern (format nil "~A-CASE"
                                                 (string-upcase (substitute #\- #\_ oneof-name))))))
                    (field-keyword (field-name-to-keyword name)))
               `(when (eq (slot-value obj ',case-slot) ,field-keyword)
                  (let ((value (slot-value obj ',slot-name)))
                    (when ,(generate-non-default-check 'value serialize-type default)
                      (write-field-tag ,field-num ,wire-type buffer)
                      ,(generate-value-serializer 'value serialize-type 'buffer)))))
             ;; Regular field
             `(let ((value (slot-value obj ',slot-name)))
                (when ,(generate-non-default-check 'value serialize-type default)
                  (write-field-tag ,field-num ,wire-type buffer)
                  ,(generate-value-serializer 'value serialize-type 'buffer)))))))))

(defun generate-non-default-check (var type default)
  "Generate code to check if a value is non-default"
  (case type
    (:string `(and ,var (plusp (length ,var))))
    (:bytes `(and ,var (plusp (length ,var))))
    (:bool var)  ; nil is default, so just check truthiness
    ((:double :float) `(and ,var (not (zerop ,var))))
    (otherwise (if (null default)
           var  ; message types - check for non-nil
           `(and ,var (not (eql ,var ,default)))))))

(defun generate-value-serializer (value-var type buffer-var)
  "Generate code to serialize a value of the given type to a buffer"
  (case type
    ((:int32 :int64 :uint32 :uint64 :enum)
     `(write-varint ,value-var ,buffer-var))
    (:bool
     `(write-varint (if ,value-var 1 0) ,buffer-var))
    ((:sint32)
     `(write-varint (zigzag-encode ,value-var) ,buffer-var))
    ((:sint64)
     `(write-varint (zigzag-encode ,value-var) ,buffer-var))
    (:fixed32
     `(write-fixed32 ,value-var ,buffer-var))
    (:fixed64
     `(write-fixed64 ,value-var ,buffer-var))
    (:sfixed32
     `(write-fixed32 (if (minusp ,value-var)
                         (+ ,value-var #x100000000)
                         ,value-var)
                     ,buffer-var))
    (:sfixed64
     `(write-fixed64 (if (minusp ,value-var)
                         (+ ,value-var #x10000000000000000)
                         ,value-var)
                     ,buffer-var))
    (:float
     `(write-fixed32 (float-to-ieee754-32 ,value-var) ,buffer-var))
    (:double
     `(write-fixed64 (float-to-ieee754-64 ,value-var) ,buffer-var))
    (:string
     `(write-length-delimited (string-to-utf8 ,value-var) ,buffer-var))
    (:bytes
     `(write-length-delimited ,value-var ,buffer-var))
    (otherwise
     ;; Message type - serialize recursively
     `(write-length-delimited (serialize-to-bytes ,value-var) ,buffer-var))))

;;; Deserialization code generation

(defun generate-map-entry-deserializer (key-type value-type)
  "Generate code to deserialize a map entry from length-delimited data.
Returns an expression that evaluates to (key . value)."
  (let ((key-deserialize-type key-type)
        (value-deserialize-type (if (enum-type-p value-type) :enum value-type)))
    `(let* ((entry-data (read-length-delimited buffer))
            (entry-buf (cons entry-data 0))
            (key ,(proto3-default-value key-type))
            (value ,(proto3-default-value value-deserialize-type)))
       (loop while (< (rest entry-buf) (length entry-data))
             do (let* ((entry-tag (read-varint entry-buf))
                       (entry-field-number (ash entry-tag -3))
                       (entry-wire-type (logand entry-tag #x7)))
                  (declare (ignorable entry-field-number entry-wire-type))
                  (case entry-field-number
                    (1 (setf key ,(generate-value-deserializer key-deserialize-type
                                                               'entry-buf 'entry-wire-type)))
                    (2 (setf value ,(generate-value-deserializer value-deserialize-type
                                                                  'entry-buf 'entry-wire-type)))
                    (otherwise (skip-field entry-buf (logand entry-tag #x7))))))
       (cons key value))))

(defun generate-field-deserializer-case (field class-name &optional oneofs)
  "Generate a case clause for deserializing a field.
ONEOFS is the list of oneof descriptors for the message."
  (declare (ignorable class-name))
  (let* ((name (proto-field-name field))
         (slot-name (field-name-to-slot-name name))
         (field-num (proto-field-number field))
         (type (proto-field-type field))
         ;; Check if this is an enum type
         (is-enum (enum-type-p type))
         ;; Use :enum for deserialization if this is an enum type
         (deserialize-type (if is-enum :enum type))
         (repeated-p (eql (proto-field-label field) :repeated))
         (map-p (map-field-p field))
         (oneof-index (proto-field-oneof-index field)))
    `(,field-num
      ,(cond
         ;; Map field - deserialize entry and push onto alist
         (map-p
          `(push ,(generate-map-entry-deserializer
                   (proto-field-map-key-type field)
                   (proto-field-map-value-type field))
                 (slot-value obj ',slot-name)))
         ;; Repeated field
         (repeated-p
          `(push ,(generate-value-deserializer deserialize-type 'buffer 'wire-type)
                 (slot-value obj ',slot-name)))
         ;; Oneof field
         (oneof-index
          (let* ((oneof (nth oneof-index oneofs))
                 (oneof-name (when oneof (proto-oneof-name oneof)))
                 (case-slot (when oneof-name
                              (intern (format nil "~A-CASE"
                                              (string-upcase (substitute #\- #\_ oneof-name))))))
                 (field-keyword (field-name-to-keyword name))
                 (other-fields (when oneof
                                 (remove field (proto-oneof-fields oneof)))))
            `(progn
               ;; Clear other fields in this oneof
               ,@(mapcar (lambda (f)
                           `(setf (slot-value obj ',(field-name-to-slot-name (proto-field-name f)))
                                  ,(proto3-default-value (proto-field-type f))))
                         other-fields)
               ;; Set this field
               (setf (slot-value obj ',slot-name)
                     ,(generate-value-deserializer deserialize-type 'buffer 'wire-type))
               ;; Update case slot
               ,@(when case-slot
                   `((setf (slot-value obj ',case-slot) ,field-keyword))))))
         ;; Regular field
         (t
          `(setf (slot-value obj ',slot-name)
                 ,(generate-value-deserializer deserialize-type 'buffer 'wire-type)))))))
(defun generate-value-deserializer (type buffer-var wire-type-var)
  "Generate code to deserialize a value of the given type"
  (declare (ignorable wire-type-var))
  (case type
    (:int32
     `(let ((n (logand (read-varint ,buffer-var) #xFFFFFFFF)))  ; mask to 32 bits
        (if (>= n #x80000000)
            (- n #x100000000)
            n)))
    (:int64
     `(let ((n (read-varint ,buffer-var)))
        (if (>= n #x8000000000000000)
            (- n #x10000000000000000)
            n)))
    ((:uint32 :uint64 :enum)
     `(read-varint ,buffer-var))
    (:sint32
     `(let ((n (read-varint ,buffer-var)))
        (zigzag-decode (logand n #xffffffff))))
    (:sint64
     `(zigzag-decode (read-varint ,buffer-var)))
    (:bool
     `(not (zerop (read-varint ,buffer-var))))
    (:fixed32
     `(read-fixed32 ,buffer-var))
    (:fixed64
     `(read-fixed64 ,buffer-var))
    (:sfixed32
     `(let ((n (read-fixed32 ,buffer-var)))
        (if (>= n #x80000000)
            (- n #x100000000)
            n)))
    (:sfixed64
     `(let ((n (read-fixed64 ,buffer-var)))
        (if (>= n #x8000000000000000)
            (- n #x10000000000000000)
            n)))
    (:float
     `(ieee754-32-to-float (read-fixed32 ,buffer-var)))
    (:double
     `(ieee754-64-to-float (read-fixed64 ,buffer-var)))
    (:string
     `(utf8-to-string (read-length-delimited ,buffer-var)))
    (:bytes
     `(read-length-delimited-view ,buffer-var))
    (otherwise
     ;; Message type - deserialize recursively
     ;; Strip package prefix (e.g., "google.protobuf.Any" -> "Any")
     ;; and convert to kebab-case (e.g., "UnaryRequest" -> "UNARY-REQUEST")
     ;; Use safe-class-name to handle reserved symbols like ERROR
     (let* ((simple-name (if (keywordp type)
                             (symbol-name type)
                             (let ((pos (position #\. type :from-end t)))
                               (if pos
                                   (subseq type (1+ pos))
                                   type))))
            (lisp-name-str (lisp-name simple-name))
            (type-class (safe-class-name lisp-name-str nil)))
       `(let ((data (read-length-delimited ,buffer-var)))
          (when (and *max-recursion-depth*
                     (>= *current-recursion-depth* *max-recursion-depth*))
            (error 'wire-format-error
                   :message (format nil "Maximum recursion depth ~D exceeded"
                                    *max-recursion-depth*)))
          (let ((*current-recursion-depth* (1+ *current-recursion-depth*)))
            (deserialize-from-bytes ',type-class data)))))))

;;; Buffer-based serialization primitives

(defun write-varint (n buffer)
  "Write a varint to a buffer (adjustable vector).
   Negative numbers are treated as 64-bit unsigned (10 bytes max)."
  ;; Convert negative numbers to their 64-bit unsigned representation
  (when (minusp n)
    (incf n 18446744073709551616))
  (loop
    (let ((byte (logand n #x7f)))
      (setf n (ash n -7))
      (cond ((zerop n) (vector-push-extend byte buffer) (return)) (t (vector-push-extend (logior byte 128) buffer))))))

(defun write-fixed32 (n buffer)
  "Write a fixed32 (little-endian) to a buffer"
  (vector-push-extend (logand n #xff) buffer)
  (vector-push-extend (logand (ash n -8) #xff) buffer)
  (vector-push-extend (logand (ash n -16) #xff) buffer)
  (vector-push-extend (logand (ash n -24) #xff) buffer))

(defun write-fixed64 (n buffer)
  "Write a fixed64 (little-endian) to a buffer"
  (loop for i from 0 below 8
        do (vector-push-extend (logand (ash n (* i -8)) #xff) buffer)))

(defun write-length-delimited (data buffer)
  "Write length-delimited data to a buffer"
  (write-varint (length data) buffer)
  (loop for byte across data
        do (vector-push-extend byte buffer)))

(defun write-field-tag (field-number wire-type buffer)
  "Write a field tag to a buffer"
  (write-varint (make-field-tag field-number wire-type) buffer))

;;; Buffer-based deserialization primitives

(defun read-varint (buffer)
  "Read a varint from a buffer (vector with position tracking).
   Buffer should be a cons of (vector . position)."
  (let* ((result 0)
         (shift 0)
         (raw-data (first buffer))
         ;; Ensure data is a simple array for SBCL optimization
         (data (if (typep raw-data '(simple-array (unsigned-byte 8) (*)))
                   raw-data
                   (let ((simple (make-array (length raw-data) :element-type '(unsigned-byte 8))))
                     (replace simple raw-data)
                     (setf (first buffer) simple)
                     simple)))
         (pos (rest buffer)))
    (loop
      (when (>= pos (length data))
        (error "Unexpected end of buffer reading varint"))
      (let ((byte (aref data pos)))
        (incf pos)
        (setf result (logior result (ash (logand byte #x7f) shift)))
        (when (zerop (logand byte #x80))
          (setf (rest buffer) pos)
          (return result))
        (incf shift 7)))))

(defun read-fixed32 (buffer)
  "Read a fixed32 from a buffer"
  (let ((data (first buffer))
        (pos (rest buffer)))
    (when (> (+ pos 4) (length data))
      (error "Unexpected end of buffer reading fixed32"))
    (let ((result (logior (aref data pos)
                          (ash (aref data (1+ pos)) 8)
                          (ash (aref data (+ pos 2)) 16)
                          (ash (aref data (+ pos 3)) 24))))
      (setf (rest buffer) (+ pos 4))
      result)))

(defun read-fixed64 (buffer)
  "Read a fixed64 from a buffer"
  (let ((data (first buffer))
        (pos (rest buffer)))
    (when (> (+ pos 8) (length data))
      (error "Unexpected end of buffer reading fixed64"))
    (let ((result 0))
      (loop for i from 0 below 8
            do (setf result (logior result (ash (aref data (+ pos i)) (* i 8)))))
      (setf (rest buffer) (+ pos 8))
      result)))

(defvar *zero-copy-bytes* nil
  "When true, read-length-delimited-view returns displaced arrays that share
storage with the receive buffer instead of copying.  This reduces allocation
on the decode path but changes the representation: displaced arrays are not
simple-arrays, and they share ownership with the backing buffer.
Set to NIL to always copy (safe default for backwards compatibility).
Default: NIL.")

(defun read-length-delimited (buffer)
  "Read length-delimited data from a buffer, returns a fresh vector"
  (let* ((len (read-varint buffer))
         (data (first buffer))
         (pos (rest buffer)))
    (when (> (+ pos len) (length data))
      (error "Unexpected end of buffer reading length-delimited data"))
    (let ((result (make-array len :element-type '(unsigned-byte 8))))
      (loop for i from 0 below len
            do (setf (aref result i) (aref data (+ pos i))))
      (setf (rest buffer) (+ pos len))
      result)))

(defun read-length-delimited-view (buffer)
  "Read length-delimited data from a buffer.  When *zero-copy-bytes* is true
and the backing data is a simple-array, returns a displaced array sharing
storage with the buffer (zero-copy).  Otherwise falls back to copying."
  (let* ((len (read-varint buffer))
         (data (first buffer))
         (pos (rest buffer)))
    (when (> (+ pos len) (length data))
      (error "Unexpected end of buffer reading length-delimited data"))
    (setf (rest buffer) (+ pos len))
    (if (and *zero-copy-bytes*
             (typep data '(simple-array (unsigned-byte 8) (*))))
        ;; Zero-copy: displaced array into the receive buffer
        (make-array len :element-type '(unsigned-byte 8)
                        :displaced-to data
                        :displaced-index-offset pos)
        ;; Fallback: copy
        (let ((result (make-array len :element-type '(unsigned-byte 8))))
          (replace result data :start2 pos :end2 (+ pos len))
          result))))

(defun skip-field (buffer wire-type)
  "Skip a field in the buffer based on wire type"
  (case wire-type
    (#.+wire-type-varint+
     (read-varint buffer))
    (#.+wire-type-fixed64+
     (incf (rest buffer) 8))
    (#.+wire-type-length-delimited+
     (let ((len (read-varint buffer)))
       (incf (rest buffer) len)))
    (#.+wire-type-fixed32+
     (incf (rest buffer) 4))
    (otherwise
     (error "Unknown wire type: ~A" wire-type))))

;;; Class and method generation

(defun generate-class-definition (message-desc &optional package)
  "Generate a CLOS class definition from a message descriptor"
  (let* ((name (proto-message-name message-desc))
         (lisp-name-str (lisp-name name))
         (class-name (safe-class-name lisp-name-str package))
         (fields (proto-message-fields message-desc))
         (oneofs (proto-message-oneofs message-desc))
         ;; Pass message name to generate-slot-definition for message-specific accessor names
         (field-slots (mapcar (lambda (f) (generate-slot-definition f package name)) fields))
         ;; Generate a case slot for each oneof to track which variant is set
         (oneof-case-slots (mapcar (lambda (o) (generate-oneof-case-slot o package)) oneofs))
         (all-slots (append field-slots oneof-case-slots)))
    `(defclass ,class-name (proto-message)
       ,all-slots
       (:documentation ,(format nil "Proto message: ~A" name)))))

(defun generate-oneof-case-slot (oneof-desc &optional package)
  "Generate a slot to track which oneof variant is currently set"
  (let* ((name (proto-oneof-name oneof-desc))
         (slot-name (intern (format nil "~A-CASE" (string-upcase (substitute #\- #\_ name))) package))
         (accessor-name (if package
                            (intern (format nil "~A-CASE" (string-upcase (substitute #\- #\_ name))) package)
                            slot-name)))
    `(,slot-name
      :initform nil
      :accessor ,accessor-name
      :documentation ,(format nil "Which ~A variant is set (field name keyword or nil)" name))))

(defun generate-serializer (message-desc &optional package)
  "Generate a serialize-to-bytes method for a message"
  (let* ((name (proto-message-name message-desc))
         (lisp-name-str (lisp-name name))
         (class-name (safe-class-name lisp-name-str package))
         (fields (proto-message-fields message-desc))
         (oneofs (proto-message-oneofs message-desc))
         (field-serializers (mapcar (lambda (f) (generate-field-serializer f oneofs)) fields)))
    `(defmethod serialize-to-bytes ((obj ,class-name))
       (let ((buffer (make-array 64 :element-type '(unsigned-byte 8)
                                    :fill-pointer 0 :adjustable t)))
         ,@field-serializers
         buffer))))

(defun generate-deserializer (message-desc &optional package)
  "Generate a deserialize-from-bytes method for a message"
  (let* ((name (proto-message-name message-desc))
         (lisp-name-str (lisp-name name))
         (class-name (safe-class-name lisp-name-str package))
         (fields (proto-message-fields message-desc))
         (oneofs (proto-message-oneofs message-desc))
         (field-cases (mapcar (lambda (f) (generate-field-deserializer-case f class-name oneofs))
                              fields))
         ;; Reverse repeated and map fields at the end (accumulated with push)
         (list-fields (remove-if-not (lambda (f)
                                       (or (eql (proto-field-label f) :repeated)
                                           (map-field-p f)))
                                     fields))
         (reverse-stmts (mapcar (lambda (f)
                                  (let ((slot-name (field-name-to-slot-name (proto-field-name f))))
                                    `(setf (slot-value obj ',slot-name)
                                           (nreverse (slot-value obj ',slot-name)))))
                                list-fields)))
    `(defmethod deserialize-from-bytes ((type (eql ',class-name)) data)
       (let ((obj (make-instance ',class-name))
             (buffer (cons data 0)))  ; (vector . position)
         (loop while (< (rest buffer) (length data))
               do (let* ((tag (read-varint buffer))
                         (field-number (ash tag -3))
                         (wire-type (logand tag #x7)))
                    (case field-number
                      ,@field-cases
                      (otherwise (skip-field buffer wire-type)))))
         ,@reverse-stmts
         obj))))

;;; Enum generation

(defun generate-enum-definition (enum-desc &optional package)
  "Generate constants for an enum"
  (let* ((name (proto-enum-name enum-desc))
         (prefix (string-upcase name))
         (values (proto-enum-values enum-desc)))
    `(progn
       ,@(mapcar (lambda (v)
                   (let* ((value-name (proto-enum-value-name v))
                          (const-name (intern (format nil "+~A-~A+" prefix value-name) package))
                          (number (proto-enum-value-number v)))
                     `(defconstant ,const-name ,number)))
                 values))))

;;; Main code generation entry points

(defun collect-enum-names (file-desc)
  "Collect all enum type names from a file descriptor into a hash table.
Includes top-level enums and nested enums from messages."
  (let ((enum-table (make-hash-table :test 'equal)))
    ;; Top-level enums
    (dolist (enum (proto-file-enums file-desc))
      (setf (gethash (proto-enum-name enum) enum-table) t))
    ;; Nested enums in messages
    (labels ((collect-from-message (msg prefix)
               (let ((full-prefix (if prefix
                                      (format nil "~A.~A" prefix (proto-message-name msg))
                                      (proto-message-name msg))))
                 ;; Nested enums
                 (dolist (nested-enum (proto-message-nested-enums msg))
                   (setf (gethash (proto-enum-name nested-enum) enum-table) t)
                   (setf (gethash (format nil "~A.~A" full-prefix (proto-enum-name nested-enum))
                                  enum-table) t))
                 ;; Recurse into nested messages
                 (dolist (nested-msg (proto-message-nested-messages msg))
                   (collect-from-message nested-msg full-prefix)))))
      (dolist (msg (proto-file-messages file-desc))
        (collect-from-message msg nil)))
    enum-table))

(defun generate-lisp-code (file-desc &key (package *package*) (generate-stubs t) additional-enum-types class-prefix)
  "Generate Lisp code for all messages in a proto file descriptor.
Returns a list of forms to be compiled.
If GENERATE-STUBS is true (default), also generates client stubs for services.
ADDITIONAL-ENUM-TYPES is a hash table of extra enum type names to include (from imports).
CLASS-PREFIX is an optional string to prefix all generated class and accessor names (e.g., \"PROTO-\")."
  ;; Build enum types table for this file, merging with additional types
  (let* ((local-enums (collect-enum-names file-desc))
         (*known-enum-types* (cond (additional-enum-types
                                        (maphash (lambda (k v)
                                                   (setf (gethash k local-enums) v))
                                                 additional-enum-types)
                                        local-enums)
                                       (t local-enums)))
         (*class-prefix* class-prefix)
         (messages (proto-file-messages file-desc))
         (enums (proto-file-enums file-desc))
         (services (proto-file-services file-desc))
         (proto-package (proto-file-package file-desc))
         (forms nil))
    ;; Generate enums first
    (dolist (enum enums)
      (push (generate-enum-definition enum package) forms))
    ;; Generate message classes
    (dolist (msg messages)
      (push (generate-class-definition msg package) forms)
      (push (generate-serializer msg package) forms)
      (push (generate-deserializer msg package) forms)
      ;; Handle nested enums
      (dolist (nested-enum (proto-message-nested-enums msg))
        (push (generate-enum-definition nested-enum package) forms))
      ;; Handle nested messages
      (dolist (nested-msg (proto-message-nested-messages msg))
        (push (generate-class-definition nested-msg package) forms)
        (push (generate-serializer nested-msg package) forms)
        (push (generate-deserializer nested-msg package) forms)))
    ;; Generate service stubs
    (when generate-stubs
      (dolist (service services)
        (dolist (form (generate-service-code service proto-package package))
          (push form forms))))
    (nreverse forms)))

(defun compile-proto-file (pathname &key (output-file nil) (load t) (package *package*))
  "Compile a .proto file to Lisp code.
If OUTPUT-FILE is provided, writes the generated code to that file.
If LOAD is true, also loads the generated code."
  (let* ((file-desc (parse-proto-file pathname))
         (forms (generate-lisp-code file-desc :package package)))
    (when output-file
      (with-open-file (out output-file :direction :output :if-exists :supersede)
        (format out ";;;; Generated from ~A~%~%" pathname)
        (format out "(in-package ~S)~%~%" (package-name package))
        (dolist (form forms)
          (pprint form out)
          (terpri out)
          (terpri out))))
    (when load
      (mapc #'eval forms))
    forms))

(defun compile-proto-string (string &key (load t) (package *package*))
  "Compile a proto definition from a string.
If LOAD is true, evaluates the generated code.
Returns the list of generated forms."
  (let* ((file-desc (parse-proto-string string))
         (forms (generate-lisp-code file-desc :package package)))
    (when load
      (mapc #'eval forms))
    forms))

;;;; ========================================================================
;;;; Client Stub Generation
;;;;
;;;; Generates typed client stubs for gRPC services.
;;;; Each service becomes a stub class with methods for each RPC.
;;;; ========================================================================

(defun generate-stub-class (service-desc package)
  "Generate a stub class definition for a service"
  (let* ((name (proto-service-name service-desc))
         (class-name (intern (lisp-name name "-STUB") package)))
    `(defclass ,class-name ()
       ((channel :initarg :channel :accessor stub-channel
                 :documentation "gRPC channel for this stub"))
       (:documentation ,(format nil "Client stub for ~A service" name)))))

(defun generate-stub-constructor (service-desc package)
  "Generate a constructor function for a stub"
  (let* ((name (proto-service-name service-desc))
         (class-name (intern (lisp-name name "-STUB") package))
         (constructor-name (intern (concatenate 'string "MAKE-" (lisp-name name "-STUB")) package)))
    `(defun ,constructor-name (channel)
       ,(format nil "Create a new ~A client stub" name)
       (make-instance ',class-name :channel channel))))

(defun generate-rpc-method (service-desc method-desc proto-package package)
  "Generate a method for an RPC call"
  (let* ((service-name (proto-service-name service-desc))
         (method-name (proto-method-name method-desc))
         (stub-class (intern (lisp-name service-name "-STUB") package))
         (fn-name (intern (lisp-name (format nil "~A-~A" service-name method-name)) package))
         (output-type (proto-method-output-type method-desc))
         (response-class (safe-class-name (lisp-name output-type) package))
         ;; Build the method path: /package.Service/Method
         (method-path (if (and proto-package (plusp (length proto-package)))
                          (format nil "/~A.~A/~A" proto-package service-name method-name)
                          (format nil "/~A/~A" service-name method-name)))
         (client-streaming (proto-method-client-streaming method-desc))
         (server-streaming (proto-method-server-streaming method-desc)))
    (cond
      ;; Bidirectional streaming - returns a bidi stream object
      ((and client-streaming server-streaming)
       `(defmethod ,fn-name ((stub ,stub-class) &key metadata timeout)
          ,(format nil "Initiate ~A.~A bidirectional streaming RPC.
Returns a grpc-bidi-stream. Use stream-send to send messages,
stream-read-message to receive messages, and stream-close-send when done sending." service-name method-name)
          (let* ((grpc-pkg (find-package :ag-grpc))
                 (call-fn (and grpc-pkg (symbol-function (find-symbol "CALL-BIDIRECTIONAL-STREAMING" grpc-pkg)))))
            (unless call-fn
              (cl:error "ag-grpc package not loaded. Load ag-grpc before calling RPC methods."))
            (funcall call-fn
                     (stub-channel stub)
                     ,method-path
                     :response-type ',response-class
                     :metadata metadata
                     :timeout timeout))))
      ;; Client streaming - returns a client stream object
      (client-streaming
       `(defmethod ,fn-name ((stub ,stub-class) &key metadata timeout)
          ,(format nil "Initiate ~A.~A client streaming RPC.
Returns a grpc-client-stream. Use stream-send to send messages,
then stream-close-and-recv to get the response." service-name method-name)
          (let* ((grpc-pkg (find-package :ag-grpc))
                 (call-fn (and grpc-pkg (symbol-function (find-symbol "CALL-CLIENT-STREAMING" grpc-pkg)))))
            (unless call-fn
              (cl:error "ag-grpc package not loaded. Load ag-grpc before calling RPC methods."))
            (funcall call-fn
                     (stub-channel stub)
                     ,method-path
                     :response-type ',response-class
                     :metadata metadata
                     :timeout timeout))))
      ;; Server streaming - returns a stream object
      (server-streaming
       `(defmethod ,fn-name ((stub ,stub-class) request &key metadata timeout)
          ,(format nil "Call ~A.~A server streaming RPC.
Returns a grpc-server-stream. Use stream-read-message or do-stream-messages to consume." service-name method-name)
          (let* ((grpc-pkg (find-package :ag-grpc))
                 (call-fn (and grpc-pkg (symbol-function (find-symbol "CALL-SERVER-STREAM" grpc-pkg)))))
            (unless call-fn
              (cl:error "ag-grpc package not loaded. Load ag-grpc before calling RPC methods."))
            (funcall call-fn
                     (stub-channel stub)
                     ,method-path
                     request
                     :response-type ',response-class
                     :metadata metadata
                     :timeout timeout))))
      ;; Unary RPC
      (t
       `(defmethod ,fn-name ((stub ,stub-class) request &key metadata timeout)
          ,(format nil "Call ~A.~A unary RPC" service-name method-name)
          (let* ((grpc-pkg (find-package :ag-grpc))
                 (call-fn (and grpc-pkg (symbol-function (find-symbol "CALL-UNARY" grpc-pkg))))
                 (response-fn (and grpc-pkg (fdefinition (find-symbol "CALL-RESPONSE" grpc-pkg))))
                 (status-fn (and grpc-pkg (fdefinition (find-symbol "CALL-STATUS" grpc-pkg)))))
            (unless call-fn
              (cl:error "ag-grpc package not loaded. Load ag-grpc before calling RPC methods."))
            (let ((call (funcall call-fn
                                 (stub-channel stub)
                                 ,method-path
                                 request
                                 :response-type ',response-class
                                 :metadata metadata
                                 :timeout timeout)))
              (values (funcall response-fn call)
                      (funcall status-fn call)
                      call))))))))

(defun generate-service-code (service-desc proto-package package)
  "Generate all code for a service (stub class, constructor, methods)"
  (let ((forms nil))
    ;; Stub class
    (push (generate-stub-class service-desc package) forms)
    ;; Constructor
    (push (generate-stub-constructor service-desc package) forms)
    ;; RPC methods
    (dolist (method (proto-service-methods service-desc))
      (push (generate-rpc-method service-desc method proto-package package) forms))
    ;; Server-side registration helper (monomorphic dispatch)
    (push (generate-service-registrar service-desc proto-package package) forms)
    (nreverse forms)))

(defun generate-service-registrar (service-desc proto-package package)
  "Generate a server registration function that uses compile-time dispatch.
Produces REGISTER-<SERVICE>-SERVICE which registers all handlers and installs
a cond-based dispatch function, avoiding hash-table lookup on the hot path."
  (let* ((service-name (proto-service-name service-desc))
         (register-fn (intern (concatenate 'string "REGISTER-"
                                           (lisp-name service-name)
                                           "-SERVICE")
                              package))
         (methods (proto-service-methods service-desc))
         (handler-params (mapcar (lambda (m)
                                   (intern (lisp-name (format nil "~A-HANDLER"
                                                              (proto-method-name m)))
                                           package))
                                 methods))
         ;; Build method paths
         (method-paths (mapcar (lambda (m)
                                 (if (and proto-package (plusp (length proto-package)))
                                     (format nil "/~A.~A/~A" proto-package service-name
                                             (proto-method-name m))
                                     (format nil "/~A/~A" service-name (proto-method-name m))))
                               methods)))
    `(defun ,register-fn (server &key ,@handler-params)
       ,(format nil "Register all ~A service handlers with SERVER.~%Uses compile-time dispatch on method path (no hash-table lookup per request).~%Each keyword arg is a handler function for the corresponding RPC method."
                service-name)
       ;; Register each handler individually (for introspection/reflection)
       ,@(loop for method in methods
               for param in handler-params
               for path in method-paths
               collect
               (let* ((input-type (proto-method-input-type method))
                      (output-type (proto-method-output-type method))
                      (request-class (safe-class-name (lisp-name input-type) package))
                      (response-class (safe-class-name (lisp-name output-type) package)))
                 `(when ,param
                    (let* ((grpc-pkg (find-package :ag-grpc))
                           (register-fn (and grpc-pkg (fdefinition
                                                        (find-symbol "SERVER-REGISTER-HANDLER" grpc-pkg)))))
                      (when register-fn
                        (funcall register-fn server ,path ,param
                                 :request-type ',request-class
                                 :response-type ',response-class
                                 :client-streaming ,(proto-method-client-streaming method)
                                 :server-streaming ,(proto-method-server-streaming method)))))))
       server)))
