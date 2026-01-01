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
    (t t)))  ; Message types become t

(defun proto3-default-value (type)
  "Return the Proto3 default value for a type"
  (case type
    (:double 0.0d0)
    (:float 0.0f0)
    ((:int32 :int64 :uint32 :uint64 :sint32 :sint64
      :fixed32 :fixed64 :sfixed32 :sfixed64 :enum) 0)
    (:bool nil)
    (:string "")
    (:bytes (make-array 0 :element-type '(unsigned-byte 8)))
    (t nil)))  ; Message types default to nil

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
    "OPEN" "CLOSE" "TIME" "SLEEP" "RANDOM")
  "CL symbols that should not be used as accessor names (to avoid package lock violations)")

(defun safe-accessor-name (name package)
  "Generate a safe accessor name, prefixing if it conflicts with CL symbols"
  (let ((upname (string-upcase (substitute #\- #\_ name))))
    (if (member upname *cl-reserved-names* :test #'string=)
        (intern (concatenate 'string "PROTO-" upname) package)
        (intern upname package))))

(defun field-name-to-slot-name (name)
  "Convert a field name to a slot name symbol"
  (intern (string-upcase (substitute #\- #\_ name))))

(defun field-name-to-keyword (name)
  "Convert a field name to a keyword symbol"
  (intern (string-upcase (substitute #\- #\_ name)) :keyword))

;;; Slot generation

(defun generate-slot-definition (field &optional package)
  "Generate a slot definition from a field descriptor"
  (let* ((name (proto-field-name field))
         (slot-name (field-name-to-slot-name name))
         (accessor-name (if package
                            (safe-accessor-name name package)
                            slot-name))
         (type (proto-field-type field))
         (lisp-type (proto-type-to-lisp-type type))
         (default (proto3-default-value type))
         (repeated-p (eq (proto-field-label field) :repeated)))
    `(,slot-name
      :initarg ,(field-name-to-keyword name)
      :accessor ,accessor-name
      :initform ,(if repeated-p 'nil default)
      :type ,(if repeated-p 'list lisp-type))))

;;; Serialization code generation

(defun generate-field-serializer (field)
  "Generate serialization code for a field"
  (let* ((name (proto-field-name field))
         (slot-name (field-name-to-slot-name name))
         (field-num (proto-field-number field))
         (type (proto-field-type field))
         (wire-type (proto-type-wire-type type))
         (repeated-p (eq (proto-field-label field) :repeated)))
    (if repeated-p
        ;; Repeated field - serialize each element
        `(dolist (elem (slot-value obj ',slot-name))
           (write-field-tag ,field-num ,wire-type buffer)
           ,(generate-value-serializer 'elem type 'buffer))
        ;; Singular field - only serialize if non-default
        (let ((default (proto3-default-value type)))
          `(let ((value (slot-value obj ',slot-name)))
             (when ,(generate-non-default-check 'value type default)
               (write-field-tag ,field-num ,wire-type buffer)
               ,(generate-value-serializer 'value type 'buffer)))))))

(defun generate-non-default-check (var type default)
  "Generate code to check if a value is non-default"
  (case type
    (:string `(and ,var (plusp (length ,var))))
    (:bytes `(and ,var (plusp (length ,var))))
    (:bool var)  ; nil is default, so just check truthiness
    ((:double :float) `(and ,var (not (zerop ,var))))
    (t (if (null default)
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
    (t
     ;; Message type - serialize recursively
     `(write-length-delimited (serialize-to-bytes ,value-var) ,buffer-var))))

;;; Deserialization code generation

(defun generate-field-deserializer-case (field class-name)
  "Generate a case clause for deserializing a field"
  (let* ((name (proto-field-name field))
         (slot-name (field-name-to-slot-name name))
         (field-num (proto-field-number field))
         (type (proto-field-type field))
         (repeated-p (eq (proto-field-label field) :repeated)))
    `(,field-num
      ,(if repeated-p
           `(push ,(generate-value-deserializer type 'buffer 'wire-type)
                  (slot-value obj ',slot-name))
           `(setf (slot-value obj ',slot-name)
                  ,(generate-value-deserializer type 'buffer 'wire-type))))))

(defun generate-value-deserializer (type buffer-var wire-type-var)
  "Generate code to deserialize a value of the given type"
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
     `(read-length-delimited ,buffer-var))
    (t
     ;; Message type - deserialize recursively
     (let ((type-class (if (keywordp type)
                           type
                           (intern (string-upcase type)))))
       `(let ((data (read-length-delimited ,buffer-var)))
          (deserialize-from-bytes ',type-class data))))))

;;; Buffer-based serialization primitives

(defun write-varint (n buffer)
  "Write a varint to a buffer (adjustable vector).
   Negative numbers are treated as 64-bit unsigned (10 bytes max)."
  ;; Convert negative numbers to their 64-bit unsigned representation
  (when (minusp n)
    (setf n (+ n #x10000000000000000)))
  (loop
    (let ((byte (logand n #x7f)))
      (setf n (ash n -7))
      (if (zerop n)
          (progn
            (vector-push-extend byte buffer)
            (return))
          (vector-push-extend (logior byte #x80) buffer)))))

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
  (let ((result 0)
        (shift 0)
        (data (car buffer))
        (pos (cdr buffer)))
    (loop
      (when (>= pos (length data))
        (error "Unexpected end of buffer reading varint"))
      (let ((byte (aref data pos)))
        (incf pos)
        (setf result (logior result (ash (logand byte #x7f) shift)))
        (when (zerop (logand byte #x80))
          (setf (cdr buffer) pos)
          (return result))
        (incf shift 7)))))

(defun read-fixed32 (buffer)
  "Read a fixed32 from a buffer"
  (let ((data (car buffer))
        (pos (cdr buffer)))
    (when (> (+ pos 4) (length data))
      (error "Unexpected end of buffer reading fixed32"))
    (let ((result (logior (aref data pos)
                          (ash (aref data (+ pos 1)) 8)
                          (ash (aref data (+ pos 2)) 16)
                          (ash (aref data (+ pos 3)) 24))))
      (setf (cdr buffer) (+ pos 4))
      result)))

(defun read-fixed64 (buffer)
  "Read a fixed64 from a buffer"
  (let ((data (car buffer))
        (pos (cdr buffer)))
    (when (> (+ pos 8) (length data))
      (error "Unexpected end of buffer reading fixed64"))
    (let ((result 0))
      (loop for i from 0 below 8
            do (setf result (logior result (ash (aref data (+ pos i)) (* i 8)))))
      (setf (cdr buffer) (+ pos 8))
      result)))

(defun read-length-delimited (buffer)
  "Read length-delimited data from a buffer, returns a fresh vector"
  (let* ((len (read-varint buffer))
         (data (car buffer))
         (pos (cdr buffer)))
    (when (> (+ pos len) (length data))
      (error "Unexpected end of buffer reading length-delimited data"))
    (let ((result (make-array len :element-type '(unsigned-byte 8))))
      (loop for i from 0 below len
            do (setf (aref result i) (aref data (+ pos i))))
      (setf (cdr buffer) (+ pos len))
      result)))

(defun skip-field (buffer wire-type)
  "Skip a field in the buffer based on wire type"
  (case wire-type
    (#.+wire-type-varint+
     (read-varint buffer))
    (#.+wire-type-fixed64+
     (incf (cdr buffer) 8))
    (#.+wire-type-length-delimited+
     (let ((len (read-varint buffer)))
       (incf (cdr buffer) len)))
    (#.+wire-type-fixed32+
     (incf (cdr buffer) 4))
    (t
     (error "Unknown wire type: ~A" wire-type))))

;;; Class and method generation

(defun generate-class-definition (message-desc &optional package)
  "Generate a CLOS class definition from a message descriptor"
  (let* ((name (proto-message-name message-desc))
         (class-name (intern (string-upcase name) package))
         (fields (proto-message-fields message-desc))
         (slots (mapcar (lambda (f) (generate-slot-definition f package)) fields)))
    `(defclass ,class-name (proto-message)
       ,slots
       (:documentation ,(format nil "Proto message: ~A" name)))))

(defun generate-serializer (message-desc &optional package)
  "Generate a serialize-to-bytes method for a message"
  (let* ((name (proto-message-name message-desc))
         (class-name (intern (string-upcase name) package))
         (fields (proto-message-fields message-desc))
         (field-serializers (mapcar #'generate-field-serializer fields)))
    `(defmethod serialize-to-bytes ((obj ,class-name))
       (let ((buffer (make-array 64 :element-type '(unsigned-byte 8)
                                    :fill-pointer 0 :adjustable t)))
         ,@field-serializers
         buffer))))

(defun generate-deserializer (message-desc &optional package)
  "Generate a deserialize-from-bytes method for a message"
  (let* ((name (proto-message-name message-desc))
         (class-name (intern (string-upcase name) package))
         (fields (proto-message-fields message-desc))
         (field-cases (mapcar (lambda (f) (generate-field-deserializer-case f class-name))
                              fields))
         ;; Reverse repeated fields at the end
         (repeated-fields (remove-if-not (lambda (f) (eq (proto-field-label f) :repeated))
                                         fields))
         (reverse-stmts (mapcar (lambda (f)
                                  (let ((slot-name (field-name-to-slot-name (proto-field-name f))))
                                    `(setf (slot-value obj ',slot-name)
                                           (nreverse (slot-value obj ',slot-name)))))
                                repeated-fields)))
    `(defmethod deserialize-from-bytes ((type (eql ',class-name)) data)
       (let ((obj (make-instance ',class-name))
             (buffer (cons data 0)))  ; (vector . position)
         (loop while (< (cdr buffer) (length data))
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

(defun generate-lisp-code (file-desc &key (package *package*) (generate-stubs t))
  "Generate Lisp code for all messages in a proto file descriptor.
Returns a list of forms to be compiled.
If GENERATE-STUBS is true (default), also generates client stubs for services."
  (let ((messages (proto-file-messages file-desc))
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
      (dolist (form forms)
        (eval form)))
    forms))

(defun compile-proto-string (string &key (load t) (package *package*))
  "Compile a proto definition from a string.
If LOAD is true, evaluates the generated code.
Returns the list of generated forms."
  (let* ((file-desc (parse-proto-string string))
         (forms (generate-lisp-code file-desc :package package)))
    (when load
      (dolist (form forms)
        (eval form)))
    forms))

;;;; ========================================================================
;;;; Client Stub Generation
;;;;
;;;; Generates typed client stubs for gRPC services.
;;;; Each service becomes a stub class with methods for each RPC.
;;;; ========================================================================

(defun lisp-name (name &optional suffix)
  "Convert a proto name to a Lisp symbol name.
   FooBar becomes FOO-BAR, with optional SUFFIX appended."
  (with-output-to-string (s)
    (loop for i from 0 below (length name)
          for char = (char name i)
          for prev-char = (if (> i 0) (char name (1- i)) nil)
          do (cond
               ;; Insert hyphen before uppercase if previous was lowercase
               ((and prev-char
                     (upper-case-p char)
                     (lower-case-p prev-char))
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
         (response-class (intern (string-upcase output-type) package))
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
              (error "ag-grpc package not loaded. Load ag-grpc before calling RPC methods."))
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
              (error "ag-grpc package not loaded. Load ag-grpc before calling RPC methods."))
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
                 (call-fn (and grpc-pkg (symbol-function (find-symbol "CALL-SERVER-STREAMING" grpc-pkg)))))
            (unless call-fn
              (error "ag-grpc package not loaded. Load ag-grpc before calling RPC methods."))
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
              (error "ag-grpc package not loaded. Load ag-grpc before calling RPC methods."))
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
    (nreverse forms)))
