;;;; codegen-tests.lisp - Tests for proto code generation

(in-package #:ag-grpc-tests)

(in-suite codegen-tests)

;;;; ========================================================================
;;;; Code Generation Tests
;;;; ========================================================================

(test codegen-simple-message
  "Generate and use a simple message class"
  (let ((forms (ag-proto:compile-proto-string "
syntax = \"proto3\";
message Person {
  string name = 1;
  int32 age = 2;
  bool active = 3;
}")))
    ;; Check that forms were generated
    (is (= 3 (length forms)))
    ;; Create an instance
    (let ((person (make-instance 'person :name "Alice" :age 30 :active t)))
      (is (string= "Alice" (name person)))
      (is (= 30 (age person)))
      (is (eq t (active person))))))

(test codegen-serialize-roundtrip
  "Serialize and deserialize a message"
  (ag-proto:compile-proto-string "
syntax = \"proto3\";
message TestMsg {
  string text = 1;
  int32 num = 2;
}")
  (let* ((original (make-instance 'testmsg :text "Hello" :num 42))
         (bytes (ag-proto:serialize-to-bytes original))
         (restored (ag-proto:deserialize-from-bytes 'testmsg bytes)))
    (is (string= "Hello" (text restored)))
    (is (= 42 (num restored)))))

(test codegen-all-scalar-types
  "Generate and roundtrip message with all scalar types"
  (ag-proto:compile-proto-string "
syntax = \"proto3\";
message AllScalars {
  double d = 1;
  float f = 2;
  int32 i32 = 3;
  int64 i64 = 4;
  uint32 u32 = 5;
  uint64 u64 = 6;
  sint32 s32 = 7;
  sint64 s64 = 8;
  fixed32 f32 = 9;
  fixed64 f64 = 10;
  sfixed32 sf32 = 11;
  sfixed64 sf64 = 12;
  bool b = 13;
  string s = 14;
  bytes by = 15;
}")
  (let* ((original (make-instance 'allscalars
                                  :d 3.14159d0
                                  :f 2.718f0
                                  :i32 -100
                                  :i64 -1000000000000
                                  :u32 100
                                  :u64 1000000000000
                                  :s32 -50
                                  :s64 -500000000000
                                  :f32 12345
                                  :f64 123456789012
                                  :sf32 -12345
                                  :sf64 -123456789012
                                  :b t
                                  :s "test string"
                                  :by #(1 2 3 4 5)))
         (bytes (ag-proto:serialize-to-bytes original))
         (restored (ag-proto:deserialize-from-bytes 'allscalars bytes)))
    ;; Check floats with tolerance
    (is (< (abs (- (d restored) 3.14159d0)) 1d-10))
    (is (< (abs (- (f restored) 2.718f0)) 1f-5))
    ;; Check integers
    (is (= -100 (i32 restored)))
    (is (= -1000000000000 (i64 restored)))
    (is (= 100 (u32 restored)))
    (is (= 1000000000000 (u64 restored)))
    (is (= -50 (s32 restored)))
    (is (= -500000000000 (s64 restored)))
    (is (= 12345 (f32 restored)))
    (is (= 123456789012 (f64 restored)))
    (is (= -12345 (sf32 restored)))
    (is (= -123456789012 (sf64 restored)))
    ;; Check bool, string, bytes
    (is (eq t (b restored)))
    (is (string= "test string" (s restored)))
    (is (equalp #(1 2 3 4 5) (by restored)))))

(test codegen-repeated-field
  "Generate and roundtrip message with repeated field"
  (ag-proto:compile-proto-string "
syntax = \"proto3\";
message Numbers {
  repeated int32 nums = 1;
}")
  (let* ((original (make-instance 'numbers :nums '(1 2 3 4 5)))
         (bytes (ag-proto:serialize-to-bytes original))
         (restored (ag-proto:deserialize-from-bytes 'numbers bytes)))
    (is (equal '(1 2 3 4 5) (nums restored)))))

(test codegen-enum
  "Generate enum constants"
  (ag-proto:compile-proto-string "
syntax = \"proto3\";
enum Status {
  UNKNOWN = 0;
  ACTIVE = 1;
  INACTIVE = 2;
}")
  (is (= 0 +status-unknown+))
  (is (= 1 +status-active+))
  (is (= 2 +status-inactive+)))

(test codegen-default-values
  "Proto3 default values"
  (ag-proto:compile-proto-string "
syntax = \"proto3\";
message Defaults {
  string s = 1;
  int32 n = 2;
  bool b = 3;
}")
  (let ((msg (make-instance 'defaults)))
    (is (string= "" (s msg)))
    (is (= 0 (n msg)))
    (is (null (b msg)))))

(test codegen-skip-default
  "Don't serialize default values"
  (ag-proto:compile-proto-string "
syntax = \"proto3\";
message Sparse {
  int32 a = 1;
  int32 b = 2;
  int32 c = 3;
}")
  ;; Only set 'b', leave 'a' and 'c' as defaults
  (let* ((original (make-instance 'sparse :b 42))
         (bytes (ag-proto:serialize-to-bytes original)))
    ;; Should be small - only one field encoded
    ;; Field 2, wire type 0 (varint) = tag 16 (0x10)
    ;; Value 42 = 0x2a
    (is (<= (length bytes) 3))
    (let ((restored (ag-proto:deserialize-from-bytes 'sparse bytes)))
      (is (= 0 (a restored)))
      (is (= 42 (b restored)))
      (is (= 0 (c restored))))))

(test codegen-unknown-fields
  "Skip unknown fields during deserialization"
  (ag-proto:compile-proto-string "
syntax = \"proto3\";
message Small {
  int32 a = 1;
}")
  ;; Create bytes with an extra field that Small doesn't know about
  (let* ((known-msg (make-instance 'small :a 100))
         (bytes (ag-proto:serialize-to-bytes known-msg))
         ;; Manually add an unknown field (field 99, varint 12345)
         (extra-bytes (make-array (+ (length bytes) 3)
                                  :element-type '(unsigned-byte 8)))
         (pos 0))
    ;; Copy original bytes
    (loop for b across bytes
          do (setf (aref extra-bytes pos) b)
             (incf pos))
    ;; Add unknown field: tag = 99 << 3 | 0 = 792 = varint bytes #xb8 #x06
    (setf (aref extra-bytes pos) #xb8)
    (incf pos)
    (setf (aref extra-bytes pos) #x06)
    (incf pos)
    ;; Value = 42
    (setf (aref extra-bytes pos) 42)
    ;; Should deserialize without error, ignoring unknown field
    (let ((restored (ag-proto:deserialize-from-bytes 'small extra-bytes)))
      (is (= 100 (a restored))))))
