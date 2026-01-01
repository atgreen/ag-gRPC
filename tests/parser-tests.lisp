;;;; parser-tests.lisp - Tests for Proto3 parser

(in-package #:ag-grpc-tests)

(in-suite parser-tests)

;;;; ========================================================================
;;;; Simple Proto Parsing Tests
;;;; ========================================================================

(test parse-empty-message
  "Parse a simple empty message"
  (let* ((proto "syntax = \"proto3\";
message Empty {
}")
         (file-desc (ag-proto:parse-proto-string proto)))
    (is (not (null file-desc)))
    (is (= 1 (length (ag-proto:proto-file-messages file-desc))))
    (let ((msg (first (ag-proto:proto-file-messages file-desc))))
      (is (string= "Empty" (ag-proto:proto-message-name msg))))))

(test parse-message-with-fields
  "Parse a message with scalar fields"
  (let* ((proto "syntax = \"proto3\";
message Person {
  string name = 1;
  int32 age = 2;
  bool active = 3;
}")
         (file-desc (ag-proto:parse-proto-string proto)))
    (is (not (null file-desc)))
    (let* ((msg (first (ag-proto:proto-file-messages file-desc)))
           (fields (ag-proto:proto-message-fields msg)))
      (is (= 3 (length fields)))
      (let ((name-field (find "name" fields :key #'ag-proto:proto-field-name :test #'string=)))
        (is (not (null name-field)))
        (is (eq :string (ag-proto:proto-field-type name-field)))
        (is (= 1 (ag-proto:proto-field-number name-field)))))))

(test parse-repeated-field
  "Parse a message with repeated field"
  (let* ((proto "syntax = \"proto3\";
message Numbers {
  repeated int32 values = 1;
}")
         (file-desc (ag-proto:parse-proto-string proto)))
    (is (not (null file-desc)))
    (let* ((msg (first (ag-proto:proto-file-messages file-desc)))
           (field (first (ag-proto:proto-message-fields msg))))
      (is (eq :repeated (ag-proto:proto-field-label field))))))

(test parse-enum
  "Parse an enum definition"
  (let* ((proto "syntax = \"proto3\";
enum Status {
  UNKNOWN = 0;
  ACTIVE = 1;
  INACTIVE = 2;
}")
         (file-desc (ag-proto:parse-proto-string proto)))
    (is (not (null file-desc)))
    (is (= 1 (length (ag-proto:proto-file-enums file-desc))))
    (let* ((enum (first (ag-proto:proto-file-enums file-desc)))
           (values (ag-proto:proto-enum-values enum)))
      (is (string= "Status" (ag-proto:proto-enum-name enum)))
      (is (= 3 (length values)))
      (let ((unknown (find "UNKNOWN" values :key #'ag-proto:proto-enum-value-name :test #'string=)))
        (is (not (null unknown)))
        (is (= 0 (ag-proto:proto-enum-value-number unknown)))))))

(test parse-package
  "Parse package declaration"
  (let* ((proto "syntax = \"proto3\";
package myapp.models;

message Test {}")
         (file-desc (ag-proto:parse-proto-string proto)))
    (is (string= "myapp.models" (ag-proto:proto-file-package file-desc)))))

(test parse-service
  "Parse a service definition"
  (let* ((proto "syntax = \"proto3\";
message Request {}
message Response {}

service Greeter {
  rpc SayHello (Request) returns (Response);
}")
         (file-desc (ag-proto:parse-proto-string proto)))
    (is (= 1 (length (ag-proto:proto-file-services file-desc))))
    (let* ((svc (first (ag-proto:proto-file-services file-desc)))
           (methods (ag-proto:proto-service-methods svc)))
      (is (string= "Greeter" (ag-proto:proto-service-name svc)))
      (is (= 1 (length methods)))
      (let ((method (first methods)))
        (is (string= "SayHello" (ag-proto:proto-method-name method)))
        (is (string= "Request" (ag-proto:proto-method-input-type method)))
        (is (string= "Response" (ag-proto:proto-method-output-type method)))))))

(test parse-streaming-rpc
  "Parse streaming RPC methods"
  (let* ((proto "syntax = \"proto3\";
message Msg {}

service Streamer {
  rpc ClientStream (stream Msg) returns (Msg);
  rpc ServerStream (Msg) returns (stream Msg);
  rpc BidiStream (stream Msg) returns (stream Msg);
}")
         (file-desc (ag-proto:parse-proto-string proto)))
    (let* ((svc (first (ag-proto:proto-file-services file-desc)))
           (methods (ag-proto:proto-service-methods svc)))
      (is (= 3 (length methods)))
      (let ((client-stream (find "ClientStream" methods
                                 :key #'ag-proto:proto-method-name :test #'string=))
            (server-stream (find "ServerStream" methods
                                 :key #'ag-proto:proto-method-name :test #'string=))
            (bidi-stream (find "BidiStream" methods
                               :key #'ag-proto:proto-method-name :test #'string=)))
        (is (ag-proto:proto-method-client-streaming client-stream))
        (is (not (ag-proto:proto-method-server-streaming client-stream)))
        (is (not (ag-proto:proto-method-client-streaming server-stream)))
        (is (ag-proto:proto-method-server-streaming server-stream))
        (is (ag-proto:proto-method-client-streaming bidi-stream))
        (is (ag-proto:proto-method-server-streaming bidi-stream))))))

(test parse-nested-message
  "Parse nested message definitions"
  (let* ((proto "syntax = \"proto3\";
message Outer {
  message Inner {
    string value = 1;
  }
  Inner nested = 1;
}")
         (file-desc (ag-proto:parse-proto-string proto)))
    (let* ((outer (first (ag-proto:proto-file-messages file-desc)))
           (nested (ag-proto:proto-message-nested-messages outer)))
      (is (= 1 (length nested)))
      (is (string= "Inner" (ag-proto:proto-message-name (first nested))))
      (is (string= "Outer.Inner" (ag-proto:proto-message-full-name (first nested)))))))

(test parse-comments
  "Parse proto with comments"
  (let* ((proto "syntax = \"proto3\";

// This is a line comment
message Test {
  /* This is a
     block comment */
  string value = 1;
}")
         (file-desc (ag-proto:parse-proto-string proto)))
    (is (not (null file-desc)))
    (is (= 1 (length (ag-proto:proto-file-messages file-desc))))))

(test parse-all-scalar-types
  "Parse message with all scalar types"
  (let* ((proto "syntax = \"proto3\";
message AllTypes {
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
         (file-desc (ag-proto:parse-proto-string proto)))
    (let* ((msg (first (ag-proto:proto-file-messages file-desc)))
           (fields (ag-proto:proto-message-fields msg)))
      (is (= 15 (length fields)))
      ;; Check a few types
      (is (eq :double (ag-proto:proto-field-type (find "d" fields :key #'ag-proto:proto-field-name :test #'string=))))
      (is (eq :string (ag-proto:proto-field-type (find "s" fields :key #'ag-proto:proto-field-name :test #'string=))))
      (is (eq :bytes (ag-proto:proto-field-type (find "by" fields :key #'ag-proto:proto-field-name :test #'string=)))))))
