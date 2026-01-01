;;;; descriptor.lisp - CLOS classes representing proto descriptors

(in-package #:ag-proto)

;;;; ========================================================================
;;;; Proto Descriptor Classes
;;;;
;;;; These classes represent the structure of a .proto file after parsing.
;;;; They are used by the code generator to produce CLOS classes.
;;;; ========================================================================

(defclass proto-file-descriptor ()
  ((name :initarg :name :accessor proto-file-name
         :documentation "The .proto filename")
   (package :initarg :package :accessor proto-file-package
            :initform nil
            :documentation "The proto package name")
   (syntax :initarg :syntax :accessor proto-file-syntax
           :initform "proto3"
           :documentation "Syntax version (proto2 or proto3)")
   (imports :initarg :imports :accessor proto-file-imports
            :initform nil
            :documentation "List of imported .proto files")
   (options :initarg :options :accessor proto-file-options
            :initform nil
            :documentation "File-level options")
   (messages :initarg :messages :accessor proto-file-messages
             :initform nil
             :documentation "List of message descriptors")
   (enums :initarg :enums :accessor proto-file-enums
          :initform nil
          :documentation "List of enum descriptors")
   (services :initarg :services :accessor proto-file-services
             :initform nil
             :documentation "List of service descriptors"))
  (:documentation "Descriptor for a .proto file"))

(defclass proto-message-descriptor ()
  ((name :initarg :name :accessor proto-message-name
         :documentation "Message type name")
   (full-name :initarg :full-name :accessor proto-message-full-name
              :initform nil
              :documentation "Fully qualified name including package")
   (fields :initarg :fields :accessor proto-message-fields
           :initform nil
           :documentation "List of field descriptors")
   (nested-messages :initarg :nested-messages :accessor proto-message-nested-messages
                    :initform nil
                    :documentation "Nested message definitions")
   (nested-enums :initarg :nested-enums :accessor proto-message-nested-enums
                 :initform nil
                 :documentation "Nested enum definitions")
   (oneofs :initarg :oneofs :accessor proto-message-oneofs
           :initform nil
           :documentation "Oneof definitions")
   (options :initarg :options :accessor proto-message-options
            :initform nil
            :documentation "Message-level options"))
  (:documentation "Descriptor for a proto message type"))

(defclass proto-field-descriptor ()
  ((name :initarg :name :accessor proto-field-name
         :documentation "Field name")
   (number :initarg :number :accessor proto-field-number
           :documentation "Field number (tag)")
   (type :initarg :type :accessor proto-field-type
         :documentation "Field type (keyword or message name)")
   (label :initarg :label :accessor proto-field-label
          :initform :optional
          :documentation "Field label (:optional, :repeated, :required)")
   (type-name :initarg :type-name :accessor proto-field-type-name
              :initform nil
              :documentation "For message/enum types, the type name")
   (default-value :initarg :default-value :accessor proto-field-default-value
                  :initform nil
                  :documentation "Default value (proto2)")
   (oneof-index :initarg :oneof-index :accessor proto-field-oneof-index
                :initform nil
                :documentation "Index of containing oneof, if any")
   (options :initarg :options :accessor proto-field-options
            :initform nil
            :documentation "Field-level options")
   (map-key-type :initarg :map-key-type :accessor proto-field-map-key-type
                 :initform nil
                 :documentation "For map fields, the key type")
   (map-value-type :initarg :map-value-type :accessor proto-field-map-value-type
                   :initform nil
                   :documentation "For map fields, the value type"))
  (:documentation "Descriptor for a proto message field"))

(defclass proto-enum-descriptor ()
  ((name :initarg :name :accessor proto-enum-name
         :documentation "Enum type name")
   (full-name :initarg :full-name :accessor proto-enum-full-name
              :initform nil
              :documentation "Fully qualified name including package")
   (values :initarg :values :accessor proto-enum-values
           :initform nil
           :documentation "List of enum value descriptors")
   (options :initarg :options :accessor proto-enum-options
            :initform nil
            :documentation "Enum-level options"))
  (:documentation "Descriptor for a proto enum type"))

(defclass proto-enum-value-descriptor ()
  ((name :initarg :name :accessor proto-enum-value-name
         :documentation "Enum value name")
   (number :initarg :number :accessor proto-enum-value-number
           :documentation "Enum value number")
   (options :initarg :options :accessor proto-enum-value-options
            :initform nil
            :documentation "Value-level options"))
  (:documentation "Descriptor for a proto enum value"))

(defclass proto-service-descriptor ()
  ((name :initarg :name :accessor proto-service-name
         :documentation "Service name")
   (full-name :initarg :full-name :accessor proto-service-full-name
              :initform nil
              :documentation "Fully qualified name including package")
   (methods :initarg :methods :accessor proto-service-methods
            :initform nil
            :documentation "List of method descriptors")
   (options :initarg :options :accessor proto-service-options
            :initform nil
            :documentation "Service-level options"))
  (:documentation "Descriptor for a gRPC service"))

(defclass proto-method-descriptor ()
  ((name :initarg :name :accessor proto-method-name
         :documentation "Method name")
   (input-type :initarg :input-type :accessor proto-method-input-type
               :documentation "Input message type name")
   (output-type :initarg :output-type :accessor proto-method-output-type
                :documentation "Output message type name")
   (client-streaming :initarg :client-streaming :accessor proto-method-client-streaming
                     :initform nil
                     :documentation "True if client sends a stream")
   (server-streaming :initarg :server-streaming :accessor proto-method-server-streaming
                     :initform nil
                     :documentation "True if server sends a stream")
   (options :initarg :options :accessor proto-method-options
            :initform nil
            :documentation "Method-level options"))
  (:documentation "Descriptor for a gRPC service method"))

;;;; ========================================================================
;;;; Proto3 Scalar Types
;;;; ========================================================================

(defparameter *proto3-scalar-types*
  '(:double :float :int32 :int64 :uint32 :uint64
    :sint32 :sint64 :fixed32 :fixed64 :sfixed32 :sfixed64
    :bool :string :bytes)
  "List of proto3 scalar type keywords")

(defun scalar-type-p (type)
  "Return T if TYPE is a proto3 scalar type keyword"
  (member type *proto3-scalar-types*))

(defun proto-type-wire-type (type)
  "Return the wire type for a given proto type"
  (case type
    ((:int32 :int64 :uint32 :uint64 :sint32 :sint64 :bool :enum)
     +wire-type-varint+)
    ((:fixed64 :sfixed64 :double)
     +wire-type-fixed64+)
    ((:fixed32 :sfixed32 :float)
     +wire-type-fixed32+)
    ((:string :bytes :message)
     +wire-type-length-delimited+)
    (t
     ;; Default to length-delimited for unknown types (messages)
     +wire-type-length-delimited+)))

;;;; ========================================================================
;;;; Proto3 Default Values
;;;; ========================================================================

(defun proto3-default-value (type)
  "Return the proto3 default value for a type"
  (case type
    ((:double) 0.0d0)
    ((:float) 0.0f0)
    ((:int32 :int64 :uint32 :uint64 :sint32 :sint64
      :fixed32 :fixed64 :sfixed32 :sfixed64 :enum) 0)
    ((:bool) nil)
    ((:string) "")
    ((:bytes) #())
    (t nil)))  ; Messages default to nil (not present)
