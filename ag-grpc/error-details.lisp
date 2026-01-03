;;;; error-details.lisp - Rich error details (google.rpc.Status)
;;;;
;;;; Provides structured error information beyond status codes.
;;;; Based on google.rpc.Status and common error detail types.

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; google.rpc.Status Message
;;;; ========================================================================

(defclass rpc-status (ag-proto:proto-message)
  ((code :initarg :code :accessor rpc-status-code :initform 0
         :documentation "gRPC status code")
   (message :initarg :message :accessor rpc-status-message :initform ""
            :documentation "Developer-facing error message")
   (details :initarg :details :accessor rpc-status-details :initform nil
            :documentation "List of detail messages (Any protos)"))
  (:documentation "Rich error status from google.rpc.Status."))

(defmethod ag-proto:serialize-to-bytes ((obj rpc-status))
  (let ((buffer (make-array 128 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    ;; Field 1: code (int32)
    (let ((code (rpc-status-code obj)))
      (when (and code (not (zerop code)))
        (ag-proto::write-field-tag 1 0 buffer)
        (ag-proto::write-varint code buffer)))
    ;; Field 2: message (string)
    (let ((msg (rpc-status-message obj)))
      (when (and msg (plusp (length msg)))
        (ag-proto::write-field-tag 2 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 msg) buffer)))
    ;; Field 3: details (repeated Any)
    (dolist (detail (rpc-status-details obj))
      (ag-proto::write-field-tag 3 2 buffer)
      (ag-proto::write-length-delimited (ag-proto:serialize-to-bytes detail) buffer))
    buffer))

(defmethod ag-proto:deserialize-from-bytes ((type (eql 'rpc-status)) data)
  (let ((obj (make-instance 'rpc-status))
        (buffer (cons data 0)))
    (loop while (< (cdr buffer) (length data))
          do (let* ((tag (ag-proto::read-varint buffer))
                    (field-number (ash tag -3))
                    (wire-type (logand tag 7)))
               (case field-number
                 (1 (setf (rpc-status-code obj) (ag-proto::read-varint buffer)))
                 (2 (setf (rpc-status-message obj)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (3 (push (ag-proto::read-length-delimited buffer)
                          (rpc-status-details obj)))
                 (otherwise (ag-proto::skip-field buffer wire-type)))))
    (setf (rpc-status-details obj) (nreverse (rpc-status-details obj)))
    obj))

;;;; ========================================================================
;;;; google.protobuf.Any - For wrapping detail messages
;;;; ========================================================================

(defclass proto-any (ag-proto:proto-message)
  ((type-url :initarg :type-url :accessor any-type-url :initform ""
             :documentation "Type URL like 'type.googleapis.com/full.type.name'")
   (value :initarg :value :accessor any-value :initform #()
          :documentation "Serialized message bytes"))
  (:documentation "Protocol buffer Any type for wrapping arbitrary messages."))

(defmethod ag-proto:serialize-to-bytes ((obj proto-any))
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (let ((type-url (any-type-url obj)))
      (when (and type-url (plusp (length type-url)))
        (ag-proto::write-field-tag 1 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 type-url) buffer)))
    (let ((value (any-value obj)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 2 2 buffer)
        (ag-proto::write-length-delimited value buffer)))
    buffer))

(defmethod ag-proto:deserialize-from-bytes ((type (eql 'proto-any)) data)
  (let ((obj (make-instance 'proto-any))
        (buffer (cons data 0)))
    (loop while (< (cdr buffer) (length data))
          do (let* ((tag (ag-proto::read-varint buffer))
                    (field-number (ash tag -3))
                    (wire-type (logand tag 7)))
               (case field-number
                 (1 (setf (any-type-url obj)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (2 (setf (any-value obj) (ag-proto::read-length-delimited buffer)))
                 (otherwise (ag-proto::skip-field buffer wire-type)))))
    obj))

(defun make-any (type-name message)
  "Wrap a message in a proto-any with the given type name."
  (make-instance 'proto-any
                 :type-url (format nil "type.googleapis.com/~A" type-name)
                 :value (ag-proto:serialize-to-bytes message)))

;;;; ========================================================================
;;;; Common Error Detail Types
;;;; ========================================================================

;;; ErrorInfo - Describes the cause of an error
(defclass error-info (ag-proto:proto-message)
  ((reason :initarg :reason :accessor error-info-reason :initform ""
           :documentation "Error reason code")
   (domain :initarg :domain :accessor error-info-domain :initform ""
           :documentation "Error domain (e.g., 'example.com')")
   (metadata :initarg :metadata :accessor error-info-metadata :initform nil
             :documentation "Alist of additional metadata"))
  (:documentation "Describes the cause of the error."))

(defmethod ag-proto:serialize-to-bytes ((obj error-info))
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (let ((reason (error-info-reason obj)))
      (when (and reason (plusp (length reason)))
        (ag-proto::write-field-tag 1 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 reason) buffer)))
    (let ((domain (error-info-domain obj)))
      (when (and domain (plusp (length domain)))
        (ag-proto::write-field-tag 2 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 domain) buffer)))
    ;; Field 3: metadata map (simplified - just key-value pairs)
    (dolist (pair (error-info-metadata obj))
      ;; Map entry: key (field 1) and value (field 2)
      (let ((entry-buffer (make-array 32 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
        (ag-proto::write-field-tag 1 2 entry-buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 (car pair)) entry-buffer)
        (ag-proto::write-field-tag 2 2 entry-buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 (cdr pair)) entry-buffer)
        (ag-proto::write-field-tag 3 2 buffer)
        (ag-proto::write-length-delimited entry-buffer buffer)))
    buffer))

(defun make-error-info (reason domain &optional metadata)
  "Create an ErrorInfo detail."
  (make-instance 'error-info
                 :reason reason
                 :domain domain
                 :metadata metadata))

;;; RetryInfo - When to retry the request
(defclass retry-info (ag-proto:proto-message)
  ((retry-delay-seconds :initarg :retry-delay-seconds :accessor retry-info-delay-seconds :initform 0)
   (retry-delay-nanos :initarg :retry-delay-nanos :accessor retry-info-delay-nanos :initform 0))
  (:documentation "Describes when the client should retry."))

(defmethod ag-proto:serialize-to-bytes ((obj retry-info))
  (let ((buffer (make-array 32 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    ;; Duration message embedded in field 1
    (let ((secs (retry-info-delay-seconds obj))
          (nanos (retry-info-delay-nanos obj)))
      (when (or (plusp secs) (plusp nanos))
        (let ((duration-buffer (make-array 16 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
          (when (plusp secs)
            (ag-proto::write-field-tag 1 0 duration-buffer)
            (ag-proto::write-varint secs duration-buffer))
          (when (plusp nanos)
            (ag-proto::write-field-tag 2 0 duration-buffer)
            (ag-proto::write-varint nanos duration-buffer))
          (ag-proto::write-field-tag 1 2 buffer)
          (ag-proto::write-length-delimited duration-buffer buffer))))
    buffer))

(defun make-retry-info (delay-seconds &optional (delay-nanos 0))
  "Create a RetryInfo detail."
  (make-instance 'retry-info
                 :retry-delay-seconds delay-seconds
                 :retry-delay-nanos delay-nanos))

;;; DebugInfo - Debugging information
(defclass debug-info (ag-proto:proto-message)
  ((stack-entries :initarg :stack-entries :accessor debug-info-stack :initform nil
                  :documentation "Stack trace entries")
   (detail :initarg :detail :accessor debug-info-detail :initform ""
           :documentation "Debug detail message"))
  (:documentation "Debugging information for developers."))

(defmethod ag-proto:serialize-to-bytes ((obj debug-info))
  (let ((buffer (make-array 128 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (dolist (entry (debug-info-stack obj))
      (ag-proto::write-field-tag 1 2 buffer)
      (ag-proto::write-length-delimited (ag-proto::string-to-utf8 entry) buffer))
    (let ((detail (debug-info-detail obj)))
      (when (and detail (plusp (length detail)))
        (ag-proto::write-field-tag 2 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 detail) buffer)))
    buffer))

(defun make-debug-info (detail &optional stack-entries)
  "Create a DebugInfo detail."
  (make-instance 'debug-info
                 :detail detail
                 :stack-entries stack-entries))

;;;; ========================================================================
;;;; Enhanced Error Signaling
;;;; ========================================================================

(defun make-rich-status-error (code message &rest details)
  "Create a grpc-status-error with rich error details.
CODE: gRPC status code
MESSAGE: Error message
DETAILS: List of detail objects (error-info, retry-info, debug-info, etc.)

Example:
  (make-rich-status-error +grpc-status-invalid-argument+
    \"Invalid field value\"
    (make-error-info \"INVALID_FORMAT\" \"myapp.example.com\"
                     '((\"field\" . \"email\") (\"expected\" . \"valid email\"))))"
  (let* ((wrapped-details (mapcar (lambda (detail)
                                    (etypecase detail
                                      (error-info (make-any "google.rpc.ErrorInfo" detail))
                                      (retry-info (make-any "google.rpc.RetryInfo" detail))
                                      (debug-info (make-any "google.rpc.DebugInfo" detail))
                                      (proto-any detail)))
                                  details))
         (status (make-instance 'rpc-status
                                :code code
                                :message message
                                :details wrapped-details)))
    (make-condition 'grpc-status-error
                    :code code
                    :message message
                    :details (ag-proto:serialize-to-bytes status))))

(defun extract-status-details (error)
  "Extract rich status details from a grpc-status-error.
Returns the rpc-status object, or NIL if no details are present."
  (when (and (typep error 'grpc-status-error)
             (grpc-status-error-details error))
    (handler-case
        (ag-proto:deserialize-from-bytes 'rpc-status (grpc-status-error-details error))
      (error () nil))))
