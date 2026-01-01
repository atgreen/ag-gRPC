;;;; runtime.lisp - Runtime support for proto messages

(in-package #:ag-proto)

;;;; ========================================================================
;;;; Conditions
;;;; ========================================================================

(define-condition proto-error (error)
  ((message :initarg :message :reader proto-error-message))
  (:report (lambda (condition stream)
             (format stream "Proto error: ~A" (proto-error-message condition)))))

(define-condition proto-parse-error (proto-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Proto parse error: ~A" (proto-error-message condition)))))

;;;; ========================================================================
;;;; Base Message Class
;;;; ========================================================================

(defclass proto-message ()
  ()
  (:documentation "Base class for all proto messages"))

;;;; ========================================================================
;;;; Serialization Interface
;;;; ========================================================================

(defgeneric serialize-to-stream (message stream)
  (:documentation "Serialize a proto message to a binary stream"))

(defgeneric deserialize-from-stream (type stream)
  (:documentation "Deserialize a proto message from a binary stream"))

(defgeneric serialize-to-bytes (message)
  (:documentation "Serialize a proto message to a byte vector"))

(defgeneric deserialize-from-bytes (type data)
  (:documentation "Deserialize a proto message from a byte vector"))

;;;; ========================================================================
;;;; Stream Utilities
;;;; ========================================================================

;;; These macros provide portable sequence streams for serialization

(defmacro with-output-to-sequence ((var sequence &key (element-type ''(unsigned-byte 8))) &body body)
  "Execute BODY with VAR bound to a stream that writes to SEQUENCE.
SEQUENCE should be an adjustable vector with a fill-pointer."
  (let ((seq-var (gensym "SEQ")))
    `(let ((,seq-var ,sequence))
       (let ((,var (make-sequence-output-stream ,seq-var)))
         ,@body))))

(defmacro with-input-from-sequence ((var sequence &key (element-type ''(unsigned-byte 8))) &body body)
  "Execute BODY with VAR bound to a stream that reads from SEQUENCE."
  (declare (ignore element-type))
  `(let ((,var (make-sequence-input-stream ,sequence)))
     ,@body))

;;; Simple sequence streams (portable implementation)

(defclass sequence-input-stream ()
  ((data :initarg :data :reader stream-data)
   (position :initform 0 :accessor stream-position)))

(defclass sequence-output-stream ()
  ((data :initarg :data :accessor stream-data)))

(defun make-sequence-input-stream (sequence)
  "Create an input stream reading from a sequence"
  (make-instance 'sequence-input-stream :data sequence))

(defun make-sequence-output-stream (sequence)
  "Create an output stream writing to an adjustable sequence"
  (make-instance 'sequence-output-stream :data sequence))

(defmethod stream-read-byte ((stream sequence-input-stream))
  "Read a byte from a sequence input stream"
  (let ((data (stream-data stream))
        (pos (stream-position stream)))
    (if (>= pos (length data))
        nil
        (prog1 (aref data pos)
          (incf (stream-position stream))))))

(defmethod stream-write-byte ((stream sequence-output-stream) byte)
  "Write a byte to a sequence output stream"
  (vector-push-extend byte (stream-data stream)))

;;; Gray stream integration would go here for full stream compatibility
;;; For now, use these simple stream classes with explicit method calls

(defun read-byte-from-seq-stream (stream)
  "Read a byte from a sequence stream, return NIL on EOF"
  (stream-read-byte stream))

(defun write-byte-to-seq-stream (byte stream)
  "Write a byte to a sequence stream"
  (stream-write-byte stream byte))

;;;; ========================================================================
;;;; Message Utilities
;;;; ========================================================================

(defgeneric proto-message-p (object)
  (:documentation "Return T if object is a proto message")
  (:method (object) nil)
  (:method ((object proto-message)) t))

(defgeneric proto-message-type-name (message)
  (:documentation "Return the proto type name for a message")
  (:method ((message proto-message))
    (class-name (class-of message))))

(defgeneric proto-clear (message)
  (:documentation "Reset all fields in a message to their default values")
  (:method ((message proto-message))
    ;; Default implementation does nothing
    ;; Generated classes should override this
    message))

(defgeneric proto-merge (target source)
  (:documentation "Merge fields from source message into target")
  (:method ((target proto-message) (source proto-message))
    ;; Default implementation does nothing
    ;; Generated classes should override this
    target))
