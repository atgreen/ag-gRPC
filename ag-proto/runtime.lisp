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

;;; Gray stream implementation for sequence I/O
;;; These classes implement the Gray stream protocol for full CL stream compatibility

(defclass sequence-input-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((data :initarg :data :reader stream-data)
   (position :initform 0 :accessor stream-position)
   (end :initarg :end :reader stream-end))
  (:documentation "Gray stream for reading from a byte sequence"))

(defclass sequence-output-stream (trivial-gray-streams:fundamental-binary-output-stream)
  ((data :initarg :data :accessor stream-data))
  (:documentation "Gray stream for writing to an adjustable byte sequence"))

(defun make-sequence-input-stream (sequence &key (start 0) (end nil))
  "Create an input stream reading from a sequence.
Works with standard CL stream operations (read-byte, read-sequence)."
  (make-instance 'sequence-input-stream
                 :data sequence
                 :end (or end (length sequence))))

(defun make-sequence-output-stream (&optional sequence)
  "Create an output stream writing to an adjustable sequence.
If SEQUENCE is nil, creates a new adjustable byte vector.
Works with standard CL stream operations (write-byte, write-sequence)."
  (make-instance 'sequence-output-stream
                 :data (or sequence
                           (make-array 64 :element-type '(unsigned-byte 8)
                                          :adjustable t :fill-pointer 0))))

;;; Gray stream methods for sequence-input-stream

(defmethod trivial-gray-streams:stream-read-byte ((stream sequence-input-stream))
  "Read a byte from a sequence input stream"
  (let ((pos (stream-position stream)))
    (if (>= pos (stream-end stream))
        :eof
        (prog1 (aref (stream-data stream) pos)
          (incf (stream-position stream))))))

(defmethod trivial-gray-streams:stream-read-sequence ((stream sequence-input-stream)
                                                       sequence start end
                                                       &key &allow-other-keys)
  "Read bytes from sequence input stream into SEQUENCE"
  (let* ((src (stream-data stream))
         (src-pos (stream-position stream))
         (src-end (stream-end stream))
         (available (- src-end src-pos))
         (requested (- end start))
         (to-read (min available requested)))
    (when (plusp to-read)
      (replace sequence src
               :start1 start :end1 (+ start to-read)
               :start2 src-pos :end2 (+ src-pos to-read))
      (incf (stream-position stream) to-read))
    (+ start to-read)))

(defmethod trivial-gray-streams:stream-listen ((stream sequence-input-stream))
  "Check if data is available"
  (< (stream-position stream) (stream-end stream)))

(defmethod stream-element-type ((stream sequence-input-stream))
  '(unsigned-byte 8))

;;; Gray stream methods for sequence-output-stream

(defmethod trivial-gray-streams:stream-write-byte ((stream sequence-output-stream) byte)
  "Write a byte to a sequence output stream"
  (vector-push-extend byte (stream-data stream))
  byte)

(defmethod trivial-gray-streams:stream-write-sequence ((stream sequence-output-stream)
                                                        sequence start end
                                                        &key &allow-other-keys)
  "Write bytes from SEQUENCE to the output stream"
  (loop for i from start below end
        do (vector-push-extend (aref sequence i) (stream-data stream)))
  sequence)

(defmethod stream-element-type ((stream sequence-output-stream))
  '(unsigned-byte 8))

;;; Stream utility functions

(defun sequence-stream-contents (stream)
  "Get the byte vector from a sequence output stream"
  (stream-data stream))

(defun sequence-stream-position (stream)
  "Get the current position in a sequence input stream"
  (stream-position stream))

(defun sequence-stream-remaining (stream)
  "Get remaining bytes in a sequence input stream"
  (- (stream-end stream) (stream-position stream)))

;;; Legacy compatibility (deprecated, use standard stream operations)

(defun read-byte-from-seq-stream (stream)
  "Read a byte from a sequence stream, return NIL on EOF.
DEPRECATED: Use (read-byte stream nil nil) instead."
  (let ((byte (read-byte stream nil :eof)))
    (if (eq byte :eof) nil byte)))

(defun write-byte-to-seq-stream (byte stream)
  "Write a byte to a sequence stream.
DEPRECATED: Use (write-byte byte stream) instead."
  (write-byte byte stream))

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
