;;;; metadata.lisp - gRPC metadata handling

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; gRPC Metadata
;;;;
;;;; Metadata is a set of key-value pairs that can be attached to RPC calls.
;;;; Keys are case-insensitive ASCII strings.
;;;; Values are ASCII strings or binary (keys ending in "-bin").
;;;;
;;;; Reference: https://grpc.io/docs/what-is-grpc/core-concepts/#metadata
;;;; ========================================================================

(defclass grpc-metadata ()
  ((entries :initform nil :accessor metadata-entries
            :documentation "Alist of (key . value) pairs"))
  (:documentation "gRPC metadata container"))

(defun make-grpc-metadata (&optional initial-entries)
  "Create a new metadata container with optional initial entries"
  (let ((md (make-instance 'grpc-metadata)))
    (dolist (entry initial-entries)
      (metadata-add md (car entry) (cdr entry)))
    md))

(defun metadata-get (metadata key)
  "Get the first value for a key, or NIL if not present"
  (cdr (assoc (string-downcase key) (metadata-entries metadata)
              :test #'string=)))

(defun metadata-get-all (metadata key)
  "Get all values for a key as a list"
  (loop for (k . v) in (metadata-entries metadata)
        when (string= (string-downcase key) k)
          collect v))

(defun metadata-set (metadata key value)
  "Set a key to a single value, replacing any existing values"
  (let ((normalized-key (string-downcase key)))
    (setf (metadata-entries metadata)
          (cons (cons normalized-key value)
                (remove normalized-key (metadata-entries metadata)
                        :key #'car :test #'string=)))))

(defun metadata-add (metadata key value)
  "Add a value for a key (allows multiple values per key)"
  (push (cons (string-downcase key) value)
        (metadata-entries metadata)))

(defun metadata-remove (metadata key)
  "Remove all values for a key"
  (setf (metadata-entries metadata)
        (remove (string-downcase key) (metadata-entries metadata)
                :key #'car :test #'string=)))

(defun metadata-clear (metadata)
  "Remove all entries"
  (setf (metadata-entries metadata) nil))

(defun metadata-keys (metadata)
  "Return a list of all unique keys"
  (remove-duplicates (mapcar #'car (metadata-entries metadata))
                     :test #'string=))

(defun metadata-count (metadata)
  "Return the number of entries"
  (length (metadata-entries metadata)))

(defun metadata-empty-p (metadata)
  "Return T if metadata has no entries"
  (null (metadata-entries metadata)))

(defun metadata-copy (metadata)
  "Create a copy of the metadata (for immutable-style updates)"
  (let ((copy (make-instance 'grpc-metadata)))
    (setf (metadata-entries copy)
          (copy-alist (metadata-entries metadata)))
    copy))

(defun metadata-merge (base &rest others)
  "Merge multiple metadata objects, later entries override earlier ones.
Returns a new metadata object."
  (let ((result (metadata-copy base)))
    (dolist (other others)
      (dolist (entry (metadata-entries other))
        (metadata-set result (car entry) (cdr entry))))
    result))

(defun metadata-to-alist (metadata)
  "Convert metadata to an alist"
  (copy-alist (metadata-entries metadata)))

(defun alist-to-metadata (alist)
  "Convert an alist to metadata"
  (make-grpc-metadata alist))

(defmethod print-object ((md grpc-metadata) stream)
  "Print metadata in a readable format"
  (print-unreadable-object (md stream :type t)
    (format stream "~D entries" (metadata-count md))))

;;;; ========================================================================
;;;; Binary Metadata
;;;; ========================================================================

(defun binary-metadata-key-p (key)
  "Return T if the key is a binary metadata key (ends with -bin)"
  (let ((key-str (string-downcase key)))
    (and (> (length key-str) 4)
         (string= (subseq key-str (- (length key-str) 4)) "-bin"))))

(defun encode-binary-metadata (bytes)
  "Encode binary data as base64 for metadata transport"
  (base64-encode bytes))

(defun decode-binary-metadata (string)
  "Decode base64-encoded binary metadata"
  (base64-decode string))

(defun decode-metadata-headers (headers)
  "Decode metadata from HTTP/2 headers, auto-decoding -bin keys.
Returns an alist with binary values decoded.
Skips pseudo-headers and standard gRPC headers."
  (when (and headers (listp headers))
    (loop for entry in headers
          when (and (consp entry)
                    (let ((key (car entry)))
                      ;; Keep only custom metadata headers
                      (not (or (and (keywordp key)
                                    (member key '(:status :method :scheme :path :authority)))
                               (and (stringp key)
                                    (member key '("content-type" "te" "user-agent" "grpc-encoding"
                                                  "grpc-accept-encoding" "grpc-timeout" "grpc-status"
                                                  "grpc-message")
                                            :test #'string-equal))))))
            collect (let ((key (car entry))
                          (value (cdr entry)))
                      (if (and (stringp key) (binary-metadata-key-p key))
                          (handler-case
                              (cons key (decode-binary-metadata value))
                            (error () (cons key value)))  ; Fall back to raw value on decode error
                          (cons key value))))))

;;; Simple base64 implementation for metadata encoding

(defparameter *base64-chars*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun base64-encode (bytes)
  "Encode bytes to base64 string"
  (let* ((len (length bytes))
         (result (make-array (ceiling (* len 4) 3)
                             :element-type 'character
                             :fill-pointer 0)))
    (loop for i from 0 below len by 3
          for b0 = (aref bytes i)
          for b1 = (if (< (1+ i) len) (aref bytes (1+ i)) 0)
          for b2 = (if (< (+ i 2) len) (aref bytes (+ i 2)) 0)
          for remaining = (- len i)
          do (vector-push-extend (char *base64-chars* (ash b0 -2)) result)
             (vector-push-extend (char *base64-chars*
                                       (logior (ash (logand b0 #x03) 4)
                                               (ash b1 -4)))
                                 result)
             (if (> remaining 1)
                 (vector-push-extend (char *base64-chars*
                                           (logior (ash (logand b1 #x0f) 2)
                                                   (ash b2 -6)))
                                     result)
                 (vector-push-extend #\= result))
             (if (> remaining 2)
                 (vector-push-extend (char *base64-chars* (logand b2 #x3f)) result)
                 (vector-push-extend #\= result)))
    (coerce result 'string)))

(defun base64-decode (string)
  "Decode base64 string to bytes"
  (let* ((len (length string))
         (padding (count #\= string))
         (result (make-array (- (/ (* len 3) 4) padding)
                             :element-type '(unsigned-byte 8)
                             :fill-pointer 0)))
    (loop for i from 0 below len by 4
          for c0 = (position (char string i) *base64-chars*)
          for c1 = (position (char string (+ i 1)) *base64-chars*)
          for c2-char = (char string (+ i 2))
          for c3-char = (char string (+ i 3))
          for c2 = (unless (char= c2-char #\=) (position c2-char *base64-chars*))
          for c3 = (unless (char= c3-char #\=) (position c3-char *base64-chars*))
          do (vector-push-extend (logior (ash c0 2) (ash c1 -4)) result)
             (when c2
               (vector-push-extend (logior (ash (logand c1 #x0f) 4)
                                           (ash c2 -2))
                                   result))
             (when c3
               (vector-push-extend (logior (ash (logand c2 #x03) 6) c3)
                                   result)))
    result))

;;;; ========================================================================
;;;; Standard gRPC Headers
;;;; ========================================================================

(defparameter *grpc-content-type* "application/grpc"
  "Standard gRPC content type")

(defparameter *grpc-user-agent* "ag-grpc/1.0.0"
  "User agent string for this library")

(defparameter *grpc-encoding* "identity"
  "Request encoding name")

(defparameter *grpc-accept-encoding* "identity,gzip"
  "Accepted response encoding(s)")

(defun make-request-headers (method &key timeout metadata authority tls)
  "Create standard gRPC request headers.
   If TLS is true, use https scheme."
  ;; Check if metadata overrides grpc-encoding
  (let* ((custom-encoding (and metadata
                               (metadata-get metadata "grpc-encoding")))
         (headers (list (cons :method "POST")
                       (cons :scheme (if tls "https" "http"))
                       (cons :path method)
                       (cons :authority (or authority ""))
                       (cons "content-type" *grpc-content-type*)
                       (cons "te" "trailers")
                       (cons "user-agent" *grpc-user-agent*)
                       (cons "grpc-encoding" (or custom-encoding *grpc-encoding*))
                       (cons "grpc-accept-encoding" *grpc-accept-encoding*))))
    ;; Pseudo-headers must appear before regular headers, so only append extras.
    (when timeout
      (setf headers (append headers
                            (list (cons "grpc-timeout" (format-grpc-timeout timeout))))))
    (when metadata
      (dolist (entry (metadata-entries metadata))
        (let ((key (car entry))
              (value (cdr entry)))
          ;; Skip grpc-encoding since we already handled it above
          (unless (string-equal key "grpc-encoding")
            ;; Binary metadata keys (ending in -bin) must be base64 encoded
            (if (binary-metadata-key-p key)
                (setf headers (append headers
                                      (list (cons key (encode-binary-metadata value)))))
                (setf headers (append headers (list entry))))))))
    headers))

(defun make-response-headers (&key metadata encoding)
  "Create standard gRPC response headers.
ENCODING - Optional compression encoding to advertise (e.g., \"gzip\")."
  (let ((headers (list (cons :status "200")
                       (cons "content-type" *grpc-content-type*))))
    ;; Add grpc-encoding header if using compression
    (when (and encoding (not (string-equal encoding "identity")))
      (push (cons "grpc-encoding" encoding) headers))
    (when metadata
      (dolist (entry (metadata-entries metadata))
        (let ((key (car entry))
              (value (cdr entry)))
          ;; Binary metadata keys (ending in -bin) must be base64 encoded
          (if (binary-metadata-key-p key)
              (push (cons key (encode-binary-metadata value)) headers)
              (push entry headers)))))
    headers))

(defun make-trailers (status &key message metadata)
  "Create gRPC trailers"
  (let ((trailers (list (cons "grpc-status" (write-to-string status)))))
    (when message
      (push (cons "grpc-message" (percent-encode message)) trailers))
    (when metadata
      (dolist (entry (metadata-entries metadata))
        (let ((key (car entry))
              (value (cdr entry)))
          ;; Binary metadata keys (ending in -bin) must be base64 encoded
          (if (binary-metadata-key-p key)
              (push (cons key (encode-binary-metadata value)) trailers)
              (push entry trailers)))))
    (nreverse trailers)))

;;;; ========================================================================
;;;; Timeout Handling
;;;; ========================================================================

(defun format-grpc-timeout (seconds)
  "Format a timeout value for the grpc-timeout header.
Timeout is specified in seconds."
  (cond
    ((< seconds 1)
     (format nil "~Dm" (round (* seconds 1000))))  ; milliseconds
    ((< seconds 60)
     (format nil "~DS" (round seconds)))           ; seconds
    ((< seconds 3600)
     (format nil "~DM" (round (/ seconds 60))))    ; minutes
    (t
     (format nil "~DH" (round (/ seconds 3600))))))  ; hours

(defun parse-grpc-timeout (timeout-str)
  "Parse a grpc-timeout header value. Returns timeout in seconds."
  (let* ((len (length timeout-str))
         (unit (char timeout-str (1- len)))
         (value (parse-integer (subseq timeout-str 0 (1- len)))))
    (case unit
      (#\n (* value 1e-9))   ; nanoseconds
      (#\u (* value 1e-6))   ; microseconds
      (#\m (* value 1e-3))   ; milliseconds
      (#\S value)            ; seconds
      (#\M (* value 60))     ; minutes
      (#\H (* value 3600))   ; hours
      (t value))))

;;;; ========================================================================
;;;; Percent Encoding (for grpc-message)
;;;; ========================================================================

(defun percent-encode (string)
  "Percent-encode a string for grpc-message header"
  (with-output-to-string (out)
    (loop for char across string
          for code = (char-code char)
          do (if (or (alphanumericp char)
                     (member char '(#\- #\_ #\. #\~)))
                 (write-char char out)
                 (format out "%~2,'0X" code)))))

(defun percent-decode (string)
  "Percent-decode a string from grpc-message header.
Properly handles UTF-8 encoded bytes."
  ;; First pass: collect bytes (percent-encoded bytes become raw bytes,
  ;; ASCII characters become their byte values)
  (let ((bytes (make-array (length string)
                           :element-type '(unsigned-byte 8)
                           :adjustable t
                           :fill-pointer 0)))
    (loop with i = 0
          with len = (length string)
          while (< i len)
          for char = (char string i)
          do (cond
               ((and (char= char #\%)
                     (<= (+ i 2) len))
                ;; Percent-encoded byte
                (vector-push-extend (parse-integer string
                                                   :start (1+ i)
                                                   :end (+ i 3)
                                                   :radix 16)
                                    bytes)
                (incf i 3))
               (t
                ;; Plain ASCII character
                (vector-push-extend (char-code char) bytes)
                (incf i))))
    ;; Convert UTF-8 bytes to string
    (trivial-utf-8:utf-8-bytes-to-string bytes)))
