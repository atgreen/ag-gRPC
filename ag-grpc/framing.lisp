;;;; framing.lisp - gRPC message framing

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; gRPC Message Framing
;;;;
;;;; gRPC uses a simple framing format for messages:
;;;; - 1 byte: Compressed flag (0 = uncompressed, 1 = compressed)
;;;; - 4 bytes: Message length (big-endian)
;;;; - N bytes: Message data
;;;;
;;;; Reference: https://grpc.io/docs/what-is-grpc/core-concepts/#length-prefixed-message-framing
;;;; ========================================================================

(defconstant +grpc-frame-header-size+ 5
  "Size of gRPC message frame header in bytes")

;;;; ========================================================================
;;;; Encoding
;;;; ========================================================================

(defun encode-grpc-message (message &key (compressed nil))
  "Encode a protobuf message for gRPC transmission.
MESSAGE should be a byte vector (serialized protobuf).
Returns a byte vector with the gRPC frame header prepended."
  (let* ((data (if (typep message 'vector)
                   message
                   (ag-proto:serialize-to-bytes message)))
         (length (length data))
         (frame (make-array (+ +grpc-frame-header-size+ length)
                            :element-type '(unsigned-byte 8))))
    ;; Compressed flag
    (setf (aref frame 0) (if compressed 1 0))
    ;; Message length (big-endian)
    (setf (aref frame 1) (logand (ash length -24) #xff))
    (setf (aref frame 2) (logand (ash length -16) #xff))
    (setf (aref frame 3) (logand (ash length -8) #xff))
    (setf (aref frame 4) (logand length #xff))
    ;; Message data
    (replace frame data :start1 +grpc-frame-header-size+)
    frame))

(defun encode-grpc-message-to-stream (message stream &key (compressed nil))
  "Encode and write a gRPC message to a stream."
  (let ((frame (encode-grpc-message message :compressed compressed)))
    (write-sequence frame stream)
    (force-output stream)))

;;;; ========================================================================
;;;; Decoding
;;;; ========================================================================

(defun decode-grpc-message (bytes &optional (start 0))
  "Decode a gRPC message frame from bytes.
Returns (values message-data compressed-p bytes-consumed) or NIL if incomplete."
  (let ((available (- (length bytes) start)))
    (when (< available +grpc-frame-header-size+)
      (return-from decode-grpc-message nil))
    (let* ((compressed-p (= (aref bytes start) 1))
           (length (logior (ash (aref bytes (+ start 1)) 24)
                           (ash (aref bytes (+ start 2)) 16)
                           (ash (aref bytes (+ start 3)) 8)
                           (aref bytes (+ start 4))))
           (total-size (+ +grpc-frame-header-size+ length)))
      (when (< available total-size)
        (return-from decode-grpc-message nil))
      (let ((data (subseq bytes
                          (+ start +grpc-frame-header-size+)
                          (+ start total-size))))
        (values data compressed-p total-size)))))

(defun decode-grpc-message-from-stream (stream)
  "Read and decode a gRPC message from a stream.
Returns (values message-data compressed-p) or NIL on EOF."
  (let ((header (make-array +grpc-frame-header-size+
                            :element-type '(unsigned-byte 8))))
    (when (< (read-sequence header stream) +grpc-frame-header-size+)
      (return-from decode-grpc-message-from-stream nil))
    (let* ((compressed-p (= (aref header 0) 1))
           (length (logior (ash (aref header 1) 24)
                           (ash (aref header 2) 16)
                           (ash (aref header 3) 8)
                           (aref header 4)))
           (data (make-array length :element-type '(unsigned-byte 8))))
      (when (< (read-sequence data stream) length)
        (error 'grpc-error :message "Incomplete gRPC message"))
      (values data compressed-p))))

;;;; ========================================================================
;;;; Multi-Message Handling
;;;; ========================================================================

(defun decode-all-grpc-messages (bytes)
  "Decode all gRPC messages from a byte vector.
Returns a list of message data byte vectors."
  (loop with pos = 0
        with len = (length bytes)
        while (< pos len)
        collect (multiple-value-bind (data compressed consumed)
                    (decode-grpc-message bytes pos)
                  (declare (ignore compressed))
                  (unless data
                    (return))
                  (incf pos consumed)
                  data)))

;;;; ========================================================================
;;;; Compression
;;;; ========================================================================

;;; gRPC supports multiple compression algorithms, specified via the
;;; "grpc-encoding" header. For now, we only support uncompressed messages.

(defun decompress-grpc-message (data encoding)
  "Decompress a gRPC message body.
ENCODING is the compression algorithm name (e.g., \"gzip\", \"deflate\")."
  (cond
    ((or (null encoding) (string= encoding "identity"))
     data)
    ((string= encoding "gzip")
     ;; TODO: Implement gzip decompression
     (error 'grpc-error :message "gzip compression not yet implemented"))
    ((string= encoding "deflate")
     ;; TODO: Implement deflate decompression
     (error 'grpc-error :message "deflate compression not yet implemented"))
    (t
     (error 'grpc-error
            :message (format nil "Unknown compression: ~A" encoding)))))

(defun compress-grpc-message (data encoding)
  "Compress a gRPC message body.
ENCODING is the compression algorithm name."
  (cond
    ((or (null encoding) (string= encoding "identity"))
     (values data nil))
    ((string= encoding "gzip")
     ;; TODO: Implement gzip compression
     (error 'grpc-error :message "gzip compression not yet implemented"))
    ((string= encoding "deflate")
     ;; TODO: Implement deflate compression
     (error 'grpc-error :message "deflate compression not yet implemented"))
    (t
     (error 'grpc-error
            :message (format nil "Unknown compression: ~A" encoding)))))
