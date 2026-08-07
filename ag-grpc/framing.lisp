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
;;;; COMPRESSION SUPPORT:
;;;; This implementation supports gzip and deflate compression via chipz
;;;; (decompression) and salza2 (compression). The grpc-encoding header can
;;;; be set to "gzip", "deflate", or "identity" (no compression). Compressed
;;;; messages are automatically decompressed when the compressed flag is set.
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

(defun decode-grpc-message (bytes &optional (start 0) encoding)
  "Decode a gRPC message frame from bytes.
Returns (values message-data compressed-p bytes-consumed) or NIL if incomplete.
ENCODING is the compression algorithm (e.g., \"gzip\") for decompression."
  (let ((available (- (length bytes) start)))
    (when (< available +grpc-frame-header-size+)
      (return-from decode-grpc-message nil))
    (let* ((compressed-p (= (aref bytes start) 1))
           (length (logior (ash (aref bytes (1+ start)) 24)
                           (ash (aref bytes (+ start 2)) 16)
                           (ash (aref bytes (+ start 3)) 8)
                           (aref bytes (+ start 4))))
           (total-size (+ +grpc-frame-header-size+ length)))
      (when (< available total-size)
        (return-from decode-grpc-message nil))
      (let ((data (subseq bytes
                          (+ start +grpc-frame-header-size+)
                          (+ start total-size))))
        ;; Decompress if needed
        (when compressed-p
          (setf data (decompress-grpc-message data (or encoding "gzip"))))
        ;; ALWAYS create a fresh simple array to ensure type compatibility
        (let ((simple (make-array (length data) :element-type '(unsigned-byte 8))))
          (replace simple data)
          (values simple compressed-p total-size))))))

(defun decode-grpc-message-from-stream (stream &optional encoding)
  "Read and decode a gRPC message from a stream.
Returns (values message-data compressed-p) or NIL on EOF.
ENCODING is the compression algorithm (e.g., \"gzip\") for decompression."
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
      ;; Decompress if needed
      (when compressed-p
        (setf data (decompress-grpc-message data (or encoding "gzip"))))
      ;; ALWAYS create a fresh simple array to ensure type compatibility
      (let ((simple (make-array (length data) :element-type '(unsigned-byte 8))))
        (replace simple data)
        (values simple compressed-p)))))

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
;;; "grpc-encoding" header. We support gzip via chipz (decompression)
;;; and salza2 (compression).

(defun gzip-compress (data)
  "Compress DATA using gzip. Returns compressed byte vector."
  ;; Ensure input is a simple array for salza2
  (let* ((simple-data (if (typep data '(simple-array (unsigned-byte 8) (*)))
                          data
                          (let ((simple (make-array (length data) :element-type '(unsigned-byte 8))))
                            (replace simple data)
                            simple)))
         (output (make-array (length data)
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer 0)))
    (salza2:with-compressor (compressor 'salza2:gzip-compressor
                              :callback (lambda (buffer end)
                                          (loop for i from 0 below end
                                                do (vector-push-extend (aref buffer i) output))))
      (salza2:compress-octet-vector simple-data compressor))
    ;; Return as simple array
    (coerce output '(simple-array (unsigned-byte 8) (*)))))

(defvar *max-decompressed-size* (* 16 1024 1024)
  "Maximum allowed size in bytes for decompressed gRPC message data.
Prevents gzip bomb attacks where a small compressed payload expands to
exhaust memory.  Set to NIL to disable the check.  Default: 16 MB.")

(defun bounded-inflate (data format)
  "Decompress DATA (FORMAT is 'chipz:gzip or 'chipz:deflate) with peak output
allocation bounded by *MAX-DECOMPRESSED-SIZE*.

A decompression bomb (a small payload that expands ~1000:1) can no longer force
a multi-gigabyte transient allocation.  Instead of decompressing fully and
checking the length afterwards -- which must first materialize the entire
output in memory -- this drives chipz incrementally into a fixed CAP+1 buffer
and rejects the stream the instant its output would exceed the cap, so no more
than CAP+1 bytes are ever committed.  Signals GRPC-ERROR when the limit is
exceeded (or the stream is truncated/corrupt).  When *MAX-DECOMPRESSED-SIZE*
is NIL the bound is disabled (unbounded, single-shot decompress).  CL-SEC-2026-0212."
  (let* ((simple-data (if (typep data '(simple-array (unsigned-byte 8) (*)))
                          data
                          (let ((simple (make-array (length data)
                                                    :element-type '(unsigned-byte 8))))
                            (replace simple data)
                            simple)))
         (cap *max-decompressed-size*))
    (if (null cap)
        ;; Bound disabled: preserve the original single-shot behavior.
        (let ((result (chipz:decompress nil format simple-data)))
          (if (typep result '(simple-array (unsigned-byte 8) (*)))
              result
              (coerce result '(simple-array (unsigned-byte 8) (*)))))
        ;; CAP+1 output buffer: chipz stops at output-full without erroring,
        ;; returning (values consumed produced).  PRODUCED = CAP+1 means the
        ;; decompressed data is at least CAP+1 bytes -> reject (bomb) having
        ;; allocated only CAP+1 bytes.  If it stopped short of the cap but the
        ;; stream never finished, FINISH-DSTATE signals (truncated/corrupt).
        (let ((out (make-array (1+ cap) :element-type '(unsigned-byte 8)))
              (state (chipz:make-dstate format)))
          (multiple-value-bind (consumed produced)
              (chipz:decompress out state simple-data)
            (declare (ignore consumed))
            (when (> produced cap)
              (error 'grpc-error
                     :message (format nil "Decompressed size exceeds limit ~D bytes (gzip bomb defense)"
                                      cap)))
            (handler-case (chipz:finish-dstate state)
              (chipz:chipz-error ()
                (error 'grpc-error
                       :message (format nil "Decompressed stream is truncated or exceeds limit ~D bytes (gzip bomb defense)"
                                        cap))))
            (subseq out 0 produced))))))

(defun gzip-decompress (data)
  "Decompress gzip DATA. Returns decompressed byte vector.
Bounds peak output allocation to *MAX-DECOMPRESSED-SIZE* during decompression
(not post-hoc) to defend against gzip-bomb memory exhaustion.  CL-SEC-2026-0212."
  (bounded-inflate data 'chipz:gzip))

(defun decompress-grpc-message (data encoding)
  "Decompress a gRPC message body.
ENCODING is the compression algorithm name (e.g., \"gzip\", \"deflate\")."
  (cond
    ((or (null encoding) (string= encoding "identity"))
     data)
    ((string= encoding "gzip")
     (gzip-decompress data))
    ((string= encoding "deflate")
     (bounded-inflate data 'chipz:deflate))
    (t
     (error 'grpc-error
            :message (format nil "Unknown compression: ~A" encoding)))))

(defun compress-grpc-message (data encoding)
  "Compress a gRPC message body.
ENCODING is the compression algorithm name.
Returns (values compressed-data compressed-p)."
  (cond
    ((or (null encoding) (string= encoding "identity"))
     (values data nil))
    ((string= encoding "gzip")
     (values (gzip-compress data) t))
    ((string= encoding "deflate")
     ;; salza2 supports deflate via zlib-compressor
     (let ((output (make-array (length data)
                               :element-type '(unsigned-byte 8)
                               :adjustable t
                               :fill-pointer 0)))
       (salza2:with-compressor (compressor 'salza2:deflate-compressor
                                 :callback (lambda (buffer end)
                                             (loop for i from 0 below end
                                                   do (vector-push-extend (aref buffer i) output))))
         (salza2:compress-octet-vector data compressor))
       ;; Return as simple array
       (values (coerce output '(simple-array (unsigned-byte 8) (*))) t)))
    (t
     (error 'grpc-error
            :message (format nil "Unknown compression: ~A" encoding)))))
