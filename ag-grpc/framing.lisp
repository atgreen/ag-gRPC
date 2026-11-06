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

(defparameter *inflate-initial-ratio* 8
  "Start a decompression output buffer at this multiple of the compressed size.")

(defparameter *inflate-minimum-buffer* 4096
  "Smallest decompression output buffer, for a tiny compressed message.")

(defun bounded-inflate (data format)
  "Decompress DATA (FORMAT is 'chipz:gzip or 'chipz:deflate), holding output
allocation proportional to the real message, bounded by *MAX-DECOMPRESSED-SIZE*.

A decompression bomb (a small payload that expands ~1000:1) can no longer force
a multi-gigabyte transient allocation: output is produced incrementally and the
stream is rejected the instant its output would exceed the cap.  But neither
does an ordinary small message pay for the whole cap -- the previous version
allocated a fixed CAP+1 (16 MB default) buffer for every message, however tiny,
which on a busy stream churned gigabytes of garbage a minute.  The buffer now
starts near the expected output size (*INFLATE-INITIAL-RATIO* x the compressed
length, floored at *INFLATE-MINIMUM-BUFFER*) and doubles as chipz fills it,
clamped to CAP+1, so peak allocation is bounded by ~2x the cap for a bomb and by
the real size for an ordinary message.  Signals GRPC-ERROR when the limit is
exceeded (or the stream is truncated/corrupt).  When *MAX-DECOMPRESSED-SIZE*
is NIL the bound is disabled.  CL-SEC-2026-0212."
  (let* ((input (if (typep data '(simple-array (unsigned-byte 8) (*)))
                    data
                    (let ((simple (make-array (length data)
                                              :element-type '(unsigned-byte 8))))
                      (replace simple data)
                      simple)))
         (cap *max-decompressed-size*)
         (state (chipz:make-dstate format))
         (size (max *inflate-minimum-buffer*
                    (* *inflate-initial-ratio* (length input))))
         (output (make-array (if cap (min size (1+ cap)) size)
                             :element-type '(unsigned-byte 8)))
         (input-start 0)
         (produced 0))
    (loop
      (multiple-value-bind (consumed written)
          (chipz:decompress output state input
                            :input-start input-start :output-start produced)
        (incf input-start consumed)
        (incf produced written)
        ;; Over the cap having allocated at most CAP+1 -> reject (bomb).
        (when (and cap (> produced cap))
          (error 'grpc-error
                 :message (format nil "Decompressed size exceeds limit ~D bytes (gzip bomb defense)"
                                  cap)))
        ;; Stream complete -> done.  FINISH-DSTATE signals premature-end while
        ;; the stream is unfinished; that just means the buffer filled first.
        (when (handler-case (chipz:finish-dstate state)
                (chipz:premature-end-of-stream () nil))
          (return (subseq output 0 produced)))
        ;; Stopped with room left but the stream is not done -> the input ran
        ;; out mid-stream, so it is truncated or corrupt.
        (when (< produced (length output))
          (error 'grpc-error
                 :message "Decompressed stream is truncated or corrupt"))
        ;; Buffer full and more to come: grow (double, clamped to CAP+1).
        (let ((grown (make-array (if cap
                                     (min (* 2 (length output)) (1+ cap))
                                     (* 2 (length output)))
                                 :element-type '(unsigned-byte 8))))
          (replace grown output)
          (setf output grown))))))

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
