;;;; grpc-tests.lisp - Tests for gRPC protocol

(in-package #:ag-grpc-tests)

(in-suite grpc-tests)

;;;; ------------------------------------------------------------------------
;;;; Compression framing (ag-grpc/framing.lisp)
;;;; ------------------------------------------------------------------------

(test bounded-inflate-grows-through-many-doublings
  "Issue #20. BOUNDED-INFLATE no longer allocates the full *MAX-DECOMPRESSED-SIZE* cap for
   every message; the output buffer starts near the compressed size and doubles
   as needed. A highly compressible message far larger than the initial buffer
   must still round-trip exactly, exercising the grow loop several times."
  (let* ((n (* 200 1024))               ; 200 KB -> initial buffer is 4 KB
         (payload (make-array n :element-type '(unsigned-byte 8)
                                :initial-element 0))
         (compressed (ag-grpc::gzip-compress payload)))
    ;; A small initial buffer relative to the output forces repeated growth.
    (is (< (length compressed) ag-grpc::*inflate-minimum-buffer*))
    (is (equalp payload (ag-grpc::gzip-decompress compressed)))))

(test bounded-inflate-round-trips-incompressible-data
  "Issue #20. A message that barely compresses must also round-trip byte-for-byte, so the
   grow/clamp arithmetic is exercised on output near the compressed size too."
  (let* ((n (* 64 1024))
         (payload (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref payload i) (ldb (byte 8 0) (* i 2654435761))))
    (is (equalp payload (ag-grpc::gzip-decompress (ag-grpc::gzip-compress payload))))))
