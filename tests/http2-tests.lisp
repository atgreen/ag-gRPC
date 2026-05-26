;;;; http2-tests.lisp - Tests for HTTP/2 protocol

(in-package #:ag-grpc-tests)

(in-suite http2-tests)

(defun call-with-octet-stream-from-bytes (bytes fn)
  "Write BYTES to a temp file and call FN with a binary input stream
   opened on that file. The file is deleted after FN returns."
  (let ((path (uiop:tmpize-pathname
               (merge-pathnames "ag-grpc-read-frame-test"
                                (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
             (write-sequence bytes out))
           (with-open-file (in path :direction :input
                                    :element-type '(unsigned-byte 8))
             (funcall fn in)))
      (ignore-errors (delete-file path)))))

(test read-frame-signals-end-of-file-on-clean-eof
  "When the underlying stream is at EOF before any byte is read, READ-FRAME
   must signal END-OF-FILE so callers can unwind their frame loops instead
   of busy-looping on a dead socket."
  (call-with-octet-stream-from-bytes
   (make-array 0 :element-type '(unsigned-byte 8))
   (lambda (stream)
     (signals end-of-file (ag-http2::read-frame stream)))))

(test read-frame-signals-frame-error-on-truncated-header
  "A partial frame header (1-8 bytes) is a protocol violation, not EOF."
  (call-with-octet-stream-from-bytes
   (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)
   (lambda (stream)
     (signals ag-http2:http2-frame-error
       (ag-http2::read-frame stream)))))
