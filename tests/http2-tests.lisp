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

(test apply-remote-settings-ignores-unknown-ids
  "RFC 7540 6.5.2: unknown SETTINGS identifiers must be ignored, not crash.
   nghttp2/Caddy send id 9 (ENABLE_CONNECT_PROTOCOL); a missing alist entry
   used to make (setf (rest (assoc ...)) ..) signal NIL-is-not-CONS."
  (let ((conn (make-instance 'ag-http2::http2-connection)))
    ;; Unknown id 9 must not error, and should be recorded.
    (finishes (ag-http2::apply-remote-settings conn '((9 . 1))))
    (is (eql 1 (cdr (assoc 9 (ag-http2::connection-remote-settings conn)))))
    ;; A known setting still applies (header-table-size touches the hpack
    ;; encoder, which is initialized by default).
    (finishes
      (ag-http2::apply-remote-settings
       conn (list (cons ag-http2::+settings-header-table-size+ 8192))))
    (is (eql 8192 (cdr (assoc ag-http2::+settings-header-table-size+
                              (ag-http2::connection-remote-settings conn)))))))

(test flow-control-lock-accepts-the-bt2-ops-the-send-path-uses
  "Regression for the BT2 lock type mismatch (issue #17). CONNECTION-SEND-DATA
   drives the flow-control lock with bt2:condition-wait and, on the zero-window
   fallback, bt2:release-lock / bt2:acquire-lock. If that lock ever reverts to a
   v1 (bt:make-lock) raw sb-thread:mutex, those bt2 ops signal \"BT2:LOCK is not
   of type SB-THREAD:MUTEX\" the instant HTTP/2 flow control engages during a
   large transfer. Assert the connection's real lock and CV accept every bt2 op
   the send path performs on them."
  (let* ((conn (make-instance 'ag-http2::http2-connection))
         (lock (ag-http2::connection-flow-control-lock conn))
         (cv (ag-http2::connection-flow-control-cv conn)))
    ;; The zero-window, no-reader-thread fallback: release then reacquire.
    (finishes
      (bt2:acquire-lock lock)
      (bt2:release-lock lock))
    ;; The zero-window wait. A short timeout returns without a type error.
    (finishes
      (bt2:with-lock-held (lock)
        (bt2:condition-wait cv lock :timeout 0.05)))))

(test discard-unsent-output-drops-buffered-bytes-so-close-cannot-resend-them
  "Regression for the interrupted-write resend (issue #22). A thread unwound by
   sb-thread:interrupt-thread between write(2) and SBCL's buffer reset leaves
   already-sent frame bytes in the fd-stream's output buffer; CONNECTION-CLOSE's
   GOAWAY FORCE-OUTPUT would then send them again (a repeated DATA+END_STREAM
   crashed grpc-go servers). CONNECTION-CLOSE now calls DISCARD-UNSENT-OUTPUT
   before writing GOAWAY. Stage a frame in a real fd-stream's buffer and show
   that discarding it keeps it off the wire, where a plain flush would send it."
  (uiop:with-temporary-file (:pathname path)
    (let ((staged (coerce #(0 0 0 0 1 0 0 0 1) ; DATA, END_STREAM, stream 1
                          '(simple-array (unsigned-byte 8) (*)))))
      (flet ((flushed-length ()
               (with-open-file (in path :element-type '(unsigned-byte 8))
                 (file-length in))))
        ;; Control: without the discard, a flush sends the staged frame.
        (with-open-file (out path :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
          (write-sequence staged out)
          (force-output out))
        (is (= (length staged) (flushed-length)))
        ;; With the discard, the staged bytes never reach the wire.
        (with-open-file (out path :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
          (let ((conn (make-instance 'ag-http2::http2-connection
                                     :stream out :socket nil :client-p t)))
            (write-sequence staged out)
            (ag-http2::discard-unsent-output conn)
            (force-output out)))
        (is (zerop (flushed-length)))))))
