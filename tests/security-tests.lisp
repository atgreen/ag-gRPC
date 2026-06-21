;;;; security-tests.lisp - Security regression reproducers
;;;;
;;;; These are TRIAGE-CANDIDATE reproducers from an in-session security scan.
;;;; Each asserts the *correct/secure* behavior, so it is expected to FAIL on
;;;; the current (unfixed) tree and turn green once the corresponding fix lands.
;;;; They are intentionally wired into AG-GRPC-ALL-TESTS as failing tripwires.
;;;;
;;;;   MEDIUM-1  data-frame-buffering-is-bounded
;;;;             ag-http2/connection.lisp (DATA-frame handling),
;;;;             ag-http2/streams.lisp (unbounded stream-append-data)
;;;;   MEDIUM-2  read-frame-rejects-frame-exceeding-max-frame-size
;;;;             ag-http2/frames.lisp (read-frame allocates before size check)
;;;;   LOW-3     percent-decode-tolerates-malformed-server-escapes
;;;;             ag-grpc/metadata.lisp (percent-decode), reachable from a
;;;;             malicious server's grpc-message trailer (ag-grpc/call.lisp)

(in-package #:ag-grpc-tests)

(in-suite security-tests)

(defun call-with-octet-output-stream (fn)
  "Call FN with a binary output stream backed by a temp file (contents
   discarded). Mirrors CALL-WITH-OCTET-STREAM-FROM-BYTES for write paths so a
   frame handler that emits WINDOW_UPDATE/etc. has a real sink to write to."
  (let ((path (uiop:tmpize-pathname
               (merge-pathnames "ag-grpc-sec-out-test"
                                (uiop:temporary-directory)))))
    (unwind-protect
         (with-open-file (out path :direction :output
                                   :element-type '(unsigned-byte 8)
                                   :if-exists :supersede)
           (funcall fn out))
      (ignore-errors (delete-file path)))))

;;;; ------------------------------------------------------------------------
;;;; MEDIUM-1 — inbound flow control must apply backpressure
;;;; ------------------------------------------------------------------------

(test data-frame-buffering-is-bounded
  "Because the receiver replenishes its flow-control window as DATA is accepted,
   a peer that streams DATA without ever sending END_STREAM (or declares a huge
   gRPC message length and trickles the body) would grow the per-stream buffer
   (stream-append-data) without bound and exhaust server memory — there is no
   max-receive-message-size (MEDIUM-1).

   Secure behavior asserted: buffered-but-unconsumed request data per stream is
   capped (*max-receive-buffer-size*); once it is exceeded the connection is
   failed instead of buffering unboundedly. The cap is bound small here so the
   test stays cheap."
  (let ((conn (make-instance 'ag-http2::http2-connection :client-p nil))
        (ag-http2::*max-receive-buffer-size* 2048))
    (setf (ag-http2::connection-multiplexer conn)
          (ag-http2::make-stream-multiplexer :client-p nil))
    (call-with-octet-output-stream
     (lambda (out)
       (setf (ag-http2::connection-stream conn) out)
       (flet ((feed (n)
                (ag-http2::process-frame
                 conn (ag-http2::make-data-frame
                       1 (make-array n :element-type '(unsigned-byte 8)
                                       :initial-element 65)))))
         ;; Up to the cap is fine...
         (finishes (feed 1024))
         (finishes (feed 1024))
         ;; ...exceeding it without END_STREAM must be rejected, not buffered.
         (signals ag-http2::http2-connection-error (feed 1024)))))))

;;;; ------------------------------------------------------------------------
;;;; MEDIUM-2 — frames over SETTINGS_MAX_FRAME_SIZE must be rejected
;;;; ------------------------------------------------------------------------

(test read-frame-rejects-frame-exceeding-max-frame-size
  "RFC 7540 §4.2: an endpoint MUST treat a frame whose length exceeds the
   advertised SETTINGS_MAX_FRAME_SIZE (default 16384) as a connection error of
   type FRAME_SIZE_ERROR, and the check must happen BEFORE the payload is
   allocated. read-frame currently parses the 24-bit length and unconditionally
   allocates a buffer of that size (up to ~16 MB) before reading, so a 9-byte
   header forces a large allocation (MEDIUM-2).

   This header declares length 16385 (one byte over the RFC default) and sends
   no payload. Secure behavior asserted: read-frame signals an HTTP/2 error
   carrying FRAME_SIZE_ERROR. (Today it allocates the buffer, fails the read,
   and signals INTERNAL_ERROR via \"Incomplete frame payload\" instead.)"
  (let ((header (make-array 9 :element-type '(unsigned-byte 8)
                              :initial-contents
                              '(#x00 #x40 #x01   ; length = 16385 (> 16384)
                                #x00             ; type  = DATA
                                #x00             ; flags
                                #x00 #x00 #x00 #x01)))) ; stream id = 1
    (call-with-octet-stream-from-bytes
     header
     (lambda (stream)
       (handler-case
           (progn
             (ag-http2::read-frame stream)
             (is-true nil
                      "read-frame accepted a frame larger than MAX_FRAME_SIZE ~
                       instead of signalling FRAME_SIZE_ERROR (MEDIUM-2)."))
         (ag-http2::http2-error (e)
           (is (= (ag-http2::http2-error-code e)
                  ag-http2::+error-frame-size-error+)
               "oversize frame must signal FRAME_SIZE_ERROR, got ~A (MEDIUM-2)."
               (ag-http2::error-code-name (ag-http2::http2-error-code e)))))))))

;;;; ------------------------------------------------------------------------
;;;; LOW-3 — percent-decode must tolerate malformed peer-supplied escapes
;;;; ------------------------------------------------------------------------

(test percent-decode-tolerates-malformed-server-escapes
  "The grpc-message trailer is attacker-influenced (sent by the peer/server)
   and is percent-decoded on the receiving side (ag-grpc/call.lisp). A
   truncated escape (\"%\" + one hex digit) drives parse-integer past the end
   of the string, and a non-hex escape makes parse-integer signal — either way
   an uncaught error crashes the in-flight RPC's response handling (LOW-3).

   Secure behavior asserted: percent-decode never signals on malformed input;
   it should pass such bytes through (or drop them) rather than error."
  (finishes (ag-grpc::percent-decode "ok%2"))   ; truncated: only one hex digit
  (finishes (ag-grpc::percent-decode "ok%ZZ"))  ; non-hex escape
  (finishes (ag-grpc::percent-decode "trailing%"))) ; bare percent at end
