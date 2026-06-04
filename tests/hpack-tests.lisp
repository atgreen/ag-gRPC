;;;; hpack-tests.lisp - Tests for HPACK compression

(in-package #:ag-grpc-tests)

(in-suite hpack-tests)

;;; ---------------------------------------------------------------------------
;;; CVE-2026-49975 / HTTP/2 Bomb mitigations
;;; ---------------------------------------------------------------------------

(defun encode-indexed (index)
  "Build an HPACK indexed-header-field byte (1xxxxxxx) for small indices."
  (logior #x80 index))

(test hpack-decode-rejects-decoded-list-over-max
  "An attacker-controlled stream of 1-byte indexed references must not blow
   past the locally-advertised SETTINGS_MAX_HEADER_LIST_SIZE — the decoder
   accumulates name+value+32 per RFC 7541 §4.1 and aborts."
  ;; Static index 2 is (:method . \"GET\"), entry size = 32 + 7 + 3 = 42.
  ;; 4 entries = 168 bytes; a 100-byte cap must trip on the third entry.
  (let* ((decoder (ag-http2:make-hpack-decoder :max-header-list-size 100))
         (block (make-array 4
                            :element-type '(unsigned-byte 8)
                            :initial-contents (loop repeat 4
                                                    collect (encode-indexed 2)))))
    (signals ag-http2:http2-connection-error
      (ag-http2:hpack-decode decoder block))))

(test hpack-decode-allows-list-within-max
  "Same shape, but a single indexed reference under the cap must succeed."
  (let* ((decoder (ag-http2:make-hpack-decoder :max-header-list-size 100))
         (block (make-array 1
                            :element-type '(unsigned-byte 8)
                            :initial-contents (list (encode-indexed 2))))
         (headers (ag-http2:hpack-decode decoder block)))
    (is (= 1 (length headers)))
    (is (eq :method (first (first headers))))
    (is (string= "GET" (rest (first headers))))))

(test hpack-decode-rejects-table-size-update-over-advertised
  "Dynamic-table-size update exceeding SETTINGS_HEADER_TABLE_SIZE is a decoding
   error (RFC 7541 §6.3)."
  (let* ((decoder (ag-http2:make-hpack-decoder :table-size 4096))
         ;; 001xxxxx prefix with 5-bit value 31, then varint continuation
         ;; encoding 65505 + 31 = 65536 (well over the 4096 cap).
         ;; HPACK varint: value 65505, prefix 31 -> first byte #x3f, then
         ;; encode 65505 as multi-byte continuation.
         (block (make-array 4
                            :element-type '(unsigned-byte 8)
                            :initial-contents '(#x3f #xe1 #xff #x03))))
    (signals ag-http2:http2-connection-error
      (ag-http2:hpack-decode decoder block))))

(test hpack-decode-accepts-table-size-update-within-advertised
  "A size update at or below the advertised limit must succeed."
  (let* ((decoder (ag-http2:make-hpack-decoder :table-size 4096))
         ;; 001 prefix + 5-bit value 4 = size 4 (well within cap).
         (block (make-array 1
                            :element-type '(unsigned-byte 8)
                            :initial-contents '(#x24))))
    (finishes (ag-http2:hpack-decode decoder block))))

(test hpack-decode-nil-list-size-disables-check
  "Passing NIL for max-header-list-size opts out of the cap (escape hatch
   for trusted peers / tests)."
  (let* ((decoder (ag-http2:make-hpack-decoder :max-header-list-size nil))
         (block (make-array 50
                            :element-type '(unsigned-byte 8)
                            :initial-contents (loop repeat 50
                                                    collect (encode-indexed 2)))))
    (finishes (ag-http2:hpack-decode decoder block))))
