;;;; client.lisp - Conformance test client for ag-gRPC

;; Redirect all output to stderr during loading to avoid polluting
;; the length-prefixed protocol on stdout
(let ((*standard-output* *error-output*)
      (*trace-output* *error-output*))
  (require 'asdf)
  (asdf:initialize-source-registry
   `(:source-registry
     :inherit-configuration
     (:directory ,(truename "/home/green/git/cl-gRPC"))
     (:tree ,(merge-pathnames "ocicl/" (truename "/home/green/git/cl-gRPC")))))

  (asdf:load-system :ag-grpc)

  ;; Load generated proto code
  (load (merge-pathnames "conformance/lisp/package.lisp" (truename "/home/green/git/cl-gRPC/")))
  (load (merge-pathnames "conformance/lisp/struct.lisp" (truename "/home/green/git/cl-gRPC/")))
  (load (merge-pathnames "conformance/lisp/empty.lisp" (truename "/home/green/git/cl-gRPC/")))
  (load (merge-pathnames "conformance/lisp/any.lisp" (truename "/home/green/git/cl-gRPC/")))
  (load (merge-pathnames "conformance/lisp/config.lisp" (truename "/home/green/git/cl-gRPC/")))
  (load (merge-pathnames "conformance/lisp/service.lisp" (truename "/home/green/git/cl-gRPC/")))
  (load (merge-pathnames "conformance/lisp/client_compat.lisp" (truename "/home/green/git/cl-gRPC/"))))

(defpackage #:conformance-client
  (:use #:cl)
  (:import-from #:conformance-proto
                ;; Message classes
                #:client-compat-request
                #:client-compat-response
                #:client-response-result
                #:client-error-result
                #:proto-error
                #:wire-details
                #:header
                #:conformance-payload
                #:unary-request
                #:unary-response
                ;; Google protobuf types
                #:any))

(in-package #:conformance-client)

;;; Base64 decoding for grpc-status-details-bin

(defparameter *base64-decode-table*
  (let ((table (make-array 256 :element-type '(signed-byte 8) :initial-element -1)))
    (loop for char across "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
          for i from 0
          do (setf (aref table (char-code char)) i))
    ;; Also handle URL-safe variants
    (setf (aref table (char-code #\-)) 62)  ; - instead of +
    (setf (aref table (char-code #\_)) 63)  ; _ instead of /
    table))

(defun base64-decode (string)
  "Decode a base64 string to a byte vector."
  (let* ((len (length string))
         ;; Remove padding and calculate output size
         (padding (cond ((and (> len 0) (char= (char string (1- len)) #\=))
                         (if (and (> len 1) (char= (char string (- len 2)) #\=)) 2 1))
                        (t 0)))
         (input-len (- len padding))
         (output-len (floor (* input-len 3) 4))
         (output (make-array output-len :element-type '(unsigned-byte 8)))
         (out-pos 0)
         (buffer 0)
         (bits 0))
    (loop for i from 0 below input-len
          for char = (char string i)
          for val = (aref *base64-decode-table* (char-code char))
          when (>= val 0)
            do (setf buffer (logior (ash buffer 6) val))
               (incf bits 6)
               (when (>= bits 8)
                 (decf bits 8)
                 (when (< out-pos output-len)
                   (setf (aref output out-pos) (logand (ash buffer (- bits)) #xff))
                   (incf out-pos))))
    output))

;;; Parsing grpc-status-details-bin (google.rpc.Status message)

(defun parse-grpc-status-details (data)
  "Parse a google.rpc.Status message and return the details as a list of Any objects.
google.rpc.Status: int32 code (1), string message (2), repeated Any details (3)"
  (let ((pos 0)
        (len (length data))
        (details nil))
    (labels ((read-varint ()
               (let ((result 0)
                     (shift 0))
                 (loop
                   (when (>= pos len) (return result))
                   (let ((byte (aref data pos)))
                     (incf pos)
                     (setf result (logior result (ash (logand byte #x7f) shift)))
                     (incf shift 7)
                     (when (zerop (logand byte #x80))
                       (return result))))))
             (read-length-delimited ()
               (let* ((length (read-varint))
                      (end (+ pos length)))
                 (when (> end len) (return-from read-length-delimited nil))
                 (let ((result (subseq data pos end)))
                   (setf pos end)
                   result)))
             (skip-field (wire-type)
               (case wire-type
                 (0 (read-varint))  ; varint
                 (1 (incf pos 8))   ; 64-bit
                 (2 (read-length-delimited))  ; length-delimited
                 (5 (incf pos 4))))) ; 32-bit
      (loop while (< pos len)
            do (let* ((tag (read-varint))
                      (field-number (ash tag -3))
                      (wire-type (logand tag 7)))
                 (case field-number
                   (1 (read-varint))  ; code - skip
                   (2 (read-length-delimited))  ; message - skip
                   (3 ;; details - parse as Any
                    (let ((any-bytes (read-length-delimited)))
                      (when any-bytes
                        (handler-case
                            (push (ag-proto:deserialize-from-bytes 'any any-bytes)
                                  details)
                          (error () nil)))))
                   (otherwise
                    (skip-field wire-type)))))
      (nreverse details))))

(defun extract-error-details (trailers)
  "Extract error details from grpc-status-details-bin header."
  (let ((details-header (assoc "grpc-status-details-bin" trailers :test #'string-equal)))
    (when details-header
      (handler-case
          (let* ((base64-data (cdr details-header))
                 (decoded (base64-decode base64-data)))
            (parse-grpc-status-details decoded))
        (error (e)
          (log-msg "Error parsing grpc-status-details-bin: ~A" e)
          nil)))))

;;; Logging (defined early for use throughout)

(defvar *log-stream* nil)

(defun log-msg (fmt &rest args)
  ;; Always log to stderr for debugging
  (apply #'format *error-output* fmt args)
  (terpri *error-output*)
  (force-output *error-output*)
  ;; Also log to file if available
  (when *log-stream*
    (apply #'format *log-stream* fmt args)
    (terpri *log-stream*)
    (finish-output *log-stream*)))

;;; Length-prefixed I/O (big-endian 32-bit length prefix)

(defun read-length-prefix (stream)
  "Read a big-endian 32-bit length prefix from stream."
  (let ((bytes (make-array 4 :element-type '(unsigned-byte 8))))
    (let ((n (read-sequence bytes stream)))
      (log-msg "read-length-prefix: read ~A bytes: ~{~2,'0X ~}" n (coerce bytes 'list))
      (unless (= n 4)
        (return-from read-length-prefix nil)))
    (let ((len (logior (ash (aref bytes 0) 24)
                       (ash (aref bytes 1) 16)
                       (ash (aref bytes 2) 8)
                       (aref bytes 3))))
      (log-msg "read-length-prefix: length = ~A" len)
      len)))

(defun write-length-prefix (stream length)
  "Write a big-endian 32-bit length prefix to stream."
  (let ((bytes (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref bytes 0) (ldb (byte 8 24) length))
    (setf (aref bytes 1) (ldb (byte 8 16) length))
    (setf (aref bytes 2) (ldb (byte 8 8) length))
    (setf (aref bytes 3) (ldb (byte 8 0) length))
    (write-sequence bytes stream)))

(defun read-request (stream)
  "Read a ClientCompatRequest from the stream."
  (let ((len (read-length-prefix stream)))
    (when (null len)
      (return-from read-request nil))
    (let ((data (make-array len :element-type '(unsigned-byte 8))))
      (let ((n (read-sequence data stream)))
        (unless (= n len)
          (cl:error "Short read: expected ~D bytes, got ~D" len n)))
      (ag-proto:deserialize-from-bytes 'client-compat-request data))))

(defun write-response (stream response)
  "Write a ClientCompatResponse to the stream."
  (let ((data (ag-proto:serialize-to-bytes response)))
    (write-length-prefix stream (length data))
    (write-sequence data stream)
    (finish-output stream)))

;;; gRPC client call execution

(defun make-grpc-connection (request)
  "Create a gRPC connection based on the request parameters."
  (let* ((host (slot-value request 'conformance-proto::host))
         (port (slot-value request 'conformance-proto::port))
         (protocol (slot-value request 'conformance-proto::protocol)))
    (log-msg "Connecting to ~A:~A protocol=~A" host port protocol)
    ;; Only support gRPC for now (protocol = 2)
    (unless (= protocol 2)
      (error "Unsupported protocol: ~A (only GRPC supported)" protocol))
    ;; Create channel with no default timeout - timeout should be per-call
    (ag-grpc:make-channel host port :timeout nil)))

(defun convert-request-headers (headers test-name)
  "Convert conformance Header objects to gRPC metadata.
Also adds the x-test-case-name header."
  (let ((metadata (ag-grpc:make-grpc-metadata)))
    ;; Add test name header first
    (ag-grpc:metadata-set metadata "x-test-case-name" test-name)
    ;; Add headers from the request
    ;; Process in reverse order because metadata-add uses push (prepends)
    (dolist (h (reverse headers))
      (let ((name (slot-value h 'conformance-proto::name))
            (values (slot-value h 'conformance-proto::value)))
        ;; Value is a list of strings - add each value in reverse
        ;; because metadata-add prepends (preserves original order)
        (dolist (v (reverse values))
          (ag-grpc:metadata-add metadata name v))))
    metadata))

(defun run-cancelled-unary-call (conn service method request-messages metadata cancel-after-ms &optional encoding)
  "Execute a unary gRPC call and then cancel it.
CANCEL-AFTER-MS - Delay before cancellation (0 = immediate).
ENCODING - Compression encoding to use.
Returns a cancelled status result."
  ;; Get the actual request
  (let* ((any-msg (first request-messages))
         (request-data (slot-value any-msg 'conformance-proto::value))
         (request (ag-proto:deserialize-from-bytes 'unary-request request-data))
         (method-path (format nil "/~A/~A" service method)))
    (log-msg "Cancelled call to ~A, cancel-after-ms=~A" method-path cancel-after-ms)
    ;; Low-level call: open stream, send request, then cancel
    (let* ((stream (ag-grpc::channel-new-stream conn))
           (stream-id (ag-http2:stream-id stream)))
      ;; Send headers (no timeout since we're cancelling)
      (ag-grpc::channel-send-headers conn stream-id method-path
                                      :metadata metadata
                                      :timeout nil)
      ;; Send request with END_STREAM
      (let ((request-bytes (ag-proto:serialize-to-bytes request)))
        (ag-grpc::channel-send-message conn stream-id request-bytes
                                        :end-stream t
                                        :encoding encoding))
      ;; Wait if needed
      (when (and cancel-after-ms (plusp cancel-after-ms))
        (sleep (/ cancel-after-ms 1000.0)))
      ;; Cancel the stream with RST_STREAM
      (ag-grpc:channel-cancel-stream conn stream-id)
      (log-msg "Stream ~A cancelled" stream-id)
      ;; Return cancelled status
      (values nil nil nil ag-grpc:+grpc-status-cancelled+ "Canceled"))))

(defun run-compressed-unary-call (conn method-path request metadata timeout-seconds encoding)
  "Execute a unary gRPC call with compression using low-level APIs.
Returns (values response headers trailers status-code status-message)."
  (ag-grpc::ensure-connected conn)
  (let* ((stream (ag-grpc::channel-new-stream conn))
         (stream-id (ag-http2:stream-id stream)))
    ;; Send headers
    (ag-grpc::channel-send-headers conn stream-id method-path
                                    :metadata metadata
                                    :timeout timeout-seconds)
    ;; Send request with compression
    (let ((request-bytes (ag-proto:serialize-to-bytes request)))
      (ag-grpc::channel-send-message conn stream-id request-bytes
                                      :end-stream t
                                      :encoding encoding))
    ;; Receive response headers
    (let ((raw-headers (ag-grpc::channel-receive-headers conn stream-id)))
      ;; Check HTTP status
      (let ((status-header (assoc :status raw-headers)))
        (unless (and status-header (string= (cdr status-header) "200"))
          (let ((http-status (if status-header (parse-integer (cdr status-header) :junk-allowed t) 0)))
            (return-from run-compressed-unary-call
              (values nil raw-headers raw-headers
                      (ag-grpc::http-status-to-grpc-status (or http-status 0))
                      (format nil "HTTP ~A" (or http-status "unknown")))))))
      ;; Get response encoding for decompression
      (let* ((response-encoding (ag-grpc::get-response-encoding raw-headers))
             (response-data (ag-grpc::channel-receive-message conn stream-id response-encoding))
             (response (when response-data
                         (ag-proto:deserialize-from-bytes 'unary-response response-data))))
        ;; Receive trailers
        (let ((raw-trailers (ag-grpc::channel-receive-trailers conn stream-id)))
          ;; Extract status
          (let* ((status-trailer (assoc "grpc-status" raw-trailers :test #'string-equal))
                 (status-header (assoc "grpc-status" raw-headers :test #'string-equal))
                 (status-code (cond
                                (status-trailer (parse-integer (cdr status-trailer)))
                                (status-header (parse-integer (cdr status-header)))
                                (t ag-grpc:+grpc-status-internal+)))
                 (message-trailer (assoc "grpc-message" (or raw-trailers raw-headers)
                                         :test #'string-equal))
                 (status-message (when message-trailer
                                   (ag-grpc::percent-decode (cdr message-trailer)))))
            ;; Signal error if not OK
            (unless (zerop status-code)
              (error 'ag-grpc:grpc-status-error
                     :code status-code
                     :message status-message
                     :headers raw-headers
                     :trailers raw-trailers))
            ;; Check for missing response
            (when (and (zerop status-code) (null response))
              (error 'ag-grpc:grpc-status-error
                     :code ag-grpc:+grpc-status-unimplemented+
                     :message "OK status but no response message"
                     :headers raw-headers
                     :trailers raw-trailers))
            (values response raw-headers raw-trailers status-code status-message)))))))

(defun run-unary-call (conn service method request-messages codec metadata timeout-ms &optional encoding)
  "Execute a unary gRPC call and return the response.
TIMEOUT-MS - Timeout in milliseconds (0 means no timeout).
ENCODING - Compression encoding to use (e.g., \"gzip\")."
  (declare (ignore codec))
  ;; For unary, we expect exactly one request message
  (let* ((any-msg (first request-messages))
         ;; Unpack the Any to get the actual request
         (request-data (slot-value any-msg 'conformance-proto::value))
         (request (ag-proto:deserialize-from-bytes 'unary-request request-data))
         ;; Make the actual gRPC call
         (method-path (format nil "/~A/~A" service method))
         ;; Convert timeout from ms to seconds, nil if 0
         (timeout-seconds (when (and timeout-ms (plusp timeout-ms))
                           (/ timeout-ms 1000.0))))
    (log-msg "Calling ~A with ~A metadata headers, timeout=~A ms, encoding=~A"
             method-path (ag-grpc:metadata-count metadata) timeout-ms encoding)
    (handler-case
        ;; Use low-level API when compression is needed
        (if encoding
            (run-compressed-unary-call conn method-path request metadata timeout-seconds encoding)
            (let ((call (ag-grpc:call-unary conn method-path request
                                            :metadata metadata
                                            :timeout timeout-seconds
                                            :response-type 'unary-response)))
              (log-msg "Call completed, status=~A" (ag-grpc:call-status call))
              (values (ag-grpc:call-response call)
                      (ag-grpc::call-response-headers call)
                      (ag-grpc::call-response-trailers call)
                      (or (ag-grpc:call-status call) 0)
                      (ag-grpc::call-status-message call))))
      (ag-grpc:grpc-status-error (e)
        ;; Return the error status with headers and trailers
        (log-msg "gRPC status error: code=~A msg=~A"
                 (ag-grpc:grpc-status-error-code e)
                 (ag-grpc:grpc-status-error-message e))
        (values nil
                (ag-grpc:grpc-status-error-headers e)
                (ag-grpc:grpc-status-error-trailers e)
                (ag-grpc:grpc-status-error-code e)
                (ag-grpc:grpc-status-error-message e)))
      (ag-grpc:grpc-error (e)
        ;; Protocol errors (compression, etc.) - return as INTERNAL
        (log-msg "gRPC protocol error: ~A" (ag-grpc:grpc-error-message e))
        (values nil nil nil
                ag-grpc:+grpc-status-internal+
                (ag-grpc:grpc-error-message e)))
      (error (e)
        ;; Any other error (e.g., protobuf deserialization) - return as INTERNAL
        (log-msg "Call error: ~A" e)
        (values nil nil nil
                ag-grpc:+grpc-status-internal+
                (format nil "~A" e))))))

(defun run-server-stream-call (conn service method request-messages codec metadata timeout-ms &optional encoding)
  "Execute a server streaming gRPC call and return all responses.
TIMEOUT-MS - Timeout in milliseconds (0 means no timeout).
ENCODING - Compression encoding to use."
  (declare (ignore codec encoding))  ; TODO: implement compression for streaming
  ;; For server streaming, we expect exactly one request message
  (let* ((any-msg (first request-messages))
         ;; Unpack the Any to get the actual request
         (request-data (slot-value any-msg 'conformance-proto::value))
         (request (ag-proto:deserialize-from-bytes 'conformance-proto::server-stream-request request-data))
         ;; Make the actual gRPC call
         (method-path (format nil "/~A/~A" service method))
         ;; Convert timeout from ms to seconds, nil if 0
         (timeout-seconds (when (and timeout-ms (plusp timeout-ms))
                           (/ timeout-ms 1000.0))))
    (log-msg "Server stream call to ~A with ~A metadata headers, timeout=~A ms"
             method-path (ag-grpc:metadata-count metadata) timeout-ms)
    (handler-case
        (let ((stream (ag-grpc:call-server-stream conn method-path request
                                                   :metadata metadata
                                                   :timeout timeout-seconds
                                                   :response-type 'conformance-proto::server-stream-response)))
          ;; Collect all responses - wrap in timeout for client-side deadline enforcement
          (flet ((receive-all ()
                   (let ((responses nil))
                     (loop for response = (ag-grpc:stream-receive-message stream)
                           while response
                           do (push response responses))
                     ;; Finish the stream to get final status
                     (ag-grpc:stream-finish stream)
                     (log-msg "Server stream completed, got ~A responses, status=~A"
                              (length responses) (ag-grpc:stream-call-status stream))
                     (values (nreverse responses)
                             (ag-grpc:stream-call-response-headers stream)
                             (ag-grpc:stream-call-response-trailers stream)
                             (or (ag-grpc:stream-call-status stream) 0)
                             (ag-grpc:stream-call-status-message stream)))))
            (if timeout-seconds
                (handler-case
                    (bt2:with-timeout (timeout-seconds)
                      (receive-all))
                  (bt2:timeout ()
                    ;; Cancel the stream and return deadline exceeded
                    (ag-grpc:channel-cancel-stream conn (ag-grpc::stream-call-stream-id stream))
                    (log-msg "Server stream deadline exceeded")
                    (values nil
                            (ag-grpc:stream-call-response-headers stream)
                            nil
                            ag-grpc:+grpc-status-deadline-exceeded+
                            "Deadline exceeded")))
                (receive-all))))
      (ag-grpc:grpc-status-error (e)
        ;; Return the error status with headers and trailers
        (log-msg "gRPC status error: code=~A msg=~A"
                 (ag-grpc:grpc-status-error-code e)
                 (ag-grpc:grpc-status-error-message e))
        (values nil
                (ag-grpc:grpc-status-error-headers e)
                (ag-grpc:grpc-status-error-trailers e)
                (ag-grpc:grpc-status-error-code e)
                (ag-grpc:grpc-status-error-message e)))
      (ag-grpc:grpc-error (e)
        ;; Protocol errors - return as INTERNAL
        (log-msg "gRPC protocol error: ~A" (ag-grpc:grpc-error-message e))
        (values nil nil nil
                ag-grpc:+grpc-status-internal+
                (ag-grpc:grpc-error-message e)))
      (error (e)
        ;; Any other error - return as INTERNAL
        (log-msg "Server stream error: ~A" e)
        (values nil nil nil
                ag-grpc:+grpc-status-internal+
                (format nil "~A" e))))))

(defun run-cancelled-server-stream-call (conn service method request-messages metadata
                                         cancel-after-close-send-ms cancel-after-responses &optional encoding)
  "Execute a server streaming gRPC call and cancel it.
CANCEL-AFTER-CLOSE-SEND-MS - Cancel after this delay (ms) following close-send.
ENCODING - Compression encoding to use.
CANCEL-AFTER-RESPONSES - Cancel after receiving this many responses.
Returns collected responses and cancelled status."
  (declare (ignore encoding))  ; Encoding is for request; we get response encoding from headers
  (let* ((any-msg (first request-messages))
         (request-data (slot-value any-msg 'conformance-proto::value))
         (request (ag-proto:deserialize-from-bytes 'conformance-proto::server-stream-request request-data))
         (method-path (format nil "/~A/~A" service method)))
    (log-msg "Server stream cancellation to ~A, after-close-send=~Ams after-responses=~A"
             method-path cancel-after-close-send-ms cancel-after-responses)
    ;; Low-level call: open stream, send request, receive some responses, then cancel
    (let* ((stream (ag-grpc::channel-new-stream conn))
           (stream-id (ag-http2:stream-id stream))
           (responses nil)
           (headers nil)
           (response-encoding nil))
      ;; Send headers
      (ag-grpc::channel-send-headers conn stream-id method-path
                                      :metadata metadata
                                      :timeout nil)
      ;; Send request with END_STREAM (we're done sending)
      (let ((request-bytes (ag-proto:serialize-to-bytes request)))
        (ag-grpc::channel-send-message conn stream-id request-bytes :end-stream t))
      ;; If cancel-after-close-send, wait and then cancel immediately
      (when (and cancel-after-close-send-ms (plusp cancel-after-close-send-ms))
        (sleep (/ cancel-after-close-send-ms 1000.0))
        (ag-grpc:channel-cancel-stream conn stream-id)
        (log-msg "Stream ~A cancelled after close-send" stream-id)
        (return-from run-cancelled-server-stream-call
          (values nil nil nil ag-grpc:+grpc-status-cancelled+ "Canceled")))
      ;; If cancel-after-responses, receive that many and then cancel
      (when (and cancel-after-responses (plusp cancel-after-responses))
        ;; Receive headers first
        (setf headers (ag-grpc::channel-receive-headers conn stream-id))
        ;; Extract response encoding for decompression
        (setf response-encoding (ag-grpc::get-response-encoding headers))
        ;; Receive the specified number of responses
        (loop repeat cancel-after-responses
              for data = (ag-grpc::channel-receive-message conn stream-id response-encoding)
              while data
              do (push (ag-proto:deserialize-from-bytes
                        'conformance-proto::server-stream-response data)
                       responses))
        ;; Cancel the stream
        (ag-grpc:channel-cancel-stream conn stream-id)
        (log-msg "Stream ~A cancelled after ~A responses" stream-id (length responses))
        (return-from run-cancelled-server-stream-call
          (values (nreverse responses) headers nil
                  ag-grpc:+grpc-status-cancelled+ "Canceled")))
      ;; Default: cancel immediately after close-send
      (ag-grpc:channel-cancel-stream conn stream-id)
      (log-msg "Stream ~A cancelled immediately" stream-id)
      (values nil nil nil ag-grpc:+grpc-status-cancelled+ "Canceled"))))

;;; ============================================================================
;;; Client Streaming RPC
;;; ============================================================================

(defun run-client-stream-call (conn service method request-messages codec metadata timeout-ms &optional encoding)
  "Execute a client streaming gRPC call and return the response.
TIMEOUT-MS - Timeout in milliseconds (0 means no timeout).
ENCODING - Compression encoding to use."
  (declare (ignore codec encoding))  ; TODO: implement compression for streaming
  (let* ((method-path (format nil "/~A/~A" service method))
         (timeout-seconds (when (and timeout-ms (plusp timeout-ms))
                           (/ timeout-ms 1000.0))))
    (log-msg "Client stream call to ~A with ~A messages, ~A metadata headers, timeout=~A ms"
             method-path (length request-messages) (ag-grpc:metadata-count metadata) timeout-ms)
    (handler-case
        (let ((stream (ag-grpc:call-client-streaming conn method-path
                                                      :metadata metadata
                                                      :timeout timeout-seconds
                                                      :response-type 'conformance-proto::client-stream-response)))
          ;; Send all request messages
          (flet ((send-all-and-recv ()
                   (dolist (any-msg request-messages)
                     (let* ((request-data (slot-value any-msg 'conformance-proto::value))
                            (request (ag-proto:deserialize-from-bytes
                                      'conformance-proto::client-stream-request request-data)))
                       (ag-grpc:stream-send stream request)))
                   ;; Close send and receive the response
                   (multiple-value-bind (response status)
                       (ag-grpc:stream-close-and-recv stream)
                     (let ((call (ag-grpc::stream-call stream)))
                       (log-msg "Client stream completed, status=~A" status)
                       (values response
                               (ag-grpc::call-response-headers call)
                               (ag-grpc::call-response-trailers call)
                               (or status 0)
                               (ag-grpc::call-status-message call))))))
            (if timeout-seconds
                (handler-case
                    (bt2:with-timeout (timeout-seconds)
                      (send-all-and-recv))
                  (bt2:timeout ()
                    (ag-grpc:channel-cancel-stream conn (ag-grpc::client-stream-id stream))
                    (log-msg "Client stream deadline exceeded")
                    (values nil nil nil
                            ag-grpc:+grpc-status-deadline-exceeded+
                            "Deadline exceeded")))
                (send-all-and-recv))))
      (ag-grpc:grpc-status-error (e)
        (log-msg "gRPC status error: code=~A msg=~A"
                 (ag-grpc:grpc-status-error-code e)
                 (ag-grpc:grpc-status-error-message e))
        (values nil
                (ag-grpc:grpc-status-error-headers e)
                (ag-grpc:grpc-status-error-trailers e)
                (ag-grpc:grpc-status-error-code e)
                (ag-grpc:grpc-status-error-message e)))
      (ag-grpc:grpc-error (e)
        (log-msg "gRPC protocol error: ~A" (ag-grpc:grpc-error-message e))
        (values nil nil nil
                ag-grpc:+grpc-status-internal+
                (ag-grpc:grpc-error-message e)))
      (error (e)
        (log-msg "Client stream error: ~A" e)
        (values nil nil nil
                ag-grpc:+grpc-status-internal+
                (format nil "~A" e))))))

(defun run-cancelled-client-stream-call (conn service method request-messages metadata
                                         cancel-before-close-send cancel-after-close-send-ms &optional encoding)
  "Execute a client streaming gRPC call and cancel it.
CANCEL-BEFORE-CLOSE-SEND - If true, cancel before closing the send side.
CANCEL-AFTER-CLOSE-SEND-MS - Cancel after this delay (ms) following close-send.
ENCODING - Compression encoding to use."
  (declare (ignore encoding))  ; TODO: implement compression
  (let ((method-path (format nil "/~A/~A" service method)))
    (log-msg "Client stream cancellation to ~A, before-close-send=~A after-close-send-ms=~A"
             method-path cancel-before-close-send cancel-after-close-send-ms)
    ;; Low-level: open stream, send messages, then cancel
    (let* ((h2-stream (ag-grpc::channel-new-stream conn))
           (stream-id (ag-http2:stream-id h2-stream))
           (headers nil))
      ;; Send headers (NOT end-stream)
      (ag-grpc::channel-send-headers conn stream-id method-path
                                      :metadata metadata
                                      :timeout nil
                                      :end-stream nil)
      ;; Send all request messages
      (dolist (any-msg request-messages)
        (let* ((request-data (slot-value any-msg 'conformance-proto::value))
               (request (ag-proto:deserialize-from-bytes
                         'conformance-proto::client-stream-request request-data)))
          (ag-grpc::channel-send-message conn stream-id
                                          (ag-proto:serialize-to-bytes request)
                                          :end-stream nil)))
      ;; Cancel before close-send if requested
      (when cancel-before-close-send
        (ag-grpc:channel-cancel-stream conn stream-id)
        (log-msg "Stream ~A cancelled before close-send" stream-id)
        (return-from run-cancelled-client-stream-call
          (values nil nil nil ag-grpc:+grpc-status-cancelled+ "Canceled")))
      ;; Close send side (send empty data with END_STREAM)
      (ag-http2:connection-send-data (ag-grpc::channel-connection conn) stream-id
                                     (make-array 0 :element-type '(unsigned-byte 8))
                                     :end-stream t)
      ;; Cancel after close-send with delay
      (when (and cancel-after-close-send-ms (plusp cancel-after-close-send-ms))
        (sleep (/ cancel-after-close-send-ms 1000.0)))
      ;; Cancel the stream
      (ag-grpc:channel-cancel-stream conn stream-id)
      (log-msg "Stream ~A cancelled after close-send" stream-id)
      (values nil headers nil ag-grpc:+grpc-status-cancelled+ "Canceled"))))

;;; ============================================================================
;;; Bidirectional Streaming RPC
;;; ============================================================================

(defun run-bidi-stream-call (conn service method request-messages codec metadata timeout-ms full-duplex &optional encoding)
  "Execute a bidirectional streaming gRPC call.
REQUEST-MESSAGES - List of request messages to send
FULL-DUPLEX - If true, interleave sends and receives; otherwise send all then receive all
TIMEOUT-MS - Timeout in milliseconds (0 means no timeout).
ENCODING - Compression encoding to use."
  (declare (ignore codec encoding))  ; TODO: implement compression
  (let* ((method-path (format nil "/~A/~A" service method))
         (timeout-seconds (when (and timeout-ms (plusp timeout-ms))
                           (/ timeout-ms 1000.0))))
    (log-msg "Bidi stream call to ~A with ~A messages, full-duplex=~A, timeout=~A ms"
             method-path (length request-messages) full-duplex timeout-ms)
    (handler-case
        (let ((stream (ag-grpc:call-bidirectional-streaming conn method-path
                                                             :metadata metadata
                                                             :timeout timeout-seconds
                                                             :response-type 'conformance-proto::bidi-stream-response)))
          (flet ((do-bidi ()
                   (let ((responses nil))
                     ;; Use handler-case inside to preserve collected responses on error
                     (handler-case
                         (progn
                           (if full-duplex
                               ;; Full duplex: interleave sends and receives
                               (progn
                                 (dolist (any-msg request-messages)
                                   (let* ((request-data (slot-value any-msg 'conformance-proto::value))
                                          (request (ag-proto:deserialize-from-bytes
                                                    'conformance-proto::bidi-stream-request request-data)))
                                     ;; Send request
                                     (ag-grpc:stream-send stream request)
                                     ;; Try to receive a response
                                     (let ((response (ag-grpc:stream-read-message stream)))
                                       (when response
                                         (push response responses)))))
                                 ;; Close send side
                                 (ag-grpc:stream-close-send stream)
                                 ;; Drain any remaining responses
                                 (loop for response = (ag-grpc:stream-read-message stream)
                                       while response
                                       do (push response responses)))
                               ;; Half duplex: send all, then receive all
                               (progn
                                 ;; Send all requests
                                 (dolist (any-msg request-messages)
                                   (let* ((request-data (slot-value any-msg 'conformance-proto::value))
                                          (request (ag-proto:deserialize-from-bytes
                                                    'conformance-proto::bidi-stream-request request-data)))
                                     (ag-grpc:stream-send stream request)))
                                 ;; Close send side
                                 (ag-grpc:stream-close-send stream)
                                 ;; Receive all responses
                                 (loop for response = (ag-grpc:stream-read-message stream)
                                       while response
                                       do (push response responses))))
                           ;; Get final status (success case)
                           (let ((call (ag-grpc::stream-call stream)))
                             (log-msg "Bidi stream completed, got ~A responses, status=~A"
                                      (length responses) (ag-grpc:stream-status stream))
                             (values (nreverse responses)
                                     (ag-grpc::call-response-headers call)
                                     (ag-grpc::call-response-trailers call)
                                     (or (ag-grpc:stream-status stream) 0)
                                     (ag-grpc::call-status-message call))))
                       ;; Catch error inside to preserve responses
                       (ag-grpc:grpc-status-error (e)
                         (log-msg "gRPC status error: code=~A msg=~A (collected ~A responses)"
                                  (ag-grpc:grpc-status-error-code e)
                                  (ag-grpc:grpc-status-error-message e)
                                  (length responses))
                         (values (nreverse responses)
                                 (ag-grpc:grpc-status-error-headers e)
                                 (ag-grpc:grpc-status-error-trailers e)
                                 (ag-grpc:grpc-status-error-code e)
                                 (ag-grpc:grpc-status-error-message e)))))))
            (if timeout-seconds
                (handler-case
                    (bt2:with-timeout (timeout-seconds)
                      (do-bidi))
                  (bt2:timeout ()
                    (ag-grpc:channel-cancel-stream conn (ag-grpc::bidi-stream-id stream))
                    (log-msg "Bidi stream deadline exceeded")
                    (values nil nil nil
                            ag-grpc:+grpc-status-deadline-exceeded+
                            "Deadline exceeded")))
                (do-bidi))))
      (ag-grpc:grpc-error (e)
        (log-msg "gRPC protocol error: ~A" (ag-grpc:grpc-error-message e))
        (values nil nil nil
                ag-grpc:+grpc-status-internal+
                (ag-grpc:grpc-error-message e)))
      (error (e)
        (log-msg "Bidi stream error: ~A" e)
        (values nil nil nil
                ag-grpc:+grpc-status-internal+
                (format nil "~A" e))))))

(defun run-cancelled-bidi-stream-call (conn service method request-messages metadata
                                       cancel-before-close-send cancel-after-close-send-ms
                                       cancel-after-responses &optional encoding)
  "Execute a bidirectional streaming gRPC call and cancel it.
ENCODING - Compression encoding to use."
  (declare (ignore encoding))  ; Encoding is for request; we get response encoding from headers
  (let ((method-path (format nil "/~A/~A" service method)))
    (log-msg "Bidi stream cancellation to ~A, before-close-send=~A after-close-send-ms=~A after-responses=~A"
             method-path cancel-before-close-send cancel-after-close-send-ms cancel-after-responses)
    (let* ((h2-stream (ag-grpc::channel-new-stream conn))
           (stream-id (ag-http2:stream-id h2-stream))
           (responses nil)
           (headers nil)
           (response-encoding nil))
      ;; Send headers (NOT end-stream)
      (ag-grpc::channel-send-headers conn stream-id method-path
                                      :metadata metadata
                                      :timeout nil
                                      :end-stream nil)
      ;; Send all request messages
      (dolist (any-msg request-messages)
        (let* ((request-data (slot-value any-msg 'conformance-proto::value))
               (request (ag-proto:deserialize-from-bytes
                         'conformance-proto::bidi-stream-request request-data)))
          (ag-grpc::channel-send-message conn stream-id
                                          (ag-proto:serialize-to-bytes request)
                                          :end-stream nil)))
      ;; Cancel before close-send if requested
      (when cancel-before-close-send
        (ag-grpc:channel-cancel-stream conn stream-id)
        (log-msg "Stream ~A cancelled before close-send" stream-id)
        (return-from run-cancelled-bidi-stream-call
          (values nil nil nil ag-grpc:+grpc-status-cancelled+ "Canceled")))
      ;; Close send side
      (ag-http2:connection-send-data (ag-grpc::channel-connection conn) stream-id
                                     (make-array 0 :element-type '(unsigned-byte 8))
                                     :end-stream t)
      ;; Cancel after receiving N responses
      (when (and cancel-after-responses (plusp cancel-after-responses))
        (setf headers (ag-grpc::channel-receive-headers conn stream-id))
        ;; Extract response encoding for decompression
        (setf response-encoding (ag-grpc::get-response-encoding headers))
        (loop repeat cancel-after-responses
              for data = (ag-grpc::channel-receive-message conn stream-id response-encoding)
              while data
              do (push (ag-proto:deserialize-from-bytes
                        'conformance-proto::bidi-stream-response data)
                       responses))
        (ag-grpc:channel-cancel-stream conn stream-id)
        (log-msg "Stream ~A cancelled after ~A responses" stream-id (length responses))
        (return-from run-cancelled-bidi-stream-call
          (values (nreverse responses) headers nil
                  ag-grpc:+grpc-status-cancelled+ "Canceled")))
      ;; Cancel after close-send with delay
      (when (and cancel-after-close-send-ms (plusp cancel-after-close-send-ms))
        (sleep (/ cancel-after-close-send-ms 1000.0)))
      ;; Default: cancel immediately
      (ag-grpc:channel-cancel-stream conn stream-id)
      (log-msg "Stream ~A cancelled after close-send" stream-id)
      (values nil headers nil ag-grpc:+grpc-status-cancelled+ "Canceled"))))

(defun make-error-response (test-name message)
  "Create an error response for conformance tests."
  (let ((resp (make-instance 'client-compat-response
                :test-name test-name
                :error (make-instance 'client-error-result :message message))))
    (setf (slot-value resp 'conformance-proto::result-case) :error)
    resp))

(defun make-success-response (test-name result)
  "Create a success response for conformance tests."
  (let ((resp (make-instance 'client-compat-response
                :test-name test-name
                :response result)))
    (setf (slot-value resp 'conformance-proto::result-case) :response)
    resp))

(defun convert-headers (headers)
  "Convert HTTP/2 headers to conformance Header objects.
Combines duplicate header names into single Header objects with multiple values.
Handles edge cases where headers might be malformed."
  (when (and headers (listp headers))
    ;; First pass: group values by header name
    (let ((header-values (make-hash-table :test #'equal)))
      (loop for entry in headers
            when (and (consp entry) (car entry))
              do (handler-case
                     (let* ((name (car entry))
                            (value (cdr entry))
                            ;; Convert keyword names to strings (e.g., :status -> ":status")
                            (name-str (cond
                                        ((keywordp name)
                                         (format nil ":~(~A~)" (symbol-name name)))
                                        ((stringp name)
                                         (string-downcase name))
                                        (t
                                         (princ-to-string name))))
                            ;; Ensure value is always a string
                            (value-str (cond
                                         ((stringp value) value)
                                         ((null value) "")
                                         (t (princ-to-string value)))))
                       ;; Append to existing values or create new entry
                       (push value-str (gethash name-str header-values)))
                   (error (e)
                     (log-msg "Warning: skipping malformed header entry ~A: ~A" entry e))))
      ;; Second pass: create Header objects with all values (in original order)
      (let ((result nil))
        (maphash (lambda (name values)
                   (push (make-instance 'header
                           :name name
                           :value (nreverse values))
                         result))
                 header-values)
        result))))

(defun execute-request (request)
  "Execute a conformance test request and return a response."
  (let ((test-name (slot-value request 'conformance-proto::test-name)))
    (handler-case
        (let ((stream-type (slot-value request 'conformance-proto::stream-type)))
          ;; Support unary (1), client streaming (2), server streaming (3), bidi (4)
          (unless (member stream-type '(1 2 3 4))
            (return-from execute-request
              (make-error-response test-name
                (format nil "Unsupported stream type: ~A" stream-type))))
          ;; Create connection and execute
          (let* ((service (slot-value request 'conformance-proto::service))
                 (method (slot-value request 'conformance-proto::proto-method))
                 (request-messages (slot-value request 'conformance-proto::request-messages))
                 (request-headers (slot-value request 'conformance-proto::request-headers))
                 (codec (slot-value request 'conformance-proto::codec))
                 (timeout-ms (slot-value request 'conformance-proto::timeout-ms))
                 (cancel (slot-value request 'conformance-proto::cancel))
                 (compression (slot-value request 'conformance-proto::compression))
                 ;; Convert compression enum to encoding string
                 (encoding (case compression
                             (2 "gzip")   ; COMPRESSION_GZIP
                             (t nil)))    ; COMPRESSION_IDENTITY or unspecified
                 (metadata (convert-request-headers request-headers test-name))
                 (conn (make-grpc-connection request)))
            ;; Set grpc-encoding header if using compression
            (when encoding
              (ag-grpc:metadata-set metadata "grpc-encoding" encoding))
            (unwind-protect
                (case stream-type
                  ;; Unary RPC (stream-type = 1)
                  (1 (execute-unary-request conn service method request-messages
                                            codec metadata timeout-ms cancel test-name encoding))
                  ;; Client streaming RPC (stream-type = 2)
                  (2 (execute-client-stream-request conn service method request-messages
                                                    codec metadata timeout-ms cancel test-name encoding))
                  ;; Server streaming RPC (stream-type = 3)
                  (3 (execute-server-stream-request conn service method request-messages
                                                    codec metadata timeout-ms cancel test-name encoding))
                  ;; Bidirectional streaming RPC (stream-type = 4)
                  (4 (execute-bidi-stream-request conn service method request-messages
                                                  codec metadata timeout-ms cancel test-name encoding)))
              (ag-grpc:channel-close conn))))
      (error (e)
        (make-error-response test-name
          (format nil "Client error: ~A" e))))))

(defun execute-unary-request (conn service method request-messages codec metadata timeout-ms cancel test-name &optional encoding)
  "Execute a unary RPC request."
  (multiple-value-bind (response headers trailers status-code status-msg)
      ;; Check if this is a cancellation test
      (if cancel
          (let ((cancel-after-ms
                  (let ((timing-case (slot-value cancel 'conformance-proto::cancel-timing-case)))
                    (case timing-case
                      (:after-close-send-ms
                       (slot-value cancel 'conformance-proto::after-close-send-ms))
                      (otherwise 0)))))
            (run-cancelled-unary-call conn service method request-messages metadata cancel-after-ms encoding))
          (run-unary-call conn service method request-messages codec metadata timeout-ms encoding))
    ;; Build the response
    (let ((result (make-instance 'client-response-result
                    :response-headers (convert-headers headers)
                    :response-trailers (convert-headers trailers))))
      ;; Add payload if we have a successful response
      (when (and response (zerop status-code))
        (let ((payload (or (slot-value response 'conformance-proto::payload)
                           (make-instance 'conformance-payload))))
          (setf (slot-value result 'conformance-proto::payloads) (list payload))))
      ;; Add error info if non-zero status
      (when (and status-code (not (zerop status-code)))
        (let* ((error-details (extract-error-details trailers))
               (err (make-instance 'proto-error
                      :code status-code
                      :message (or status-msg "")
                      :details error-details)))
          (setf (slot-value result 'conformance-proto::proto-error) err)))
      (make-success-response test-name result))))

(defun execute-server-stream-request (conn service method request-messages codec metadata timeout-ms cancel test-name &optional encoding)
  "Execute a server streaming RPC request."
  (multiple-value-bind (responses headers trailers status-code status-msg)
      ;; Check if this is a cancellation test
      (if cancel
          (let ((timing-case (slot-value cancel 'conformance-proto::cancel-timing-case)))
            (case timing-case
              (:after-close-send-ms
               (run-cancelled-server-stream-call conn service method request-messages metadata
                                                 (slot-value cancel 'conformance-proto::after-close-send-ms) nil encoding))
              (:after-num-responses
               (run-cancelled-server-stream-call conn service method request-messages metadata
                                                 nil (slot-value cancel 'conformance-proto::after-num-responses) encoding))
              (otherwise
               ;; Default: immediate cancel after close-send
               (run-cancelled-server-stream-call conn service method request-messages metadata 0 nil encoding))))
          (run-server-stream-call conn service method request-messages codec metadata timeout-ms encoding))
    ;; Build the response
    (let ((result (make-instance 'client-response-result
                    :response-headers (convert-headers headers)
                    :response-trailers (convert-headers trailers))))
      ;; Add payloads from all responses (even if there's an error)
      (when responses
        (let ((payloads (mapcar (lambda (resp)
                                  (or (slot-value resp 'conformance-proto::payload)
                                      (make-instance 'conformance-payload)))
                                responses)))
          (setf (slot-value result 'conformance-proto::payloads) payloads)))
      ;; Add error info if non-zero status
      (when (and status-code (not (zerop status-code)))
        (let* ((error-details (extract-error-details trailers))
               (err (make-instance 'proto-error
                      :code status-code
                      :message (or status-msg "")
                      :details error-details)))
          (setf (slot-value result 'conformance-proto::proto-error) err)))
      (make-success-response test-name result))))

(defun execute-client-stream-request (conn service method request-messages codec metadata timeout-ms cancel test-name &optional encoding)
  "Execute a client streaming RPC request."
  (multiple-value-bind (response headers trailers status-code status-msg)
      ;; Check if this is a cancellation test
      (if cancel
          (let ((timing-case (slot-value cancel 'conformance-proto::cancel-timing-case)))
            (case timing-case
              (:before-close-send
               (run-cancelled-client-stream-call conn service method request-messages metadata t nil encoding))
              (:after-close-send-ms
               (run-cancelled-client-stream-call conn service method request-messages metadata
                                                 nil (slot-value cancel 'conformance-proto::after-close-send-ms) encoding))
              (otherwise
               ;; Default: immediate cancel after close-send
               (run-cancelled-client-stream-call conn service method request-messages metadata nil 0 encoding))))
          (run-client-stream-call conn service method request-messages codec metadata timeout-ms encoding))
    ;; Build the response
    (let ((result (make-instance 'client-response-result
                    :response-headers (convert-headers headers)
                    :response-trailers (convert-headers trailers))))
      ;; Add payload if we have a response
      (when response
        (let ((payload (or (slot-value response 'conformance-proto::payload)
                           (make-instance 'conformance-payload))))
          (setf (slot-value result 'conformance-proto::payloads) (list payload))))
      ;; Add error info if non-zero status
      (when (and status-code (not (zerop status-code)))
        (let* ((error-details (extract-error-details trailers))
               (err (make-instance 'proto-error
                      :code status-code
                      :message (or status-msg "")
                      :details error-details)))
          (setf (slot-value result 'conformance-proto::proto-error) err)))
      (make-success-response test-name result))))

(defun execute-bidi-stream-request (conn service method request-messages codec metadata timeout-ms cancel test-name &optional encoding)
  "Execute a bidirectional streaming RPC request."
  (multiple-value-bind (responses headers trailers status-code status-msg)
      ;; Check if this is a cancellation test
      (if cancel
          (let ((timing-case (slot-value cancel 'conformance-proto::cancel-timing-case)))
            (case timing-case
              (:before-close-send
               (run-cancelled-bidi-stream-call conn service method request-messages metadata t nil nil encoding))
              (:after-close-send-ms
               (run-cancelled-bidi-stream-call conn service method request-messages metadata
                                               nil (slot-value cancel 'conformance-proto::after-close-send-ms) nil encoding))
              (:after-num-responses
               (run-cancelled-bidi-stream-call conn service method request-messages metadata
                                               nil nil (slot-value cancel 'conformance-proto::after-num-responses) encoding))
              (otherwise
               ;; Default: immediate cancel after close-send
               (run-cancelled-bidi-stream-call conn service method request-messages metadata nil 0 nil encoding))))
          ;; Use half-duplex mode (send all, then receive all)
          (run-bidi-stream-call conn service method request-messages codec metadata timeout-ms nil encoding))
    ;; Build the response
    (let ((result (make-instance 'client-response-result
                    :response-headers (convert-headers headers)
                    :response-trailers (convert-headers trailers))))
      ;; Add payloads from all responses (even if there's an error)
      (when responses
        (let ((payloads (mapcar (lambda (resp)
                                  (or (slot-value resp 'conformance-proto::payload)
                                      (make-instance 'conformance-payload)))
                                responses)))
          (setf (slot-value result 'conformance-proto::payloads) payloads)))
      ;; Add error info if non-zero status
      (when (and status-code (not (zerop status-code)))
        (let* ((error-details (extract-error-details trailers))
               (err (make-instance 'proto-error
                      :code status-code
                      :message (or status-msg "")
                      :details error-details)))
          (setf (slot-value result 'conformance-proto::proto-error) err)))
      (make-success-response test-name result))))

;;; Main loop

(defun read-bytes-from-fd (fd count)
  "Read exactly COUNT bytes from file descriptor FD. Returns byte vector or nil on EOF."
  (let ((buffer (make-array count :element-type '(unsigned-byte 8)))
        (total 0))
    (sb-sys:with-pinned-objects (buffer)
      (loop while (< total count)
            do (let ((n (sb-unix:unix-read fd
                                           (sb-sys:sap+ (sb-sys:vector-sap buffer) total)
                                           (- count total))))
                 (cond
                   ((null n) ; Error
                    (return-from read-bytes-from-fd nil))
                   ((zerop n) ; EOF
                    (return-from read-bytes-from-fd nil))
                   (t (incf total n))))))
    buffer))

(defun write-bytes-to-fd (fd bytes)
  "Write BYTES to file descriptor FD."
  ;; sb-unix:unix-write can take a simple array directly (not a SAP)
  ;; Args: fd buf offset len
  (let ((total 0)
        (len (length bytes)))
    (loop while (< total len)
          do (multiple-value-bind (n err)
                 (sb-unix:unix-write fd bytes total (- len total))
               (when (or (null n) (<= n 0))
                 (log-msg "unix-write error: n=~A err=~A" n err)
                 (return-from write-bytes-to-fd nil))
               (incf total n)))
    t))

(defun read-request-from-fd (fd)
  "Read a ClientCompatRequest from file descriptor."
  (let ((len-bytes (read-bytes-from-fd fd 4)))
    (unless len-bytes
      (return-from read-request-from-fd nil))
    (let ((len (logior (ash (aref len-bytes 0) 24)
                       (ash (aref len-bytes 1) 16)
                       (ash (aref len-bytes 2) 8)
                       (aref len-bytes 3))))
      (log-msg "read-request-from-fd: length = ~A" len)
      (let ((data (read-bytes-from-fd fd len)))
        (unless data
          (return-from read-request-from-fd nil))
        (ag-proto:deserialize-from-bytes 'client-compat-request data)))))

(defun write-response-to-fd (fd response)
  "Write a ClientCompatResponse to file descriptor."
  (let* ((data-raw (ag-proto:serialize-to-bytes response))
         ;; Coerce to simple array for sb-unix:unix-write
         (data (make-array (length data-raw) :element-type '(unsigned-byte 8)
                           :initial-contents data-raw))
         (len (length data))
         (len-bytes (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref len-bytes 0) (ldb (byte 8 24) len))
    (setf (aref len-bytes 1) (ldb (byte 8 16) len))
    (setf (aref len-bytes 2) (ldb (byte 8 8) len))
    (setf (aref len-bytes 3) (ldb (byte 8 0) len))
    (write-bytes-to-fd fd len-bytes)
    (write-bytes-to-fd fd data)))

(defun main ()
  "Main entry point for the conformance client."
  (format *error-output* "~&MAIN STARTING~%")
  (force-output *error-output*)
  ;; Open log file for debugging
  (with-open-file (*log-stream* "/tmp/conformance-client.log"
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
    (format *error-output* "~&LOG FILE OPENED~%")
    (force-output *error-output*)
    (log-msg "Conformance client starting...")
    (handler-case
        (loop
          (log-msg "Waiting for request...")
          (let ((request (handler-case (read-request-from-fd 0)
                           (error (e)
                             (log-msg "Error reading request: ~A" e)
                             nil))))
            (unless request
              (log-msg "No more requests, exiting")
              (return))
            (log-msg "Got request: test=~A stream-type=~A"
                     (slot-value request 'conformance-proto::test-name)
                     (slot-value request 'conformance-proto::stream-type))
            (let ((response (handler-case (execute-request request)
                              (error (e)
                                (log-msg "Error executing request: ~A" e)
                                (make-error-response
                                  (slot-value request 'conformance-proto::test-name)
                                  (format nil "Fatal error: ~A" e))))))
              (log-msg "Writing response...")
              (handler-case (write-response-to-fd 1 response)
                (error (e)
                  (log-msg "Error writing response: ~A" e))))))
      (error (e)
        (log-msg "FATAL ERROR: ~A" e)))
    (log-msg "Client exiting")))

;; Run the client
(main)
