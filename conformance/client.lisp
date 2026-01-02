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

(defun run-unary-call (conn service method request-messages codec metadata timeout-ms)
  "Execute a unary gRPC call and return the response.
TIMEOUT-MS - Timeout in milliseconds (0 means no timeout)."
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
    (log-msg "Calling ~A with ~A metadata headers, timeout=~A ms"
             method-path (ag-grpc:metadata-count metadata) timeout-ms)
    (handler-case
        (let ((call (ag-grpc:call-unary conn method-path request
                                        :metadata metadata
                                        :timeout timeout-seconds
                                        :response-type 'unary-response)))
          (log-msg "Call completed, status=~A" (ag-grpc:call-status call))
          (values (ag-grpc:call-response call)
                  (ag-grpc::call-response-headers call)
                  (ag-grpc::call-response-trailers call)
                  (or (ag-grpc:call-status call) 0)
                  (ag-grpc::call-status-message call)))
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
        (progn
          ;; Only support unary for now (stream-type = 1)
          (let ((stream-type (slot-value request 'conformance-proto::stream-type)))
            (unless (= stream-type 1)
              (return-from execute-request
                (make-error-response test-name
                  (format nil "Unsupported stream type: ~A" stream-type)))))
          ;; Create connection and execute
          (let* ((service (slot-value request 'conformance-proto::service))
                 (method (slot-value request 'conformance-proto::proto-method))
                 (request-messages (slot-value request 'conformance-proto::request-messages))
                 (request-headers (slot-value request 'conformance-proto::request-headers))
                 (codec (slot-value request 'conformance-proto::codec))
                 (timeout-ms (slot-value request 'conformance-proto::timeout-ms))
                 (metadata (convert-request-headers request-headers test-name))
                 (conn (make-grpc-connection request)))
            (unwind-protect
                (multiple-value-bind (response headers trailers status-code status-msg)
                    (run-unary-call conn service method request-messages codec metadata timeout-ms)
                  ;; Build the response - always use ClientResponseResult
                  ;; ClientErrorResult is only for client-side failures, not gRPC errors
                  (let ((result (make-instance 'client-response-result
                                  :response-headers (convert-headers headers)
                                  :response-trailers (convert-headers trailers))))
                    ;; Add payload if we have a successful response
                    ;; The response's payload slot contains a ConformancePayload object
                    ;; For empty responses, payload might be nil but we still need to return
                    ;; an empty ConformancePayload to indicate we got a response
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
                    (make-success-response test-name result)))
              (ag-grpc:channel-close conn))))
      (error (e)
        (make-error-response test-name
          (format nil "Client error: ~A" e))))))

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
