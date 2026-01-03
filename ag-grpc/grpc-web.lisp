;;;; grpc-web.lisp - gRPC-Web protocol support for browser clients
;;;;
;;;; Implements gRPC-Web protocol for browser compatibility.
;;;; Supports both gRPC-Web (base64) and gRPC-Web-Text formats.

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; gRPC-Web Content Types
;;;; ========================================================================

(defparameter +grpc-web-content-type+ "application/grpc-web"
  "Content type for binary gRPC-Web.")

(defparameter +grpc-web-text-content-type+ "application/grpc-web-text"
  "Content type for base64-encoded gRPC-Web.")

(defparameter +grpc-web-proto-content-type+ "application/grpc-web+proto"
  "Content type for binary gRPC-Web with protobuf.")

(defparameter +grpc-web-text-proto-content-type+ "application/grpc-web-text+proto"
  "Content type for base64 gRPC-Web with protobuf.")

;;;; ========================================================================
;;;; gRPC-Web Frame Format
;;;; ========================================================================

;;; gRPC-Web uses the same 5-byte frame header as gRPC:
;;; - Byte 0: Flags (0x00 = data, 0x80 = trailers)
;;; - Bytes 1-4: Message length (big-endian)

(defconstant +grpc-web-data-frame+ #x00
  "Flag for data frame.")

(defconstant +grpc-web-trailer-frame+ #x80
  "Flag for trailer frame.")

;;;; ========================================================================
;;;; Base64 Encoding/Decoding
;;;; ========================================================================

(defparameter *base64-chars*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defparameter *base64-decode-table*
  (let ((table (make-array 256 :initial-element nil)))
    (loop for char across *base64-chars*
          for i from 0
          do (setf (aref table (char-code char)) i))
    (setf (aref table (char-code #\=)) 0) ; padding
    table))

(defun base64-encode (bytes)
  "Encode byte vector to base64 string."
  (let* ((len (length bytes))
         (output-len (* 4 (ceiling len 3)))
         (result (make-string output-len)))
    (loop for i from 0 below len by 3
          for j from 0 by 4
          do (let* ((b0 (aref bytes i))
                    (b1 (if (< (1+ i) len) (aref bytes (1+ i)) 0))
                    (b2 (if (< (+ i 2) len) (aref bytes (+ i 2)) 0))
                    (remaining (- len i)))
               (setf (char result j)
                     (char *base64-chars* (ash b0 -2)))
               (setf (char result (1+ j))
                     (char *base64-chars* (logior (ash (logand b0 #x03) 4)
                                                   (ash b1 -4))))
               (setf (char result (+ j 2))
                     (if (> remaining 1)
                         (char *base64-chars* (logior (ash (logand b1 #x0f) 2)
                                                       (ash b2 -6)))
                         #\=))
               (setf (char result (+ j 3))
                     (if (> remaining 2)
                         (char *base64-chars* (logand b2 #x3f))
                         #\=))))
    result))

(defun base64-decode (string)
  "Decode base64 string to byte vector."
  (let* ((len (length string))
         ;; Remove padding for length calculation
         (padding (cond ((and (> len 0) (char= (char string (1- len)) #\=))
                         (if (and (> len 1) (char= (char string (- len 2)) #\=))
                             2 1))
                        (t 0)))
         (output-len (- (* 3 (/ len 4)) padding))
         (result (make-array output-len :element-type '(unsigned-byte 8))))
    (loop for i from 0 below len by 4
          for j from 0 by 3
          do (let* ((c0 (aref *base64-decode-table* (char-code (char string i))))
                    (c1 (aref *base64-decode-table* (char-code (char string (1+ i)))))
                    (c2 (aref *base64-decode-table* (char-code (char string (+ i 2)))))
                    (c3 (aref *base64-decode-table* (char-code (char string (+ i 3))))))
               (when (< j output-len)
                 (setf (aref result j) (logior (ash c0 2) (ash c1 -4))))
               (when (< (1+ j) output-len)
                 (setf (aref result (1+ j)) (logior (ash (logand c1 #x0f) 4)
                                                     (ash c2 -2))))
               (when (< (+ j 2) output-len)
                 (setf (aref result (+ j 2)) (logior (ash (logand c2 #x03) 6)
                                                      c3)))))
    result))

;;;; ========================================================================
;;;; gRPC-Web Message Framing
;;;; ========================================================================

(defun grpc-web-frame-message (message &key (trailer-p nil))
  "Frame a message for gRPC-Web.
Returns byte vector with 5-byte header + message."
  (let* ((msg-bytes (if (typep message '(vector (unsigned-byte 8)))
                        message
                        (babel:string-to-octets message :encoding :utf-8)))
         (len (length msg-bytes))
         (frame (make-array (+ 5 len) :element-type '(unsigned-byte 8))))
    ;; Frame flag
    (setf (aref frame 0) (if trailer-p +grpc-web-trailer-frame+ +grpc-web-data-frame+))
    ;; Length (big-endian)
    (setf (aref frame 1) (ldb (byte 8 24) len))
    (setf (aref frame 2) (ldb (byte 8 16) len))
    (setf (aref frame 3) (ldb (byte 8 8) len))
    (setf (aref frame 4) (ldb (byte 8 0) len))
    ;; Message
    (replace frame msg-bytes :start1 5)
    frame))

(defun grpc-web-parse-frame (data &optional (offset 0))
  "Parse a gRPC-Web frame from DATA starting at OFFSET.
Returns (values message trailer-p next-offset) or NIL if incomplete."
  (let ((remaining (- (length data) offset)))
    (when (< remaining 5)
      (return-from grpc-web-parse-frame nil))
    (let* ((flags (aref data offset))
           (len (logior (ash (aref data (+ offset 1)) 24)
                        (ash (aref data (+ offset 2)) 16)
                        (ash (aref data (+ offset 3)) 8)
                        (aref data (+ offset 4))))
           (trailer-p (plusp (logand flags +grpc-web-trailer-frame+))))
      (when (< remaining (+ 5 len))
        (return-from grpc-web-parse-frame nil))
      (let ((message (make-array len :element-type '(unsigned-byte 8))))
        (replace message data :start2 (+ offset 5) :end2 (+ offset 5 len))
        (values message trailer-p (+ offset 5 len))))))

;;;; ========================================================================
;;;; gRPC-Web Trailer Encoding
;;;; ========================================================================

(defun grpc-web-encode-trailers (status message &optional metadata)
  "Encode trailers as gRPC-Web trailer frame.
Returns byte vector ready to send."
  (let ((trailer-string
          (with-output-to-string (s)
            (format s "grpc-status: ~D~C~C" status #\Return #\Newline)
            (when message
              (format s "grpc-message: ~A~C~C" message #\Return #\Newline))
            (when metadata
              (maphash (lambda (k v)
                         (format s "~A: ~A~C~C" k v #\Return #\Newline))
                       metadata)))))
    (grpc-web-frame-message trailer-string :trailer-p t)))

(defun grpc-web-parse-trailers (data)
  "Parse trailers from a gRPC-Web trailer frame.
Returns (values status message metadata)."
  (let ((text (babel:octets-to-string data :encoding :utf-8))
        (status 0)
        (message nil)
        (metadata (make-hash-table :test 'equal)))
    (dolist (line (split-lines text))
      (let ((colon-pos (position #\: line)))
        (when colon-pos
          (let ((key (string-trim '(#\Space #\Tab) (subseq line 0 colon-pos)))
                (value (string-trim '(#\Space #\Tab) (subseq line (1+ colon-pos)))))
            (cond
              ((string-equal key "grpc-status")
               (setf status (parse-integer value :junk-allowed t)))
              ((string-equal key "grpc-message")
               (setf message value))
              (t
               (setf (gethash key metadata) value)))))))
    (values status message metadata)))

(defun split-lines (string)
  "Split string by newlines."
  (loop with start = 0
        for end = (position #\Newline string :start start)
        collect (string-trim '(#\Return) (subseq string start (or end (length string))))
        while end
        do (setf start (1+ end))))

;;;; ========================================================================
;;;; gRPC-Web Server Handler
;;;; ========================================================================

(defclass grpc-web-handler ()
  ((server :initarg :server :accessor web-handler-server
           :documentation "The underlying gRPC server")
   (allow-origins :initarg :allow-origins :accessor web-handler-allow-origins
                  :initform '("*")
                  :documentation "Allowed CORS origins")
   (expose-headers :initarg :expose-headers :accessor web-handler-expose-headers
                   :initform '("grpc-status" "grpc-message")
                   :documentation "Headers to expose in CORS"))
  (:documentation "HTTP handler that translates gRPC-Web to gRPC."))

(defun make-grpc-web-handler (server &key (allow-origins '("*"))
                                          (expose-headers '("grpc-status" "grpc-message")))
  "Create a gRPC-Web handler wrapping a gRPC server.
ALLOW-ORIGINS: List of allowed CORS origins (default: allow all)
EXPOSE-HEADERS: Headers exposed to browser (default: grpc-status, grpc-message)"
  (make-instance 'grpc-web-handler
                 :server server
                 :allow-origins allow-origins
                 :expose-headers expose-headers))

(defun grpc-web-request-p (content-type)
  "Check if content-type indicates a gRPC-Web request."
  (and content-type
       (or (search "grpc-web" content-type :test #'char-equal))))

(defun grpc-web-text-request-p (content-type)
  "Check if content-type indicates a base64 gRPC-Web request."
  (and content-type
       (search "grpc-web-text" content-type :test #'char-equal)))

(defun grpc-web-cors-headers (handler origin)
  "Generate CORS headers for gRPC-Web response."
  (let ((allowed-origin
          (if (member "*" (web-handler-allow-origins handler) :test #'string=)
              "*"
              (if (member origin (web-handler-allow-origins handler) :test #'string=)
                  origin
                  nil))))
    (when allowed-origin
      (list (cons "access-control-allow-origin" allowed-origin)
            (cons "access-control-allow-methods" "POST, OPTIONS")
            (cons "access-control-allow-headers" "content-type, x-grpc-web, x-user-agent")
            (cons "access-control-expose-headers"
                  (format nil "~{~A~^, ~}" (web-handler-expose-headers handler)))))))

;;;; ========================================================================
;;;; gRPC-Web Request Processing
;;;; ========================================================================

(defun grpc-web-process-request (handler method body content-type headers)
  "Process a gRPC-Web request and return (values response-body response-headers status-code).
HANDLER: gRPC-Web handler
METHOD: RPC method path (e.g., \"/package.Service/Method\")
BODY: Request body (raw bytes or base64 string)
CONTENT-TYPE: Request content-type
HEADERS: Alist of request headers"
  (let* ((text-mode-p (grpc-web-text-request-p content-type))
         (request-data (if text-mode-p
                           (base64-decode body)
                           body))
         (origin (cdr (assoc "origin" headers :test #'string-equal)))
         (cors-headers (grpc-web-cors-headers handler origin)))
    ;; Parse the gRPC-Web frame
    (multiple-value-bind (message trailer-p next-offset)
        (grpc-web-parse-frame request-data)
      (declare (ignore trailer-p next-offset))
      (unless message
        (return-from grpc-web-process-request
          (values nil
                  (append cors-headers '(("content-type" . "text/plain")))
                  400)))
      ;; Dispatch to the gRPC server
      (multiple-value-bind (response-message grpc-status grpc-message)
          (grpc-web-dispatch-rpc handler method message headers)
        ;; Build response
        (let* ((response-frame (when response-message
                                 (grpc-web-frame-message response-message)))
               (trailer-frame (grpc-web-encode-trailers
                               (or grpc-status 0)
                               grpc-message))
               (full-response (if response-frame
                                  (concatenate '(vector (unsigned-byte 8))
                                               response-frame
                                               trailer-frame)
                                  trailer-frame))
               (response-body (if text-mode-p
                                  (base64-encode full-response)
                                  full-response))
               (response-content-type (if text-mode-p
                                          +grpc-web-text-content-type+
                                          +grpc-web-content-type+)))
          (values response-body
                  (append cors-headers
                          (list (cons "content-type" response-content-type)))
                  200))))))

(defun grpc-web-dispatch-rpc (handler method message headers)
  "Dispatch an RPC call through the gRPC server.
Returns (values response-bytes grpc-status grpc-message)."
  (declare (ignore handler headers))
  ;; This is a simplified dispatch - in a full implementation,
  ;; this would use the server's handler dispatch mechanism
  (declare (ignore method message))
  (values nil +grpc-status-unimplemented+ "gRPC-Web dispatch not implemented"))

;;;; ========================================================================
;;;; gRPC-Web Server Integration
;;;; ========================================================================

(defun server-enable-grpc-web (server &key (allow-origins '("*"))
                                           (expose-headers '("grpc-status" "grpc-message")))
  "Enable gRPC-Web support on a gRPC server.
Returns the gRPC-Web handler for use with HTTP servers.

Example with a typical HTTP server:
  (let ((web-handler (server-enable-grpc-web grpc-server)))
    (http-server:add-handler \"/api/*\"
      (lambda (request)
        (grpc-web-handle-http-request web-handler request))))"
  (make-grpc-web-handler server
                         :allow-origins allow-origins
                         :expose-headers expose-headers))

;;;; ========================================================================
;;;; gRPC-Web Client Support
;;;; ========================================================================

(defclass grpc-web-channel ()
  ((endpoint :initarg :endpoint :accessor web-channel-endpoint
             :documentation "HTTP endpoint URL")
   (text-mode :initarg :text-mode :accessor web-channel-text-mode
              :initform nil
              :documentation "Use base64 text mode")
   (headers :initarg :headers :accessor web-channel-headers
            :initform nil
            :documentation "Additional HTTP headers"))
  (:documentation "A gRPC-Web channel for making calls over HTTP/1.1."))

(defun make-grpc-web-channel (endpoint &key text-mode headers)
  "Create a gRPC-Web channel.
ENDPOINT: HTTP endpoint URL (e.g., \"http://localhost:8080\")
TEXT-MODE: Use base64 encoding (required for some proxies)
HEADERS: Additional HTTP headers"
  (make-instance 'grpc-web-channel
                 :endpoint endpoint
                 :text-mode text-mode
                 :headers headers))

(defun grpc-web-call-unary (channel method request-bytes &key metadata)
  "Make a unary gRPC-Web call.
Returns (values response-bytes grpc-status grpc-message trailing-metadata)."
  (let* ((text-mode (web-channel-text-mode channel))
         (frame (grpc-web-frame-message request-bytes))
         (body (if text-mode (base64-encode frame) frame))
         (content-type (if text-mode
                           +grpc-web-text-content-type+
                           +grpc-web-content-type+))
         (url (concatenate 'string (web-channel-endpoint channel) method))
         (headers (append (list (cons "content-type" content-type)
                                (cons "x-grpc-web" "1"))
                          (web-channel-headers channel)
                          (when metadata
                            (loop for (k . v) in metadata
                                  collect (cons k v))))))
    ;; Note: Actual HTTP request would use an HTTP client library
    ;; This is the interface - implementation depends on available HTTP client
    (values nil +grpc-status-unimplemented+
            "HTTP client not configured"
            (list url headers body))))

;;;; ========================================================================
;;;; Content-Type Detection
;;;; ========================================================================

(defun grpc-web-detect-format (content-type)
  "Detect gRPC-Web format from content-type.
Returns :BINARY, :TEXT, or NIL."
  (cond
    ((null content-type) nil)
    ((search "grpc-web-text" content-type :test #'char-equal) :text)
    ((search "grpc-web" content-type :test #'char-equal) :binary)
    (t nil)))

