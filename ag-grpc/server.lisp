;;;; server.lisp - gRPC server implementation

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; gRPC Server Class
;;;; ========================================================================

(defclass grpc-server ()
  ((host :initarg :host :accessor server-host :initform "0.0.0.0"
         :documentation "Host address to bind to")
   (port :initarg :port :accessor server-port
         :documentation "Port to listen on")
   (socket :initform nil :accessor server-socket
           :documentation "Listening socket")
   (handlers :initform (make-hash-table :test #'equal) :accessor server-handlers
             :documentation "Map of method path -> grpc-handler")
   (state :initform :stopped :accessor server-state
          :documentation "Server state: :stopped, :running, :shutting-down")
   (connections :initform nil :accessor server-connections
                :documentation "List of active connections")
   (max-concurrent-streams :initarg :max-concurrent-streams
                           :accessor server-max-concurrent-streams
                           :initform 100
                           :documentation "Maximum concurrent streams per connection")
   (tls :initarg :tls :accessor server-tls :initform nil
        :documentation "Use TLS encryption")
   (tls-certificate :initarg :tls-certificate :accessor server-tls-certificate
                    :initform nil
                    :documentation "Path to TLS certificate file")
   (tls-key :initarg :tls-key :accessor server-tls-key
            :initform nil
            :documentation "Path to TLS private key file")
   (interceptors :initform nil :accessor server-interceptors
                 :documentation "List of server interceptors"))
  (:documentation "gRPC server"))

(defun make-grpc-server (port &key (host "0.0.0.0") tls tls-certificate tls-key
                                   (max-concurrent-streams 100))
  "Create a new gRPC server.
PORT - Port to listen on
HOST - Host address to bind to (default \"0.0.0.0\")
TLS - Enable TLS encryption
TLS-CERTIFICATE - Path to TLS certificate file
TLS-KEY - Path to TLS private key file"
  (when (and tls (not (and tls-certificate tls-key)))
    (error "TLS requires both :tls-certificate and :tls-key"))
  (make-instance 'grpc-server
                 :host host
                 :port port
                 :tls tls
                 :tls-certificate tls-certificate
                 :tls-key tls-key
                 :max-concurrent-streams max-concurrent-streams))

(defun server-add-interceptor (server interceptor)
  "Add an interceptor to the server's interceptor chain.
Interceptors are called in the order they are added."
  (setf (server-interceptors server)
        (append (server-interceptors server) (list interceptor)))
  server)

;;;; ========================================================================
;;;; Handler Registration
;;;; ========================================================================

(defclass grpc-handler ()
  ((method-path :initarg :method :reader handler-method-path
                :documentation "Full method path (e.g., /pkg.Service/Method)")
   (function :initarg :function :reader handler-function
             :documentation "Handler function to call")
   (request-type :initarg :request-type :reader handler-request-type
                 :documentation "Request message type symbol")
   (response-type :initarg :response-type :reader handler-response-type
                  :documentation "Response message type symbol")
   (client-streaming-p :initarg :client-streaming :reader handler-client-streaming-p
                       :initform nil
                       :documentation "T if client streams requests")
   (server-streaming-p :initarg :server-streaming :reader handler-server-streaming-p
                       :initform nil
                       :documentation "T if server streams responses"))
  (:documentation "Registered RPC handler"))

(defun server-register-handler (server method-path handler-fn
                                 &key request-type response-type
                                      client-streaming server-streaming)
  "Register an RPC handler with the server.
METHOD-PATH - Full path like \"/package.Service/Method\"
HANDLER-FN - Function to handle requests
REQUEST-TYPE - Symbol for request message type
RESPONSE-TYPE - Symbol for response message type
CLIENT-STREAMING - T if client sends multiple messages
SERVER-STREAMING - T if server sends multiple messages"
  (let ((handler (make-instance 'grpc-handler
                                :method method-path
                                :function handler-fn
                                :request-type request-type
                                :response-type response-type
                                :client-streaming client-streaming
                                :server-streaming server-streaming)))
    (setf (gethash method-path (server-handlers server)) handler)))

(defun server-get-handler (server method-path)
  "Look up a handler by method path"
  (gethash method-path (server-handlers server)))

;;;; ========================================================================
;;;; Call Context
;;;; ========================================================================

(defclass grpc-call-context ()
  ((connection :initarg :connection :reader context-connection
               :documentation "HTTP/2 connection")
   (stream-id :initarg :stream-id :reader context-stream-id
              :documentation "HTTP/2 stream ID")
   (method-path :initarg :method :reader context-method-path
                :documentation "RPC method path")
   (request-headers :initarg :headers :reader context-request-headers
                    :documentation "Raw request headers")
   (request-metadata :initform nil :accessor context-request-metadata
                     :documentation "Decoded request metadata")
   (peer-address :initarg :peer-address :reader context-peer-address
                 :documentation "Client address (host:port)")
   (deadline :initform nil :accessor context-deadline
             :documentation "Request deadline")
   (cancelled-p :initform nil :accessor context-cancelled-p
                :documentation "T if request was cancelled")
   (response-headers-sent-p :initform nil :accessor context-response-headers-sent-p
                            :documentation "T if response headers were sent")
   (response-metadata :initform nil :accessor context-response-metadata
                      :documentation "Response metadata to send")
   (trailing-metadata :initform nil :accessor context-trailing-metadata
                      :documentation "Trailing metadata to send")
   (request-encoding :initform nil :accessor context-request-encoding
                     :documentation "Compression encoding used by client (from grpc-encoding header)")
   (response-encoding :initform nil :accessor context-response-encoding
                      :documentation "Compression encoding to use for responses (negotiated from grpc-accept-encoding)"))
  (:documentation "Context for an RPC call, passed to handlers"))

(defun context-metadata (ctx &optional key)
  "Get request metadata. If KEY is provided, return just that value."
  (unless (context-request-metadata ctx)
    (setf (context-request-metadata ctx)
          (alist-to-metadata (decode-metadata-headers
                              (context-request-headers ctx)))))
  (if key
      (metadata-get (context-request-metadata ctx) key)
      (context-request-metadata ctx)))

(defun context-set-response-metadata (ctx metadata)
  "Set response metadata to be sent with headers"
  (setf (context-response-metadata ctx) metadata))

(defun context-set-trailing-metadata (ctx metadata)
  "Set trailing metadata to be sent with trailers"
  (setf (context-trailing-metadata ctx) metadata))

(defun context-check-cancelled (ctx)
  "Check if the client has cancelled this RPC.
Updates and returns context-cancelled-p. Handlers can call this periodically
during long operations to detect client cancellation early."
  (unless (context-cancelled-p ctx)
    (let* ((stream-id (context-stream-id ctx))
           (h2-stream (ag-http2:multiplexer-get-stream
                       (ag-http2:connection-multiplexer (context-connection ctx))
                       stream-id)))
      (when (and h2-stream (ag-http2:stream-rst-stream-error h2-stream))
        (setf (context-cancelled-p ctx) t))))
  (context-cancelled-p ctx))

;;;; ========================================================================
;;;; Server Call Stream (for streaming RPCs)
;;;; ========================================================================

(defclass grpc-server-call-stream ()
  ((context :initarg :context :reader server-stream-context
            :documentation "Call context")
   (connection :initarg :connection :reader server-stream-connection
               :documentation "HTTP/2 connection")
   (stream-id :initarg :stream-id :reader server-stream-id
              :documentation "HTTP/2 stream ID")
   (request-type :initarg :request-type :reader server-stream-request-type
                 :documentation "Request message type")
   (response-type :initarg :response-type :reader server-stream-response-type
                  :documentation "Response message type")
   (recv-buffer :initform (make-array 0 :element-type '(unsigned-byte 8)
                                        :adjustable t :fill-pointer 0)
                :accessor server-stream-recv-buffer
                :documentation "Buffer for incoming messages")
   (send-closed-p :initform nil :accessor server-stream-send-closed-p
                  :documentation "T when send side is closed")
   (recv-closed-p :initform nil :accessor server-stream-recv-closed-p
                  :documentation "T when receive side is closed"))
  (:documentation "Stream object for streaming RPCs on server side"))

;;;; ========================================================================
;;;; Server Lifecycle
;;;; ========================================================================

(defun server-start (server)
  "Start the gRPC server. This function blocks while handling connections.
Use server-stop from another thread to shut down."
  (when (eq (server-state server) :running)
    (error "Server is already running"))
  ;; Create listening socket
  (setf (server-socket server)
        (usocket:socket-listen (server-host server)
                               (server-port server)
                               :reuse-address t
                               :element-type '(unsigned-byte 8)))
  (setf (server-state server) :running)
  (unwind-protect
       (server-accept-loop server)
    ;; Cleanup on exit
    (setf (server-state server) :stopped)
    (when (server-socket server)
      (usocket:socket-close (server-socket server))
      (setf (server-socket server) nil))))

(defun server-stop (server &key graceful)
  "Stop the gRPC server.
If GRACEFUL is true, wait for active connections to finish."
  (declare (ignore graceful))  ; TODO: Implement graceful shutdown
  (setf (server-state server) :shutting-down)
  ;; Close listening socket to break accept loop
  (when (server-socket server)
    (usocket:socket-close (server-socket server))
    (setf (server-socket server) nil))
  ;; Close all active connections
  (dolist (conn (server-connections server))
    (ignore-errors
      (ag-http2:connection-close conn)))
  (setf (server-connections server) nil)
  (setf (server-state server) :stopped))

(defun server-accept-loop (server)
  "Accept and handle incoming connections"
  (loop while (eq (server-state server) :running)
        do (handler-case
               (let ((client-socket (usocket:socket-accept (server-socket server))))
                 (when client-socket
                   (handler-case
                       (server-handle-connection server client-socket)
                     (error (e)
                       (format *error-output* "Connection error: ~A~%" e)
                       (ignore-errors
                         (usocket:socket-close client-socket))))))
             (usocket:socket-error (e)
               ;; Socket closed during shutdown
               (declare (ignore e))
               (return)))))

(defun server-handle-connection (server client-socket)
  "Handle a single client connection"
  (let* ((conn (ag-http2:make-server-connection client-socket
                                                 :tls (server-tls server)
                                                 :certificate (server-tls-certificate server)
                                                 :key (server-tls-key server)))
         (peer-addr (format nil "~A:~A"
                            (usocket:get-peer-address client-socket)
                            (usocket:get-peer-port client-socket))))
    ;; Perform HTTP/2 handshake
    (ag-http2:server-connection-handshake conn)
    ;; Track connection
    (push conn (server-connections server))
    (unwind-protect
         (server-connection-loop server conn peer-addr)
      ;; Cleanup
      (setf (server-connections server)
            (remove conn (server-connections server)))
      (ignore-errors
        (ag-http2:connection-close conn)))))

(defun server-connection-loop (server conn peer-addr)
  "Process frames for a connection until closed"
  (loop while (eq (ag-http2:connection-state conn) :open)
        do (handler-case
               (let ((frame (ag-http2:connection-read-frame conn)))
                 (when frame
                   (server-process-frame server conn frame peer-addr)))
             (ag-http2:http2-connection-error (e)
               (declare (ignore e))
               (return))
             (end-of-file ()
               (return))
             (error (e)
               (format *error-output* "Frame processing error: ~A~%" e)
               (return)))))

;;;; ========================================================================
;;;; Frame Processing
;;;; ========================================================================

(defun server-process-frame (server conn frame peer-addr)
  "Process an incoming frame and dispatch to appropriate handler"
  (typecase frame
    (ag-http2:headers-frame
     (server-handle-headers server conn frame peer-addr))
    (ag-http2:data-frame
     (server-handle-data server conn frame))
    ;; Other frame types are handled by connection-read-frame
    (t nil)))

(defun server-handle-headers (server conn frame peer-addr)
  "Handle incoming HEADERS frame (new RPC request)"
  (let* ((stream-id (ag-http2:frame-stream-id frame))
         (h2-stream (ag-http2:multiplexer-get-stream
                     (ag-http2:connection-multiplexer conn)
                     stream-id))
         (headers (ag-http2:stream-headers h2-stream))
         (method-path (cdr (assoc :path headers)))
         (handler (server-get-handler server method-path)))
    (unless handler
      ;; No handler registered - send UNIMPLEMENTED
      (server-send-error conn stream-id +grpc-status-unimplemented+
                         (format nil "Method not found: ~A" method-path))
      (return-from server-handle-headers))
    ;; Create call context
    (let ((ctx (make-instance 'grpc-call-context
                              :connection conn
                              :stream-id stream-id
                              :method method-path
                              :headers headers
                              :peer-address peer-addr)))
      ;; Parse timeout if present
      (let ((timeout-header (cdr (assoc "grpc-timeout" headers :test #'string-equal))))
        (when timeout-header
          (setf (context-deadline ctx)
                (+ (get-universal-time) (parse-grpc-timeout timeout-header)))))
      ;; Extract compression encoding from client request
      (let ((request-encoding (cdr (assoc "grpc-encoding" headers :test #'string-equal))))
        (when (and request-encoding (not (string-equal request-encoding "identity")))
          (setf (context-request-encoding ctx) request-encoding)))
      ;; Negotiate response compression based on client's accept-encoding
      (let ((accept-encoding (cdr (assoc "grpc-accept-encoding" headers :test #'string-equal))))
        (when accept-encoding
          ;; Check if client accepts gzip
          (when (search "gzip" accept-encoding :test #'char-equal)
            (setf (context-response-encoding ctx) "gzip"))))
      ;; Store context for DATA frame handling
      (setf (stream-call-context h2-stream) ctx)
      (setf (stream-handler h2-stream) handler)
      ;; If END_STREAM is set, this is a unary call with no body
      ;; (unusual but valid for requests with no data)
      (when (plusp (logand (ag-http2:frame-flags frame) ag-http2:+flag-end-stream+))
        (server-dispatch-handler server conn ctx handler nil)))))

(defun server-handle-data (server conn frame)
  "Handle incoming DATA frame (request body)"
  (let* ((stream-id (ag-http2:frame-stream-id frame))
         (h2-stream (ag-http2:multiplexer-get-stream
                     (ag-http2:connection-multiplexer conn)
                     stream-id))
         (ctx (stream-call-context h2-stream))
         (handler (stream-handler h2-stream)))
    (unless (and ctx handler)
      (return-from server-handle-data))
    ;; Append data to stream buffer
    (let ((data (ag-http2:frame-payload frame))
          (buffer (ag-http2:stream-data-buffer h2-stream)))
      (loop for byte across data
            do (vector-push-extend byte buffer)))
    ;; If END_STREAM, process the complete request
    (when (plusp (logand (ag-http2:frame-flags frame) ag-http2:+flag-end-stream+))
      (let ((buffer (ag-http2:stream-data-buffer h2-stream)))
        (server-dispatch-handler server conn ctx handler buffer)))))

;;;; ========================================================================
;;;; Handler Dispatch
;;;; ========================================================================

(defun server-dispatch-handler (server conn ctx handler request-data)
  "Dispatch to the appropriate handler based on streaming type"
  (let* ((client-streaming (handler-client-streaming-p handler))
         (server-streaming (handler-server-streaming-p handler))
         (handler-type (cond
                         ((and (not client-streaming) (not server-streaming)) :unary)
                         ((and (not client-streaming) server-streaming) :server-streaming)
                         ((and client-streaming (not server-streaming)) :client-streaming)
                         (t :bidi-streaming)))
         (interceptors (server-interceptors server))
         (handler-info (list :method-path (context-method-path ctx)
                             :handler-type handler-type))
         (call-contexts nil)
         (response nil)
         (error-occurred nil))
    ;; Run pre-handler interceptors
    (when interceptors
      (setf call-contexts (run-interceptors-call-start interceptors ctx handler-info)))
    ;; Dispatch to handler
    (handler-case
        (setf response
              (case handler-type
                (:unary
                 (server-handle-unary conn ctx handler request-data interceptors))
                (:server-streaming
                 (server-handle-server-streaming conn ctx handler request-data interceptors))
                (:client-streaming
                 (server-handle-client-streaming conn ctx handler request-data interceptors))
                (:bidi-streaming
                 (server-handle-bidi-streaming conn ctx handler request-data interceptors))))
      (error (e)
        (setf error-occurred e)))
    ;; Run post-handler interceptors
    (when interceptors
      (run-interceptors-call-end interceptors ctx handler-info
                                  call-contexts response error-occurred))))

;;;; ========================================================================
;;;; Unary RPC Handler
;;;; ========================================================================

(defun server-handle-unary (conn ctx handler request-data &optional interceptors)
  "Handle a unary RPC call"
  (handler-case
      (let* ((request-type (handler-request-type handler))
             (request-encoding (context-request-encoding ctx))
             (request (when (and request-data (> (length request-data) 0))
                        (multiple-value-bind (msg-data compressed consumed)
                            (decode-grpc-message request-data 0 request-encoding)
                          (declare (ignore compressed consumed))
                          (when msg-data
                            (ag-proto:deserialize-from-bytes request-type msg-data)))))
             ;; Run recv interceptors on request
             (processed-request (if interceptors
                                    (run-interceptors-recv-message interceptors ctx request)
                                    request))
             (handler-fn (handler-function handler))
             (response (funcall handler-fn processed-request ctx))
             ;; Run send interceptors on response
             (processed-response (if interceptors
                                     (run-interceptors-send-message interceptors ctx response)
                                     response)))
        ;; Send response
        (server-send-response conn ctx processed-response)
        processed-response)
    (grpc-status-error (e)
      (server-send-error conn (context-stream-id ctx)
                         (grpc-status-error-code e)
                         (grpc-status-error-message e))
      (error e))
    (error (e)
      (server-send-error conn (context-stream-id ctx)
                         +grpc-status-internal+
                         (format nil "Internal error: ~A" e))
      (error e))))

;;;; ========================================================================
;;;; Server Streaming RPC Handler
;;;; ========================================================================

(defun server-handle-server-streaming (conn ctx handler request-data &optional interceptors)
  "Handle a server streaming RPC call"
  (declare (ignore interceptors))  ; Message interception happens in stream-send
  (handler-case
      (let* ((request-type (handler-request-type handler))
             (response-type (handler-response-type handler))
             (request-encoding (context-request-encoding ctx))
             (request (when (and request-data (> (length request-data) 0))
                        (multiple-value-bind (msg-data compressed consumed)
                            (decode-grpc-message request-data 0 request-encoding)
                          (declare (ignore compressed consumed))
                          (when msg-data
                            (ag-proto:deserialize-from-bytes request-type msg-data)))))
             (handler-fn (handler-function handler))
             (stream (make-instance 'grpc-server-call-stream
                                    :context ctx
                                    :connection conn
                                    :stream-id (context-stream-id ctx)
                                    :request-type request-type
                                    :response-type response-type)))
        ;; Send response headers
        (server-send-headers conn ctx)
        ;; Call handler with stream for sending responses
        (funcall handler-fn request ctx stream)
        ;; Send trailers
        (server-send-trailers conn ctx +grpc-status-ok+))
    (grpc-status-error (e)
      (server-send-error conn (context-stream-id ctx)
                         (grpc-status-error-code e)
                         (grpc-status-error-message e))
      (error e))
    (error (e)
      (server-send-error conn (context-stream-id ctx)
                         +grpc-status-internal+
                         (format nil "Internal error: ~A" e))
      (error e))))

;;;; ========================================================================
;;;; Client Streaming RPC Handler
;;;; ========================================================================

(defun server-handle-client-streaming (conn ctx handler request-data &optional interceptors)
  "Handle a client streaming RPC call"
  (declare (ignore request-data interceptors))  ; Data comes via stream-recv
  (handler-case
      (let* ((request-type (handler-request-type handler))
             (response-type (handler-response-type handler))
             (handler-fn (handler-function handler))
             (stream (make-instance 'grpc-server-call-stream
                                    :context ctx
                                    :connection conn
                                    :stream-id (context-stream-id ctx)
                                    :request-type request-type
                                    :response-type response-type)))
        ;; Call handler with stream for receiving requests
        ;; Handler should call stream-recv to get messages
        (let ((response (funcall handler-fn ctx stream)))
          ;; Send response
          (server-send-response conn ctx response)
          response))
    (grpc-status-error (e)
      (server-send-error conn (context-stream-id ctx)
                         (grpc-status-error-code e)
                         (grpc-status-error-message e))
      (error e))
    (error (e)
      (server-send-error conn (context-stream-id ctx)
                         +grpc-status-internal+
                         (format nil "Internal error: ~A" e))
      (error e))))

;;;; ========================================================================
;;;; Bidirectional Streaming RPC Handler
;;;; ========================================================================

(defun server-handle-bidi-streaming (conn ctx handler request-data &optional interceptors)
  "Handle a bidirectional streaming RPC call"
  (declare (ignore request-data interceptors))
  (handler-case
      (let* ((request-type (handler-request-type handler))
             (response-type (handler-response-type handler))
             (handler-fn (handler-function handler))
             (stream (make-instance 'grpc-server-call-stream
                                    :context ctx
                                    :connection conn
                                    :stream-id (context-stream-id ctx)
                                    :request-type request-type
                                    :response-type response-type)))
        ;; Send response headers
        (server-send-headers conn ctx)
        ;; Call handler with stream for both sending and receiving
        (funcall handler-fn ctx stream)
        ;; Send trailers
        (server-send-trailers conn ctx +grpc-status-ok+))
    (grpc-status-error (e)
      (server-send-error conn (context-stream-id ctx)
                         (grpc-status-error-code e)
                         (grpc-status-error-message e))
      (error e))
    (error (e)
      (server-send-error conn (context-stream-id ctx)
                         +grpc-status-internal+
                         (format nil "Internal error: ~A" e))
      (error e))))

;;;; ========================================================================
;;;; Stream Operations (for handlers)
;;;; ========================================================================

(defmethod stream-send ((stream grpc-server-call-stream) message)
  "Send a message on a server stream"
  (when (server-stream-send-closed-p stream)
    (error "Cannot send on closed stream"))
  ;; Check if client cancelled
  (when (context-check-cancelled (server-stream-context stream))
    (error 'grpc-status-error
           :code +grpc-status-cancelled+
           :message "Client cancelled the RPC"))
  (let* ((conn (server-stream-connection stream))
         (stream-id (server-stream-id stream))
         (response-encoding (context-response-encoding (server-stream-context stream)))
         (message-bytes (ag-proto:serialize-to-bytes message))
         (frame-data (if (and response-encoding (string-equal response-encoding "gzip"))
                         (encode-grpc-message (compress-grpc-message message-bytes response-encoding) :compressed t)
                         (encode-grpc-message message-bytes))))
    (ag-http2:connection-send-data conn stream-id frame-data :end-stream nil))
  stream)

(defun stream-recv (stream)
  "Receive a message from a server stream (for client-streaming/bidi).
Returns the deserialized message, or NIL if no more messages."
  (when (server-stream-recv-closed-p stream)
    (return-from stream-recv nil))
  (let* ((conn (server-stream-connection stream))
         (stream-id (server-stream-id stream))
         (h2-stream (ag-http2:multiplexer-get-stream
                     (ag-http2:connection-multiplexer conn)
                     stream-id))
         (buffer (server-stream-recv-buffer stream))
         (request-encoding (context-request-encoding (server-stream-context stream))))
    ;; Try to decode from buffer first
    (multiple-value-bind (data compressed consumed)
        (decode-grpc-message buffer 0 request-encoding)
      (declare (ignore compressed))
      (when data
        ;; Got a message, remove consumed bytes from buffer
        (let ((remaining (subseq buffer consumed)))
          (setf (fill-pointer buffer) 0)
          (loop for byte across remaining
                do (vector-push-extend byte buffer)))
        (return-from stream-recv
          (ag-proto:deserialize-from-bytes
           (server-stream-request-type stream) data))))
    ;; Need more data - read frames
    (loop
      (unless (ag-http2:stream-can-recv-p h2-stream)
        (setf (server-stream-recv-closed-p stream) t)
        (return-from stream-recv nil))
      (ag-http2:connection-read-frame conn)
      ;; Check for client cancellation (RST_STREAM received)
      (when (ag-http2:stream-rst-stream-error h2-stream)
        (setf (context-cancelled-p (server-stream-context stream)) t)
        (setf (server-stream-recv-closed-p stream) t)
        (return-from stream-recv nil))
      ;; Copy new data to our buffer
      (let ((new-data (ag-http2:stream-consume-data h2-stream)))
        (loop for byte across new-data
              do (vector-push-extend byte buffer)))
      ;; Try to decode
      (multiple-value-bind (data compressed consumed)
          (decode-grpc-message buffer 0 request-encoding)
        (declare (ignore compressed))
        (when data
          (let ((remaining (subseq buffer consumed)))
            (setf (fill-pointer buffer) 0)
            (loop for byte across remaining
                  do (vector-push-extend byte buffer)))
          (return-from stream-recv
            (ag-proto:deserialize-from-bytes
             (server-stream-request-type stream) data)))))))

(defmacro do-stream-recv ((var stream &optional result) &body body)
  "Iterate over received messages from a stream.
VAR is bound to each message in turn.
Returns RESULT (default NIL) when no more messages."
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,stream))
       (loop for ,var = (stream-recv ,stream-var)
             while ,var
             do (progn ,@body)
             finally (return ,result)))))

;;;; ========================================================================
;;;; Response Sending
;;;; ========================================================================

(defun server-send-headers (conn ctx)
  "Send response headers"
  (unless (context-response-headers-sent-p ctx)
    (let ((headers (make-response-headers
                    :metadata (context-response-metadata ctx)
                    :encoding (context-response-encoding ctx))))
      (ag-http2:connection-send-headers conn (context-stream-id ctx)
                                        headers :end-stream nil)
      (setf (context-response-headers-sent-p ctx) t))))

(defun server-send-response (conn ctx response)
  "Send a complete response (headers + data + trailers)"
  ;; Send headers if not already sent
  (server-send-headers conn ctx)
  ;; Send response data
  (when response
    (let* ((response-encoding (context-response-encoding ctx))
           (response-bytes (ag-proto:serialize-to-bytes response))
           (frame-data (if (and response-encoding (string-equal response-encoding "gzip"))
                           (encode-grpc-message (compress-grpc-message response-bytes response-encoding) :compressed t)
                           (encode-grpc-message response-bytes))))
      (ag-http2:connection-send-data conn (context-stream-id ctx)
                                     frame-data :end-stream nil)))
  ;; Send trailers
  (server-send-trailers conn ctx +grpc-status-ok+))

(defun server-send-trailers (conn ctx status &optional message)
  "Send trailers with gRPC status"
  (let ((trailers (make-trailers status
                                 :message message
                                 :metadata (context-trailing-metadata ctx))))
    (ag-http2:connection-send-headers conn (context-stream-id ctx)
                                      trailers :end-stream t)))

(defun server-send-error (conn stream-id status &optional message)
  "Send an error response"
  (let ((headers (make-response-headers))
        (trailers (make-trailers status :message message)))
    ;; Send headers
    (ag-http2:connection-send-headers conn stream-id headers :end-stream nil)
    ;; Send trailers
    (ag-http2:connection-send-headers conn stream-id trailers :end-stream t)))

;;;; ========================================================================
;;;; Convenience Macros
;;;; ========================================================================

(defmacro with-grpc-server ((var port &rest options) &body body)
  "Execute BODY with VAR bound to a gRPC server.
The server is started and stopped automatically.

Example:
  (with-grpc-server (server 50051)
    (server-register-handler server \"/hello.Greeter/SayHello\"
                             #'handle-say-hello
                             :request-type 'hello-request
                             :response-type 'hello-reply)
    (server-start server))"
  `(let ((,var (make-grpc-server ,port ,@options)))
     (unwind-protect
          (progn ,@body)
       (when (eq (server-state ,var) :running)
         (server-stop ,var)))))

;;;; ========================================================================
;;;; HTTP/2 Stream Extensions for Server
;;;; ========================================================================

;;; Add slots to store context and handler on HTTP/2 streams
;;; These are accessed via methods below

(defvar *stream-contexts* (make-hash-table)
  "Map of stream -> call context")

(defvar *stream-handlers* (make-hash-table)
  "Map of stream -> handler")

(defun stream-call-context (stream)
  "Get the call context for a stream"
  (gethash stream *stream-contexts*))

(defun (setf stream-call-context) (value stream)
  "Set the call context for a stream"
  (setf (gethash stream *stream-contexts*) value))

(defun stream-handler (stream)
  "Get the handler for a stream"
  (gethash stream *stream-handlers*))

(defun (setf stream-handler) (value stream)
  "Set the handler for a stream"
  (setf (gethash stream *stream-handlers*) value))
