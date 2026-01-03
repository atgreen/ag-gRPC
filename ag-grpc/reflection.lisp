;;;; reflection.lisp - gRPC Server Reflection Protocol
;;;;
;;;; Implements the grpc.reflection.v1alpha.ServerReflection service
;;;; for runtime service discovery by tools like grpcurl.

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; Reflection Protocol Messages
;;;; ========================================================================

;;; Request message
(defclass server-reflection-request (ag-proto:proto-message)
  ((host :initarg :host :accessor reflection-request-host :initform "")
   ;; One of the following (oneof message_request):
   (file-by-filename :initarg :file-by-filename :accessor reflection-file-by-filename :initform nil)
   (file-containing-symbol :initarg :file-containing-symbol :accessor reflection-file-containing-symbol :initform nil)
   (file-containing-extension :initarg :file-containing-extension :accessor reflection-file-containing-extension :initform nil)
   (all-extension-numbers-of-type :initarg :all-extension-numbers-of-type :accessor reflection-all-extension-numbers :initform nil)
   (list-services :initarg :list-services :accessor reflection-list-services :initform nil))
  (:documentation "Server reflection request message."))

(defmethod ag-proto:deserialize-from-bytes ((type (eql 'server-reflection-request)) data)
  (let ((obj (make-instance 'server-reflection-request))
        (buffer (cons data 0)))
    (loop while (< (cdr buffer) (length data))
          do (let* ((tag (ag-proto::read-varint buffer))
                    (field-number (ash tag -3))
                    (wire-type (logand tag 7)))
               (declare (ignore wire-type))
               (case field-number
                 (1 (setf (reflection-request-host obj)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (3 (setf (reflection-file-by-filename obj)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (4 (setf (reflection-file-containing-symbol obj)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (7 (setf (reflection-list-services obj) t)
                    (ag-proto::read-length-delimited buffer))  ; empty string
                 (otherwise (ag-proto::skip-field buffer (logand tag 7))))))
    obj))

;;; Response messages
(defclass service-response (ag-proto:proto-message)
  ((name :initarg :name :accessor service-name :initform ""))
  (:documentation "Service name in list services response."))

(defmethod ag-proto:serialize-to-bytes ((obj service-response))
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (let ((value (service-name obj)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 1 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    buffer))

(defclass list-service-response (ag-proto:proto-message)
  ((services :initarg :services :accessor list-services-response :initform nil))
  (:documentation "List of services response."))

(defmethod ag-proto:serialize-to-bytes ((obj list-service-response))
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (dolist (svc (list-services-response obj))
      (ag-proto::write-field-tag 1 2 buffer)
      (ag-proto::write-length-delimited (ag-proto:serialize-to-bytes svc) buffer))
    buffer))

(defclass error-response (ag-proto:proto-message)
  ((error-code :initarg :error-code :accessor error-response-code :initform 0)
   (error-message :initarg :error-message :accessor error-response-message :initform ""))
  (:documentation "Error response message."))

(defmethod ag-proto:serialize-to-bytes ((obj error-response))
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (let ((code (error-response-code obj)))
      (when (and code (not (zerop code)))
        (ag-proto::write-field-tag 1 0 buffer)
        (ag-proto::write-varint code buffer)))
    (let ((msg (error-response-message obj)))
      (when (and msg (plusp (length msg)))
        (ag-proto::write-field-tag 2 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 msg) buffer)))
    buffer))

(defclass server-reflection-response (ag-proto:proto-message)
  ((valid-host :initarg :valid-host :accessor reflection-response-host :initform "")
   (original-request :initarg :original-request :accessor reflection-response-request :initform nil)
   ;; One of (oneof message_response):
   (list-services-response :initarg :list-services-response :accessor reflection-list-services-response :initform nil)
   (error-response :initarg :error-response :accessor reflection-error-response :initform nil))
  (:documentation "Server reflection response message."))

(defmethod ag-proto:serialize-to-bytes ((obj server-reflection-response))
  (let ((buffer (make-array 128 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    ;; Field 1: valid_host
    (let ((host (reflection-response-host obj)))
      (when (and host (plusp (length host)))
        (ag-proto::write-field-tag 1 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 host) buffer)))
    ;; Field 6: list_services_response
    (let ((list-resp (reflection-list-services-response obj)))
      (when list-resp
        (ag-proto::write-field-tag 6 2 buffer)
        (ag-proto::write-length-delimited (ag-proto:serialize-to-bytes list-resp) buffer)))
    ;; Field 7: error_response
    (let ((err-resp (reflection-error-response obj)))
      (when err-resp
        (ag-proto::write-field-tag 7 2 buffer)
        (ag-proto::write-length-delimited (ag-proto:serialize-to-bytes err-resp) buffer)))
    buffer))

;;;; ========================================================================
;;;; Reflection Service Implementation
;;;; ========================================================================

(defclass reflection-service ()
  ((server :initarg :server :accessor reflection-server
           :documentation "The gRPC server to reflect"))
  (:documentation "Server reflection service implementation."))

(defun make-reflection-service (server)
  "Create a reflection service for the given server."
  (make-instance 'reflection-service :server server))

(defun get-registered-services (server)
  "Get list of registered service names from a gRPC server."
  (let ((services (make-hash-table :test 'equal)))
    (maphash (lambda (path handler)
               (declare (ignore handler))
               ;; Extract service name from path like "/package.Service/Method"
               (let* ((trimmed (string-left-trim "/" path))
                      (slash-pos (position #\/ trimmed)))
                 (when slash-pos
                   (setf (gethash (subseq trimmed 0 slash-pos) services) t))))
             (server-handlers server))
    (loop for service being the hash-keys of services collect service)))

(defun make-reflection-handler (reflection-service)
  "Create the ServerReflectionInfo bidirectional streaming handler."
  (lambda (ctx stream)
    (let ((server (reflection-server reflection-service)))
      (ag-grpc:do-stream-recv (request stream)
        (let ((response (handle-reflection-request server request)))
          (stream-send stream response))))))

(defun handle-reflection-request (server request)
  "Handle a single reflection request and return a response."
  (cond
    ;; List services
    ((reflection-list-services request)
     (let* ((service-names (get-registered-services server))
            (services (mapcar (lambda (name)
                                (make-instance 'service-response :name name))
                              service-names))
            (list-response (make-instance 'list-service-response :services services)))
       (make-instance 'server-reflection-response
                      :valid-host (reflection-request-host request)
                      :list-services-response list-response)))
    ;; File by filename - not supported
    ((reflection-file-by-filename request)
     (make-instance 'server-reflection-response
                    :valid-host (reflection-request-host request)
                    :error-response (make-instance 'error-response
                                                   :error-code +grpc-status-unimplemented+
                                                   :error-message "File descriptor lookup not implemented")))
    ;; File containing symbol - not supported
    ((reflection-file-containing-symbol request)
     (make-instance 'server-reflection-response
                    :valid-host (reflection-request-host request)
                    :error-response (make-instance 'error-response
                                                   :error-code +grpc-status-unimplemented+
                                                   :error-message "Symbol lookup not implemented")))
    ;; Default: unknown request type
    (t
     (make-instance 'server-reflection-response
                    :valid-host (reflection-request-host request)
                    :error-response (make-instance 'error-response
                                                   :error-code +grpc-status-unimplemented+
                                                   :error-message "Unknown reflection request type")))))

;;;; ========================================================================
;;;; Server Integration
;;;; ========================================================================

(defun server-enable-reflection (server)
  "Enable the gRPC server reflection service on this server.
Returns the reflection-service instance."
  (let ((reflection (make-reflection-service server)))
    ;; Register the bidirectional streaming handler
    (server-register-handler server
                             "/grpc.reflection.v1alpha.ServerReflection/ServerReflectionInfo"
                             (make-reflection-handler reflection)
                             :bidi-streaming
                             'server-reflection-request)
    reflection))
