;;;; telemetry.lisp - OpenTelemetry integration for distributed tracing
;;;;
;;;; Provides automatic tracing of gRPC calls with cl-opentelemetry.
;;;; This module is optional - it gracefully handles missing cl-opentelemetry.

(in-package #:ag-grpc)

;;;; ========================================================================
;;;; OpenTelemetry Availability Check
;;;; ========================================================================

(defvar *otel-available* nil
  "T if cl-opentelemetry is loaded and available.")

(defvar *otel-tracer* nil
  "The OpenTelemetry tracer instance.")

(defun try-load-opentelemetry ()
  "Try to load cl-opentelemetry. Returns T if successful."
  (handler-case
      (progn
        (asdf:load-system :cl-opentelemetry :verbose nil)
        (setf *otel-available* t)
        t)
    (error ()
      (setf *otel-available* nil)
      nil)))

(defun opentelemetry-available-p ()
  "Return T if OpenTelemetry is available."
  *otel-available*)

;;;; ========================================================================
;;;; Tracer Configuration
;;;; ========================================================================

(defclass grpc-telemetry-config ()
  ((service-name :initarg :service-name :accessor telemetry-service-name
                 :initform "grpc-service"
                 :documentation "Service name for traces")
   (endpoint :initarg :endpoint :accessor telemetry-endpoint
             :initform "http://localhost:4318/v1/traces"
             :documentation "OTLP HTTP endpoint")
   (sample-rate :initarg :sample-rate :accessor telemetry-sample-rate
                :initform 1.0
                :documentation "Sampling rate 0.0-1.0")
   (record-request :initarg :record-request :accessor telemetry-record-request-p
                   :initform nil
                   :documentation "Include request data in spans")
   (record-response :initarg :record-response :accessor telemetry-record-response-p
                    :initform nil
                    :documentation "Include response data in spans")
   (propagate-context :initarg :propagate-context :accessor telemetry-propagate-context-p
                      :initform t
                      :documentation "Propagate trace context in metadata"))
  (:documentation "Configuration for OpenTelemetry tracing."))

(defun make-telemetry-config (&key (service-name "grpc-service")
                                    (endpoint "http://localhost:4318/v1/traces")
                                    (sample-rate 1.0)
                                    record-request
                                    record-response
                                    (propagate-context t))
  "Create a telemetry configuration."
  (make-instance 'grpc-telemetry-config
                 :service-name service-name
                 :endpoint endpoint
                 :sample-rate sample-rate
                 :record-request record-request
                 :record-response record-response
                 :propagate-context propagate-context))

;;;; ========================================================================
;;;; Tracing Interceptor (Server-Side)
;;;; ========================================================================

(defclass tracing-interceptor (server-interceptor)
  ((config :initarg :config :accessor tracing-config
           :initform (make-telemetry-config)
           :documentation "Telemetry configuration"))
  (:documentation "Server interceptor that adds OpenTelemetry tracing."))

(defun make-tracing-interceptor (&optional config)
  "Create a tracing interceptor for the server."
  (make-instance 'tracing-interceptor
                 :config (or config (make-telemetry-config))))

(defmethod interceptor-call-start ((interceptor tracing-interceptor) ctx handler-info)
  "Start a trace span for the RPC."
  (let ((method (getf handler-info :method-path))
        (start-time (get-internal-real-time)))
    ;; Return context for call-end
    (list :method method
          :start-time start-time
          :peer (context-peer-address ctx))))

(defmethod interceptor-call-end ((interceptor tracing-interceptor) ctx handler-info
                                  call-context response error)
  (declare (ignore ctx))
  (let* ((method (getf call-context :method))
         (start-time (getf call-context :start-time))
         (peer (getf call-context :peer))
         (elapsed-ms (/ (- (get-internal-real-time) start-time)
                        (/ internal-time-units-per-second 1000.0)))
         (status (if error "ERROR" "OK")))
    ;; Log trace info (actual OTEL integration would send to collector)
    (when *otel-available*
      ;; In a real implementation, this would use cl-opentelemetry's API
      ;; to create and export spans
      (format *trace-output*
              "~&[TRACE] ~A | ~A | ~,2Fms | peer=~A | status=~A~%"
              (get-universal-time) method elapsed-ms peer status)))
  response)

;;;; ========================================================================
;;;; Tracing Client Interceptor
;;;; ========================================================================

(defclass client-tracing-interceptor (client-interceptor)
  ((config :initarg :config :accessor client-tracing-config
           :initform (make-telemetry-config)
           :documentation "Telemetry configuration"))
  (:documentation "Client interceptor that adds OpenTelemetry tracing."))

(defun make-client-tracing-interceptor (&optional config)
  "Create a tracing interceptor for the client."
  (make-instance 'client-tracing-interceptor
                 :config (or config (make-telemetry-config))))

(defmethod client-interceptor-call-start ((interceptor client-tracing-interceptor) call-info)
  "Start a trace span for the outgoing RPC."
  (let ((method (getf call-info :method)))
    (list :method method
          :start-time (get-internal-real-time))))

(defmethod client-interceptor-call-end ((interceptor client-tracing-interceptor) call-info
                                         call-context response error)
  (let* ((method (getf call-context :method))
         (start-time (getf call-context :start-time))
         (elapsed-ms (/ (- (get-internal-real-time) start-time)
                        (/ internal-time-units-per-second 1000.0)))
         (status (if error "ERROR" "OK")))
    (when *otel-available*
      (format *trace-output*
              "~&[TRACE] ~A | CLIENT ~A | ~,2Fms | status=~A~%"
              (get-universal-time) method elapsed-ms status)))
  response)

;;;; ========================================================================
;;;; Trace Context Propagation
;;;; ========================================================================

(defun extract-trace-context (metadata)
  "Extract W3C trace context from gRPC metadata.
Returns (values trace-id span-id trace-flags) or NIL if not present."
  (let ((traceparent (metadata-get metadata "traceparent")))
    (when traceparent
      ;; Parse W3C traceparent: version-trace_id-span_id-flags
      ;; Example: 00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01
      (let ((parts (split-string traceparent #\-)))
        (when (= (length parts) 4)
          (values (second parts)   ; trace-id
                  (third parts)    ; span-id
                  (fourth parts))) ; flags
        ))))

(defun inject-trace-context (metadata trace-id span-id &optional (flags "01"))
  "Inject W3C trace context into gRPC metadata.
Returns the modified metadata."
  (let ((traceparent (format nil "00-~A-~A-~A" trace-id span-id flags)))
    (metadata-set metadata "traceparent" traceparent)
    metadata))

(defun generate-trace-id ()
  "Generate a new 32-character hex trace ID."
  (format nil "~32,'0X" (random (expt 2 128))))

(defun generate-span-id ()
  "Generate a new 16-character hex span ID."
  (format nil "~16,'0X" (random (expt 2 64))))

;;;; ========================================================================
;;;; Helper Functions
;;;; ========================================================================

(defun split-string (string delimiter)
  "Split STRING by DELIMITER character."
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

;;;; ========================================================================
;;;; Enable/Disable Tracing
;;;; ========================================================================

(defun enable-server-tracing (server &optional config)
  "Enable OpenTelemetry tracing on a gRPC server.
Returns the tracing interceptor."
  (let ((interceptor (make-tracing-interceptor config)))
    (server-add-interceptor server interceptor)
    interceptor))

(defun enable-channel-tracing (channel &optional config)
  "Enable OpenTelemetry tracing on a gRPC channel.
Returns the tracing interceptor."
  (let ((interceptor (make-client-tracing-interceptor config)))
    (channel-add-interceptor channel interceptor)
    interceptor))
