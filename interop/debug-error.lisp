;;;; debug-error.lisp - Debug error handling

(require 'asdf)

(asdf:initialize-source-registry
 `(:source-registry
   :inherit-configuration
   (:directory ,(uiop:getcwd))
   (:tree ,(merge-pathnames "ocicl/" (uiop:getcwd)))))

(asdf:load-system :ag-grpc)
(load "interop/interop.lisp")

(format t "~%Testing error status handling...~%")

;; First test serialization
(let* ((status (make-instance 'echostatus :code 2 :message "test error"))
       (status-bytes (ag-proto:serialize-to-bytes status)))
  (format t "EchoStatus code=2 serialized: ~A (~A bytes)~%" status-bytes (length status-bytes))
  (let* ((request (make-instance 'simplerequest :response-status status))
         (request-bytes (ag-proto:serialize-to-bytes request)))
    (format t "SimpleRequest with response-status serialized: ~A (~A bytes)~%"
            request-bytes (length request-bytes))))

(let ((channel (ag-grpc:make-channel "localhost" 10000)))
  (unwind-protect
    (let* ((request (make-instance 'simplerequest
                      :response-status (make-instance 'echostatus
                                         :code 2
                                         :message "test error"))))
      (format t "Making call with response-status code=2...~%")
      (handler-case
          (let* ((call (ag-grpc:call-unary channel
                        "/grpc.testing.TestService/UnaryCall"
                        request
                        :response-type 'simpleresponse)))
            (format t "Call returned: ~A~%" call)
            (format t "Call status: ~A~%" (ag-grpc:call-status call))
            (format t "Response headers: ~A~%" (ag-grpc::call-response-headers call))
            (format t "Response trailers: ~A~%" (ag-grpc::call-response-trailers call))
            (format t "No error raised!~%"))
        (ag-grpc:grpc-status-error (e)
          (format t "Got grpc-status-error: code=~A message=~A~%"
                  (ag-grpc:grpc-status-error-code e)
                  (ag-grpc:grpc-status-error-message e)))
        (error (e)
          (format t "Got other error: ~A~%" e))))
    (ag-grpc:channel-close channel)))

(uiop:quit 0)
