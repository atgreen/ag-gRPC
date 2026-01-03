;;;; ag-grpc.asd - gRPC implementation for Common Lisp

(asdf:defsystem #:ag-grpc
  :description "Pure Common Lisp gRPC implementation"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:ag-proto #:ag-http2 #:version-string #:bordeaux-threads
               #:chipz #:salza2)
  :pathname "ag-grpc"
  :components ((:file "package")
               (:file "status")
               (:file "framing")
               (:file "metadata")
               (:file "channel")
               (:file "call")
               (:file "response")
               (:file "interceptor")
               (:file "retry")
               (:file "channel-pool")
               (:file "balancer")
               (:file "server")
               (:file "health")
               (:file "reflection")
               (:file "error-details")
               (:file "async")
               (:file "circuit-breaker")
               (:file "hedge")
               (:file "telemetry")
               (:file "grpc-web")))
