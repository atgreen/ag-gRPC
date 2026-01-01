;;;; ag-grpc.asd - gRPC implementation for Common Lisp

(asdf:defsystem #:ag-grpc
  :description "Pure Common Lisp gRPC implementation"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:ag-proto #:ag-http2 #:version-string)
  :pathname "ag-grpc"
  :components ((:file "package")
               (:file "status")
               (:file "framing")
               (:file "metadata")
               (:file "channel")
               (:file "call")
               (:file "response")
               (:file "server")))
