;;;; ag-grpc-all.asd - Top-level system that loads all subsystems

(asdf:defsystem #:ag-grpc-all
  :description "Complete Pure Common Lisp gRPC stack (Proto3 + HTTP/2 + gRPC)"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "1.0.0"
  :depends-on (#:ag-proto #:ag-http2 #:ag-grpc))

(asdf:defsystem #:ag-grpc-all/tests
  :description "Test suite for ag-grpc"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :depends-on (#:ag-grpc-all #:fiveam)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "wire-format-tests")
               (:file "parser-tests")
               (:file "codegen-tests")
               (:file "hpack-tests")
               (:file "http2-tests")
               (:file "grpc-tests")))
