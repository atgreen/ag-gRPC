;;;; ag-proto.asd - Protocol Buffers (Proto3) implementation for Common Lisp

(asdf:defsystem #:ag-proto
  :description "Pure Common Lisp Protocol Buffers (Proto3) implementation"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:iparse #:trivial-utf-8 #:ieee-floats #:trivial-gray-streams)
  :pathname "ag-proto"
  :components ((:file "package")
               (:file "wire-format")
               (:file "descriptor")
               (:file "parser")
               (:file "codegen")
               (:file "runtime")))
