;;;; ag-http2.asd - HTTP/2 implementation for Common Lisp

(asdf:defsystem #:ag-http2
  :description "Pure Common Lisp HTTP/2 implementation (RFC 7540)"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:usocket)
  :pathname "ag-http2"
  :components ((:file "package")
               (:file "hpack")
               (:file "frames")
               (:file "streams")
               (:file "flow-control")
               (:file "connection")))
