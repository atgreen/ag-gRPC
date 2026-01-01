;;;; ag-proto-cli.asd - CLI tool for generating Lisp from .proto files

(asdf:defsystem #:ag-proto-cli
  :description "CLI tool for generating Common Lisp code from Protocol Buffer files"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:ag-proto #:clingon #:version-string)
  :pathname "ag-proto-cli"
  :components ((:file "package")
               (:file "main"))
  :build-operation "program-op"
  :build-pathname "ag-protoc"
  :entry-point "ag-proto-cli:main")
