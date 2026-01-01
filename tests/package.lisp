;;;; package.lisp - Test suite package for ag-grpc

(defpackage #:ag-grpc-tests
  (:use #:cl #:fiveam)
  (:shadow #:run-all-tests)
  (:export
   #:run-tests
   #:ag-grpc-all-tests
   #:wire-format-tests
   #:parser-tests
   #:codegen-tests
   #:hpack-tests
   #:http2-tests
   #:grpc-tests))

(in-package #:ag-grpc-tests)

(def-suite ag-grpc-all-tests
  :description "All tests for ag-grpc")

(def-suite wire-format-tests
  :description "Wire format encoding/decoding tests"
  :in ag-grpc-all-tests)

(def-suite parser-tests
  :description "Proto3 parser tests"
  :in ag-grpc-all-tests)

(def-suite codegen-tests
  :description "Proto code generation tests"
  :in ag-grpc-all-tests)

(def-suite hpack-tests
  :description "HPACK compression tests"
  :in ag-grpc-all-tests)

(def-suite http2-tests
  :description "HTTP/2 protocol tests"
  :in ag-grpc-all-tests)

(def-suite grpc-tests
  :description "gRPC protocol tests"
  :in ag-grpc-all-tests)

(defun run-tests ()
  "Run all ag-grpc tests"
  (run! 'ag-grpc-all-tests))
