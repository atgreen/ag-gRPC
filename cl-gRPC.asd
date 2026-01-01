;;; cl-gRPC.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(asdf:defsystem #:cl-gRPC
  :description "A basic application."
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :version     "0.1.0"
  :depends-on  ()
  :serial t
  :components ((:file "src/package")
               (:file "src/main")))
