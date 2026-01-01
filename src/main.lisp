;;; main.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2024  Anthony Green <green@moxielogic.com>

(in-package #:cl-grpc)

(defun main ()
  "Entry point for the application."
  (format t "ag-gRPC ~A~%" ag-grpc:+version+))
