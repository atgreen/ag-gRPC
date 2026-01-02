;;;; simple-test.lisp - Simple interop test

(require 'asdf)

(asdf:initialize-source-registry
 `(:source-registry
   :inherit-configuration
   (:directory ,(uiop:getcwd))
   (:tree ,(merge-pathnames "ocicl/" (uiop:getcwd)))))

(format t "~%Loading ag-grpc...~%")
(force-output)
(asdf:load-system :ag-grpc)

(format t "Loading interop.lisp...~%")
(force-output)
(load "interop/interop.lisp")

(format t "~%=== Testing Empty Unary ===~%")
(force-output)

;; Simple test
(let ((channel (ag-grpc:make-channel "localhost" 10000)))
  (unwind-protect
       (let ((stub (make-test-service-stub channel)))
         (format t "Created stub: ~A~%" stub)
         (force-output)
         (handler-case
             (multiple-value-bind (response status)
                 (test-service-empty-call stub (make-instance 'empty))
               (format t "Response: ~A~%" response)
               (format t "Status: ~A~%" status)
               (if (zerop status)
                   (format t "~%TEST PASSED!~%")
                   (format t "~%TEST FAILED: status ~A~%" status)))
           (error (e)
             (format t "~%TEST FAILED: ~A~%" e))))
    (ag-grpc:channel-close channel)))

(format t "~%Done.~%")
(uiop:quit 0)
