;;;; test-client.lisp - Interop test client for ag-grpc
;;;;
;;;; Tests the ag-grpc client against a Go gRPC server.
;;;;
;;;; Usage:
;;;;   1. Start the Go server: ./go-server/hello-server
;;;;   2. Run this script: sbcl --script test-client.lisp

(require :asdf)

;; Configure ASDF to find our systems
(let ((project-root (make-pathname :directory (pathname-directory *load-truename*))))
  (pushnew (merge-pathnames "../" project-root) asdf:*central-registry*)
  (pushnew project-root asdf:*central-registry*)
  (setf asdf:*central-registry*
        (append asdf:*central-registry*
                (directory (merge-pathnames "../ocicl/*/" project-root))
                ;; Some systems (e.g., mgl-pax autoload) live one level deeper.
                (directory (merge-pathnames "../ocicl/*/*/" project-root)))))

;; Load the ag-grpc-all system
(format t "~&Loading ag-grpc-all...~%")
(asdf:load-system :ag-grpc-all)

;; Generate the hello proto code in-memory
(format t "~&Parsing hello.proto...~%")
(let ((hello-proto (merge-pathnames "../examples/hello.proto"
                                     (make-pathname :directory
                                                    (pathname-directory *load-truename*)))))
  (ag-proto:compile-proto-file hello-proto :load t))

(format t "~&Running interop test...~%")

;; Test the client
(defun run-interop-test ()
  (handler-case
      (let* (;; Create a channel to the server
             (channel (ag-grpc:make-channel "localhost" 50051))
             ;; Create the request message
             (request (make-instance 'hellorequest :name "World")))

        (format t "~&Sending HelloRequest with name='World'...~%")

        ;; Make the RPC call
        (let ((call (ag-grpc:call-unary channel
                                        "/hello.Greeter/SayHello"
                                        request
                                        :response-type 'helloreply)))
          (format t "~&Response received!~%")
          (format t "~&  Status: ~A~%" (ag-grpc:call-status call))
          (format t "~&  Message: ~A~%" (message (ag-grpc:call-response call)))

          ;; Close the channel
          (ag-grpc:channel-close channel)

          ;; Return success
          (if (eql (ag-grpc:call-status call) ag-grpc:+grpc-status-ok+)
              (progn
                (format t "~&~%SUCCESS: Interop test passed!~%")
                t)
              (progn
                (format t "~&~%FAILED: Call returned non-OK status~%")
                nil))))
    (error (e)
      (format t "~&~%ERROR: ~A~%" e)
      nil)))

(let ((result (run-interop-test)))
  (sb-ext:exit :code (if result 0 1)))
