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

(format t "~&Running interop tests...~%")

;; Test unary RPC
(defun test-unary (channel)
  (format t "~&~%=== Test 1: Unary RPC ===~%")
  (let ((request (make-instance 'hellorequest :name "World")))
    (format t "~&Sending HelloRequest with name='World'...~%")
    (let ((call (ag-grpc:call-unary channel
                                    "/hello.Greeter/SayHello"
                                    request
                                    :response-type 'helloreply)))
      (format t "~&Response received!~%")
      (format t "~&  Status: ~A~%" (ag-grpc:call-status call))
      (format t "~&  Message: ~A~%" (message (ag-grpc:call-response call)))
      (eql (ag-grpc:call-status call) ag-grpc:+grpc-status-ok+))))

;; Test server streaming RPC
(defun test-server-streaming (channel)
  (format t "~&~%=== Test 2: Server Streaming RPC ===~%")
  (let ((request (make-instance 'hellorequest :name "Stream" :count 3)))
    (format t "~&Sending HelloRequest with name='Stream', count=3...~%")
    (let* ((stream (ag-grpc:call-server-streaming channel
                                                   "/hello.Greeter/SayHelloStream"
                                                   request
                                                   :response-type 'helloreply))
           (message-count 0))
      ;; Read all messages from the stream
      (ag-grpc:do-stream-messages (reply stream)
        (incf message-count)
        (format t "~&  [~A] ~A~%" (index reply) (message reply)))
      (format t "~&Stream finished. Status: ~A~%" (ag-grpc:stream-status stream))
      (format t "~&Total messages received: ~A~%" message-count)
      (and (eql (ag-grpc:stream-status stream) ag-grpc:+grpc-status-ok+)
           (= message-count 3)))))

;; Test client streaming RPC
(defun test-client-streaming (channel)
  (format t "~&~%=== Test 3: Client Streaming RPC ===~%")
  (format t "~&Opening client stream...~%")
  (let ((stream (ag-grpc:call-client-streaming channel
                                                "/hello.Greeter/CollectHellos"
                                                :response-type 'hellosummary)))
    ;; Send several messages
    (format t "~&Sending 3 HelloRequest messages...~%")
    (ag-grpc:stream-send stream (make-instance 'hellorequest :name "Alice"))
    (format t "~&  Sent: name='Alice'~%")
    (ag-grpc:stream-send stream (make-instance 'hellorequest :name "Bob"))
    (format t "~&  Sent: name='Bob'~%")
    (ag-grpc:stream-send stream (make-instance 'hellorequest :name "Charlie"))
    (format t "~&  Sent: name='Charlie'~%")
    ;; Close and get response
    (format t "~&Closing stream and receiving response...~%")
    (multiple-value-bind (response status)
        (ag-grpc:stream-close-and-recv stream)
      (format t "~&Response received!~%")
      (format t "~&  Status: ~A~%" status)
      (format t "~&  Total requests: ~A~%" (total-requests response))
      (format t "~&  Combined names: ~A~%" (combined-names response))
      (and (eql status ag-grpc:+grpc-status-ok+)
           (= (total-requests response) 3)
           (string= (combined-names response) "Alice, Bob, Charlie")))))

;; Run all tests
(defun run-interop-tests ()
  (handler-case
      (let ((channel (ag-grpc:make-channel "localhost" 50051))
            (all-passed t))
        (unwind-protect
             (progn
               ;; Test 1: Unary RPC
               (unless (test-unary channel)
                 (setf all-passed nil))
               ;; Test 2: Server Streaming RPC
               (unless (test-server-streaming channel)
                 (setf all-passed nil))
               ;; Test 3: Client Streaming RPC
               (unless (test-client-streaming channel)
                 (setf all-passed nil)))
          ;; Clean up
          (ag-grpc:channel-close channel))
        (if all-passed
            (progn
              (format t "~&~%SUCCESS: All interop tests passed!~%")
              t)
            (progn
              (format t "~&~%FAILED: Some tests failed~%")
              nil)))
    (error (e)
      (format t "~&~%ERROR: ~A~%" e)
      nil)))

(let ((result (run-interop-tests)))
  (sb-ext:exit :code (if result 0 1)))
