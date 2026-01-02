;;;; run-tests.lisp - gRPC interop conformance tests
;;;;
;;;; Run with: sbcl --load run-tests.lisp

(require 'asdf)

;; Set up ASDF to find our systems
(asdf:initialize-source-registry
 `(:source-registry
   :inherit-configuration
   (:directory ,(uiop:getcwd))
   (:tree ,(merge-pathnames "ocicl/" (uiop:getcwd)))))

;; Load ag-grpc
(asdf:load-system :ag-grpc)

;; Load the generated interop code
(load "interop/interop.lisp")

(defpackage #:interop-tests
  (:use #:cl)
  (:import-from #:cl-user
   ;; Message classes
   #:empty #:payload #:echostatus
   #:simplerequest #:simpleresponse
   #:responseparameters
   #:streamingoutputcallrequest #:streamingoutputcallresponse
   #:streaminginputcallrequest #:streaminginputcallresponse
   ;; Accessor
   #:body #:aggregated-payload-size
   ;; Stub and service functions
   #:make-test-service-stub
   #:make-unimplemented-service-stub
   #:test-service-empty-call
   #:test-service-unary-call
   #:test-service-streaming-output-call
   #:test-service-streaming-input-call
   #:test-service-full-duplex-call
   #:test-service-unimplemented-call
   #:unimplemented-service-unimplemented-call))

(in-package #:interop-tests)

(defvar *server-host* "localhost")
(defvar *server-port* 10000)
(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defmacro define-test (name &body body)
  `(defun ,name ()
     (format t "~%Running test: ~A... " ',name)
     (force-output)
     (handler-case
         (progn
           ,@body
           (format t "PASSED~%")
           (incf *tests-passed*))
       (error (e)
         (format t "FAILED: ~A~%" e)
         (incf *tests-failed*)))))

(defmacro assert-equal (expected actual &optional msg)
  `(unless (equal ,expected ,actual)
     (error "~A~%  Expected: ~S~%  Got: ~S"
            (or ,msg "Assertion failed") ,expected ,actual)))

(defmacro assert-true (expr &optional msg)
  `(unless ,expr
     (error "~A: ~S is not true" (or ,msg "Assertion failed") ',expr)))

;;; ========================================================================
;;; Test: empty_unary
;;; ========================================================================

(define-test test-empty-unary
  "Test that empty unary calls work"
  (ag-grpc:with-channel (channel *server-host* *server-port*)
    (let ((stub (make-test-service-stub channel)))
      (multiple-value-bind (response status)
          (test-service-empty-call stub (make-instance 'empty))
        (assert-equal 0 status "Status should be OK")
        (assert-true (typep response 'empty) "Response should be Empty")))))

;;; ========================================================================
;;; Test: large_unary
;;; ========================================================================

(define-test test-large-unary
  "Test that large unary calls work"
  (ag-grpc:with-channel (channel *server-host* *server-port*)
    (let* ((stub (make-test-service-stub channel))
           (request-size 271828)
           (response-size 314159)
           (request (make-instance 'simplerequest
                      :response-size response-size
                      :payload (make-instance 'payload
                                 :body (make-array request-size
                                         :element-type '(unsigned-byte 8)
                                         :initial-element 0)))))
      (multiple-value-bind (response status)
          (test-service-unary-call stub request)
        (assert-equal 0 status "Status should be OK")
        (assert-true (typep response 'simpleresponse) "Response type")
        (assert-equal response-size (length (body (payload response)))
                      "Response payload size")))))

;;; ========================================================================
;;; Test: client_streaming
;;; ========================================================================

(define-test test-client-streaming
  "Test client streaming aggregates payload sizes"
  (ag-grpc:with-channel (channel *server-host* *server-port*)
    (let ((stub (make-test-service-stub channel))
          (sizes '(27182 8 1828 45904)))
      (let ((stream (test-service-streaming-input-call stub)))
        ;; Send requests
        (dolist (size sizes)
          (ag-grpc:stream-send stream
            (make-instance 'streaminginputcallrequest
              :payload (make-instance 'payload
                         :body (make-array size
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))))
        ;; Close and get response
        (multiple-value-bind (response status)
            (ag-grpc:stream-close-and-recv stream)
          (assert-equal 0 status "Status should be OK")
          (assert-equal (reduce #'+ sizes)
                        (aggregated-payload-size response)
                        "Aggregated size should match"))))))

;;; ========================================================================
;;; Test: server_streaming
;;; ========================================================================

(define-test test-server-streaming
  "Test server streaming returns multiple responses"
  (ag-grpc:with-channel (channel *server-host* *server-port*)
    (let* ((stub (make-test-service-stub channel))
           (sizes '(31415 9 2653 58979))
           (request (make-instance 'streamingoutputcallrequest
                      :response-parameters
                      (mapcar (lambda (size)
                                (make-instance 'responseparameters :size size))
                              sizes))))
      (let ((stream (test-service-streaming-output-call stub request)))
        (let ((received-sizes nil))
          (ag-grpc:do-stream-messages (response stream)
            (push (length (body (payload response))) received-sizes))
          (assert-equal (reverse sizes) received-sizes
                        "Response sizes should match"))))))

;;; ========================================================================
;;; Test: ping_pong (bidirectional streaming)
;;; ========================================================================

(define-test test-ping-pong
  "Test bidirectional streaming with alternating messages"
  (ag-grpc:with-channel (channel *server-host* *server-port*)
    (let* ((stub (make-test-service-stub channel))
           (sizes '(31415 9 2653 58979))
           (stream (test-service-full-duplex-call stub)))
      (dolist (size sizes)
        ;; Send request
        (ag-grpc:stream-send stream
          (make-instance 'streamingoutputcallrequest
            :response-parameters
            (list (make-instance 'responseparameters :size size))))
        ;; Receive response
        (let ((response (ag-grpc:stream-read-message stream)))
          (assert-true response "Should receive response")
          (assert-equal size (length (body (payload response)))
                        "Response size should match")))
      ;; Close send side
      (ag-grpc:stream-close-send stream)
      ;; Verify no more messages
      (assert-true (null (ag-grpc:stream-read-message stream))
                   "No more messages after close"))))

;;; ========================================================================
;;; Test: empty_stream
;;; ========================================================================

(define-test test-empty-stream
  "Test that empty streams work"
  (ag-grpc:with-channel (channel *server-host* *server-port*)
    (let ((stub (make-test-service-stub channel)))
      ;; Empty server streaming
      (let* ((request (make-instance 'streamingoutputcallrequest))
             (stream (test-service-streaming-output-call stub request))
             (count 0))
        (ag-grpc:do-stream-messages (response stream)
          (incf count))
        (assert-equal 0 count "Should receive no messages"))
      ;; Empty client streaming
      (let ((stream (test-service-streaming-input-call stub)))
        (multiple-value-bind (response status)
            (ag-grpc:stream-close-and-recv stream)
          (assert-equal 0 status "Status should be OK")
          (assert-equal 0 (aggregated-payload-size response)
                        "Aggregated size should be 0"))))))

;;; ========================================================================
;;; Test: status_code_and_message
;;; ========================================================================

(define-test test-status-code-and-message
  "Test that status codes and messages propagate correctly"
  (ag-grpc:with-channel (channel *server-host* *server-port*)
    (let* ((stub (make-test-service-stub channel))
           (test-code 2)  ; UNKNOWN
           (test-message "test status message")
           (request (make-instance 'simplerequest
                      :response-status (make-instance 'echostatus
                                         :code test-code
                                         :message test-message))))
      (handler-case
          (progn
            (test-service-unary-call stub request)
            (error "Should have raised an error"))
        (ag-grpc:grpc-status-error (e)
          (assert-equal test-code (ag-grpc:grpc-status-error-code e)
                        "Status code should match")
          ;; Note: message may be URL-encoded
          (assert-true (search "test" (ag-grpc:grpc-status-error-message e))
                       "Status message should contain test string"))))))

;;; ========================================================================
;;; Test: unimplemented_method
;;; ========================================================================

(define-test test-unimplemented-method
  "Test that unimplemented methods return UNIMPLEMENTED"
  (ag-grpc:with-channel (channel *server-host* *server-port*)
    (let ((stub (make-test-service-stub channel)))
      (handler-case
          (progn
            (test-service-unimplemented-call stub (make-instance 'empty))
            (error "Should have raised an error"))
        (ag-grpc:grpc-status-error (e)
          (assert-equal ag-grpc:+grpc-status-unimplemented+
                        (ag-grpc:grpc-status-error-code e)
                        "Status should be UNIMPLEMENTED"))))))

;;; ========================================================================
;;; Test: unimplemented_service
;;; ========================================================================

(define-test test-unimplemented-service
  "Test that unimplemented services return UNIMPLEMENTED"
  (ag-grpc:with-channel (channel *server-host* *server-port*)
    (let ((stub (make-unimplemented-service-stub channel)))
      (handler-case
          (progn
            (unimplemented-service-unimplemented-call stub (make-instance 'empty))
            (error "Should have raised an error"))
        (ag-grpc:grpc-status-error (e)
          (assert-equal ag-grpc:+grpc-status-unimplemented+
                        (ag-grpc:grpc-status-error-code e)
                        "Status should be UNIMPLEMENTED"))))))

;;; ========================================================================
;;; Run all tests
;;; ========================================================================

(defun run-all-tests ()
  (setf *tests-passed* 0)
  (setf *tests-failed* 0)

  (format t "~%========================================~%")
  (format t "ag-gRPC Interop Conformance Tests~%")
  (format t "========================================~%")
  (format t "Server: ~A:~A~%" *server-host* *server-port*)

  ;; Run tests
  (test-empty-unary)
  (test-large-unary)
  (test-client-streaming)
  (test-server-streaming)
  (test-ping-pong)
  (test-empty-stream)
  (test-status-code-and-message)
  (test-unimplemented-method)
  (test-unimplemented-service)

  (format t "~%========================================~%")
  (format t "Results: ~A passed, ~A failed~%"
          *tests-passed* *tests-failed*)
  (format t "========================================~%")

  (if (zerop *tests-failed*)
      (progn
        (format t "~%All tests passed!~%")
        (uiop:quit 0))
      (progn
        (format t "~%Some tests failed.~%")
        (uiop:quit 1))))

;; Run tests
(run-all-tests)
