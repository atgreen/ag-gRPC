;;;; server-tests.lisp - Server liveness regressions
;;;;
;;;; These exercise the accept loop and connection reaper rather than the wire
;;;; protocol: a gRPC server that stops accepting is indistinguishable from a
;;;; dead one, and nothing above this layer can route around it.

(in-package #:ag-grpc-tests)

(in-suite server-tests)

;;;; ------------------------------------------------------------------
;;;; Helpers
;;;; ------------------------------------------------------------------

(defun start-test-server (&rest options)
  "Start a gRPC server on an ephemeral loopback port.
Returns (values server thread port)."
  (let* ((server (apply #'ag-grpc:make-grpc-server 0 :host "127.0.0.1" options))
         (thread (bt:make-thread (lambda () (ignore-errors (ag-grpc:server-start server)))
                                 :name "test-grpc-server")))
    (loop repeat 500
          until (and (eql (ag-grpc:server-state server) :running)
                     (ag-grpc::server-socket server))
          do (sleep 0.01))
    (unless (ag-grpc::server-socket server)
      (error "Test server failed to start listening"))
    (values server thread (usocket:get-local-port (ag-grpc::server-socket server)))))

(defun wait-for-thread-exit (thread &key (timeout 5))
  "T if THREAD has finished within TIMEOUT seconds."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop until (< deadline (get-internal-real-time))
          when (not (bt:thread-alive-p thread)) return t
          do (sleep 0.02)
          finally (return (not (bt:thread-alive-p thread))))))

(defun stop-test-server (server thread)
  (ignore-errors (ag-grpc:server-stop server))
  (wait-for-thread-exit thread :timeout 5))

(defun connect-raw (port)
  "Open a TCP connection that never speaks - the shape of a client that
connected and then went away."
  (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8)))

(defun server-closed-connection-p (socket &key (timeout 5))
  "T if the server closes SOCKET within TIMEOUT seconds.
Drains any farewell frames (GOAWAY) the server sends before the close.

The read runs in a helper thread and the caller watches the clock, because a
blocking read is the only close-detection that behaves the same everywhere:
usocket's Windows backend registers stream sockets for FD_READ alone and
decides readiness with FIONREAD, so a peer's close never wakes
WAIT-FOR-INPUT there. If the server never closes, the helper stays parked in
the read until the caller closes the socket on its way out."
  (let* ((stream (usocket:socket-stream socket))
         (closed nil)
         (reader (bt:make-thread
                  (lambda ()
                    (handler-case
                        (loop until (eq :eof (read-byte stream nil :eof))
                              finally (setf closed t))
                      (error () nil)))
                  :name "close-watcher")))
    (and (wait-for-thread-exit reader :timeout timeout)
         closed)))

;;;; ------------------------------------------------------------------
;;;; Tests
;;;; ------------------------------------------------------------------

(test accept-loop-survives-connection-limit
  "A peer that connects and never speaks must not deadlock the accept loop.

Regression (cave-ear): server-accept-loop waited on the connection semaphore
with no timeout, so once max-connections silent peers held every permit the
loop blocked inside wait-on-semaphore forever. The listener stayed open, the
backlog filled, and every later client hung on connect - a server alive but
permanently deaf."
  (multiple-value-bind (server thread port)
      (start-test-server :max-connections 1)
    (unwind-protect
         (let ((hog (connect-raw port)))
           (unwind-protect
                (let ((over-limit (connect-raw port)))
                  (unwind-protect
                       (is-true (server-closed-connection-p over-limit :timeout 5)
                                "server at capacity must shed the connection, not hang")
                    (ignore-errors (usocket:socket-close over-limit))))
             (ignore-errors (usocket:socket-close hog)))
           (is (eql :running (ag-grpc:server-state server))))
      (stop-test-server server thread))))

(test idle-connections-are-reaped
  "A connection that goes silent must lose its slot, so capacity returns.

This is the other half of the wedge: permits are only released when a
connection thread exits, and the thread blocks in the handshake read (or the
frame loop) with no deadline. Without a reaper, max-connections abandoned
peers retire the server permanently."
  (multiple-value-bind (server thread port)
      (start-test-server :max-connections 1 :connection-idle-timeout 1)
    (unwind-protect
         (let ((abandoned (connect-raw port)))
           (unwind-protect
                (is-true (server-closed-connection-p abandoned :timeout 10)
                         "silent connection must be reaped after the idle timeout")
             (ignore-errors (usocket:socket-close abandoned)))
           ;; The permit is back: a real client can still be served.
           (let ((conn (ag-http2:make-client-connection "127.0.0.1" port)))
             (unwind-protect
                  (is (eql :open (ag-http2:connection-state conn)))
               (ignore-errors (ag-http2:connection-close conn)))))
      (stop-test-server server thread))))

(test connection-close-releases-socket-when-goaway-is-blocked
  "CONNECTION-CLOSE must close the socket even when it cannot send GOAWAY.

Regression (cave-ear): connection-close took the write lock to send GOAWAY
before closing the socket, with no timeout and no unwind-protect. A peer
stalled mid-write held that lock, so close blocked and the file descriptor
leaked - which is how the client side of a wedged connection kept its half of
the socket open forever, hiding the failure from the server."
  (multiple-value-bind (server thread port)
      (start-test-server :max-connections 8)
    (unwind-protect
         (let* ((conn (ag-http2:make-client-connection "127.0.0.1" port))
                (socket (ag-http2::connection-socket conn))
                (lock-held (bt:make-semaphore))
                (release (bt:make-semaphore))
                (hog (bt:make-thread
                      (lambda ()
                        (bt2:with-lock-held ((ag-http2::connection-write-lock conn))
                          (bt:signal-semaphore lock-held)
                          (bt:wait-on-semaphore release :timeout 30)))
                      :name "write-lock-hog")))
           (unwind-protect
                (progn
                  (is-true (bt:wait-on-semaphore lock-held :timeout 5)
                           "test setup: write lock should be held")
                  (let ((closer (bt:make-thread
                                 (lambda () (ignore-errors (ag-http2:connection-close conn)))
                                 :name "connection-closer")))
                    (is-true (wait-for-thread-exit closer :timeout 5)
                             "connection-close must not block on the write lock")
                    (is-false (open-stream-p (usocket:socket-stream socket))
                              "connection-close must close the socket")))
             (bt:signal-semaphore release)
             (wait-for-thread-exit hog :timeout 5)))
      (stop-test-server server thread))))

(test handshaked-connection-that-goes-silent-is-reaped
  "The production shape of the leak: a peer completes the handshake, uses the
connection, then vanishes without closing its socket. TCP says the connection
is fine, the server's frame read blocks forever, and the slot never comes
back. Reaping has to cover the frame loop, not just the handshake."
  (multiple-value-bind (server thread port)
      (start-test-server :max-connections 1 :connection-idle-timeout 1)
    (unwind-protect
         (let ((conn (ag-http2:make-client-connection "127.0.0.1" port)))
           (unwind-protect
                (progn
                  (is (eql :open (ag-http2:connection-state conn)))
                  (is-true (server-closed-connection-p (ag-http2::connection-socket conn)
                                                       :timeout 10)
                           "a handshaked but silent connection must be reaped"))
             (ignore-errors (ag-http2:connection-close conn))))
      (stop-test-server server thread))))

(test connection-serves-more-rpcs-than-max-concurrent-streams
  "A connection's stream capacity must be about concurrency, not lifetime use.

Regression (cave-ear): connection-active-streams was incremented for every
request and decremented only from the stream cleanup callback - which normal
completion never ran, since only RST_STREAM and connection teardown invoked
it. The count therefore only climbed, and after max-concurrent-streams
requests the server refused every further stream on that connection. Clients
abandoned those connections, and each abandoned connection took a server
connection slot with it."
  (multiple-value-bind (server thread port)
      (start-test-server :max-concurrent-streams 2)
    (ag-grpc:server-enable-health-checking server)
    (unwind-protect
         (let ((channel (ag-grpc:make-channel "127.0.0.1" port :timeout 5)))
           (unwind-protect
                (dotimes (i 5)
                  (let ((response
                          (handler-case
                              (ag-grpc:grpc-call channel "/grpc.health.v1.Health/Check"
                                                 (make-instance 'ag-grpc:health-check-request
                                                                :service "")
                                                 :response-type 'ag-grpc:health-check-response)
                            (error (e) (format nil "error: ~A" e)))))
                    (is (typep response 'ag-grpc:health-check-response)
                        "request ~A of 5 on one connection should be served, got ~A"
                        (1+ i) response)))
             (ignore-errors (ag-grpc:channel-close channel))))
      (stop-test-server server thread))))
