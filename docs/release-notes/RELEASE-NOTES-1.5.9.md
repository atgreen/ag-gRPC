# ag-gRPC 1.5.9

**Release date:** 2026-09-20

## Bug Fixes

Three defects that compound into a permanently deaf server. A long-running
deployment hit all three in sequence: connections died after 100 requests,
clients abandoned them, and the abandoned connections retired the server.

- **A connection's stream capacity is no longer consumed for good.**
  `connection-active-streams` was incremented for every request and
  decremented only from the HTTP/2 stream's cleanup callback - which normal
  completion never ran, since only `RST_STREAM` and connection teardown
  invoked it. The count therefore only climbed, and after
  `max-concurrent-streams` (default 100) requests the server answered every
  further stream on that connection with `REFUSED_STREAM`, for the life of the
  connection. Reaching `:closed` now runs the stream's cleanup exactly once,
  whatever ends the stream: ordinary `END_STREAM`, `RST_STREAM`, or connection
  close.

- **The accept loop can no longer be blocked by idle peers.**
  `server-accept-loop` waited on the connection semaphore with no timeout, so
  once `max-connections` peers held every permit, the loop blocked inside
  `wait-on-semaphore` forever: the listener stayed open, the backlog filled,
  and every later client hung on connect. The wait is now bounded; a server at
  capacity closes the incoming connection (and says so on `*error-output*`)
  instead of stalling the loop. Accepted sockets are also closed
  unconditionally when their connection thread exits.

- **Idle connections are reaped, so capacity comes back.**
  A peer that connects and says nothing - or goes away without closing its
  socket - used to own a connection thread and its slot forever, because both
  the handshake read and the frame loop block with no deadline. Connections
  that send nothing for `connection-idle-timeout` seconds (new
  `make-grpc-server` keyword, default 300, `NIL` disables) are now closed.
  Connections with RPCs in flight are never considered idle, so a client is
  still free to stay quiet while the server streams.

- **`connection-close` always releases the socket.** It took the write lock to
  send a farewell `GOAWAY` before closing, with no timeout and no
  `unwind-protect`, so a peer stalled mid-write could block the close and leak
  the file descriptor. `GOAWAY` is now best effort; closing the socket is not.

## Compatibility

No API changes. `make-grpc-server` gains an optional `:connection-idle-timeout`
keyword; servers that want the old behaviour of holding connections until the
peer closes them can pass `NIL`.
