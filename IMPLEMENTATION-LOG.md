# Implementation Log: Concurrency Fixes

## Day 1: Baseline Verification (2026-02-06)

### Task 1.1: Test Baseline ✓

**Result**: All 241 tests pass (100%)

**Test Coverage**:
- Wire format tests (varints, zigzag, fixed-width, etc.)
- Protocol buffer parser tests
- Code generation tests
- Serialization round-trips

**Note**: No HTTP/2, HPACK, or gRPC integration tests exist (only TODOs)

**Warnings** (non-critical):
- Style warnings about undefined functions (test helper macros)
- These are pre-existing and don't affect test results

**Baseline established**: Tests will be re-run after each implementation phase to ensure no regressions.

### Task 1.2: Global State Audit ✓

**Global Mutable State Identified**:

1. **`*stream-contexts*`** (line 871) - Maps stream → grpc-call-context
2. **`*stream-handlers*`** (line 874) - Maps stream → handler function

**Accessor Functions**:
- `stream-call-context` (line 877) - Read access
- `(setf stream-call-context)` (line 881) - Write access
- `stream-handler` (line 885) - Read access
- `(setf stream-handler)` (line 889) - Write access

**Usage Points**:
- Line 462-463: Set context and handler when creating stream
- Line 494-495: Read context and handler when processing DATA frames

**Thread Safety Analysis**:
- ❌ No locking around hash table access
- ❌ Multiple connections = multiple threads accessing same hash tables
- ❌ Common Lisp hash tables not thread-safe for concurrent mutation
- **Risk**: Corruption, crashes, race conditions

**Blocking Issue Confirmed**:
- `stream-recv` (line 724) calls `connection-read-frame` (line 754) in loop
- Streaming handlers call `stream-recv` on connection thread
- **Impact**: Connection thread blocked, multiplexing broken

### Task 1.3: Code Paths to Modify ✓

**Phase 2 (Connection-Local State)**:
- `ag-grpc/server.lisp:871-891` - Move globals to connection object
- `ag-grpc/server.lisp:462-463` - Update setf calls
- `ag-grpc/server.lisp:494-495` - Update read calls

**Phase 3 (Async Handlers)**:
- `ag-grpc/server.lisp:724-775` - Refactor `stream-recv` to read from buffer
- `ag-grpc/server.lisp:478-483` - Spawn handler thread instead of inline call
- `ag-grpc/server.lisp:488-508` - Update DATA frame handling to append to buffer

**Phase 4 (Cleanup & Limits)**:
- Add stream cleanup on close
- Add max-concurrent-streams check in HEADERS handler (line 420)

---

## Day 1 Summary

✅ Tests verified: 241/241 passing
✅ Global state identified: 2 hash tables, 4 accessors, 2 usage points
✅ Blocking call confirmed: `connection-read-frame` in `stream-recv`
✅ Code paths mapped for Days 2-4

**Ready to proceed with Day 2: Connection-Local State**

---

## Day 2: Connection-Local State (2026-02-06)

### Changes Made ✓

**1. Added Connection-Local Storage** (`ag-http2/connection.lisp`):
- Added `stream-contexts` slot (hash table: stream → context)
- Added `stream-handlers` slot (hash table: stream → handler)
- Added `stream-buffers` slot (hash table: stream-id → buffer) [for Day 3]
- Added `stream-state-lock` slot (bt:lock for thread-safety)
- Added `active-streams` slot (counter for max-concurrent enforcement)

**2. Replaced Global Hash Tables** (`ag-grpc/server.lisp`):
- Removed `*stream-contexts*` global variable
- Removed `*stream-handlers*` global variable
- Added `connection-get-stream-context` (thread-safe read)
- Added `connection-set-stream-context` (thread-safe write)
- Added `connection-get-stream-handler` (thread-safe read)
- Added `connection-set-stream-handler` (thread-safe write)
- Added `connection-remove-stream-state` (cleanup helper)

**3. Updated Usage Points**:
- Line 462-463: Use `connection-set-*` instead of `setf` on globals
- Line 494-495: Use `connection-get-*` instead of `gethash` on globals

**4. Exported New Symbols** (`ag-http2/package.lisp`):
- Exported all new connection accessors for use in ag-grpc

### Testing ✓

- All 241 tests pass
- No regressions introduced
- Thread-safety ensured via `bt:with-lock-held`

### Next: Day 3 - Async Handlers + Frame Buffering

**Ready to proceed with Day 3**

---

## Day 3: Async Handlers + Frame Buffering (2026-02-06)

### Goal
Fix Finding #1: Streaming handlers currently block connection thread by calling
`connection-read-frame`. Need to:
1. Connection thread appends DATA to per-stream message buffers
2. `stream-recv` reads from buffer (blocks on condition variable, not frame read)
3. Streaming handlers run in separate threads

### Implementation Steps

**Step 3.1: Create Stream Message Buffer Structure** ✓
- Added `stream-message-buffer` struct with:
  - Messages array (bounded queue)
  - Lock and condition variable for synchronization
  - Closed flag for EOF signaling
  - Error field for propagating failures
- Added `buffer-push-message` (connection thread appends)
- Added `buffer-pop-message` (handler thread consumes, blocks if empty)
- Added `buffer-close` (signals end-of-stream)

**Step 3.2: Spawn Handler Threads** ⏭️
- Need to modify server-handle-headers to spawn threads for streaming
- Store buffer in connection-stream-buffers map
- Handler runs in separate thread (doesn't block connection)

**Step 3.3: Refactor stream-recv** ⏭️
- Change from calling connection-read-frame to buffer-pop-message
- Remove blocking loop
- Return nil when buffer closed

**Step 3.4: Update DATA Frame Handling** ⏭️
- Decode gRPC message in connection thread
- Append to buffer instead of stream data buffer
- Signal condition variable to wake handler

**Status**: Buffer structure complete, need to wire it up
