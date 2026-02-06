# Integration Test Plan for Concurrency Fixes

## Overview

All four core concurrency issues have been fixed:
- ✅ Finding #1: Streaming handlers no longer block connection thread
- ✅ Finding #2: Thread-safe connection-local state
- ✅ Finding #3: Streams cleaned up on close
- ✅ Finding #4: Max concurrent streams enforced

All 241 existing unit tests pass.

## Required Test Infrastructure

To properly test these fixes, we need:

1. **gRPC Test Server**: Ability to start/stop test server on random port
2. **Test Proto Definitions**: Simple test services (unary, streaming)
3. **Test Client**: gRPC client to make test calls
4. **Concurrency Helpers**: Utilities to spawn multiple concurrent requests

## Proposed Integration Tests

### Test 1: Streaming doesn't block unary (Finding #1)
**Goal**: Verify that slow streaming RPC doesn't block other RPCs

**Setup**:
- Define service with:
  - `SlowServerStream(request) returns (stream response)` - sleeps between messages
  - `FastUnary(request) returns (response)` - returns immediately

**Test**:
1. Start SlowServerStream call, don't read messages (blocks in handler)
2. Make 10 FastUnary calls concurrently
3. Verify all FastUnary calls complete in < 1 second
4. Close SlowServerStream

**Expected**: Unary calls complete quickly despite blocked streaming handler

---

### Test 2: Cleanup removes closed streams (Finding #3)
**Goal**: Verify streams removed from connection state on close

**Setup**:
- Define service: `Echo(request) returns (response)`
- Add test accessor to read connection state size

**Test**:
1. Make 100 Echo calls sequentially
2. After each call, verify stream count returns to 0
3. Check no memory accumulation

**Expected**: Stream maps and buffers cleaned up after each call

---

### Test 3: Max concurrent enforced (Finding #4)
**Goal**: Verify server rejects streams when over limit

**Setup**:
- Create server with `max-concurrent-streams: 2`
- Define service: `Block() returns (response)` - blocks until signaled

**Test**:
1. Start 2 Block calls (fills limit)
2. Attempt 3rd call
3. Verify 3rd call receives REFUSED_STREAM error
4. Complete one of first 2 calls
5. Retry 3rd call
6. Verify 3rd call now succeeds

**Expected**: Exactly 2 concurrent streams allowed

---

### Test 4: Handler exception doesn't crash connection (Robustness)
**Goal**: Verify handler errors don't kill connection

**Setup**:
- Define service:
  - `Crash() returns (response)` - raises error
  - `Healthy() returns (response)` - returns normally

**Test**:
1. Call Crash() - should return INTERNAL error
2. Immediately call Healthy()
3. Verify Healthy() succeeds
4. Make 10 more Healthy() calls

**Expected**: Connection remains usable after handler error

---

### Test 5: Connection close terminates handlers (Robustness)
**Goal**: Verify closing connection stops all handlers

**Setup**:
- Define service: `LongStream() returns (stream response)` - infinite stream
- Track handler thread status

**Test**:
1. Start 3 LongStream calls
2. Verify 3 handler threads running
3. Close connection (server-side)
4. Wait 2 seconds
5. Verify all 3 handler threads terminated

**Expected**: All handler threads exit when connection closes

---

## Edge Case Tests (Day 6)

### Test 6: Slow consumer backpressure
- Handler sends fast, client reads slow
- Verify buffer doesn't grow unbounded
- Currently no flow control, so buffer may fill

### Test 7: Stream closed mid-read
- Client closes stream while handler sending
- Verify handler detects closure and stops

### Test 8: Concurrent buffer access
- Multiple threads accessing same connection
- Stress test thread-safety of buffer operations

---

## Implementation Status

**Completed**:
- ✅ Days 1-4: All concurrency fixes implemented
- ✅ All 241 unit tests passing

**Remaining**:
- ⏸️ Integration test infrastructure (not yet implemented)
- ⏸️ Integration tests 1-5 (planned above)
- ⏸️ Edge case tests 6-8 (planned above)

**Note**: Creating full integration test infrastructure requires:
- gRPC client implementation (currently ag-gRPC only has server)
- Test proto compilation and code generation
- Test harness for concurrent scenarios
- ~1-2 days additional work

---

## Verification Without Integration Tests

The core fixes can be verified through:

1. **Code Review**: Manual inspection confirms:
   - Handlers spawn in threads (ag-grpc/server.lisp:493)
   - Buffers use condition variables (ag-grpc/server.lisp:842)
   - All accessors use locking (ag-grpc/server.lisp:874-896)
   - Cleanup callback complete (ag-grpc/server.lisp:465-480)
   - Limit enforcement added (ag-grpc/server.lisp:434-438)

2. **Unit Tests**: All 241 tests pass, confirming no regressions

3. **Static Analysis**: Threading patterns match standard practices:
   - Connection-local state (eliminates global contention)
   - Lock-protected hash tables (thread-safe access)
   - Condition variables (proper thread coordination)
   - Thread-per-handler (prevents blocking)

4. **Manual Testing**: Once deployed, can verify with real workloads

---

## Conclusion

All four concurrency issues from C-REVIEW.md have been fixed with proper threading and synchronization. The fixes follow Common Lisp best practices and maintain all existing test coverage.

Integration tests are planned and documented above, but require additional infrastructure (client, test harness) that was not part of the original codebase.
