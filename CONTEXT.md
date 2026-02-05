# cl-context Integration Plan for ag-gRPC

This document outlines a pragmatic plan for integrating `cl-context` into ag-gRPC, addressing the critiques in C-REVIEW.md and grounded in the current codebase.

**Status**: Updated based on C-REVIEW.md feedback (11th iteration - final implementation-ready)

## Executive Summary

**Goal**: Use `cl-context` for cancellation, deadlines, and request-scoped values while maintaining backward compatibility and addressing the real constraints of the current implementation.

**Key Insight**: `cl-context` provides cooperative cancellation via `check-context`, not preemptive interruption. This requires careful integration with the I/O layer.

**Hard Dependency**: `cl-context` is a required dependency (not optional). All code assumes it's available.

## Final Issues Resolved (11th Iteration - C-REVIEW.md)

The 11th review iteration identified 3 final verification issues. All are now resolved:

### Issue #1: Reader Naming Consistency Verification ✓

**Problem**: Need to confirm actual reader names in existing `grpc-status-error` definition to avoid naming mismatches when adding `:cause` slot.

**Solution**: Verified existing reader names from `ag-grpc/status.lisp` lines 73-92.

**Existing Definition - Verified Reader Names**:
```lisp
;; From ag-grpc/status.lisp lines 73-92 (ACTUAL CURRENT CODE)

(define-condition grpc-status-error (error)
  ((code :initarg :code :reader grpc-status-error-code          ; ← Verified
         :documentation "gRPC status code")
   (message :initarg :message :reader grpc-status-error-message ; ← Verified
            :initform nil
            :documentation "Error message")
   (details :initarg :details :reader grpc-status-error-details ; ← Verified (exists!)
            :initform nil
            :documentation "Additional error details")
   (headers :initarg :headers :reader grpc-status-error-headers ; ← Verified
            :initform nil
            :documentation "Response headers")
   (trailers :initarg :trailers :reader grpc-status-error-trailers ; ← Verified
             :initform nil
             :documentation "Response trailers"))
  (:report (lambda (c s)
             (format s "gRPC error ~A (~A)~@[: ~A~]"
                     (grpc-status-error-code c)
                     (grpc-status-error-name (grpc-status-error-code c))
                     (grpc-status-error-message c)))))
```

**Confirmed Reader Names** (all use `grpc-status-error-` prefix):
- ✅ `grpc-status-error-code` (not `grpc-error-code`)
- ✅ `grpc-status-error-message` (not `grpc-error-message`)
- ✅ `grpc-status-error-details` (exists in current definition)
- ✅ `grpc-status-error-headers`
- ✅ `grpc-status-error-trailers`

**New Reader for :cause slot**:
- ✅ `grpc-status-error-cause` (follows same naming convention)

**Consistency Verified**: All readers follow `grpc-status-error-<slot>` pattern.

### Issue #2: Details Slot Verification ✓

**Problem**: Plan mentioned `details` slot but need to verify it exists in current definition (adding it would be breaking change).

**Solution**: Verified that `details` slot **DOES exist** in current definition (line 79).

**Current Definition** (ag-grpc/status.lisp line 79):
```lisp
(details :initarg :details :reader grpc-status-error-details
         :initform nil
         :documentation "Additional error details")
```

**Modification Plan - Add :cause ONLY**:
```lisp
;; MODIFY existing definition in ag-grpc/status.lisp
;; Keep ALL existing slots exactly as-is, add ONLY :cause

(define-condition grpc-status-error (error)
  ;; EXISTING SLOTS - DO NOT MODIFY
  ((code :initarg :code :reader grpc-status-error-code
         :documentation "gRPC status code")
   (message :initarg :message :reader grpc-status-error-message
            :initform nil
            :documentation "Error message")
   (details :initarg :details :reader grpc-status-error-details
            :initform nil
            :documentation "Additional error details")
   (headers :initarg :headers :reader grpc-status-error-headers
            :initform nil
            :documentation "Response headers")
   (trailers :initarg :trailers :reader grpc-status-error-trailers
             :initform nil
             :documentation "Response trailers")

   ;; NEW SLOT - ADD THIS ONLY
   (cause :initarg :cause :reader grpc-status-error-cause
          :initform nil
          :documentation "Original condition that caused this error (for debugging)"))

  ;; UPDATE REPORT - Add cause chain display
  (:report (lambda (c s)
             (format s "gRPC error ~A (~A)~@[: ~A~]"
                     (grpc-status-error-code c)
                     (grpc-status-error-name (grpc-status-error-code c))
                     (grpc-status-error-message c))
             (when (grpc-status-error-cause c)
               (format s "~%  Caused by: ~A" (grpc-status-error-cause c))))))
```

**Key Points**:
- ✅ `details` slot already exists - no breaking change
- ✅ Add ONLY `:cause` slot
- ✅ Keep all existing slots unchanged
- ✅ Update `:report` to show cause chain

### Issue #3: Cancellation Mapping Path Documentation ✓

**Problem**: Plan says cancellation handled elsewhere but doesn't document specific mapping path for client-side operations.

**Solution**: Document complete cancellation mapping paths for both client and server side.

**Cancellation Mapping Paths**:

**1. Server-Side Cancellation → CANCELLED**:
```lisp
;; In server handler code (user's handler implementation)
(defun my-handler (request ctx)
  ;; Check for cancellation explicitly
  (context-ensure-not-cancelled ctx)  ; ← Signals grpc-status-error with CANCELLED

  ;; Or check RST_STREAM via predicate
  (when (context-check-cancelled ctx)  ; ← Checks RST_STREAM + cl-context
    (error 'grpc-status-error
           :code +grpc-status-cancelled+
           :message "Request cancelled")))
```

**From `context-ensure-not-cancelled` implementation** (see section 3 in plan):
```lisp
(defun context-ensure-not-cancelled (ctx)
  "Check if cancelled and signal grpc-status-error if so.

  Cancellation precedence:
  1. Deadline exceeded takes precedence (handled by with-stream-timeout)
  2. RST_STREAM second (client-initiated cancellation)
  3. Other cl-context cancellation"

  ;; Check RST_STREAM (second precedence)
  (let* ((stream-id (context-stream-id ctx))
         (h2-stream (ag-http2:multiplexer-get-stream ...)))
    (when (and h2-stream (ag-http2:stream-rst-stream-error h2-stream))
      (error 'grpc-status-error
             :code +grpc-status-cancelled+      ; ← CANCELLED mapping
             :message "Cancelled by client (RST_STREAM)"
             ...)))

  ;; Check other cl-context cancellation
  (handler-case
      (cl-context:check-context cl-ctx)
    (cl-context:context-cancelled (e)
      (error 'grpc-status-error
             :code +grpc-status-cancelled+      ; ← CANCELLED mapping
             :message (format nil "~A" e)
             ...))))
```

**2. Client-Side Cancellation → CANCELLED**:
```lisp
;; In call.lisp - Call-level cancellation handling
(defun call-unary (channel method request &key metadata timeout response-type)
  (multiple-value-bind (ctx cancel-fn)
      (if effective-timeout
          (cl-context:with-timeout ...)
          (values (cl-context:ensure-context) nil))
    (unwind-protect
         (cl-context:with-context (ctx ctx)
           (handler-case
               (if effective-timeout
                   (bt2:with-timeout (effective-timeout) ...)
                   ...)
             ;; Deadline handled by with-stream-timeout in streaming ops
             ((or bt2:timeout cl-context:context-deadline-exceeded) (e)
               (error 'grpc-status-error
                      :code +grpc-status-deadline-exceeded+ ...))
             ;; Cancellation mapped here
             (cl-context:context-cancelled (e)
               (channel-cancel-stream channel stream-id)
               (error 'grpc-status-error
                      :code +grpc-status-cancelled+      ; ← CANCELLED mapping
                      :message "Cancelled"
                      ...))))
      (when cancel-fn (funcall cancel-fn)))))
```

**3. Stream-Level Cancellation** (for streaming calls):
- User calls cancel function explicitly
- `context-cancelled` propagates through stream operations
- Caught at call boundaries (call-server-stream, call-client-streaming, etc.)
- Mapped to `grpc-status-error` with `CANCELLED` code

**Summary - Complete Error Mapping**:

| Exception Source | Mapped By | gRPC Status Code | Where |
|-----------------|-----------|------------------|-------|
| `bt2:timeout` | `with-stream-timeout` | `DEADLINE_EXCEEDED` | Stream operations |
| `context-deadline-exceeded` | `with-stream-timeout` | `DEADLINE_EXCEEDED` | Stream operations |
| `context-cancelled` (explicit) | Call-level handler | `CANCELLED` | call-unary/call-*-stream |
| RST_STREAM | `context-ensure-not-cancelled` | `CANCELLED` | Server handlers |

**Key Points**:
- ✅ Deadline → `with-stream-timeout` → DEADLINE_EXCEEDED
- ✅ Cancellation → Call-level handler → CANCELLED
- ✅ RST_STREAM → `context-ensure-not-cancelled` → CANCELLED
- ✅ Complete mapping documented

## Previous Issues Resolved (10th Iteration)

The 10th review iteration identified 3 final clarification issues. All are now resolved:

### Issue #1: grpc-status-error Definition Location ✓

**Problem**: Plan suggested adding `:cause` slot to `grpc-status-error`, but didn't specify where the condition is currently defined or how to avoid duplicate definitions.

**Solution**: Modify existing condition definition in `ag-grpc/status.lisp` (lines 73-92).

**Canonical Location**: `ag-grpc/status.lisp`

**Current Definition** (lines 73-92):
```lisp
(define-condition grpc-status-error (error)
  ((code :initarg :code :reader grpc-status-error-code ...)
   (message :initarg :message :reader grpc-status-error-message ...)
   (details :initarg :details :reader grpc-status-error-details ...)
   (headers :initarg :headers :reader grpc-status-error-headers ...)
   (trailers :initarg :trailers :reader grpc-status-error-trailers ...))
  ...)
```

**Updated Definition** (add `:cause` slot):
```lisp
;; In ag-grpc/status.lisp (lines 73-92) - MODIFY EXISTING DEFINITION

(define-condition grpc-status-error (error)
  ((code :initarg :code :reader grpc-status-error-code
         :documentation "gRPC status code")
   (message :initarg :message :reader grpc-status-error-message
            :initform nil
            :documentation "Error message")
   (details :initarg :details :reader grpc-status-error-details
            :initform nil
            :documentation "Additional error details")
   (headers :initarg :headers :reader grpc-status-error-headers
            :initform nil
            :documentation "Response headers")
   (trailers :initarg :trailers :reader grpc-status-error-trailers
             :initform nil
             :documentation "Response trailers")
   ;; NEW: Preserve original exception for debugging
   (cause :initarg :cause :reader grpc-status-error-cause
          :initform nil
          :documentation "Original condition that caused this error (for debugging)"))
  (:report (lambda (c s)
             (format s "gRPC error ~A (~A)~@[: ~A~]"
                     (grpc-status-error-code c)
                     (grpc-status-name (grpc-status-error-code c))
                     (grpc-status-error-message c))
             ;; Show cause chain if present
             (when (grpc-status-error-cause c)
               (format s "~%  Caused by: ~A" (grpc-status-error-cause c))))))
```

**Key Points**:
- **Modify existing definition** in `ag-grpc/status.lisp`, don't create new one
- **Reader name**: `grpc-status-error-cause` (consistent with naming convention)
- **Location**: After existing slots, before `:report`
- **No duplicate definitions**: Single canonical definition

### Issue #2: Backward Compatibility of :cause Slot ✓

**Problem**: Need to ensure existing code that constructs `grpc-status-error` without `:cause` continues to work.

**Solution**: `:cause` slot has `:initform nil`, making it optional.

**Compatibility Analysis**:

1. **Existing constructors without `:cause` - Work unchanged**:
```lisp
;; Old code - still works
(error 'grpc-status-error
       :code +grpc-status-internal+
       :message "Internal error")
;; Result: cause slot initialized to NIL automatically
```

2. **New constructors with `:cause` - Enhanced debugging**:
```lisp
;; New code - with cause
(handler-case
    (some-operation)
  (bt2:timeout (c)
    (error 'grpc-status-error
           :code +grpc-status-deadline-exceeded+
           :message "Deadline exceeded"
           :cause c)))  ; Original condition preserved
```

3. **Accessor always works**:
```lisp
;; Safe to call on any grpc-status-error
(let ((cause (grpc-status-error-cause error)))
  (when cause
    (format t "Original: ~A~%" cause)))
;; Returns NIL for errors without cause (safe)
```

4. **Tests assuming fixed slot set**:
   - Old tests that check slot names still work (`:cause` just appears as additional slot)
   - Old tests that create errors without `:cause` still work (slot defaults to NIL)
   - **No breaking changes**

**Compatibility Guarantee**: All existing code that constructs, catches, or inspects `grpc-status-error` continues to work unchanged. `:cause` is purely additive.

### Issue #3: Macro Doesn't Map context-cancelled ✓

**Problem**: `with-stream-timeout` maps deadline exceptions but not `cl-context:context-cancelled`. Need to clarify that this is intentional and where cancellation is handled.

**Solution**: Document explicitly that the macro handles **deadlines only**, not cancellations.

**Rationale**:

1. **Separation of Concerns**:
   - **Deadline errors** (timeout): Mapped to `DEADLINE_EXCEEDED` by `with-stream-timeout`
   - **Cancellation errors**: Handled separately by higher-level code

2. **Where Cancellation is Handled**:
   - **Server-side**: `context-ensure-not-cancelled` in handler code
   - **Client-side**: Cancel function called explicitly, or caught at call boundaries
   - **RST_STREAM**: Detected via `context-check-cancelled` → maps to `CANCELLED`

3. **Why Not in Macro**:
   - Cancellation is **explicit** (user calls cancel function)
   - Deadline is **implicit** (time passes)
   - Different error codes: `DEADLINE_EXCEEDED` vs `CANCELLED`
   - Different handling semantics in application code

**Updated Macro Documentation**:
```lisp
(defmacro with-stream-timeout ((stream) &body body)
  "Execute BODY with timeout enforcement from STREAM's context.

Three-branch pattern:
1. Positive remaining → bt2:with-timeout with double-float coercion
2. Zero/negative remaining → check-context for immediate DEADLINE_EXCEEDED
3. No deadline → cooperative checking only

Maps DEADLINE exceptions only:
- bt2:timeout → grpc-status-error with DEADLINE_EXCEEDED
- cl-context:context-deadline-exceeded → grpc-status-error with DEADLINE_EXCEEDED

Does NOT map cancellation exceptions:
- cl-context:context-cancelled is handled by higher-level code
- Use context-ensure-not-cancelled for explicit cancellation checking
- RST_STREAM handled via context-check-cancelled → CANCELLED status

Preserves original condition in :cause slot for debugging.
..."
```

**Summary**:
- ✅ Macro maps **deadline exceptions** (timeout-related)
- ❌ Macro does NOT map **cancellation exceptions** (explicit cancel)
- ✅ Cancellation handled at higher level (context-ensure-not-cancelled, RST_STREAM detection)
- ✅ Clear separation: deadline vs cancellation errors

## Previous Issues Resolved (9th Iteration)

The 9th review iteration identified 3 final refinement issues. All are now resolved:

### Issue #1: Macro Placement and Compile Order ✓

**Problem**: Plan described `with-stream-timeout` macro but didn't specify where it should be defined relative to wrapped functions. Compile-order issues could arise if macro is defined after its usage.

**Solution**: Define macro early in `ag-grpc/call.lisp`, before any function definitions that use it.

**Implementation**:

**File Structure for `ag-grpc/call.lisp`**:
```lisp
;;;; call.lisp - gRPC client call implementation

(in-package #:ag-grpc)

;; ========================================================================
;; Stream Timeout Wrapper Macro (MUST be defined early for compile order)
;; ========================================================================

(defmacro with-stream-timeout ((stream) &body body)
  "Execute BODY with timeout enforcement from STREAM's context.

  ... [full docstring] ...")

;; ========================================================================
;; Class Definitions
;; ========================================================================

(defclass grpc-call () ...)
(defclass grpc-server-stream () ...)
(defclass grpc-client-stream () ...)
(defclass grpc-bidi-stream () ...)

;; ========================================================================
;; Stream Functions (use with-stream-timeout macro)
;; ========================================================================

(defun stream-receive-headers (server-stream)
  (with-stream-timeout (server-stream)
    (stream-receive-headers-internal server-stream)))

;; ... rest of wrapped functions
```

**Rationale**:
- **Macros must be defined before use** for compile-time expansion
- Defining at top ensures availability for all functions in file
- Clear section separation for maintainability
- No circular dependencies or forward declarations needed

### Issue #2: cl-context Deadline Exception Mapping ✓

**Problem**: Macro only maps `bt2:timeout` to `grpc-status-error`. When `check-context` signals `cl-context:context-deadline-exceeded`, that exception isn't caught and mapped, leading to inconsistent error types.

**Solution**: Update macro to catch and map both `bt2:timeout` AND `cl-context:context-deadline-exceeded` to `grpc-status-error`.

**Updated Macro**:
```lisp
(defmacro with-stream-timeout ((stream) &body body)
  "Execute BODY with timeout enforcement from STREAM's context.

Three-branch pattern:
1. Positive remaining → bt2:with-timeout with double-float coercion
2. Zero/negative remaining → check-context for immediate DEADLINE_EXCEEDED
3. No deadline → cooperative checking only

Maps BOTH bt2:timeout and cl-context:context-deadline-exceeded to
grpc-status-error with DEADLINE_EXCEEDED for consistent error handling.

Nested wrappers are safe: if BODY calls another wrapped function, the inner
wrapper catches and converts exceptions first. The outer wrapper's handlers
won't fire (already converted to grpc-status-error), so no duplicate error
conversion occurs."
  (let ((ctx-var (gensym "CTX"))
        (deadline-var (gensym "DEADLINE"))
        (remaining-var (gensym "REMAINING")))
    `(let ((,ctx-var (stream-cl-context ,stream)))
       (cl-context:with-context (,ctx-var ,ctx-var)
         (let* ((,deadline-var (cl-context:deadline ,ctx-var))
                (,remaining-var (when ,deadline-var
                                  (- ,deadline-var (cl-context:get-current-time)))))
           (cond
             ;; Positive remaining → preemptive timeout
             ((and ,remaining-var (> ,remaining-var 0))
              (handler-case
                  (bt2:with-timeout ((coerce ,remaining-var 'double-float))
                    ,@body)
                ;; Map bt2 timeout to gRPC error
                (bt2:timeout (c)
                  (error 'grpc-status-error
                         :code +grpc-status-deadline-exceeded+
                         :message "Deadline exceeded"
                         :cause c))  ; Preserve original condition
                ;; Map cl-context deadline exceeded to gRPC error
                (cl-context:context-deadline-exceeded (c)
                  (error 'grpc-status-error
                         :code +grpc-status-deadline-exceeded+
                         :message (format nil "Deadline exceeded: ~A" c)
                         :cause c))))  ; Preserve original condition
             ;; Deadline passed → check immediately
             (,deadline-var
              (handler-case
                  (progn
                    (cl-context:check-context ,ctx-var)
                    ,@body)
                ;; Map cl-context deadline exceeded to gRPC error
                (cl-context:context-deadline-exceeded (c)
                  (error 'grpc-status-error
                         :code +grpc-status-deadline-exceeded+
                         :message (format nil "Deadline exceeded: ~A" c)
                         :cause c))))
             ;; No deadline → cooperative only
             (t
              ,@body)))))))
```

**Key Changes**:
- ✅ Catches **both** `bt2:timeout` and `cl-context:context-deadline-exceeded`
- ✅ Maps both to `grpc-status-error` with `DEADLINE_EXCEEDED` code
- ✅ Consistent error type regardless of timeout source
- ✅ Preserves original condition in `:cause` slot for debugging

**Rationale**:
- Both timeout sources should produce same gRPC error type
- User code only needs to catch `grpc-status-error`, not both condition types
- Consistent API: timeout source is implementation detail

### Issue #3: Stack Trace Preservation for Debugging ✓

**Problem**: Converting `bt2:timeout` and `cl-context:context-deadline-exceeded` to `grpc-status-error` loses original exception context, making debugging harder.

**Solution**: Add `:cause` slot to `grpc-status-error` condition to preserve original condition.

**Updated grpc-status-error Condition**:
```lisp
;; In ag-grpc/errors.lisp or ag-grpc/call.lisp

(define-condition grpc-status-error (error)
  ((code :initarg :code :reader grpc-error-code
         :documentation "gRPC status code (integer)")
   (message :initarg :message :reader grpc-error-message
            :documentation "Error message string")
   (headers :initarg :headers :reader grpc-error-headers
            :initform nil
            :documentation "Response headers (alist)")
   (trailers :initarg :trailers :reader grpc-error-trailers
             :initform nil
             :documentation "Response trailers (alist)")
   (cause :initarg :cause :reader grpc-error-cause
          :initform nil
          :documentation "Original condition that caused this error (for debugging)"))
  (:report (lambda (condition stream)
             (format stream "gRPC error ~D: ~A"
                     (grpc-error-code condition)
                     (grpc-error-message condition))
             (when (grpc-error-cause condition)
               (format stream "~%  Caused by: ~A"
                       (grpc-error-cause condition)))))
  (:documentation "gRPC status error condition"))
```

**Usage in Macro**:
```lisp
;; Preserve original condition
(bt2:timeout (c)
  (error 'grpc-status-error
         :code +grpc-status-deadline-exceeded+
         :message "Deadline exceeded"
         :cause c))  ; Original bt2:timeout condition
```

**Debugging Benefits**:
- ✅ Original exception preserved in `:cause` slot
- ✅ Full stack trace available via `(grpc-error-cause error)`
- ✅ Error report shows both gRPC error and underlying cause
- ✅ No information loss during error conversion
- ✅ Compatible with existing error handling (`:cause` is optional)

**Example Debugging Session**:
```lisp
(handler-case
    (stream-receive-message stream)
  (grpc-status-error (e)
    (format t "gRPC error: ~A~%" e)
    (when (grpc-error-cause e)
      (format t "Original cause: ~A~%" (grpc-error-cause e))
      (format t "Stack trace: ~A~%" (trivial-backtrace:print-backtrace
                                      (grpc-error-cause e))))))
```

**Backward Compatibility**:
- `:cause` slot defaults to NIL (optional)
- Existing code that creates `grpc-status-error` without `:cause` continues to work
- Existing error handlers unaffected

## Previous Issues Resolved (8th Iteration)

The 8th review iteration identified 3 refinement issues. All are now resolved with a shared helper macro:

### Issue #1: Exception Mapping for bt2 Timeouts Must Be Consistent ✓

**Problem**: The plan showed `bt2:timeout` → `grpc-status-error` mapping in one snippet but didn't explicitly show it for all five blocking operations. Need consistency across all wrappers.

**Solution**: Create a shared macro `with-stream-timeout` that encapsulates the three-branch pattern and error mapping, then use it consistently across all blocking operations.

**Implementation**:

```lisp
;; In ag-grpc/call.lisp - Shared timeout wrapper macro

(defmacro with-stream-timeout ((stream) &body body)
  "Execute BODY with timeout enforcement from STREAM's context.

Three-branch pattern:
1. Positive remaining → bt2:with-timeout with double-float coercion
2. Zero/negative remaining → check-context for immediate DEADLINE_EXCEEDED
3. No deadline → cooperative checking only

Maps bt2:timeout to grpc-status-error with DEADLINE_EXCEEDED.
Use this macro for all blocking stream operations."
  (let ((ctx-var (gensym "CTX"))
        (deadline-var (gensym "DEADLINE"))
        (remaining-var (gensym "REMAINING")))
    `(let ((,ctx-var (stream-cl-context ,stream)))
       (cl-context:with-context (,ctx-var ,ctx-var)
         (let* ((,deadline-var (cl-context:deadline ,ctx-var))
                (,remaining-var (when ,deadline-var
                                  (- ,deadline-var (cl-context:get-current-time)))))
           (cond
             ;; Positive remaining → preemptive timeout
             ((and ,remaining-var (> ,remaining-var 0))
              (handler-case
                  (bt2:with-timeout ((coerce ,remaining-var 'double-float))
                    ,@body)
                (bt2:timeout ()
                  (error 'grpc-status-error
                         :code +grpc-status-deadline-exceeded+
                         :message "Deadline exceeded"))))
             ;; Deadline passed → check immediately
             (,deadline-var
              (cl-context:check-context ,ctx-var)
              ,@body)
             ;; No deadline → cooperative only
             (t
              ,@body)))))))
```

**Rationale**:
- **Single source of truth** for timeout logic and error mapping
- **Consistent** across all 5 blocking operations
- **No drift** - changes to timeout logic happen in one place
- **Clear documentation** in macro docstring
- **Handles nested wrappers** - see Issue #3 below

### Issue #2: Nested Wrapper Overhead Acceptable but Unquantified ✓

**Problem**: Plan accepts nested wrappers but doesn't quantify overhead or provide rationale for why it's acceptable.

**Solution**: Document overhead analysis and rationale.

**Overhead Analysis**:

1. **Nested Timeout Cost**:
   - `bt2:with-timeout` establishes a timer thread (or uses existing scheduler)
   - Inner timeout created even though outer timeout exists
   - Cost: ~microseconds per timeout setup (minimal for I/O-bound operations)
   - **Context**: gRPC calls typically take milliseconds to seconds (network I/O)

2. **Actual Nesting in Practice**:
   - `stream-receive-message` may call `stream-receive-headers` **once** (first message only)
   - `stream-receive-message` may call `stream-finish` **once** (last message only)
   - Typical streaming call: 1 header read + N message reads + 1 finish
   - **Nested overhead**: 2 extra timeouts per stream (amortized over all messages)

3. **Comparative Overhead**:
   - Network I/O: milliseconds to seconds
   - Timeout setup: microseconds
   - Ratio: 1:1000 to 1:1000000
   - **Negligible** compared to actual I/O time

4. **Alternative Cost**:
   - Adding "already wrapped" flag: complexity + maintenance burden
   - Creating `-internal` variants: doubles function count + harder to maintain
   - **Trade-off**: Micro-optimization for unmeasurable gain vs. significant code complexity

**Rationale for Accepting Nested Wrappers**:
- ✅ **Correct**: Both use same deadline, safe behavior
- ✅ **Simple**: No special cases or complexity
- ✅ **Future-proof**: New functions automatically get timeout protection
- ✅ **Maintainable**: Clear pattern, easy to understand
- ✅ **Negligible overhead**: μs cost vs. ms-to-s I/O time
- ❌ Alternative: Complex, unmaintainable, no measurable performance gain

**Conclusion**: Nested wrapper overhead is **unmeasurable in practice** and far outweighed by code simplicity benefits.

### Issue #3: Nested Wrappers May Double-Map Errors ✓

**Problem**: If `stream-receive-message` (outer) calls `stream-receive-headers` (inner), and both wrap with `handler-case` for `bt2:timeout`, the inner wrapper might convert the timeout to `grpc-status-error`, then the outer wrapper tries to catch `bt2:timeout` again (which won't fire).

**Solution**: Error mapping happens at the **first** (innermost) wrapper that catches the timeout. Outer wrappers see the already-converted `grpc-status-error` and propagate it unchanged.

**Analysis**:

1. **Call chain**: `stream-receive-message` → `stream-receive-headers` (nested)
2. **Inner wrapper** (stream-receive-headers):
   - Catches `bt2:timeout`
   - Converts to `grpc-status-error` with DEADLINE_EXCEEDED
   - Signals the gRPC error
3. **Outer wrapper** (stream-receive-message):
   - `bt2:timeout` never reached (already converted)
   - `grpc-status-error` propagates through unchanged
   - No duplicate conversion

**Key Insight**: `handler-case` only catches exceptions raised within its body. Once inner handler converts `bt2:timeout` → `grpc-status-error`, the outer `bt2:timeout` handler never fires.

**Behavior**:
```lisp
;; Pseudo-code showing exception flow
(handler-case                               ; OUTER wrapper
    (stream-receive-message-internal ...)
  (bt2:timeout () ...))                     ; Won't fire - inner already converted

  ;; Inside stream-receive-message-internal:
  (handler-case                             ; INNER wrapper
      (stream-receive-headers-internal ...)
    (bt2:timeout ()                         ; Fires first
      (error 'grpc-status-error ...)))      ; Converts to gRPC error

;; Result: INNER converts, OUTER sees grpc-status-error (not bt2:timeout)
```

**Verification**: With shared `with-stream-timeout` macro:
- All wrappers use identical error handling
- Inner wrapper catches and converts first
- Outer wrapper's `bt2:timeout` handler is a no-op (but harmless)
- **No duplicate conversion** - only first (innermost) handler runs

**Documentation Note**: Add comment in macro that nested wrappers are safe because conversion happens at innermost level only.

## Previous Issues Resolved (7th Iteration)

The 7th review iteration identified 4 refinement issues. All are now resolved:

### Issue #1: Deadline Already Expired Still Allows Blocking I/O ✓

**Problem**: When `remaining <= 0`, the current pattern skips `bt2:with-timeout` and directly calls the internal function. This means a call could still block indefinitely even though the deadline has already passed.

**Solution**: Explicitly check the context status before calling the internal function when there's no time remaining.

**Implementation Pattern**:
```lisp
(defun stream-receive-message (server-stream)
  "Receive the next message from a server stream with timeout enforcement."
  (let ((ctx (stream-cl-context server-stream)))
    (cl-context:with-context (ctx ctx)
      (let* ((deadline (cl-context:deadline ctx))
             (remaining (when deadline
                          (- deadline (cl-context:get-current-time)))))
        (cond
          ;; Positive remaining time → wrap with preemptive timeout
          ((and remaining (> remaining 0))
           (handler-case
               (bt2:with-timeout ((coerce remaining 'double-float))
                 (stream-receive-message-internal server-stream))
             (bt2:timeout ()
               ;; Map bt2 timeout to gRPC deadline exceeded
               (error 'grpc-status-error
                      :code +grpc-status-deadline-exceeded+
                      :message "Deadline exceeded"))))

          ;; Deadline exists but already passed → check immediately
          (deadline
           (cl-context:check-context ctx)  ; Will signal deadline-exceeded
           ;; If we reach here, context was OK - shouldn't happen with deadline
           (stream-receive-message-internal server-stream))

          ;; No deadline → just cooperative checking
          (t
           (stream-receive-message-internal server-stream)))))))
```

**Key Points**:
- Three branches: positive remaining (preemptive), zero/negative remaining (immediate check), no deadline (cooperative)
- `check-context` signals `context-deadline-exceeded` immediately if deadline passed
- Ensures DEADLINE_EXCEEDED error even if deadline expired before operation starts
- No indefinite blocking when deadline already passed

### Issue #2: stream-cl-context Slot Must Exist on All Stream Types ✓

**Problem**: The plan introduced `stream-cl-context` accessor but didn't explicitly show the slot definition on all stream classes. If any stream lacks the slot, the `:around` method will fail.

**Solution**: Document the slot addition for all three stream classes with complete definitions.

**Implementation**:

```lisp
;; In ag-grpc/call.lisp

(defclass grpc-server-stream ()
  ((channel :initarg :channel :accessor stream-call-channel)
   (stream-id :initarg :stream-id :accessor stream-call-stream-id)
   (method :initarg :method :accessor stream-call-method)
   (call :initarg :call :accessor stream-call)

   ;; NEW: cl-context integration
   (cl-context :initarg :cl-context
               :accessor stream-cl-context
               :documentation "Context for cancellation/deadlines")
   (cancel-fn :initarg :cancel-fn
              :accessor stream-cancel-fn
              :initform nil
              :documentation "Cancel function for cleanup")

   (headers-received-p :initform nil :accessor stream-headers-received-p)
   (finished-p :initform nil :accessor stream-finished-p)
   ...))

(defclass grpc-client-stream ()
  ((call :initarg :call :accessor stream-call)
   (channel :initarg :channel :accessor client-stream-channel)
   (stream-id :initarg :stream-id :accessor client-stream-id)

   ;; NEW: cl-context integration
   (cl-context :initarg :cl-context
               :accessor stream-cl-context
               :documentation "Context for cancellation/deadlines")
   (cancel-fn :initarg :cancel-fn
              :accessor stream-cancel-fn
              :initform nil
              :documentation "Cancel function for cleanup")

   (closed-p :initform nil :accessor client-stream-closed-p)
   ...))

(defclass grpc-bidi-stream ()
  ((call :initarg :call :accessor stream-call)
   (channel :initarg :channel :accessor bidi-stream-channel)
   (stream-id :initarg :stream-id :accessor bidi-stream-id)

   ;; NEW: cl-context integration
   (cl-context :initarg :cl-context
               :accessor stream-cl-context
               :documentation "Context for cancellation/deadlines")
   (cancel-fn :initarg :cancel-fn
              :accessor stream-cancel-fn
              :initform nil
              :documentation "Cancel function for cleanup")

   (send-closed-p :initform nil :accessor bidi-stream-send-closed-p)
   ...))
```

**Rationale**:
- All three stream classes now explicitly have `stream-cl-context` and `stream-cancel-fn` slots
- Common accessor name `stream-cl-context` works for all types
- `:around` method can safely access slot on any stream type
- No runtime errors from missing slots

### Issue #3: bt2:with-timeout Accepts Positive Real; Remaining Can Be Rational ✓

**Problem**: `cl-context` uses rational numbers for time (fractional seconds). Need to ensure `bt2:with-timeout` accepts rationals, or coerce to float.

**Solution**: Coerce remaining time to `double-float` when passing to `bt2:with-timeout`.

**Implementation**:
```lisp
;; Always coerce rational to float for bt2:with-timeout
(when (and remaining (> remaining 0))
  (bt2:with-timeout ((coerce remaining 'double-float))
    (stream-receive-message-internal server-stream)))
```

**Rationale**:
- `bt2:with-timeout` expects a real number (may not optimize rationals)
- `coerce` to `double-float` ensures compatibility
- Minimal overhead (coercion happens once per operation)
- Safe: rationals convert cleanly to floats for timeout values

**Note**: `bordeaux-threads-2` documentation confirms `bt2:with-timeout` accepts any real number, but coercion to float is defensive and ensures consistent behavior across implementations.

### Issue #4: Nested Wrappers in Call Paths ✓

**Problem**: `stream-receive-message` internally calls `stream-receive-headers` and `stream-finish`, which are also wrapped with timeout. This creates nested timeouts that may be redundant.

**Solution**: Wrapper functions detect whether they're called from top-level or nested context, and only apply timeout at top-level.

**Analysis**:
1. **`stream-receive-message`** calls:
   - `stream-receive-headers` (first call only, if not already received)
   - `stream-finish` (when END_STREAM detected)

2. **Nested timeout behavior**:
   - Inner timeout uses same remaining deadline
   - Inner timeout <= outer timeout (both use same deadline source)
   - If inner times out, outer catches it immediately
   - Functionally correct but adds overhead

**Decision**: **Accept nested wrappers as-is** - they're safe and simpler than alternative approaches.

**Rationale**:
- **Safe**: Both timeouts use same deadline source, so inner <= outer
- **Correct**: If inner times out, error propagates immediately
- **Simple**: No need for "already wrapped" flag or internal variants
- **Minimal overhead**: Timeout is only set once per operation (bt2 implementation detail)
- **Future-proof**: Each function is independently timeout-protected

**Alternative (Rejected)**: Create `-internal` variants that skip timeout wrapping:
- More complex (doubles number of functions)
- Harder to maintain (must remember which variant to call)
- Breaks encapsulation (internal functions exposed)
- Minimal performance gain

**Conclusion**: Keep simple pattern where every public function wraps with timeout. Nested calls are safe and correct.

## Previous Issues Resolved (6th Iteration)

The 6th review iteration identified 4 remaining issues. All are now resolved with concrete implementation strategies:

### Issue #1: Client-Side Streaming Preemptive Timeout Enforcement ✓

**Problem**: Stream operations wrapped with `with-context` only provide cooperative cancellation. Blocking I/O operations like `stream-receive-message`, `stream-close-and-recv`, and `stream-read-message` can still block indefinitely without preemptive interruption.

**Solution**: Each blocking stream operation calculates the remaining deadline from the cl-context and wraps the actual I/O with `bt2:with-timeout` for that remaining duration.

**Implementation Pattern**:
```lisp
(defun stream-receive-message (server-stream)
  "Receive the next message from a server stream with timeout enforcement."
  (let ((ctx (stream-cl-context server-stream)))
    (cl-context:with-context (ctx ctx)
      ;; Calculate remaining deadline
      (let* ((deadline (cl-context:deadline ctx))
             (remaining (when deadline
                          (- deadline (cl-context:get-current-time)))))
        (if (and remaining (> remaining 0))
            ;; Wrap with bt2:with-timeout for preemptive interruption
            (bt2:with-timeout (remaining)
              (stream-receive-message-internal server-stream))
            ;; No deadline or already passed - just cooperative checking
            (stream-receive-message-internal server-stream))))))
```

**Apply to All Blocking Stream Operations**:
- `stream-receive-headers` - waits for headers
- `stream-receive-message` - blocks on message receive
- `stream-finish` - waits for trailers
- `stream-close-and-recv` - blocks until response received
- `stream-read-message` - blocks on bidi message receive

**Non-Blocking Operations** (no bt2:with-timeout needed):
- `stream-send` - non-blocking write to buffer
- `stream-close-send` - closes send side (non-blocking)

**Rationale**:
- Provides both preemptive (bt2) and cooperative (cl-context) cancellation
- Remaining deadline calculation ensures timeout enforcement throughout stream lifetime
- Operations that fail bt2:timeout will be caught and mapped to gRPC deadline error
- Zero or negative remaining time → skip bt2:with-timeout, rely on cooperative checks

### Issue #2: Wrapper List Count Inconsistency ✓

**Problem**: Plan says "7 functions" but lists 8 line numbers (263, 287, 311, 454, 467, 617, 632, 648).

**Resolution**:
- **7 unique function names**: `stream-receive-headers`, `stream-receive-message`, `stream-finish`, `stream-send`, `stream-close-and-recv`, `stream-close-send`, `stream-read-message`
- **8 method implementations**: `stream-send` appears twice (line 454: grpc-client-stream method, line 617: grpc-bidi-stream method)

**Clarification**: The count is correct either way:
- 7 function/generic names
- 8 actual method definitions in the source code

Updated wrapper list to reflect this distinction.

### Issue #3: stream-send Generic Method Wrapping Strategy ✓

**Problem**: `stream-send` is a generic function with multiple methods (client-stream and bidi-stream). Need to specify wrapping strategy to avoid duplicating context binding code or changing method combination behavior.

**Solution**: Use a single `:around` method on the generic function to provide context binding for all specialized methods.

**Implementation**:
```lisp
;; Define generic function
(defgeneric stream-send (stream message)
  (:documentation "Send a message on a stream"))

;; Add :around method for context binding
(defmethod stream-send :around ((stream t) message)
  "Wrap all stream-send methods with context binding"
  (let ((ctx (stream-cl-context stream)))
    (cl-context:with-context (ctx ctx)
      (call-next-method))))

;; Existing primary methods unchanged
(defmethod stream-send ((client-stream grpc-client-stream) message)
  "Send a message on a client stream"
  ;; Existing implementation - context already bound by :around method
  ...)

(defmethod stream-send ((bidi-stream grpc-bidi-stream) message)
  "Send a message on a bidirectional stream"
  ;; Existing implementation - context already bound by :around method
  ...)
```

**Rationale**:
- Single `:around` method applies to all specialized methods automatically
- Avoids code duplication
- No changes to method combination or dispatch
- If new stream types are added later, they automatically get context binding
- Clean separation: `:around` handles cross-cutting concern (context), primary methods handle logic

### Issue #4: Double-Cancel Coordination ✓

**Problem**: `cancel-fn` is stored on gRPC stream objects, and cleanup callbacks are registered on HTTP/2 streams. Both `finalize-client-stream` (called explicitly) and the HTTP/2 cleanup callback (called on stream close) might try to cancel, potentially causing double-cancel errors or spurious log messages.

**Solution**: Explicit idempotency contract - both paths check for non-nil `cancel-fn` before calling, and clear it after first invocation.

**Implementation**:

**Client-Side** (`finalize-client-stream` in ag-grpc/call.lisp):
```lisp
(defun finalize-client-stream (stream)
  "Clean up stream and cancel its context.
  Safe to call multiple times (idempotent).

  Idempotency contract:
  - Check cancel-fn is non-nil before calling
  - Clear cancel-fn to nil after calling
  - Subsequent calls are no-ops"
  (let ((cancel-fn (stream-cancel-fn stream)))
    (when cancel-fn
      ;; Call cancel function
      (funcall cancel-fn)
      ;; Clear to prevent double-cancel (CRITICAL)
      (setf (stream-cancel-fn stream) nil))))
```

**Server-Side** (cleanup callback in ag-grpc/server.lisp):
```lisp
;; In server-handle-headers - register cleanup callback
(setf (ag-http2:stream-cleanup-callback h2-stream)
      (lambda (stream)
        (declare (ignore stream))
        ;; Same idempotency pattern as client-side
        (let ((cancel-fn (context-cancel-fn ctx)))
          (when cancel-fn
            (funcall cancel-fn)
            ;; Clear to prevent double-cancel (CRITICAL)
            (setf (context-cancel-fn ctx) nil)))))
```

**Coordination Rules**:
1. **First-Wins**: Whoever calls `cancel-fn` first does the cleanup
2. **Clear After Call**: Must set `cancel-fn` to nil after calling
3. **Check Before Call**: Always check `(when cancel-fn ...)` before calling
4. **Thread-Safety**: Both paths may run from different threads, but clearing slot is atomic enough for our purposes (worst case: both call cancel, which should be idempotent in cl-context)

**Scenarios**:
- **Normal completion**: User calls `finalize-client-stream` → clears cancel-fn → HTTP/2 callback is no-op
- **Stream closed remotely**: HTTP/2 callback fires first → clears cancel-fn → later finalize is no-op
- **Error path**: Either path may fire first, the other becomes no-op
- **Race condition**: Both paths check non-nil and call → cl-context cancel-fn should be idempotent anyway

**Testing Requirements**:
- Verify `finalize-client-stream` can be called multiple times safely
- Verify cleanup callback + finalize doesn't double-cancel
- Verify cancel-fn is cleared in all paths
- Check for spurious error messages in logs

## cl-context API Verification (Issue #3 from C-REVIEW.md)

**Confirmed from cl-context README (lines 168-358)**:

- `(with-timeout parent seconds)` → `(values context cancel-function)` ✓
- `(with-deadline parent deadline-time)` → `(values context cancel-function)` ✓
- `(with-cancel parent)` → `(values context cancel-function)` ✓
- `(done-p context)` → boolean ✓
- `(err context)` → condition or nil (`context-cancelled` or `context-deadline-exceeded`) ✓
- `(check-context &optional context)` → signals error if done ✓
- `(value context key &optional default)` → value ✓
- `(with-context (var context) &body body)` - binds both var and `*current-context*` ✓
- `(get-current-time)` → rational (fractional seconds since epoch) ✓
- `(ensure-context &optional context)` → context (Issue #1 fix: was missing from list) ✓
  - Returns context if non-nil, else `*current-context*`, else new `background` context

Convenience macros also available:
- `with-timeout-context` - auto-calls cancel
- `with-deadline-context` - auto-calls cancel
- `with-cancel-context` - auto-calls cancel

## Critical Issues from C-REVIEW.md

### 1. Blocking I/O Cannot Be Interrupted by `check-context` Alone

**Problem**: `cl-context:check-context` only works at explicit check points. It won't interrupt a thread blocked in `channel-receive-headers` waiting on a condition variable.

**Current Code** (call.lisp:86-100):
```lisp
(if effective-timeout
    (bt2:with-timeout (effective-timeout)
      (do-receive))
    (do-receive))
```

**Solution**: Keep `bt2:with-timeout` as the outer enforcement layer, but use `cl-context` for cooperative checking and cross-layer coordination.

**Implementation**:
```lisp
(defun call-unary (channel method request &key metadata timeout response-type)
  (ensure-connected channel)
  (let* ((stream (channel-new-stream channel))
         (stream-id (ag-http2:stream-id stream))
         (call (make-instance 'grpc-call ...))
         (effective-timeout (or timeout (channel-default-timeout channel))))

    ;; Create cl-context for this call
    (multiple-value-bind (ctx cancel-fn)
        (if effective-timeout
            (cl-context:with-timeout (cl-context:ensure-context) effective-timeout)
            (values (cl-context:ensure-context) nil))
      (unwind-protect
           ;; Bind context and wrap with bt2:with-timeout for hard deadline
           (cl-context:with-context (ctx ctx)
             (handler-case
                 (if effective-timeout
                     ;; bt2:with-timeout provides preemptive interruption
                     (bt2:with-timeout (effective-timeout)
                       (call-unary-internal channel call stream-id method request
                                          metadata response-type))
                     (call-unary-internal channel call stream-id method request
                                        metadata response-type))
               ;; Map both timeout mechanisms to same gRPC error
               ((or bt2:timeout cl-context:context-deadline-exceeded) (e)
                 (declare (ignore e))
                 (channel-cancel-stream channel stream-id)
                 (setf (call-status call) +grpc-status-deadline-exceeded+)
                 (setf (call-status-message call) "Deadline exceeded")
                 (error 'grpc-status-error
                        :code +grpc-status-deadline-exceeded+
                        :message "Deadline exceeded"
                        :headers (call-response-headers call)
                        :trailers nil))
               ;; Handle explicit cancellation
               (cl-context:context-cancelled (e)
                 (declare (ignore e))
                 (channel-cancel-stream channel stream-id)
                 (setf (call-status call) +grpc-status-cancelled+)
                 (setf (call-status-message call) "Cancelled")
                 (error 'grpc-status-error
                        :code +grpc-status-cancelled+
                        :message "Cancelled"
                        :headers (call-response-headers call)
                        :trailers nil))))
        ;; Always clean up context
        (when cancel-fn (funcall cancel-fn))))))
```

**Rationale**: This layered approach provides:
- **Preemptive timeout** via `bt2:with-timeout` (existing behavior preserved)
- **Cooperative cancellation** via `cl-context` (enables clean checks)
- **Unified error handling** for both mechanisms
- **Cross-thread coordination** (child operations can check `*current-context*`)

### 1a. Timeout Formatter Audit and Migration (Issue #4)

**AUDIT RESULTS**: Only one call site of `format-grpc-timeout` in request generation:
- `ag-grpc/metadata.lisp:235` - in `make-request-headers`

**Migration Plan**:
1. Replace line 235 with call to `format-grpc-timeout-spec-compliant`
2. Keep `format-grpc-timeout` function with **conditional** deprecation warning (Issue #5):
```lisp
(defparameter *warn-deprecated-timeout-formatter* nil
  "When T, emit deprecation warnings for format-grpc-timeout.
  Set to T in development, NIL in production/tests to avoid noise.")

(defun format-grpc-timeout (seconds)
  "Format timeout for grpc-timeout header.
  DEPRECATED: Use format-grpc-timeout-spec-compliant for spec compliance.
  This function may emit non-compliant headers for large timeouts.

  Note: Deprecation warnings are controlled by *warn-deprecated-timeout-formatter*.
  Set it to T to see warnings during development."
  (when *warn-deprecated-timeout-formatter*
    (warn "format-grpc-timeout is deprecated. Use format-grpc-timeout-spec-compliant."))
  ;; Existing implementation...
  (cond
    ((< seconds 1)
     (format nil "~Dm" (round (* seconds 1000))))
    ((< seconds 60)
     (format nil "~DS" (round seconds)))
    ((< seconds 3600)
     (format nil "~DM" (round (/ seconds 60))))
    (t
     (format nil "~DH" (round (/ seconds 3600))))))
```

**Deprecation Warning Strategy** (Issue #5):
- Default: **NIL** (no warnings) - prevents test noise
- Development: Set `(setf *warn-deprecated-timeout-formatter* t)` to see warnings
- External users: Can enable warnings to find their own usages
- Internal ag-grpc: Single call site already being replaced
- After 2 releases: Consider removing or making warnings unconditional

3. No other uses in codebase - only one replacement needed

### 2. Time Base Consistency

**Problem**: Server uses `get-universal-time` (wall clock), but `cl-context` uses monotonic time via `precise-time:get-monotonic-time`.

**Current Code** (server.lisp:361-364):
```lisp
(let ((timeout-header (cdr (assoc "grpc-timeout" headers :test #'string-equal))))
  (when timeout-header
    (setf (context-deadline ctx)
          (+ (get-universal-time) (parse-grpc-timeout timeout-header)))))
```

**cl-context Time API** (from cl-context README:311-323):
```lisp
(cl-context:get-current-time)  ; => rational number (fractional seconds since epoch)
```

**Solution**: Use `cl-context:get-current-time` consistently throughout ag-gRPC.

**Implementation**:

```lisp
;; In metadata.lisp, add conversion utilities
(defun grpc-current-time ()
  "Get current time in seconds (rational) compatible with cl-context"
  (cl-context:get-current-time))

(defun deadline-to-grpc-timeout (deadline)
  "Convert absolute deadline to relative timeout in seconds.
  Returns NIL if deadline already passed (Issue #4 fix)."
  (when deadline
    (let ((remaining (- deadline (grpc-current-time))))
      (when (> remaining 0)
        remaining))))

;; NEW: Spec-compliant timeout formatter (Issue #4 fix)
(defun format-grpc-timeout-spec-compliant (seconds)
  "Format timeout for grpc-timeout header per gRPC spec.
  Enforces 8-digit maximum and chooses appropriate unit.
  Spec: timeout = 1*8DIGIT TimeoutUnit
  TimeoutUnit = \"H\" / \"M\" / \"S\" / \"m\" / \"u\" / \"n\""
  (when (and seconds (> seconds 0))
    (cond
      ;; Hours (if >= 360000 seconds = 100 hours)
      ((>= seconds 360000)
       (let ((hours (min 99999999 (floor seconds 3600))))
         (format nil "~DH" hours)))
      ;; Minutes (if >= 6000 seconds = 100 minutes)
      ((>= seconds 6000)
       (let ((minutes (min 99999999 (floor seconds 60))))
         (format nil "~DM" minutes)))
      ;; Seconds (if >= 1 second)
      ((>= seconds 1)
       (let ((secs (min 99999999 (floor seconds))))
         (format nil "~DS" secs)))
      ;; Milliseconds (if >= 0.001 seconds)
      ((>= seconds 0.001)
       (let ((millis (min 99999999 (floor (* seconds 1000)))))
         (format nil "~Dm" millis)))
      ;; Microseconds (if >= 0.000001 seconds)
      ((>= seconds 0.000001)
       (let ((micros (min 99999999 (floor (* seconds 1000000)))))
         (format nil "~Du" micros)))
      ;; Nanoseconds (minimum precision)
      (t
       (let ((nanos (min 99999999 (floor (* seconds 1000000000)))))
         (format nil "~Dn" nanos))))))

(defun format-grpc-timeout (seconds)
  "Format timeout for grpc-timeout header.
  DEPRECATED: Use format-grpc-timeout-spec-compliant.
  Kept for backward compatibility."
  ;; Existing implementation (unchanged for now)
  (cond
    ((< seconds 1)
     (format nil "~Dm" (round (* seconds 1000))))
    ((< seconds 60)
     (format nil "~DS" (round seconds)))
    ((< seconds 3600)
     (format nil "~DM" (round (/ seconds 60))))
    (t
     (format nil "~DH" (round (/ seconds 3600))))))
```

```lisp
;; In server.lisp, update deadline parsing (line 361-364)
(let ((timeout-header (cdr (assoc "grpc-timeout" headers :test #'string-equal))))
  (when timeout-header
    (let ((timeout-seconds (parse-grpc-timeout timeout-header)))
      (setf (context-deadline ctx)
            (+ (grpc-current-time) timeout-seconds)))))
```

```lisp
;; In call.lisp, update header generation (Issues #4 + #5 fix)
(defun channel-send-headers (channel stream-id method &key metadata timeout ...)
  (let* ((timeout-seconds (or timeout
                             (deadline-to-grpc-timeout
                              (cl-context:deadline cl-context:*current-context*))))
         (headers (make-request-headers
                   method
                   ;; NIL timeout means omit header (Issue #5 fix)
                   :timeout timeout-seconds
                   :metadata metadata
                   ...)))
    ...))

;; In metadata.lisp, update make-request-headers (Issues #4 + #5 fix)
(defun make-request-headers (method &key timeout metadata authority tls)
  "Create standard gRPC request headers.
  TIMEOUT - Timeout in seconds (rational), or NIL to omit header.
  If TLS is true, use https scheme."
  (let* ((custom-encoding (and metadata
                               (metadata-get metadata "grpc-encoding")))
         (headers (list (cons :method "POST")
                       (cons :scheme (if tls "https" "http"))
                       (cons :path method)
                       (cons :authority (or authority ""))
                       (cons "content-type" *grpc-content-type*)
                       (cons "te" "trailers")
                       (cons "user-agent" *grpc-user-agent*)
                       (cons "grpc-encoding" (or custom-encoding *grpc-encoding*))
                       (cons "grpc-accept-encoding" *grpc-accept-encoding*))))
    ;; Only add grpc-timeout if timeout is non-NIL and positive
    (when (and timeout (> timeout 0))
      (setf headers (append headers
                            (list (cons "grpc-timeout"
                                       (format-grpc-timeout-spec-compliant timeout))))))
    ;; Add custom metadata
    (when metadata
      (dolist (entry (metadata-entries metadata))
        (let ((key (car entry))
              (value (cdr entry)))
          (unless (string-equal key "grpc-encoding")
            (if (binary-metadata-key-p key)
                (setf headers (append headers
                                      (list (cons key (encode-binary-metadata value)))))
                (setf headers (append headers (list entry))))))))
    headers))
```

**Rationale**:
- Single time base eliminates conversion errors
- `cl-context:get-current-time` provides nanosecond precision (better than `get-universal-time`)
- Graceful fallback when cl-context not available
- Existing `format-grpc-timeout` and `parse-grpc-timeout` work unchanged

### 3. gRPC Context Contains cl-context (Not Unified)

**Problem**: `grpc-call-context` has its own cancellation/deadline tracking that needs to coordinate with `cl-context`.

**Current Code** (server.lisp:132-190):
```lisp
(defclass grpc-call-context ()
  ((connection :initarg :connection ...)
   (stream-id :initarg :stream-id ...)
   (deadline :initform nil :accessor context-deadline ...)
   (cancelled-p :initform nil :accessor context-cancelled-p ...)
   ...))

(defun context-check-cancelled (ctx)
  "Check if RST_STREAM received"
  (unless (context-cancelled-p ctx)
    (let* ((h2-stream ...))
      (when (ag-http2:stream-rst-stream-error h2-stream)
        (setf (context-cancelled-p ctx) t))))
  (context-cancelled-p ctx))
```

**Solution**: Add `cl-context` slot and delegate cancellation checks to BOTH sources.

**BREAKING CHANGE FIX (Issue #1)**: Keep `context-check-cancelled` as a pure predicate (backward compatible), add new `context-ensure-not-cancelled` for signaling.

**Implementation**:

```lisp
;; In server.lisp, update grpc-call-context
(defclass grpc-call-context ()
  ((connection :initarg :connection :reader context-connection ...)
   (stream-id :initarg :stream-id :reader context-stream-id ...)
   (method-path :initarg :method :reader context-method-path ...)
   (request-headers :initarg :headers :reader context-request-headers ...)
   (request-metadata :initform nil :accessor context-request-metadata ...)
   (peer-address :initarg :peer-address :reader context-peer-address ...)

   ;; cl-context integration
   (cl-context :initarg :cl-context
               :accessor context-cl-context
               :initform (cl-context:background)
               :documentation "Context for cancellation/deadlines")
   (cancel-fn :initarg :cancel-fn
              :accessor context-cancel-fn
              :initform nil
              :documentation "Cancel function to clean up context (Issue #5 fix)")

   ;; Keep existing fields for backward compatibility
   (deadline :initform nil :accessor context-deadline
             :documentation "Absolute deadline (seconds, rational)")
   (deadline-synced-p :initform nil :accessor context-deadline-synced-p
                      :documentation "T if deadline cached from cl-context")
   (cancelled-p :initform nil :accessor context-cancelled-p
                :documentation "Cached cancellation state")

   ;; Response state (mutable)
   (response-headers-sent-p :initform nil :accessor context-response-headers-sent-p ...)
   (response-metadata :initform nil :accessor context-response-metadata ...)
   (trailing-metadata :initform nil :accessor context-trailing-metadata ...)
   ...)
  (:documentation "Context for an RPC call, passed to handlers"))

;; Sync deadline from cl-context on first access (Issue #2 fix)
(defmethod context-deadline :around ((ctx grpc-call-context))
  "Sync deadline from cl-context on first access"
  (unless (context-deadline-synced-p ctx)
    (let ((ctx-deadline (cl-context:deadline (context-cl-context ctx))))
      (when ctx-deadline
        (setf (slot-value ctx 'deadline) ctx-deadline)))
    (setf (context-deadline-synced-p ctx) t))
  (call-next-method))

;; BACKWARD COMPATIBLE: Pure predicate with no side effects (Issue #1 + #3 fix)
(defun context-check-cancelled (ctx)
  "Check if cancelled via RST_STREAM or cl-context.
  Updates and returns context-cancelled-p. Never signals.

  Pure predicate: uses done-p + err, not check-context (avoids side effects).
  For signaling behavior, use context-ensure-not-cancelled."
  (unless (context-cancelled-p ctx)
    ;; Check HTTP/2 RST_STREAM
    (let* ((stream-id (context-stream-id ctx))
           (h2-stream (ag-http2:multiplexer-get-stream
                       (ag-http2:connection-multiplexer (context-connection ctx))
                       stream-id)))
      (when (and h2-stream (ag-http2:stream-rst-stream-error h2-stream))
        (setf (context-cancelled-p ctx) t)))

    ;; Check cl-context cancellation (Issue #3 fix: use done-p, not check-context)
    (when (cl-context:done-p (context-cl-context ctx))
      (setf (context-cancelled-p ctx) t)))

  (context-cancelled-p ctx))

;; NEW FUNCTION: Signaling version (Issue #1 + #3 + #5 fix)
(defun context-ensure-not-cancelled (ctx)
  "Check if cancelled and signal grpc-status-error if so.
  Use this when you want to propagate cancellation as an error.

  Uses check-context (may have side effects like logging).
  For polling behavior, use context-check-cancelled.

  Cancellation precedence (Issue #5 fix):
  1. Deadline exceeded takes precedence (deterministic, time-based)
  2. RST_STREAM second (client-initiated cancellation)
  This ensures deadline errors are reported even if RST_STREAM also present."

  ;; First check deadline (highest precedence)
  (let ((cl-ctx (context-cl-context ctx)))
    (when (cl-context:done-p cl-ctx)
      (let ((err (cl-context:err cl-ctx)))
        (when (typep err 'cl-context:context-deadline-exceeded)
          (error 'grpc-status-error
                 :code +grpc-status-deadline-exceeded+
                 :message (format nil "~A" err)
                 :headers (context-request-headers ctx)
                 :trailers nil)))))

  ;; Then check RST_STREAM (second precedence)
  (let* ((stream-id (context-stream-id ctx))
         (h2-stream (ag-http2:multiplexer-get-stream
                     (ag-http2:connection-multiplexer (context-connection ctx))
                     stream-id)))
    (when (and h2-stream (ag-http2:stream-rst-stream-error h2-stream))
      (error 'grpc-status-error
             :code +grpc-status-cancelled+
             :message "Cancelled by client (RST_STREAM)"
             :headers (context-request-headers ctx)
             :trailers nil)))

  ;; Finally check other cl-context cancellation
  (handler-case
      (cl-context:check-context cl-ctx)
    (cl-context:context-cancelled (e)
      (error 'grpc-status-error
             :code +grpc-status-cancelled+
             :message (format nil "~A" e)
             :headers (context-request-headers ctx)
             :trailers nil))))
```

**Rationale**:
- Preserves existing `RST_STREAM` detection (backward compatible)
- Adds `cl-context` cancellation as second source
- Both mechanisms feed into unified `context-cancelled-p` state
- Handlers can call existing `context-check-cancelled` unchanged

### 4. Server Handler Context Creation and Lifetime

**Problem**: Need to bind `cl-context` when creating server call contexts.

**CRITICAL (Issue #5)**: Context lifetime must match handler execution, not just the dispatch stack frame. For streaming handlers that may spawn threads or return streams, context must remain valid.

**Current Code** (server.lisp:354-376):
```lisp
(let ((ctx (make-instance 'grpc-call-context
                          :connection conn
                          :stream-id stream-id
                          :method method-path
                          :headers headers
                          :peer-address peer-addr)))
  (let ((timeout-header (cdr (assoc "grpc-timeout" headers :test #'string-equal))))
    (when timeout-header
      (setf (context-deadline ctx)
            (+ (get-universal-time) (parse-grpc-timeout timeout-header)))))
  ...)
```

**Solution**: Create `cl-context` with deadline and bind it to `*current-context*`.

**Implementation**:

```lisp
(defun server-handle-headers (server conn frame peer-addr)
  "Handle incoming HEADERS frame (new RPC request)"
  (let* ((stream-id (ag-http2:frame-stream-id frame))
         (h2-stream (ag-http2:multiplexer-get-stream
                     (ag-http2:connection-multiplexer conn)
                     stream-id))
         (headers (ag-http2:stream-headers h2-stream))
         (method-path (cdr (assoc :path headers)))
         (handler (server-get-handler server method-path)))

    (unless handler
      (server-send-error conn stream-id +grpc-status-unimplemented+
                         (format nil "Method not found: ~A" method-path))
      (return-from server-handle-headers))

    ;; Parse timeout and create cl-context with deadline
    (let* ((timeout-header (cdr (assoc "grpc-timeout" headers :test #'string-equal)))
           (timeout-seconds (when timeout-header
                             (parse-grpc-timeout timeout-header)))
           (deadline (when timeout-seconds
                      (+ (grpc-current-time) timeout-seconds))))

      ;; Create cl-context with deadline if present
      (multiple-value-bind (call-ctx cancel-fn)
          (if deadline
              (cl-context:with-deadline (cl-context:background) deadline)
              (values (cl-context:background) nil))

        ;; IMPORTANT (Issue #5): Don't cancel context immediately in unwind-protect.
        ;; For streaming handlers, the context must remain valid as long as the
        ;; stream is active. Instead:
        ;; 1. Store cancel-fn in grpc-call-context
        ;; 2. Call cancel when stream closes (via stream-finalize or connection cleanup)
        ;; For unary handlers, cancel happens when dispatch completes.

        ;; Create grpc-call-context with cl-context embedded
        (let ((ctx (make-instance 'grpc-call-context
                                  :connection conn
                                  :stream-id stream-id
                                  :method method-path
                                  :headers headers
                                  :peer-address peer-addr
                                  :cl-context call-ctx
                                  :cancel-fn cancel-fn)))  ; Store for later cleanup

          ;; Set deadline for backward compatibility
          (when deadline
            (setf (context-deadline ctx) deadline))

          ;; Extract and store compression encodings
          (let ((request-encoding (cdr (assoc "grpc-encoding" headers
                                             :test #'string-equal))))
            (when (and request-encoding
                      (not (string-equal request-encoding "identity")))
              (setf (context-request-encoding ctx) request-encoding)))

          (let ((accept-encoding (cdr (assoc "grpc-accept-encoding" headers
                                            :test #'string-equal))))
            (when (and accept-encoding (search "gzip" accept-encoding
                                              :test #'char-equal))
              (setf (context-response-encoding ctx) "gzip")))

          ;; Store context for DATA frame handling
          (setf (stream-call-context h2-stream) ctx)
          (setf (stream-handler h2-stream) handler)

          ;; Bind cl-context and dispatch handler
          (cl-context:with-context (call-ctx call-ctx)
            (let ((is-streaming (or (handler-client-streaming-p handler)
                                   (handler-server-streaming-p handler))))
              (if is-streaming
                  ;; Streaming: context remains bound, cancel happens when stream closes
                  (handler-case
                      (server-dispatch-handler server conn ctx handler nil)
                    (error (e)
                      (format *error-output* "~&gRPC handler error: ~A~%" e)))
                  ;; Unary: cancel after dispatch completes
                  (unwind-protect
                       (when (plusp (logand (ag-http2:frame-flags frame)
                                           ag-http2:+flag-end-stream+))
                         (server-dispatch-handler server conn ctx handler nil))
                    (when cancel-fn (funcall cancel-fn)))))))))))
```

**Context Lifetime Contract (Issue #5)**:

1. **Unary handlers**: Context is cancelled immediately after handler returns
2. **Streaming handlers**: Context remains valid until stream is closed
3. **Stream cleanup**: Must call `context-cancel-fn` when stream finishes
4. **Connection cleanup**: Must call `context-cancel-fn` for any open streams

**Exact Hook Points with Callback Interface (Issue #2 fix - Circular Dependency)**:

**CRITICAL**: Cannot make `ag-http2` call `ag-grpc` functions directly (circular dependency). Instead, use a callback slot that `ag-grpc` registers.

**1. Add cleanup callback to HTTP/2 stream** - `ag-http2/streams.lisp`:
```lisp
;; MODIFY: Add cleanup-callback slot to http2-stream
(defclass http2-stream ()
  ((stream-id :initarg :stream-id :accessor stream-id ...)
   (state :initform :idle :accessor stream-state ...)
   (cleanup-callback :initform nil :accessor stream-cleanup-callback
                     :documentation "Optional callback invoked when stream closes")
   ...))

;; MODIFY: ag-http2:multiplexer-close-stream (line 153)
(defun multiplexer-close-stream (mux stream-id)
  "Mark a stream as closed and invoke cleanup callback"
  (let ((stream (gethash stream-id (multiplexer-streams mux))))
    (when stream
      ;; Call cleanup callback BEFORE changing state
      (let ((callback (stream-cleanup-callback stream)))
        (when callback
          (funcall callback stream)))
      (setf (stream-state stream) :closed))))
```

**Migration Compatibility Note (Issue #2)**:
- Adding `:initform nil` slot is backward compatible
- No `:initarg` specified - won't conflict with existing `make-instance` calls
- Default `nil` means "no callback" - safe fallback for existing code
- Only ag-grpc sets callback via `(setf (stream-cleanup-callback ...) callback)`
- ✓ No constructor changes required in ag-http2 codebase

**2. Register callback from ag-grpc** - `ag-grpc/server.lisp`:
```lisp
;; NEW: Register cleanup callback when creating context
(defun server-handle-headers (server conn frame peer-addr)
  ...
  (let ((ctx (make-instance 'grpc-call-context
                            ...
                            :cl-context call-ctx
                            :cancel-fn cancel-fn)))
    ...
    ;; Register cleanup callback with HTTP/2 stream
    (setf (ag-http2:stream-cleanup-callback h2-stream)
          (lambda (stream)
            (declare (ignore stream))
            (when cancel-fn (funcall cancel-fn))))
    ...))
```

**2. Connection Close Hook** - `ag-http2/connection.lisp:387`:
```lisp
;; MODIFY: ag-http2:connection-close (line 387)
(defun connection-close (conn &key (error-code +error-no-error+) debug-data)
  "Close connection and invoke cleanup callbacks for all streams"
  (let ((mux (connection-multiplexer conn)))
    ;; Invoke cleanup callbacks for all streams BEFORE closing
    (maphash
     (lambda (stream-id stream)
       (declare (ignore stream-id))
       (let ((callback (stream-cleanup-callback stream)))
         (when callback
           (funcall callback stream))))
     (multiplexer-streams mux)))
  ;; ... existing connection-close logic ...
  )
```

No ag-grpc changes needed - callbacks already registered on streams.

**3. Server Connection Loop Cleanup** - `ag-grpc/server.lisp:309`:
```lisp
;; MODIFY: server-connection-loop
(defun server-connection-loop (server conn peer-addr)
  "Process frames for a connection until closed"
  (unwind-protect
       (loop while (eq (ag-http2:connection-state conn) :open)
             do (handler-case
                    (let ((frame (ag-http2:connection-read-frame conn)))
                      (when frame
                        (server-process-frame server conn frame peer-addr)))
                  (ag-http2:http2-connection-error (e)
                    (declare (ignore e))
                    (return))
                  (end-of-file ()
                    (return))
                  (error (e)
                    (format *error-output* "Frame processing error: ~A~%" e)
                    (return))))
    ;; Cleanup: connection-close will invoke callbacks
    ;; No explicit ag-grpc code needed here
    ))
```

**4. RST_STREAM Handler** - `ag-http2/connection.lisp:374-381` (Issue #3 - Specific Location):

**Existing RST_STREAM processing already correct**:
```lisp
;; CURRENT CODE (line 374-381):
(rst-stream-frame
 (let* ((stream-id (frame-stream-id frame))
        (error-code (rst-stream-frame-error-code frame))
        (stream (multiplexer-get-stream (connection-multiplexer conn) stream-id)))
   ;; Store the error code before closing the stream
   (when stream
     (setf (stream-rst-stream-error stream) error-code))
   ;; This call will invoke cleanup-callback - NO CHANGES NEEDED
   (multiplexer-close-stream (connection-multiplexer conn) stream-id)))
```

**Why no changes needed**:
- Line 381 calls `multiplexer-close-stream`
- `multiplexer-close-stream` modified to invoke `cleanup-callback` (see section 1)
- Callback automatically invoked when RST_STREAM received
- Error code already stored (line 380) before callback fires
- ✓ RST_STREAM cleanup already wired correctly via callback mechanism

**Rationale**:
- Deadline from `grpc-timeout` header creates `cl-context` with deadline
- `*current-context*` is bound for entire handler execution
- Handler can call `(cl-context:check-context)` for cooperative cancellation
- Context lifetime matches stream lifetime (not stack frame)
- Cleanup is explicit and tied to stream/connection lifecycle

### 5. All Client-Side Call Types (Issue #7 + New Issue #1)

**Problem**: Plan only shows `call-unary`, but there are multiple client call types that all need context binding.

**CRITICAL NEW ISSUE #1**: Streaming calls return stream objects that are used outside the dynamic extent of `with-context`. Context must remain accessible for the entire stream lifetime.

**Solution**: Store `cl-context` on stream objects and bind it for all stream operations.

**Call Types in ag-grpc/call.lisp**:
- `call-unary` (line 49) - Already shown in plan
- `call-server-stream` (line 233) - Server streaming
- `call-client-streaming` (line 419) - Client streaming
- `call-bidirectional-streaming` (line 587) - Bidirectional streaming

**Implementation Pattern** (apply to all):

```lisp
(defun call-server-stream (channel method request &key metadata timeout response-type)
  "Initiate a server streaming RPC call."
  (ensure-connected channel)
  (let* ((effective-timeout (or timeout (channel-default-timeout channel))))

    ;; Create cl-context for this call
    (multiple-value-bind (ctx cancel-fn)
        (if effective-timeout
            (cl-context:with-timeout (cl-context:ensure-context) effective-timeout)
            (values (cl-context:ensure-context) nil))

      (unwind-protect
           (cl-context:with-context (ctx ctx)
             (handler-case
                 (if effective-timeout
                     (bt2:with-timeout (effective-timeout)
                       (call-server-stream-internal channel method request
                                                   metadata response-type))
                     (call-server-stream-internal channel method request
                                                metadata response-type))
               ((or bt2:timeout cl-context:context-deadline-exceeded) (e)
                 (declare (ignore e))
                 (error 'grpc-status-error :code +grpc-status-deadline-exceeded+ ...))
               (cl-context:context-cancelled (e)
                 (declare (ignore e))
                 (error 'grpc-status-error :code +grpc-status-cancelled+ ...))))

        (when cancel-fn (funcall cancel-fn))))))
```

**Apply same pattern to**:
- `call-client-streaming`
- `call-bidirectional-streaming`

**Client Streaming Context Binding Strategy (Issue #1 fix)**:

Since streaming operations happen outside the initial `with-context` scope, we must store the context on the stream object and bind it for each operation.

**Add cl-context slot to stream classes:**
```lisp
;; In call.lisp
(defclass grpc-server-stream ()
  ((channel :initarg :channel :accessor stream-call-channel)
   (stream-id :initarg :stream-id :accessor stream-call-stream-id)
   (method :initarg :method :accessor stream-call-method)
   (cl-context :initarg :cl-context :accessor stream-cl-context  ; NEW
               :documentation "Context for this stream")
   (cancel-fn :initarg :cancel-fn :accessor stream-cancel-fn      ; NEW
              :initform nil
              :documentation "Cancel function for cleanup")
   ...))

(defclass grpc-client-stream ()
  ((call :initarg :call :accessor stream-call)
   (channel :initarg :channel :accessor client-stream-channel)
   (stream-id :initarg :stream-id :accessor client-stream-id)
   (cl-context :initarg :cl-context :accessor stream-cl-context  ; NEW
               :documentation "Context for this stream")
   (cancel-fn :initarg :cancel-fn :accessor stream-cancel-fn      ; NEW
              :initform nil)
   ...))

(defclass grpc-bidi-stream ()
  ((call :initarg :call :accessor stream-call)
   (channel :initarg :channel :accessor bidi-stream-channel)
   (stream-id :initarg :stream-id :accessor bidi-stream-id)
   (cl-context :initarg :cl-context :accessor stream-cl-context  ; NEW
               :documentation "Context for this stream")
   (cancel-fn :initarg :cancel-fn :accessor stream-cancel-fn      ; NEW
              :initform nil)
   ...))
```

**Wrap stream operations with context:**
```lisp
;; Pattern for all stream operations
(defun stream-receive-message (server-stream)
  "Receive the next message from a server stream."
  (let ((ctx (stream-cl-context server-stream)))
    (cl-context:with-context (ctx ctx)
      ;; Now *current-context* is bound for this operation
      (stream-receive-message-internal server-stream))))

(defmethod stream-send ((client-stream grpc-client-stream) message)
  "Send a message on a client stream."
  (let ((ctx (stream-cl-context client-stream)))
    (cl-context:with-context (ctx ctx)
      (stream-send-internal client-stream message))))

(defmethod stream-read-message ((bidi-stream grpc-bidi-stream))
  "Read the next message from a bidirectional stream."
  (let ((ctx (stream-cl-context bidi-stream)))
    (cl-context:with-context (ctx ctx)
      (stream-read-message-internal bidi-stream))))
```

**Create streams with context:**
```lisp
(defun call-server-stream (channel method request &key metadata timeout response-type)
  "Initiate a server streaming RPC call."
  (ensure-connected channel)
  (let* ((effective-timeout (or timeout (channel-default-timeout channel))))
    ;; Create cl-context that outlives this function
    (multiple-value-bind (ctx cancel-fn)
        (if effective-timeout
            (cl-context:with-timeout (cl-context:ensure-context) effective-timeout)
            (values (cl-context:ensure-context) nil))

      ;; Store context on stream object (not in unwind-protect!)
      (let ((server-stream (make-instance 'grpc-server-stream
                                          ...
                                          :cl-context ctx
                                          :cancel-fn cancel-fn)))
        ;; Return stream with embedded context
        server-stream))))

;; User code - context automatically bound for each operation
(let ((stream (call-server-stream channel method request :timeout 30)))
  (loop for msg = (stream-receive-message stream)  ; context bound here
        while msg
        do (process msg))
  ;; Must call cleanup when done
  (finalize-client-stream stream))
```

**Client-Side Stream Cleanup Trigger Points (Issue #4)**:

```lisp
(defun finalize-client-stream (stream)
  "Clean up stream and cancel its context.
  Safe to call multiple times (idempotent)."
  (let ((cancel-fn (stream-cancel-fn stream)))
    (when cancel-fn
      (funcall cancel-fn)
      ;; Clear to prevent double-cancel
      (setf (stream-cancel-fn stream) nil))))
```

**When `finalize-client-stream` is called**:

1. **Server Streaming** - `stream-receive-message` returns NIL:
```lisp
(defun stream-receive-message (server-stream)
  (let ((ctx (stream-cl-context server-stream)))
    (cl-context:with-context (ctx ctx)
      (let ((msg (stream-receive-message-internal server-stream)))
        ;; Finalize when stream ends (NIL return)
        (when (and (null msg) (stream-finished-p server-stream))
          (finalize-client-stream server-stream))
        msg))))
```

2. **Client Streaming** - `stream-close-and-recv` completion:
```lisp
(defun stream-close-and-recv (client-stream)
  (let ((ctx (stream-cl-context client-stream)))
    (cl-context:with-context (ctx ctx)
      (unwind-protect
           (stream-close-and-recv-internal client-stream)
        ;; Always finalize, even on error
        (finalize-client-stream client-stream)))))
```

3. **Bidirectional Streaming** - User closes or stream ends:
```lisp
(defmacro with-bidi-stream ((var channel method &rest args) &body body)
  `(let ((,var (call-bidirectional-streaming ,channel ,method ,@args)))
     (unwind-protect
          (progn ,@body)
       ;; Finalize on normal exit or error
       (finalize-client-stream ,var))))
```

4. **Error Path** - Timeout or exception:
- All call functions use `unwind-protect` around `finalize-client-stream`
- Ensures cleanup even when bt2:timeout or cl-context errors occur

5. **Remote Close (END_STREAM)** - Stream exhausted:
- Detected in `stream-receive-message` when `(stream-finished-p ...)`
- Finalize called automatically before returning NIL

**Double-Cancel Safety**:
- `finalize-client-stream` clears `cancel-fn` after calling it
- Multiple calls are safe (idempotent)
- First call does cleanup, subsequent calls are no-ops

**Concrete Stream Function Wrappers (9th Iteration - Final with Complete Error Mapping)**:

**Functions to modify in `ag-grpc/call.lisp`**:

**Count Clarification**: 7 unique function names, 8 method implementations (stream-send appears twice)

**File Structure** (ag-grpc/call.lisp):
```lisp
;;;; call.lisp - gRPC client call implementation

(in-package #:ag-grpc)

;; ========================================================================
;; Stream Timeout Wrapper Macro (MUST be defined early for compile order)
;; ========================================================================
```

**New: Shared Timeout Wrapper Macro** (define at top of file, before class definitions):
```lisp
(defmacro with-stream-timeout ((stream) &body body)
  "Execute BODY with timeout enforcement from STREAM's context.

Three-branch pattern:
1. Positive remaining → bt2:with-timeout with double-float coercion
2. Zero/negative remaining → check-context for immediate DEADLINE_EXCEEDED
3. No deadline → cooperative checking only

Maps DEADLINE exceptions only (timeout-related):
- bt2:timeout → grpc-status-error with DEADLINE_EXCEEDED
- cl-context:context-deadline-exceeded → grpc-status-error with DEADLINE_EXCEEDED

Does NOT map cancellation exceptions:
- cl-context:context-cancelled is handled by higher-level code
- Use context-ensure-not-cancelled for explicit cancellation checking
- RST_STREAM handled via context-check-cancelled → CANCELLED status

Preserves original condition in :cause slot (grpc-status-error-cause) for debugging.

Nested wrappers are safe: if BODY calls another wrapped function, the inner
wrapper catches and converts exceptions first. The outer wrapper's handlers
won't fire (already converted to grpc-status-error), so no duplicate error
conversion occurs."
  (let ((ctx-var (gensym "CTX"))
        (deadline-var (gensym "DEADLINE"))
        (remaining-var (gensym "REMAINING")))
    `(let ((,ctx-var (stream-cl-context ,stream)))
       (cl-context:with-context (,ctx-var ,ctx-var)
         (let* ((,deadline-var (cl-context:deadline ,ctx-var))
                (,remaining-var (when ,deadline-var
                                  (- ,deadline-var (cl-context:get-current-time)))))
           (cond
             ;; Positive remaining → preemptive timeout
             ((and ,remaining-var (> ,remaining-var 0))
              (handler-case
                  (bt2:with-timeout ((coerce ,remaining-var 'double-float))
                    ,@body)
                ;; Map bt2 timeout to gRPC error
                (bt2:timeout (c)
                  (error 'grpc-status-error
                         :code +grpc-status-deadline-exceeded+
                         :message "Deadline exceeded"
                         :cause c))
                ;; Map cl-context deadline exceeded to gRPC error
                (cl-context:context-deadline-exceeded (c)
                  (error 'grpc-status-error
                         :code +grpc-status-deadline-exceeded+
                         :message (format nil "Deadline exceeded: ~A" c)
                         :cause c))))
             ;; Deadline passed → check immediately
             (,deadline-var
              (handler-case
                  (progn
                    (cl-context:check-context ,ctx-var)
                    ,@body)
                ;; Map cl-context deadline exceeded to gRPC error
                (cl-context:context-deadline-exceeded (c)
                  (error 'grpc-status-error
                         :code +grpc-status-deadline-exceeded+
                         :message (format nil "Deadline exceeded: ~A" c)
                         :cause c))))
             ;; No deadline → cooperative only
             (t
              ,@body)))))))
```

1. **`stream-receive-headers` (line 263)** - Server streaming (BLOCKING):
```lisp
(defun stream-receive-headers (server-stream)
  "Receive and return response headers with timeout enforcement."
  (with-stream-timeout (server-stream)
    (stream-receive-headers-internal server-stream)))

(defun stream-receive-headers-internal (server-stream)
  "Internal implementation - context already bound"
  (unless (stream-headers-received-p server-stream)
    ;; Existing implementation unchanged
    ...))
```

2. **`stream-receive-message` (line 287)** - Server streaming (BLOCKING):
```lisp
(defun stream-receive-message (server-stream)
  "Receive the next message from a server stream with timeout enforcement."
  (with-stream-timeout (server-stream)
    (stream-receive-message-internal server-stream)))

(defun stream-receive-message-internal (server-stream)
  "Internal implementation - context already bound.

  May call stream-receive-headers or stream-finish internally (both also wrapped).
  Nested wrappers are safe: inner wrapper converts bt2:timeout first, outer
  wrapper's handler won't fire. No duplicate error conversion."
  (when (stream-finished-p server-stream)
    (return-from stream-receive-message-internal nil))
  ;; Existing implementation unchanged
  ...)
```

3. **`stream-finish` (line 311)** - Server streaming (BLOCKING):
```lisp
(defun stream-finish (server-stream)
  "Finish the stream by receiving trailers with timeout enforcement."
  (with-stream-timeout (server-stream)
    (stream-finish-internal server-stream)))

(defun stream-finish-internal (server-stream)
  "Internal implementation"
  ;; Existing implementation...
  )
```

4. **`stream-send` (line 454 & 617)** - Client streaming & Bidi (NON-BLOCKING):
```lisp
;; Use :around method for context binding (applies to all methods)
(defmethod stream-send :around ((stream t) message)
  "Wrap all stream-send methods with context binding.
  No timeout needed - send operations are non-blocking."
  (let ((ctx (stream-cl-context stream)))
    (cl-context:with-context (ctx ctx)
      (call-next-method))))

;; Existing primary methods unchanged
(defmethod stream-send ((client-stream grpc-client-stream) message)
  "Send a message on a client stream."
  ;; Context already bound by :around method
  ;; Existing implementation unchanged
  ...)

(defmethod stream-send ((bidi-stream grpc-bidi-stream) message)
  "Send a message on a bidirectional stream."
  ;; Context already bound by :around method
  ;; Existing implementation unchanged
  ...)
```

5. **`stream-close-and-recv` (line 467)** - Client streaming (BLOCKING):
```lisp
(defun stream-close-and-recv (client-stream)
  "Close the client stream and receive response with timeout enforcement."
  (unwind-protect
       (with-stream-timeout (client-stream)
         (stream-close-and-recv-internal client-stream))
    ;; Clean up context when done (idempotent)
    (finalize-client-stream client-stream)))

(defun stream-close-and-recv-internal (client-stream)
  "Internal implementation"
  ;; Existing implementation...
  )
```

6. **`stream-close-send` (line 632)** - Bidi streaming (NON-BLOCKING):
```lisp
(defun stream-close-send (bidi-stream)
  "Close the send side of a bidirectional stream.
  Non-blocking operation - no timeout needed."
  (let ((ctx (stream-cl-context bidi-stream)))
    (cl-context:with-context (ctx ctx)
      (stream-close-send-internal bidi-stream))))

(defun stream-close-send-internal (bidi-stream)
  "Internal implementation"
  ;; Existing implementation...
  )
```

7. **`stream-read-message` (line 648)** - Bidi streaming (BLOCKING):
```lisp
(defmethod stream-read-message ((bidi-stream grpc-bidi-stream))
  "Read the next message from a bidirectional stream with timeout enforcement."
  (with-stream-timeout (bidi-stream)
    (stream-read-message-internal bidi-stream)))

(defun stream-read-message-internal (bidi-stream)
  "Internal implementation"
  ;; Existing implementation...
  )
```

**Context Slot Used**: `stream-cl-context` accessor on each stream object.

**Key Points** (8th Iteration - Shared Macro Approach):
- **7 unique function names, 8 method implementations** (stream-send counts as 2 methods)
- **Shared macro `with-stream-timeout`** encapsulates three-branch pattern:
  1. **Positive remaining** → `bt2:with-timeout` with coerced double-float
  2. **Zero/negative remaining** → `cl-context:check-context` for immediate error
  3. **No deadline** → cooperative checking only
- **Blocking operations** (5 functions) all use `with-stream-timeout` macro consistently
- **Non-blocking operations** (2 functions) wrapped only with `with-context` (no bt2 timeout needed)
- `stream-send` uses `:around` method for clean generic function wrapping
- **Consistent error mapping**: All blocking ops use same macro, ensuring uniform `bt2:timeout` → `grpc-status-error` conversion
- **No code drift**: Timeout logic and error handling centralized in one macro
- Every client call type gets layered timeout (bt2 + cl-context)
- **Rational to float coercion**: Handled in macro, all remaining times coerced to `double-float`
- **Nested wrappers are safe**: Inner wrapper converts `bt2:timeout` first, outer wrapper's handler won't fire (no duplicate conversion)
- **Negligible overhead**: μs timeout setup vs. ms-to-s I/O time (see Issue #2 analysis)
- **User must call finalization or use with-*-stream macros**
- All wrappers calculate remaining deadline dynamically to enforce timeout throughout stream lifetime
- **No indefinite blocking**: Expired deadlines trigger immediate DEADLINE_EXCEEDED error

### 6. Context Value Support for Request-Scoped Data

**Solution**: Define gRPC-specific context keys for common request-scoped values.

**Implementation**:

```lisp
;; In package.lisp, add exports
(:export
 ;; Context keys
 #:+grpc-request-id+
 #:+grpc-trace-context+
 #:+grpc-auth-token+
 #:+grpc-peer-address+
 #:grpc-context-value
 ...)

;; In new file: ag-grpc/context-values.lisp
(in-package #:ag-grpc)

;; Define standard context keys
(cl-context:define-context-key +grpc-request-id+
  "Unique request ID for tracing")
(cl-context:define-context-key +grpc-trace-context+
  "Distributed tracing context (trace-id, span-id)")
(cl-context:define-context-key +grpc-auth-token+
  "Authentication token from metadata")
(cl-context:define-context-key +grpc-peer-address+
  "Remote peer address")

(defun grpc-context-value (key &optional default)
  "Get a value from the current gRPC context.
  Wraps cl-context:value for convenience."
  (cl-context:value cl-context:*current-context* key default))

;; Helper to enrich context from metadata
(defun enrich-context-from-metadata (ctx headers peer-address)
  "Add request-scoped values to context from headers"
  (let ((enriched ctx))
    ;; Add peer address
    (setf enriched (cl-context:with-value enriched +grpc-peer-address+
                                          peer-address))

    ;; Extract trace context from headers (if present)
    (let ((trace-id (cdr (assoc "x-trace-id" headers :test #'string-equal))))
      (when trace-id
        (setf enriched (cl-context:with-value enriched +grpc-trace-context+
                                              trace-id))))

    ;; Extract auth token (if present)
    (let ((auth (cdr (assoc "authorization" headers :test #'string-equal))))
      (when auth
        (setf enriched (cl-context:with-value enriched +grpc-auth-token+
                                              auth))))

    enriched))
```

```lisp
;; Context enrichment integrated into canonical server-handle-headers
;; (Issue #3 fix: removed conflicting snippet)
;; See "4. Server Handler Context Creation and Lifetime" section above
;; for the complete, canonical implementation that properly handles
;; streaming lifetime.

;; Quick reference:
(let ((enriched-ctx (enrich-context-from-metadata call-ctx headers peer-addr)))
  (make-instance 'grpc-call-context
                 :cl-context enriched-ctx
                 :cancel-fn cancel-fn
                 ...))
;; Cleanup registered as callback - NOT in unwind-protect
```

**Usage in Handlers**:
```lisp
(defun my-handler (request ctx)
  ;; Access request-scoped values
  (let ((peer (grpc-context-value +grpc-peer-address+))
        (trace-id (grpc-context-value +grpc-trace-context+)))
    (format t "Request from ~A, trace: ~A~%" peer trace-id)
    ...))
```

**Rationale**:
- Immutable context values avoid parameter plumbing
- Standard keys for common patterns (tracing, auth)
- Easy to extend with application-specific keys
- Works across interceptors without explicit passing

## System Definition and Package Updates (Issue #7)

**CRITICAL**: cl-context is a hard dependency. These files MUST be updated:

### 1. System Definition - `ag-grpc.asd`:
```lisp
(asdf:defsystem #:ag-grpc
  :description "gRPC for Common Lisp"
  :version "1.0.0"
  :author "Your Name"
  :license "MIT"
  :depends-on (#:ag-http2
               #:ag-proto
               #:cl-context          ; NEW: Hard dependency
               #:bordeaux-threads
               #:usocket
               #:flexi-streams
               #:trivial-utf-8
               ...)
  :components ...)
```

### 2. Package Imports - `ag-grpc/package.lisp`:
```lisp
(defpackage #:ag-grpc
  (:use #:cl)
  ;; NEW: Import cl-context for use
  (:import-from #:cl-context
                #:*current-context*
                #:check-context
                #:with-context
                #:with-timeout
                #:with-deadline
                #:with-cancel
                #:with-value
                #:done-p
                #:err
                #:deadline
                #:get-current-time
                #:ensure-context
                #:define-context-key
                #:context-cancelled
                #:context-deadline-exceeded)
  (:export
   ;; Existing exports...

   ;; NEW: Context-related exports
   #:grpc-context-value
   #:context-ensure-not-cancelled

   ;; NEW: Context keys
   #:+grpc-request-id+
   #:+grpc-trace-context+
   #:+grpc-auth-token+
   #:+grpc-peer-address+

   ;; Keep existing context exports
   #:grpc-call-context
   #:context-metadata
   #:context-peer-address
   #:context-deadline
   #:context-cancelled-p
   #:context-check-cancelled
   #:context-set-response-metadata
   #:context-set-trailing-metadata
   ...))
```

### 3. New File - `ag-grpc/context-values.lisp`:
```lisp
;;;; context-values.lisp - cl-context integration for request-scoped values

(in-package #:ag-grpc)

;; Define standard context keys
(cl-context:define-context-key +grpc-request-id+
  "Unique request ID for tracing")
(cl-context:define-context-key +grpc-trace-context+
  "Distributed tracing context (trace-id, span-id)")
(cl-context:define-context-key +grpc-auth-token+
  "Authentication token from metadata")
(cl-context:define-context-key +grpc-peer-address+
  "Remote peer address")

(defun grpc-context-value (key &optional default)
  "Get a value from the current gRPC context.

  Accesses cl-context:*current-context*. Outside of server handlers
  or client calls, this will use the background context.

  See also: context-metadata (for accessing request headers)"
  (cl-context:value cl-context:*current-context* key default))
```

### 4. Add to `ag-grpc.asd` components:
```lisp
:components ((:file "package")
             (:file "constants" :depends-on ("package"))
             (:file "metadata" :depends-on ("package" "constants"))
             (:file "context-values" :depends-on ("package"))  ; NEW
             (:file "call" :depends-on ("package" "metadata" "context-values"))
             (:file "server" :depends-on ("package" "metadata" "context-values"))
             ...)
```

## Implementation Phases

### Phase 1: Foundation (No Breaking Changes)
- [ ] **Update `ag-grpc.asd`**: Add `:cl-context` dependency
- [ ] **Update `ag-grpc/package.lisp`**: Import cl-context symbols
- [ ] **Create `ag-grpc/context-values.lisp`**: Define context keys
- [ ] **Add `cleanup-callback` slot to `ag-http2:http2-stream`** (Issue #2 fix - circular dep)
- [ ] Add `grpc-current-time` function using `cl-context:get-current-time`
- [ ] Add `format-grpc-timeout-spec-compliant` with 8-digit enforcement (Issue #4)
- [ ] **Deprecate `format-grpc-timeout`** with warning (Issue #4)
- [ ] **Audit complete**: Only one call site at metadata.lisp:235 (Issue #4)
- [ ] Update `deadline-to-grpc-timeout` to return NIL when deadline passed (Issue #5)
- [ ] **MODIFY existing `grpc-status-error` in `ag-grpc/status.lisp`** (10th Iteration - Issue #1):
  - [ ] Add `:cause` slot with `:initform nil` (optional, backward compatible)
  - [ ] Reader: `grpc-status-error-cause` (consistent naming)
  - [ ] Update `:report` to show cause chain when present
  - [ ] Do NOT create duplicate definition elsewhere
- [ ] Add `cl-context` and `cancel-fn` slots to `grpc-call-context`
- [ ] **Add `cl-context` and `cancel-fn` slots to all three stream classes** (7th Iteration - Issue #2):
  - [ ] `grpc-server-stream` with `stream-cl-context` accessor
  - [ ] `grpc-client-stream` with `stream-cl-context` accessor
  - [ ] `grpc-bidi-stream` with `stream-cl-context` accessor
- [ ] Add `deadline-synced-p` slot to fix deadline sync
- [ ] Add `context-ensure-not-cancelled` with precedence: deadline > RST_STREAM > other (Issue #5)
- [ ] Keep `context-check-cancelled` as pure predicate using `done-p` (no side effects)
- [ ] **Add `with-stream-timeout` macro at top of call.lisp** (10th Iteration - Issues #2, #3):
  - [ ] Maps DEADLINE exceptions: `bt2:timeout` and `cl-context:context-deadline-exceeded`
  - [ ] Does NOT map cancellation: `cl-context:context-cancelled` handled elsewhere
  - [ ] Preserves original condition in `:cause` slot
  - [ ] Defined before class definitions for compile order

### Phase 2: Client-Side Integration
- [ ] **Update `make-request-headers` at metadata.lisp:235**: Replace `format-grpc-timeout` with spec-compliant version
- [ ] Handle NIL timeout correctly in `make-request-headers`
- [ ] Update `call-unary` with layered timeout (bt2 + cl-context)
- [ ] Update `call-server-stream` with layered timeout, store context on stream
- [ ] Update `call-client-streaming` with layered timeout, store context on stream
- [ ] Update `call-bidirectional-streaming` with layered timeout, store context on stream
- [ ] **Wrap 7 stream operations (8 methods)** (8th Iteration - Shared Macro):
  - [ ] **BLOCKING operations (5)** - Use `with-stream-timeout` macro:
    - [ ] `stream-receive-headers` (call.lisp:263): `(with-stream-timeout (server-stream) ...)`
    - [ ] `stream-receive-message` (call.lisp:287): `(with-stream-timeout (server-stream) ...)`
    - [ ] `stream-finish` (call.lisp:311): `(with-stream-timeout (server-stream) ...)`
    - [ ] `stream-close-and-recv` (call.lisp:467): `(unwind-protect (with-stream-timeout ...) (finalize...))`
    - [ ] `stream-read-message` (call.lisp:648): `(with-stream-timeout (bidi-stream) ...)`
  - [ ] **NON-BLOCKING operations (2)** - Context binding only:
    - [ ] `stream-send` generic :around method - with-context only
      - [ ] Primary method for client-stream (call.lisp:454)
      - [ ] Primary method for bidi-stream (call.lisp:617)
    - [ ] `stream-close-send` (call.lisp:632) - with-context only
- [ ] **Macro handles all complexity**: Coercion, three-branch pattern, error mapping centralized
- [ ] **Add `finalize-client-stream`** with idempotent double-cancel safety (6th Iteration)
- [ ] **Add 5 cleanup trigger points** (Issue #4):
  - [ ] Server streaming: in `stream-receive-message` when NIL
  - [ ] Client streaming: in `stream-close-and-recv` unwind-protect
  - [ ] Bidirectional: in `with-bidi-stream` macro
  - [ ] Error path: via unwind-protect in all call functions
  - [ ] Remote close: via `stream-finished-p` detection
- [ ] Add condition handlers for both bt2:timeout and cl-context conditions
- [ ] Update `channel-send-headers` to extract deadline from `*current-context*` and handle NIL

### Phase 3: Server-Side Integration
- [ ] Update `server-handle-headers` to create cl-context with deadline
- [ ] Store `cancel-fn` in grpc-call-context (don't cancel immediately)
- [ ] **Register cleanup callback on HTTP/2 stream** (Issue #2 fix - callback pattern)
- [ ] Bind `*current-context*` before dispatching handlers
- [ ] Add context value enrichment from headers
- [ ] **Modify `multiplexer-close-stream`** to invoke callbacks in `ag-http2/streams.lisp:153` (Issue #2)
- [ ] **Modify `connection-close`** to invoke callbacks in `ag-http2/connection.lisp:387` (Issue #2)
- [ ] **Remove conflicting server-handle-headers snippets** - use only canonical version (Issue #3)
- [ ] Document context lifetime contract for unary vs streaming
- [ ] **Document cancellation precedence**: deadline > RST_STREAM > other (Issue #5)

### Phase 4: Context Values
- [ ] Define standard context keys (+grpc-request-id+, etc.)
- [ ] Add `grpc-context-value` convenience function
- [ ] Add `enrich-context-from-metadata` helper
- [ ] Document context value usage patterns

### Phase 5: Interceptor Integration (Issue #8 - Not Deferred)
- [ ] Interceptors access context via `*current-context*` (implicit)
- [ ] Add documentation for interceptor context access
- [ ] Add example interceptors using context values
- [ ] Backward compatible: existing interceptors continue to work
- [ ] No signature changes needed (context is implicit)

### Phase 6: Documentation and Examples
- [ ] Update README with context usage
- [ ] Add examples showing timeout composition
- [ ] Add examples showing request-scoped values
- [ ] Document context lifetime for streaming handlers
- [ ] Document migration path from bt2:timeout
- [ ] Add examples of `context-ensure-not-cancelled` vs `context-check-cancelled`

## Testing Strategy

1. **Backward Compatibility Tests**
   - All existing tests must pass unchanged
   - Verify bt2:timeout still works
   - Verify RST_STREAM detection still works

2. **Context Integration Tests**
   - Deadline propagation from client to server
   - Cancellation via cl-context
   - Context value propagation
   - Nested context creation (parent deadlines inherited)

3. **Stress Tests**
   - Many concurrent requests with different timeouts
   - Cancel contexts while calls in progress
   - Verify cleanup (no leaked timers)

## Non-Goals (Explicitly Out of Scope)

1. **Making ag-http2 context-aware** - Too invasive, keep bt2:with-timeout
2. **Removing bt2:timeout** - Keep for preemptive interruption
3. **Changing interceptor signatures** - Context is implicit via `*current-context*`
4. **Async API changes** - Can be added later if needed
5. **Optional cl-context support** - Hard dependency is simpler and cleaner (Issue #9)

## How C-REVIEW.md Issues Were Addressed (11th Iteration - Final)

### Issues from 11th Review (Final Verification)

#### Issue #1: Reader naming consistency verification ✓
**Problem**: Need to confirm actual reader names to avoid mismatches (e.g., `grpc-error-code` vs `grpc-status-error-code`).

**Solution**:
- **Verified from source**: Read `ag-grpc/status.lisp` lines 73-92
- **Confirmed naming**: All readers use `grpc-status-error-` prefix
- **Documented**: Listed all 5 existing readers with verification marks
- **New reader**: `grpc-status-error-cause` follows same convention
- **Consistency guaranteed**: Pattern explicitly documented in plan

#### Issue #2: Details slot verification ✓
**Problem**: Plan mentioned `details` slot but need to verify it exists (adding would be breaking).

**Solution**:
- **Verified**: `details` slot DOES exist at line 79 of status.lisp
- **Reader**: `grpc-status-error-details` (confirmed)
- **Modification scope**: Add ONLY `:cause` slot, keep all existing slots unchanged
- **Documented**: Showed complete before/after with "DO NOT MODIFY" markers
- **No breaking changes**: Only additive modification

#### Issue #3: Cancellation mapping path documentation ✓
**Problem**: Plan says cancellation handled elsewhere but doesn't show specific mapping paths.

**Solution - Complete Documentation**:
- **Server-side**: `context-ensure-not-cancelled` maps RST_STREAM and `context-cancelled` → CANCELLED
- **Client-side**: Call-level handlers map `context-cancelled` → CANCELLED
- **Stream-level**: Cancel function invocation propagates through operations
- **Table added**: Shows complete exception → status code mapping
- **Paths verified**: All cancellation sources mapped to CANCELLED code

## How C-REVIEW.md Issues Were Addressed (10th Iteration)

### Issues from 10th Review (Final Clarifications)

#### Issue #1: grpc-status-error definition location ✓
**Problem**: Plan suggested adding `:cause` slot but didn't specify where condition is currently defined, risking duplicate definitions.

**Solution**:
- **Canonical location**: `ag-grpc/status.lisp` (lines 73-92)
- **Action**: MODIFY existing definition, don't create new one
- **Reader name**: `grpc-status-error-cause` (consistent with convention)
- **Verified**: Used Grep to locate existing definition before updating plan
- **Documentation**: Added exact line numbers and file structure

#### Issue #2: Backward compatibility of :cause slot ✓
**Problem**: Need to ensure existing code that creates `grpc-status-error` without `:cause` continues to work.

**Solution**:
- **Slot is optional**: `:initform nil` makes it backward compatible
- **Existing constructors work unchanged**: Old code that omits `:cause` still valid
- **Accessor always safe**: `(grpc-status-error-cause e)` returns NIL if not set
- **Tests unaffected**: Old tests continue to work, `:cause` is purely additive
- **Compatibility guarantee documented**: All existing code continues unchanged

#### Issue #3: Macro doesn't map context-cancelled ✓
**Problem**: `with-stream-timeout` maps deadline exceptions but not cancellations. Need to clarify this is intentional.

**Solution - Explicit Separation of Concerns**:
- **Macro handles DEADLINES only**: `bt2:timeout` and `context-deadline-exceeded`
- **Cancellation handled elsewhere**:
  - Server-side: `context-ensure-not-cancelled` in handlers
  - Client-side: Cancel function called explicitly
  - RST_STREAM: Detected via `context-check-cancelled` → CANCELLED status
- **Why separate**: Deadline is implicit (time), cancellation is explicit (user action)
- **Different error codes**: DEADLINE_EXCEEDED vs CANCELLED
- **Updated macro docstring**: Explicitly documents deadline-only scope

## How C-REVIEW.md Issues Were Addressed (9th Iteration)

### Issues from 9th Review (Final Refinements)

#### Issue #1: Macro placement and compile order ✓
**Problem**: Plan described macro but didn't specify where it should be defined. Compile-order issues could arise if macro defined after usage.

**Solution**:
- Define `with-stream-timeout` at **top of ag-grpc/call.lisp**
- Before all class definitions and function definitions
- Clear section comments marking macro definitions
- Ensures macro available at compile-time for all function expansions
- File structure documented with section markers

#### Issue #2: Macro doesn't map cl-context deadline exceptions ✓
**Problem**: Macro only caught `bt2:timeout`, not `cl-context:context-deadline-exceeded`. When `check-context` signals deadline exceeded, it wasn't mapped to `grpc-status-error`.

**Solution**:
- Updated macro to catch **both** exception types
- Three handler-case blocks:
  1. Positive remaining: catches `bt2:timeout` + `cl-context:context-deadline-exceeded`
  2. Zero/negative remaining: catches `cl-context:context-deadline-exceeded` from `check-context`
  3. No deadline: no handlers needed
- **Consistent error type**: Both timeout sources produce same `grpc-status-error`
- User code only catches one exception type regardless of timeout source

#### Issue #3: Stack trace preservation for debugging ✓
**Problem**: Converting timeout exceptions to `grpc-status-error` loses original exception context, making debugging harder.

**Solution**:
- Added `:cause` slot to `grpc-status-error` condition
- All error conversions preserve original condition: `:cause c`
- Updated error reporter to show cause chain
- **Debugging benefits**: Full stack trace via `(grpc-error-cause error)`
- **Backward compatible**: `:cause` defaults to NIL, optional
- Example debugging: Access original `bt2:timeout` or `context-deadline-exceeded` via cause slot

## How C-REVIEW.md Issues Were Addressed (8th Iteration)

### Issues from 8th Review (Final Consistency and Documentation)

#### Issue #1: Exception mapping must be consistent across all blocking operations ✓
**Problem**: Plan showed `bt2:timeout` → `grpc-status-error` mapping in snippets but didn't ensure consistency across all five blocking operations.

**Solution**:
- Created shared macro `with-stream-timeout` that encapsulates three-branch pattern and error mapping
- All 5 blocking operations use the same macro
- Single source of truth for timeout logic and error conversion
- No code drift - changes to timeout behavior happen in one place
- Documented in macro docstring with clear usage notes

#### Issue #2: Nested wrapper overhead unquantified ✓
**Problem**: Plan accepted nested wrappers but didn't provide rationale or overhead analysis.

**Solution - Overhead Analysis**:
- **Nested timeout cost**: μs per timeout setup (timer thread creation)
- **Actual nesting**: 2 extra timeouts per stream (header read + finish)
- **I/O time**: ms to seconds (network latency dominates)
- **Ratio**: 1:1000 to 1:1000000 (negligible)
- **Alternative cost**: Code complexity, maintenance burden, doubled function count
- **Conclusion**: Unmeasurable overhead, significant simplicity benefit

**Rationale documented**: Correct, simple, future-proof, maintainable, negligible performance impact

#### Issue #3: Nested wrappers may double-map errors ✓
**Problem**: If `stream-receive-message` calls `stream-receive-headers`, both wrap with error handling. Could result in duplicate error conversion.

**Solution - No Duplicate Conversion**:
- **Analysis**: `handler-case` only catches exceptions within its body
- **Inner wrapper** (stream-receive-headers): Catches `bt2:timeout`, converts to `grpc-status-error`
- **Outer wrapper** (stream-receive-message): Sees `grpc-status-error` (not `bt2:timeout`), propagates unchanged
- **Key insight**: Once converted, outer `bt2:timeout` handler never fires
- **Verification**: With shared macro, all wrappers have identical handling, innermost handler converts first
- **Documentation**: Added note in macro docstring explaining nested wrapper safety

## How C-REVIEW.md Issues Were Addressed (7th Iteration)

### Issues from 7th Review (Final Refinements)

#### Issue #1: Deadline already expired still allows blocking I/O ✓
**Problem**: When `remaining <= 0`, the pattern skipped `bt2:with-timeout` and directly called internal function, allowing indefinite blocking even with expired deadline.

**Solution**:
- Three-branch pattern: positive remaining (preemptive), zero/negative (immediate check), no deadline (cooperative)
- When deadline exists but remaining <= 0, call `cl-context:check-context` before internal function
- `check-context` signals `context-deadline-exceeded` immediately
- Ensures DEADLINE_EXCEEDED error even if deadline expired before operation starts
- No indefinite blocking when deadline already passed

#### Issue #2: stream-cl-context slot must exist on all stream types ✓
**Problem**: Plan introduced `stream-cl-context` accessor but didn't show slot definition on all stream classes. Missing slots would cause `:around` method to fail.

**Solution**:
- Documented complete slot definitions for all three stream classes
- `grpc-server-stream` gets `cl-context` and `cancel-fn` slots
- `grpc-client-stream` gets `cl-context` and `cancel-fn` slots
- `grpc-bidi-stream` gets `cl-context` and `cancel-fn` slots
- All use common accessor name `stream-cl-context`
- Phase 1 checklist updated with explicit slot additions

#### Issue #3: bt2:with-timeout accepts positive real; remaining can be rational ✓
**Problem**: `cl-context` uses rational numbers for time. Need to ensure `bt2:with-timeout` accepts rationals or coerce.

**Solution**:
- Coerce remaining time to `double-float` when passing to `bt2:with-timeout`
- Pattern: `(bt2:with-timeout ((coerce remaining 'double-float)) ...)`
- Defensive: ensures compatibility across all CL implementations
- Minimal overhead: coercion happens once per operation
- Updated all 5 blocking operation wrappers with coercion

#### Issue #4: Nested wrappers in call paths ✓
**Problem**: `stream-receive-message` internally calls `stream-receive-headers` and `stream-finish`, creating nested timeouts.

**Solution - Accept nested wrappers**:
- **Analysis**: Both timeouts use same deadline source, so inner <= outer
- **Safe**: If inner times out, error propagates immediately
- **Simple**: No need for "already wrapped" flag or internal variants
- **Decision**: Keep simple pattern where every public function wraps with timeout
- **Documented**: Added note in wrapper implementations that nested calls are safe
- **Rationale**: Minimal overhead, correct behavior, future-proof

## How C-REVIEW.md Issues Were Addressed (6th Iteration)

### Issues from 6th Review (Final Refinements)

#### Issue #1: Client-side streaming preemptive timeout enforcement ✓
**Problem**: Stream operations only had cooperative cancellation (with-context), but blocking I/O operations like `stream-receive-message` need preemptive timeout enforcement.

**Solution**:
- Each blocking stream operation calculates remaining deadline: `(- (cl-context:deadline ctx) (cl-context:get-current-time))`
- Wraps I/O with `bt2:with-timeout` using remaining time
- Provides both preemptive (bt2) and cooperative (cl-context) cancellation
- Non-blocking operations (stream-send, stream-close-send) skip bt2:with-timeout
- **5 blocking operations** get dual timeout: stream-receive-headers, stream-receive-message, stream-finish, stream-close-and-recv, stream-read-message

#### Issue #2: Wrapper list count clarification ✓
**Problem**: Plan said "7 functions" but listed 8 line numbers.

**Solution**:
- **Clarified**: 7 unique function names, 8 method implementations
- `stream-send` appears twice (lines 454 and 617) because it's a generic function with two methods
- Count is correct either way depending on whether you count function names or method implementations
- Documentation now explicitly states both counts for clarity

#### Issue #3: stream-send generic method wrapping strategy ✓
**Problem**: `stream-send` is a generic function with multiple methods. Need to specify wrapping strategy to avoid code duplication or method combination issues.

**Solution**:
- Use single `:around` method on the generic function
- `:around` method applies context binding to all specialized methods automatically
- Avoids duplicating wrapper code on each primary method
- Clean separation: `:around` handles cross-cutting concern, primary methods handle logic
- Future-proof: new stream types automatically get context binding

#### Issue #4: Double-cancel coordination ✓
**Problem**: Both `finalize-client-stream` and HTTP/2 cleanup callback store cancel-fn and might try to cancel, causing double-cancel or spurious errors.

**Solution - Explicit Idempotency Contract**:
- Both paths check `(when cancel-fn ...)` before calling
- Both paths clear `(setf ... nil)` after calling
- First-wins: whoever calls first does cleanup, second call is no-op
- Thread-safe enough: worst case both call cancel, which is idempotent in cl-context
- Documented in code comments and plan
- Added testing requirements to verify no double-cancel

## How C-REVIEW.md Issues Were Addressed (5th Iteration)

### New Issues from 5th Review (Implementation Details)

#### Issue #1: Stream wrapper implementations not concrete ✓
**Problem**: Design described but no exact function changes specified.

**Solution**:
- Listed all 7 stream functions to modify in `ag-grpc/call.lisp`
- Line numbers included: 263, 287, 311, 454, 467, 617, 632, 648
- Exact wrapper pattern shown for each function
- Internal `-internal` functions separate existing logic
- Context accessor specified: `stream-cl-context`

#### Issue #2: cleanup-callback slot migration plan ✓
**Problem**: Adding slot to `http2-stream` might break existing constructors.

**Solution**:
- `:initform nil` ensures backward compatibility
- No `:initarg` specified - no constructor conflicts
- Default NIL = "no callback" (safe fallback)
- Only ag-grpc sets via accessor after construction
- ✓ No changes needed to existing `make-instance` calls

#### Issue #3: RST_STREAM cleanup not wired ✓
**Problem**: RST_STREAM handler location unspecified.

**Solution**:
- **Exact location**: `ag-http2/connection.lisp:374-381`
- Existing code already calls `multiplexer-close-stream` (line 381)
- `multiplexer-close-stream` modified to invoke callback (from Issue #2)
- No additional RST_STREAM code needed
- Cleanup happens automatically via callback mechanism

#### Issue #4: Client-side cleanup trigger points unclear ✓
**Problem**: When is `finalize-client-stream` called?

**Solution - 5 trigger points defined**:
1. Server streaming: When `stream-receive-message` returns NIL
2. Client streaming: In `unwind-protect` of `stream-close-and-recv`
3. Bidirectional: In `with-bidi-stream` macro's `unwind-protect`
4. Error path: All use `unwind-protect` for exception safety
5. Remote close: Detected via `stream-finished-p` check

**Double-cancel safety**: Clear `cancel-fn` after first call (idempotent)

#### Issue #5: Deprecation warning noise ✓
**Problem**: Unconditional warnings cause test output noise.

**Solution**:
- Conditional warning via `*warn-deprecated-timeout-formatter*`
- Default: NIL (no warnings, quiet tests)
- Development: Set T to find usages
- Documented migration path over 2 releases
- External users can opt-in to see their own usages

## How C-REVIEW.md Issues Were Addressed (4th Iteration)

### New Issues from 4th Review

#### Issue #1: Client streaming context binding outside with-context scope ✓
**Problem**: Stream operations happen after call function returns, outside dynamic extent of `with-context`.

**Solution**:
- Store `cl-context` and `cancel-fn` on all stream objects
- Wrap every stream operation (`stream-receive-message`, `stream-send`, etc.) with `with-context`
- Add `finalize-client-stream` for cleanup
- Context remains accessible for entire stream lifetime

#### Issue #2: Circular dependency (ag-http2 → ag-grpc) ✓
**Problem**: Direct calls from `ag-http2` to `ag-grpc` functions create circular dependency.

**Solution**:
- Add `cleanup-callback` slot to `ag-http2:http2-stream` class
- `ag-grpc` registers callback when creating context
- `ag-http2` invokes callback when closing stream/connection
- No direct dependency: generic callback interface

#### Issue #3: Conflicting server-handle-headers snippets ✓
**Problem**: Multiple versions with different cleanup strategies (immediate vs streaming lifetime).

**Solution**:
- Removed conflicting snippet that used `unwind-protect` with immediate cancel
- Single canonical version in "Server Handler Context Creation" section
- All references point to canonical implementation
- Cleanup via callback registration, not `unwind-protect`

#### Issue #4: Formatter migration audit ✓
**Problem**: Need to find all uses of `format-grpc-timeout` and migrate to spec-compliant version.

**Solution**:
- **Audit complete**: Only ONE call site found at `ag-grpc/metadata.lisp:235`
- Replace with `format-grpc-timeout-spec-compliant`
- Deprecate old function with warning
- No other migration needed

#### Issue #5: RST_STREAM vs deadline precedence ✓
**Problem**: When both deadline exceeded and RST_STREAM present, which error should be reported?

**Solution**:
- **Precedence order**: deadline > RST_STREAM > other cancellation
- Rationale: Deadline is deterministic and time-based, RST_STREAM is client-initiated
- Check deadline first in `context-ensure-not-cancelled`
- Ensures deadline errors reported even if RST_STREAM also occurred
- Documented in function comments and plan

### New Issues from 3rd Review (Previously Addressed)

## How Previous C-REVIEW.md Issues Were Addressed (3rd Iteration)

### New Issues from 3rd Review

#### Issue #1: ensure-context missing from API verification ✓
- Added `(ensure-context &optional context)` to verified API list
- Confirmed from cl-context README line 355-357

#### Issue #2: Cleanup hooks not wired to actual code paths ✓
- Identified exact hook points with file names and line numbers:
  - `ag-http2/streams.lisp:153` - `multiplexer-close-stream`
  - `ag-http2/connection.lisp:387` - `connection-close`
  - `ag-grpc/server.lisp:309` - `server-connection-loop` cleanup
- Added new functions: `finalize-stream-context`, `cancel-connection-contexts`
- Added RST_STREAM handler with context cleanup

#### Issue #3: context-check-cancelled side effects ✓
- Changed to use `done-p` + `err` (pure, no side effects)
- Moved `check-context` to `context-ensure-not-cancelled` (where side effects acceptable)
- Clear separation: predicate vs signaling function

#### Issue #4: Spec-compliant formatter not wired everywhere ✓
- Updated `make-request-headers` to use `format-grpc-timeout-spec-compliant`
- Removed all calls to old `format-grpc-timeout` in header generation
- Old function kept for one release cycle with deprecation warning

#### Issue #5: NIL timeout handling ✓
- `make-request-headers` only adds grpc-timeout header when timeout is non-NIL and positive
- `deadline-to-grpc-timeout` returns NIL when deadline already passed
- All callers handle NIL correctly (omit header)

#### Issue #6: :around method on context-deadline ✓
- Documented that `:around` method is safe
- Added test requirements for both "no deadline" and "deadline set" cases
- No MOP dispatch issues (accessor is simple)

#### Issue #7: System definition updates ✓
- Documented exact changes to `ag-grpc.asd`
- Documented exact changes to `ag-grpc/package.lisp`
- Created new file `ag-grpc/context-values.lisp`
- Added to Phase 1 implementation checklist

#### Issue #8: grpc-context-value binding assumptions ✓
- Documented that `*current-context*` must be bound inside handlers/calls
- Outside those contexts, returns from background context
- Added clarification in function documentation

### Previous Issues (1st & 2nd Iteration)

## How Previous C-REVIEW.md Issues Were Addressed

### Issue #1: context-check-cancelled breaking change ✓
- Keep as pure predicate (backward compatible)
- Add new `context-ensure-not-cancelled` for signaling

### Issue #2: deadline sync slot-boundp bug ✓
- Add `deadline-synced-p` flag
- Use `:around` method to sync on first access

### Issue #3: cl-context API assumptions ✓
- Verified all API signatures from cl-context README
- Listed confirmed APIs in document

### Issue #4: grpc-timeout digit limits ✓
- Add `format-grpc-timeout-spec-compliant`
- Enforce 8-digit max, choose appropriate unit
- Return NIL from `deadline-to-grpc-timeout` when deadline passed

### Issue #5: Streaming handler context lifetime ✓
- Store `cancel-fn` in grpc-call-context
- Don't cancel immediately for streaming
- Add explicit cleanup contract
- Implement `finalize-server-stream` and `cancel-connection-contexts`

### Issue #6: Use check-context not just done-p ✓
- Updated `context-check-cancelled` to call `cl-context:check-context`
- Catches both `context-cancelled` and `context-deadline-exceeded`

### Issue #7: All client entrypoints need binding ✓
- Documented pattern for all call types
- Apply to unary, server-streaming, client-streaming, bidi

### Issue #8: Interceptor integration gap ✓
- Moved to Phase 5 (not deferred)
- Access via implicit `*current-context*`
- No signature changes needed

### Issue #9: Conditional compilation inconsistency ✓
- Made `cl-context` a hard dependency
- Removed all `#+cl-context` guards
- Simpler, cleaner implementation

## Success Criteria

### Backward Compatibility
- [ ] All existing tests pass unchanged
- [ ] `context-check-cancelled` remains a pure predicate (no side effects)
- [ ] Existing handler code works without modification

### API Correctness
- [ ] All cl-context API calls use verified signatures (including `ensure-context`)
- [ ] `context-check-cancelled` uses `done-p` (pure predicate)
- [ ] `context-ensure-not-cancelled` uses `check-context` (signaling)

### Client-Side
- [ ] Client-side timeouts work with cl-context (all 4 call types)
- [ ] **Shared `with-stream-timeout` macro** implements three-branch pattern consistently:
  - [ ] Positive remaining → bt2:with-timeout with double-float coercion
  - [ ] Zero/negative remaining → check-context for immediate DEADLINE_EXCEEDED
  - [ ] No deadline → cooperative checking only
  - [ ] **Maps BOTH bt2:timeout and cl-context:context-deadline-exceeded** uniformly
  - [ ] **Preserves original condition** in `:cause` slot for debugging
- [ ] **Macro defined at top of call.lisp** before all class/function definitions (compile order)
- [ ] **All 5 blocking operations** use `with-stream-timeout` macro: stream-receive-headers, stream-receive-message, stream-finish, stream-close-and-recv, stream-read-message
- [ ] **2 non-blocking operations** have cooperative only: stream-send, stream-close-send
- [ ] **Complete exception mapping**: Both timeout sources produce same error type
- [ ] **Consistent error mapping** across all operations (single source of truth)
- [ ] **No indefinite blocking** when deadline already expired
- [ ] **Rational to double-float coercion** handled in macro
- [ ] **Nested wrappers safe**: Inner converts first, outer propagates (no duplicate conversion)
- [ ] **Overhead documented**: μs setup vs. ms-to-s I/O (negligible)
- [ ] **Debugging support**: Original exceptions accessible via `grpc-status-error-cause`
- [ ] grpc-timeout headers are spec-compliant (8-digit max, correct unit)
- [ ] NIL timeout correctly omits grpc-timeout header
- [ ] Deadline from `*current-context*` propagates to headers
- [ ] `stream-send` :around method applies context to all stream types
- [ ] **All three stream classes** have `stream-cl-context` and `stream-cancel-fn` slots
- [ ] **grpc-status-error has `:cause` slot** (modified in ag-grpc/status.lisp, backward compatible)
- [ ] **Macro handles deadlines only**: bt2:timeout and context-deadline-exceeded (not context-cancelled)
- [ ] **Cancellation handled separately**: context-ensure-not-cancelled and RST_STREAM detection
- [ ] **No duplicate condition definitions**: Single canonical definition in status.lisp

### Server-Side
- [ ] Server-side deadlines honor client's grpc-timeout header
- [ ] Deadline sync works correctly (no slot-boundp bug)
- [ ] Streaming handler contexts remain valid until stream closes
- [ ] Unary handler contexts are cancelled immediately after completion
- [ ] `context-check-cancelled` checks both RST_STREAM and cl-context

### Cleanup and Resource Management
- [ ] Stream close triggers `finalize-stream-context`
- [ ] Connection close cancels all remaining contexts
- [ ] RST_STREAM triggers context cleanup
- [ ] Server shutdown cancels all contexts
- [ ] **Idempotent cleanup**: `finalize-client-stream` and HTTP/2 callback coordinate (check before call, clear after call)
- [ ] **No double-cancel errors** or spurious log messages
- [ ] No context leaks under any scenario
- [ ] Clean resource cleanup (contexts cancelled, timers stopped)

### System Integration
- [ ] `ag-grpc.asd` includes `:cl-context` dependency
- [ ] `ag-grpc/package.lisp` imports cl-context symbols
- [ ] `ag-grpc/context-values.lisp` compiles and loads
- [ ] All hook points correctly wired (4 locations identified)

### Feature Completeness
- [ ] Context values propagate through call chain
- [ ] Deadline inheritance works (child contexts can't extend parent deadline)
- [ ] Interceptors can access context via `*current-context*`
- [ ] `grpc-context-value` works in handlers and calls (background elsewhere)

### Documentation
- [ ] API documentation updated with examples
- [ ] Context lifetime contract documented
- [ ] Cleanup hook points documented
- [ ] Migration guide from bt2:timeout included

## Resolved Questions (from C-REVIEW.md)

1. ~~Should `context-check-cancelled` signal errors?~~
   - **Resolved**: No, keep as predicate. Add `context-ensure-not-cancelled` for signaling.

2. ~~Should `cl-context` be optional?~~
   - **Resolved**: No, make it a hard dependency. Simpler and cleaner.

3. ~~How should interceptors access context?~~
   - **Resolved**: Via `*current-context*` (implicit), keeps signatures backward compatible.

4. ~~How to handle streaming context lifetime?~~
   - **Resolved**: Store `cancel-fn` in context, call on stream finalization.

## Open Questions

1. Should async operations (future API) capture and propagate context automatically?
   - **Proposed**: Yes, capture `*current-context*` and restore in thread

2. Should we provide a way to cancel all streams on a channel?
   - **Proposed**: Future enhancement, not required for initial integration

3. Should `format-grpc-timeout` be deprecated in favor of `format-grpc-timeout-spec-compliant`?
   - **Proposed**: Yes, but keep old function for one release cycle with deprecation warning

## Summary of Changes from Previous Version

### 11th Iteration Changes (Final Implementation-Ready with Verification)

This version resolves the **final 3 verification issues** from the 11th C-REVIEW.md iteration:

1. **Reader Names Verified from Source** (Issue #1):
   - **Read actual source**: `ag-grpc/status.lisp` lines 73-92
   - **Confirmed naming**: All readers use `grpc-status-error-` prefix
   - **Listed all 5 existing readers**: code, message, details, headers, trailers
   - **New reader consistent**: `grpc-status-error-cause` follows pattern
   - **No naming mismatches**: Explicit verification prevents errors

2. **Details Slot Confirmed to Exist** (Issue #2):
   - **Verified at line 79**: `details` slot exists in current definition
   - **Modification scope clarified**: Add ONLY `:cause` slot
   - **Documented with markers**: "DO NOT MODIFY" for existing slots
   - **No breaking changes**: All existing slots kept unchanged
   - **Complete before/after**: Shows exact modification

3. **Complete Cancellation Mapping Documented** (Issue #3):
   - **Server-side path**: `context-ensure-not-cancelled` → CANCELLED
   - **Client-side path**: Call-level handlers → CANCELLED
   - **RST_STREAM handling**: Via `context-ensure-not-cancelled` → CANCELLED
   - **Mapping table added**: Shows all exception sources and their destinations
   - **Complete story**: Deadline (macro) + Cancellation (handlers) = full coverage

**Verification Summary**:

| Aspect | Verified From | Result |
|--------|--------------|--------|
| Reader names | ag-grpc/status.lisp:73-92 | All use `grpc-status-error-` prefix ✓ |
| Details slot | ag-grpc/status.lisp:79 | Exists, no breaking change ✓ |
| Cancellation paths | Plan sections + code | Complete mapping documented ✓ |

**Status**: Plan is now **fully implementation-ready** with all names verified from source, modification scope explicit, and complete error mapping documented.

### 10th Iteration Changes

This version resolves the **final 3 clarification issues** from the 10th C-REVIEW.md iteration:

1. **Canonical Definition Location** (Issue #1):
   - **Verified location**: `ag-grpc/status.lisp` (lines 73-92) - found via Grep
   - **Action**: MODIFY existing `grpc-status-error` definition
   - **Reader name**: `grpc-status-error-cause` (follows naming convention)
   - **No duplicates**: Single canonical definition, documented explicitly
   - **Phase 1 updated**: Shows exact file and lines to modify

2. **Backward Compatibility Guarantee** (Issue #2):
   - **Detailed compatibility analysis** added to plan
   - **Existing constructors work**: Old code omitting `:cause` continues unchanged
   - **Optional slot**: `:initform nil` ensures compatibility
   - **Accessor safe**: Always returns value (NIL if not set)
   - **Tests unaffected**: `:cause` is purely additive
   - **Documentation**: Explicit compatibility guarantee in plan

3. **Deadline vs Cancellation Scope** (Issue #3):
   - **Clarified macro scope**: Handles DEADLINE exceptions only
   - **Cancellation separate**: Handled by `context-ensure-not-cancelled` and RST_STREAM detection
   - **Rationale documented**: Deadline (implicit/time-based) vs Cancellation (explicit/user-action)
   - **Updated macro docstring**: Lists what IS and ISN'T mapped
   - **Error code separation**: DEADLINE_EXCEEDED vs CANCELLED

**Documentation Improvements**:
```lisp
;; Macro docstring now explicitly states:
"Maps DEADLINE exceptions only (timeout-related):
- bt2:timeout → grpc-status-error with DEADLINE_EXCEEDED
- cl-context:context-deadline-exceeded → grpc-status-error with DEADLINE_EXCEEDED

Does NOT map cancellation exceptions:
- cl-context:context-cancelled is handled by higher-level code
- Use context-ensure-not-cancelled for explicit cancellation checking
- RST_STREAM handled via context-check-cancelled → CANCELLED status"
```

**Status**: Plan is now **fully implementation-ready** with no ambiguity about where to modify code, backward compatibility guaranteed, and clear scope boundaries.

### 9th Iteration Changes

This version resolves the **final 3 refinement issues** from the 9th C-REVIEW.md iteration:

1. **Macro Placement Specified** (Issue #1):
   - **Location**: Top of `ag-grpc/call.lisp`, before class definitions
   - **Section markers**: Clear comments delineating macro definitions
   - **Compile order**: Macro defined before any usage for compile-time expansion
   - **File structure documented**: Shows exact organization of file sections

2. **Complete Exception Mapping** (Issue #2):
   - **Updated macro** to catch both `bt2:timeout` AND `cl-context:context-deadline-exceeded`
   - **Three handler locations**:
     - Positive remaining branch: catches both bt2 and cl-context timeouts
     - Zero/negative branch: catches cl-context deadline exceeded from `check-context`
     - No deadline branch: no handlers (no timeouts possible)
   - **Consistent API**: Both timeout sources produce same `grpc-status-error` type
   - **User code simplified**: Only need to catch one exception type

3. **Stack Trace Preservation** (Issue #3):
   - **Added `:cause` slot** to `grpc-status-error` condition
   - **All conversions preserve original**: `:cause c` in error creation
   - **Updated error reporter**: Shows cause chain in error output
   - **Debugging enhanced**: `(grpc-error-cause e)` retrieves original condition
   - **Backward compatible**: `:cause` slot optional, defaults to NIL
   - **No information loss**: Full context available for debugging

**Key Improvements**:
```lisp
;; Before: Only bt2:timeout mapped
(handler-case
    (bt2:with-timeout ...)
  (bt2:timeout ()
    (error 'grpc-status-error ...)))

;; After: Both exceptions mapped, original preserved
(handler-case
    (bt2:with-timeout ...)
  (bt2:timeout (c)
    (error 'grpc-status-error ... :cause c))
  (cl-context:context-deadline-exceeded (c)
    (error 'grpc-status-error ... :cause c)))
```

**Status**: Plan is now **fully implementation-ready** with complete error mapping, proper compile order, and debugging support.

### 8th Iteration Changes

This version resolves the **final 3 consistency issues** from the 8th C-REVIEW.md iteration:

1. **Shared Timeout Macro** (Issue #1):
   - **Created `with-stream-timeout` macro** encapsulating three-branch pattern and error mapping
   - All 5 blocking operations use the same macro consistently
   - **Single source of truth**: No code drift, changes happen in one place
   - **Consistent error mapping**: Every blocking op converts `bt2:timeout` → `grpc-status-error` identically
   - Macro includes detailed docstring explaining usage and nested wrapper safety

2. **Overhead Analysis Documented** (Issue #2):
   - **Quantified nested wrapper cost**: μs timeout setup vs. ms-to-s I/O time
   - **Ratio analysis**: 1:1000 to 1:1000000 (negligible)
   - **Actual nesting**: 2 extra timeouts per stream (amortized over N messages)
   - **Alternative cost**: Code complexity, maintenance burden, doubled functions
   - **Conclusion**: Unmeasurable performance impact, significant simplicity benefit
   - Added comprehensive rationale: correct, simple, future-proof, maintainable

3. **No Duplicate Error Conversion** (Issue #3):
   - **Verified behavior**: Inner wrapper converts `bt2:timeout` first
   - **Outer wrapper**: Sees already-converted `grpc-status-error`, propagates unchanged
   - **Key insight**: `handler-case` only catches exceptions within its body
   - **With shared macro**: All wrappers have identical handling, guaranteed consistency
   - Added note in macro docstring explaining nested wrapper safety

**Simplification**: All blocking operations now reduced to simple one-line macro invocations:
```lisp
(defun stream-receive-message (server-stream)
  (with-stream-timeout (server-stream)
    (stream-receive-message-internal server-stream)))
```

**Status**: Plan is now **fully implementation-ready** with consistent error handling, documented overhead rationale, and verified nested wrapper safety.

### 7th Iteration Changes

This version resolves the **final 4 refinement issues** from the 7th C-REVIEW.md iteration:

1. **Expired Deadline Immediate Check** (Issue #1):
   - **Three-branch pattern** for all blocking operations:
     - Positive remaining → `bt2:with-timeout` with coerced float
     - Zero/negative remaining → `cl-context:check-context` for immediate DEADLINE_EXCEEDED
     - No deadline → cooperative checking only
   - Prevents indefinite blocking when deadline already passed
   - Updated all 5 blocking operations: stream-receive-headers, stream-receive-message, stream-finish, stream-close-and-recv, stream-read-message

2. **Stream Slot Definitions** (Issue #2):
   - **Documented complete slot definitions** for all three stream classes:
     - `grpc-server-stream` + `cl-context`/`cancel-fn` slots
     - `grpc-client-stream` + `cl-context`/`cancel-fn` slots
     - `grpc-bidi-stream` + `cl-context`/`cancel-fn` slots
   - All use common accessor `stream-cl-context`
   - Ensures `:around` method safely accesses slots on any stream type

3. **Rational to Float Coercion** (Issue #3):
   - **Coerce remaining to double-float**: `(coerce remaining 'double-float)`
   - Applied to all 5 blocking operations
   - Ensures bt2:with-timeout compatibility across CL implementations
   - Defensive coding: minimal overhead, guaranteed correctness

4. **Nested Wrappers Strategy** (Issue #4):
   - **Decision: Accept nested wrappers as-is**
   - Analysis: Both timeouts use same deadline, inner <= outer, safe behavior
   - Simple: No "already wrapped" flags or internal variants needed
   - Documented: Added notes in wrapper implementations
   - Rationale: Correct, minimal overhead, future-proof

**Status**: Plan is now **fully implementation-ready** with all refinement issues resolved. The three-branch pattern ensures immediate deadline errors, rational-to-float coercion guarantees bt2 compatibility, all stream slots are documented, and nested wrappers are analyzed and accepted.

### 6th Iteration Changes

This version resolves the **final 4 issues** from the 6th C-REVIEW.md iteration:

1. **Preemptive Timeout for Streaming** (Issue #1):
   - All **5 blocking stream operations** now calculate remaining deadline
   - Wrap I/O with `bt2:with-timeout` using remaining time
   - Provides layered timeout: preemptive (bt2) + cooperative (cl-context)
   - Non-blocking operations (stream-send, stream-close-send) only use cooperative
   - Fixes: stream-receive-headers, stream-receive-message, stream-finish, stream-close-and-recv, stream-read-message

2. **Count Clarification** (Issue #2):
   - Explicitly states: **7 unique function names, 8 method implementations**
   - `stream-send` generic has 2 methods (client-stream, bidi-stream)
   - Documentation now clear on both interpretations

3. **Generic Method Strategy** (Issue #3):
   - `stream-send` uses **`:around` method** for context binding
   - Applies to all specialized methods automatically
   - No code duplication, clean separation of concerns
   - Future-proof for new stream types

4. **Double-Cancel Safety** (Issue #4):
   - **Explicit idempotency contract**: check before call, clear after call
   - Both `finalize-client-stream` and HTTP/2 cleanup callback follow same pattern
   - First-wins: first path does cleanup, second is no-op
   - Testing requirements added to verify no spurious errors

**Status**: Plan is now **implementation-ready** with all issues resolved. Each blocking stream operation has both preemptive and cooperative timeout enforcement, count ambiguity is clarified, generic method wrapping is specified, and double-cancel coordination is explicit.

### 5th Iteration Changes (Implementation-Ready Details)

This version provides **implementation-ready details** for all 5 refinement issues:

1. **Concrete Stream Wrappers** (Issue #1):
   - 7 specific functions listed with line numbers
   - Exact wrapper pattern: outer function → `with-context` → internal function
   - Context accessor: `stream-cl-context`
   - All in `ag-grpc/call.lisp`: lines 263, 287, 311, 454, 467, 617, 632, 648

2. **Slot Migration Plan** (Issue #2):
   - `cleanup-callback` slot: `:initform nil`, no `:initarg`
   - ✓ Backward compatible - no constructor changes needed
   - ag-grpc sets via accessor, not at construction
   - Verification: won't conflict with existing `make-instance` calls

3. **RST_STREAM Location** (Issue #3):
   - Exact handler: `ag-http2/connection.lisp:374-381`
   - Already calls `multiplexer-close-stream` (line 381)
   - No additional code needed - callback mechanism handles it
   - ✓ Already wired correctly

4. **Cleanup Triggers Defined** (Issue #4):
   - 5 specific trigger points documented
   - Idempotent: clears `cancel-fn` after first call
   - Works in normal close, remote close, error, and timeout paths
   - `unwind-protect` ensures cleanup in all scenarios

5. **Conditional Deprecation** (Issue #5):
   - `*warn-deprecated-timeout-formatter*` parameter (default NIL)
   - Opt-in warnings for development/external users
   - No test noise by default
   - Documented migration timeline

### 4th Iteration Changes

This version addresses all 5 critical issues from the 4th C-REVIEW.md:

1. **Client Streaming Context Binding** (Issue #1):
   - Store context on all stream objects (server-stream, client-stream, bidi-stream)
   - Wrap all stream operations with `with-context`
   - Add cleanup function `finalize-client-stream`
   - Context accessible throughout stream lifetime, not just call scope

2. **Circular Dependency Eliminated** (Issue #2):
   - Generic callback interface: `stream-cleanup-callback` slot in `ag-http2:http2-stream`
   - ag-grpc registers callback, ag-http2 invokes it
   - No direct ag-http2 → ag-grpc dependency
   - Clean separation of concerns

3. **Conflicting Snippets Removed** (Issue #3):
   - Single canonical `server-handle-headers` implementation
   - Removed snippet with incorrect immediate cleanup
   - All references point to correct version
   - Consistent streaming lifetime handling

4. **Formatter Audit Complete** (Issue #4):
   - Only ONE call site: `ag-grpc/metadata.lisp:235`
   - Simple one-line replacement
   - Deprecate old function with warning
   - No hidden usages in codebase

5. **Cancellation Precedence Defined** (Issue #5):
   - Explicit order: deadline > RST_STREAM > other
   - Ensures deterministic error reporting
   - Documented in code and tests
   - Handles overlapping cancellation correctly

### 3rd Iteration Changes

This version addresses all 8 issues from the 3rd C-REVIEW.md:

1. **API Verification Complete**: Added missing `ensure-context` to verified API list
2. **Cleanup Hooks Wired**: Identified exact hook points with file:line references:
   - `ag-http2/streams.lisp:153` - stream close
   - `ag-http2/connection.lisp:387` - connection close
   - `ag-grpc/server.lisp:309` - connection loop cleanup
   - New RST_STREAM handler
3. **Pure Predicate**: `context-check-cancelled` now uses `done-p` (no side effects)
4. **Spec-Compliant Everywhere**: `make-request-headers` updated to use new formatter
5. **NIL Handling**: Header generation correctly omits grpc-timeout when NIL
6. **Method Safety**: `:around` method documented and test requirements added
7. **System Updates**: Complete system definition and package updates documented
8. **Binding Clarification**: `grpc-context-value` behavior outside handlers documented

### 2nd Iteration Summary

This version addresses all 9 issues raised in the 2nd iteration C-REVIEW.md:

1. **Backward Compatibility**: `context-check-cancelled` remains a pure predicate, new `context-ensure-not-cancelled` for signaling
2. **Bug Fix**: Fixed deadline sync using `deadline-synced-p` flag instead of broken `slot-boundp` check
3. **API Verification**: All cl-context API signatures confirmed from official README
4. **Spec Compliance**: New `format-grpc-timeout-spec-compliant` enforces 8-digit limit and proper unit selection
5. **Context Lifetime**: Explicit lifetime management for streaming handlers, contexts cancelled only when streams close
6. **Better Cancellation**: Use `check-context` instead of just `done-p` for richer error information
7. **Complete Coverage**: All 4 client call types get context binding (not just unary)
8. **Interceptor Integration**: Moved to Phase 5, uses implicit `*current-context*`, no signature changes
9. **Hard Dependency**: cl-context is required, not optional - simpler implementation

**Key Architectural Decisions**:
- Layered timeout: bt2:with-timeout (preemptive) + cl-context (cooperative)
- Context lifetime tied to stream lifecycle, not stack frames
- Backward compatible: existing code works, new features opt-in
- Unified time base: `cl-context:get-current-time` everywhere

## References

- cl-context README: `/home/green/git/ag-gRPC/ocicl/cl-context-20260205-b61eaa0/README.md`
- cl-context API verification: Lines 168-358 (confirmed all signatures)
- pure-tls integration example: `/home/green/git/ag-gRPC/ocicl/pure-tls-20260205-814ba9b/README.md` (lines 138-207)
- Current timeout implementation: `ag-grpc/call.lisp` (lines 86-100)
- Server context: `ag-grpc/server.lisp` (lines 132-190)
- Server handler dispatch: `ag-grpc/server.lisp` (lines 417-453)
- Timeout formatting: `ag-grpc/metadata.lisp` (lines 288-314)
- gRPC timeout spec: https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md#requests
