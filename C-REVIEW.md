Updated critique based on the 11th‑iteration plan in `CONTEXT.md`.

**What’s now solid**

1. Reader names and slot list for `grpc-status-error` are verified against `ag-grpc/status.lisp` and consistent (`grpc-status-error-*`).
2. The `details` slot is confirmed to exist; the plan now adds only `:cause`, avoiding breaking changes.
3. Cancellation mapping paths are explicitly documented for both client and server sides, completing the error‑mapping story.
4. Macro scope and compile‑order remain clear and unambiguous.

**Remaining issues or risks**

1. **Updated report function must use correct reader names.**
   The plan’s report snippet uses `grpc-status-error-name`, but the existing code appears to call `grpc-status-error-name` or `grpc-status-name` depending on the file. Confirm the actual helper used in `ag-grpc/status.lisp` and keep it unchanged when adding the `:cause` display. Otherwise, a small rename mistake will break error printing.

2. **Ensure `:cause` is added in the canonical definition only.**
   The plan now says to modify `ag-grpc/status.lisp` directly; make sure no other files define or re‑export `grpc-status-error` in a way that shadows the updated definition.

**Suggested edits to `CONTEXT.md` before implementation**

1. Add a short “do not change existing report formatting except to append cause” note, and name the exact helper (`grpc-status-name` or `grpc-status-error-name`) used in the current report lambda.
2. Add a quick reminder that only the `ag-grpc/status.lisp` definition should be modified, and no other condition definitions should be introduced.
