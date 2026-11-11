;;;; streams.lisp - HTTP/2 stream management

(in-package #:ag-http2)

;;;; ========================================================================
;;;; Stream States (RFC 7540 Section 5.1)
;;;; ========================================================================

(deftype stream-state ()
  '(member :idle :reserved-local :reserved-remote :open
           :half-closed-local :half-closed-remote :closed))

;;;; ========================================================================
;;;; HTTP/2 Stream Class
;;;; ========================================================================

(defclass http2-stream ()
  ((id :initarg :id :accessor stream-id
       :documentation "Stream identifier")
   (state :initarg :state :accessor stream-state :initform :idle
          :documentation "Current stream state")
   (local-window :initarg :local-window :accessor stream-local-window
                 :initform 65535
                 :documentation "Local flow control window")
   (remote-window :initarg :remote-window :accessor stream-remote-window
                  :initform 65535
                  :documentation "Remote flow control window")
   (headers :initform nil :accessor stream-headers
            :documentation "Received headers")
   (data-buffer :initform (make-array 0 :element-type '(unsigned-byte 8)
                                        :adjustable t :fill-pointer 0)
                :accessor stream-data-buffer
                :documentation "Buffered data frames")
   (trailers :initform nil :accessor stream-trailers
             :documentation "Received trailers")
   (rst-stream-error :initform nil :accessor stream-rst-stream-error
                     :documentation "RST_STREAM error code if received")
   (cleanup-callback :initform nil :accessor stream-cleanup-callback
                     :documentation "Optional callback invoked when stream closes")
   (cleanup-lock :initform (bt2:make-lock :name "http2-stream-cleanup")
                 :reader stream-cleanup-lock
                 :documentation "Guards claiming the cleanup callback, so a stream
closed from two directions at once (peer RST while the handler sends its last
frame) still runs cleanup exactly once"))
  (:documentation "Represents an HTTP/2 stream"))

(defun make-http2-stream (id &key (initial-window-size 65535))
  "Create a new HTTP/2 stream"
  (make-instance 'http2-stream
                 :id id
                 :local-window initial-window-size
                 :remote-window initial-window-size))

;;;; ========================================================================
;;;; Stream State Transitions
;;;; ========================================================================

(defun stream-run-cleanup (stream)
  "Invoke STREAM's cleanup callback, at most once over the stream's life.
The callback is claimed under the lock and run outside it, so a callback that
blocks cannot hold up the peer's half of the close."
  (let ((callback (bt2:with-lock-held ((stream-cleanup-lock stream))
                    (shiftf (stream-cleanup-callback stream) nil))))
    (when callback
      (funcall callback stream))))

(defun stream-transition (stream event)
  "Transition a stream to a new state based on an event.
Returns the new state or signals an error for invalid transitions.
Reaching :closed runs the stream's cleanup callback: a stream that ends the
ordinary way - the handler sending END_STREAM - has to release what an
RST_STREAM would, or the resources it holds are held for the life of the
connection."
  (let ((current (stream-state stream)))
    (setf (stream-state stream)
          (ecase current
            (:idle
             (ecase event
               (:send-headers :open)
               (:recv-headers :open)
               (:send-push-promise :reserved-local)
               (:recv-push-promise :reserved-remote)))
            (:reserved-local
             (ecase event
               (:send-headers :half-closed-remote)
               (:send-rst-stream :closed)
               (:recv-rst-stream :closed)))
            (:reserved-remote
             (ecase event
               (:recv-headers :half-closed-local)
               (:send-rst-stream :closed)
               (:recv-rst-stream :closed)))
            (:open
             (ecase event
               (:recv-headers :open)  ; Response headers, no state change
               (:recv-data :open)     ; Response body, no state change
               (:send-end-stream :half-closed-local)
               (:recv-end-stream :half-closed-remote)
               (:send-rst-stream :closed)
               (:recv-rst-stream :closed)))
            (:half-closed-local
             ;; Can receive any frame type (RFC 7540 Section 5.1)
             ;; Only END_STREAM or RST_STREAM cause state change
             (ecase event
               (:recv-headers :half-closed-local)  ; Response/trailers, no state change
               (:recv-data :half-closed-local)     ; Response body, no state change
               (:recv-end-stream :closed)
               (:send-rst-stream :closed)
               (:recv-rst-stream :closed)))
            (:half-closed-remote
             (ecase event
               (:send-end-stream :closed)
               (:send-rst-stream :closed)
               (:recv-rst-stream :closed)))
            (:closed
             ;; Already closed, ignore
             :closed)))
    (when (and (eql (stream-state stream) :closed)
               (not (eql current :closed)))
      (stream-run-cleanup stream))
    (stream-state stream)))

(defun stream-can-send-p (stream)
  "Return T if the stream can send data"
  (member (stream-state stream) '(:open :half-closed-remote)))

(defun stream-can-recv-p (stream)
  "Return T if the stream can receive data"
  (member (stream-state stream) '(:open :half-closed-local)))

;;;; ========================================================================
;;;; Stream Data Handling
;;;; ========================================================================

(defun stream-append-data (stream data)
  "Append data to the stream's buffer"
  (loop for byte across data
        do (vector-push-extend byte (stream-data-buffer stream))))

(defun stream-consume-data (stream)
  "Consume and return all buffered data"
  (let ((data (copy-seq (stream-data-buffer stream))))
    ;; Closed stream records remain in the multiplexer. A zero fill pointer
    ;; still retains the entire request allocation on a long-lived connection.
    (setf (stream-data-buffer stream)
          (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
    data))

;;;; ========================================================================
;;;; Stream Multiplexer
;;;; ========================================================================

(defclass stream-multiplexer ()
  ((lock :initform (bt2:make-lock :name "http2-multiplexer") :reader multiplexer-lock)
   (odd-high-water :initform -1 :accessor multiplexer-odd-high-water)
   (even-high-water :initform 0 :accessor multiplexer-even-high-water)
   (streams :initform (make-hash-table) :accessor multiplexer-streams
            :documentation "Map of stream-id to http2-stream")
   (next-stream-id :initarg :next-stream-id :accessor multiplexer-next-stream-id
                   :documentation "Next available stream ID")
   (max-concurrent :initform 100 :accessor multiplexer-max-concurrent
                   :documentation "Maximum concurrent streams"))
  (:documentation "Manages multiple HTTP/2 streams"))

(defun make-stream-multiplexer (&key (client-p t))
  "Create a stream multiplexer.
Client streams use odd IDs, server streams use even IDs."
  (make-instance 'stream-multiplexer
                 :next-stream-id (if client-p 1 2)))

(defun multiplexer-note-id (mux id)
  (if (oddp id)
      (setf (multiplexer-odd-high-water mux) (max id (multiplexer-odd-high-water mux)))
      (setf (multiplexer-even-high-water mux) (max id (multiplexer-even-high-water mux)))))

(defun multiplexer-get-stream (mux stream-id)
  "Find a stream. Forgotten or implicitly closed IDs never become idle again."
  (bt2:with-lock-held ((multiplexer-lock mux))
    (or (gethash stream-id (multiplexer-streams mux))
        (let ((stream (make-http2-stream stream-id)))
          (if (<= stream-id (if (oddp stream-id) (multiplexer-odd-high-water mux)
                               (multiplexer-even-high-water mux)))
              ;; A transient closed sentinel preserves protocol classification
              ;; without retaining one object per historical stream ID.
              (setf (stream-state stream) :closed)
              (progn (multiplexer-note-id mux stream-id)
                     (setf (gethash stream-id (multiplexer-streams mux)) stream)))
          stream))))

(defun multiplexer-new-stream (mux)
  "Allocate and publish a new local stream atomically."
  (bt2:with-lock-held ((multiplexer-lock mux))
    (let* ((id (multiplexer-next-stream-id mux)) (stream (make-http2-stream id)))
      (incf (multiplexer-next-stream-id mux) 2)
      (multiplexer-note-id mux id)
      (setf (gethash id (multiplexer-streams mux)) stream)
      stream)))

(defun multiplexer-close-stream (mux stream-id)
  "Mark closed, then run cleanup outside the multiplexer lock."
  (let ((stream (bt2:with-lock-held ((multiplexer-lock mux))
                  (let ((stream (gethash stream-id (multiplexer-streams mux))))
                    (when stream (setf (stream-state stream) :closed)) stream))))
    (when stream (stream-run-cleanup stream))))

(defun multiplexer-forget-stream (mux stream-id)
  "Release a closed stream after its application has finished with its data."
  (bt2:with-lock-held ((multiplexer-lock mux))
    (let ((stream (gethash stream-id (multiplexer-streams mux))))
      (when stream
        (unless (eq :closed (stream-state stream)) (error "Cannot retire an active HTTP2 stream"))
        (remhash stream-id (multiplexer-streams mux))))))

(defun multiplexer-active-streams (mux)
  "Return count of non-closed streams"
  (bt2:with-lock-held ((multiplexer-lock mux))
    (loop for stream being the hash-values of (multiplexer-streams mux)
          count (not (eql (stream-state stream) :closed)))))
