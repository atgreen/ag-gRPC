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
             :documentation "Received trailers"))
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

(defun stream-transition (stream event)
  "Transition a stream to a new state based on an event.
Returns the new state or signals an error for invalid transitions."
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
             :closed)))))

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
    (setf (fill-pointer (stream-data-buffer stream)) 0)
    data))

;;;; ========================================================================
;;;; Stream Multiplexer
;;;; ========================================================================

(defclass stream-multiplexer ()
  ((streams :initform (make-hash-table) :accessor multiplexer-streams
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

(defun multiplexer-get-stream (mux stream-id)
  "Get or create a stream by ID"
  (or (gethash stream-id (multiplexer-streams mux))
      (setf (gethash stream-id (multiplexer-streams mux))
            (make-http2-stream stream-id))))

(defun multiplexer-new-stream (mux)
  "Create a new stream with the next available ID"
  (let ((id (multiplexer-next-stream-id mux)))
    (incf (multiplexer-next-stream-id mux) 2)  ; Skip by 2 (odd/even)
    (multiplexer-get-stream mux id)))

(defun multiplexer-close-stream (mux stream-id)
  "Mark a stream as closed"
  (let ((stream (gethash stream-id (multiplexer-streams mux))))
    (when stream
      (setf (stream-state stream) :closed))))

(defun multiplexer-active-streams (mux)
  "Return count of non-closed streams"
  (loop for stream being the hash-values of (multiplexer-streams mux)
        count (not (eq (stream-state stream) :closed))))
