;;;; frames.lisp - HTTP/2 frame types (RFC 7540 Section 6)

(in-package #:ag-http2)

;;;; ========================================================================
;;;; Frame Base Class
;;;; ========================================================================

(defclass frame ()
  ((type :initarg :type :accessor frame-type
         :documentation "Frame type (0-9)")
   (flags :initarg :flags :accessor frame-flags :initform 0
          :documentation "Frame flags byte")
   (stream-id :initarg :stream-id :accessor frame-stream-id :initform 0
              :documentation "Stream identifier")
   (payload :initarg :payload :accessor frame-payload :initform #()
            :documentation "Frame payload bytes"))
  (:documentation "Base class for HTTP/2 frames"))

;;;; ========================================================================
;;;; Specific Frame Types
;;;; ========================================================================

(defclass data-frame (frame)
  ((data :initarg :data :accessor data-frame-data
         :documentation "Application data"))
  (:default-initargs :type +frame-type-data+)
  (:documentation "HTTP/2 DATA frame"))

(defclass headers-frame (frame)
  ((headers :initarg :headers :accessor headers-frame-headers
            :documentation "Decoded header list")
   (priority :initarg :priority :accessor headers-frame-priority
             :initform nil
             :documentation "Priority information if present"))
  (:default-initargs :type +frame-type-headers+)
  (:documentation "HTTP/2 HEADERS frame"))

(defclass priority-frame (frame)
  ((exclusive :initarg :exclusive :accessor priority-frame-exclusive)
   (stream-dependency :initarg :stream-dependency :accessor priority-frame-stream-dependency)
   (weight :initarg :weight :accessor priority-frame-weight))
  (:default-initargs :type +frame-type-priority+)
  (:documentation "HTTP/2 PRIORITY frame"))

(defclass rst-stream-frame (frame)
  ((error-code :initarg :error-code :accessor rst-stream-frame-error-code))
  (:default-initargs :type +frame-type-rst-stream+)
  (:documentation "HTTP/2 RST_STREAM frame"))

(defclass settings-frame (frame)
  ((settings :initarg :settings :accessor settings-frame-settings
             :initform nil
             :documentation "Alist of setting id to value"))
  (:default-initargs :type +frame-type-settings+)
  (:documentation "HTTP/2 SETTINGS frame"))

(defun settings-frame-ack-p (frame)
  "Return T if SETTINGS frame has ACK flag set."
  (plusp (logand (frame-flags frame) +flag-ack+)))

(defclass push-promise-frame (frame)
  ((promised-stream-id :initarg :promised-stream-id :accessor push-promise-frame-promised-stream-id)
   (headers :initarg :headers :accessor push-promise-frame-headers))
  (:default-initargs :type +frame-type-push-promise+)
  (:documentation "HTTP/2 PUSH_PROMISE frame"))

(defclass ping-frame (frame)
  ((opaque-data :initarg :opaque-data :accessor ping-frame-opaque-data))
  (:default-initargs :type +frame-type-ping+)
  (:documentation "HTTP/2 PING frame"))

(defclass goaway-frame (frame)
  ((last-stream-id :initarg :last-stream-id :accessor goaway-frame-last-stream-id)
   (error-code :initarg :error-code :accessor goaway-frame-error-code)
   (debug-data :initarg :debug-data :accessor goaway-frame-debug-data :initform #()))
  (:default-initargs :type +frame-type-goaway+)
  (:documentation "HTTP/2 GOAWAY frame"))

(defclass window-update-frame (frame)
  ((window-size-increment :initarg :window-size-increment
                          :accessor window-update-frame-window-size-increment))
  (:default-initargs :type +frame-type-window-update+)
  (:documentation "HTTP/2 WINDOW_UPDATE frame"))

(defclass continuation-frame (frame)
  ((header-block :initarg :header-block :accessor continuation-frame-header-block))
  (:default-initargs :type +frame-type-continuation+)
  (:documentation "HTTP/2 CONTINUATION frame"))

;;;; ========================================================================
;;;; Frame Serialization
;;;; ========================================================================

(defun write-frame (frame stream)
  "Write an HTTP/2 frame to a binary stream"
  (let* ((payload (frame-payload frame))
         (length (length payload)))
    ;; Frame header: 9 bytes
    ;; Length (24 bits)
    (write-byte (logand (ash length -16) #xff) stream)
    (write-byte (logand (ash length -8) #xff) stream)
    (write-byte (logand length #xff) stream)
    ;; Type (8 bits)
    (write-byte (frame-type frame) stream)
    ;; Flags (8 bits)
    (write-byte (frame-flags frame) stream)
    ;; Stream ID (32 bits, high bit reserved)
    (let ((id (frame-stream-id frame)))
      (write-byte (logand (ash id -24) #x7f) stream)
      (write-byte (logand (ash id -16) #xff) stream)
      (write-byte (logand (ash id -8) #xff) stream)
      (write-byte (logand id #xff) stream))
    ;; Payload
    (write-sequence payload stream)))

(defun read-full-sequence (buffer stream)
  "Read exactly (length buffer) bytes from stream.
Handles partial reads by retrying until complete or EOF."
  (let ((total 0)
        (len (length buffer)))
    (loop while (< total len)
          for read = (read-sequence buffer stream :start total)
          do (if (= read total)
                 ;; No progress - EOF or error
                 (return total)
                 (setf total read)))
    total))

(defun read-frame (stream &optional (max-frame-size +default-max-frame-size+))
  "Read an HTTP/2 frame from a binary stream.
Signals END-OF-FILE on a clean peer close (zero bytes available),
HTTP2-FRAME-ERROR on a truncated header. Returning NIL here would
strand callers in their read loops, busy-spinning on a dead socket.

MAX-FRAME-SIZE is the receiver's advertised SETTINGS_MAX_FRAME_SIZE. RFC 7540
4.2 requires rejecting a frame whose declared length exceeds it; the check
happens BEFORE the payload is allocated so a 9-byte header cannot force a
multi-megabyte (up to 2^24-1) allocation."
  (let* ((header (make-array 9 :element-type '(unsigned-byte 8)))
         (header-bytes (read-full-sequence header stream)))
    (cond
      ((zerop header-bytes)
       (error 'end-of-file :stream stream))
      ((/= 9 header-bytes)
       (error 'http2-frame-error :message "Truncated frame header")))
    (let* ((length (logior (ash (aref header 0) 16)
                           (ash (aref header 1) 8)
                           (aref header 2)))
           (type (aref header 3))
           (flags (aref header 4))
           (stream-id (logior (ash (logand (aref header 5) #x7f) 24)
                              (ash (aref header 6) 16)
                              (ash (aref header 7) 8)
                              (aref header 8))))
      ;; RFC 7540 4.2: reject (FRAME_SIZE_ERROR) before allocating the payload.
      (when (> length max-frame-size)
        (error 'http2-frame-error
               :message (format nil "Frame length ~D exceeds SETTINGS_MAX_FRAME_SIZE ~D"
                                length max-frame-size)
               :error-code +error-frame-size-error+))
      (let ((payload (make-array length :element-type '(unsigned-byte 8))))
        (when (and (plusp length)
                   (/= length (read-full-sequence payload stream)))
          (error 'http2-frame-error :message "Incomplete frame payload"))
        (make-frame-from-raw type flags stream-id payload)))))

(defun make-frame-from-raw (type flags stream-id payload)
  "Create an appropriate frame object from raw frame data"
  (case type
    (#.+frame-type-data+
     (make-instance 'data-frame
                    :flags flags :stream-id stream-id :payload payload
                    :data payload))
    (#.+frame-type-headers+
     (make-instance 'headers-frame
                    :flags flags :stream-id stream-id :payload payload))
    (#.+frame-type-settings+
     (make-instance 'settings-frame
                    :flags flags :stream-id stream-id :payload payload
                    :settings (parse-settings-payload payload)))
    (#.+frame-type-ping+
     (make-instance 'ping-frame
                    :flags flags :stream-id stream-id :payload payload
                    :opaque-data payload))
    (#.+frame-type-goaway+
     (make-instance 'goaway-frame
                    :flags flags :stream-id stream-id :payload payload))
    (#.+frame-type-window-update+
     (make-instance 'window-update-frame
                    :flags flags :stream-id stream-id :payload payload
                    :window-size-increment (parse-window-update-payload payload)))
    (#.+frame-type-rst-stream+
     (make-instance 'rst-stream-frame
                    :flags flags :stream-id stream-id :payload payload
                    :error-code (parse-error-code-payload payload)))
    (otherwise
     (make-instance 'frame
                    :type type :flags flags :stream-id stream-id :payload payload))))

(defun parse-settings-payload (payload)
  "Parse a SETTINGS frame payload into an alist"
  (loop for i from 0 below (length payload) by 6
        collect (cons (logior (ash (aref payload i) 8)
                              (aref payload (1+ i)))
                      (logior (ash (aref payload (+ i 2)) 24)
                              (ash (aref payload (+ i 3)) 16)
                              (ash (aref payload (+ i 4)) 8)
                              (aref payload (+ i 5))))))

(defun parse-window-update-payload (payload)
  "Parse a WINDOW_UPDATE frame payload"
  (logior (ash (logand (aref payload 0) #x7f) 24)
          (ash (aref payload 1) 16)
          (ash (aref payload 2) 8)
          (aref payload 3)))

(defun parse-error-code-payload (payload)
  "Parse a RST_STREAM frame payload"
  (logior (ash (aref payload 0) 24)
          (ash (aref payload 1) 16)
          (ash (aref payload 2) 8)
          (aref payload 3)))

(defun extract-header-block (frame)
  "Extract the header block fragment from a HEADERS or CONTINUATION frame.
Strips padding (if PADDED flag set) and priority data (if PRIORITY flag set)."
  (let* ((payload (frame-payload frame))
         (flags (frame-flags frame))
         (padded-p (plusp (logand flags +flag-padded+)))
         (priority-p (plusp (logand flags +flag-priority+)))
         (pos 0)
         (pad-length 0))
    ;; Handle PADDED flag - first byte is pad length
    (when padded-p
      (setf pad-length (aref payload 0))
      (incf pos))
    ;; Handle PRIORITY flag - 5 bytes of priority data
    (when priority-p
      (incf pos 5))
    ;; Extract header block (everything except padding at end)
    (let* ((end (- (length payload) pad-length))
           (header-block (make-array (- end pos) :element-type '(unsigned-byte 8))))
      (replace header-block payload :start2 pos :end2 end)
      header-block)))

;;;; ========================================================================
;;;; Frame Construction Helpers
;;;; ========================================================================

(defun make-settings-frame (&key (ack nil) settings)
  "Create a SETTINGS frame"
  (let ((payload (if ack
                     #()
                     (make-settings-payload settings))))
    (make-instance 'settings-frame
                   :flags (if ack +flag-ack+ 0)
                   :payload payload
                   :settings settings)))

(defun make-settings-payload (settings)
  "Create a SETTINGS frame payload from an alist"
  (let ((payload (make-array (* 6 (length settings))
                             :element-type '(unsigned-byte 8))))
    (loop for (id . value) in settings
          for i from 0 by 6
          do (setf (aref payload i) (ash id -8)
                   (aref payload (1+ i)) (logand id #xff)
                   (aref payload (+ i 2)) (ash value -24)
                   (aref payload (+ i 3)) (logand (ash value -16) #xff)
                   (aref payload (+ i 4)) (logand (ash value -8) #xff)
                   (aref payload (+ i 5)) (logand value #xff)))
    payload))

(defun make-headers-frame (stream-id header-block &key end-stream end-headers)
  "Create a HEADERS frame"
  (make-instance 'headers-frame
                 :stream-id stream-id
                 :flags (logior (if end-stream +flag-end-stream+ 0)
                                (if end-headers +flag-end-headers+ 0))
                 :payload header-block))

(defun make-data-frame (stream-id data &key end-stream)
  "Create a DATA frame"
  (make-instance 'data-frame
                 :stream-id stream-id
                 :flags (if end-stream +flag-end-stream+ 0)
                 :payload data
                 :data data))

(defun make-window-update-frame (stream-id increment)
  "Create a WINDOW_UPDATE frame"
  (let ((payload (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref payload 0) (logand (ash increment -24) #x7f)
          (aref payload 1) (logand (ash increment -16) #xff)
          (aref payload 2) (logand (ash increment -8) #xff)
          (aref payload 3) (logand increment #xff))
    (make-instance 'window-update-frame
                   :stream-id stream-id
                   :payload payload
                   :window-size-increment increment)))

(defun make-ping-frame (opaque-data &key ack)
  "Create a PING frame"
  (make-instance 'ping-frame
                 :flags (if ack +flag-ack+ 0)
                 :payload opaque-data
                 :opaque-data opaque-data))

(defun make-goaway-frame (last-stream-id error-code &optional debug-data)
  "Create a GOAWAY frame"
  (let* ((debug-bytes (or debug-data #()))
         (payload (make-array (+ 8 (length debug-bytes))
                              :element-type '(unsigned-byte 8))))
    (setf (aref payload 0) (logand (ash last-stream-id -24) #x7f)
          (aref payload 1) (logand (ash last-stream-id -16) #xff)
          (aref payload 2) (logand (ash last-stream-id -8) #xff)
          (aref payload 3) (logand last-stream-id #xff)
          (aref payload 4) (ash error-code -24)
          (aref payload 5) (logand (ash error-code -16) #xff)
          (aref payload 6) (logand (ash error-code -8) #xff)
          (aref payload 7) (logand error-code #xff))
    (loop for i from 0 below (length debug-bytes)
          do (setf (aref payload (+ 8 i)) (aref debug-bytes i)))
    (make-instance 'goaway-frame
                   :payload payload
                   :last-stream-id last-stream-id
                   :error-code error-code
                   :debug-data debug-bytes)))

(defun make-rst-stream-frame (stream-id error-code)
  "Create a RST_STREAM frame to immediately terminate a stream.
ERROR-CODE is an HTTP/2 error code (e.g., +error-cancel+ for client cancellation)."
  (let ((payload (make-array 4 :element-type '(unsigned-byte 8))))
    ;; RST_STREAM payload is just the 32-bit error code
    (setf (aref payload 0) (logand (ash error-code -24) #xff)
          (aref payload 1) (logand (ash error-code -16) #xff)
          (aref payload 2) (logand (ash error-code -8) #xff)
          (aref payload 3) (logand error-code #xff))
    (make-instance 'rst-stream-frame
                   :stream-id stream-id
                   :payload payload
                   :error-code error-code)))
