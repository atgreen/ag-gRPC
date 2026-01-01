;;;; frames.lisp - HTTP/2 frame types (RFC 7540 Section 6)

(in-package #:ag-http2)

;;;; ========================================================================
;;;; Frame Constants
;;;; ========================================================================

;;; Frame types (RFC 7540 Section 6)
(defconstant +frame-type-data+ #x0)
(defconstant +frame-type-headers+ #x1)
(defconstant +frame-type-priority+ #x2)
(defconstant +frame-type-rst-stream+ #x3)
(defconstant +frame-type-settings+ #x4)
(defconstant +frame-type-push-promise+ #x5)
(defconstant +frame-type-ping+ #x6)
(defconstant +frame-type-goaway+ #x7)
(defconstant +frame-type-window-update+ #x8)
(defconstant +frame-type-continuation+ #x9)

;;; Common flags
(defconstant +flag-end-stream+ #x1)
(defconstant +flag-end-headers+ #x4)
(defconstant +flag-padded+ #x8)
(defconstant +flag-priority+ #x20)
(defconstant +flag-ack+ #x1)

;;; Settings identifiers (RFC 7540 Section 6.5.2)
(defconstant +settings-header-table-size+ #x1)
(defconstant +settings-enable-push+ #x2)
(defconstant +settings-max-concurrent-streams+ #x3)
(defconstant +settings-initial-window-size+ #x4)
(defconstant +settings-max-frame-size+ #x5)
(defconstant +settings-max-header-list-size+ #x6)

;;; Default settings values
(defparameter *default-settings*
  `((,+settings-header-table-size+ . 4096)
    (,+settings-enable-push+ . 1)
    (,+settings-max-concurrent-streams+ . 100)
    (,+settings-initial-window-size+ . 65535)
    (,+settings-max-frame-size+ . 16384)
    (,+settings-max-header-list-size+ . 8192)))

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
  (:default-initargs :type +frame-type-data+))

(defclass headers-frame (frame)
  ((headers :initarg :headers :accessor headers-frame-headers
            :documentation "Decoded header list")
   (priority :initarg :priority :accessor headers-frame-priority
             :initform nil
             :documentation "Priority information if present"))
  (:default-initargs :type +frame-type-headers+))

(defclass priority-frame (frame)
  ((exclusive :initarg :exclusive :accessor priority-frame-exclusive)
   (stream-dependency :initarg :stream-dependency :accessor priority-frame-stream-dependency)
   (weight :initarg :weight :accessor priority-frame-weight))
  (:default-initargs :type +frame-type-priority+))

(defclass rst-stream-frame (frame)
  ((error-code :initarg :error-code :accessor rst-stream-frame-error-code))
  (:default-initargs :type +frame-type-rst-stream+))

(defclass settings-frame (frame)
  ((settings :initarg :settings :accessor settings-frame-settings
             :initform nil
             :documentation "Alist of setting id to value"))
  (:default-initargs :type +frame-type-settings+))

(defclass push-promise-frame (frame)
  ((promised-stream-id :initarg :promised-stream-id :accessor push-promise-frame-promised-stream-id)
   (headers :initarg :headers :accessor push-promise-frame-headers))
  (:default-initargs :type +frame-type-push-promise+))

(defclass ping-frame (frame)
  ((opaque-data :initarg :opaque-data :accessor ping-frame-opaque-data))
  (:default-initargs :type +frame-type-ping+))

(defclass goaway-frame (frame)
  ((last-stream-id :initarg :last-stream-id :accessor goaway-frame-last-stream-id)
   (error-code :initarg :error-code :accessor goaway-frame-error-code)
   (debug-data :initarg :debug-data :accessor goaway-frame-debug-data :initform #()))
  (:default-initargs :type +frame-type-goaway+))

(defclass window-update-frame (frame)
  ((window-size-increment :initarg :window-size-increment
                          :accessor window-update-frame-window-size-increment))
  (:default-initargs :type +frame-type-window-update+))

(defclass continuation-frame (frame)
  ((header-block :initarg :header-block :accessor continuation-frame-header-block))
  (:default-initargs :type +frame-type-continuation+))

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

(defun read-frame (stream)
  "Read an HTTP/2 frame from a binary stream"
  (let ((header (make-array 9 :element-type '(unsigned-byte 8))))
    (when (/= 9 (read-sequence header stream))
      (return-from read-frame nil))
    (let* ((length (logior (ash (aref header 0) 16)
                           (ash (aref header 1) 8)
                           (aref header 2)))
           (type (aref header 3))
           (flags (aref header 4))
           (stream-id (logior (ash (logand (aref header 5) #x7f) 24)
                              (ash (aref header 6) 16)
                              (ash (aref header 7) 8)
                              (aref header 8)))
           (payload (make-array length :element-type '(unsigned-byte 8))))
      (when (plusp length)
        (read-sequence payload stream))
      (make-frame-from-raw type flags stream-id payload))))

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
    (t
     (make-instance 'frame
                    :type type :flags flags :stream-id stream-id :payload payload))))

(defun parse-settings-payload (payload)
  "Parse a SETTINGS frame payload into an alist"
  (loop for i from 0 below (length payload) by 6
        collect (cons (logior (ash (aref payload i) 8)
                              (aref payload (+ i 1)))
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
                   (aref payload (+ i 1)) (logand id #xff)
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
