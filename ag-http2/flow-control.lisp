;;;; flow-control.lisp - HTTP/2 flow control (RFC 7540 Section 5.2)

(in-package #:ag-http2)

;;;; ========================================================================
;;;; Error Codes (RFC 7540 Section 7)
;;;; ========================================================================

(defconstant +error-no-error+ #x0)
(defconstant +error-protocol-error+ #x1)
(defconstant +error-internal-error+ #x2)
(defconstant +error-flow-control-error+ #x3)
(defconstant +error-settings-timeout+ #x4)
(defconstant +error-stream-closed+ #x5)
(defconstant +error-frame-size-error+ #x6)
(defconstant +error-refused-stream+ #x7)
(defconstant +error-cancel+ #x8)
(defconstant +error-compression-error+ #x9)
(defconstant +error-connect-error+ #xa)
(defconstant +error-enhance-your-calm+ #xb)
(defconstant +error-inadequate-security+ #xc)
(defconstant +error-http-1-1-required+ #xd)

(defun error-code-name (code)
  "Return the name of an error code"
  (case code
    (#.+error-no-error+ "NO_ERROR")
    (#.+error-protocol-error+ "PROTOCOL_ERROR")
    (#.+error-internal-error+ "INTERNAL_ERROR")
    (#.+error-flow-control-error+ "FLOW_CONTROL_ERROR")
    (#.+error-settings-timeout+ "SETTINGS_TIMEOUT")
    (#.+error-stream-closed+ "STREAM_CLOSED")
    (#.+error-frame-size-error+ "FRAME_SIZE_ERROR")
    (#.+error-refused-stream+ "REFUSED_STREAM")
    (#.+error-cancel+ "CANCEL")
    (#.+error-compression-error+ "COMPRESSION_ERROR")
    (#.+error-connect-error+ "CONNECT_ERROR")
    (#.+error-enhance-your-calm+ "ENHANCE_YOUR_CALM")
    (#.+error-inadequate-security+ "INADEQUATE_SECURITY")
    (#.+error-http-1-1-required+ "HTTP_1_1_REQUIRED")
    (t (format nil "UNKNOWN_ERROR_~A" code))))

;;;; ========================================================================
;;;; Flow Control Constants
;;;; ========================================================================

(defconstant +default-initial-window-size+ 65535
  "Default initial window size (2^16 - 1)")

(defconstant +max-window-size+ (1- (ash 1 31))
  "Maximum window size (2^31 - 1)")

(defparameter +connection-preface+
  ;; "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" with actual CR LF bytes
  (make-array 24 :element-type '(unsigned-byte 8)
              :initial-contents '(80 82 73 32 42 32 72 84 84 80 47 50 46 48  ; "PRI * HTTP/2.0"
                                  13 10 13 10                                 ; "\r\n\r\n"
                                  83 77                                       ; "SM"
                                  13 10 13 10))                               ; "\r\n\r\n"
  "HTTP/2 connection preface magic bytes")

;;;; ========================================================================
;;;; Flow Control Window
;;;; ========================================================================

(defclass flow-control-window ()
  ((size :initarg :size :accessor window-size
         :initform +default-initial-window-size+
         :documentation "Current window size")
   (initial-size :initarg :initial-size :accessor window-initial-size
                 :initform +default-initial-window-size+
                 :documentation "Initial window size from settings"))
  (:documentation "Flow control window for a stream or connection"))

(defun make-flow-control-window (&key (initial-size +default-initial-window-size+))
  "Create a new flow control window"
  (make-instance 'flow-control-window
                 :size initial-size
                 :initial-size initial-size))

(defun window-consume (window amount)
  "Consume bytes from the window. Returns T if successful, NIL if would go negative."
  (when (>= (window-size window) amount)
    (decf (window-size window) amount)
    t))

(defun window-increment (window amount)
  "Increment the window size. Signals error if would exceed max."
  (let ((new-size (+ (window-size window) amount)))
    (when (> new-size +max-window-size+)
      (error 'http2-connection-error
             :message "Flow control window overflow"
             :error-code +error-flow-control-error+))
    (setf (window-size window) new-size)))

(defun window-update-initial-size (window new-initial-size)
  "Update the initial window size and adjust current size accordingly."
  (let ((delta (- new-initial-size (window-initial-size window))))
    (setf (window-initial-size window) new-initial-size)
    (window-increment window delta)))
