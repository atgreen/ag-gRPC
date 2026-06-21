;;;; constants.lisp - HTTP/2 frame and protocol constants (RFC 7540)

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

;;; RFC 7540 4.2: the initial/minimum SETTINGS_MAX_FRAME_SIZE. A peer may not
;;; send a frame larger than the value the receiver advertised; the default
;;; (and floor) is 2^14. read-frame uses this to reject oversize frames before
;;; allocating their payload (FRAME_SIZE_ERROR).
(defconstant +default-max-frame-size+ 16384)

;;; Default settings values
(defparameter *default-settings*
  `((,+settings-header-table-size+ . 4096)
    (,+settings-enable-push+ . 1)
    (,+settings-max-concurrent-streams+ . 100)
    (,+settings-initial-window-size+ . 65535)
    (,+settings-max-frame-size+ . 16384)
    (,+settings-max-header-list-size+ . 8192)))

;;; Error codes (RFC 7540 Section 7)
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
