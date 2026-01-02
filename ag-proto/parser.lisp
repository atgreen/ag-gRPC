;;;; parser.lisp - Proto3 parser using iparse

(in-package #:ag-proto)

;;;; ========================================================================
;;;; Proto3 Grammar Definition
;;;;
;;;; This uses the iparse library to define a grammar for Proto3 syntax.
;;;; Reference: https://protobuf.dev/reference/protobuf/proto3-spec/
;;;; ========================================================================

(iparse:defparser proto3-parser "
  (* Main proto file structure *)
  proto = <ws> syntax? (<ws> (import / package / option / topLevelDef / emptyStatement))* <ws>

  syntax = <'syntax'> <ws> <'='> <ws> (<dquote> <'proto3'> <dquote> / <squote> <'proto3'> <squote>) <ws> <';'>

  import = <'import'> <ws> ('weak' <ws> / 'public' <ws>)? strLit <ws> <';'>

  package = <'package'> <ws> fullIdent <ws> <';'>

  option = <'option'> <ws> optionName <ws> <'='> <ws> constant <ws> <';'>

  optionName = (ident / <'('> <ws> fullIdent <ws> <')'>) (<'.'> ident)*

  topLevelDef = message / enum / service

  emptyStatement = <';'>

  (* Message definition *)
  message = <'message'> <ws> messageName <ws> messageBody

  messageBody = <'{'> (<ws> (field / enum / message / option / oneof / mapField / reserved / emptyStatement))* <ws> <'}'>

  field = fieldLabel? <ws> type <ws> fieldName <ws> <'='> <ws> fieldNumber fieldOptions? <ws> <';'>

  fieldLabel = 'repeated' / 'optional'

  fieldOptions = <ws> <'['> <ws> fieldOption (<ws> <','> <ws> fieldOption)* <ws> <']'>

  fieldOption = optionName <ws> <'='> <ws> constant

  oneof = <'oneof'> <ws> oneofName <ws> <'{'> (<ws> (oneofField / emptyStatement))* <ws> <'}'>

  oneofField = type <ws> fieldName <ws> <'='> <ws> fieldNumber fieldOptions? <ws> <';'>

  mapField = <'map'> <ws> <'<'> <ws> keyType <ws> <','> <ws> type <ws> <'>'> <ws> mapName <ws> <'='> <ws> fieldNumber fieldOptions? <ws> <';'>

  keyType = 'int32' / 'int64' / 'uint32' / 'uint64' / 'sint32' / 'sint64' /
            'fixed32' / 'fixed64' / 'sfixed32' / 'sfixed64' / 'bool' / 'string'

  reserved = <'reserved'> <ws> (ranges / fieldNames) <ws> <';'>

  ranges = range (<ws> <','> <ws> range)*

  range = intLit (<ws> 'to' <ws> (intLit / 'max'))?

  fieldNames = strLit (<ws> <','> <ws> strLit)*

  (* Enum definition *)
  enum = <'enum'> <ws> enumName <ws> enumBody

  enumBody = <'{'> (<ws> (option / enumField / emptyStatement / reserved))* <ws> <'}'>

  enumField = ident <ws> <'='> <ws> '-'? intLit enumValueOptions? <ws> <';'>

  enumValueOptions = <ws> <'['> <ws> enumValueOption (<ws> <','> <ws> enumValueOption)* <ws> <']'>

  enumValueOption = optionName <ws> <'='> <ws> constant

  (* Service definition *)
  service = <'service'> <ws> serviceName <ws> <'{'> (<ws> (option / rpc / emptyStatement))* <ws> <'}'>

  rpc = <'rpc'> <ws> rpcName <ws> <'('> <ws> 'stream'? <ws> messageType <ws> <')'> <ws>
        <'returns'> <ws> <'('> <ws> 'stream'? <ws> messageType <ws> <')'> <ws>
        (<'{'> (<ws> (option / emptyStatement))* <ws> <'}'> / <';'>)

  (* Common elements *)
  type = 'double' / 'float' / 'int32' / 'int64' / 'uint32' / 'uint64' /
         'sint32' / 'sint64' / 'fixed32' / 'fixed64' / 'sfixed32' / 'sfixed64' /
         'bool' / 'string' / 'bytes' / messageType

  messageType = '.'? (ident <'.'>)* ident

  constant = fullIdent / signedNumber / strLit / boolLit

  signedNumber = ('-' / '+')? (floatLit / intLit)

  fullIdent = ident (<'.'> ident)*

  (* Identifiers *)
  messageName = ident
  enumName = ident
  fieldName = ident
  oneofName = ident
  mapName = ident
  serviceName = ident
  rpcName = ident
  fieldNumber = intLit

  ident = #'[A-Za-z_][A-Za-z0-9_]*'

  (* Literals *)
  intLit = hexLit / octalLit / decimalLit

  decimalLit = #'[1-9][0-9]*' / '0'

  octalLit = #'0[0-7]+'

  hexLit = #'0[xX][0-9A-Fa-f]+'

  floatLit = #'[0-9]+\\.[0-9]*([eE][+-]?[0-9]+)?' /
             #'[0-9]+[eE][+-]?[0-9]+' /
             #'\\.[0-9]+([eE][+-]?[0-9]+)?' /
             'inf' / 'nan'

  boolLit = 'true' / 'false'

  strLit = dquoteStr / squoteStr

  dquoteStr = <dquote> dquoteContent <dquote>

  squoteStr = <squote> squoteContent <squote>

  dquoteContent = #'[^\"\\\\]*(?:\\\\.[^\"\\\\]*)*'

  squoteContent = #'[^\\x27\\\\]*(?:\\\\.[^\\x27\\\\]*)*'

  (* Whitespace and comments - hidden *)
  <ws> = (whitespace / lineComment / blockComment)*

  <whitespace> = #'[ \\t\\n\\r]+'

  <lineComment> = '//' #'[^\\n]*'

  <blockComment> = #'/\\*[^*]*\\*+(?:[^/*][^*]*\\*+)*/'

  <dquote> = '\"'
  <squote> = #'\\x27'
")

;;;; ========================================================================
;;;; AST Transformation
;;;; ========================================================================

(defun transform-proto-ast (ast)
  "Transform the raw parse tree into proto descriptor objects"
  (when (null ast)
    (return-from transform-proto-ast nil))
  (let ((file-desc (make-instance 'proto-file-descriptor)))
    (dolist (node (if (eq (car ast) :proto) (cdr ast) ast))
      (when (consp node)
        (case (car node)
          (:syntax
           (setf (proto-file-syntax file-desc) "proto3"))
          (:package
           (setf (proto-file-package file-desc) (extract-full-ident (cdr node))))
          (:import
           (push (extract-string-value (cdr node)) (proto-file-imports file-desc)))
          (:message
           (push (transform-message node nil) (proto-file-messages file-desc)))
          (:enum
           (push (transform-enum node nil) (proto-file-enums file-desc)))
          (:service
           (push (transform-service node) (proto-file-services file-desc)))
          (:option
           (push (transform-option node) (proto-file-options file-desc)))
          ;; Handle topLevelDef wrapper - unwrap and process inner node
          (:topLevelDef
           (dolist (inner (cdr node))
             (when (consp inner)
               (case (car inner)
                 (:message
                  (push (transform-message inner nil) (proto-file-messages file-desc)))
                 (:enum
                  (push (transform-enum inner nil) (proto-file-enums file-desc)))
                 (:service
                  (push (transform-service inner) (proto-file-services file-desc))))))))))
    ;; Reverse lists to maintain order
    (setf (proto-file-imports file-desc) (nreverse (proto-file-imports file-desc)))
    (setf (proto-file-messages file-desc) (nreverse (proto-file-messages file-desc)))
    (setf (proto-file-enums file-desc) (nreverse (proto-file-enums file-desc)))
    (setf (proto-file-services file-desc) (nreverse (proto-file-services file-desc)))
    file-desc))

(defun extract-full-ident (nodes)
  "Extract a full identifier from parse nodes"
  (with-output-to-string (s)
    (loop for node in nodes
          for first = t then nil
          do (cond
               ((stringp node)
                (unless first (write-char #\. s))
                (write-string node s))
               ((and (consp node) (eq (car node) :ident))
                (unless first (write-char #\. s))
                (write-string (cadr node) s))
               ((and (consp node) (eq (car node) :fullIdent))
                (unless first (write-char #\. s))
                (write-string (extract-full-ident (cdr node)) s))))))

(defun extract-string-value (nodes)
  "Extract string content from strLit nodes"
  (with-output-to-string (s)
    (dolist (node nodes)
      (cond
        ((stringp node) (write-string node s))
        ((and (consp node) (member (car node) '(:dquoteStr :squoteStr :strLit
                                                :dquoteContent :squoteContent)))
         (write-string (extract-string-value (cdr node)) s))
        ((and (consp node) (member (car node) '(:dquoteChar :squoteChar)))
         (write-string (extract-string-value (cdr node)) s))))))

(defun extract-int-value (node)
  "Extract integer value from intLit node"
  (cond
    ((integerp node) node)
    ((stringp node)
     (cond
       ((and (> (length node) 2)
             (char= (char node 0) #\0)
             (member (char node 1) '(#\x #\X)))
        (parse-integer node :start 2 :radix 16))
       ((and (> (length node) 1)
             (char= (char node 0) #\0))
        (parse-integer node :radix 8))
       (t (parse-integer node))))
    ((and (consp node) (member (car node) '(:intLit :decimalLit :hexLit :octalLit :fieldNumber)))
     (extract-int-value (cadr node)))
    (t 0)))

(defun transform-message (node prefix)
  "Transform a message parse node into a proto-message-descriptor"
  (let* ((name (find-name-in-node node))
         (full-name (if prefix (format nil "~A.~A" prefix name) name))
         (msg-desc (make-instance 'proto-message-descriptor
                                  :name name
                                  :full-name full-name)))
    ;; Process message body
    (dolist (child (find-body-children node))
      (when (consp child)
        (case (car child)
          (:field
           (push (transform-field child) (proto-message-fields msg-desc)))
          (:message
           (push (transform-message child full-name) (proto-message-nested-messages msg-desc)))
          (:enum
           (push (transform-enum child full-name) (proto-message-nested-enums msg-desc)))
          (:oneof
           (push (transform-oneof child msg-desc) (proto-message-oneofs msg-desc)))
          (:option
           (push (transform-option child) (proto-message-options msg-desc))))))
    ;; Reverse to maintain order
    (setf (proto-message-fields msg-desc) (nreverse (proto-message-fields msg-desc)))
    (setf (proto-message-nested-messages msg-desc) (nreverse (proto-message-nested-messages msg-desc)))
    (setf (proto-message-nested-enums msg-desc) (nreverse (proto-message-nested-enums msg-desc)))
    msg-desc))

(defun find-name-in-node (node)
  "Find the name identifier in a message/enum/service node"
  (dolist (child (cdr node))
    (cond
      ((stringp child) (return-from find-name-in-node child))
      ((and (consp child) (member (car child) '(:messageName :enumName :serviceName :ident)))
       (return-from find-name-in-node (if (stringp (cadr child)) (cadr child) (find-name-in-node child))))))
  "Unknown")

(defun find-body-children (node)
  "Find the body children in a message/enum/service node"
  (dolist (child (cdr node))
    (when (and (consp child) (member (car child) '(:messageBody :enumBody)))
      (return-from find-body-children (cdr child))))
  nil)

(defun transform-field (node)
  "Transform a field parse node into a proto-field-descriptor"
  (let ((field-desc (make-instance 'proto-field-descriptor))
        (label nil)
        (type nil)
        (name nil)
        (number nil))
    (dolist (child (cdr node))
      (cond
        ;; Handle fieldLabel node (from new grammar)
        ((and (consp child) (eq (car child) :fieldLabel))
         (let ((label-str (cadr child)))
           (cond ((equal label-str "repeated") (setf label :repeated))
                 ((equal label-str "optional") (setf label :optional)))))
        ;; Handle old-style direct string for backwards compatibility
        ((equal child "repeated") (setf label :repeated))
        ((equal child "optional") (setf label :optional))
        ((stringp child)
         (cond
           ((member child '("double" "float" "int32" "int64" "uint32" "uint64"
                            "sint32" "sint64" "fixed32" "fixed64" "sfixed32" "sfixed64"
                            "bool" "string" "bytes") :test #'string=)
            (setf type (intern (string-upcase child) :keyword)))
           ((null name) (setf name child))))
        ((and (consp child) (eq (car child) :type))
         (setf type (extract-type child)))
        ((and (consp child) (eq (car child) :fieldName))
         (setf name (find-name-in-node child)))
        ((and (consp child) (eq (car child) :fieldNumber))
         (setf number (extract-int-value child)))
        ((and (consp child) (member (car child) '(:intLit :decimalLit)))
         (setf number (extract-int-value child)))))
    (setf (proto-field-name field-desc) (or name "unknown"))
    (setf (proto-field-number field-desc) (or number 0))
    (setf (proto-field-type field-desc) (or type :unknown))
    (setf (proto-field-label field-desc) (or label :optional))
    field-desc))

(defun extract-type (node)
  "Extract type from a type node"
  (dolist (child (cdr node))
    (cond
      ((stringp child)
       (if (member child '("double" "float" "int32" "int64" "uint32" "uint64"
                           "sint32" "sint64" "fixed32" "fixed64" "sfixed32" "sfixed64"
                           "bool" "string" "bytes") :test #'string=)
           (return-from extract-type (intern (string-upcase child) :keyword))
           (return-from extract-type child)))
      ((and (consp child) (eq (car child) :messageType))
       (return-from extract-type (extract-full-ident (cdr child))))))
  :unknown)

(defun transform-enum (node prefix)
  "Transform an enum parse node into a proto-enum-descriptor"
  (let* ((name (find-name-in-node node))
         (full-name (if prefix (format nil "~A.~A" prefix name) name))
         (enum-desc (make-instance 'proto-enum-descriptor
                                   :name name
                                   :full-name full-name)))
    (dolist (child (find-body-children node))
      (when (and (consp child) (eq (car child) :enumField))
        (push (transform-enum-value child) (proto-enum-values enum-desc))))
    (setf (proto-enum-values enum-desc) (nreverse (proto-enum-values enum-desc)))
    enum-desc))

(defun transform-enum-value (node)
  "Transform an enumField parse node into a proto-enum-value-descriptor"
  (let ((name nil)
        (number nil)
        (negative nil))
    (dolist (child (cdr node))
      (cond
        ((stringp child)
         (cond
           ((string= child "-") (setf negative t))
           ((null name) (setf name child))))
        ((and (consp child) (eq (car child) :ident))
         (setf name (cadr child)))
        ((and (consp child) (member (car child) '(:intLit :decimalLit)))
         (setf number (extract-int-value child)))))
    (make-instance 'proto-enum-value-descriptor
                   :name (or name "UNKNOWN")
                   :number (if negative (- (or number 0)) (or number 0)))))

(defun transform-service (node)
  "Transform a service parse node into a proto-service-descriptor"
  (let* ((name (find-name-in-node node))
         (svc-desc (make-instance 'proto-service-descriptor
                                  :name name
                                  :full-name name)))
    (dolist (child (cdr node))
      (when (and (consp child) (eq (car child) :rpc))
        (push (transform-rpc child) (proto-service-methods svc-desc))))
    (setf (proto-service-methods svc-desc) (nreverse (proto-service-methods svc-desc)))
    svc-desc))

(defun transform-rpc (node)
  "Transform an rpc parse node into a proto-method-descriptor"
  (let ((name nil)
        (input-type nil)
        (output-type nil)
        (client-streaming nil)
        (server-streaming nil)
        (pending-stream nil)  ; true if we saw 'stream' and are waiting for messageType
        (type-count 0))       ; track which messageType we're on
    (dolist (child (cdr node))
      (cond
        ((stringp child)
         (cond
           ((string= child "stream")
            (setf pending-stream t))
           ((null name)
            (setf name child))))
        ((and (consp child) (eq (car child) :rpcName))
         (setf name (find-name-in-node child)))
        ((and (consp child) (eq (car child) :messageType))
         (incf type-count)
         (let ((type-name (extract-full-ident (cdr child))))
           (if (= type-count 1)
               (progn
                 (setf input-type type-name)
                 (when pending-stream
                   (setf client-streaming t)))
               (progn
                 (setf output-type type-name)
                 (when pending-stream
                   (setf server-streaming t)))))
         (setf pending-stream nil))))
    (make-instance 'proto-method-descriptor
                   :name (or name "Unknown")
                   :input-type (or input-type "")
                   :output-type (or output-type "")
                   :client-streaming client-streaming
                   :server-streaming server-streaming)))

(defun transform-oneof (node msg-desc)
  "Transform a oneof parse node into a proto-oneof-descriptor.
Also adds the oneof's fields to the message with oneof-index set."
  (let* ((name (find-oneof-name node))
         ;; Oneof index is the current count of oneofs in the message
         (oneof-index (length (proto-message-oneofs msg-desc)))
         (oneof-desc (make-instance 'proto-oneof-descriptor
                                    :name name
                                    :index oneof-index))
         (fields nil))
    ;; Parse each oneofField in the oneof body
    (dolist (child (cdr node))
      (when (and (consp child) (eq (car child) :oneofField))
        (let ((field (transform-oneof-field child oneof-index)))
          (push field fields)
          ;; Also add to message's fields list for serialization
          (push field (proto-message-fields msg-desc)))))
    (setf (proto-oneof-fields oneof-desc) (nreverse fields))
    oneof-desc))

(defun find-oneof-name (node)
  "Find the name in a oneof node"
  (dolist (child (cdr node))
    (cond
      ((stringp child) (return-from find-oneof-name child))
      ((and (consp child) (eq (car child) :oneofName))
       (return-from find-oneof-name (if (stringp (cadr child))
                                        (cadr child)
                                        (find-name-in-node child))))))
  "unknown")

(defun transform-oneof-field (node oneof-index)
  "Transform a oneofField parse node into a proto-field-descriptor with oneof-index set"
  (let ((field-desc (make-instance 'proto-field-descriptor))
        (type nil)
        (name nil)
        (number nil))
    (dolist (child (cdr node))
      (cond
        ((stringp child)
         (cond
           ((member child '("double" "float" "int32" "int64" "uint32" "uint64"
                            "sint32" "sint64" "fixed32" "fixed64" "sfixed32" "sfixed64"
                            "bool" "string" "bytes") :test #'string=)
            (setf type (intern (string-upcase child) :keyword)))
           ((null name) (setf name child))))
        ((and (consp child) (eq (car child) :type))
         (setf type (extract-type child)))
        ((and (consp child) (eq (car child) :fieldName))
         (setf name (find-name-in-node child)))
        ((and (consp child) (eq (car child) :fieldNumber))
         (setf number (extract-int-value child)))
        ((and (consp child) (member (car child) '(:intLit :decimalLit)))
         (setf number (extract-int-value child)))))
    (setf (proto-field-name field-desc) (or name "unknown"))
    (setf (proto-field-number field-desc) (or number 0))
    (setf (proto-field-type field-desc) (or type :unknown))
    (setf (proto-field-label field-desc) :optional)
    (setf (proto-field-oneof-index field-desc) oneof-index)
    field-desc))

(defun transform-option (node)
  "Transform an option parse node into a cons of (name . value)"
  (let ((name nil)
        (value nil))
    (dolist (child (cdr node))
      (cond
        ((and (consp child) (eq (car child) :optionName))
         (setf name (extract-full-ident (cdr child))))
        ((and (consp child) (eq (car child) :constant))
         (setf value (extract-constant-value child)))))
    (cons (or name "unknown") value)))

(defun extract-constant-value (node)
  "Extract value from a constant node"
  (dolist (child (cdr node))
    (cond
      ((stringp child)
       (cond
         ((string= child "true") (return-from extract-constant-value t))
         ((string= child "false") (return-from extract-constant-value nil))
         (t (return-from extract-constant-value child))))
      ((and (consp child) (eq (car child) :strLit))
       (return-from extract-constant-value (extract-string-value (cdr child))))
      ((and (consp child) (member (car child) '(:intLit :floatLit :signedNumber)))
       (return-from extract-constant-value (extract-int-value child)))
      ((and (consp child) (eq (car child) :fullIdent))
       (return-from extract-constant-value (extract-full-ident (cdr child))))))
  nil)

;;;; ========================================================================
;;;; Public API
;;;; ========================================================================

(defun parse-proto-string (string)
  "Parse a proto3 definition from a string.
Returns a proto-file-descriptor."
  (multiple-value-bind (ast success-p) (proto3-parser string)
    (if success-p
        (transform-proto-ast ast)
        (error 'proto-parse-error :message "Failed to parse proto3 content"))))

(defvar *include-paths* nil
  "List of directories to search for imported proto files.")

(defvar *parsed-imports* nil
  "Hash table of already-parsed import files to avoid cycles.")

(defun ensure-directory-pathname (path)
  "Ensure PATH is treated as a directory for merge-pathnames."
  (let ((p (pathname path)))
    (if (pathname-name p)
        ;; Path looks like a file (e.g., "/foo/bar" without trailing slash)
        ;; Convert to directory form
        (make-pathname :directory (append (or (pathname-directory p) '(:relative))
                                          (list (pathname-name p)))
                       :defaults p)
        p)))

(defun find-proto-file (import-path include-paths base-dir)
  "Find a proto file in include paths or relative to base directory.
Returns the full pathname if found, NIL otherwise."
  (let ((paths (append include-paths (list base-dir))))
    (dolist (dir paths)
      (let* ((dir-path (ensure-directory-pathname dir))
             (full-path (merge-pathnames import-path dir-path)))
        (when (probe-file full-path)
          (return-from find-proto-file full-path))))
    nil))

(defvar *last-parsed-imports* nil
  "After parse-proto-file completes, contains all file descriptors parsed (including imports).")

(defun parse-proto-file (pathname &key include-paths)
  "Parse a proto3 file.
INCLUDE-PATHS is a list of directories to search for imports.
Returns a proto-file-descriptor.
After parsing, *last-parsed-imports* will contain all parsed file descriptors."
  (let* ((*include-paths* (or include-paths *include-paths*))
         (*parsed-imports* (or *parsed-imports* (make-hash-table :test 'equal)))
         (abs-path (truename pathname))
         (base-dir (make-pathname :directory (pathname-directory abs-path)))
         (content (uiop:read-file-string abs-path))
         (file-desc (parse-proto-string content)))
    ;; Set the file name on the descriptor
    (setf (proto-file-name file-desc) (file-namestring abs-path))
    ;; Mark this file as parsed
    (setf (gethash (namestring abs-path) *parsed-imports*) file-desc)
    ;; Process imports
    (dolist (import-path (proto-file-imports file-desc))
      (unless (gethash import-path *parsed-imports*)
        (let ((import-file (find-proto-file import-path *include-paths* base-dir)))
          (if import-file
              (parse-proto-file import-file :include-paths *include-paths*)
              ;; Silently skip missing imports for now (common for well-known types)
              nil))))
    ;; Store all parsed imports for later use
    (setf *last-parsed-imports* *parsed-imports*)
    file-desc))

(defun get-all-enum-types ()
  "Collect all enum type names from all parsed file descriptors (including imports).
Should be called after parse-proto-file. Returns a hash table of enum names."
  (let ((all-enums (make-hash-table :test 'equal)))
    (when *last-parsed-imports*
      (maphash (lambda (path file-desc)
                 (declare (ignore path))
                 (let ((file-enums (ag-proto:collect-enum-names file-desc)))
                   (maphash (lambda (k v) (setf (gethash k all-enums) v))
                            file-enums)))
               *last-parsed-imports*))
    all-enums))
