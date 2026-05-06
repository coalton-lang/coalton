(defpackage #:coalton-impl/parser/reader
  (:use #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:source #:coalton-impl/source))
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:export
   #:*coalton-eclector-client*
   #:install-coalton-reader-syntax
   #:collection-builder-marker
   #:association-builder-marker
   #:collection-comprehension-marker
   #:association-comprehension-marker
   #:association-entry-marker
   #:builder-with-marker
   #:builder-for-marker
   #:builder-below-marker
   #:builder-when-marker
   #:desugar-bracket-builder
   #:with-reader-context
   #:maybe-read-form))

(in-package #:coalton-impl/parser/reader)

(defclass coalton-eclector-client (eclector.concrete-syntax-tree:cst-client)
  ())

(defvar *coalton-eclector-client* (make-instance 'coalton-eclector-client))

(defun reader-syntax-error (control &rest args)
  (error "~?" control args))

(defun builder-marker-symbol (name)
  (intern name (find-package "COALTON-IMPL/PARSER/READER")))

(defun collection-builder-marker ()
  (builder-marker-symbol "%COLLECTION-BUILDER"))

(defun association-builder-marker ()
  (builder-marker-symbol "%ASSOCIATION-BUILDER"))

(defun collection-comprehension-marker ()
  (builder-marker-symbol "%COLLECTION-COMPREHENSION"))

(defun association-comprehension-marker ()
  (builder-marker-symbol "%ASSOCIATION-COMPREHENSION"))

(defun association-entry-marker ()
  (builder-marker-symbol "%ASSOCIATION-ENTRY"))

(defun builder-with-marker ()
  (builder-marker-symbol "%BUILDER-WITH"))

(defun builder-for-marker ()
  (builder-marker-symbol "%BUILDER-FOR"))

(defun builder-below-marker ()
  (builder-marker-symbol "%BUILDER-BELOW"))

(defun builder-when-marker ()
  (builder-marker-symbol "%BUILDER-WHEN"))

(defstruct (short-lambda-param
            (:constructor make-short-lambda-param (name source)))
  (name   (error "Missing short lambda parameter name") :type symbol :read-only t)
  (source (error "Missing short lambda parameter source") :type cons   :read-only t))

(defstruct (short-lambda-form
            (:constructor make-short-lambda-form
                          (params start dot-position)))
  (params       nil                                      :type list    :read-only t)
  (start        (error "Missing short lambda start")     :type integer :read-only t)
  (dot-position (error "Missing short lambda dot")       :type integer :read-only t))

(defstruct (bracket-form
            (:constructor make-bracket-form (source)))
  "Source metadata for reader-expanded bracket builder syntax."
  (source (error "Missing bracket form source") :type cons :read-only t))

(defvar *reader-source-table* (make-hash-table :test #'eq)
  "Map raw reader expansion lists to source metadata while reading.")

;; Reader macros must return the real raw expansion lists so enclosing CSTs see
;; their eventual forms, not internal marker objects.  This table carries
;; syntax-specific source spans forward until Eclector builds the CST for the
;; exact list object returned by the reader macro.

(defun make-cst-atom (raw source)
  (make-instance 'cst:atom-cst
    :raw raw
    :source source))

(defun make-cst-list (source elements &optional raw)
  (if raw
      (labels ((build (raw-tail elements)
                 (if elements
                     (make-instance 'cst:cons-cst
                       :raw raw-tail
                       :source source
                       :first (first elements)
                       :rest (build (rest raw-tail)
                                    (rest elements)))
                     (make-cst-atom nil source))))
        (build raw elements))
      (loop :with result := (make-cst-atom nil source)
            :for element :in (reverse elements)
            :do (setf result (cst:cons element result :source source))
            :finally (return result))))

(defun span-from-csts (default-source &rest forms)
  (let ((sources (remove nil (mapcar #'cst:source forms))))
    (if sources
        (cons (car (first sources))
              (cdr (first (last sources))))
        default-source)))

(defun bracket-opener-source (source)
  (cons (car source) (1+ (car source))))

(defun short-lambda-char-string (char)
  (let* ((readtable (eclector.reader:state-value eclector.base:*client* 'cl:*readtable*))
         (string (string char)))
    (ecase (eclector.readtable:readtable-case readtable)
      (:upcase
       (string-upcase string))
      (:downcase
       (string-downcase string))
      (:preserve
       string)
      (:invert
       (cond
         ((upper-case-p char)
          (string-downcase string))
         ((lower-case-p char)
          (string-upcase string))
         (t
          string))))))

(defun short-lambda-char-symbol (char)
  (intern (short-lambda-char-string char)
          (eclector.reader:state-value eclector.base:*client* '*package*)))

(defun read-short-lambda-form (stream char)
  "Reader macro for `ƒx.body`, desugaring it to `(fn (x) body)`."
  (declare (ignore char))
  (let ((start (1- (file-position stream)))
        (params nil)
        (seen-names (make-hash-table :test #'eq)))
    (loop :for param-char := (read-char stream nil nil)
          :for position := (and param-char (1- (file-position stream)))
          :do
             (cond
               ((null param-char)
                (reader-syntax-error "Malformed short lambda: missing `.`"))
               ((char= #\. param-char)
                (let ((next-char (peek-char t stream nil nil)))
                  (unless next-char
                    (reader-syntax-error "Malformed short lambda: missing body expression")))
                (let* ((params (nreverse params))
                       (body (eclector.reader:read stream t nil t))
                       (raw (list 'coalton:fn
                                  (mapcar #'short-lambda-param-name params)
                                  body)))
                  (setf (gethash raw *reader-source-table*)
                        (make-short-lambda-form params start position))
                  (return raw)))
               ((char= #\_ param-char)
                (push (make-short-lambda-param
                       (short-lambda-char-symbol param-char)
                       (cons position (1+ position)))
                      params))
               ((alpha-char-p param-char)
                (let ((name (short-lambda-char-symbol param-char)))
                  (when (gethash name seen-names)
                    (reader-syntax-error
                     "Malformed short lambda: duplicate parameter `~A`"
                     param-char))
                  (setf (gethash name seen-names) t)
                  (push (make-short-lambda-param name (cons position (1+ position)))
                        params)))
               (t
                (reader-syntax-error
                 "Malformed short lambda: expected alphabetic parameter, `_`, or `.`, got `~A`"
                 param-char))))))

(defun short-lambda-cst (expression form children source)
  (unless (= 1 (length children))
    (reader-syntax-error "Malformed short lambda: expected one body expression"))
  (let* ((body (first children))
         (start (short-lambda-form-start form))
         (dot-position (short-lambda-form-dot-position form))
         (source (if (cst:source body)
                     (cons start (cdr (cst:source body)))
                     source))
         (fn-form (make-cst-atom 'coalton:fn (cons start (1+ start))))
         (params (loop :for param :in (short-lambda-form-params form)
                       :collect (make-cst-atom
                                 (short-lambda-param-name param)
                                 (short-lambda-param-source param))))
         (param-list (make-cst-list (cons (1+ start) dot-position)
                                    params
                                    (second expression))))
    (make-cst-list source (list fn-form param-list body) expression)))

(defun marker-name= (form name)
  (and (symbolp form)
       (string= (symbol-name form) name)))

(defun clause-marker-p (form)
  (or (eq form ':with)
      (eq form ':for)
      (eq form ':when)))

(defun ensure-marker (form name context)
  "Signal an error unless FORM is a symbol whose name matches NAME."
  (unless (marker-name= form name)
    (reader-syntax-error "Malformed ~A: expected ~A" context name))
  (values))

(defun parse-builder-clauses (items context)
  (loop :while items
        :for marker := (pop items)
        :collect
        (cond
          ((eq marker ':with)
           (unless items
             (reader-syntax-error "Malformed ~A: missing :WITH binder" context))
           (let ((binder (pop items)))
             (unless items
               (reader-syntax-error "Malformed ~A: missing = after :WITH binder" context))
             (ensure-marker (pop items) "=" context)
             (unless items
               (reader-syntax-error "Malformed ~A: missing :WITH expression" context))
             (list ':with binder (pop items))))
          ((eq marker ':for)
           (unless items
             (reader-syntax-error "Malformed ~A: missing :FOR binder" context))
           (let ((binder (pop items)))
             (unless items
               (reader-syntax-error "Malformed ~A: missing :IN or :BELOW after :FOR binder" context))
             (let ((range-marker (pop items)))
               (cond
                 ((eq range-marker ':in)
                  (unless items
                    (reader-syntax-error "Malformed ~A: missing :FOR iterator expression" context))
                  (list ':for binder (pop items)))
                 ((eq range-marker ':below)
                  (unless items
                    (reader-syntax-error "Malformed ~A: missing :FOR upper bound expression" context))
                  (list ':below binder (pop items)))
                 (t
                  (reader-syntax-error "Malformed ~A: expected :IN or :BELOW after :FOR binder" context))))))
          ((eq marker ':when)
           (unless items
             (reader-syntax-error "Malformed ~A: missing :WHEN predicate" context))
           (list ':when (pop items)))
          (t
           (reader-syntax-error "Malformed ~A: expected :WITH, :FOR, or :WHEN clause" context)))))

(defun builder-clause-form (clause)
  (destructuring-bind (kind &rest args) clause
    (cond
      ((eq kind ':with)
       (destructuring-bind (binder expr) args
         `(,(builder-with-marker) ,binder ,expr)))
      ((eq kind ':for)
       (destructuring-bind (binder expr) args
         `(,(builder-for-marker) ,binder ,expr)))
      ((eq kind ':below)
       (destructuring-bind (binder expr) args
         `(,(builder-below-marker) ,binder ,expr)))
      ((eq kind ':when)
       (destructuring-bind (predicate) args
         `(,(builder-when-marker) ,predicate)))
      (t
       (reader-syntax-error "Unexpected builder clause kind ~S" kind)))))

(defun collection-builder-form (items)
  `(,(collection-builder-marker) ,@items))

(defun association-entry-form (key value)
  `(,(association-entry-marker) ,key ,value))

(defun association-builder-form (pairs)
  `(,(association-builder-marker)
    ,@(loop :for (key value) :in pairs
            :collect (association-entry-form key value))))

(defun collection-comprehension-form (head clauses)
  `(,(collection-comprehension-marker)
    ,head
    ,@(mapcar #'builder-clause-form clauses)))

(defun association-comprehension-form (key value clauses)
  `(,(association-comprehension-marker)
    ,key
    ,value
    ,@(mapcar #'builder-clause-form clauses)))

(defun desugar-collection-builder (items)
  (if (and items
           (clause-marker-p (first (rest items))))
      (collection-comprehension-form (first items)
                                     (parse-builder-clauses (rest items)
                                                            "collection comprehension"))
      (collection-builder-form items)))

(defun parse-association-pairs (items context)
  (loop :while items
        :do (unless (consp items)
              (reader-syntax-error "Malformed ~A: missing key expression" context))
        :collect
        (let ((key (pop items)))
          (unless items
            (reader-syntax-error "Malformed ~A: missing => after key expression" context))
          (ensure-marker (pop items) "=>" context)
          (unless items
            (reader-syntax-error "Malformed ~A: missing value expression" context))
          (list key (pop items)))))

(defun desugar-association-builder (items)
  (cond
    ((and (= 1 (length items))
          (marker-name= (first items) "=>"))
     (association-builder-form nil))
    ((null items)
     (association-builder-form nil))
    ((or (null (rest items))
         (null (rest (rest items))))
     (reader-syntax-error "Malformed association builder: expected KEY => VALUE"))
    (t
     (let ((key (first items))
           (arrow (second items))
           (value (third items))
           (rest-items (cdddr items)))
       (ensure-marker arrow "=>" "association builder")
       (if (and rest-items
                (clause-marker-p (first rest-items)))
           (association-comprehension-form key
                                           value
                                           (parse-builder-clauses rest-items
                                                                  "association comprehension"))
           (association-builder-form
            (cons (list key value)
                  (parse-association-pairs rest-items "association builder"))))))))

(defun association-builder-items-p (items)
  (or (and (= 1 (length items))
           (marker-name= (first items) "=>"))
      (and (consp (rest items))
           (marker-name= (second items) "=>"))))

(defun desugar-bracket-builder (items)
  (if (association-builder-items-p items)
      (desugar-association-builder items)
      (desugar-collection-builder items)))

(defun cst-marker-name= (form name)
  (and (cst:atom form)
       (marker-name= (cst:raw form) name)))

(defun ensure-cst-marker (form name context)
  (unless (cst-marker-name= form name)
    (reader-syntax-error "Malformed ~A: expected ~A" context name))
  (values))

(defun make-reader-marker-cst (marker source)
  (make-cst-atom marker source))

(defun builder-clause-cst-form (marker marker-source source elements)
  (make-cst-list source
                 (cons (make-reader-marker-cst marker marker-source)
                       elements)))

(defun parse-builder-clause-csts (items context)
  (loop :while items
        :for marker := (pop items)
        :collect
        (cond
          ((and (cst:atom marker)
                (eq (cst:raw marker) ':with))
           (unless items
             (reader-syntax-error "Malformed ~A: missing :WITH binder" context))
           (let ((binder (pop items)))
             (unless items
               (reader-syntax-error "Malformed ~A: missing = after :WITH binder" context))
             (ensure-cst-marker (pop items) "=" context)
             (unless items
               (reader-syntax-error "Malformed ~A: missing :WITH expression" context))
             (let ((expr (pop items)))
               (builder-clause-cst-form
                (builder-with-marker)
                (cst:source marker)
                (span-from-csts nil marker expr)
                (list binder expr)))))
          ((and (cst:atom marker)
                (eq (cst:raw marker) ':for))
           (unless items
             (reader-syntax-error "Malformed ~A: missing :FOR binder" context))
           (let ((binder (pop items)))
             (unless items
               (reader-syntax-error "Malformed ~A: missing :IN or :BELOW after :FOR binder" context))
             (let ((range-marker (pop items)))
               (cond
                 ((cst-marker-name= range-marker "IN")
                  (unless items
                    (reader-syntax-error "Malformed ~A: missing :FOR iterator expression" context))
                  (let ((expr (pop items)))
                    (builder-clause-cst-form
                     (builder-for-marker)
                     (cst:source marker)
                     (span-from-csts nil marker expr)
                     (list binder expr))))
                 ((cst-marker-name= range-marker "BELOW")
                  (unless items
                    (reader-syntax-error "Malformed ~A: missing :FOR upper bound expression" context))
                  (let ((expr (pop items)))
                    (builder-clause-cst-form
                     (builder-below-marker)
                     (cst:source marker)
                     (span-from-csts nil marker expr)
                     (list binder expr))))
                 (t
                  (reader-syntax-error "Malformed ~A: expected :IN or :BELOW after :FOR binder" context))))))
          ((and (cst:atom marker)
                (eq (cst:raw marker) ':when))
           (unless items
             (reader-syntax-error "Malformed ~A: missing :WHEN predicate" context))
           (let ((predicate (pop items)))
             (builder-clause-cst-form
              (builder-when-marker)
              (cst:source marker)
              (span-from-csts nil marker predicate)
              (list predicate))))
          (t
           (reader-syntax-error "Malformed ~A: expected :WITH, :FOR, or :WHEN clause" context)))))

(defun association-entry-cst-form (key arrow value)
  (make-cst-list (span-from-csts nil key value)
                 (list (make-reader-marker-cst (association-entry-marker)
                                               (cst:source arrow))
                       key
                       value)))

(defun parse-association-entry-csts (items context)
  (loop :while items
        :collect
        (let ((key (pop items)))
          (unless items
            (reader-syntax-error "Malformed ~A: missing => after key expression" context))
          (let ((arrow (pop items)))
            (ensure-cst-marker arrow "=>" context)
            (unless items
              (reader-syntax-error "Malformed ~A: missing value expression" context))
            (association-entry-cst-form key arrow (pop items))))))

(defun collection-builder-cst (expression children source)
  (make-cst-list source
                 (cons (make-reader-marker-cst (collection-builder-marker)
                                               (bracket-opener-source source))
                       children)
                 expression))

(defun association-builder-cst (expression children source)
  (make-cst-list source
                 (cons (make-reader-marker-cst (association-builder-marker)
                                               (bracket-opener-source source))
                       (parse-association-entry-csts children
                                                     "association builder"))
                 expression))

(defun collection-comprehension-cst (expression children source)
  (unless children
    (reader-syntax-error "Malformed collection comprehension: missing head expression"))
  (let ((head (first children))
        (clauses (rest children)))
    (make-cst-list source
                   (list* (make-reader-marker-cst (collection-comprehension-marker)
                                                  (bracket-opener-source source))
                          head
                          (parse-builder-clause-csts clauses
                                                     "collection comprehension"))
                   expression)))

(defun association-comprehension-cst (expression children source)
  (unless children
    (reader-syntax-error "Malformed association comprehension: missing key expression"))
  (let ((key (pop children)))
    (unless children
      (reader-syntax-error "Malformed association comprehension: missing => after key expression"))
    (let ((arrow (pop children)))
      (ensure-cst-marker arrow "=>" "association comprehension")
      (unless children
        (reader-syntax-error "Malformed association comprehension: missing value expression"))
      (let ((value (pop children)))
        (make-cst-list source
                       (list* (make-reader-marker-cst (association-comprehension-marker)
                                                      (bracket-opener-source source))
                              key
                              value
                              (parse-builder-clause-csts children
                                                         "association comprehension"))
                       expression)))))

(defun bracket-cst (expression children form)
  (let ((marker (car expression)))
    (cond
      ((eq marker (collection-builder-marker))
       (collection-builder-cst expression children (bracket-form-source form)))
      ((eq marker (association-builder-marker))
       (if (cdr expression)
           (association-builder-cst expression children (bracket-form-source form))
           (make-cst-list (bracket-form-source form)
                          (list (make-reader-marker-cst (association-builder-marker)
                                                        (bracket-opener-source
                                                         (bracket-form-source form))))
                          expression)))
      ((eq marker (collection-comprehension-marker))
       (collection-comprehension-cst expression children (bracket-form-source form)))
      ((eq marker (association-comprehension-marker))
       (association-comprehension-cst expression children (bracket-form-source form)))
      (t
       (reader-syntax-error "Unknown bracket reader expansion ~S" marker)))))

(defun read-bracket-form (stream char)
  (declare (ignore char))
  (let* ((start (1- (file-position stream)))
         (raw (desugar-bracket-builder
               (eclector.reader:read-delimited-list #\] stream t)))
         (end (file-position stream)))
    (setf (gethash raw *reader-source-table*)
          (make-bracket-form (cons start end)))
    raw))

(defmethod eclector.parse-result:make-expression-result
    ((client coalton-eclector-client)
     (expression cons)
     children
     source)
  (let ((form (gethash expression *reader-source-table*)))
    (if (null form)
        (call-next-method)
        (progn
          (remhash expression *reader-source-table*)
          (typecase form
            (short-lambda-form
             (short-lambda-cst expression form children source))
            (bracket-form
             (bracket-cst expression children form))
            (t
             (reader-syntax-error "Unknown reader expansion source metadata ~S" form)))))))

(defun install-coalton-reader-syntax (&optional (readtable eclector.readtable:*readtable*))
  (multiple-value-bind (close-paren)
      (eclector.readtable:get-macro-character readtable #\))
    (eclector.readtable:set-macro-character readtable #\[ #'read-bracket-form)
    (eclector.readtable:set-macro-character readtable #\] close-paren)
    (eclector.readtable:set-macro-character readtable #\ƒ #'read-short-lambda-form))
  readtable)

(defun cst-proper-list-elements (form)
  (loop :for tail := form :then (cst:rest tail)
        :while (cst:consp tail)
        :collect (cst:first tail)))

(defun cst-proper-list-tail (form)
  (loop :for tail := form :then (cst:rest tail)
        :while (cst:consp tail)
        :finally (return tail)))

(defun reader-generated-form-source (form)
  (when (and (cst:consp form)
             (cst:atom (cst:first form)))
    (let* ((marker (cst:raw (cst:first form)))
           (elements (cst-proper-list-elements form)))
      (cond
        ((member marker
                 (list (collection-builder-marker)
                       (association-builder-marker)
                       (collection-comprehension-marker)
                       (association-comprehension-marker))
                 :test #'eq)
         (or (cst:source (cst-proper-list-tail form))
             (span-from-csts nil (first elements) (first (last elements)))))
        ((eq marker (association-entry-marker))
         (span-from-csts nil (second elements) (third elements)))
        ((member marker
                 (list (builder-with-marker)
                       (builder-for-marker)
                       (builder-below-marker)
                       (builder-when-marker))
                 :test #'eq)
         (span-from-csts nil (first elements) (first (last elements))))))))

(defun populate-reader-sources (form &optional inherited-span)
  (let ((span (or (cst:source form) inherited-span)))
    (when (and span
               (null (cst:source form)))
      (setf (cst:source form) span))
    (when (cst:consp form)
      (populate-reader-sources (cst:first form) span)
      (populate-reader-sources (cst:rest form) span)
      (let ((reader-source (reader-generated-form-source form)))
        (when reader-source
          (setf (cst:source form) reader-source))))
    form))

(defmacro with-reader-context (stream &rest body)
  "Run the body in the toplevel reader context."
  `(let ((*reader-source-table* (make-hash-table :test #'eq)))
     (eclector.reader:call-as-top-level-read
      *coalton-eclector-client*
      (lambda ()
        ,@body)
      ,stream
      nil
      'eof
      nil)))

(defun maybe-read-form (stream source &optional (eclector-client eclector.base:*client*))
  "Read the next form or return if there is no next form.

Returns (VALUES FORM PRESENTP EOFP)"
  (let ((begin (file-position stream)))
    (handler-case
        (loop :do
          ;; On empty lists report nothing
          (when (eq #\) (peek-char t stream nil))
            (read-char stream)
            (return (values nil nil nil)))

          ;; Otherwise, try to read in the next form
          (multiple-value-call
              (lambda (form type &optional parse-result)

                ;; Return the read form when valid
                (when (eq :object type)
                  (let ((reader-form (or parse-result form)))
                    (return (values (if (typep reader-form 'cst:cst)
                                        (populate-reader-sources reader-form)
                                        reader-form)
                                    t
                                    nil))))

                (when (eq :eof type)
                  (return (values nil nil t))))

            (eclector.reader:read-maybe-nothing
             eclector-client
             stream
             nil 'eof)))
      (eclector.reader:unterminated-list ()
        (let ((end (file-position stream)))
          (parse-error "Unterminated form"
                       (source:note (source:make-location source (cons begin end))
                                    "Missing close parenthesis for form starting at offset ~a" begin))))
      (error (condition)
        (let ((end (file-position stream)))
          (parse-error "Reader error"
                       (source:note (source:make-location source (cons begin end))
                                    "reader error: ~a" condition)))))))
