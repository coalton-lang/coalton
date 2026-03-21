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

(defun read-bracket-form (stream char)
  (declare (ignore char))
  (desugar-bracket-builder
   (eclector.reader:read-delimited-list #\] stream t)))

(defun install-coalton-reader-syntax (&optional (readtable eclector.readtable:*readtable*))
  (multiple-value-bind (close-paren)
      (eclector.readtable:get-macro-character readtable #\))
    (eclector.readtable:set-macro-character readtable #\[ #'read-bracket-form)
    (eclector.readtable:set-macro-character readtable #\] close-paren))
  readtable)

(defun populate-reader-sources (form &optional inherited-span)
  (let ((span (or (cst:source form) inherited-span)))
    (when (and span
               (null (cst:source form)))
      (setf (cst:source form) span))
    (when (cst:consp form)
      (populate-reader-sources (cst:first form) span)
      (populate-reader-sources (cst:rest form) span))
    form))

(defmacro with-reader-context (stream &rest body)
  "Run the body in the toplevel reader context."
  `(eclector.reader:call-as-top-level-read
    *coalton-eclector-client*
    (lambda ()
      ,@body)
    ,stream
    nil
    'eof
    nil))

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
