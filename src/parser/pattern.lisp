(defpackage #:coalton-impl/parser/pattern
  (:use
   #:cl
   #:coalton-impl/parser/base)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:se #:source-error)
   (#:util #:coalton-impl/util))
  (:export
   #:pattern                            ; STRUCT
   #:pattern-source                     ; ACCESSOR
   #:pattern-list                       ; TYPE
   #:pattern-var                        ; STRUCT
   #:make-pattern-var                   ; ACCESSOR
   #:pattern-var-name                   ; ACCESSOR
   #:pattern-var-orig-name              ; ACCESSOR
   #:pattern-var-p                      ; FUNCTION
   #:pattern-literal                    ; STRUCT
   #:make-pattern-literal               ; CONSTRUCTOR
   #:pattern-literal-value              ; ACCESSOR
   #:pattern-wildcard                   ; STRUCT
   #:make-pattern-wildcard              ; ACCESSOR
   #:pattern-constructor                ; STRUCT
   #:make-pattern-constructor           ; CONSTRUCTOR
   #:pattern-constructor-name           ; ACCESSOR
   #:pattern-constructor-patterns       ; ACCESSOR
   #:parse-pattern                      ; FUNCTION
   #:pattern-variables                  ; FUNCTION
   ))

(in-package #:coalton-impl/parser/pattern)

;;;; # Pattern Parsing
;;;;
;;;; literal := <a lisp literal value>
;;;;
;;;; variable := <a lisp symbol not including "_">
;;;;
;;;; pattern := literal
;;;;          | variable
;;;;          | "_"
;;;;          | "(" variable pattern* ")"

(defstruct (pattern
            (:constructor nil)
            (:copier nil))
  (source (util:required 'source) :type cons :read-only t))

(defmethod make-load-form ((self pattern) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun pattern-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'pattern-p x)))

(deftype pattern-list ()
  '(satisfies pattern-list-p))

(defstruct (pattern-var
            (:include pattern)
            (:copier nil))
  (name      (util:required 'name)      :type identifier :read-only t)
  (orig-name (util:required 'orig-name) :type identifier :read-only t))

(defun pattern-var-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'pattern-var-p x)))

(deftype pattern-var-list ()
  '(satisfies pattern-var-list-p))

(defstruct (pattern-literal
            (:include pattern)
            (:copier nil))
  (value (util:required 'value) :type util:literal-value :read-only t))

(defstruct (pattern-wildcard
            (:include pattern)
            (:copier nil)))

(defstruct (pattern-constructor
            (:include pattern)
            (:copier nil))
  (name     (util:required 'name)     :type identifier   :read-only t)
  (patterns (util:required 'patterns) :type pattern-list :read-only t))

(defun parse-pattern (form file)
  (declare (type cst:cst form)
           (type se:file file))

  (cond
    ((and (cst:atom form)
          (typep (cst:raw form) 'util:literal-value))
     (make-pattern-literal
      :value (cst:raw form)
      :source (cst:source form)))

    ((and (cst:atom form)
          (eq (cst:raw form) 'coalton:_))
     (make-pattern-wildcard
      :source (cst:source form)))

    ((and (cst:atom form)
          (identifierp (cst:raw form)))
     (when (string= "_" (symbol-name (cst:raw form)))
       (error 'parse-error
              :err  (se:source-error
                     :span (cst:source form)
                     :file file
                     :message "Invalid variable"
                     :primary-note "invalid variable name '_'")))
     (make-pattern-var
      :name (cst:raw form)
      :orig-name (cst:raw form)
      :source (cst:source form)))

    ((cst:atom form)
     (error 'parse-error
            :err (se:source-error
                  :span (cst:source form)
                  :file file
                  :message "Invalid pattern"
                  :primary-note "unknown pattern literal")))

    ((not (cst:proper-list-p form))
     (error 'parse-error
            :err (se:source-error
                  :span (cst:source form)
                  :file file
                  :message "Invalid match branch"
                  :primary-note "unexpected dotted list")))

    ((not (and (cst:atom (cst:first form))
               (identifierp (cst:raw (cst:first form)))))
     (error 'parse-error
            :err (se:source-error
                  :span (cst:source (cst:first form))
                  :file file
                  :message "Invalid pattern"
                  :primary-note "invalid constructor in pattern")))

    (t
     (make-pattern-constructor
      :name (cst:raw (cst:first form))
      :patterns (loop :for patterns := (cst:rest form) :then (cst:rest patterns)
                      :while (cst:consp patterns)
                      :collect (parse-pattern (cst:first patterns) file))
      :source (cst:source form)))))

(defun pattern-variables (pattern)
  (declare (type t pattern)
           (values pattern-var-list))

  (remove-duplicates (pattern-variables-generic% pattern) :test #'eq))

(defgeneric pattern-variables-generic% (pattern)
  (:method ((pattern pattern-var))
    (declare (values pattern-var-list))
    (list pattern))

  (:method ((pattern pattern-literal))
    (declare (values pattern-var-list))
    nil)

  (:method ((pattern pattern-wildcard))
    (declare (values pattern-var-list))
    nil)

  (:method ((pattern pattern-constructor))
    (declare (values pattern-var-list &optional))
    (pattern-variables-generic% (pattern-constructor-patterns pattern)))

  (:method ((list list))
    (declare (values pattern-var-list))
    (mapcan #'pattern-variables-generic% list)))


(defmethod print-object ((obj pattern-var) stream)
  (declare (type stream stream))
  (if *print-readably*
      (call-next-method)
      (princ (pattern-var-name obj) stream)))

(defmethod print-object ((obj pattern-literal) stream)
  (declare (type stream stream))
  (if *print-readably*
      (call-next-method)
      (princ (pattern-literal-value obj) stream)))

(defmethod print-object ((obj pattern-wildcard) stream)
  (declare (type stream stream))
  (if *print-readably*
      (call-next-method)
      (princ "_" stream)))

(defmethod print-object ((obj pattern-constructor) stream)
  (when *print-readably*
    (return-from print-object (call-next-method)))

  (princ "(" stream)
  (princ (pattern-constructor-name obj) stream)
  (loop :for pattern :in (pattern-constructor-patterns obj)
        :do (princ " " stream)
        :do (princ pattern stream))
  (princ ")" stream))
