(defpackage #:coalton-impl/parser/pattern
  (:use
   #:cl
   #:coalton-impl/parser/base)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:util #:coalton-impl/util))
  (:export
   #:pattern                            ; STRUCT
   #:pattern-source                     ; ACCESSOR
   #:pattern-list                       ; TYPE
   #:pattern-var                        ; STRUCT
   #:make-pattern-var                   ; ACCESSOR
   #:pattern-var-name                   ; ACCESSOR
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
   ))

(in-package #:coalton-impl/parser/pattern)

;;;; # Pattern Parsing
;;;;
;;;; literal := <a lisp literal value>
;;;;
;;;; variable := <a lisp symbol>
;;;;
;;;; pattern := literal
;;;;          | variable
;;;;          | "_"
;;;;          | "(" variable pattern* ")"

(defstruct (pattern
            (:constructor nil)
            (:copier nil))
  (source (util:required 'source) :type cons :read-only t))

(defun pattern-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'pattern-p x)))

(deftype pattern-list ()
  '(satisfies pattern-list-p))

(defstruct (pattern-var
            (:include pattern)
            (:copier nil))
  (name (util:required 'name) :type identifier :read-only t))

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
           (type file-stream file))

  (when (cst:atom form)
    (when (typep (cst:raw form) 'util:literal-value)
      (return-from parse-pattern
        (make-pattern-literal
         :value (cst:raw form)
         :source (cst:source form))))

    (when (eq (cst:raw form) 'coalton:_)
      (return-from parse-pattern
        (make-pattern-wildcard
         :source (cst:source form))))

    (when (identifierp (cst:raw form))
      (return-from parse-pattern
        (make-pattern-var
         :name (cst:raw form)
         :source (cst:source form))))

    (error 'parse-error
           :err (coalton-error
                 form file
                 :message "Invalid pattern"
                 :primary-note "unknown pattern literal")))

  (unless (cst:proper-list-p form)
    (error 'parse-error
             :err (coalton-error
                   form file
                   :message "Invalid match branch"
                   :primary-note "unexpected dotted list")))

  (let ((name)
        (patterns))

    (unless (and (cst:atom (cst:first form))
                 (identifierp (cst:raw (cst:first form))))
      (error 'parse-error
             :err (coalton-error
                   (cst:first form) file
                   :message "Invalid pattern"
                   :primary-note "invalid constructor in pattern")))

    (setf name (cst:raw (cst:first form)))

    (setf patterns
          (loop :for patterns := (cst:rest form) :then (cst:rest patterns)
                :while (cst:consp patterns)
                :collect (parse-pattern (cst:first patterns) file)))

    (make-pattern-constructor
     :name name
     :patterns patterns
     :source (cst:source form))))
