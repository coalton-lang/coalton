;;;;
;;;; Mirror of patterns in src/parser/pattern.lisp with types attached.
;;;;

(defpackage #:coalton-impl/typechecker/pattern
  (:use
   #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:pattern                            ; STRUCT
   #:pattern-type                       ; ACCESSOR
   #:pattern-location                   ; ACCESSOR
   #:pattern-list-p                     ; FUNCTION
   #:pattern-list                       ; TYPE
   #:pattern-var                        ; STRUCT
   #:make-pattern-var                   ; ACCESSOR
   #:pattern-var-name                   ; ACCESSOR
   #:pattern-var-orig-name              ; ACCESSOR
   #:pattern-var-p                      ; FUNCTION
   #:pattern-binding                    ; STRUCT
   #:pattern-binding-var                ; ACCESSOR
   #:pattern-binding-pattern            ; ACCESSOR
   #:make-pattern-binding               ; CONSTRUCTOR
   #:pattern-binding-p                  ; FUNCTION
   #:pattern-literal                    ; STRUCT
   #:make-pattern-literal               ; CONSTRUCTOR
   #:pattern-literal-value              ; ACCESSOR
   #:pattern-literal-p                  ; FUNCTION
   #:pattern-wildcard                   ; STRUCT
   #:make-pattern-wildcard              ; ACCESSOR
   #:pattern-wildcard-p                 ; FUNCTION
   #:pattern-constructor                ; STRUCT
   #:make-pattern-constructor           ; CONSTRUCTOR
   #:pattern-constructor-name           ; ACCESSOR
   #:pattern-constructor-patterns       ; ACCESSOR
   #:pattern-constructor-p              ; FUNCTION
   #:pattern-variables                  ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/pattern)

(defstruct (pattern
            (:constructor nil)
            (:copier nil))
  (type     (util:required 'type) :type tc:qualified-ty           :read-only t)
  (location nil                   :type (or source:location null) :read-only t))

(defmethod source:location ((self pattern))
  (pattern-location self))

(defun pattern-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'pattern-p x)))

(deftype pattern-list ()
  '(satisfies pattern-list-p))

(defstruct (pattern-var
            (:include pattern)
            (:copier nil))
  (name      (util:required 'name)      :type parser:identifier :read-only t)
  (orig-name (util:required 'orig-name) :type parser:identifier :read-only t))

(defstruct (pattern-binding
            (:include pattern)
            (:copier nil))
  (var     (util:required 'var)     :type pattern-var :read-only t)
  (pattern (util:required 'pattern) :type pattern     :read-only t))

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
  (name     (util:required 'name)     :type parser:identifier :read-only t)
  (patterns (util:required 'patterns) :type pattern-list      :read-only t))

;;;
;;; Methods
;;;

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

  (:method ((pattern pattern-binding))
    (declare (values pattern-var-list &optional))
    (cons (pattern-binding-var pattern)
          (pattern-variables-generic% (pattern-binding-pattern pattern))))

  (:method ((list list))
    (declare (values pattern-var-list))
    (mapcan #'pattern-variables-generic% list)))

(defmethod tc:apply-substitution (subs (node pattern-var))
  (declare (type tc:substitution-list subs)
           (values pattern-var &optional))
  (make-pattern-var
   :type (tc:apply-substitution subs (pattern-type node))
   :location (pattern-location node)
   :name (pattern-var-name node)
   :orig-name (pattern-var-orig-name node)))

(defmethod tc:apply-substitution (subs (node pattern-literal))
  (declare (type tc:substitution-list subs)
           (values pattern-literal &optional))
  (make-pattern-literal
   :type (tc:apply-substitution subs (pattern-type node))
   :location (pattern-location node)
   :value (pattern-literal-value node)))

(defmethod tc:apply-substitution (subs (node pattern-wildcard))
  (declare (type tc:substitution-list subs)
           (values pattern-wildcard &optional))
  (make-pattern-wildcard
   :type (tc:apply-substitution subs (pattern-type node))
   :location (pattern-location node)))

(defmethod tc:apply-substitution (subs (node pattern-binding))
  (declare (type tc:substitution-list subs)
           (values pattern-binding &optional))
  (make-pattern-binding
   :type (tc:apply-substitution subs (pattern-type node))
   :location (pattern-location node)
   :var (tc:apply-substitution subs (pattern-binding-var node))
   :pattern (tc:apply-substitution subs (pattern-binding-pattern node))))

(defmethod tc:apply-substitution (subs (node pattern-constructor))
  (declare (type tc:substitution-list subs)
           (values pattern-constructor &optional))
  (make-pattern-constructor
   :type (tc:apply-substitution subs (pattern-type node))
   :location (pattern-location node)
   :name (pattern-constructor-name node)
   :patterns (tc:apply-substitution subs (pattern-constructor-patterns node))))
