(defpackage #:coalton-impl/codegen/pattern
  (:use
   #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:pattern                            ; STRUCT
   #:pattern-type                       ; ACCESSOR
   #:pattern-list                       ; TYPE
   #:pattern-var                        ; STRUCT
   #:make-pattern-var                   ; ACCESSOR
   #:pattern-var-name                   ; ACCESSOR
   #:pattern-var-p                      ; FUNCTION
   #:skolem-pattern-var                 ; STRUCT
   #:make-skolem-pattern-var            ; ACCESSOR
   #:skolem-pattern-var-predicates      ; ACCESSOR
   #:skolem-pattern-var-p               ; FUNCTION
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

(in-package #:coalton-impl/codegen/pattern)

(defstruct (pattern
            (:constructor nil)
            (:copier nil))
  (type (util:required 'type) :type tc:ty :read-only t))

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
  (name (util:required 'name) :type symbol :read-only t))

(defstruct (skolem-pattern-var
            (:include pattern-var)
            (:copier nil))
  (predicates (util:required 'predicates) :type tc:ty-predicate-list :read-only t))

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
  (name     (util:required 'name)     :type symbol       :read-only t)
  (patterns (util:required 'patterns) :type pattern-list :read-only t))

(defun pattern-variables (pattern)
  (delete-duplicates (pattern-variables-generic% pattern) :test #'eq))

(defgeneric pattern-variables-generic% (pattern)
  (:method ((pattern pattern-var))
    (declare (values util:symbol-list))

    (list (pattern-var-name pattern)))

  (:method ((pattern pattern-literal))
    (declare (values util:symbol-list))

    nil)

  (:method ((pattern pattern-wildcard))
    (declare (values util:symbol-list))

    nil)

  (:method ((pattern pattern-constructor))
    (declare (values util:symbol-list))

    (mapcan #'pattern-variables-generic% (pattern-constructor-patterns pattern))))

;;;
;;; Methods
;;;

(defmethod tc:apply-substitution (subs (pattern pattern-var))
  (declare (type tc:substitution-list subs)
           (values pattern-var &optional))
  (make-pattern-var
   :type (tc:apply-substitution subs (pattern-type pattern))
   :name (pattern-var-name pattern)))

(defmethod tc:apply-substitution (subs (pattern pattern-literal))
  (declare (type tc:substitution-list subs)
           (values pattern-literal &optional))
  (make-pattern-literal
   :type (tc:apply-substitution subs (pattern-type pattern))
   :value (pattern-literal-value pattern)))

(defmethod tc:apply-substitution (subs (pattern pattern-wildcard))
  (declare (type tc:substitution-list subs)
           (values pattern-wildcard &optional))
  (make-pattern-wildcard
   :type (tc:apply-substitution subs (pattern-type pattern))))

(defmethod tc:apply-substitution (subs (pattern pattern-constructor))
  (declare (type tc:substitution-list subs)
           (values pattern-constructor &optional))
  (make-pattern-constructor
   :type (tc:apply-substitution subs (pattern-type pattern))
   :name (pattern-constructor-name pattern)
   :patterns (tc:apply-substitution subs (pattern-constructor-patterns pattern))))

(defmethod tc:type-variables ((pattern pattern-var))
  (tc:type-variables (pattern-type pattern)))

(defmethod tc:type-variables ((pattern pattern-literal))
  (tc:type-variables (pattern-type pattern)))

(defmethod tc:type-variables ((pattern pattern-wildcard))
  (tc:type-variables (pattern-type pattern)))

(defmethod tc:type-variables ((pattern pattern-constructor))
  (remove-duplicates (alexandria:mappend #'tc:type-variables
                                         (cons (pattern-type pattern)
                                               (pattern-constructor-patterns pattern)))))
