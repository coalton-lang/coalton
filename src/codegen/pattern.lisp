(defpackage #:coalton-impl/codegen/pattern
  (:use
   #:cl)
  (:local-nicknames
   (#:pe #:coalton-impl/analysis/pattern-exhaustiveness)
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
   #:patterns-exhaustive-p              ; FUNCTION
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

(defstruct (pattern-binding
            (:include pattern)
            (:copier nil))
  (var     (util:required 'var)     :type pattern-var :read-only t)
  (pattern (util:required 'pattern) :type pattern     :read-only t))

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

  (:method ((pattern pattern-binding))
    (declare (values util:symbol-list))
    
    (append (pattern-variables-generic% (pattern-binding-var pattern))
            (pattern-variables-generic% (pattern-binding-pattern pattern))))

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

(defmethod tc:apply-substitution (subs (pattern pattern-binding))
  (declare (type tc:substitution-list subs)
           (values pattern-binding &optional))
  (make-pattern-binding
   :type (tc:apply-substitution subs (pattern-type pattern))
   :var (tc:apply-substitution subs (pattern-binding-var pattern))
   :pattern (tc:apply-substitution subs (pattern-binding-pattern pattern))))

(defmethod tc:apply-substitution (subs (pattern pattern-constructor))
  (declare (type tc:substitution-list subs)
           (values pattern-constructor &optional))
  (make-pattern-constructor
   :type (tc:apply-substitution subs (pattern-type pattern))
   :name (pattern-constructor-name pattern)
   :patterns (tc:apply-substitution subs (pattern-constructor-patterns pattern))))

(defmethod tc:type-variables ((pattern pattern-var))
  (tc:type-variables (pattern-type pattern)))

(defmethod tc:type-variables ((pattern pattern-binding))
  (remove-duplicates (append (tc:type-variables (pattern-binding-var pattern))
                             (tc:type-variables (pattern-binding-pattern pattern)))))

(defmethod tc:type-variables ((pattern pattern-literal))
  (tc:type-variables (pattern-type pattern)))

(defmethod tc:type-variables ((pattern pattern-wildcard))
  (tc:type-variables (pattern-type pattern)))

(defmethod tc:type-variables ((pattern pattern-constructor))
  (remove-duplicates (alexandria:mappend #'tc:type-variables
                                         (cons (pattern-type pattern)
                                               (pattern-constructor-patterns pattern)))))

(defun codegen-pattern->typechecker-pattern (pattern)
  "Convert codegen PATTERN into a typechecker pattern."
  (declare (type pattern pattern)
           (values tc:pattern &optional))
  (labels ((qualify (type)
             (declare (type tc:ty type)
                      (values tc:qualified-ty &optional))
             (tc:qualify nil type))
           (convert (pattern)
             (declare (type pattern pattern)
                      (values tc:pattern &optional))
             (etypecase pattern
               (pattern-var
                (tc:make-pattern-var
                 :type (qualify (pattern-type pattern))
                 :name (pattern-var-name pattern)
                 ;; Codegen patterns do not preserve original source names.
                 :orig-name (pattern-var-name pattern)))
               (pattern-binding
                (tc:make-pattern-binding
                 :type (qualify (pattern-type pattern))
                 :var (convert (pattern-binding-var pattern))
                 :pattern (convert (pattern-binding-pattern pattern))))
               (pattern-literal
                (tc:make-pattern-literal
                 :type (qualify (pattern-type pattern))
                 :value (pattern-literal-value pattern)))
               (pattern-wildcard
                (tc:make-pattern-wildcard
                 :type (qualify (pattern-type pattern))))
               (pattern-constructor
                (tc:make-pattern-constructor
                 :type (qualify (pattern-type pattern))
                 :name (pattern-constructor-name pattern)
                 :patterns (mapcar #'convert (pattern-constructor-patterns pattern)))))))
    (convert pattern)))

(defun patterns-exhaustive-p (patterns type env)
  "Does the LIST, PATTERNS, cover every case of TYPE?"
  (declare (type pattern-list patterns)
           (type tc:ty type)
           (type tc:environment env)
           (ignore type)
           (values boolean &optional))
  (pe:exhaustive-patterns-p
   (mapcar #'pe:collapse-binding-patterns
           (mapcar #'codegen-pattern->typechecker-pattern patterns))
   env))
