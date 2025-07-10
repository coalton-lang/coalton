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

(defmethod tc:type-variables ((pattern pattern-literal))
  (tc:type-variables (pattern-type pattern)))

(defmethod tc:type-variables ((pattern pattern-wildcard))
  (tc:type-variables (pattern-type pattern)))

(defmethod tc:type-variables ((pattern pattern-constructor))
  (remove-duplicates (alexandria:mappend #'tc:type-variables
                                         (cons (pattern-type pattern)
                                               (pattern-constructor-patterns pattern)))))

(defun constructor-grouping-exhaustive-p (constructor grouping type env)
  "Does the LIST, GROUPING, cover every case of the constructor
CONSTRUCTOR of type TYPE?"
  (let* ((generic-type (tc:qualified-ty-type (tc:fresh-inst (tc:lookup-value-type env constructor))))
         (matched-subs (tc:match (tc:function-return-type generic-type) type))
         (matched-type (tc:apply-substitution matched-subs generic-type)))
    (every (lambda (patterns type) (patterns-exhaustive-p patterns type env))
           ;; "Rotate" the list of patterns s.t. the first element of
           ;; the resulting list is a list of patterns covering the
           ;; first argument of the constructor, and the second
           ;; element is a list of patterns covering the second
           ;; argument of the constructor, etc.
           (apply #'mapcar #'list (mapcar #'pattern-constructor-patterns grouping))
           ;; Grab the types of the arguments to the constructor as a
           ;; list.
           (tc:function-type-arguments matched-type))))

(defun pattern-constructors-exhaustive-p (pattern-constructors type env)
  "Does the LIST, PATTERN-CONSTRUCTORS, cover every case of TYPE?"
  ;; When PATTERN-CONSTRUCTORS is empty, it is not exhaustive.
  (when (endp pattern-constructors)
    (return-from pattern-constructors-exhaustive-p nil))

  ;; If we got this far, then PATTERN-CONSTRUCTORS is NOT empty,
  ;; which means we do have a list of constructors, and we can
  ;; assume that TYPE is a TYCON or a TAPP.
  (let* ((type-name (tc:tycon-name (first (tc:flatten-type type))))
         (constructors (tc:type-entry-constructors (tc:lookup-type env type-name)))
         ;; Collect the patterns associated with each constructor of
         ;; type TYPE into groupings.
         (groupings (mapcar
                     (lambda (constructor)
                       (remove-if-not
                        (lambda (pattern)
                          (eq constructor (pattern-constructor-name pattern)))
                        pattern-constructors))
                     constructors)))
    (and
     ;; Is there at least one pattern for each constructor of type
     ;; TYPE?
     (every #'consp groupings)
     ;; And, if so, are the patterns for each constructor exhaustive
     ;; for the cases of that constructor?
     (every (lambda (constructor grouping)
              (constructor-grouping-exhaustive-p constructor grouping type env))
            constructors
            groupings))))

(defun patterns-exhaustive-p (patterns type env)
  "Does the LIST, PATTERNS, cover every case of TYPE?"
  (or
   ;; If any pattern is a variable or wildcard, then the entire list
   ;; is exhaustive.
   (member-if #'pattern-var-p patterns)
   (member-if #'pattern-wildcard-p patterns)
   ;; Otherwise, the list of patterns is only exhaustive if it
   ;; contains a list of constructors which is exhaustive.
   (pattern-constructors-exhaustive-p
    (remove-if-not #'pattern-constructor-p patterns)
    type
    env)))
