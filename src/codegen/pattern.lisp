(defpackage #:coalton-impl/codegen/pattern
  (:use
   #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
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
   #:pattern-variables                  ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/pattern)

(defstruct (pattern
            (:constructor nil)
            (:copier nil))
  (type (util:required 'type) :type tc:ty :read-only t))

(defun pattern-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'pattern-p x)))

(deftype pattern-list ()
  '(satisfies pattern-list-p))

(defstruct (pattern-var
            (:include pattern)
            (:copier nil))
  (name (util:required 'name) :type symbol :read-only t))

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
