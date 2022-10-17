(defpackage #:coalton-impl/ast/pattern
  (:use
   #:cl
   #:coalton-impl/algorithm)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:pattern                            ; STRUCT
   #:pattern-list                       ; TYPE
   #:pattern-var                        ; STRUCT
   #:make-pattern-var                   ; CONSTRUCTOR
   #:pattern-var-id                     ; ACCESSOR
   #:pattern-var-p                      ; FUNCTION
   #:pattern-wildcard                   ; STRUCT
   #:make-pattern-wildcard              ; CONSTRUCTOR
   #:pattern-wildcard-p                 ; FUNCTION
   #:pattern-literal                    ; STRUCT
   #:make-pattern-literal               ; CONSTRUCTOR
   #:pattern-literal-value              ; ACCESSOR
   #:pattern-literal-p                  ; FUNCTION
   #:pattern-constructor                ; STRUCT
   #:make-pattern-constructor           ; CONSTRUCTOR
   #:pattern-constructor-name           ; ACCESSOR
   #:pattern-constructor-patterns       ; ACCESSOR
   #:pattern-constructor-p              ; FUNCTION
   #:rewrite-pattern-vars               ; FUNCTION
   #:pattern-variables                  ; FUNCTION
   ))

(in-package #:coalton-impl/ast/pattern)

;;;
;;; Patterns
;;;

(defstruct (pattern (:constructor nil)))

(defun pattern-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'pattern-p x)))

(deftype pattern-list ()
  '(satisfies pattern-list-p))

(defstruct (pattern-var (:include pattern))
  (id (util:required 'id) :type symbol :read-only t))

(defmethod make-load-form ((self pattern-var) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type pattern-var))

(defstruct (pattern-wildcard (:include pattern)))

(defmethod make-load-form ((self pattern-wildcard) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type pattern-wildcard))

(defstruct (pattern-literal (:include pattern))
  (value (util:required 'value) :type util:literal-value :read-only t))

(defmethod make-load-form ((self pattern-literal) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type pattern-literal))

(defstruct (pattern-constructor (:include pattern))
  (name      (util:required 'name) :type symbol       :read-only t)
  (patterns  (util:required 'type) :type pattern-list :read-only t))

(defmethod make-load-form ((self pattern-constructor) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type pattern-constructor))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type pattern))

(defun rewrite-pattern-vars (pattern m)
  "Rewrite the variables in PATTERN according to the mapping defined in M."
  ;; This is used when canonicalizing variable names in the parser.
  ;; It is then used to reverse canonicalization when printing error messages.
  (declare (type pattern pattern)
           (type immutable-map m)
           (values pattern))
  (etypecase pattern
    (pattern-literal pattern)

    (pattern-wildcard pattern)

    (pattern-constructor
     (make-pattern-constructor
      :name (pattern-constructor-name pattern)
      :patterns (mapcar
                 (lambda (pattern)
                   (rewrite-pattern-vars pattern m))
                 (pattern-constructor-patterns pattern))))

    (pattern-var
     (multiple-value-bind (id found-p)
         (immutable-map-lookup m (pattern-var-id pattern))
       (unless found-p
         (util:coalton-bug "Invalid state reached in rewrite-pattern-vars"))

       (make-pattern-var :id id)))))


(defun pattern-variables (pattern)
  (declare (type pattern pattern))
  "Symbols of all variables bound in PATTERN"
  (etypecase pattern
    (pattern-literal nil)
    (pattern-wildcard nil)
    (pattern-var (list (pattern-var-id pattern)))
    (pattern-constructor (mapcan #'pattern-variables
                                 (pattern-constructor-patterns pattern)))))

(defun pprint-pattern (stream pattern &optional colon-p at-sign-p)
  (declare (type stream stream)
           (type pattern pattern)
           (ignore colon-p at-sign-p)
           (values pattern))
  (etypecase pattern
    (pattern-var (write (pattern-var-id pattern) :stream stream))
    (pattern-wildcard (write-char #\_ stream))
    (pattern-literal (write (pattern-literal-value pattern) :stream stream))
    (pattern-constructor
     (write-char #\( stream)
     (write (pattern-constructor-name pattern) :stream stream)
     (loop :for pat :in (pattern-constructor-patterns pattern)
           :do (write-char #\space stream)
               (write pat :stream stream))
     (write-char #\) stream)))
  pattern)

(set-pprint-dispatch 'pattern 'pprint-pattern)
