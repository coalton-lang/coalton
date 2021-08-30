(in-package #:coalton-impl/ast)

;;;
;;; Patterns
;;;

(serapeum:defstruct-read-only (pattern (:constructor nil)))

(defun pattern-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'pattern-p x)))

(deftype pattern-list ()
  '(satisfies pattern-list-p))

#+sbcl
(declaim (sb-ext:freeze-type pattern-list))

(serapeum:defstruct-read-only
    (pattern-var
     (:include pattern)
     (:constructor pattern-var (id)))
  (id :type symbol))

#+sbcl
(declaim (sb-ext:freeze-type pattern-var))

(serapeum:defstruct-read-only
    (pattern-wildcard
     (:include pattern)
     (:constructor pattern-wildcard)))

#+sbcl
(declaim (sb-ext:freeze-type pattern-wildcard))

(serapeum:defstruct-read-only
    (pattern-literal
     (:include pattern)
     (:constructor pattern-literal (value)))
  (value  :type node-literal))

#+sbcl
(declaim (sb-ext:freeze-type pattern-literal))

(serapeum:defstruct-read-only
    (pattern-constructor
     (:include pattern)
     (:constructor pattern-constructor (name patterns)))
  (name      :type symbol)
  (patterns  :type pattern-list))

#+sbcl
(declaim (sb-ext:freeze-type pattern-constructor))

#+sbcl
(declaim (sb-ext:freeze-type pattern))

(defun rewrite-pattern-vars (pattern sr)
  "Rewrite the variables in PATTERN according to the mapping defined in SR."
  ;; This is used when canonicalizing variable names in the parser.
  ;; It is then used to reverse canonicalization when printing error messages.
  (declare (type pattern pattern)
	   (type shadow-realm sr)
	   (values pattern))
  (etypecase pattern
    (pattern-literal pattern)

    (pattern-wildcard pattern)

    (pattern-constructor
     (pattern-constructor
      (pattern-constructor-name pattern)
      (mapcar
       (lambda (pattern)
	 (rewrite-pattern-vars pattern sr))
       (pattern-constructor-patterns pattern))))

    (pattern-var
     (pattern-var
      (or (shadow-realm-lookup sr (pattern-var-id pattern))
	  (coalton-impl::coalton-bug "Invalid state reached in rewrite-pattern-vars"))))))


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
    (pattern-var (format stream "~A" (string (pattern-var-id pattern))))
    (pattern-wildcard (format stream "-"))
    (pattern-literal (format stream "~A" (pattern-literal-value pattern)))
    (pattern-constructor (format stream "(~A~{ ~A~})"
				 (pattern-constructor-name pattern)
				 (pattern-constructor-patterns pattern))))
  pattern)

(set-pprint-dispatch 'pattern 'pprint-pattern)
