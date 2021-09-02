(in-package #:coalton-impl/ast)

(serapeum:defstruct-read-only
    (node
     (:constructor nil))
  (unparsed :type t))

(defun node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-p x)))

(deftype node-list ()
  '(satisfies node-list-p))

#+sbcl
(declaim (sb-ext:freeze-type node-list))

(defun binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol node))) x)))

(deftype binding-list ()
  `(satisfies binding-list-p))

#+sbcl
(declaim (sb-ext:freeze-type binding-list))

(defun symbol-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'symbolp x)))

(deftype symbol-list ()
  '(satisfies symbol-list-p))

#+sbcl
(declaim (sb-ext:freeze-type symbol-list))


;;;;;;;;;;;;;;;;;;;;;;;;; The types of nodes ;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype literal-value ()
  "Allowed literal values as Lisp objects."
  '(or integer single-float double-float string character))

#+sbcl
(declaim (sb-ext:freeze-type literal-value))

(serapeum:defstruct-read-only
    (node-literal
     (:include node)
     (:constructor node-literal (unparsed value)))
  "A literal value. These include things like integers and strings."
  (value :type literal-value))

#+sbcl
(declaim (sb-ext:freeze-type node-literal))

(serapeum:defstruct-read-only
    (node-variable
     (:include node)
     (:constructor node-variable (unparsed name)))
  (name :type symbol))

#+sbcl
(declaim (sb-ext:freeze-type node-variable))

(serapeum:defstruct-read-only
    (node-application
     (:include node)
     (:constructor node-application (unparsed rator rands)))
  (rator :type node)
  (rands :type node-list))

#+sbcl
(declaim (sb-ext:freeze-type node-application))

(serapeum:defstruct-read-only
    (node-abstraction
     (:include node)
     (:constructor node-abstraction (unparsed vars subexpr name-map)))
  (vars    :type symbol-list)
  (subexpr :type node)
  (name-map :type list))

#+sbcl
(declaim (sb-ext:freeze-type node-abstraction))

(serapeum:defstruct-read-only
    (node-let
     (:include node)
     (:constructor node-let (unparsed bindings subexpr name-map)))
  (bindings :type binding-list)
  (subexpr  :type node)
  (name-map :type list))

#+sbcl
(declaim (sb-ext:freeze-type node-let))

(serapeum:defstruct-read-only
    (node-lisp
     (:include node)
     (:constructor node-lisp (unparsed type variables form)))
  (type :type t)
  (variables :type list)
  (form :type t))

#+sbcl
(declaim (sb-ext:freeze-type node-lisp))

(serapeum:defstruct-read-only match-branch
  (unparsed :type t)
  (pattern :type pattern)
  (subexpr :type node)
  (name-map :type list))

#+sbcl
(declaim (sb-ext:freeze-type match-branch))

(serapeum:defstruct-read-only
    (node-match
     (:include node)
     (:constructor node-match (unparsed expr branches)))
  (expr     :type node)
  (branches :type list))

#+sbcl
(declaim (sb-ext:freeze-type node-match))

(serapeum:defstruct-read-only
    (node-seq
     (:include node)
     (:constructor node-seq (unparsed subnodes)))
     (subnodes :type node-list))

#+sbcl
(declaim (sb-ext:freeze-type node-seq))

#+sbcl
(declaim (sb-ext:freeze-type node))
