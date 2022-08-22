(in-package #:coalton-impl/ast)

(defstruct (node (:constructor nil))
  (unparsed (required 'unparsed) :type t :read-only t))

(defun node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-p x)))

(deftype node-list ()
  '(satisfies node-list-p))

(defun binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol node))) x)))

(deftype binding-list ()
  `(satisfies binding-list-p))

(defstruct (node-literal (:include node))
  "A literal value. These include things like integers and strings."
  (value (required 'value) :type literal-value :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-literal))

(defstruct (node-variable (:include node))
  (name (required 'name) :type symbol :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-variable))

(defstruct (node-application (:include node))
  (rator (required 'rator) :type node      :read-only t)
  (rands (required 'rands) :type node-list :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-application))

(defstruct (node-abstraction (:include node))
  (vars     (required 'vars)     :type t           :read-only t)
  (subexpr  (required 'subexpr)  :type node        :read-only t)
  (name-map (required 'name-map) :type list        :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-abstraction))

(defstruct (node-let (:include node))
  (bindings       (required 'bindings)      :type binding-list :read-only t)
  (declared-types (required 'declare-types) :type list         :read-only t)
  (subexpr        (required 'subexpr)       :type node         :read-only t)
  (name-map       (required 'name-map)      :type list         :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-let))

(defstruct (node-lisp (:include node))
  (type      (required 'type)      :type t :read-only t)
  (variables (required 'variables) :type t :read-only t)
  (form      (required 'form)      :type t :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-lisp))

(defstruct match-branch
  (unparsed  (required 'unparsed) :type t       :read-only t)
  (pattern   (required 'pattern)  :type pattern :read-only t)
  (subexpr   (required 'subexpr)  :type node    :read-only t)
  (name-map  (required 'name-map) :type list    :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type match-branch))

(defstruct (node-match (:include node))
  (expr     (required 'expr)     :type node :read-only t)
  (branches (required 'branches) :type list :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-match))

(defstruct (node-seq (:include node))
  (subnodes (required 'subnodes) :type node-list :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-seq))

(defstruct (node-the (:include node))
  (type    (required 'type)       :type t    :read-only t)
  (subnode (required 'subnode)    :type node :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-the))

(defstruct (node-return (:include node))
  (expr (required 'expr) :type node :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-return))

(defstruct (node-bind (:include node))
  (name (required 'name) :type symbol :read-only t)
  (expr (required 'expr) :type node   :read-only t)
  (body (required 'body) :type node   :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-bind))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node))
