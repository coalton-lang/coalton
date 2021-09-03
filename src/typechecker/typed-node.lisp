(in-package #:coalton-impl/typechecker)

;;;
;;; Typed AST nodes
;;;

(serapeum:defstruct-read-only (typed-node (:constructor nil))
  (type :type ty-scheme)
  (unparsed :type t))

(defun typed-node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'typed-node-p x)))

(deftype typed-node-list ()
  '(satisfies typed-node-list-p))

#+sbcl
(declaim (sb-ext:freeze-type typed-node-list))

(defun typed-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol typed-node))) x)))

(deftype typed-binding-list ()
  `(satisfies typed-binding-list-p))

#+sbcl
(declaim (sb-ext:freeze-type typed-binding-list))

(serapeum:defstruct-read-only
    (typed-node-literal
     (:include typed-node)
     (:constructor typed-node-literal (type unparsed value)))
  (value :type literal-value))

#+sbcl
(declaim (sb-ext:freeze-type typed-node-literal))

(serapeum:defstruct-read-only
    (typed-node-variable
     (:include typed-node)
     (:constructor typed-node-variable (type unparsed name)))
  ;; The name of the variable
  (name :type symbol))

#+sbcl
(declaim (sb-ext:freeze-type typed-node-variable))

(serapeum:defstruct-read-only
    (typed-node-application
     (:include typed-node)
     (:constructor typed-node-application (type unparsed rator rands)))
  ;; The function
  (rator :type typed-node)

  ;; The arguments
  (rands :type typed-node-list))

#+sbcl
(declaim (sb-ext:freeze-type typed-node-application))

(serapeum:defstruct-read-only
    (typed-node-direct-application
     (:include typed-node)
     (:constructor typed-node-direct-application (type unparsed rator-type rator rands)))
  "Application where the function is know statically and all arguments are provided.

  This allows emitting a direct call instead of funcalling a function-entry."
  ;; The type of the function
  (rator-type :type ty-scheme)

  ;; The name of the function
  (rator :type symbol)

  ;; The arguments
  (rands :type typed-node-list))

#+sbcl
(declaim (sb-ext:freeze-type typed-node-direct-application))

(serapeum:defstruct-read-only
    (typed-node-abstraction
     (:include typed-node)
     (:constructor typed-node-abstraction (type unparsed vars subexpr name-map)))
  ;; The functions arguments and their types
  (vars    :type scheme-binding-list)

  ;; The body of the function
  (subexpr :type typed-node)

  ;; An alist mapping of the current paramater names
  ;; to their origional names
  (name-map :type list))

#+sbcl
(declaim (sb-ext:freeze-type typed-node-abstraction))

(serapeum:defstruct-read-only
    (typed-node-let
     (:include typed-node)
     (:constructor typed-node-let (type unparsed bindings subexpr sorted-bindings dynamic-extent-bindings name-map)))
  ;; Bindings declared in the let
  (bindings :type typed-binding-list)

  ;; The body of the let expression
  (subexpr  :type typed-node)

  ;; The bindings' SCCS
  (sorted-bindings :type list)

  ;; Bindings which can be declared dynamic-extent durring codegen
  (dynamic-extent-bindings :type symbol-list)

  ;; An alist mapping the current binding names
  ;; to their origional names
  (name-map :type list))

#+sbcl
(declaim (sb-ext:freeze-type typed-node-let))

(serapeum:defstruct-read-only
    (typed-node-lisp
     (:include typed-node)
     (:constructor typed-node-lisp (type unparsed variables form)))
  ;; Local variables used in the lisp block
  (variables :type list)

  ;; The lisp block
  (form :type t))

#+sbcl
(declaim (sb-ext:freeze-type typed-node-lisp))

(serapeum:defstruct-read-only
    (typed-match-branch
     (:constructor typed-match-branch (unparsed pattern subexpr bindings name-map)))
  (unparsed :type t)
  (pattern :type pattern)
  (subexpr :type typed-node)
  (bindings :type scheme-binding-list)
  (name-map :type list))

#+sbcl
(declaim (sb-ext:freeze-type typed-match-branch))

(defun typed-match-branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'typed-match-branch-p x)))

(deftype typed-match-branch-list ()
  '(satisfies typed-match-branch-list-p))

#+sbcl
(declaim (sb-ext:freeze-type typed-match-branch-list))

(serapeum:defstruct-read-only
    (typed-node-match
     (:include typed-node)
     (:constructor typed-node-match (type unparsed expr branches)))
  (expr     :type typed-node)
  (branches :type typed-match-branch-list))

#+sbcl
(declaim (sb-ext:freeze-type typed-node-match))

(serapeum:defstruct-read-only
    (typed-node-seq
     (:include typed-node)
     (:constructor typed-node-seq (type unparsed subnodes)))
     (subnodes :type typed-node-list))

#+sbcl
(declaim (sb-ext:freeze-type typed-node-seq))

#+sbcl
(declaim (sb-ext:freeze-type typed-node))

;;;
;;; Methods
;;;

(defmethod type-variables ((node typed-node))
  (type-variables (typed-node-type node)))

;; typed-node-seq has type type of it's last argument. As such it can contain type variables in it's subnodes that it does not include at the top level. This change is done for check-variables.
(defmethod type-variables ((node typed-node-seq))
  (remove-duplicates (mapcan #'type-variables (typed-node-seq-subnodes node)) :test #'equalp))

(defmethod apply-substitution (subs (node typed-node-literal))
  (declare (type substitution-list subs)
	   (values typed-node-literal))
  (typed-node-literal
   (apply-substitution subs (typed-node-type node))
   (typed-node-unparsed node)
   (typed-node-literal-value node)))

(defmethod apply-substitution (subs (node typed-node-variable))
  (declare (type substitution-list subs)
	   (values typed-node-variable))
  (typed-node-variable
   (apply-substitution subs (typed-node-type node))
   (typed-node-unparsed node)
   (typed-node-variable-name node)))

(defmethod apply-substitution (subs (node typed-node-application))
  (declare (type substitution-list subs)
	   (values typed-node-application))
  (typed-node-application
   (apply-substitution subs (typed-node-type node))
   (typed-node-unparsed node)
   (apply-substitution subs (typed-node-application-rator node))
   (apply-substitution subs (typed-node-application-rands node))))

(defmethod apply-substitution (subs (node typed-node-direct-application))
  (declare (type substitution-list subs)
	   (values typed-node-direct-application))
  (typed-node-direct-application
   (apply-substitution subs (typed-node-type node))
   (typed-node-unparsed node)
   (apply-substitution subs (typed-node-direct-application-rator-type node))
   (typed-node-direct-application-rator node)
   (apply-substitution subs (typed-node-direct-application-rands node))))

(defmethod apply-substitution (subs (node typed-node-abstraction))
  (declare (type substitution-list subs)
	   (values typed-node-abstraction))
  (typed-node-abstraction
   (apply-substitution subs (typed-node-type node))
   (typed-node-unparsed node)
   (mapcar
    (lambda (binding) (cons (car binding) (apply-substitution subs (cdr binding))))
    (typed-node-abstraction-vars node))
   (apply-substitution subs (typed-node-abstraction-subexpr node))
   (typed-node-abstraction-name-map node)))

(defmethod apply-substitution (subs (node typed-node-let))
  (declare (type substitution-list subs)
	   (values typed-node-let))
  (typed-node-let
   (apply-substitution subs (typed-node-type node))
   (typed-node-unparsed node)
   (mapcar
    (lambda (binding) (cons (car binding) (apply-substitution subs (cdr binding))))
    (typed-node-let-bindings node))
   (apply-substitution subs (typed-node-let-subexpr node))
   (typed-node-let-sorted-bindings node)
   (typed-node-let-dynamic-extent-bindings node)
   (typed-node-let-name-map node)))

(defmethod apply-substitution (subs (node typed-node-lisp))
  (declare (type substitution-list subs)
	   (values typed-node-lisp))
  (typed-node-lisp
   (apply-substitution subs (typed-node-type node))
   (typed-node-unparsed node)
   (typed-node-lisp-variables node)
   (typed-node-lisp-form node)))

(defmethod apply-substitution (subs (node typed-node-match))
  (declare (type substitution-list subs)
	   (values typed-node-match))
  (typed-node-match
   (apply-substitution subs (typed-node-type node))
   (typed-node-unparsed node)
   (apply-substitution subs (typed-node-match-expr node))
   (apply-substitution subs (typed-node-match-branches node))))

(defmethod apply-substitution (subs (node typed-match-branch))
  (declare (type substitution-list subs)
	   (values typed-match-branch))
  (typed-match-branch
   (typed-match-branch-unparsed node)
   (typed-match-branch-pattern node)
   (apply-substitution subs (typed-match-branch-subexpr node))
   (mapcar (lambda (b)
             (cons (car b)
                   (apply-substitution subs (cdr b))))
           (typed-match-branch-bindings node))
   (typed-match-branch-name-map node)))

(defmethod apply-substitution (subs (node typed-node-seq))
  (declare (type substitution-list subs)
	   (values typed-node-seq))
  (typed-node-seq
   (apply-substitution subs (typed-node-type node))
   (typed-node-unparsed node)
   (apply-substitution subs (typed-node-seq-subnodes node))))

(defgeneric replace-node-type (node new-type)
  (:method ((node typed-node-literal) new-type)
    (typed-node-literal
     new-type
     (typed-node-unparsed node)
     (typed-node-literal-value node)))

  (:method ((node typed-node-variable) new-type)
    (typed-node-variable
     new-type
     (typed-node-unparsed node)
     (typed-node-variable-name node)))

  (:method ((node typed-node-abstraction) new-type)
    (typed-node-abstraction
     new-type
     (typed-node-unparsed node)
     (typed-node-abstraction-vars node)
     (typed-node-abstraction-subexpr node)
     (typed-node-abstraction-name-map node)))

  (:method ((node typed-node-lisp) new-type)
    (typed-node-lisp
     new-type
     (typed-node-unparsed node)
     (typed-node-lisp-variables node)
     (typed-node-lisp-form node)))

  (:method ((node typed-node-application) new-type)
    (typed-node-application
     new-type
     (typed-node-unparsed node)
     (typed-node-application-rator node)
     (typed-node-application-rands node)))

  (:method ((node typed-node-direct-application) new-type)
    (typed-node-direct-application
     new-type
     (typed-node-unparsed node)
     (typed-node-direct-application-rator-type node)
     (typed-node-direct-application-rator node)
     (typed-node-direct-application-rands node)))

  (:method ((node typed-node-let) new-type)
    (typed-node-let
     new-type
     (typed-node-unparsed node)
     (typed-node-let-bindings node)
     (typed-node-let-subexpr node)
     (typed-node-let-sorted-bindings node)
     (typed-node-let-dynamic-extent-bindings node)
     (typed-node-let-name-map node)))

  (:method ((node typed-node-match) new-type)
    (typed-node-match
     new-type
     (typed-node-unparsed node)
     (typed-node-match-expr node)
     (typed-node-match-branches node))))

(defgeneric collect-type-predicates (node)
  (:method ((type qualified-ty))
    (qualified-ty-predicates type))
  
  (:method ((type ty-scheme))
    (collect-type-predicates (fresh-inst type)))


  (:method ((node typed-node-literal))
    (collect-type-predicates (typed-node-type node)))
  
  (:method ((node typed-node-variable))
    (collect-type-predicates (typed-node-type node)))

  (:method ((node typed-node-lisp))
    (collect-type-predicates (typed-node-type node)))
  
  (:method ((node typed-node-application))
    (remove-duplicates
     (append (collect-type-predicates (typed-node-type node))
             (collect-type-predicates (typed-node-application-rator node))
             (mapcan #'collect-type-predicates (typed-node-application-rands node)))
     :test #'equalp))

  (:method ((node typed-node-direct-application))
    (remove-duplicates
     (append (collect-type-predicates (typed-node-type node))
             (collect-type-predicates (typed-node-direct-application-rator-type node))
             (mapcan #'collect-type-predicates (typed-node-direct-application-rands node)))
     :test #'equalp))

  (:method ((node typed-node-abstraction))
    (remove-duplicates
     (append (collect-type-predicates (typed-node-type node))
             (collect-type-predicates (typed-node-abstraction-subexpr node)))
     :test #'equalp))

  (:method ((node typed-node-lisp))
    (collect-type-predicates (typed-node-type node)))

  (:method ((node typed-node-let))
    (remove-duplicates
     (append (collect-type-predicates (typed-node-type node))
             (collect-type-predicates (typed-node-let-subexpr node))
             (mapcan #'collect-type-predicates (mapcar #'cdr (typed-node-let-bindings node))))
     :test #'equalp))

  (:method ((node typed-node-match))
    (remove-duplicates
     (append
      (collect-type-predicates (typed-node-type node))
      (collect-type-predicates (typed-node-match-expr node))
      (mapcan #'collect-type-predicates (typed-node-match-branches node)))))

  (:method ((node typed-match-branch))
    (remove-duplicates
     (append (collect-type-predicates (typed-match-branch-subexpr node))
             (mapcan #'collect-type-predicates (mapcar #'cdr (typed-match-branch-bindings node))))
     :test #'equalp))

  (:method ((node typed-node-seq))
    (remove-duplicates
     (mapcan #'collect-type-predicates (typed-node-seq-subnodes node))
     :test #'equalp)))

(defun collect-variable-namespace (node)
  "Returns the name of every variable that will be referenced in the variable namespace in the generated code."
  (declare (type typed-node node)
	   (values symbol-list))
  (remove-duplicates (collect-variable-namespace-g node) :test #'equalp))

(defgeneric collect-variable-namespace-g (node)
  (:method ((node typed-node-literal))
    nil)

  (:method ((node typed-node-variable))
    (list (typed-node-variable-name node)))

  (:method ((node typed-node-application))
    (append
     (collect-variable-namespace-g (typed-node-application-rator node))
     (mapcan #'collect-variable-namespace-g (typed-node-application-rands node))))

  (:method ((node typed-node-direct-application))
    (mapcan #'collect-variable-namespace-g (typed-node-direct-application-rands node)))

  (:method ((node typed-node-abstraction))
    (collect-variable-namespace-g (typed-node-abstraction-subexpr node)))

  (:method ((node typed-node-let))
    (append
     (loop :for (name . node) :in (typed-node-let-bindings node)
	   :append (collect-variable-namespace-g node))
     (collect-variable-namespace-g (typed-node-let-subexpr node))))

  (:method ((node typed-node-lisp))
    nil)

  (:method ((node typed-node-match))
    (append
     (collect-variable-namespace-g (typed-node-match-expr node))
     (mapcan #'collect-variable-namespace-g (typed-node-match-branches node))))

  (:method ((branch typed-match-branch))
    (collect-variable-namespace-g (typed-match-branch-subexpr branch)))

  (:method ((node typed-node-seq))
    (mapcan #'collect-variable-namespace-g (typed-node-seq-subnodes node))))
