(in-package #:coalton-impl/typechecker)

;;;
;;; Typed AST nodes
;;;

(defstruct (typed-node (:constructor nil))
  (type     (required 'type)     :type ty-scheme :read-only t)
  (unparsed (required 'unparsed) :type t         :read-only t))

(defun typed-node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'typed-node-p x)))

(deftype typed-node-list ()
  '(satisfies typed-node-list-p))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-list))

(defun typed-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol typed-node))) x)))

(deftype typed-binding-list ()
  `(satisfies typed-binding-list-p))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-binding-list))

(defstruct
    (typed-node-literal
     (:include typed-node)
     (:constructor typed-node-literal (type unparsed value)))
  (value (required 'value) :type literal-value :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-literal))

(defstruct
    (typed-node-variable
     (:include typed-node)
     (:constructor typed-node-variable (type unparsed name)))
  ;; The name of the variable
  (name (required 'name) :type symbol :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-variable))

(defstruct
    (typed-node-application
     (:include typed-node)
     (:constructor typed-node-application (type unparsed rator rands)))
  ;; The function
  (rator (required 'rator) :type typed-node :read-only t)

  ;; The arguments
  (rands (required 'rands) :type typed-node-list :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-application))

(defstruct
    (typed-node-abstraction
     (:include typed-node)
     (:constructor typed-node-abstraction (type unparsed vars subexpr name-map)))
  ;; The functions arguments and their types
  (vars (required 'vars)         :type scheme-binding-list :read-only t)

  ;; The body of the function
  (subexpr (required 'subexpr)   :type typed-node          :read-only t)

  ;; An alist mapping of the current paramater names
  ;; to their origional names
  (name-map (required 'name-map) :type list                :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-abstraction))

(defstruct
    (typed-node-let
     (:include typed-node)
     (:constructor typed-node-let (type unparsed bindings subexpr  name-map)))
  ;; Bindings declared in the let
  (bindings (required 'bindings) :type typed-binding-list :read-only t)

  ;; The body of the let expression
  (subexpr  (required 'subexpr)  :type typed-node         :read-only t)

  ;; An alist mapping the current binding names
  ;; to their origional names
  (name-map (required 'name-map) :type list               :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-let))

(defstruct
    (typed-node-lisp
     (:include typed-node)
     (:constructor typed-node-lisp (type unparsed variables form)))
  ;; Local variables used in the lisp block
  (variables (required 'variables) :type list :read-only t)

  ;; The lisp block
  (form      (required 'form)      :type t :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-lisp))

(defstruct
    (typed-match-branch
     (:constructor typed-match-branch (unparsed pattern subexpr bindings name-map)))
  (unparsed (required 'unparsed) :type t                   :read-only t)
  (pattern  (required 'pattern)  :type pattern             :read-only t)
  (subexpr  (required 'subexpr)  :type typed-node          :read-only t)
  (bindings (required 'bindings) :type scheme-binding-list :read-only t)
  (name-map (required 'name-map) :type list                :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-match-branch))

(defun typed-match-branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'typed-match-branch-p x)))

(deftype typed-match-branch-list ()
  '(satisfies typed-match-branch-list-p))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-match-branch-list))

(defstruct
    (typed-node-match
     (:include typed-node)
     (:constructor typed-node-match (type unparsed expr branches)))
  (expr     (required 'expr)     :type typed-node              :read-only t)
  (branches (required 'branches) :type typed-match-branch-list :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-match))

(defstruct
    (typed-node-seq
     (:include typed-node)
     (:constructor typed-node-seq (type unparsed subnodes)))
  (subnodes (required 'subnodes) :type typed-node-list :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-seq))

(defstruct
    (typed-node-return
     (:include typed-node)
     (:constructor typed-node-return (type unparsed expr)))
  (expr (required 'expr) :type typed-node :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-return))

#+(and sbcl coalton-release)
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

(defmethod apply-substitution (subs (node typed-node-return))
  (declare (type substitution-list subs)
           (values typed-node-return))
  (typed-node-return
   (apply-substitution subs (typed-node-type node))
   (typed-node-unparsed node)
   (apply-substitution subs (typed-node-return-expr node))))

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

  (:method ((node typed-node-let) new-type)
    (typed-node-let
     new-type
     (typed-node-unparsed node)
     (typed-node-let-bindings node)
     (typed-node-let-subexpr node)
     (typed-node-let-name-map node)))

  (:method ((node typed-node-match) new-type)
    (typed-node-match
     new-type
     (typed-node-unparsed node)
     (typed-node-match-expr node)
     (typed-node-match-branches node)))

  (:method ((node typed-node-seq) new-type)
    (typed-node-seq
     new-type
     (typed-node-unparsed node)
     (typed-node-seq-subnodes node)))

  (:method ((node typed-node-return) new-type)
    (typed-node-return
     new-type
     (typed-node-unparsed node)
     (typed-node-return-expr node))))

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
     :test #'equalp))

  (:method ((node typed-node-return))
    (remove-duplicates
     (append
      (collect-type-predicates (typed-node-type node))
      (collect-type-predicates (typed-node-return-expr node)))
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

  (:method ((node typed-node-abstraction))
    (collect-variable-namespace-g (typed-node-abstraction-subexpr node)))

  (:method ((node typed-node-let))
    (append
     (loop :for (name . node) :in (typed-node-let-bindings node)
           :append (collect-variable-namespace-g node))
     (collect-variable-namespace-g (typed-node-let-subexpr node))))

  (:method ((node typed-node-lisp))
    (mapcar #'cdr (typed-node-lisp-variables node)))

  (:method ((node typed-node-match))
    (append
     (collect-variable-namespace-g (typed-node-match-expr node))
     (mapcan #'collect-variable-namespace-g (typed-node-match-branches node))))

  (:method ((branch typed-match-branch))
    (collect-variable-namespace-g (typed-match-branch-subexpr branch)))

  (:method ((node typed-node-seq))
    (mapcan #'collect-variable-namespace-g (typed-node-seq-subnodes node)))

  (:method ((node typed-node-return))
    (collect-variable-namespace-g (typed-node-return-expr node))))

(defgeneric remove-static-preds (node)
  (:method ((node typed-node-literal))
    node)

  (:method ((node typed-node-variable))
    node)

  (:method ((node typed-node-application))
    (typed-node-application
     (typed-node-type node)
     (typed-node-unparsed node)
     (remove-static-preds (typed-node-application-rator node))
     (mapcar #'remove-static-preds (typed-node-application-rands node))))

  (:method ((node typed-node-abstraction))
    (let* ((qual-ty (ty-scheme-type (typed-node-type node)))
           (preds (remove-if #'static-predicate-p (qualified-ty-predicates qual-ty)))
           (scheme (quantify nil (qualify preds (qualified-ty-type qual-ty)))))
      (typed-node-abstraction
       scheme
       (typed-node-unparsed node)
       (typed-node-abstraction-vars node)
       (remove-static-preds (typed-node-abstraction-subexpr node))
       (typed-node-abstraction-name-map node))))

  (:method ((node typed-node-let))
    (typed-node-let
     (typed-node-type node)
     (typed-node-unparsed node)
     (loop :for (name . node) :in (typed-node-let-bindings node)
           :collect (cons name (remove-static-preds node)))
     (remove-static-preds (typed-node-let-subexpr node))
     (typed-node-let-name-map node)))

  (:method ((node typed-node-lisp))
    node)

  (:method ((node typed-match-branch))
    (typed-match-branch
     (typed-match-branch-unparsed node)
     (typed-match-branch-pattern node)
     (remove-static-preds (typed-match-branch-subexpr node))
     (typed-match-branch-bindings node)
     (typed-match-branch-name-map node)))


  (:method ((node typed-node-match))
    (typed-node-match
     (typed-node-type node)
     (typed-node-unparsed node)
     (remove-static-preds (typed-node-match-expr node))
     (mapcar #'remove-static-preds (typed-node-match-branches node))))

  (:method ((node typed-node-seq))
    (typed-node-seq
     (typed-node-type node)
     (typed-node-unparsed node)
     (mapcar #'remove-static-preds (typed-node-seq-subnodes node))))

  (:method ((node typed-node-return))
    (typed-node-return
     (typed-node-type node)
     (typed-node-unparsed node)
     (remove-static-preds (typed-node-return-expr node)))))

