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

(defun typed-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol typed-node))) x)))

(deftype typed-binding-list ()
  `(satisfies typed-binding-list-p))

(defstruct (typed-node-literal (:include typed-node))
  (value (required 'value) :type literal-value :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-literal))

(defstruct (typed-node-variable (:include typed-node))
  ;; The name of the variable
  (name (required 'name) :type symbol :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-variable))

(defstruct (typed-node-application (:include typed-node))
  ;; The function
  (rator (required 'rator) :type typed-node :read-only t)

  ;; The arguments
  (rands (required 'rands) :type typed-node-list :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-application))

(defstruct (typed-node-abstraction (:include typed-node))
  ;; The functions arguments and their types
  (vars (required 'vars)         :type scheme-binding-list :read-only t)

  ;; The body of the function
  (subexpr (required 'subexpr)   :type typed-node          :read-only t)

  ;; An alist mapping of the current paramater names
  ;; to their origional names
  (name-map (required 'name-map) :type list                :read-only t))

(defun typed-node-abstraction-source-parameter-names (node)
  (declare (type typed-node-abstraction node)
           (values symbol-list &optional))
  (mapcar #'cdr (typed-node-abstraction-name-map node)))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-abstraction))

(defstruct (typed-node-let (:include typed-node))
  ;; Bindings declared in the let
  (bindings (required 'bindings) :type typed-binding-list :read-only t)

  ;; The body of the let expression
  (subexpr  (required 'subexpr)  :type typed-node         :read-only t)

  ;; Mapping from binding name to declared explicit types (if applicable)
  (explicit-types (required 'explicit-types) :type hash-table :read-only t)

  ;; An alist mapping the current binding names
  ;; to their origional names
  (name-map (required 'name-map) :type list               :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-let))

(defstruct (typed-node-lisp (:include typed-node))
  ;; Local variables used in the lisp block
  (variables (required 'variables) :type list :read-only t)

  ;; The lisp block
  (form      (required 'form)      :type t :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-lisp))

(defstruct typed-match-branch
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

(defstruct (typed-node-match (:include typed-node))
  (expr     (required 'expr)     :type typed-node              :read-only t)
  (branches (required 'branches) :type typed-match-branch-list :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-match))

(defstruct (typed-node-seq (:include typed-node))
  (subnodes (required 'subnodes) :type typed-node-list :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-seq))

(defstruct (typed-node-return (:include typed-node))
  (expr (required 'expr) :type typed-node :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-return))

(defstruct (typed-node-bind (:include typed-node))
  (name (required 'name) :type symbol     :read-only t)
  (expr (required 'expr) :type typed-node :read-only t)
  (body (required 'body) :type typed-node :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node-bind))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type typed-node))

;;;
;;; Methods
;;;

(defmethod apply-substitution (subs (node typed-node-literal))
  (declare (type substitution-list subs)
           (values typed-node-literal &optional))
  (make-typed-node-literal
   :type (apply-substitution subs (typed-node-type node))
   :unparsed (typed-node-unparsed node)
   :value (typed-node-literal-value node)))

(defmethod apply-substitution (subs (node typed-node-variable))
  (declare (type substitution-list subs)
           (values typed-node-variable &optional))
  (make-typed-node-variable
   :type (apply-substitution subs (typed-node-type node))
   :unparsed (typed-node-unparsed node)
   :name (typed-node-variable-name node)))

(defmethod apply-substitution (subs (node typed-node-application))
  (declare (type substitution-list subs)
           (values typed-node-application &optional))
  (make-typed-node-application
   :type (apply-substitution subs (typed-node-type node))
   :unparsed (typed-node-unparsed node)
   :rator (apply-substitution subs (typed-node-application-rator node))
   :rands (apply-substitution subs (typed-node-application-rands node))))

(defmethod apply-substitution (subs (node typed-node-abstraction))
  (declare (type substitution-list subs)
           (values typed-node-abstraction &optional))
  (make-typed-node-abstraction
   :type (apply-substitution subs (typed-node-type node))
   :unparsed (typed-node-unparsed node)
   :vars  (mapcar
           (lambda (binding) (cons (car binding) (apply-substitution subs (cdr binding))))
           (typed-node-abstraction-vars node))
   :subexpr (apply-substitution subs (typed-node-abstraction-subexpr node))
   :name-map (typed-node-abstraction-name-map node)))

(defmethod apply-substitution (subs (node typed-node-let))
  (declare (type substitution-list subs)
           (values typed-node-let &optional))
  (make-typed-node-let
   :type (apply-substitution subs (typed-node-type node))
   :unparsed (typed-node-unparsed node)
   :bindings (mapcar
              (lambda (binding) (cons (car binding) (apply-substitution subs (cdr binding))))
              (typed-node-let-bindings node))
   :subexpr (apply-substitution subs (typed-node-let-subexpr node))
   :explicit-types (maphash-values-new
                    (lambda (type)
                      (apply-substitution subs type))
                    (typed-node-let-explicit-types node))
   :name-map (typed-node-let-name-map node)))

(defmethod apply-substitution (subs (node typed-node-lisp))
  (declare (type substitution-list subs)
           (values typed-node-lisp &optional))
  (make-typed-node-lisp
   :type (apply-substitution subs (typed-node-type node))
   :unparsed (typed-node-unparsed node)
   :variables (typed-node-lisp-variables node)
   :form (typed-node-lisp-form node)))

(defmethod apply-substitution (subs (node typed-node-match))
  (declare (type substitution-list subs)
           (values typed-node-match &optional))
  (make-typed-node-match
   :type (apply-substitution subs (typed-node-type node))
   :unparsed (typed-node-unparsed node)
   :expr (apply-substitution subs (typed-node-match-expr node))
   :branches (apply-substitution subs (typed-node-match-branches node))))

(defmethod apply-substitution (subs (node typed-match-branch))
  (declare (type substitution-list subs)
           (values typed-match-branch &optional))
  (make-typed-match-branch
   :unparsed (typed-match-branch-unparsed node)
   :pattern (typed-match-branch-pattern node)
   :subexpr (apply-substitution subs (typed-match-branch-subexpr node))
   :bindings (mapcar (lambda (b)
                       (cons (car b)
                             (apply-substitution subs (cdr b))))
                     (typed-match-branch-bindings node))
   :name-map (typed-match-branch-name-map node)))

(defmethod apply-substitution (subs (node typed-node-seq))
  (declare (type substitution-list subs)
           (values typed-node-seq &optional))
  (make-typed-node-seq
   :type (apply-substitution subs (typed-node-type node))
   :unparsed (typed-node-unparsed node)
   :subnodes (apply-substitution subs (typed-node-seq-subnodes node))))

(defmethod apply-substitution (subs (node typed-node-return))
  (declare (type substitution-list subs)
           (values typed-node-return &optional))
  (make-typed-node-return
   :type (apply-substitution subs (typed-node-type node))
   :unparsed (typed-node-unparsed node)
   :expr (apply-substitution subs (typed-node-return-expr node))))

(defmethod apply-substitution (subs (node typed-node-bind))
  (declare (type substitution-list subs)
           (values typed-node-bind &optional))
  (make-typed-node-bind
   :type (apply-substitution subs (typed-node-type node))
   :unparsed (typed-node-unparsed node)
   :name (typed-node-bind-name node)
   :expr (apply-substitution subs (typed-node-bind-expr node))
   :body (apply-substitution subs (typed-node-bind-body node))))

(defgeneric replace-node-type (node new-type)
  (:method ((node typed-node-literal) new-type)
    (make-typed-node-literal
     :type new-type
     :unparsed (typed-node-unparsed node)
     :value (typed-node-literal-value node)))

  (:method ((node typed-node-variable) new-type)
    (make-typed-node-variable
     :type new-type
     :unparsed (typed-node-unparsed node)
     :name (typed-node-variable-name node)))

  (:method ((node typed-node-abstraction) new-type)
    (make-typed-node-abstraction
     :type new-type
     :unparsed (typed-node-unparsed node)
     :vars (typed-node-abstraction-vars node)
     :subexpr (typed-node-abstraction-subexpr node)
     :name-map (typed-node-abstraction-name-map node)))

  (:method ((node typed-node-lisp) new-type)
    (make-typed-node-lisp
     :type new-type
     :unparsed (typed-node-unparsed node)
     :variables (typed-node-lisp-variables node)
     :form (typed-node-lisp-form node)))

  (:method ((node typed-node-application) new-type)
    (make-typed-node-application
     :type new-type
     :unparsed (typed-node-unparsed node)
     :rator (typed-node-application-rator node)
     :rands (typed-node-application-rands node)))

  (:method ((node typed-node-let) new-type)
    (make-typed-node-let
     :type new-type
     :unparsed (typed-node-unparsed node)
     :bindings (typed-node-let-bindings node)
     :subexpr (typed-node-let-subexpr node)
     :explicit-types (typed-node-let-explicit-types node)
     :name-map (typed-node-let-name-map node)))

  (:method ((node typed-node-match) new-type)
    (make-typed-node-match
     :type new-type
     :unparsed (typed-node-unparsed node)
     :expr (typed-node-match-expr node)
     :branches (typed-node-match-branches node)))

  (:method ((node typed-node-seq) new-type)
    (make-typed-node-seq
     :type new-type
     :unparsed (typed-node-unparsed node)
     :subnodes (typed-node-seq-subnodes node)))

  (:method ((node typed-node-return) new-type)
    (make-typed-node-return
     :type new-type
     :unparsed (typed-node-unparsed node)
     :expr (typed-node-return-expr node)))

  (:method ((node typed-node-bind) new-type)
    (make-typed-node-bind
     :type new-type
     :unparsed (typed-node-unparsed node)
     :name (typed-node-bind-name node)
     :expr (typed-node-bind-expr node)
     :body (typed-node-bind-body node))))

(defun collect-variable-namespace (node)
  "Returns the name of every variable that will be referenced in the variable namespace in the generated code."
  (declare (type typed-node node)
           (values symbol-list &optional))
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
    (collect-variable-namespace-g (typed-node-return-expr node)))

  (:method ((node typed-node-bind))
    (append
     (collect-variable-namespace-g (typed-node-bind-expr node))
     (collect-variable-namespace-g (typed-node-bind-body node)))))

(defgeneric remove-static-preds (node)
  (:method ((node typed-node-literal))
    node)

  (:method ((node typed-node-variable))
    node)

  (:method ((node typed-node-application))
    (make-typed-node-application
     :type (typed-node-type node)
     :unparsed (typed-node-unparsed node)
     :rator (remove-static-preds (typed-node-application-rator node))
     :rands (mapcar #'remove-static-preds (typed-node-application-rands node))))

  (:method ((node typed-node-abstraction))
    (let* ((qual-ty (ty-scheme-type (typed-node-type node)))
           (preds (remove-if #'static-predicate-p (qualified-ty-predicates qual-ty)))
           (scheme (quantify nil (qualify preds (qualified-ty-type qual-ty)))))
      (make-typed-node-abstraction
       :type scheme
       :unparsed (typed-node-unparsed node)
       :vars (typed-node-abstraction-vars node)
       :subexpr (remove-static-preds (typed-node-abstraction-subexpr node))
       :name-map (typed-node-abstraction-name-map node))))

  (:method ((node typed-node-let))
    (make-typed-node-let
     :type (typed-node-type node)
     :unparsed (typed-node-unparsed node)
     :bindings (loop :for (name . node) :in (typed-node-let-bindings node)
                     :collect (cons name (remove-static-preds node)))
     :subexpr (remove-static-preds (typed-node-let-subexpr node))
     :explicit-types (typed-node-let-explicit-types node)
     :name-map (typed-node-let-name-map node)))

  (:method ((node typed-node-lisp))
    node)

  (:method ((node typed-match-branch))
    (make-typed-match-branch
     :unparsed (typed-match-branch-unparsed node)
     :pattern (typed-match-branch-pattern node)
     :subexpr (remove-static-preds (typed-match-branch-subexpr node))
     :bindings (typed-match-branch-bindings node)
     :name-map (typed-match-branch-name-map node)))

  (:method ((node typed-node-match))
    (make-typed-node-match
     :type (typed-node-type node)
     :unparsed (typed-node-unparsed node)
     :expr (remove-static-preds (typed-node-match-expr node))
     :branches (mapcar #'remove-static-preds (typed-node-match-branches node))))

  (:method ((node typed-node-seq))
    (make-typed-node-seq
     :type (typed-node-type node)
     :unparsed (typed-node-unparsed node)
     :subnodes (mapcar #'remove-static-preds (typed-node-seq-subnodes node))))

  (:method ((node typed-node-return))
    (make-typed-node-return
     :type (typed-node-type node)
     :unparsed (typed-node-unparsed node)
     :expr (remove-static-preds (typed-node-return-expr node))))

  (:method ((node typed-node-bind))
    (make-typed-node-bind
     :type (typed-node-type node)
     :unparsed (typed-node-unparsed node)
     :name (typed-node-bind-name node)
     :expr (remove-static-preds (typed-node-bind-expr node))
     :body (remove-static-preds (typed-node-bind-body node)))))

(defgeneric rewrite-recursive-calls (node bindings)
  (:method ((node typed-node-literal) bindings)
    (declare (type typed-node node)
             (type scheme-binding-list bindings)
             (ignore bindings))
    node)

  (:method ((node typed-node-variable) bindings)
    (declare (type typed-node node)
             (type scheme-binding-list bindings))
    (let* ((name (typed-node-variable-name node))
           (binding (find name bindings :key #'car)))
      (unless binding
        (return-from rewrite-recursive-calls node))
      (let* ((node-type (qualified-ty-type (fresh-inst (typed-node-type node))))
             (new-type (fresh-inst (cdr binding)))
             (new-type-unqualified (qualified-ty-type new-type))
             (subs (match new-type-unqualified node-type))
             (new-type (to-scheme (apply-substitution subs new-type))))
        (replace-node-type node new-type))))

  (:method ((node typed-node-application) bindings)
    (declare (type typed-node node)
             (type scheme-binding-list bindings))
    (make-typed-node-application
     :type (typed-node-type node)
     :unparsed (typed-node-unparsed node)
     :rator (rewrite-recursive-calls
             (typed-node-application-rator node)
             bindings)
     :rands (mapcar
             (lambda (node)
               (rewrite-recursive-calls node bindings))
             (typed-node-application-rands node))))

  (:method ((node typed-node-abstraction) bindings)
    (declare (type typed-node node)
             (type scheme-binding-list bindings))
    (make-typed-node-abstraction
     :type (typed-node-type node)
     :unparsed (typed-node-unparsed node)
     :vars (typed-node-abstraction-vars node)
     :subexpr (rewrite-recursive-calls (typed-node-abstraction-subexpr node) bindings)
     :name-map (typed-node-abstraction-name-map node)))

  (:method ((node typed-node-let) bindings)
    (declare (type typed-node node)
             (type scheme-binding-list bindings))

    (make-typed-node-let
     :type (typed-node-type node)
     :unparsed (typed-node-unparsed node)
     :bindings (loop :for (name . node) :in (typed-node-let-bindings node)
                     :collect (cons
                               name
                               (rewrite-recursive-calls node bindings)))
     :subexpr (rewrite-recursive-calls (typed-node-let-subexpr node) bindings)
     :explicit-types (typed-node-let-explicit-types node)
     :name-map (typed-node-let-name-map node)))

  (:method ((node typed-node-lisp) bindings)
    (declare (type typed-node node)
             (type scheme-binding-list bindings)
             (ignore bindings))
    node)

  (:method ((branch typed-match-branch) bindings)
    (declare (type typed-match-branch branch)
             (type scheme-binding-list bindings))
    (make-typed-match-branch
     :unparsed (typed-match-branch-unparsed branch)
     :pattern (typed-match-branch-pattern branch)
     :subexpr (rewrite-recursive-calls
               (typed-match-branch-subexpr branch)
               bindings)
     :bindings (typed-match-branch-bindings branch)
     :name-map (typed-match-branch-name-map branch)))

  (:method ((node typed-node-match) bindings)
    (declare (type typed-node node)
             (type scheme-binding-list bindings))
    (make-typed-node-match
     :type (typed-node-type node)
     :unparsed (typed-node-unparsed node)
     :expr (rewrite-recursive-calls (typed-node-match-expr node) bindings)
     :branches (mapcar
                (lambda (branch)
                  (rewrite-recursive-calls branch bindings))
                (typed-node-match-branches node))))

  (:method ((node typed-node-seq) bindings)
    (declare (type typed-node node)
             (type scheme-binding-list bindings))
    (make-typed-node-seq
     :type (typed-node-type node)
     :unparsed (typed-node-unparsed node)
     :subnodes (mapcar
                (lambda (node)
                  (rewrite-recursive-calls node bindings))
                (typed-node-seq-subnodes node))))

  (:method ((node typed-node-return) bindings)
    (declare (type typed-node node)
             (type scheme-binding-list bindings))
    (make-typed-node-return
     :type (typed-node-type node)
     :unparsed (typed-node-unparsed node)
     :expr (rewrite-recursive-calls (typed-node-return-expr node) bindings)))

  (:method ((node typed-node-bind) bindings)
    (declare (type typed-node node)
             (type scheme-binding-list bindings))
    (make-typed-node-bind
     :type (typed-node-type node)
     :unparsed (typed-node-unparsed node)
     :name (typed-node-bind-name node)
     :expr (rewrite-recursive-calls (typed-node-bind-expr node) bindings)
     :body (rewrite-recursive-calls (typed-node-bind-body node) bindings))))
