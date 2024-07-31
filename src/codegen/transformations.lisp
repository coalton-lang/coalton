(defpackage #:coalton-impl/codegen/transformations
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/algorithm
   #:immutable-map-data)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:traverse
   #:traverse-with-binding-list
   #:update-function-env
   #:make-function-t
   #:rename-type-variables))

(in-package #:coalton-impl/codegen/transformations)

(defun make-default-traversals ()
  "Create traversal functions which recurse through the AST, passing arguments unchanged."
  (load-time-value
   (list
    (cons (cons ':traverse 'node-application)
          (lambda (node traverse &rest args)
            (declare (type function traverse)
                     (type node-application node)
                     (values node-application &optional))
            (make-node-application
             :type (node-type node)
             :rator (funcall traverse (node-application-rator node) args)
             :rands (mapcar
                     (lambda (node)
                       (funcall traverse node args))
                     (node-application-rands node)))))
    (cons (cons ':traverse 'node-direct-application)
          (lambda (node traverse &rest args)
            (declare (type function traverse)
                     (type node-direct-application node)
                     (values node-direct-application &optional))
            (make-node-direct-application
             :type (node-type node)
             :rator-type (node-direct-application-rator-type node)
             :rator (node-direct-application-rator node)
             :rands (mapcar
                     (lambda (node)
                       (funcall traverse node args))
                     (node-direct-application-rands node)))))
    (cons (cons ':traverse 'node-abstraction)
          (lambda (node traverse &rest args)
            (declare (type function traverse)
                     (type node-abstraction node)
                     (values node-abstraction &optional))
            (make-node-abstraction
             :type (node-type node)
             :vars (node-abstraction-vars node)
             :subexpr (funcall traverse (node-abstraction-subexpr node) args))))
    (cons (cons ':traverse 'node-let)
          (lambda (node traverse &rest args)
            (declare (type function traverse)
                     (type node-let node)
                     (values node-let &optional))
            (make-node-let
             :type (node-type node)
             :bindings (loop :for (name . node) :in (node-let-bindings node)
                             :collect (cons name (funcall traverse node args)))
             :subexpr (funcall traverse (node-let-subexpr node) args))))
    (cons (cons ':traverse 'node-match)
          (lambda (node traverse &rest args)
            (declare (type function traverse)
                     (type node-match node)
                     (values node-match &optional))
            (make-node-match
             :type (node-type node)
             :expr (funcall traverse (node-match-expr node) args)
             :branches (mapcar
                        (lambda (branch)
                          (make-match-branch
                           :pattern (match-branch-pattern branch)
                           :body (funcall traverse
                                          (match-branch-body branch)
                                          args)))
                        (node-match-branches node)))))
    (cons (cons ':traverse 'node-while)
          (lambda (node traverse &rest args)
            (declare (type function traverse)
                     (type node-while node)
                     (values node-while &optional))
            (make-node-while
             :type (node-type node)
             :label (node-while-label node)
             :expr (funcall traverse (node-while-expr node) args)
             :body (funcall traverse (node-while-body node) args))))
    (cons (cons ':traverse 'node-while-let)
          (lambda (node traverse &rest args)
            (declare (type function traverse)
                     (type node-while-let node)
                     (values node-while-let &optional))
            (make-node-while-let
             :type (node-type node)
             :label (node-while-let-label node)
             :pattern (node-while-let-pattern node)
             :expr (funcall traverse (node-while-let-expr node) args)
             :body (funcall traverse (node-while-let-body node) args))))
    (cons (cons ':traverse 'node-loop)
          (lambda (node traverse &rest args)
            (declare (type function traverse)
                     (type node-loop node)
                     (values node-loop &optional))
            (make-node-loop
             :type (node-type node)
             :label (node-loop-label node)
             :body (funcall traverse (node-loop-body node) args))))
    (cons (cons ':traverse 'node-seq)
          (lambda (node traverse &rest args)
            (declare (type function traverse)
                     (type node-seq node)
                     (values node-seq &optional))
            (make-node-seq
             :type (node-type node)
             :nodes (mapcar
                     (lambda (node)
                       (funcall traverse node args))
                     (node-seq-nodes node)))))
    (cons (cons ':traverse 'node-return)
          (lambda (node traverse &rest args)
            (declare (type function traverse)
                     (type node-return node)
                     (values node-return &optional))
            (make-node-return
             :type (node-type node)
             :expr (funcall traverse (node-return-expr node) args))))
    (cons (cons ':traverse 'node-field)
          (lambda (node traverse &rest args)
            (declare (type function traverse)
                     (type node-field node)
                     (values node-field &optional))
            (make-node-field
                  :type (node-type node)
                  :name (node-field-name node)
                  :dict (funcall traverse (node-field-dict node) args))))
    (cons (cons ':traverse 'node-dynamic-extent)
          (lambda (node traverse &rest args)
            (declare (type function traverse)
                     (type node-dynamic-extent node)
                     (values node-dynamic-extent &optional))
            (make-node-dynamic-extent
                  :type (node-type node)
                  :name (node-dynamic-extent-name node)
                  :node (funcall traverse (node-dynamic-extent-node node) args)
                  :body (funcall traverse (node-dynamic-extent-body node) args))))
    (cons (cons ':traverse 'node-bind)
          (lambda (node traverse &rest args)
            (declare (type function traverse)
                     (type node-bind node)
                     (values node-bind &optional))
            (make-node-bind
                  :type (node-type node)
                  :name (node-bind-name node)
                  :expr (funcall traverse (node-bind-expr node) args)
                  :body (funcall traverse (node-bind-body node) args)))))
   t))

(defun optional-call (key funs traverse args node)
   "Look up a function in `funs` corresponding to `key`, and if it exists, call it with arguments `node`, `traverse` (if `(equal ':traverse (car key))`) and (spread) `args`."
  (declare (type (cons symbol symbol) key)
           (type list funs)
           (type function traverse)
           (type list args)
           (type node node))
  (or
   (alexandria:when-let*
       ((f (cdr (assoc key funs :test #'equal))))
     (if (equal ':traverse (car key))
         (apply f node traverse args)
         (apply f node args)))
   node))

(defun traverse (initial-node custom-funs &optional (initial-args nil))
  "Create a recursive \"generic\" traversal with pre-, in-, and post-operations fixed by `funs` (with the default traversals as a fallback), and evaluate this traversal with `initial-node` and `initial-args`."
  (declare (type node initial-node)
           (type list custom-funs)
           (type list initial-args)
           (values node &optional))
  (let ((funs (append custom-funs (make-default-traversals))))
    (labels ((traverse (current-node args)
               (declare (type node current-node)
                        (type list args)
                        (values node &optional))
               (alexandria-2:line-up-last
                current-node
                (optional-call (cons ':before   'node)                                funs #'traverse args)
                (optional-call (cons ':before   (class-name (class-of current-node))) funs #'traverse args)
                (optional-call (cons ':traverse (class-name (class-of current-node))) funs #'traverse args)
                (optional-call (cons ':after    (class-name (class-of current-node))) funs #'traverse args)
                (optional-call (cons ':after    'node)                                funs #'traverse args))))
      (traverse initial-node initial-args))))

(defun split-binding-definitions (bindings)
  (let ((functions nil)
        (variables nil))
    (loop :for (name . node) :in bindings
          :do (if (node-abstraction-p node)
                  (push (cons name (length (node-abstraction-vars node))) functions)
                  (push name variables)))
    (values functions variables)))

(defun update-function-env (bindings env)
  (declare (type binding-list bindings)
           (type tc:environment env)
           (values tc:environment))
  (multiple-value-bind (toplevel-functions toplevel-values)
      (split-binding-definitions bindings)
    (loop :for (name . arity) :in toplevel-functions
          :do
             (setf env
                   (tc:set-function
                    env
                    name
                    (tc:make-function-env-entry
                     :name name
                     :arity arity))))
    (dolist (name toplevel-values)
      (when (tc:lookup-function env name :no-error t)
        (setf env (tc:unset-function env name)))))
  env)

(defun make-function-table (env)
  (declare (type tc:environment env)
           (values hash-table))
  (let ((table (make-hash-table)))
    (fset:do-map (name entry (immutable-map-data (tc:environment-function-environment env)))
      (setf (gethash name table) (tc:function-env-entry-arity entry)))
    table))

(defun make-binding-list-traversals ()
  "These are the custom traversal behaviors needed to ensure that `traverse`'s `args` contains a list of the variables names which are bound at the given point."
  (load-time-value
   (list
    (cons (cons ':traverse 'node-abstraction)
          (lambda (node traverse &key bound-variables)
            (declare (type node-abstraction node)
                     (type function traverse)
                     (type util:symbol-list bound-variables)
                     (values node-abstraction &optional))
            (make-node-abstraction
             :type (node-type node)
             :vars (node-abstraction-vars node)
             :subexpr (funcall traverse
                               (node-abstraction-subexpr node)
                               (list ':bound-variables
                                     (append (node-abstraction-vars node) bound-variables))))))
    (cons (cons ':traverse 'node-let)
          (lambda (node traverse &key bound-variables)
            (declare (type node-let node)
                     (type function traverse)
                     (type util:symbol-list bound-variables)
                     (values node-let &optional))
            (let ((new-args (list ':bound-variables
                                  (append
                                   (mapcar #'car (node-let-bindings node))
                                   bound-variables))))
              (make-node-let
               :type (node-type node)
               :bindings (loop :for (name . node) :in (node-let-bindings node)
                               :collect (cons name (funcall traverse node new-args)))
               :subexpr (funcall traverse (node-let-subexpr node) new-args)))))
    (cons (cons ':traverse 'node-match)
          (lambda (node traverse &key bound-variables)
            (declare (type node-match node)
                     (type function traverse)
                     (type util:symbol-list bound-variables)
                     (values node-match &optional))
            (make-node-match
             :type (node-type node)
             :expr (funcall traverse (node-match-expr node) (list ':bound-variables bound-variables))
             :branches (mapcar
                        (lambda (branch)
                          (make-match-branch
                           :pattern (match-branch-pattern branch)
                           :body (funcall traverse
                                          (match-branch-body branch)
                                          (list ':bound-variables
                                                (append (pattern-variables (match-branch-pattern branch))
                                                        bound-variables)))))
                        (node-match-branches node)))))
    (cons (cons ':traverse 'node-dynamic-extent)
          (lambda (node traverse &key bound-variables)
            (declare (type node-dynamic-extent node)
                     (type function traverse)
                     (type util:symbol-list bound-variables)
                     (values node-dynamic-extent &optional))
            (let ((new-args (list ':bound-variables
                                  (cons (node-dynamic-extent-name node) bound-variables))))
              (make-node-dynamic-extent
               :type (node-type node)
               :name (node-dynamic-extent-name node)
               :node (funcall traverse (node-dynamic-extent-node node) new-args)
               :body (funcall traverse (node-dynamic-extent-body node) new-args)))))
    (cons (cons ':traverse 'node-bind)
          (lambda (node traverse &key bound-variables)
            (declare (type node-bind node)
                     (type function traverse)
                     (type util:symbol-list bound-variables)
                     (values node-bind &optional))
            (let ((new-args (list ':bound-variables
                                  (cons (node-bind-name node) bound-variables))))
              (make-node-bind
               :type (node-type node)
               :name (node-bind-name node)
               :expr (funcall traverse (node-bind-expr node) new-args)
               :body (funcall traverse (node-bind-body node) new-args))))))
   t))

(defun traverse-with-binding-list (node funs)
  "Traverse `node` while keeping track of a list of `bound-variables` which gets passed as a keyword argument whenever a function from `funs` gets called."
  (declare (type node node)
           (type list funs)
           (values node &optional))
  (traverse node
            (append funs (make-binding-list-traversals))
            (list ':bound-variables nil)))

(defmethod tc:apply-substitution (subs (node node))
  "Substitute type variables in the tree of `node` with other types specified in `subs`."
  (declare (type tc:substitution-list subs)
           (values node &optional))
  (traverse
   node
   (list
    (cons (cons ':after 'node)
          (lambda (node)
            (declare (type node node)
                     (values node &optional))
            (copy-node node
                       (tc:apply-substitution subs (node-type node)))))
    (cons (cons ':after 'node-direct-application)
          (lambda (node)
            (declare (type node-direct-application node)
                     (values node-direct-application &optional))
            (make-node-direct-application
             :type (node-type node)
             :rator-type (tc:apply-substitution subs (node-direct-application-rator-type node))
             :rator (node-direct-application-rator node)
             :rands (node-direct-application-rands node))))
    (cons (cons ':after 'node-match)
          (lambda (node)
            (declare (type node-match node)
                     (values node-match &optional))
            (make-node-match
             :type (node-type node)
             :expr (node-match-expr node)
             :branches (mapcar
                        (lambda (branch)
                          (make-match-branch
                           :pattern (tc:apply-substitution subs (match-branch-pattern branch))
                           :body (match-branch-body branch)))
                        (node-match-branches node)))))
    (cons (cons ':after 'node-while-let)
          (lambda (node)
            (declare (type node-while-let node)
                     (values node-while-let &optional))
            (make-node-while-let
             :type (node-type node)
             :label (node-while-let-label node)
             :pattern (tc:apply-substitution subs (node-while-let-pattern node))
             :expr (node-while-let-expr node)
             :body (node-while-let-body node)))))))

(defmethod tc:type-variables ((node node))
  "Collect all type variables from nodes and patterns in the tree of `node`."
  (declare (values tc:tyvar-list &optional))
  (let ((tyvars nil))
    (traverse
     node
     (list
      (cons (cons ':after 'node)
            (lambda (node)
              (declare (type node node)
                       (values))
              (alexandria:unionf tyvars
                                 (tc:type-variables (node-type node)))
              (values)))
      (cons (cons ':after 'node-direct-application)
            (lambda (node)
              (declare (type node-direct-application node)
                       (values))
              (alexandria:unionf tyvars
                                 (tc:type-variables (node-direct-application-rator-type node)))
              (values)))
      (cons (cons ':after 'node-match)
            (lambda (node)
              (declare (type node-match node)
                       (values))
              (mapcar
               (lambda (branch)
                 (alexandria:unionf tyvars
                                    (tc:type-variables (match-branch-pattern branch))))
               (node-match-branches node))
              (values)))
      (cons (cons ':after 'node-while-let)
            (lambda (node)
              (declare (type node-while-let node)
                       (values))
              (alexandria:unionf tyvars
                                 (tc:type-variables (node-while-let-pattern node)))
              (values)))))
    tyvars))

(defun rename-type-variables (node &optional (renamer #'tc:fresh-type-renamer))
  "Rename the type variables of `node` and its subnodes according to the function `renamer`."
  (declare (type node node)
           (type function renamer)
           (values node &optional))
  (alexandria:if-let ((old-type-variables (tc:type-variables node)))
    (tc:apply-substitution
     (mapcar (lambda (tyvar)
               (tc:make-substitution
                :from tyvar
                :to (funcall renamer tyvar)))
             old-type-variables)
     node)
    node))
