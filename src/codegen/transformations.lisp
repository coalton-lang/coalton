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
   #:make-action
   #:traverse
   #:traverse-with-binding-list
   #:update-function-env
   #:make-function-t
   #:rename-type-variables))

(in-package #:coalton-impl/codegen/transformations)

(defun node-subtype-p (symbol)
  (and (symbolp symbol) (subtypep symbol 'node)))

(deftype node-subtype ()
  '(satisfies node-subtype-p))

(deftype when-keyword ()
  '(member :before :after :traverse))

(defstruct (action (:constructor make-action (when type function)))
  "Container for a function to call on nodes of a particular type - before, after, or instead of the recursive call in the traversal"
  (when     ':after                   :type when-keyword :read-only t)
  (type     'node                     :type node-subtype :read-only t)
  (function (util:required 'function) :type function     :read-only t))

(defun action-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'action-p x)))

(deftype action-list ()
  '(satisfies action-list-p))

(defun make-default-traversals ()
  "Create traversal functions which recurse through the AST, passing arguments unchanged."
  (declare (values action-list &optional))
  (load-time-value
   (list
    (make-action ':traverse 'node-application
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
    (make-action ':traverse 'node-direct-application
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
    (make-action ':traverse 'node-abstraction
                 (lambda (node traverse &rest args)
                   (declare (type function traverse)
                            (type node-abstraction node)
                            (values node-abstraction &optional))
                   (make-node-abstraction
                    :type (node-type node)
                    :vars (node-abstraction-vars node)
                    :subexpr (funcall traverse (node-abstraction-subexpr node) args))))
    (make-action ':traverse 'node-let
                 (lambda (node traverse &rest args)
                   (declare (type function traverse)
                            (type node-let node)
                            (values node-let &optional))
                   (make-node-let
                    :type (node-type node)
                    :bindings (loop :for (name . node) :in (node-let-bindings node)
                                    :collect (cons name (funcall traverse node args)))
                    :subexpr (funcall traverse (node-let-subexpr node) args))))
    (make-action ':traverse 'node-match
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
    (make-action ':traverse 'node-while
                 (lambda (node traverse &rest args)
                   (declare (type function traverse)
                            (type node-while node)
                            (values node-while &optional))
                   (make-node-while
                    :type (node-type node)
                    :label (node-while-label node)
                    :expr (funcall traverse (node-while-expr node) args)
                    :body (funcall traverse (node-while-body node) args))))
    (make-action ':traverse 'node-while-let
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
    (make-action ':traverse 'node-loop
                 (lambda (node traverse &rest args)
                   (declare (type function traverse)
                            (type node-loop node)
                            (values node-loop &optional))
                   (make-node-loop
                    :type (node-type node)
                    :label (node-loop-label node)
                    :body (funcall traverse (node-loop-body node) args))))
    (make-action ':traverse 'node-seq
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
    (make-action ':traverse 'node-return
                 (lambda (node traverse &rest args)
                   (declare (type function traverse)
                            (type node-return node)
                            (values node-return &optional))
                   (make-node-return
                    :type (node-type node)
                    :expr (funcall traverse (node-return-expr node) args))))
    (make-action ':traverse 'node-field
                 (lambda (node traverse &rest args)
                   (declare (type function traverse)
                            (type node-field node)
                            (values node-field &optional))
                   (make-node-field
                    :type (node-type node)
                    :name (node-field-name node)
                    :dict (funcall traverse (node-field-dict node) args))))
    (make-action ':traverse 'node-dynamic-extent
                 (lambda (node traverse &rest args)
                   (declare (type function traverse)
                            (type node-dynamic-extent node)
                            (values node-dynamic-extent &optional))
                   (make-node-dynamic-extent
                    :type (node-type node)
                    :name (node-dynamic-extent-name node)
                    :node (funcall traverse (node-dynamic-extent-node node) args)
                    :body (funcall traverse (node-dynamic-extent-body node) args))))
    (make-action ':traverse 'node-bind
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

(defun optional-call (when-key type-key actions traverse args node)
  "Look up a function in `actions` corresponding to `when-key` and `type-key`, and if it exists, call it with arguments `node`, `traverse` (if `(equal ':traverse (car key))`) and (spread) `args`."
  (declare (type when-keyword when-key)
           (type node-subtype type-key)
           (type action-list  actions)
           (type function     traverse)
           (type list         args)
           (type node         node))
  (or
   (alexandria:when-let*
       ((action (find-if (lambda (action)
                           (declare (type action action)
                                    (values boolean &optional))
                           (and (equal when-key (action-when action))
                                (equal type-key (action-type action))))
                         actions))
        (f (action-function action)))
     (if (equal ':traverse when-key)
         (apply f node traverse args)
         (apply f node args)))
   node))

(defun traverse (initial-node custom-actions &optional (initial-args nil))
  "Create a recursive \"generic\" traversal with pre-, in-, and post-operations fixed by `custom-actions` (with the default traversals as a fallback), and evaluate this traversal with `initial-node` and `initial-args`."
  (declare (type node        initial-node)
           (type action-list custom-actions)
           (type list        initial-args)
           (values node &optional))
  (let ((actions (append custom-actions (make-default-traversals))))
    (labels ((traverse (current-node args)
               (declare (type node current-node)
                        (type list args)
                        (values node &optional))
               (alexandria-2:line-up-last
                current-node
                (optional-call ':before   'node                                actions #'traverse args)
                (optional-call ':before   (class-name (class-of current-node)) actions #'traverse args)
                (optional-call ':traverse (class-name (class-of current-node)) actions #'traverse args)
                (optional-call ':after    (class-name (class-of current-node)) actions #'traverse args)
                (optional-call ':after    'node                                actions #'traverse args))))
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
    (make-action ':traverse 'node-abstraction
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
    (make-action ':traverse 'node-let
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
    (make-action ':traverse 'node-match
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
    (make-action ':traverse 'node-dynamic-extent
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
    (make-action ':traverse 'node-bind
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
    (make-action ':after 'node
                 (lambda (node)
                   (declare (type node node)
                            (values node &optional))
                   (copy-node node
                              (tc:apply-substitution subs (node-type node)))))
    (make-action ':after 'node-direct-application
                 (lambda (node)
                   (declare (type node-direct-application node)
                            (values node-direct-application &optional))
                   (make-node-direct-application
                    :type (node-type node)
                    :rator-type (tc:apply-substitution subs (node-direct-application-rator-type node))
                    :rator (node-direct-application-rator node)
                    :rands (node-direct-application-rands node))))
    (make-action ':after 'node-match
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
    (make-action ':after 'node-while-let
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
      (make-action ':after 'node
                   (lambda (node)
                     (declare (type node node)
                              (values))
                     (alexandria:unionf tyvars
                                        (tc:type-variables (node-type node)))
                     (values)))
      (make-action ':after 'node-direct-application
                   (lambda (node)
                     (declare (type node-direct-application node)
                              (values))
                     (alexandria:unionf tyvars
                                        (tc:type-variables (node-direct-application-rator-type node)))
                     (values)))
      (make-action ':after 'node-match
                   (lambda (node)
                     (declare (type node-match node)
                              (values))
                     (mapcar
                      (lambda (branch)
                        (alexandria:unionf tyvars
                                           (tc:type-variables (match-branch-pattern branch))))
                      (node-match-branches node))
                     (values)))
      (make-action ':after 'node-while-let
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
