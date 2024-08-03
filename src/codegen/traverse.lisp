(defpackage #:coalton-impl/codegen/traverse
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:action
   #:traverse
   #:traverse-with-binding-list))

(in-package #:coalton-impl/codegen/traverse)

(defun node-subtype-p (symbol)
  (and (symbolp symbol) (subtypep symbol 'node)))

(deftype node-subtype ()
  "A symbol signifying either `'node` or a particular type of node like
`'node-application`. This is used in `action` structures to determine
what types of nodes an operation should be performed on when using
`traverse`."
  '(satisfies node-subtype-p))

(deftype when-keyword ()
  "A keyword used in `action` structures to determine at what time an
operation should be performed when using `traverse`. The keyword
`:traverse` represents the main call of the traversal which recurses
through sub-nodes of a given node. The keywords `:before` and `:after`
tag actions to be performed before or after the recursive call of a
traversal."
  '(member :before :after :traverse))

(defstruct (action (:constructor make-action (when type function)))
  "Container for a function to call on nodes of a particular `type` -
before, after, or in place of the recursive call in a traversal. If
`type` is `'node`, then the function will be called before or after
every node in the traversal; otherwise, the action only applies to
nodes of the specified `type`. The when-keyword `:traverse` will have
no effect when paired with type `'node`; a recursive call defined for
every node would have to be an entire generic already. The `function`
will be called with `(apply function node args)` if `when` is
`:before` or `:after`, and it will be called with `(apply function
node traverse args)` if `when` is `:traverse`, so that the `function`
can call the provided `traverse` to invoke recursion. The `function`
should return either a modified `node` to take its place in the newly
constructed tree, or void to indicate that the input node should be
kept unchanged."
  (when     ':after                   :type when-keyword :read-only t)
  (type     'node                     :type node-subtype :read-only t)
  (function (util:required 'function) :type function     :read-only t))

(defmacro action ((when type &rest args) &body body)
  "Construct an action block, turning `body` into a lambda function
accepting `args` if `args` are provided. If there are no `args`, then
`body` is assumed to be a function object and is used directly with
`when` and `type` to create an `action` instance. If there are `args`
and `when` is `:traverse`, then `args` must include at least the node
to traverse and the recursive function object as the first two
arguments, which are declared to have types `type` and `function`,
respectively, inside the generated lambda function. Otherwise, the
first argument is declared to be a node of type `type`, and the `args`
and `body` are again used to generate a lambda function."
  (check-type when         keyword)
  (check-type type         symbol)
  (assert (subtypep type 'node))
  (cond
    ((endp args)
     (assert (= 1 (length body)))
     `(make-action ',when ',type ,@body))
    ((eq ':traverse when)
     (assert (< 1 (length args)))
     `(make-action ',when ',type
                   (lambda (,@args)
                     (declare (type ,(first args) ,type)
                              (type ,(second args) function))
                     ,@body)))
    (t
     `(make-action ',when ',type
                   (lambda (,@args)
                     (declare (type ,(first args) ,type))
                     ,@body)))))

(defun action-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'action-p x)))

(deftype action-list ()
  "A list of actions for use in `traverse`."
  '(satisfies action-list-p))

(defun make-default-actions ()
  "Traversal actions which recurse through the AST, passing arguments
unchanged. With no other actions, this will simply create a mostly
deep copy of internal `node`s (and `match-branch`es) of the AST, but
shallow copy all other slots, including types, patterns, and leaf
nodes."
  (declare (values action-list &optional))
  (load-time-value
   (list
    (action (:traverse node-application node traverse &rest args)
      (make-node-application
       :type (node-type node)
       :rator (apply traverse (node-application-rator node) args)
       :rands (mapcar
               (lambda (node)
                 (apply traverse node args))
               (node-application-rands node))))
    (action (:traverse node-direct-application node traverse &rest args)
      (make-node-direct-application
       :type (node-type node)
       :rator-type (node-direct-application-rator-type node)
       :rator (node-direct-application-rator node)
       :rands (mapcar
               (lambda (node)
                 (apply traverse node args))
               (node-direct-application-rands node))))
    (action (:traverse node-abstraction node traverse &rest args)
      (make-node-abstraction
       :type (node-type node)
       :vars (node-abstraction-vars node)
       :subexpr (apply traverse (node-abstraction-subexpr node) args)))
    (action (:traverse node-let node traverse &rest args)
      (make-node-let
       :type (node-type node)
       :bindings (loop :for (name . node) :in (node-let-bindings node)
                       :collect (cons name (apply traverse node args)))
       :subexpr (apply traverse (node-let-subexpr node) args)))
    (action (:traverse node-match node traverse &rest args)
      (make-node-match
       :type (node-type node)
       :expr (apply traverse (node-match-expr node) args)
       :branches (mapcar
                  (lambda (branch)
                    (make-match-branch
                     :pattern (match-branch-pattern branch)
                     :body (apply traverse
                                  (match-branch-body branch)
                                  args)))
                  (node-match-branches node))))
    (action (:traverse node-while node traverse &rest args)
      (make-node-while
       :type (node-type node)
       :label (node-while-label node)
       :expr (apply traverse (node-while-expr node) args)
       :body (apply traverse (node-while-body node) args)))
    (action (:traverse node-while-let node traverse &rest args)
      (make-node-while-let
       :type (node-type node)
       :label (node-while-let-label node)
       :pattern (node-while-let-pattern node)
       :expr (apply traverse (node-while-let-expr node) args)
       :body (apply traverse (node-while-let-body node) args)))
    (action (:traverse node-loop node traverse &rest args)
      (make-node-loop
       :type (node-type node)
       :label (node-loop-label node)
       :body (apply traverse (node-loop-body node) args)))
    (action (:traverse node-seq node traverse &rest args)
      (make-node-seq
       :type (node-type node)
       :nodes (mapcar
               (lambda (node)
                 (apply traverse node args))
               (node-seq-nodes node))))
    (action (:traverse node-return node traverse &rest args)
      (make-node-return
       :type (node-type node)
       :expr (apply traverse (node-return-expr node) args)))
    (action (:traverse node-field node traverse &rest args)
      (make-node-field
       :type (node-type node)
       :name (node-field-name node)
       :dict (apply traverse (node-field-dict node) args)))
    (action (:traverse node-dynamic-extent node traverse &rest args)
      (make-node-dynamic-extent
       :type (node-type node)
       :name (node-dynamic-extent-name node)
       :node (apply traverse (node-dynamic-extent-node node) args)
       :body (apply traverse (node-dynamic-extent-body node) args)))
    (action (:traverse node-bind node traverse &rest args)
      (make-node-bind
       :type (node-type node)
       :name (node-bind-name node)
       :expr (apply traverse (node-bind-expr node) args)
       :body (apply traverse (node-bind-body node) args))))
   t))

(defun fire-action (when-key type-key actions traverse args node)
  "Look up a function in `actions` corresponding to `when-key` and
`type-key`, and if it exists, call it with arguments `node`,
`traverse` (if `(eq ':traverse (car key))`) and (spread) `args`. The
return value is the value returned by the action, or the original node
if the action returned `nil` or no action was found."
  (declare (type when-keyword when-key)
           (type node-subtype type-key)
           (type action-list  actions)
           (type function     traverse)
           (type list         args)
           (type node         node)
           (values node &optional))
  (or
   (alexandria:when-let*
       ((action (find-if (lambda (action)
                           (declare (type action action)
                                    (values boolean &optional))
                           (and (eq when-key (action-when action))
                                (eq type-key (action-type action))))
                         actions))
        (f (action-function action)))
     (if (eq ':traverse when-key)
         (apply f node traverse args)
         (apply f node args)))
   node))

(defun traverse (initial-node custom-actions &rest initial-args)
  "The intended use case of `traverse` is to recursively propagate an
operation through the AST when only a few nodes need custom behavior,
obviating the need to copy an entire `defgeneric` for each new AST
traversal. This function creates a recursive \"generic\" traversal
with pre-, in-, and post-operations fixed by `custom-actions` (with
the default traversals as a fallback), and evaluates the traversal
with `initial-node` and `initial-args`. For each node type, the action
tagged with `:traverse` will be the most general, so the `:before` and
`:after` should not be necessary. The `:before` and `:after` are
mainly useful if the default deep-copy recursion behavior is desired,
but a small change has to be made depending on the node type, or if
some external state is mutated to keep track of some information about
the recursion stack. The `:before 'node` and `:after 'node` actions
may also be useful for operations that must be performed uniformly on
every node, such as type collection and substitution.

Note that the wrapped traversal pipes the node through three actions
corresponding to its original class, so it is important to not define
any node-specific actions that will run after another action which may
return a node of a different type. For example, if there is an action
on `:traverse 'node-variable` to substitute variables with arbitrary
other nodes, then it would be inappropriate to also define an action
`:after 'node-variable`."
  (declare (type node        initial-node)
           (type action-list custom-actions)
           (type list        initial-args)
           (values node &optional))
  (let ((actions (append custom-actions (make-default-actions))))
    (labels ((traverse (current-node &rest args)
               (declare (type node current-node)
                        (type list args)
                        (values node &optional))
               (alexandria-2:line-up-last
                current-node
                (fire-action ':before   'node                                actions #'traverse args)
                (fire-action ':before   (class-name (class-of current-node)) actions #'traverse args)
                (fire-action ':traverse (class-name (class-of current-node)) actions #'traverse args)
                (fire-action ':after    (class-name (class-of current-node)) actions #'traverse args)
                (fire-action ':after    'node                                actions #'traverse args))))
      (apply #'traverse initial-node initial-args))))

;;;
;;; Traversals with bound variables
;;;

(defun make-binding-list-actions ()
  "These are the custom traversal actions needed to ensure that
`traverse`'s `args` consists of a list of the variable names which are
bound at the given point."
  (declare (values action-list &optional))
  (load-time-value
   (list
    (action (:traverse node-abstraction node traverse bound-variables)
      (make-node-abstraction
       :type (node-type node)
       :vars (node-abstraction-vars node)
       :subexpr (funcall traverse
                         (node-abstraction-subexpr node)
                         (append (node-abstraction-vars node) bound-variables))))
    (action (:traverse node-let node traverse bound-variables)
      (let ((new-bound-variables (append
                                  (mapcar #'car (node-let-bindings node))
                                  bound-variables)))
        (make-node-let
         :type (node-type node)
         :bindings (loop :for (name . node) :in (node-let-bindings node)
                         :collect (cons name (funcall traverse node new-bound-variables)))
         :subexpr (funcall traverse (node-let-subexpr node) new-bound-variables))))
    (action (:traverse node-match node traverse bound-variables)
      (make-node-match
       :type (node-type node)
       :expr (funcall traverse (node-match-expr node) bound-variables)
       :branches (mapcar
                  (lambda (branch)
                    (make-match-branch
                     :pattern (match-branch-pattern branch)
                     :body (funcall traverse
                                    (match-branch-body branch)
                                    (append (pattern-variables (match-branch-pattern branch))
                                            bound-variables))))
                  (node-match-branches node))))
    (action (:traverse node-dynamic-extent node traverse bound-variables)
      (let ((new-bound-variables (cons (node-dynamic-extent-name node) bound-variables)))
        (make-node-dynamic-extent
         :type (node-type node)
         :name (node-dynamic-extent-name node)
         :node (funcall traverse (node-dynamic-extent-node node) new-bound-variables)
         :body (funcall traverse (node-dynamic-extent-body node) new-bound-variables))))
    (action (:traverse node-bind node traverse bound-variables)
      (let ((new-bound-variables (cons (node-bind-name node) bound-variables)))
        (make-node-bind
         :type (node-type node)
         :name (node-bind-name node)
         :expr (funcall traverse (node-bind-expr node) new-bound-variables)
         :body (funcall traverse (node-bind-body node) new-bound-variables)))))
   t))

(defun traverse-with-binding-list (node actions)
  "Traverse `node` while keeping track of a list of `bound-variables`
which gets passed as an argument whenever a function from `actions`
gets called."
  (declare (type node        node)
           (type action-list actions)
           (values node &optional))
  (traverse node
            (append actions (make-binding-list-actions))
            nil))

;;;
;;; Examples
;;;

(defun count-nodes (node)
  "Count the number of nodes in the AST corresponding to `node`."
  (declare (type node node)
           (values (integer 0) &optional))
  ;; Create a counter variable to store the number of nodes seen.
  (let ((counter 0))
    ;; Traverse the node with no extra arguments, and one action.
    (traverse
     node
     (list
      ;; Create an action to perform once for each node.
      ;; (`:before` or `:after` work equally well if we just
      ;; want a single external mutation per node.)
      (alexandria:whichever
       ;; the `action` macro needs either a function as the body
       (action (:after node)
         (lambda (_node)
           (declare (ignore _node))
           (incf counter)
           (values)))
       ;; or arguments to be used in order to create a lambda function
       ;; with the given body
       (action (:before node _node)
         (declare (ignore _node))
         (incf counter)
         (values))
       )))
    counter))

(defun print-node-traversal-order (node)
  "Print the pre-order and post-order traversal structure of `node`
without any slot information."
  (declare (type node node)
           (values node &optional))
  (let ((counter 0))
    (traverse
     node
     (list
      (action (:before node node)
        (format t "PRE:  ~v@{|   ~}~A~%" counter (class-name (class-of node)))
        (incf counter)
        (values))
      (action (:after node node)
        (decf counter)
        (format t "POST: ~v@{|   ~}~A~%" counter (class-name (class-of node)))
        (values))))))
