(defpackage #:coalton-impl/codegen/traverse
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:action
   #:count-applications
   #:count-nodes
   #:make-traverse-let-action-skipping-cons-bindings
   #:*traverse*
   #:traverse
   #:traverse-with-path
   #:traverse-with-binding-list))

(in-package #:coalton-impl/codegen/traverse)

(defvar *traverse*)
(setf (documentation '*traverse* 'variable)
      "The recursive function reference for the current traversal. This will
be bound for the extent of any traversal. Inside of an
`action`, `(funcall *traverse* node)` may be used to invoke recursion.")

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
tag actions to be performed before or after the main recursive call of
a traversal."
  '(member :before :after :traverse))

(defstruct (action (:constructor make-action (when type function)))
  "Container for a function to call on nodes of a particular `type` -
before, after, or in place of the recursive call in a traversal. If
`type` is `'node`, then the function will be called before or after
every node in the traversal; otherwise, the action only applies to
nodes of the specified `type`. (The when-keyword `:traverse` will have
no effect when paired with type `'node`; a recursive call defined for
every node would have to be an entire generic already.) The `function`
will be called with `(apply function node args)`. The `function`
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
`when` and `type` to create an `action` instance. Otherwise, the first
argument in `args` is declared to be a node of type `type`, and the
`args` and `body` are used to generate a lambda function."
  (check-type when keyword)
  (check-type type symbol)
  (assert (subtypep type 'node))
  (assert (or (not (endp args)) (= 1 (length body))))
  (if (endp args)
      `(make-action ',when ',type ,@body)
      `(make-action ',when ',type
                    (lambda (,@args)
                      (declare (type ,type ,(first args)))
                      ,@body))))

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
    (action (:traverse node-application node &rest args)
      (make-node-application
       :type (node-type node)
       :rator (apply *traverse* (node-application-rator node) args)
       :rands (mapcar
               (lambda (node)
                 (apply *traverse* node args))
               (node-application-rands node))))
    (action (:traverse node-direct-application node &rest args)
      (make-node-direct-application
       :type (node-type node)
       :rator-type (node-direct-application-rator-type node)
       :rator (node-direct-application-rator node)
       :rands (mapcar
               (lambda (node)
                 (apply *traverse* node args))
               (node-direct-application-rands node))))
    (action (:traverse node-abstraction node &rest args)
      (make-node-abstraction
       :type (node-type node)
       :vars (node-abstraction-vars node)
       :subexpr (apply *traverse* (node-abstraction-subexpr node) args)))
    (action (:traverse node-let node &rest args)
      (make-node-let
       :type (node-type node)
       :bindings (loop :for (name . node) :in (node-let-bindings node)
                       :collect (cons name (apply *traverse* node args)))
       :subexpr (apply *traverse* (node-let-subexpr node) args)))
    (action (:traverse node-match node &rest args)
      (make-node-match
       :type (node-type node)
       :expr (apply *traverse* (node-match-expr node) args)
       :branches (mapcar
                  (lambda (branch)
                    (make-match-branch
                     :pattern (match-branch-pattern branch)
                     :body (apply *traverse*
                                  (match-branch-body branch)
                                  args)))
                  (node-match-branches node))))
    (action (:traverse node-while node &rest args)
      (make-node-while
       :type (node-type node)
       :label (node-while-label node)
       :expr (apply *traverse* (node-while-expr node) args)
       :body (apply *traverse* (node-while-body node) args)))
    (action (:traverse node-while-let node &rest args)
      (make-node-while-let
       :type (node-type node)
       :label (node-while-let-label node)
       :pattern (node-while-let-pattern node)
       :expr (apply *traverse* (node-while-let-expr node) args)
       :body (apply *traverse* (node-while-let-body node) args)))
    (action (:traverse node-loop node &rest args)
      (make-node-loop
       :type (node-type node)
       :label (node-loop-label node)
       :body (apply *traverse* (node-loop-body node) args)))
    (action (:traverse node-seq node &rest args)
      (make-node-seq
       :type (node-type node)
       :nodes (mapcar
               (lambda (node)
                 (apply *traverse* node args))
               (node-seq-nodes node))))
    (action (:traverse node-return-from node &rest args)
      (make-node-return-from
       :type (node-type node)
       :name (node-return-from-name node)
       :expr (apply *traverse* (node-return-from-expr node) args)))
    (action (:traverse node-block node &rest args)
      (make-node-block
       :type (node-type node)
       :name (node-block-name node)
       :body (apply *traverse* (node-block-body node) args)))
    (action (:traverse node-field node &rest args)
      (make-node-field
       :type (node-type node)
       :name (node-field-name node)
       :dict (apply *traverse* (node-field-dict node) args)))
    (action (:traverse node-dynamic-extent node &rest args)
      (make-node-dynamic-extent
       :type (node-type node)
       :name (node-dynamic-extent-name node)
       :node (apply *traverse* (node-dynamic-extent-node node) args)
       :body (apply *traverse* (node-dynamic-extent-body node) args)))
    (action (:traverse node-bind node &rest args)
      (make-node-bind
       :type (node-type node)
       :name (node-bind-name node)
       :expr (apply *traverse* (node-bind-expr node) args)
       :body (apply *traverse* (node-bind-body node) args)))
    (action (:traverse node-locally node &rest args)
      (make-node-locally
       :type (node-type node)
       :inhibit-inlining (node-locally-inhibit-inlining node)
       :noinline-functions (node-locally-noinline-functions node)
       :subexpr (apply *traverse* (node-locally-subexpr node) args))))
   t))

(defun fire-action (when-key type-key actions args node)
  "Look up a function in `actions` corresponding to `when-key` and
`type-key`, and if it exists, apply it with arguments `node`
and (spread) `args`. The return value is the value returned by the
action, or the original node if the action returned `nil` or no action
was found."
  (declare (type when-keyword when-key)
           (type node-subtype type-key)
           (type action-list  actions)
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
     (apply f node args))
   node))

(defun traverse (initial-node custom-actions &rest initial-args)
  "The intended use case of `traverse` is to recursively propagate an
operation through the AST when only a few nodes need custom behavior,
obviating the need to copy an entire `defgeneric` for each new AST
traversal. This function creates a recursive \"generic\" traversal
with pre-, in-, and post-operations fixed by `custom-actions` (with
the default traversals as a fallback), and evaluates the traversal
with `initial-node` and `initial-args`. The `:before` and `:after` are
mainly useful if the default deep-copy recursion behavior is desired,
but a small change has to be made depending on the node type, or if
some external state is mutated to keep track of some information about
the recursion stack. The `:before 'node` and `:after 'node` actions
may also be useful for operations that must be performed uniformly on
every node, such as type collection and substitution. The `:traverse`
action is necessary if special traversal behavior is required, such as
recursing with modified arguments.

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
    (flet ((current-traverse (current-node &rest args)
             (declare (type node current-node)
                      (type list args)
                      (values node &optional))
             (alexandria-2:line-up-last
              current-node
              (fire-action ':before   'node                                actions args)
              (fire-action ':before   (class-name (class-of current-node)) actions args)
              (fire-action ':traverse (class-name (class-of current-node)) actions args)
              (fire-action ':after    (class-name (class-of current-node)) actions args)
              (fire-action ':after    'node                                actions args))))
      (let ((*traverse* #'current-traverse))
        (apply *traverse* initial-node initial-args)))))

(defun traverse-with-path (node action-list &rest args)
  "Like 'traverse', but actions receive a thunk that returns a reverse
list of the current node's ascendants.
That is, `car` of the ascendants is the node's immediate parent,
and the last element of ascendants is the root of the AST on which
`traverse-with-path` is called.  If visiting node is the root, the
ascendant list is empty."
  (declare (type node node)
           (values node &optional))
  (let ((traversal-path nil))
    (labels ((wrap-action (when action)
               (if action
                   (ecase when
                     (:before
                      (make-action when 'node
                                   (lambda (node path-thunk &rest args)
                                     (push node traversal-path)
                                     (apply (action-function action) node path-thunk args))))
                     (:after
                      (make-action when 'node
                                   (lambda (node path-thunk &rest args)
                                     (prog1 (apply (action-function action)
                                                   node
                                                   path-thunk
                                                   args)
                                       (pop traversal-path))))))
                   (ecase when
                     (:before
                      (make-action when 'node
                                   (lambda (node &rest _rest)
                                     (declare (ignore _rest))
                                     (push node traversal-path))))
                     (:after
                      (make-action when 'node
                                   (lambda (&rest _rest)
                                     (declare (ignore _rest))
                                     (pop traversal-path))))))))
      (let* ((before-node-action (find-if (lambda (action)
                                            (declare (type action action)
                                                     (values boolean &optional))
                                            (and (eq :before (action-when action))
                                                 (eq 'node (action-type action))))
                                          action-list))
             (after-node-action  (find-if (lambda (action)
                                            (declare (type action action)
                                                     (values boolean &optional))
                                            (and (eq :after (action-when action))
                                                 (eq 'node (action-type action))))
                                          action-list))
             (remaining-actions (remove-if (lambda (action)
                                             (member action (list before-node-action
                                                                  after-node-action)))
                                           action-list)))
        (apply #'traverse node
               (list* (wrap-action :before before-node-action)
                      (wrap-action :after after-node-action)
                      remaining-actions)
               (lambda () (cdr traversal-path))
               args)))))

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
    (action (:traverse node-abstraction node bound-variables)
      (make-node-abstraction
       :type (node-type node)
       :vars (node-abstraction-vars node)
       :subexpr (funcall *traverse*
                         (node-abstraction-subexpr node)
                         (append (node-abstraction-vars node) bound-variables))))
    (action (:traverse node-let node bound-variables)
      (let ((new-bound-variables (append
                                  (mapcar #'car (node-let-bindings node))
                                  bound-variables)))
        (make-node-let
         :type (node-type node)
         :bindings (loop :for (name . node) :in (node-let-bindings node)
                         :collect (cons name (funcall *traverse* node new-bound-variables)))
         :subexpr (funcall *traverse* (node-let-subexpr node) new-bound-variables))))
    (action (:traverse node-match node bound-variables)
      (make-node-match
       :type (node-type node)
       :expr (funcall *traverse* (node-match-expr node) bound-variables)
       :branches (mapcar
                  (lambda (branch)
                    (make-match-branch
                     :pattern (match-branch-pattern branch)
                     :body (funcall *traverse*
                                    (match-branch-body branch)
                                    (append (pattern-variables (match-branch-pattern branch))
                                            bound-variables))))
                  (node-match-branches node))))
    (action (:traverse node-dynamic-extent node bound-variables)
      (let ((new-bound-variables (cons (node-dynamic-extent-name node) bound-variables)))
        (make-node-dynamic-extent
         :type (node-type node)
         :name (node-dynamic-extent-name node)
         :node (funcall *traverse* (node-dynamic-extent-node node) new-bound-variables)
         :body (funcall *traverse* (node-dynamic-extent-body node) new-bound-variables))))
    (action (:traverse node-bind node bound-variables)
      (let ((new-bound-variables (cons (node-bind-name node) bound-variables)))
        (make-node-bind
         :type (node-type node)
         :name (node-bind-name node)
         :expr (funcall *traverse* (node-bind-expr node) new-bound-variables)
         :body (funcall *traverse* (node-bind-body node) new-bound-variables)))))
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
       ;; The `action` macro needs either a function as the body ...
       (action (:after node)
         (lambda (_node)
           (declare (ignore _node))
           (incf counter)
           (values)))
       ;; ... or arguments to be used in order to create a lambda
       ;; function with the given body.
       (action (:before node _node)
         (declare (ignore _node))
         (incf counter)
         (values))
       )))
    counter))

(defun count-applications (node)
  "Count the number of function application nodes in the AST
corresponding to `node`."
  (declare (type node node)
           (values (integer 0) &optional))
  (let ((counter 0))
    (traverse
     node
     (list
      (action (:after node-application _node)
        (declare (ignore _node))
        (incf counter)
        (values))
      (action (:after node-direct-application _node)
        (declare (ignore _node))
        (incf counter)
        (values))))
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

(defun print-node-parent (node)
  "Print visiting node and its parent, using `traverse-with-path`."
  (declare (type node node)
           (values node &optional))
  (traverse-with-path
   node
   (list
    (action (:before node node path-thunk)
      (let ((path (funcall path-thunk)))
        (format t "PRE:  ~v@{|   ~}~A ~A~%" (length path)
                (class-name (class-of node))
                (class-name (class-of (car path)))))
      (values))
    (action (:after node node path-thunk)
      (let ((path (funcall path-thunk)))
        (format t "POST: ~v@{|   ~}~A ~A~%" (length path)
                (class-name (class-of node))
                (class-name (class-of (car path)))))
      (values)))))

(defun make-traverse-let-action-skipping-cons-bindings ()
  "This is an action to ensure that let-bindings to fully saturated
applications of `'coalton:Cons` are untouched by a traversal. The
current `codegen:codegen-let` code for recursive data requires direct
applications of a constructor, so transformations like inlining may
produce a form which the codegen is unable to recognize. Overriding
the default `:traverse node-let` action with this action will ensure
that let-bindings to saturated applications of `'coalton:Cons` will
always be returned with the syntactic form of a direct application
with rator `'coalton:Cons`."
  (declare (values action &optional))
  (load-time-value
   (action (:traverse node-let node &rest args)
     (make-node-let
      :type     (node-type node)
      :bindings (loop :for (name . subnode) :in (node-let-bindings node)
                      :collect (cons name
                                     (if (and (typep subnode '(or node-application
                                                               node-direct-application))
                                              (eq 'coalton:Cons (node-rator-name subnode))
                                              (= 2 (length (node-rands subnode))))
                                         (make-node-direct-application
                                          :type (node-type subnode)
                                          :rator-type (node-rator-type subnode)
                                          :rator 'coalton:Cons
                                          :rands (mapcar
                                                  (lambda (rand)
                                                    (apply *traverse* rand args))
                                                  (node-rands subnode)))
                                         (apply *traverse* subnode args))))
      :subexpr  (apply *traverse* (node-let-subexpr node) args)))
   t))
