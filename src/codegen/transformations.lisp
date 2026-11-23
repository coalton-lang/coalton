(defpackage #:coalton-impl/codegen/transformations
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast
   #:coalton-impl/codegen/traverse)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:rt #:coalton-impl/runtime)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:rename-type-variables
   #:prune-impossible-match-branches
   #:specialize-match
   #:open-known-constructor
   #:unwrap-known-constructor-branch
   #:pattern-contains-gadt-p
   #:inlined-some-operand
   #:inlined-cons-operands
   #:node-variables
   #:node-dynamic-variables
   #:node-mutated-variables
   #:node-free-p
   #:node-abstraction-bound-variables
   #:non-capturing-higher-order-rator-p
   #:local-functions-escape-in-node-p
   #:non-escaping-bind-function-p
   #:non-escaping-let-function-names))

(in-package #:coalton-impl/codegen/transformations)

(defun pattern-contains-gadt-p (pat env)
  (typecase pat
    (pattern-binding (pattern-contains-gadt-p (pattern-binding-pattern pat) env))
    (pattern-constructor
     (or (tc:constructor-entry-gadt-p (tc:lookup-constructor env (pattern-constructor-name pat)))
         (some (lambda (child) (pattern-contains-gadt-p child env))
               (pattern-constructor-patterns pat))))))

(defun specialized-pattern-substitutions (pattern scrutinee env &optional subs)
  ;; This is a feasibility test on copied, already well-typed code. A variable
  ;; in the scrutinee can still take any index; only inconsistent constructors
  ;; justify dropping a branch. Thread their equations through sibling fields.
  (declare (type pattern pattern)
           (type tc:ty scrutinee)
           (type tc:environment env)
           (type tc:substitution-list subs)
           (values tc:substitution-list boolean &optional))
  (labels ((walk (pat expected)
             (typecase pat
               (pattern-binding (walk (pattern-binding-pattern pat) expected))
               (pattern-constructor
                (let ((ctor (tc:qualified-ty-type
                             (tc:fresh-inst (tc:lookup-value-type env (pattern-constructor-name pat))))))
                  (multiple-value-bind (new-subs possible-p)
                      (tc:try-unify subs (tc:function-return-type ctor) expected :all)
                    (when possible-p
                      (setf subs new-subs)
                      (every #'walk (pattern-constructor-patterns pat) (tc:function-type-arguments ctor))))))
               (t t))))
    (let ((possible-p (walk pattern scrutinee)))
      (values subs possible-p))))

(defun specialized-branch-substitutions (branch scrutinee env subs)
  (declare (type match-branch branch)
           (type tc:ty scrutinee)
           (type tc:environment env)
           (type tc:substitution-list subs)
           (values tc:substitution-list boolean &optional))
  (loop :for (left right) :on (match-branch-index-equalities branch) :by #'cddr
        :do (multiple-value-bind (new-subs possible-p)
                (tc:try-unify subs left right :all)
              (unless possible-p
                (return-from specialized-branch-substitutions (values subs nil)))
              (setf subs new-subs)))
  (specialized-pattern-substitutions (match-branch-pattern branch) scrutinee env subs))

(defun specialize-match (match expr env)
  "Move the alternatives of MATCH to EXPR, retaining only feasible cases."
  (let* ((from (node-type (node-match-expr match)))
         (to (node-type expr))
         (moved (make-node-match :type (node-type match) :expr expr
                                 :branches (node-match-branches match))))
    ;; On this path EXPR supplies the original scrutinee. For example,
    ;; sinking into a GADT alternative can expose Integer where the parent
    ;; returned :A. Transport the patterns, their dictionaries and the body
    ;; together; the literal's type isn't evidence about its operand.
    (unless (tc:ty= from to)
      (setf moved (tc:apply-substitution (tc:unify-given nil from to :all) moved)))
    (if env (prune-impossible-match-branches moved env) moved)))

(defun open-known-constructor (pattern fields body env)
  "For a known constructor application, open PATTERN using its producer's
operand types. Return (VALUES SUBPATTERNS BODY SUCCESS BINDINGS). If physical
operand types don't identify the binders (e.g. a lowered forall), keep the match."
  (let ((openings (pattern-constructor-openings pattern))
        (children (pattern-constructor-patterns pattern)))
    (unless openings (return-from open-known-constructor (values children body t)))
    (unless env (return-from open-known-constructor (values nil nil nil)))
    (let* ((scheme (tc:lookup-value-type env (pattern-constructor-name pattern)))
           (args (tc:ty-scheme-instantiation-types scheme))
           (ctor (tc:fresh-inst scheme args))
           (subs (handler-case
                     (tc:match-list (mapcar #'node-type fields)
                                    (tc:function-type-arguments (tc:qualified-ty-type ctor)))
                   (tc:coalton-internal-type-error ()
                     (return-from open-known-constructor (values nil nil nil)))))
           (bindings (loop :for (index . var) :in openings
                           :for formal := (nth index args)
                           :for sub := (find formal subs :key #'tc:substitution-from :test #'tc:ty=)
                           :unless sub :do (return-from open-known-constructor (values nil nil nil))
                           :collect (tc:make-substitution :from var :to (tc:substitution-to sub)))))
      (values (tc:apply-substitution bindings children) (tc:apply-substitution bindings body) t bindings))))

(defun unwrap-known-constructor-branch (branch fields env)
  (multiple-value-bind (children body opened bindings)
      (open-known-constructor (match-branch-pattern branch) fields (match-branch-body branch) env)
    (when opened
      (make-match-branch :index-equalities (tc:apply-substitution bindings (match-branch-index-equalities branch))
                         :pattern (first children) :body body))))

(defun prune-impossible-match-branches (node env)
  "Remove GADT branches ruled out by specializing a copy of NODE."
  (traverse
   node
   (list
    (action (:traverse node-match match subs)
      (let ((expr (funcall *traverse* (node-match-expr match) subs)))
        (make-node-match
         :type (node-type match)
         :expr expr
         :branches
         (loop :for branch :in (node-match-branches match)
               :append
               (multiple-value-bind (branch-subs possible-p)
                   (specialized-branch-substitutions branch (node-type expr) env subs)
                 (when possible-p
                   (list (make-match-branch
                          :pattern (map-pattern
                                    (match-branch-pattern branch)
                                    :test-function (lambda (test) (funcall *traverse* test branch-subs)))
                          :index-equalities (match-branch-index-equalities branch)
                          :body (funcall *traverse* (match-branch-body branch) branch-subs))))))))))
   nil))

(defmethod tc:apply-substitution (subs (node node))
  "Substitute type variables in the tree of `node` with other types
specified in `subs`."
  (declare (type tc:substitution-list subs)
           (values node &optional))
  (substitute-node-types node subs #'identity))

(defun substitute-node-types (node subs binder-function)
  ;; Only an alpha-renaming rewrites a constructor's existential binders.
  (traverse
   node
   (list
    (action (:after node node)
      (copy-node node
                 (tc:apply-substitution subs (node-type node))))
    (action (:after node-direct-application node)
      (make-node-direct-application
       :type (node-type node)
       :properties (node-properties node)
       :rator-type (tc:apply-substitution subs (node-direct-application-rator-type node))
       :rator (node-direct-application-rator node)
       :rands (node-direct-application-rands node)
       :keyword-rands (node-direct-application-keyword-rands node)))
    (action (:after node-match node)
      (make-node-match
       :type (node-type node)
       :expr (node-match-expr node)
       :branches (mapcar
                  (lambda (branch)
                    (make-match-branch
                     :index-equalities (tc:apply-substitution subs (match-branch-index-equalities branch))
                     ;; Embedded tests have already been visited by TRAVERSE.
                     :pattern (map-pattern (match-branch-pattern branch)
                                           :binder-function binder-function
                                           :type-function (lambda (type) (tc:apply-substitution subs type)))
                     :body (match-branch-body branch)))
                  (node-match-branches node)))))))

(defmethod tc:type-variables ((node node))
  "Collect all type variables from nodes and patterns in the tree of `node`."
  (declare (values tc:tyvar-list &optional))
  (let ((tyvars nil))
    (traverse
     node
     (list
      (action (:after node node)
        (alexandria:unionf tyvars
                           (tc:type-variables (node-type node)))
        (values))
      (action (:after node-direct-application node)
        (alexandria:unionf tyvars
                           (tc:type-variables (node-direct-application-rator-type node)))
        (values))
      (action (:after node-match node)
        (dolist (branch (node-match-branches node))
          (alexandria:unionf tyvars
                             (tc:type-variables (match-branch-pattern branch)))
          (alexandria:unionf tyvars
                             (tc:type-variables (match-branch-index-equalities branch))))
        (values))))
    tyvars))

(defun rename-type-variables (node &optional (renamer #'tc:fresh-type-renamer))
  "Rename the type variables of `node` and its subnodes according to the
function `renamer`, returning the new node tree."
  (declare (type node     node)
           (type function renamer)
           (values node &optional))
  (alexandria:if-let ((old-type-variables (tc:type-variables node)))
    (let ((subs (loop :for var :in old-type-variables
                      :collect (tc:make-substitution :from var :to (funcall renamer var)))))
      (substitute-node-types node subs (lambda (var) (tc:apply-substitution subs var))))
    node))

(defun inlined-some-operand (node)
  "If NODE is an inlined `coalton:Some` wrapper, return its argument variable node."
  (declare (type node node)
           (values (or null node-local-variable) &optional))
  (when (and (node-lisp-p node)
             (= 1 (length (node-lisp-vars node)))
             (= 1 (length (node-lisp-form node)))
             (tc:tapp-p (node-type node))
             (tc:tycon-p (tc:tapp-from (node-type node)))
             (eq 'coalton:Optional (tc:tycon-name (tc:tapp-from (node-type node)))))
    (let ((binding (first (node-lisp-vars node)))
          (form (first (node-lisp-form node))))
      (when (and (consp form)
                 (eq 'rt:cl-some (first form))
                 (consp (cdr form))
                 (null (cddr form))
                 (eq (car binding) (second form)))
        (make-node-local-variable
         :type (tc:tapp-to (node-type node))
         :value (cdr binding))))))

(defun inlined-cons-operands (node)
  "If NODE is an inlined `coalton:Cons` wrapper, return its two argument variable nodes."
  (declare (type node node)
           (values list boolean &optional))
  (when (and (node-lisp-p node)
             (= 2 (length (node-lisp-vars node)))
             (= 1 (length (node-lisp-form node)))
             (tc:tapp-p (node-type node))
             (tc:tycon-p (tc:tapp-from (node-type node)))
             (eq 'coalton:List (tc:tycon-name (tc:tapp-from (node-type node)))))
    (let ((car-binding (first (node-lisp-vars node)))
          (cdr-binding (second (node-lisp-vars node)))
          (form (first (node-lisp-form node))))
      (when (and (consp form)
                 (eq 'cl:cons (first form))
                 (consp (cdr form))
                 (consp (cddr form))
                 (null (cdddr form))
                 (eq (car car-binding) (second form))
                 (eq (car cdr-binding) (third form)))
        (values
         (list (make-node-local-variable
                :type (tc:tapp-to (node-type node))
                :value (cdr car-binding))
               (make-node-local-variable
                :type (node-type node)
                :value (cdr cdr-binding)))
         t)))))

(defun node-variables (node &key (variable-namespace-only nil))
  "Return a deduplicated list of identifiers representing variables in
both CL namespaces appearing in `node`"
  (declare (type node    node)
           (type boolean variable-namespace-only)
           (values parser:identifier-list &optional))
  (let ((node-vars nil))
    (flet ((collect-variable (node)
             (setf node-vars
                   (adjoin (node-variable-value node) node-vars))
             (values)))
      (traverse
       node
       (list
        (action (:after node-local-variable) #'collect-variable)
        (action (:after node-global-variable) #'collect-variable)
        (action (:after node-dynamic-variable) #'collect-variable)
        (action (:after node-setq node)
          (setf node-vars
                (adjoin (node-setq-name node) node-vars))
          (values))
        (action (:after node-direct-application node)
          (unless variable-namespace-only
            (setf node-vars
                  (adjoin (node-direct-application-rator node) node-vars)))
          (values))
        (action (:after node-lisp node)
          (alexandria:unionf node-vars
                             (mapcar #'cdr (node-lisp-vars node)))
          (values)))))
    node-vars))

(defun node-dynamic-variables (node)
  "Return a deduplicated list of dynamically scoped variables read by NODE."
  (declare (type node node)
           (values parser:identifier-list &optional))
  (let ((dynamic-vars nil))
    (traverse
     node
     (list
      (action (:after node-dynamic-variable node)
        (setf dynamic-vars
              (adjoin (node-variable-value node) dynamic-vars))
        (values))))
    dynamic-vars))

(defun node-mutated-variables (node)
  "Return lexical identifiers assigned anywhere in NODE, including nested
functions. A SETQ or a non-identity loop step makes a binding mutable even if
its initializer is constant."
  (declare (type node node)
           (values parser:identifier-list &optional))
  (let ((mutated nil))
    (traverse
     node
     (list
      (action (:before node-for node)
        (dolist (binding (node-for-bindings node))
          (let ((step (node-for-binding-step binding)))
            (when (and step
                       (not (and (node-local-variable-p step)
                                 (eq (node-for-binding-name binding)
                                     (node-variable-value step)))))
              (pushnew (node-for-binding-name binding) mutated :test #'eq))))
        (values))
      (action (:before node-setq node)
        (pushnew (node-setq-name node) mutated :test #'eq)
        (values))))
    mutated))

(defun node-free-p (node bound-variables)
  "Return true if every variable in `node` is free with respect to
`bound-variables`"
  (declare (type node                   node)
           (type parser:identifier-list bound-variables)
           (values boolean &optional))
  (null (intersection (node-variables node) bound-variables)))

(defun node-abstraction-bound-variables (node)
  "Return all parameter and supplied-p identifiers bound by NODE."
  (declare (type node-abstraction node)
           (values parser:identifier-list &optional))
  (append
   (node-abstraction-vars node)
   (loop :for param :in (node-abstraction-keyword-params node)
         :append (list (keyword-param-var param)
                       (keyword-param-supplied-p-var param)))))

(defun non-capturing-higher-order-rator-p (name)
  "Return true when NAME is a primitive higher-order operator that calls its
first argument immediately without capturing it."
  (declare (type (or null symbol) name)
           (values boolean &optional))
  (alexandria:when-let ((pkg (and name (find-package "COALTON/CELL"))))
    (or (eq name (find-symbol "UPDATE!" pkg))
        (eq name (find-symbol "UPDATE-SWAP!" pkg)))))

(defvar *non-escaping-local-functions* nil
  "Local function identifiers already proven not to escape in the active traversal.")

(defun local-functions-escape-in-node-p (fn-names node
                                         &optional known-non-escaping-fns
                                                   initial-bound-variables)
  "Return true if any function in FN-NAMES escapes in NODE."
  (declare (type parser:identifier-list fn-names known-non-escaping-fns initial-bound-variables)
           (type node node)
           (values boolean &optional))
  (when (null fn-names)
    (return-from local-functions-escape-in-node-p nil))
  (let ((escaped? nil)
        (*non-escaping-local-functions* known-non-escaping-fns)
        (start-bound (set-difference initial-bound-variables fn-names :test #'eq)))
    (labels ((tracked-variable-p (subnode bound-variables)
               (and (node-local-variable-p subnode)
                    (member (node-variable-value subnode) fn-names :test #'eq)
                    (not (member (node-variable-value subnode)
                                 bound-variables
                                 :test #'eq))))
             (mark-escaping-variables (subnode bound-variables)
               (when (intersection fn-names
                                   (set-difference (node-variables subnode)
                                                   bound-variables
                                                   :test #'eq)
                                   :test #'eq)
                 (setf escaped? t)))
             (traverse-keyword-rands (keyword-rands bound-variables)
               (loop :for rand :in keyword-rands
                     :do (funcall *traverse*
                                  (node-application-keyword-arg-value rand)
                                  bound-variables)
                         (alexandria:when-let ((supplied-p (node-application-keyword-arg-supplied-p rand)))
                           (funcall *traverse* supplied-p bound-variables))))
             (traverse-call-rands (rator-name rands keyword-rands bound-variables)
               (cond
                 ((and (non-capturing-higher-order-rator-p rator-name)
                       (null keyword-rands)
                       (= 2 (length rands)))
                  (let ((fn-arg (first rands)))
                    (cond
                      ((tracked-variable-p fn-arg bound-variables)
                       nil)
                      ((node-abstraction-p fn-arg)
                       (funcall *traverse*
                                (node-abstraction-subexpr fn-arg)
                                (append (node-abstraction-bound-variables fn-arg)
                                        bound-variables)))
                      (t
                       (funcall *traverse* fn-arg bound-variables))))
                  (funcall *traverse* (second rands) bound-variables))
                 (t
                  (loop :for rand :in rands
                        :do (funcall *traverse* rand bound-variables))))
               (traverse-keyword-rands keyword-rands bound-variables)))
      (traverse
       node
       (append
        (list
         (action (:after node-local-variable subnode bound-variables)
           (when (tracked-variable-p subnode bound-variables)
             (setf escaped? t))
           (values))
         (action (:after node-lisp subnode bound-variables)
           (loop :for (_ . name) :in (node-lisp-vars subnode)
                 :when (and (member name fn-names :test #'eq)
                            (not (member name bound-variables :test #'eq)))
                   :do (setf escaped? t))
           (values))
         (action (:after node-setq subnode bound-variables)
           (when (and (member (node-setq-name subnode) fn-names :test #'eq)
                      (not (member (node-setq-name subnode) bound-variables :test #'eq)))
             (setf escaped? t))
           (values))
         (action (:traverse node-direct-application subnode bound-variables)
           (traverse-call-rands (node-direct-application-rator subnode)
                                (node-direct-application-rands subnode)
                                (node-direct-application-keyword-rands subnode)
                                bound-variables)
           subnode)
         (action (:traverse node-application subnode bound-variables)
           (let ((rator (node-application-rator subnode)))
             (cond
               ((tracked-variable-p rator bound-variables)
                nil)
               ((node-abstraction-p rator)
                (funcall *traverse*
                         (node-abstraction-subexpr rator)
                         (append (node-abstraction-bound-variables rator)
                                 bound-variables)))
               (t
                (funcall *traverse* rator bound-variables))))
           (traverse-call-rands (node-rator-name subnode)
                                (node-application-rands subnode)
                                (node-application-keyword-rands subnode)
                                bound-variables)
           subnode)
         (action (:traverse node-bind subnode bound-variables)
           (let* ((bind-name (node-bind-name subnode))
                  (bind-expr (node-bind-expr subnode))
                  (bind-body (node-bind-body subnode))
                  (new-bound (cons bind-name bound-variables)))
             (cond
               ((and (node-abstraction-p bind-expr)
                     (or (member bind-name *non-escaping-local-functions* :test #'eq)
                         (non-escaping-bind-function-p
                          subnode
                          *non-escaping-local-functions*
                          bound-variables)))
                (let ((*non-escaping-local-functions*
                        (adjoin bind-name *non-escaping-local-functions* :test #'eq)))
                  (funcall *traverse*
                           (node-abstraction-subexpr bind-expr)
                           (append (node-abstraction-bound-variables bind-expr)
                                   new-bound))
                  (funcall *traverse* bind-body new-bound)))
               (t
                (funcall *traverse* bind-expr new-bound)
                (funcall *traverse* bind-body new-bound)))
             subnode))
         (action (:traverse node-let subnode bound-variables)
           (let* ((new-bound (append (mapcar #'car (node-let-bindings subnode))
                                     bound-variables))
                  (let-non-escaping (non-escaping-let-function-names
                                     subnode
                                     *non-escaping-local-functions*
                                     bound-variables))
                  (*non-escaping-local-functions*
                    (append let-non-escaping *non-escaping-local-functions*)))
             (loop :for (b-name . b-node) :in (node-let-bindings subnode)
                   :do (if (and (node-abstraction-p b-node)
                                (member b-name *non-escaping-local-functions* :test #'eq))
                           (funcall *traverse*
                                    (node-abstraction-subexpr b-node)
                                    (append (node-abstraction-bound-variables b-node)
                                            new-bound))
                           (funcall *traverse* b-node new-bound)))
             (funcall *traverse* (node-let-subexpr subnode) new-bound)
             subnode))
         (action (:traverse node-abstraction subnode bound-variables)
           ;; Any remaining node-abstraction (anonymous lambda or escaping
           ;; local function) may outlive the defining scope.
           (mark-escaping-variables
            (node-abstraction-subexpr subnode)
            (append (node-abstraction-bound-variables subnode)
                    bound-variables))
           subnode))
        (coalton-impl/codegen/traverse::make-binding-list-actions))
       start-bound))
    escaped?))

(defun non-escaping-bind-function-p (node &optional known-non-escaping-fns initial-bound-variables)
  "Return true when NODE binds a local function that does not escape its body."
  (declare (type node-bind node)
           (type parser:identifier-list known-non-escaping-fns initial-bound-variables)
           (values boolean &optional))
  (let ((name (node-bind-name node))
        (expr (node-bind-expr node)))
    (and (node-abstraction-p expr)
         (not (local-functions-escape-in-node-p
               (list name)
               (node-bind-body node)
               known-non-escaping-fns
               initial-bound-variables)))))

(defun non-escaping-let-function-names (node &optional known-non-escaping-fns initial-bound-variables)
  "Return the function binding names in NODE that do not escape NODE."
  (declare (type node-let node)
           (type parser:identifier-list known-non-escaping-fns initial-bound-variables)
           (values parser:identifier-list &optional))
  (let* ((bindings (node-let-bindings node))
         (sccs (node-binding-sccs bindings))
         (bound-without-let
           (remove-if (lambda (var)
                        (assoc var bindings :test #'eq))
                      initial-bound-variables))
         (non-escaping nil))
    (dolist (scc (reverse sccs))
      (let ((scc-bindings
              (remove-if-not (lambda (binding)
                               (member (car binding) scc :test #'eq))
                             bindings)))
        (when (and scc-bindings
                   (every #'node-abstraction-p (mapcar #'cdr scc-bindings)))
          (let* ((known (append non-escaping known-non-escaping-fns))
                 (other-bindings
                   (remove-if (lambda (binding)
                                (member (car binding) scc :test #'eq))
                              bindings))
                 (scc-bound
                   (append (loop :for (name . _) :in bindings
                                 :unless (member name scc :test #'eq)
                                   :collect name)
                           bound-without-let))
                 (escaped-p
                   (or (loop :for (_ . fn-node) :in scc-bindings
                             :thereis (local-functions-escape-in-node-p
                                       scc
                                       (node-abstraction-subexpr fn-node)
                                       known
                                       (append (node-abstraction-bound-variables fn-node)
                                               scc-bound)))
                       (loop :for (other-name . other-node) :in other-bindings
                             :thereis (if (and (node-abstraction-p other-node)
                                               (member other-name known :test #'eq))
                                          (local-functions-escape-in-node-p
                                           scc
                                           (node-abstraction-subexpr other-node)
                                           known
                                           (append (node-abstraction-bound-variables other-node)
                                                   scc-bound))
                                          (local-functions-escape-in-node-p
                                           scc
                                           other-node
                                           known
                                           scc-bound)))
                       (local-functions-escape-in-node-p
                        scc
                        (node-let-subexpr node)
                        known
                        scc-bound))))
            (unless escaped-p
              (setf non-escaping (append scc non-escaping)))))))
    non-escaping))
