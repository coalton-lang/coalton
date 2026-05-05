(defpackage #:coalton-impl/codegen/lawnmower
  (:documentation
   "The lawnmower removes administrative AST forms left behind by earlier
optimization passes. It performs small, structural rewrites such as eliminating
identity LETs and pushing immediate applications through expression wrappers so
later passes and Lisp compilers see simpler code.")
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/transformations
   #:node-variables)
  (:import-from
   #:coalton-impl/codegen/traverse
   #:action
   #:traverse
   #:traverse-with-binding-list)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:lawnmow))

(in-package #:coalton-impl/codegen/lawnmower)

(defun identifiers-disjoint-p (left right)
  (declare (type parser:identifier-list left right)
           (values boolean &optional))
  (null (intersection left right :test #'eq)))

(defun keyword-arg-variables (arg)
  (declare (type node-application-keyword-arg arg)
           (values parser:identifier-list &optional))
  (append
   (node-variables (node-application-keyword-arg-value arg))
   (alexandria:when-let ((supplied-p (node-application-keyword-arg-supplied-p arg)))
     (node-variables supplied-p))))

(defun application-argument-variables (node)
  (declare (type (or node-application node-direct-application) node)
           (values parser:identifier-list &optional))
  (remove-duplicates
   (append
    (mapcan #'node-variables
            (copy-list (node-rands node)))
    (mapcan #'keyword-arg-variables
            (copy-list (keyword-rands node))))
   :test #'eq))

(defun application-with-rator (node rator)
  (declare (type (or node-application node-direct-application) node)
           (type node rator)
           (values node-application &optional))
  (make-node-application
   :type (node-type node)
   :properties (node-properties node)
   :rator rator
   :rands (node-rands node)
   :keyword-rands (keyword-rands node)))

(defun let-bound-names (node)
  (declare (type node-let node)
           (values parser:identifier-list &optional))
  (mapcar #'car (node-let-bindings node)))

(defun keyword-rands (node)
  (declare (type (or node-application node-direct-application) node)
           (values keyword-arg-list &optional))
  (etypecase node
    (node-application
     (node-application-keyword-rands node))
    (node-direct-application
     (node-direct-application-keyword-rands node))))

(defun application-can-enter-let-p (application let-node)
  "Return true when ((let BINDINGS F) ARGS...) can become (let BINDINGS (F ARGS...))."
  (declare (type node-application application)
           (type node-let let-node)
           (values boolean &optional))
  (identifiers-disjoint-p (let-bound-names let-node)
                          (application-argument-variables application)))

(defun application-can-enter-match-p (application match-node)
  "Return true when ((match EXPR BRANCHES...) ARGS...) can push into BRANCHES."
  (declare (type node-application application)
           (type node-match match-node)
           (values boolean &optional))
  (let ((argument-vars (application-argument-variables application)))
    (every (lambda (branch)
             (identifiers-disjoint-p
              (pattern-variables (match-branch-pattern branch))
              argument-vars))
           (node-match-branches match-node))))

(defun match-branch-free-variables (branch)
  (declare (type match-branch branch)
           (values parser:identifier-list &optional))
  (set-difference (node-variables (match-branch-body branch))
                  (pattern-variables (match-branch-pattern branch))
                  :test #'eq))

(defun match-free-variables (node)
  (declare (type node-match node)
           (values parser:identifier-list &optional))
  (remove-duplicates
   (mapcan #'match-branch-free-variables
           (copy-list (node-match-branches node)))
   :test #'eq))

(defun match-scope-variables (node)
  "Return variables whose scope could collide if MATCH moves under a binder."
  (declare (type node-match node)
           (values parser:identifier-list &optional))
  (remove-duplicates
   (append
    (match-free-variables node)
    (mapcan (lambda (branch)
              (pattern-variables (match-branch-pattern branch)))
            (copy-list (node-match-branches node))))
   :test #'eq))

(defun match-with-expr (node expr)
  (declare (type node-match node)
           (type node expr)
           (values node-match &optional))
  (make-node-match
   :type (node-type node)
   :expr expr
   :branches (node-match-branches node)))

(defun match-can-enter-let-p (match let-node)
  "Return true when (match (let BINDINGS X) BRANCHES...) can become
(let BINDINGS (match X BRANCHES...))."
  (declare (type node-match match)
           (type node-let let-node)
           (values boolean &optional))
  (identifiers-disjoint-p (let-bound-names let-node)
                          (match-scope-variables match)))

(defun match-can-enter-match-p (outer-match inner-match)
  "Return true when (match (match X BRANCHES...) OUTER-BRANCHES...) can
push OUTER-BRANCHES into the inner match branches."
  (declare (type node-match outer-match inner-match)
           (values boolean &optional))
  (let ((outer-scope-vars (match-scope-variables outer-match)))
    (every (lambda (branch)
             (identifiers-disjoint-p
              (pattern-variables (match-branch-pattern branch))
              outer-scope-vars))
           (node-match-branches inner-match))))

(defun alias-binding-target (binding)
  "Return the target variable node for a LET binding of the form (ALIAS TARGET)."
  (declare (type cons binding)
           (values (or null node-variable) &optional))
  (let ((name (car binding))
        (expr (cdr binding)))
    (when (and (node-variable-p expr)
               (not (eq name (node-variable-value expr))))
      expr)))

(defun resolve-alias-target (name aliases)
  "Resolve NAME through ALIASES, returning NIL for cyclic alias chains."
  (declare (type parser:identifier name)
           (type list aliases)
           (values (or null node-variable) &optional))
  (labels ((resolve (name seen)
             (let ((target (cdr (assoc name aliases :test #'eq))))
               (when target
                 (let ((target-name (node-variable-value target)))
                   (cond
                     ((member target-name seen :test #'eq)
                      nil)
                     ((assoc target-name aliases :test #'eq)
                      (resolve target-name (cons target-name seen)))
                     (t
                      target)))))))
    (resolve name (list name))))

(defun let-alias-substitutions (bindings)
  "Return resolvable variable alias substitutions from BINDINGS."
  (declare (type binding-list bindings)
           (values list &optional))
  (let ((aliases
          (loop :for binding :in bindings
                :for target := (alias-binding-target binding)
                :when target
                  :collect (cons (car binding) target))))
    (loop :for alias :in aliases
          :for name := (car alias)
          :for target := (resolve-alias-target name aliases)
          :when target
            :collect (cons name target))))

(defun alias-substitution-target (name aliases)
  (declare (type parser:identifier name)
           (type list aliases)
           (values (or null node-variable) &optional))
  (cdr (assoc name aliases :test #'eq)))

(defun substitute-aliases (node aliases)
  "Replace references to variable aliases in NODE.

The replacement is skipped inside nested binders for the same name."
  (declare (type node node)
           (type list aliases)
           (values node &optional))
  (traverse-with-binding-list
   node
   (list
    (action (:after node-variable node bound-variables)
      (let ((name (node-variable-value node)))
        (unless (member name bound-variables :test #'eq)
          (alexandria:when-let ((target (alias-substitution-target name aliases)))
            (copy-node target (node-type node))))))
    (action (:after node-lisp node bound-variables)
      (let ((new-vars
              (loop :for (lisp-var . coalton-var) :in (node-lisp-vars node)
                    :for target := (and (not (member coalton-var bound-variables :test #'eq))
                                        (alias-substitution-target coalton-var aliases))
                    :collect (cons lisp-var
                                   (if target
                                       (node-variable-value target)
                                       coalton-var)))))
        (unless (equal new-vars (node-lisp-vars node))
          (make-node-lisp
           :type (node-type node)
           :vars new-vars
           :form (node-lisp-form node))))))))

(defun alias-bind-safe-p (name target body)
  (declare (type parser:identifier name target)
           (type node body)
           (values boolean &optional))
  (let ((unsafe? nil))
    (traverse-with-binding-list
     body
     (list
      (action (:after node-variable node bound-variables)
        (when (and (eq name (node-variable-value node))
                   (not (member name bound-variables :test #'eq))
                   (member target bound-variables :test #'eq))
          (setf unsafe? t))
        (values))
      (action (:after node-lisp node bound-variables)
        (when (and (member target bound-variables :test #'eq)
                   (loop :for (_ . coalton-var) :in (node-lisp-vars node)
                         :thereis (and (eq name coalton-var)
                                       (not (member name bound-variables :test #'eq)))))
          (setf unsafe? t))
        (values))))
    (not unsafe?)))

(defun wrap-node-bindings (bindings body)
  (declare (type binding-list bindings)
           (type node body)
           (values node &optional))
  (loop :with out := body
        :for (name . expr) :in (reverse bindings) :do
          (setf out
                (make-node-bind
                 :type (node-type body)
                 :name name
                 :expr expr
                 :body out))
        :finally (return out)))

(defun let-node-with-bindings (node bindings subexpr)
  (declare (type node-let node)
           (type binding-list bindings)
           (type node subexpr)
           (values node &optional))
  (if bindings
      (make-node-let
       :type (node-type node)
       :bindings bindings
       :subexpr subexpr)
      (copy-node subexpr (node-type node))))

(defun try-node-rewrites (node rewrites)
  (declare (type node node)
           (type list rewrites)
           (values node boolean &optional))
  (dolist (rewrite rewrites (values node nil))
    (multiple-value-bind (new-node changed?) (funcall rewrite node)
      (when changed?
        (return (values new-node t))))))

(defun single-value-node-p (node)
  (declare (type node node)
           (values boolean &optional))
  (= 1 (tc:multiple-value-output-arity (node-type node))))

(defun single-value-function-p (node)
  (declare (type node node)
           (values boolean &optional))
  (= 1 (tc:multiple-value-output-arity
        (tc:function-return-type (node-type node)))))

(defun abstraction-application-inlineable-p (application abstraction)
  (declare (type node-application application)
           (type node-abstraction abstraction)
           (values boolean &optional))
  (and (null (node-abstraction-keyword-params abstraction))
       (null (node-application-keyword-rands application))
       (single-value-node-p application)
       (= (length (node-abstraction-vars abstraction))
          (length (node-application-rands application)))
       (identifiers-disjoint-p (node-abstraction-vars abstraction)
                               (application-argument-variables application))))

(defun inline-abstraction-application (application abstraction)
  (declare (type node-application application)
           (type node-abstraction abstraction)
           (values node &optional))
  ;; ((fn (x...) body) args...) -> (let ((x arg)...) body)
  (wrap-node-bindings
   (mapcar #'cons
           (node-abstraction-vars abstraction)
           (node-application-rands application))
   (copy-node (node-abstraction-subexpr abstraction)
              (node-type application))))

(defun node-known-application (node)
  (declare (type node node)
           (values (or null parser:identifier) node-list boolean &optional))
  (typecase node
    ((or node-application node-direct-application)
     (let ((name (node-rator-name node)))
       (if (and name (null (keyword-rands node)))
           (values name (node-rands node) t)
           (values nil nil nil))))
    (t
     (values nil nil nil))))

(defun literal-values-match-p (pattern-value node-value)
  (declare (values boolean &optional))
  (if (and (stringp pattern-value)
           (stringp node-value))
      (string= pattern-value node-value)
      (eql pattern-value node-value)))

(defun node-seq-2 (first second)
  (declare (type node first second)
           (values node-seq &optional))
  (make-node-seq
   :type (node-type second)
   :nodes (list first second)))

(defun force-value-binding-node (node)
  (declare (type node node)
           (values node &optional))
  (if (node-abstraction-p node)
      (make-node-locally
       :type (node-type node)
       :noinline-functions nil
       :type-check nil
       :subexpr node)
      node))

(defun variable-substitution-info (body name value)
  (declare (type node body value)
           (type parser:identifier name)
           (values fixnum boolean &optional))
  (let ((count 0)
        (unsafe? nil)
        (value-vars (node-variables value)))
    (traverse-with-binding-list
     body
     (list
      (action (:after node-variable node bound-variables)
        (when (and (eq name (node-variable-value node))
                   (not (member name bound-variables :test #'eq)))
          (incf count)
          (unless (identifiers-disjoint-p value-vars bound-variables)
            (setf unsafe? t)))
        (values))
      (action (:after node-lisp node bound-variables)
        (when (loop :for (_ . coalton-var) :in (node-lisp-vars node)
                    :thereis (and (eq name coalton-var)
                                  (not (member name bound-variables :test #'eq))))
          (setf unsafe? t))
        (values))))
    (values count unsafe?)))

(defun substitute-variable-node (body name value)
  (declare (type node body value)
           (type parser:identifier name)
           (values node &optional))
  (traverse-with-binding-list
   body
   (list
    (action (:after node-variable node bound-variables)
      (when (and (eq name (node-variable-value node))
                 (not (member name bound-variables :test #'eq)))
        (copy-node value (node-type node)))))))

(defun bind-known-pattern-variable (name value body)
  (declare (type parser:identifier name)
           (type node value body)
           (values node &optional))
  (multiple-value-bind (count unsafe?) (variable-substitution-info body name value)
    (if (and (= 1 count) (not unsafe?))
        (substitute-variable-node body name value)
        (make-node-bind
         :type (node-type body)
         :name name
         :expr (force-value-binding-node value)
         :body body))))

(defun rewrite-known-pattern-match (pattern value body)
  "Rewrite a match of known VALUE against PATTERN.

Returns a status keyword and a replacement body. Status is one of:

  :MATCH    PATTERN is known to match VALUE, and the replacement body is valid.
  :NO-MATCH PATTERN is known not to match VALUE.
  :UNKNOWN  The result depends on runtime tests."
  (declare (type pattern pattern)
           (type node value body)
           (values keyword (or null node) &optional))
  (typecase pattern
    (pattern-var
     (values ':match
             (bind-known-pattern-variable
              (pattern-var-name pattern)
              value
              body)))

    (pattern-wildcard
     (values ':match (node-seq-2 value body)))

    (pattern-literal
     (if (and (node-literal-p value)
              (literal-values-match-p (pattern-literal-value pattern)
                                      (node-literal-value value)))
         (values ':match body)
         (values ':unknown nil)))

    (pattern-constructor
     (multiple-value-bind (name rands known?) (node-known-application value)
       (cond
         ((not known?)
          (values ':unknown nil))

         ((not (eq name (pattern-constructor-name pattern)))
          (values ':unknown nil))

         ((/= (length rands)
              (length (pattern-constructor-patterns pattern)))
          (values ':unknown nil))

         (t
          (loop :with out := body
                :for subpattern :in (reverse (pattern-constructor-patterns pattern))
                :for rand :in (reverse rands) :do
                  (multiple-value-bind (status new-out)
                      (rewrite-known-pattern-match subpattern rand out)
                    (ecase status
                      (:match
                       (setf out new-out))
                      (:no-match
                       (return-from rewrite-known-pattern-match
                         (values ':no-match nil)))
                      (:unknown
                       (return-from rewrite-known-pattern-match
                         (values ':unknown nil)))))
                :finally (return (values ':match out)))))))

    (pattern-binding
     (values ':unknown nil))

    (t
     (values ':unknown nil))))

(defun maybe-reduce-known-match (node)
  (declare (type node-match node)
           (values node boolean &optional))
  (let ((expr (node-match-expr node)))
    (loop :for branch :in (node-match-branches node) :do
      (multiple-value-bind (status body)
          (rewrite-known-pattern-match (match-branch-pattern branch)
                                       expr
                                       (match-branch-body branch))
        (ecase status
          (:match
           (return-from maybe-reduce-known-match
             (values (copy-node body (node-type node)) t)))
          (:no-match
           nil)
          (:unknown
           (return-from maybe-reduce-known-match
             (values node nil))))))
    (values node nil)))

(defun maybe-sink-match-into-expr (node)
  (declare (type node-match node)
           (values node boolean &optional))
  (let ((expr (node-match-expr node)))
    (typecase expr
      (node-application
       (let ((rator (node-application-rator expr)))
         (if (and (node-abstraction-p rator)
                  (abstraction-application-inlineable-p expr rator)
                  (identifiers-disjoint-p (node-abstraction-vars rator)
                                          (match-free-variables node)))
             ;; (match ((fn (x...) body) args...) branches...)
             ;; -> (let ((x arg)...) (match body branches...))
             (values
              (wrap-node-bindings
               (mapcar #'cons
                       (node-abstraction-vars rator)
                       (node-application-rands expr))
               (match-with-expr node (node-abstraction-subexpr rator)))
              t)
             (values node nil))))

      (node-let
       ;; (match (let bindings x) branches...) -> (let bindings (match x branches...))
       (if (match-can-enter-let-p node expr)
           (values
            (make-node-let
             :type (node-type node)
             :bindings (node-let-bindings expr)
             :subexpr (match-with-expr node (node-let-subexpr expr)))
            t)
           (values node nil)))

      (node-match
       ;; (match (match x (p y)...) branches...)
       ;; -> (match x (p (match y branches...))...)
       (if (match-can-enter-match-p node expr)
           (values
            (make-node-match
             :type (node-type node)
             :expr (node-match-expr expr)
             :branches
             (loop :for branch :in (node-match-branches expr)
                   :collect
                   (make-match-branch
                    :pattern (match-branch-pattern branch)
                    :body (match-with-expr node (match-branch-body branch)))))
            t)
           (values node nil)))

      (node-bind
       ;; (match (bind x e y) branches...) -> (bind x e (match y branches...))
       (if (not (member (node-bind-name expr) (match-free-variables node) :test #'eq))
           (values
            (make-node-bind
             :type (node-type node)
             :name (node-bind-name expr)
             :expr (node-bind-expr expr)
             :body (match-with-expr node (node-bind-body expr)))
            t)
           (values node nil)))

      (node-seq
       ;; (match (progn e... x) branches...) -> (progn e... (match x branches...))
       (let ((nodes (node-seq-nodes expr)))
         (if (endp nodes)
             (values node nil)
             (values
              (make-node-seq
               :type (node-type node)
               :nodes (append (butlast nodes)
                              (list (match-with-expr node (car (last nodes))))))
              t))))

      (node-locally
       ;; (match (locally decls x) branches...) -> (locally decls (match x branches...))
       (if (null (node-locally-noinline-functions expr))
           (values
            (make-node-locally
             :type (node-type node)
             :noinline-functions (node-locally-noinline-functions expr)
             :type-check (node-locally-type-check expr)
             :subexpr (match-with-expr node (node-locally-subexpr expr)))
            t)
           (values node nil)))

      (t
       (values node nil)))))

(defun maybe-simplify-match (node)
  (declare (type node-match node)
           (values node boolean &optional))
  (try-node-rewrites
   node
   '(maybe-reduce-known-match
     maybe-sink-match-into-expr)))

(defun let-bound-match-scrutinee-p (name subexpr)
  (declare (type parser:identifier name)
           (type node subexpr)
           (values (or null node-match) &optional))
  (when (and (typep subexpr 'node-match)
             (node-variable-p (node-match-expr subexpr))
             (eq name (node-variable-value (node-match-expr subexpr)))
             (not (member name (match-free-variables subexpr) :test #'eq)))
    subexpr))

(defun binding-name-used-in-bindings-p (name bindings)
  (declare (type parser:identifier name)
           (type binding-list bindings)
           (values boolean &optional))
  (loop :for (_ . expr) :in bindings
        :thereis (member name (node-variables expr) :test #'eq)))

(defun node-name-occurrence-count (node name)
  (declare (type node node)
           (type parser:identifier name)
           (values (integer 0) &optional))
  (let ((count 0))
    (traverse
     node
     (list
      (action (:after node-variable node)
        (when (eq name (node-variable-value node))
          (incf count))
        (values))
      (action (:after node-direct-application node)
        (when (eq name (node-direct-application-rator node))
          (incf count))
        (values))))
    count))

(defun direct-application-occurrence-count (node name)
  (declare (type node node)
           (type parser:identifier name)
           (values (integer 0) &optional))
  (let ((count 0))
    (traverse
     node
     (list
      (action (:after node-application node)
        (when (eq name (node-rator-name node))
          (incf count))
        (values))
      (action (:after node-direct-application node)
        (when (eq name (node-rator-name node))
          (incf count))
        (values))))
    count))

(defun replace-direct-applications (node name rator)
  (declare (type node node rator)
           (type parser:identifier name)
           (values node &optional))
  (traverse
   node
   (list
    (action (:after node-application node)
      (when (eq name (node-rator-name node))
        (application-with-rator node rator)))
    (action (:after node-direct-application node)
      (when (eq name (node-rator-name node))
        (application-with-rator node rator))))))

(defun maybe-inline-single-use-local-function (node)
  (declare (type node-let node)
           (values node boolean &optional))
  (let ((bindings (node-let-bindings node))
        (subexpr (node-let-subexpr node)))
    (loop :for binding :in bindings
          :for name := (car binding)
          :for expr := (cdr binding)
          :when (and (node-abstraction-p expr)
                     (single-value-function-p expr)
                     (not (member name (node-variables expr) :test #'eq))
                     (= 1 (direct-application-occurrence-count subexpr name))
                     (= 1 (node-name-occurrence-count subexpr name))
                     (not (binding-name-used-in-bindings-p
                           name
                           (remove name bindings :key #'car :test #'eq))))
            :do
               (let ((remaining-bindings
                       (remove name bindings :key #'car :test #'eq))
                     (new-subexpr
                       (replace-direct-applications subexpr name expr)))
                 ;; (let ((f (fn ...)) ...) body[(f args...)])
                 ;; -> (let (...) body[((fn ...) args...)])
                 (return
                   (values
                    (let-node-with-bindings node remaining-bindings new-subexpr)
                    t)))
          :finally (return (values node nil)))))

(defun maybe-inline-let-bound-match-scrutinee (node)
  (declare (type node-let node)
           (values node boolean &optional))
  (let ((bindings (node-let-bindings node))
        (subexpr (node-let-subexpr node)))
    (unless (and (typep subexpr 'node-match)
                 (node-variable-p (node-match-expr subexpr)))
      (return-from maybe-inline-let-bound-match-scrutinee
        (values node nil)))
    (let* ((name (node-variable-value (node-match-expr subexpr)))
           (binding (assoc name bindings :test #'eq)))
      (unless (and binding
                   (let-bound-match-scrutinee-p name subexpr)
                   (not (member name (node-variables (cdr binding)) :test #'eq)))
        (return-from maybe-inline-let-bound-match-scrutinee
          (values node nil)))
      (let ((remaining-bindings
              (remove name bindings :key #'car :test #'eq)))
        (when (binding-name-used-in-bindings-p name remaining-bindings)
          (return-from maybe-inline-let-bound-match-scrutinee
            (values node nil)))
        ;; (let ((f ...) (x e)) (match x branches...))
        ;; -> (let ((f ...)) (match e branches...))
        (let ((new-subexpr (match-with-expr subexpr (cdr binding))))
          (values
           (let-node-with-bindings node remaining-bindings new-subexpr)
           t))))))

(defun maybe-inline-let-bound-match-application (node)
  (declare (type node-let node)
           (values node boolean &optional))
  (let ((bindings (node-let-bindings node))
        (subexpr (node-let-subexpr node)))
    (unless (and (typep subexpr 'node-match)
                 (typep (node-match-expr subexpr)
                        '(or node-application node-direct-application))
                 (node-rator-name (node-match-expr subexpr)))
      (return-from maybe-inline-let-bound-match-application
        (values node nil)))
    (let* ((application (node-match-expr subexpr))
           (name (node-rator-name application))
           (binding (assoc name bindings :test #'eq)))
      (unless (and binding
                   (single-value-node-p application)
                   (node-abstraction-p (cdr binding))
                   (not (member name (node-variables (cdr binding)) :test #'eq))
                   (not (member name (application-argument-variables application) :test #'eq))
                   (not (member name (match-free-variables subexpr) :test #'eq)))
        (return-from maybe-inline-let-bound-match-application
          (values node nil)))
      (let ((remaining-bindings
              (remove name bindings :key #'car :test #'eq)))
        (when (binding-name-used-in-bindings-p name remaining-bindings)
          (return-from maybe-inline-let-bound-match-application
            (values node nil)))
        ;; (let ((f (fn ...)) ...) (match (f args...) branches...))
        ;; -> (let (...) (match ((fn ...) args...) branches...))
        (let ((new-subexpr
                (match-with-expr
                 subexpr
                 (application-with-rator application (cdr binding)))))
          (values
           (let-node-with-bindings node remaining-bindings new-subexpr)
           t))))))

(defun immediate-bound-application-p (name subexpr)
  (declare (type parser:identifier name)
           (type node subexpr)
           (values (or null node-application node-direct-application) &optional))
  (when (and (typep subexpr '(or node-application node-direct-application))
             (eq name (node-rator-name subexpr))
             (not (member name
                          (application-argument-variables subexpr)
                          :test #'eq)))
    subexpr))

(defun maybe-collapse-bind (node)
  (declare (type node-bind node)
           (values node boolean &optional))
  (let ((name (node-bind-name node))
        (expr (node-bind-expr node))
        (body (node-bind-body node)))
    (cond
      ;; (bind x e x) -> e
      ((and (node-variable-p body)
            (eq name (node-variable-value body))
            (not (member name (node-variables expr) :test #'eq)))
       (values (copy-node expr (node-type node)) t))

      ;; (bind x y body[x]) -> body[y]
      ((and (node-variable-p expr)
            (not (eq name (node-variable-value expr)))
            (alias-bind-safe-p name (node-variable-value expr) body))
       (values
        (substitute-aliases body (list (cons name expr)))
        t))

      ;; (bind x e (match x branches...)) -> (match e branches...)
      ((let-bound-match-scrutinee-p name body)
       (values (match-with-expr body expr) t))

      ;; (bind f (fn (...) body) (f args...)) -> ((fn (...) body) args...)
      ((and (node-abstraction-p expr)
            (single-value-function-p expr)
            (not (member name (node-variables expr) :test #'eq)))
       (alexandria:if-let ((application (immediate-bound-application-p name body)))
         (values (application-with-rator application expr) t)
         (values node nil)))

      (t
       (values node nil)))))

(defun maybe-collapse-empty-let (node)
  (declare (type node-let node)
           (values node boolean &optional))
  (let ((bindings (node-let-bindings node))
        (subexpr (node-let-subexpr node)))
    ;; (let () body) -> body
    (if (null bindings)
        (values (copy-node subexpr (node-type node)) t)
        (values node nil))))

(defun maybe-collapse-single-binding-identity-let (node)
  (declare (type node-let node)
           (values node boolean &optional))
  (let ((bindings (node-let-bindings node))
        (subexpr (node-let-subexpr node)))
    ;; (let ((x e)) x) -> e
    (if (and (null (cdr bindings))
             (node-variable-p subexpr)
             (eq (caar bindings)
                 (node-variable-value subexpr))
             (not (member (caar bindings)
                          (node-variables (cdar bindings))
                          :test #'eq)))
        (values (copy-node (cdar bindings) (node-type node)) t)
        (values node nil))))

(defun maybe-collapse-let-alias-chain (node)
  (declare (type node-let node)
           (values node boolean &optional))
  (let* ((bindings (node-let-bindings node))
         (aliases (let-alias-substitutions bindings)))
    (if aliases
        ;; (let ((x e) (y x) (z y)) body[z]) -> (let ((x e)) body[x])
        (values
         (let-node-with-bindings
          node
          (loop :for binding :in bindings
                :unless (assoc (car binding) aliases :test #'eq)
                  :collect (cons (car binding)
                                 (substitute-aliases (cdr binding) aliases)))
          (substitute-aliases (node-let-subexpr node) aliases))
         t)
        (values node nil))))

(defun maybe-collapse-alias-let (node)
  (declare (type node-let node)
           (values node boolean &optional))
  (try-node-rewrites
   node
   '(maybe-collapse-empty-let
     maybe-collapse-single-binding-identity-let
     maybe-inline-let-bound-match-scrutinee
     maybe-inline-let-bound-match-application
     maybe-inline-single-use-local-function
     maybe-collapse-let-alias-chain)))

(defun maybe-sink-application-into-rator (node)
  (declare (type node-application node)
           (values node boolean &optional))
  (let ((rator (node-application-rator node)))
    (cond
      ((and (node-abstraction-p rator)
            (abstraction-application-inlineable-p node rator))
       (values (inline-abstraction-application node rator) t))

      (t
       (typecase rator
         (node-let
          ;; ((let bindings f) args...) -> (let bindings (f args...))
          (if (application-can-enter-let-p node rator)
              (values
               (make-node-let
                :type (node-type node)
                :bindings (node-let-bindings rator)
                :subexpr (application-with-rator node (node-let-subexpr rator)))
               t)
              (values node nil)))

         (node-seq
          ;; ((progn e... f) args...) -> (progn e... (f args...))
          (let ((nodes (node-seq-nodes rator)))
            (if (endp nodes)
                (values node nil)
                (values
                 (make-node-seq
                  :type (node-type node)
                  :nodes (append (butlast nodes)
                                 (list (application-with-rator node (car (last nodes))))))
                 t))))

         (node-match
          ;; ((match x (p f)...) args...) -> (match x (p (f args...))...)
          (if (application-can-enter-match-p node rator)
              (values
               (make-node-match
                :type (node-type node)
                :expr (node-match-expr rator)
                :branches
                (loop :for branch :in (node-match-branches rator)
                      :collect
                      (make-match-branch
                       :pattern (match-branch-pattern branch)
                       :body (application-with-rator node (match-branch-body branch)))))
               t)
              (values node nil)))

         (t
          (values node nil)))))))

(defun lawnmow (node)
  "Simplify administrative AST forms left behind by optimization passes."
  (declare (type node node)
           (values node boolean &optional))
  (let ((changed? nil))
    (values
     (traverse
      node
      (list
       (action (:after node-let node)
         (multiple-value-bind (new-node new-changed?) (maybe-collapse-alias-let node)
           (when new-changed?
             (setf changed? t)
             new-node)))
       (action (:after node-bind node)
         (multiple-value-bind (new-node new-changed?) (maybe-collapse-bind node)
           (when new-changed?
             (setf changed? t)
             new-node)))
       (action (:after node-match node)
         (multiple-value-bind (new-node new-changed?) (maybe-simplify-match node)
           (when new-changed?
             (setf changed? t)
             new-node)))
       (action (:after node-application node)
         (multiple-value-bind (new-node new-changed?) (maybe-sink-application-into-rator node)
           (when new-changed?
             (setf changed? t)
             new-node)))))
     changed?)))
