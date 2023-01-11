(defpackage #:coalton-impl/typechecker/derive-type
  (:use
   #:cl
   #:coalton-impl/algorithm
   #:coalton-impl/ast
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/type-errors
   #:coalton-impl/typechecker/substitutions
   #:coalton-impl/typechecker/predicate
   #:coalton-impl/typechecker/scheme
   #:coalton-impl/typechecker/typed-node
   #:coalton-impl/typechecker/environment
   #:coalton-impl/typechecker/parse-type)
  (:import-from
   #:coalton-impl/typechecker/unify
   #:unify
   #:match)
  (:import-from
   #:coalton-impl/typechecker/context-reduction
   #:entail
   #:reduce-context
   #:default-preds
   #:default-subs
   #:split-context)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error))
  (:export
   #:derive-expression-type             ; FUNCTION
   #:derive-bindings-type               ; FUNCTION
   #:derive-expl-type                   ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/derive-type)

;;;
;;; Expressions
;;;

(defun derive-literal-type (value)
  "Derive the type of a given literal VALUE. Return two values, the type (a TY) and a list of predicates (a TY-PREDICATE-LIST)."
  (declare (values ty ty-predicate-list))
  (etypecase value
    (integer      (values *integer-type*      nil))
    (single-float (values *single-float-type* nil))
    (double-float (values *double-float-type* nil))
    (string       (values *string-type*       nil))
    (character    (values *char-type*         nil))
    (ratio        (values *fraction-type*     nil))))

(defgeneric derive-expression-type (value env substs)
  (:documentation "Derive the TYPE and generate a TYPED-NODE for expression VALUE

Returns (VALUES type predicate-list typed-node subs)")
  (:method :around ((value node) env substs)
    (when (next-method-p)
      (error:with-context ("~A" (node-unparsed value))
        (call-next-method))))

  (:method ((value node-literal) env substs)
    (declare (type substitution-list substs)
             (values ty ty-predicate-list typed-node substitution-list ty-list &optional))
    (let ((literal-value (node-literal-value value)))
      (multiple-value-bind (type preds)
          (derive-literal-type literal-value)
        (values
         type
         preds
         (make-typed-node-literal
          :type (to-scheme (qualify nil type))
          :unparsed (node-unparsed value)
          :value literal-value)
         substs
         nil))))

  (:method ((value node-lisp) env substs)
    (declare (type environment env)
             (type substitution-list substs)
             (values ty ty-predicate-list typed-node substitution-list ty-list &optional))
    (let* ((scheme (parse-and-resolve-type env (node-lisp-type value)))
           (qual-type (fresh-inst scheme))
           (type (qualified-ty-type qual-type))
           (preds (qualified-ty-predicates qual-type)))
      (values type
              preds
              (make-typed-node-lisp
               :type (to-scheme qual-type)
               :unparsed (node-unparsed value)
               :variables (node-lisp-variables value)
               :form (node-lisp-form value))
              substs
              nil)))

  (:method ((value node-variable) env substs)
    (declare (type substitution-list substs)
             (values ty ty-predicate-list typed-node substitution-list ty-list &optional))
    (let* ((scheme (lookup-value-type env (node-variable-name value)))
           (qual-type (fresh-inst scheme))
           (type (qualified-ty-type qual-type))
           (preds (qualified-ty-predicates qual-type)))
      (values type
              preds
              (make-typed-node-variable
               :type (to-scheme qual-type)
               :unparsed (node-unparsed value)
               :name (node-variable-name value))
              substs
              nil)))

  (:method ((value node-application) env substs)
    (declare (type environment env)
             (type substitution-list substs)
             (values ty ty-predicate-list typed-node substitution-list ty-list &optional))

    (let* ((rator (node-application-rator value))
           (rands (node-application-rands value))
           (ret-ty (make-variable))
           (typed-rands nil))

      (when (null rands)
        (util:coalton-bug "Invalid application with 0 arguments ~A." rator))

      (multiple-value-bind (fun-ty fun-preds typed-rator substs returns)
          (derive-expression-type rator env substs)
        (unless (or (tyvar-p fun-ty)
                    (function-type-p fun-ty))
          (error 'invalid-operator-type-error :type fun-ty))
        (let ((arg-preds nil))
          (labels ((build-function (args)
                     (if (null args)
                         ret-ty
                         (multiple-value-bind (arg-ty arg-pred typed-rand new-substs-arg new-returns)
                             (derive-expression-type (car args) env substs)
                           (push typed-rand typed-rands)
                           (setf arg-preds (append arg-pred arg-preds))
                           (setf substs new-substs-arg)
                           (setf returns (append new-returns returns))
                           (make-function-type arg-ty (build-function (cdr args)))))))
            (let* ((ftype (build-function rands))
                   (preds (append fun-preds arg-preds))
                   (substs (unify substs ftype fun-ty
                                  (format nil "function implied by the operand~P" (length rands))
                                  "function type implied by the operator")))
              (values ret-ty
                      preds
                      (make-typed-node-application
                       :type (to-scheme (qualify nil ret-ty))
                       :unparsed (node-unparsed value)
                       :rator typed-rator
                       :rands (reverse typed-rands))
                      substs
                      returns)))))))

  (:method ((value node-abstraction) env substs)
    (declare (type environment env)
             (type substitution-list substs)
             (values ty ty-predicate-list typed-node substitution-list ty-list &optional))
    (let* ((subexpr (node-abstraction-subexpr value))
           (vars (node-abstraction-vars value))
           (new-env (push-value-environment
                     env
                     (mapcar (lambda (var) (cons var (to-scheme (qualify nil (make-variable)))))
                             vars))))
      (multiple-value-bind (ret-ty ret-preds typed-subexpr new-substs returns)
          (derive-expression-type subexpr new-env substs)
        (labels ((build-function (args)
                   (if (null args)
                       ret-ty
                       (make-function-type (qualified-ty-type (fresh-inst (lookup-value-type new-env (car args))))
                                           (build-function (cdr args))))))
          (let ((out-ty (build-function vars))
                (ret-preds (reduce-context env ret-preds new-substs)))
            (setf substs new-substs)

            ;; Unify the functions return type against early return
            ;; statements
            (loop :for return :in returns
                  :do (setf substs (unify substs ret-ty return "function return type" "early return statement")))

            (values out-ty
                    ret-preds
                    (make-typed-node-abstraction
                     :type (to-scheme (make-qualified-ty :predicates nil :type out-ty))
                     :unparsed (node-unparsed value)
                     :vars (mapcar (lambda (var)
                                     (cons var (lookup-value-type new-env var)))
                                   vars)
                     :subexpr typed-subexpr
                     :name-map (node-abstraction-name-map value))
                    substs
                    nil))))))

  (:method ((value node-let) env subs)
    (declare (type environment env)
             (type substitution-list subs)
             (values ty ty-predicate-list typed-node substitution-list ty-list &optional))
    (let ((name-table (make-hash-table)))
      (loop :for (name . node_) :in (node-let-bindings value) :do
        (progn
          (when (gethash name name-table)
            (error 'duplicate-definition
                   :name (cdr (assoc name (node-let-name-map value)))))
          (setf (gethash name name-table) t))))

    (let ((bindings (node-let-bindings value))
          (declared-types
            (mapcar (lambda (form)
                      (cons (car form)
                            (parse-and-resolve-type env (cdr form))))
                    (node-let-declared-types value)))
          (expl-bindings nil)
          (impl-bindings nil))
      ;; Split out explicit and implicit bindings
      (loop :for binding :in bindings
            :if (assoc (car binding) declared-types) :do
              (push binding expl-bindings)
            :else :do
              (push binding impl-bindings))

      (multiple-value-bind (typed-bindings bindings-preds env subs returns explicit-types)
          (derive-bindings-type
           impl-bindings
           expl-bindings
           (alexandria:alist-hash-table declared-types)
           env
           subs
           (node-let-name-map value))
        (multiple-value-bind (type ret-preds typed-subexpr new-subs new-returns)
            (derive-expression-type (node-let-subexpr value) env subs)

          (setf returns (append new-returns returns))

          (let ((preds (append ret-preds bindings-preds)))
            (values type
                    preds
                    (make-typed-node-let
                     :type (to-scheme (qualify nil type))
                     :unparsed (node-unparsed value)
                     :bindings typed-bindings
                     :subexpr typed-subexpr
                     :explicit-types explicit-types
                     :name-map (node-let-name-map value))
                    new-subs
                    returns))))))

  (:method ((value node-match) env subs)
    (declare (type environment env)
             (type substitution-list subs)
             (values ty ty-predicate-list typed-node substitution-list ty-list &optional))
    (let ((tvar (make-variable)))
      (multiple-value-bind (ty preds typed-expr new-subs returns)
          (derive-expression-type (node-match-expr value) env subs)
        (error:with-context ("match on ~W" (node-unparsed (node-match-expr value)))
          (multiple-value-bind (typed-branches match-preds new-subs new-returns)
              (derive-match-branches-type
               (make-function-type ty tvar)
               (node-match-branches value)
               env
               new-subs)
            (setf returns (append new-returns returns))
            (let ((preds (append preds match-preds)))
              (values
               tvar
               preds
               (make-typed-node-match
                :type (to-scheme (qualify nil tvar))
                :unparsed (node-unparsed value)
                :expr typed-expr
                :branches typed-branches)
               new-subs
               returns)))))))

  (:method ((value node-seq) env subs)
    (declare (type environment env)
             (type substitution-list subs)
             (values ty ty-predicate-list typed-node substitution-list ty-list &optional))
    (let* ((initial-elements (butlast (node-seq-subnodes value)))
           (last-element (car (last (node-seq-subnodes value))))
           (preds nil)
           (nodes nil)
           (returns nil))

      (loop :for elem :in initial-elements :do
        (multiple-value-bind (tyvar preds_ node subs_ new-returns)
            (derive-expression-type elem env subs)

          (setf subs subs_)
          (setf preds (append preds preds_))
          (push node nodes)
          (setf returns (append new-returns returns))

          (when (function-type-p (apply-substitution subs tyvar))
            (alexandria:simple-style-warning
             "Expression ~A in progn evaluates to a function. This may be intentional, but is also a common mistake in the event a function is partially applied."
             (node-unparsed elem)))))

      (multiple-value-bind (tyvar preds_ node subs new-returns)
          (derive-expression-type last-element env subs)
        (setf preds (append preds preds_))
        (push node nodes)
        (setf returns (append new-returns returns))

        (values
         tyvar
         preds
         (make-typed-node-seq
          :type (to-scheme (qualify nil tyvar))
          :unparsed (node-unparsed value)
          :subnodes (reverse nodes))
         subs
         returns))))

  (:method ((value node-the) env subs)
    (declare (type environment env)
             (type substitution-list subs)
             (values ty ty-predicate-list typed-node substitution-list ty-list &optional))

    (let* ((declared-scheme (parse-and-resolve-type env (node-the-type value)))
           (declared-qualified (fresh-inst declared-scheme))
           (declared-type (qualified-ty-type declared-qualified))
           (declared-preds (qualified-ty-predicates declared-qualified)))

      (multiple-value-bind (type preds node subs returns)
          (derive-expression-type (node-the-subnode value) env subs)

        (let* ((subs_ (match (apply-substitution subs type) declared-type))
               (subs (compose-substitution-lists subs_ subs))
               (preds_ (reduce-context env preds subs)))

          (unless (subsetp declared-preds preds_ :test #'equalp)
            (error 'declared-type-additional-predicates
                   :preds (set-difference declared-preds preds_ :test #'equalp)
                   :type declared-scheme))

          (values
           type
           preds
           node
           subs
           returns)))))

  (:method ((value node-return) env subs)
    (declare (type environment env)
             (type substitution-list subs)
             (values ty ty-predicate-list typed-node substitution-list ty-list &optional))

    (multiple-value-bind (type preds node subs returns)
        (derive-expression-type (node-return-expr value) env subs)

      (let ((return-ty (make-variable)))
        (values
         return-ty
         preds
         (make-typed-node-return
          :type (to-scheme (qualify nil return-ty))
          :unparsed (node-unparsed value)
          :expr node)
         subs
         (cons
          type
          returns)))))

  (:method ((value node-bind) env subs)
    (declare (type environment env)
             (type substitution-list subs)
             (values ty ty-predicate-list typed-node substitution-list ty-list &optional))

    (multiple-value-bind (expr-type preds expr-node subs returns)
        (derive-expression-type (node-bind-expr value) env subs)

      (multiple-value-bind (body-type new-preds body-node subs new-returns)
          (derive-expression-type
           (node-bind-body value)
           (set-value-type env (node-bind-name value) (quantify nil (qualify nil expr-type)))
           subs)
        (setf preds (append new-preds preds))
        (setf returns (append new-returns returns))

        (values
         body-type
         preds
         (make-typed-node-bind
          :type (quantify nil (qualify nil body-type))
          :unparsed (node-unparsed value)
          :name (node-bind-name value)
          :expr expr-node
          :body body-node)
         subs
         returns)))))

;;;
;;; Let Bindings
;;;

(defun derive-bindings-type (impl-bindings expl-bindings expl-declarations env subs name-map
                             &key
                               (allow-deferred-predicates t)
                               (allow-returns t))
  "IMPL-BINDINGS and EXPL-BINDIGNS are of form (SYMBOL . EXPR)
EXPL-DECLARATIONS is a HASH-TABLE from SYMBOL to SCHEME."
  (declare (type environment env)
           (type substitution-list subs)
           (type list name-map)
           (values typed-binding-list ty-predicate-list environment substitution-list ty-list hash-table &optional))

  ;; Push all the explicit type declarations on to the environment
  (let* ((expl-binds (mapcar (lambda (b)
                               (cons (car b)
                                     (gethash (car b) expl-declarations)))
                             expl-bindings))
         (env (push-value-environment env expl-binds))
         (typed-bindings nil)
         (preds nil)
         (returns nil))

    ;; First we will be checking the implicit bindings
    ;; Sort the bindings into strongly connected components of mutually
    ;; recursive bindings
    (let ((sccs (reverse (tarjan-scc (bindings-to-dag impl-bindings)))))
      (dolist (scc sccs)
        ;; Lookup all bindings in this scc
        (let ((scc-bindings
                (mapcar (lambda (b) (find b impl-bindings :key #'car)) scc)))
          ;; Derive the type of all parts of the scc together
          (multiple-value-bind (typed-impl-bindings impl-preds new-env new-subs new-returns)
              (derive-impls-type
               scc-bindings
               env
               subs
               name-map
               :allow-deferred-predicates allow-deferred-predicates
               :allow-returns allow-returns)

            ;; Update the current environment and substitutions
            (setf env new-env
                  subs new-subs)
            ;; Add the typed binding nodes to the output typed nodes
            (setf typed-bindings (append typed-bindings typed-impl-bindings))
            (setf preds (append impl-preds preds))

            (setf returns (append new-returns returns)))))

      (let ((explicit-types (make-hash-table)))

        ;; Now that we have the implicit bindings sorted out, we can type
        ;; check the explicit ones.
        (dolist (binding expl-bindings)
          ;; Derive the type of the binding
          (multiple-value-bind (ty-scheme typed-expl-binding expl-preds new-env new-subs qual-ty new-returns)
              (derive-expl-type binding
                                (gethash (car binding) expl-declarations)
                                env subs name-map
                                :allow-deferred-predicates allow-deferred-predicates
                                :allow-returns allow-returns)
            (declare (ignore ty-scheme))


            ;; Update the current environment and substitutions
            (setf env new-env
                  subs new-subs)
            ;; Add the binding to the list of bindings
            (push typed-expl-binding typed-bindings)
            (setf preds (append preds expl-preds))

            (setf returns (append new-returns returns))

            (setf (gethash (car binding) explicit-types) qual-ty)))

        (validate-bindings-for-codegen typed-bindings env (not allow-deferred-predicates))

        ;;
        ;; Binding groups are typechecked before predicates are
        ;; resolved. Once substitutions are applied the typed nodes in
        ;; an scc will have the correct type minus predicates. The
        ;; following rewrite pass will update the typed-nodes in a
        ;; binding group with those missing predicates.
        ;;
        (let ((bindings (mapcar
                         (lambda (binding)
                           (cons (car binding) (lookup-value-type env (car binding))))
                         typed-bindings)))
          (values
           (loop :for (name . node) :in typed-bindings
                 :collect (cons name
                                (rewrite-recursive-calls
                                 (remove-static-preds (apply-substitution subs node))
                                 bindings)))
           preds
           env
           subs
           returns
           explicit-types))))))

(defun length>1p (list)
  (cdr list))

(defun typed-node-binding-sccs (bindings)
  (declare (typed-binding-list bindings)
           (values list &optional))
  (let ((binding-names (mapcar #'car bindings)))
    (reverse
     (tarjan-scc
      (loop :for (name . node) :in bindings
            :collect (cons name (intersection binding-names (collect-variable-namespace node))))))))

(defun recursive-binding-p (name initform)
  "If (NAME . INITFORM) were a binding, would it be recursive? I.e. does INITFORM read from NAME?"
  (declare (symbol name)
           (typed-node initform)
           (values list &optional))
  (member name (collect-variable-namespace initform) :test #'equalp))

(defun type-recursively-constructable-p (type-entry env)
  (declare (type-entry type-entry)
           (environment env)
           (values boolean &optional))
  (or
   ;; special case: `List' is recursively constructable
   (eq type-entry (lookup-type env 'coalton:List))
   ;; a type is recursively constructable if it has `repr :lisp' or the default repr
   (and (not (type-entry-enum-repr type-entry))
        (not (type-entry-newtype type-entry))
        (or (eq (type-entry-explicit-repr type-entry) :lisp)
            (null (type-entry-explicit-repr type-entry))))))

(defun check-recursive-binding-is-recursively-constructable (name initform env &optional toplevel)
  (declare (symbol name)
           (typed-node-application initform)
           (environment env))
  (when toplevel
    (error 'self-recursive-toplevel-form
           :name name))

  (let* ((ctor (typed-node-application-rator initform)))
    (unless (typed-node-variable-p ctor)
      (error 'self-recursive-non-constructor-call
             :name name
             :function (typed-node-unparsed ctor)))
    (let* ((ctor-name (typed-node-variable-name ctor))
           (ctor-entry (lookup-constructor env ctor-name :no-error t)))
      (unless ctor-entry
        (error 'self-recursive-non-constructor-call
               :name name
               :function (typed-node-unparsed ctor)))
      (let* ((type-name (constructor-entry-constructs ctor-entry))
             (type-entry (lookup-type env type-name :no-error t)))
        (unless type-entry
          (util:coalton-bug "Constructor ~S with entry ~S but no corresponding type-entry"
                       ctor-name ctor-entry))
        (unless (type-recursively-constructable-p type-entry env)
          (error 'self-recursive-non-default-repr
                 :name name
                 :function ctor-name
                 :type type-name))
        (let* ((needed-arity (constructor-entry-arity ctor-entry))
               (found-arity (length (typed-node-application-rands initform))))
          (unless (= needed-arity found-arity)
            (error 'self-recursive-partial-application
                   :name name
                   :function ctor-name
                   :required-arg-count needed-arity
                   :supplied-args (typed-node-application-rands initform))))))))

(defun validate-single-binding-for-codegen (name initform env toplevel)
  (cond ((typed-node-abstraction-p initform) (values))
        ((recursive-binding-p name initform) (check-recursive-binding-is-recursively-constructable name initform env toplevel))
        (t (values))))

(defun validate-mutually-recursive-scc-for-codegen (names names-to-initforms env toplevel)
  (let* ((initforms (loop :for name :in names
                          :collect (gethash name names-to-initforms))))
    (cond ((every #'typed-node-abstraction-p initforms)
           (values))
          ((every #'typed-node-application-p initforms)
           (loop :for name :in names
                 :for initform :in initforms
                 :do (check-recursive-binding-is-recursively-constructable name initform env toplevel)))
          (t (error 'mutually-recursive-function-and-data
                    :names names)))))

(defun validate-binding-scc-for-codegen (scc names-to-initforms env toplevel)
  (declare (list scc)
           (hash-table names-to-initforms)
           (environment env))
  (if (length>1p scc)
      (validate-mutually-recursive-scc-for-codegen scc names-to-initforms env toplevel)
      (let* ((name (first scc))
             (initform (gethash name names-to-initforms)))
        (validate-single-binding-for-codegen name initform env toplevel))))

(defun validate-bindings-for-codegen (bindings env toplevel)
  "Disallow recursive bindings except for `repr :default' constructors or for `fn' abstractions.

TOPLEVEL is set to indicate additional checks should be completed in COALTON-TOPLEVEL forms."
  (declare (typed-binding-list bindings))

  (let* ((scc-names (typed-node-binding-sccs bindings))
         (names-to-initforms (alexandria:alist-hash-table bindings :test 'eq)))
    (loop :for scc :in scc-names
          :do (validate-binding-scc-for-codegen scc names-to-initforms env toplevel))))

(defun derive-binding-type-seq (names tvars exprs env subs name-map
                                &key (allow-deferred-predicates t)
                                  (allow-returns t))
  (declare (type tyvar-list tvars)
           (type node-list exprs)
           (type environment env)
           (type substitution-list subs)
           (type list name-map)
           (values typed-node-list ty-predicate-list substitution-list ty-list))
  (let* ((preds nil)
         (returns nil)
         (typed-nodes
           (loop :for name :in names
                 :for tvar :in tvars
                 :for expr :in exprs
                 :collect
                 (multiple-value-bind (typed-node binding-preds new-subs new-returns)
                     (derive-binding-type name tvar expr env subs name-map)
                   (setf subs new-subs)
                   (setf returns (append new-returns returns))

                   (error:with-context ("definition of ~A" name)
                     (when (and new-returns (not allow-returns))
                       (error 'unexpected-return))
                     (when (not allow-deferred-predicates)
                       ;; When we are not allowed to defer predicates,
                       ;; call reduce-context which will signal an
                       ;; error on deferred preds.
                       (reduce-context env binding-preds new-subs
                                       :allow-deferred-predicates nil)))

                   (setf preds (append preds binding-preds))
                   typed-node)))
         (bindings (mapcar
                    (lambda (name)
                      (cons name (lookup-value-type env name)))
                    names)))
    (declare (ignore bindings))
    (values
     typed-nodes
     preds
     subs
     returns)))

(defun derive-impls-type (bindings env subs name-map
                          &key
                            (allow-deferred-predicates t)
                            (allow-returns nil))
  (declare (type binding-list bindings)
           (type environment env)
           (type substitution-list subs)
           (type list name-map)
           (values typed-binding-list ty-predicate-list environment substitution-list ty-list))
  (let* (;; Generate fresh tvars and schemes for each binding
         (tvars (mapcar (lambda (_)
                          (declare (ignore _))
                          (make-variable))
                        bindings))
         (schemes (mapcar (lambda (b) (to-scheme (qualify nil b))) tvars))

         (new-bindings (mapcar (lambda (b sch) (cons (first b) sch)) bindings schemes))
         (local-env (push-value-environment env new-bindings))
         (exprs (mapcar #'cdr bindings)))

    ;; Derive the type of each binding
    (multiple-value-bind (typed-bindings local-preds local-subs returns)
        (derive-binding-type-seq (mapcar #'car bindings) tvars exprs local-env subs name-map
                                 :allow-deferred-predicates allow-deferred-predicates
                                 :allow-returns allow-returns)

      (let* ((expr-types (apply-substitution local-subs tvars)) ; ts'
             (expr-preds (apply-substitution local-subs local-preds)) ; ps'

             ;; Extract local type variables
             (env-tvars (type-variables (apply-substitution local-subs env))) ; fs
             (expr-tvars (reduce (lambda (&optional a b) (remove-duplicates (append a b) :test #'equalp :from-end t))
                                 expr-types
                                 :key #'type-variables)) ; vss
             (local-tvars (set-difference expr-tvars
                                          env-tvars
                                          :test #'equalp))) ; gs
        ;;
        ;; NOTE: this is where functional dependency substitutions are generated
        ;;

        (multiple-value-bind (fundep-preds fundep-subs)
            (solve-fundeps env expr-preds local-subs)
          (setf local-subs fundep-subs)
          (setf expr-types (apply-substitution local-subs expr-types))
          (setf expr-preds (apply-substitution local-subs fundep-preds)))

        (multiple-value-bind (deferred-preds retained-preds)
            (split-context env env-tvars expr-preds local-subs)

          (let* ((defaultable-preds (default-preds env (append env-tvars local-tvars) retained-preds))

                 (retained-preds (set-difference retained-preds defaultable-preds :test #'equalp)))

            ;; Apply defaulting to defaultable ambigious predicates
            (setf local-subs (compose-substitution-lists (default-subs env nil defaultable-preds) local-subs))

            (labels ((restricted (bindings)
                       (some (lambda (b) (not (node-abstraction-p (cdr b))))
                             bindings)))


              ;;
              ;; NOTE: This is where defaulting happens
              ;;

              (error:with-context ("definition~p of ~{~S~^, ~}" (length bindings) (mapcar #'car bindings))
                ;; Defaulting only applies to top level bindings
                (unless allow-deferred-predicates
                  (if (restricted bindings)
                      ;; Restricted bindings have all predicates defaulted
                      (progn
                        (setf local-subs
                              (compose-substitution-lists
                               (default-subs env nil (append deferred-preds retained-preds))
                               local-subs))
                        (setf deferred-preds (reduce-context env deferred-preds local-subs))
                        (setf retained-preds (reduce-context env retained-preds local-subs))
                        (setf expr-types (apply-substitution local-subs expr-types)))
                      ;; Unrestricted bindings have deferred predicates defaulted
                      (progn
                        (setf local-subs
                              (compose-substitution-lists
                               (default-subs env nil deferred-preds)
                               local-subs))
                        (setf deferred-preds (reduce-context env deferred-preds local-subs))
                        (setf expr-types (apply-substitution local-subs expr-types)))))

                ;; Error when predicates cannot be deferred
                (unless (or allow-deferred-predicates (null deferred-preds))
                  (error 'unresolvable-constraint :pred (first deferred-preds))))

              ;; NOTE: This is where the monomorphism restriction happens

              (if (restricted bindings)
                  (let* ((allowed-tvars (set-difference local-tvars (type-variables retained-preds)))
                         ;; Quantify local type variables
                         (output-schemes (mapcar (lambda (type)
                                                   (quantify
                                                    allowed-tvars
                                                    (make-qualified-ty
                                                     :predicates nil
                                                     :type type)))
                                                 expr-types))

                         ;; Build new env
                         (output-env (push-value-environment
                                      env
                                      (mapcar (lambda (b sch)
                                                (cons (first b) sch))
                                              bindings output-schemes)))

                         ;; Restricted bindings cannot retain predicates
                         (deferred-preds (append deferred-preds retained-preds)))

                    ;; If a restricted binding group would defer
                    ;; predicates, generate a monomorphism error
                    (when (and (not allow-deferred-predicates) deferred-preds)
                      (let* ((name (car (first bindings)))
                             (qual-ty (qualify nil (first expr-types))))
                        (error 'toplevel-monomorphism-restriction
                               :name name
                               :type (quantify (type-variables qual-ty) qual-ty)
                               :preds deferred-preds)))

                    (values
                     (mapcar
                      (lambda (b typed)
                        (cons (car b) typed))
                      bindings
                      typed-bindings)
                     deferred-preds
                     output-env
                     local-subs
                     returns))
                  (let* (;; Quantify local type variables
                         (output-schemes
                           (mapcar (lambda (type)
                                     (quantify
                                      local-tvars
                                      (make-qualified-ty
                                       :predicates retained-preds
                                       :type type)))
                                   expr-types))

                         ;; Build new env
                         (output-env (push-value-environment
                                      env
                                      (mapcar (lambda (b sch)
                                                (cons (first b) sch))
                                              bindings output-schemes))))

                    (values
                     ;; Rewrite the predicates on lambda forms to match
                     ;; retained predicates. This will remove deferred
                     ;; predicates, and attach predicates retained from
                     ;; a subexpression.
                     (mapcar (lambda (b node)
                               (let* ((node-qual-type (fresh-inst (typed-node-type node)))
                                      (node-type (qualified-ty-type node-qual-type)))
                                 (cons (car b)
                                       (replace-node-type node (to-scheme (qualify retained-preds node-type))))))
                             bindings typed-bindings)
                     deferred-preds
                     output-env
                     local-subs
                     returns))))))))))

(defun derive-expl-type (binding declared-ty env subs name-map
                         &key (allow-deferred-predicates t)
                           (allow-returns t))
  (declare (type cons binding)
           (type ty-scheme declared-ty)
           (type environment env)
           (type substitution-list subs)
           (type list name-map)
           (values ty-scheme cons ty-predicate-list environment substitution-list qualified-ty ty-list &optional))
  (let* (;; Generate fresh instance of declared type
         (fresh-qual-type (fresh-inst declared-ty))
         (fresh-type (qualified-ty-type fresh-qual-type))
         (fresh-preds (qualified-ty-predicates fresh-qual-type)))

    ;; Derive the type of the expression, unifying with the
    ;; declared type
    (multiple-value-bind (typed-node preds local-subs returns)
        (derive-binding-type
         (car binding)
         fresh-type
         (cdr binding)
         env subs name-map)

      (let* ((expr-type (apply-substitution local-subs fresh-type))
             (expr-preds (apply-substitution local-subs fresh-preds))

             ;; Extract local type variables
             (env-tvars (type-variables (apply-substitution local-subs env)))
             (local-tvars (set-difference
                           (append (type-variables expr-preds) (type-variables expr-type))
                           env-tvars
                           :test #'equalp))

             (output-qual-type (qualify expr-preds expr-type))
             (output-scheme (quantify local-tvars output-qual-type)))

        ;; Like implicit bindings, we only need to apply substitutions
        ;; for the predicates generated from type inference, not
        ;; including ones in our explicit type.
        (multiple-value-bind (fundep-preds local-subs)
            (solve-fundeps env (apply-substitution local-subs preds) local-subs)
          (declare (ignore fundep-preds))

          (setf expr-type (apply-substitution local-subs expr-type))

          (let ((reduced-preds
                  (remove-if-not (lambda (p)
                                   (not (entail env expr-preds p)))
                                 (apply-substitution local-subs preds))))

            (multiple-value-bind (deferred-preds retained-preds)
                (split-context env env-tvars reduced-preds local-subs)

              ;; NOTE: This is where defaulting happens
              (let* ((defaultable-preds (default-preds env (append env-tvars local-tvars) retained-preds))

                     (retained-preds (set-difference retained-preds defaultable-preds :test #'equalp)))

                (unless allow-deferred-predicates
                  (setf local-subs (compose-substitution-lists (default-subs env nil reduced-preds) local-subs)))

                (error:with-context ("definition of ~A" (car binding))
                  (when (and returns (not allow-returns))
                    (error 'unexpected-return))

                  ;; Make sure the declared scheme is not too general
                  (when (not (equalp output-scheme declared-ty))
                    (error 'type-declaration-too-general-error
                           :name (car binding)
                           :declared-type declared-ty
                           :derived-type output-scheme))

                  ;; Make sure the declared type includes all the required predicates
                  (when (not (null retained-preds))
                    (error 'weak-context-error
                           :name (car binding)
                           :declared-type output-qual-type
                           :preds retained-preds))

                  ;; Ensure that we are allowed to defer predicates (this is
                  ;; not allowable in toplevel forms)
                  (when (and (not allow-deferred-predicates)
                             deferred-preds)
                    (error 'context-reduction-failure
                           :pred (apply-substitution subs (first deferred-preds)))))

                (values output-scheme
                        (cons (car binding)
                              (apply-substitution local-subs typed-node))
                        deferred-preds
                        env
                        local-subs
                        output-qual-type
                        returns)))))))))

(defun derive-binding-type (name type expr env subs name-map)
  (declare (type ty type)
           (type node expr)
           (type environment env)
           (type substitution-list subs)
           (values typed-node ty-predicate-list substitution-list ty-list &optional))
  (let ((name (if name-map
                  (or (cdr (find name name-map :key #'car :test #'equalp))
                      (util:coalton-bug "Invalid state. Unable to find name in name map"))
                  name)))
    (error:with-context ("definition of ~A" name)
      (multiple-value-bind (new-type preds typed-node new-subs returns)
          (derive-expression-type expr env subs)
        (values typed-node
                preds
                (unify new-subs type new-type "type of binding" "type of variable")
                returns)))))


;;;
;;; Patterns
;;;

(defgeneric derive-pattern-type (pat env subs)
  (:documentation "derive the type and bindings of pattern PAT")
  (:method ((pat pattern-var) env subs)
    (declare (type environment env)
             (type substitution-list subs)
             (values ty ty-predicate-list list substitution-list))
    (let ((var-name (pattern-var-id pat))
          (var (make-variable)))
      (values
       var
       nil
       (list (cons var-name var))
       subs)))
  (:method ((pat pattern-wildcard) env subs)
    (declare (type environment env)
             (type substitution-list subs)
             (values ty ty-predicate-list list substitution-list))
    (let ((var (make-variable)))
      (values
       var
       nil
       nil
       subs)))
  (:method ((pat pattern-literal) env subs)
    (declare (type environment env)
             (type substitution-list subs)
             (values ty ty-predicate-list list substitution-list))
    (multiple-value-bind (type preds)
        (derive-literal-type (pattern-literal-value pat))
      (values
       type
       preds
       nil
       subs)))

  (:method ((pat pattern-constructor) env subs)
    (declare (type environment env)
             (type substitution-list subs)
             (values ty ty-predicate-list list substitution-list))
    (multiple-value-bind (tvars preds bindings subs)
        (derive-patterns-type (pattern-constructor-patterns pat) env subs)

      ;; Check that the constructor is known and fully applied
      (let ((constructor (lookup-constructor env (pattern-constructor-name pat) :no-error t)))
        (unless constructor
            (error 'unknown-constructor :symbol (pattern-constructor-name pat)))
        (unless (= (constructor-entry-arity constructor) (length tvars))
          (error 'invalid-constructor-arguments
                 :constructor (pattern-constructor-name pat)
                 :expected (constructor-entry-arity constructor)
                 :received (length tvars))))

      (let* ((var (make-variable))
             (constructor (lookup-value-type env (pattern-constructor-name pat)))
             (scm (fresh-inst constructor))
             (scm-type (qualified-ty-type scm))
             (scm-preds (qualified-ty-predicates scm))
             (sub-patterns (make-function-type* tvars var)))
        (values
         var
         (append preds scm-preds)
         bindings
         (unify subs scm-type sub-patterns))))))

(defun derive-patterns-type (patterns env subs)
  (declare (type list patterns)
           (type environment env)
           (type substitution-list subs)
           (values list ty-predicate-list list substitution-list))
  (let ((bindings nil)
        (preds nil)
        (type-vars nil))
    (dolist (pat patterns)
      (multiple-value-bind (tvars pat-preds new-bindings new-subs)
          (derive-pattern-type pat env subs)
        (setf type-vars (append type-vars (list tvars)))
        (setf preds (append preds pat-preds))
        (setf bindings (append bindings new-bindings))
        (setf subs new-subs)))
    (values
     type-vars
     preds
     bindings
     subs)))

;;;
;;; Match branches
;;;

(defun derive-match-branch-type (unparsed pattern expr name-map env subs)
  (declare (type t unparsed)
           (type pattern pattern)
           (type node expr)
           (type environment env)
           (type substitution-list subs)
           (values ty ty-predicate-list typed-match-branch substitution-list ty-list))

  ;; Before deriving the type of the match branch first see if any of
  ;; its variables overlap with the names of constructors
  (loop :for (_ . var) :in name-map :do
    (when (lookup-constructor env var :no-error t)
      (alexandria:simple-style-warning
       "Pattern variable ~/coalton-impl::sexp-fmt/ matches name of known ~
        constructor. If you meant to match against the constructor then ~
        use ~/coalton-impl::sexp-fmt/ instead." var (list var))))


  (multiple-value-bind (var pat-preds bindings new-subs)
      (derive-pattern-type pattern env subs)
    (let* ((bindings-schemes
             (mapcar (lambda (b)
                       (cons
                        (car b)
                        (to-scheme (qualify nil (cdr b)))))
                     bindings))
           (new-env (push-value-environment env bindings-schemes)))
      (multiple-value-bind (expr-type expr-preds typed-subexpr new-subs returns)
          (derive-expression-type expr new-env new-subs)
        (values
         (make-function-type var expr-type)
         (append pat-preds expr-preds)
         (make-typed-match-branch
          :unparsed unparsed
          :pattern pattern
          :subexpr typed-subexpr
          :bindings bindings-schemes
          :name-map name-map)
         new-subs
         returns)))))

(defun derive-match-branches-type (t_ alts env subs)
  (declare (type ty t_)
           (type list alts)
           (type environment env)
           (type substitution-list subs)
           (values typed-match-branch-list ty-predicate-list substitution-list ty-list))
  (let ((types nil)
        (preds nil)
        (typed-branches nil)
        (returns nil))
    (dolist (alt alts)

      ;; Rewrite variables in the pattern to their original names for error messages
      (let* ((pattern (match-branch-pattern alt))
             (name-map (match-branch-name-map alt))
             (m (immutable-map-set-multiple (make-immutable-map) name-map))
             (pattern (coalton-impl/ast::rewrite-pattern-vars pattern m)))

        (error:with-context ("branch ~A" pattern)
          (multiple-value-bind (ty branch-preds typed-branch new-subs new-returns)
              (derive-match-branch-type (match-branch-unparsed alt) (match-branch-pattern alt) (match-branch-subexpr alt) (match-branch-name-map alt) env subs)
            (setf subs new-subs)
            (setf types (append types (list ty)))
            (setf preds (append preds branch-preds))
            (setf typed-branches (append typed-branches (list typed-branch)))
            (setf returns (append new-returns returns))))))
    (dolist (typ types)
      (setf subs (unify subs t_ typ "match expression" "branch")))
    (values typed-branches preds subs returns)))
