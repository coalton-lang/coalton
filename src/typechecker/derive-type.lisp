(in-package #:coalton-impl/typechecker)

;;;
;;; Expressions
;;;

(defun derive-literal-type (value)
  (declare (values ty ty-predicate-list))
  (etypecase value
    (integer      (values tInt          nil))
    (single-float (values tSingle-Float nil))
    (double-float (values tDouble-Float nil))
    (string       (values tString       nil))
    (character    (values tChar         nil))))

(defgeneric derive-expression-type (value env substs)
  (:documentation "Derive the TYPE and generate a TYPED-NODE for expression VALUE

Returns (VALUES type predicate-list typed-node subs)")
  (:method ((value node-literal) env substs)
    (declare (type substitution-list substs)
             (values ty ty-predicate-list typed-node substitution-list &optional))
    (let ((literal-value (node-literal-value value)))
      (multiple-value-bind (type preds)
          (derive-literal-type literal-value)
        (values
         type
         preds
         (typed-node-literal
	  (to-scheme (qualify nil type))
	  (node-unparsed value)
	  literal-value)
         substs))))

  (:method ((value node-lisp) env substs)
    (declare (type environment env)
	     (type substitution-list substs)
             (values ty ty-predicate-list typed-node substitution-list &optional))
    (let* ((scheme (parse-and-resolve-type env (node-lisp-type value)))
	   (qual-type (fresh-inst scheme))
           (type (qualified-ty-type qual-type))
           (preds (qualified-ty-predicates qual-type)))
      (values type
              preds
              (typed-node-lisp
	       (to-scheme qual-type)
	       (node-unparsed value)
	       (node-lisp-variables value)
	       (node-lisp-form value))
              substs)))

  (:method ((value node-variable) env substs)
    (declare (type substitution-list substs)
             (values ty ty-predicate-list typed-node substitution-list &optional))
    (let* ((scheme (lookup-value-type env (node-variable-name value)))
	   (qual-type (fresh-inst scheme))
           (type (qualified-ty-type qual-type))
           (preds (qualified-ty-predicates qual-type)))
      (values type
              preds
              (typed-node-variable
	       (to-scheme qual-type)
	       (node-unparsed value)
	       (node-variable-name value))
              substs)))

  (:method ((value node-application) env substs)
    (declare (type environment env)
	     (type substitution-list substs)
             (values ty ty-predicate-list typed-node substitution-list &optional))

    (let* ((rator (node-application-rator value))
           (rands (node-application-rands value))
           (ret-ty (make-variable))
           (typed-rands nil))

      (when (null rands)
	(coalton-impl::coalton-bug "Invalid application with 0 arguments ~A." rator))

      (multiple-value-bind (fun-ty fun-preds typed-rator substs)
          (derive-expression-type rator env substs)
        (unless (or (tvar-p fun-ty)
                    (function-type-p fun-ty))
          (error 'invalid-operator-type-error :type fun-ty))
        (let ((arg-preds nil))
          (labels ((build-function (args)
                     (if (null args)
                         ret-ty
                         (multiple-value-bind (arg-ty arg-pred typed-rand new-substs-arg)
                             (derive-expression-type (car args) env substs)
                           (push typed-rand typed-rands)
                           (setf arg-preds (append arg-pred arg-preds))
                           (setf substs new-substs-arg)
                           (make-function-type arg-ty (build-function (cdr args)))))))
            (let* ((ftype (build-function rands))
		   (preds (append fun-preds arg-preds))
                   (substs (unify substs ftype fun-ty)))
              (values ret-ty
                      preds
                      (typed-node-application
		       (to-scheme (qualify (reduce-context env preds) ret-ty))
		       (node-unparsed value)
		       typed-rator
		       (reverse typed-rands))
                      substs)))))))

  (:method ((value node-abstraction) env substs)
    (declare (type environment env)
	     (type substitution-list substs)
             (values ty ty-predicate-list typed-node substitution-list &optional))
    (let* ((subexpr (node-abstraction-subexpr value))
	   (vars (node-abstraction-vars value))
	   (new-env (push-value-environment
                     env
                     (mapcar (lambda (var) (cons var (to-scheme (qualify nil (make-variable)))))
                             vars))))
      (multiple-value-bind (ret-ty ret-preds typed-subexpr new-substs)
          (derive-expression-type subexpr new-env substs)
        (labels ((build-function (args)
                   (if (null args)
                       ret-ty
                       (make-function-type (qualified-ty-type (fresh-inst (lookup-value-type new-env (car args))))
                                           (build-function (cdr args))))))
          (let ((ret-ty (build-function vars)))
            (values ret-ty
                    ret-preds
                    (typed-node-abstraction
                     (to-scheme (qualified-ty (apply-substitution new-substs ret-preds) ret-ty))
		     (node-unparsed value)
                     (mapcar (lambda (var)
                               (cons var (lookup-value-type new-env var)))
                             vars)
                     typed-subexpr
		     (node-abstraction-name-map value))
                    new-substs))))))

  (:method ((value node-let) env subs)
    (declare (type environment env)
	     (type substitution-list subs)
             (values ty ty-predicate-list typed-node substitution-list &optional))
    (let ((bindings (node-let-bindings value)))
      (multiple-value-bind (typed-bindings bindings-preds env subs sccs)
          ;; NOTE: If we wanted explicit types in let bindings this
          ;;       would be the place to do it.
	  (derive-bindings-type bindings nil nil env subs (node-let-name-map value))
	(multiple-value-bind (type ret-preds typed-subexpr new-subs)
            (derive-expression-type (node-let-subexpr value) env subs)
	  (let ((preds (append ret-preds bindings-preds)))
            (values type
                    preds
                    (typed-node-let
		     (to-scheme (qualify nil type))
		     (node-unparsed value)
		     typed-bindings
		     typed-subexpr sccs
		     nil
		     (node-let-name-map value))
                    new-subs))))))

  (:method ((value node-match) env subs)
    (declare (type environment env)
	     (type substitution-list subs)
             (values ty ty-predicate-list typed-node substitution-list &optional))
    (let ((tvar (make-variable)))
      (multiple-value-bind (ty preds typed-expr new-subs)
	  (derive-expression-type (node-match-expr value) env subs)
        (with-type-context ("match on ~A" (node-unparsed (node-match-expr value)))
          (multiple-value-bind (typed-branches match-preds new-subs)
              (derive-match-branches-type
	       (make-function-type ty tvar)
	       (node-match-branches value)
	       env
	       new-subs)
	    (let ((preds (append preds match-preds)))
	      (values
	       tvar
	       preds 
	       (typed-node-match
		(to-scheme (qualify nil tvar))
		(node-unparsed value)
		typed-expr
		typed-branches)
	       new-subs)))))))

  (:method ((value node-seq) env subs)
    (declare (type environment env)
	     (type substitution-list subs)
	     (values ty ty-predicate-list typed-node substitution-list &optional))
    (let* ((initial-elements (butlast (node-seq-subnodes value)))
	   (last-element (car (last (node-seq-subnodes value))))
	   (preds nil)
	   (nodes nil))


      (loop :for elem :in initial-elements :do
	(multiple-value-bind (tyvar preds_ node subs_)
	    (derive-expression-type elem env subs)
	  (declare (ignore tyvar))
	  (setf subs subs_)
	  (setf preds (append preds preds_))
	  (push node nodes)))

      (multiple-value-bind (tyvar preds_ node subs)
	  (derive-expression-type last-element env subs)
	(setf preds (append preds preds_))
	(push node nodes)
	(values
	 tyvar
	 preds
	 (typed-node-seq
	  (to-scheme (qualify nil tyvar))
	  (node-unparsed value)
	  (reverse nodes))
	 subs))))

  (:method (value env subs)
    (error "Unable to derive type of expression ~A" value)))

;;;
;;; Let Bindings
;;;

(defun derive-bindings-type (impl-bindings expl-bindings expl-declarations env subs name-map)
  "IMPL-BINDINGS and EXPL-BINDIGNS are of form (SYMBOL . EXPR)
EXPL-DECLARATIONS is a HASH-TABLE from SYMBOL to SCHEME"
  (declare (type environment env)
	   (type substitution-list subs)
	   (type list name-map)
           (values typed-binding-list ty-predicate-list environment substitution-list list &optional))

  ;; Push all the explicit type declarations on to the environment
  (let* ((expl-binds (mapcar (lambda (b)
                               (cons (car b)
                                     (gethash (car b) expl-declarations)))
                             expl-bindings))
         (env (push-value-environment env expl-binds))
         (typed-bindings nil)
         (preds nil))

    ;; First we will be checking the implicit bindings
    ;; Sort the bindings into strongly connected components of mutually
    ;; recursive bindings
    (let ((sccs (reverse (tarjan-scc (bindings-to-dag impl-bindings)))))
      (dolist (scc sccs)
        ;; Lookup all bindings in this scc
        (let ((scc-bindings (mapcar (lambda (b) (find b impl-bindings :key #'car)) scc)))
          ;; Derive the type of all parts of the scc together
          (multiple-value-bind (typed-impl-bindings impl-preds new-env new-subs)
              (derive-impls-type scc-bindings env subs name-map)
            ;; Update the current environment and substitutions
            (setf env new-env
                  subs new-subs)
            ;; Add the typed binding nodes to the output typed nodes
            (setf typed-bindings (append typed-bindings typed-impl-bindings))
            (setf preds (append impl-preds preds)))))

      ;; Now that we have the implicit bindings sorted out, we can type
      ;; check the explicit ones.
      (dolist (binding expl-bindings)
        ;; Derive the type of the binding
        (multiple-value-bind (ty-scheme typed-expl-binding expl-preds new-env new-subs)
            (derive-expl-type binding
                              (gethash (car binding) expl-declarations)
                              env subs name-map)
          (declare (ignore ty-scheme))
          ;; Update the current environment and substitutions
          (setf env new-env
                subs new-subs)
          ;; Add the binding to the list of bindings
          (push typed-expl-binding typed-bindings)
          (setf preds (append preds expl-preds))))

      (validate-bindings-for-codegen typed-bindings)

      (values typed-bindings preds env subs sccs))))

(defun validate-bindings-for-codegen (bindings)
  "Some coalton forms can be typechecked but cannot currently be codegened into valid lisp."
  (declare (type typed-binding-list bindings))

  (loop :for (name . node) :in bindings :do
    (unless (typed-node-abstraction-p node)
      (when (member name (collect-variable-namespace node) :test #'equalp)
	(error 'self-recursive-variable-definition :name name)))))

(defun derive-binding-type-seq (names tvars exprs env subs name-map)
  (declare (type tvar-list tvars)
           (type node-list exprs)
           (type environment env)
           (type substitution-list subs)
	   (type list name-map)
           (values typed-node-list ty-predicate-list substitution-list))
  (let* ((preds nil)
         (typed-nodes
           (loop :for name :in names
                 :for tvar :in tvars
	         :for expr :in exprs
	         :collect
                 (multiple-value-bind (typed-node binding-preds new-subs)
                     (derive-binding-type name tvar expr env subs name-map)
                   (setf subs new-subs)
                   (setf preds (append preds binding-preds))
                   typed-node))))
    (values typed-nodes preds subs)))

(defun derive-impls-type (bindings env subs name-map)
  (declare (type binding-list bindings)
           (type environment env)
           (type substitution-list subs)
	   (type list name-map)
           (values typed-binding-list ty-predicate-list environment substitution-list))
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
    (multiple-value-bind (typed-bindings local-preds local-subs)
	(derive-binding-type-seq (mapcar #'car bindings) tvars exprs local-env subs name-map)
      
      (let* ((expr-types (apply-substitution local-subs tvars)) ; ts'
             (expr-preds (apply-substitution local-subs local-preds)) ; ps'

             ;; Extract local type variables
             (env-tvars (type-variables (apply-substitution local-subs env))) ; fs
             (expr-tvars (reduce (lambda (&optional a b) (remove-duplicates (append a b) :test #'equalp :from-end t))
				 expr-types
				 :key #'type-variables)) ; vss
             (local-tvars (set-difference expr-tvars
                                          env-tvars)) ; gs
             )
        (multiple-value-bind (deferred-preds retained-preds)
            (split-context env env-tvars expr-preds)
          (labels ((restricted (bindings)
                     (some (lambda (b) (not (coalton-impl/ast::node-abstraction-p (cdr b))))
                           bindings)))


	    ;; NOTE: This is where the monomorphism restriction happens

            (if (restricted bindings)
                (let* ((allowed-tvars (set-difference local-tvars (type-variables retained-preds)))
                       ;; Quantify local type variables
                       (output-schemes (mapcar (lambda (type)
			                         (quantify allowed-tvars (qualified-ty nil type)))
			                       expr-types))

                       ;; Build new env
                       (output-env (push-value-environment
                                    env
                                    (mapcar (lambda (b sch)
                                              (cons (first b) sch))
                                            bindings output-schemes))))
                  (values
                   (mapcar (lambda (b typed) (cons (car b) typed)) bindings typed-bindings)
                   (append deferred-preds retained-preds)
                   output-env
                   local-subs))
                (let* (;; Quantify local type variables
                       (output-schemes (mapcar (lambda (type)
                                                 ;; Here we need to manually qualify the
                                                 ;; type (without calling QUALIFY) since the
                                                 ;; substitutions have not been fully applied yet.
			                         (quantify local-tvars (qualified-ty retained-preds type)))
			                       expr-types))

                       ;; Build new env
                       (output-env (push-value-environment
                                    env
                                    (mapcar (lambda (b sch)
                                              (cons (first b) sch))
                                            bindings output-schemes))))
                  (values
                   ;; Strip off any retained predicates from bindings
                   ;; so that bindings don't have unnecessary parameters
                   ;; for predicates
                   (mapcar (lambda (b node)
                             (let* ((node-qual-type (fresh-inst (typed-node-type node)))
                                    (node-type (qualified-ty-type node-qual-type))
                                    (node-preds (qualified-ty-predicates node-qual-type))
                                    (new-preds (remove-if
                                                (lambda (p)
                                                  (member p deferred-preds :test #'equalp))
                                                node-preds)))
                               (cons (car b)
                                     (replace-node-type node (to-scheme (qualify new-preds node-type)))
                                     )))
                           bindings typed-bindings)
                   deferred-preds
                   output-env
                   local-subs)))))))))

(defun derive-expl-type (binding declared-ty env subs name-map)
  (declare (type cons binding)
           (type ty-scheme declared-ty)
           (type environment env)
           (type substitution-list subs)
	   (type list name-map)
           (values ty-scheme cons ty-predicate-list environment substitution-list))
  (let* (;; Generate fresh instance of declared type
         (fresh-qual-type (fresh-inst declared-ty))
         (fresh-type (qualified-ty-type fresh-qual-type))
         (fresh-preds (qualified-ty-predicates fresh-qual-type)))

    ;; Derive the type of the expression, unifying with the
    ;; declared type
    (multiple-value-bind (typed-node preds local-subs)
        (derive-binding-type
         (car binding)
         fresh-type
         (cdr binding)
         env subs name-map)
      
      (let* ((expr-type (apply-substitution local-subs fresh-type))
             (expr-preds (apply-substitution local-subs fresh-preds))

             ;; Extract local type variables
             (env-tvars (type-variables (apply-substitution local-subs env)))
             (local-tvars (set-difference (type-variables expr-type) env-tvars))

             (output-qual-type (qualify expr-preds expr-type))
             (output-scheme (quantify local-tvars output-qual-type))
             (reduced-preds (remove-if-not (lambda (p)
                                             (not (entail env expr-preds p)))
                                           (apply-substitution local-subs preds))))

        (multiple-value-bind (deferred-preds retained-preds)
            (split-context env env-tvars reduced-preds)
          
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
                   :declared-type declared-ty
                   :preds retained-preds))

          (values output-scheme
                  (cons (car binding)
                        (replace-node-type (apply-substitution local-subs typed-node)
                                           (to-scheme (qualified-ty
                                                       (append
                                                        (remove-if-not (lambda (p)
                                                                         (member p expr-preds :test #'equalp))
                                                                       (apply-substitution local-subs preds))
                                                        (remove-if-not (lambda (p)
                                                                         (not (super-entail env expr-preds p)))
                                                                       (apply-substitution local-subs preds))
                                                        (remove-if-not (lambda (p)
                                                                         (not (super-entail env (apply-substitution local-subs preds) p)))
                                                                       expr-preds))
                                                       
                                                       expr-type))))
                  deferred-preds env local-subs
                  output-qual-type))))))

(defun derive-binding-type (name type expr env subs name-map)
  (declare (type ty type)
	   (type node expr)
	   (type environment env)
	   (type substitution-list subs)
	   (values typed-node ty-predicate-list substitution-list &optional))
  (let ((name (if name-map
		  (or (cdr (find name name-map :key #'car :test #'equalp))
		      (coalton-impl::coalton-bug "Invalid state. Unable to find name in name map"))
		  name)))
    (with-type-context ("definition of ~A" name)
      (multiple-value-bind (new-type preds typed-node new-subs)
          (derive-expression-type expr env subs)
	(values typed-node preds (unify new-subs type new-type))))))


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
        (derive-literal-type (node-literal-value (pattern-literal-value pat)))
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
	   (values ty ty-predicate-list typed-match-branch substitution-list))

  ;; Before deriving the type of the match branch first see if any of
  ;; its variables overlap with the names of constructors
  (loop :for (_ . var) :in name-map :do
    (when (lookup-constructor env var :no-error t)
      (alexandria:simple-style-warning "Pattern variable ~S matches name of known constructor. If
      you meant to match against the constructor then use (~S)" var
      var)))


  (multiple-value-bind (var pat-preds bindings new-subs)
      (derive-pattern-type pattern env subs)
    (let* ((bindings-schemes
             (mapcar (lambda (b)
                       (cons
                        (car b)
                        (to-scheme (qualify nil (cdr b)))))
                     bindings))
           (new-env (push-value-environment env bindings-schemes)))
      (multiple-value-bind (expr-type expr-preds typed-subexpr new-subs)
          (derive-expression-type expr new-env new-subs)
        (values
         (make-function-type var expr-type)
         (append pat-preds expr-preds)
         (typed-match-branch
	  unparsed
	  pattern
	  typed-subexpr
	  bindings-schemes
	  name-map)
         new-subs)))))

(defun derive-match-branches-type (t_ alts env subs)
  (declare (type ty t_)
	   (type list alts)
	   (type environment env)
	   (type substitution-list subs)
	   (values typed-match-branch-list ty-predicate-list substitution-list))
  (let ((types nil)
        (preds nil)
        (typed-branches nil))
    (dolist (alt alts)

      ;; Rewrite variables in the pattern to their original names for error messages
      (let* ((pattern (match-branch-pattern alt))
	     (name-map (match-branch-name-map alt))
	     (sr (shadow-realm-push-frame (make-shadow-realm) name-map))
	     (pattern (coalton-impl/ast::rewrite-pattern-vars pattern sr)))

	(with-type-context ("branch ~A" pattern)
          (multiple-value-bind (ty branch-preds typed-branch new-subs)
	      (derive-match-branch-type (match-branch-unparsed alt) (match-branch-pattern alt) (match-branch-subexpr alt) (match-branch-name-map alt) env subs)
	    (setf subs new-subs)
	    (setf types (append types (list ty)))
            (setf preds (append preds branch-preds))
            (setf typed-branches (append typed-branches (list typed-branch)))))))
    (dolist (typ types)
      (setf subs (unify subs t_ typ)))
    (values typed-branches preds subs)))
