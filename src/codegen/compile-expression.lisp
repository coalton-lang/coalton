(in-package #:coalton-impl/codegen)

(defvar *emit-type-annotations* t)

(defgeneric compile-expression (expr ctx env)
  (:documentation "Compile coalton expression into lisp code")

  (:method ((expr typed-node-literal) ctx env)
    (typed-node-literal-value expr))

  (:method ((expr typed-node-variable) ctx env)
    (let* ((preds (remove-duplicates (scheme-predicates (typed-node-type expr)) :test #'equalp))
           (function-application (gethash (length preds) *function-application-functions*)))
      (if preds
          `(,function-application ,(typed-node-variable-name expr)
                                  ,@(compile-typeclass-dicts preds ctx env))
          (typed-node-variable-name expr))))

  (:method ((expr typed-node-application) ctx env)
    (let* ((arity (length (typed-node-application-rands expr)))
           ;; NOTE: We will not apply predicates to the rator if it is
           ;;       an application because we can assume it will
           ;;       already apply the arguments.
           (preds (and (not (coalton-impl/typechecker::typed-node-application-p
                             (typed-node-application-rator expr)))
                       (remove-duplicates
                        (scheme-predicates
                         (typed-node-type (typed-node-application-rator expr)))
                        :test #'equalp)))
           (num-preds (length preds))
           (function-application (gethash (+ arity num-preds) *function-application-functions*))
           (rator (typed-node-application-rator expr)))
      (unless function-application
        (coalton-impl::coalton-bug "Unable to apply function of arity ~A" arity))

      `(,function-application
        ,(if (coalton-impl/typechecker::typed-node-variable-p rator)
             (typed-node-variable-name rator)
             (compile-expression rator ctx env))
        ,@(compile-typeclass-dicts preds
                                   ctx env)
        ,@(mapcar (lambda (expr) (compile-expression expr ctx env))
                  (typed-node-application-rands expr)))))

  (:method ((expr typed-node-direct-application) ctx env)
    `(,(typed-node-direct-application-rator expr)
      ,@(compile-typeclass-dicts (scheme-predicates
                                  (typed-node-direct-application-rator-type expr))
                                 ctx env)
      ,@(mapcar
         (lambda (expr) (compile-expression expr ctx env))
         (typed-node-direct-application-rands expr))))

  (:method ((expr typed-node-abstraction) ctx env)
    (let* ((lambda-expr `(lambda ,(mapcar #'car (typed-node-abstraction-vars expr))
                           (declare (ignorable ,@(mapcar #'car (typed-node-abstraction-vars expr)))
                                    ,@(when *emit-type-annotations*
                                        `(,@(mapcar (lambda (var) `(type ,(lisp-type (cdr var) env) ,(car var)))
                                                    (typed-node-abstraction-vars expr))
                                          (values ,(lisp-type (typed-node-abstraction-subexpr expr) env) &optional))))
                           ,(compile-expression (typed-node-abstraction-subexpr expr) ctx env)))
           (arity (length (typed-node-abstraction-vars expr))))
      (cond
        ((= 1 arity)
         lambda-expr)
        (t
         (let ((function-constructor (gethash arity *function-constructor-functions*)))
           (unless function-constructor
             (coalton-impl::coalton-bug "Unable to construct function of arity ~A" arity))
           `(,function-constructor ,lambda-expr))))))

  (:method ((expr typed-node-let) ctx env)
    (compile-binding-list (typed-node-let-bindings expr)
                          (typed-node-let-sorted-bindings expr)
                          (typed-node-let-subexpr expr)
                          (typed-node-let-dynamic-extent-bindings expr)
                          ctx
                          env))

  (:method ((expr typed-node-lisp) ctx env)
    (let ((inner
            (if *emit-type-annotations*
                `(the (values ,(lisp-type expr env) &optional) ,(typed-node-lisp-form expr))
                (typed-node-lisp-form expr))))
      (if (typed-node-lisp-variables expr)
          `(let ,(mapcar
                  (lambda (vars)
                    (list (car vars) (cdr vars)))
                  (typed-node-lisp-variables expr))
             ,inner)
          inner)))

  (:method ((expr typed-node-match) ctx env)
    (let ((subexpr (compile-expression (typed-node-match-expr expr) ctx env))
          (branches (mapcar (lambda (b)
                              `(,(compile-pattern (typed-match-branch-pattern b) env)
                                ,(compile-expression (typed-match-branch-subexpr b) ctx env)))
                            (typed-node-match-branches expr))))

      `(trivia:match (the ,(lisp-type (typed-node-match-expr expr) env) ,subexpr)
         ;; NOTE: This error intentionally has no helpful
         ;; information. The pattern exhastiveness checks should
         ;; make it so this should never be hit and this allows us
         ;; to avoid bloat with error allocations.
         ,@branches (_ (error "Pattern match not exaustive error")))))

  (:method ((expr typed-node-seq) ctx env)
    `(progn ,@(mapcar (lambda (subnode)
                        (compile-expression subnode ctx env))
                      (typed-node-seq-subnodes expr)))))

(defun reduce-preds-for-codegen (preds env)
  (reduce-context env (remove-duplicates preds :test #'equalp) nil))

(defun compile-binding-list (typed-bindings sorted-bindings subnode dynamic-extent-bindings ctx env)
  "Compiles a binding list to nested LET and LABELS based on topological sorting of the bindings."
  (declare (type typed-binding-list typed-bindings)
           (type list sorted-bindings)
           (type typed-node subnode)
           (type symbol-list dynamic-extent-bindings)
           (type list ctx)
           (type environment env)
           (values list &optional))

  (labels ((compile-sccs (sccs)
             (if (null sccs)
                 (compile-expression subnode ctx env)

                 (let* ((scc (car sccs))
                        (scc-typed-bindings
                          (mapcar
                           (lambda (b)
                             (find b typed-bindings :key #'car))
                           scc))
                        (local-dynamic-extent-bindings
                          (mapcan
                           (lambda (b)
                             (when (member b dynamic-extent-bindings :test #'equalp)
                               (list b)))
                           scc)))
                   ;; TODO: We should move some of these checks to typechecking

                   ;; NOTE: Due to constraints with the way we are able to nest
                   ;;       statements, we only allow function abstractions to be
                   ;;       mutually recursive or for a single non-recursive
                   ;;       variable to be in a group.
                   (cond
                     ;; Either everything is a function
                     ((every #'coalton-impl/typechecker::typed-node-abstraction-p (mapcar #'cdr scc-typed-bindings))
                      (let* ((function-names (mapcar #'car scc-typed-bindings))
                             (variable-namespace (append
                                                  (coalton-impl/typechecker::collect-variable-namespace subnode)
                                                  (mapcan #'coalton-impl/typechecker::collect-variable-namespace (mapcar #'cdr typed-bindings))))

                             ;; If the functions are referenced in the
                             ;; variable namespace then genenerate the
                             ;; scaffolding to declare them in both
                             ;; namespaces. Otherwise only declare
                             ;; them in the function namespace
                             (generate-function-scaffold (subsetp function-names variable-namespace :test #'equalp))

                             (labels_ `(labels ,(mapcar
                                                 (lambda (b)
                                                   (unless (null local-dynamic-extent-bindings)
                                                     (coalton-impl::coalton-bug "Functions should not be declared dynamic extent."))
                                                   (let* ((type (typed-node-type (cdr b)))
                                                          (preds (reduce-preds-for-codegen (scheme-predicates type) env))
                                                          (dict-context (mapcar (lambda (pred) (cons pred (gensym)))
                                                                                preds))
                                                          (dict-types (mapcar (lambda (dict-context)
                                                                                (cons
                                                                                 (ty-class-codegen-sym
                                                                                  (lookup-class env (ty-predicate-class (car dict-context))))
                                                                                 (cdr dict-context)))
                                                                              dict-context)))
                                                     `(,(car b)
                                                       (,@(mapcar #'cdr dict-context)
                                                        ,@(mapcar #'car (typed-node-abstraction-vars (cdr b))))

                                                       (declare (ignorable
                                                                 ,@(mapcar #'cdr dict-context)
                                                                 ,@(mapcar #'car (typed-node-abstraction-vars (cdr b))))
                                                                ,@(when *emit-type-annotations*
                                                                    `(,@(mapcar (lambda (var) `(type ,(lisp-type (cdr var) env) ,(car var)))
                                                                                (typed-node-abstraction-vars (cdr b)))
                                                                      ,@(mapcar (lambda (dict) `(type ,(car dict) ,(cdr dict))) dict-types)
                                                                      (values
                                                                       ,(lisp-type
                                                                         (typed-node-type (typed-node-abstraction-subexpr (cdr b))) env) &optional))))
                                                       ,(compile-expression
                                                         (typed-node-abstraction-subexpr (cdr b))
                                                         (append dict-context ctx) env))))
                                                 scc-typed-bindings)
                                         ,@(when generate-function-scaffold
                                            `((setf ,@(mapcan (lambda (b)
                                                                 (let* ((type (typed-node-type (cdr b)))
                                                                        (preds (reduce-preds-for-codegen (scheme-predicates type) env)))
                                                                   `(,(car b) ,(construct-function-entry
                                                                                `#',(car b)
                                                                                (+ (length (typed-node-abstraction-vars (cdr b)))
                                                                                   (length preds))))))
                                                               scc-typed-bindings))))

                                         ,(compile-sccs (cdr sccs)))))
                        (if generate-function-scaffold
                            `(let ,(mapcar (lambda (v) `(,(car v) ,(construct-function-entry `(lambda () (error "")) 1))) scc-typed-bindings)
                               (declare (ignorable ,@(mapcar #'car scc-typed-bindings))
                                        ,@(when (not (null local-dynamic-extent-bindings))
                                            `((dynamic-extent ,@local-dynamic-extent-bindings))))
                               ,labels_)
                            labels_)))

                     ;; Or we have a single non-recursive variable
                     ((and (= 1 (length scc-typed-bindings))
                           t ;; TODO: We need to check that the variable is non-self-recursive using free-variables
                           )
                      (let* ((binding (car scc-typed-bindings))
                             (name (car binding))
                             (node (cdr binding))
                             (type (typed-node-type node))

                             (preds (reduce-preds-for-codegen (scheme-predicates type) env))

                             (dict-context (mapcar (lambda (pred) (cons pred (gensym))) preds)))

                        (if (not (= 0 (length dict-context)))
                            (let ((function-constructor (gethash (length dict-context) *function-constructor-functions*)))
                              (progn
                                (unless (null local-dynamic-extent-bindings)
                                  (coalton-impl::coalton-bug "Functions should not be declared dynamic extent."))
                                `(let ((,name (,function-constructor (lambda ,(mapcar #'cdr dict-context)
                                                                       ,(compile-expression node (append dict-context ctx) env)))))
                                   ,(compile-sccs (cdr sccs)))))


                            `(let ,(mapcar (lambda (b)
                                             `(,(car b) ,(compile-expression (cdr b) ctx env)))
                                    scc-typed-bindings)
                               (declare
                                (ignorable ,@(mapcar #'car scc-typed-bindings))

                                ,@(when *emit-type-annotations*
                                    `(,@(mapcar (lambda (var) `(type ,(lisp-type (cdr var) env) ,(car var)))
                                                scc-typed-bindings)))

                                ,@(when (not (null local-dynamic-extent-bindings))
                                    `((dynamic-extent ,@local-dynamic-extent-bindings))))
                               ,(compile-sccs (cdr sccs))))))

                     (t
                      (coalton-impl::coalton-bug "Invalid SCC detected in codegen.")))))))

    ;; Verify that each binding is contained in one and only one scc
    (let ((a (mapcan (lambda (x) (copy-list x)) sorted-bindings))
          (b (mapcar #'car typed-bindings)))
      (unless (and (subsetp a b :test #'equalp)
                   (subsetp b a :test #'equalp)
                   (= (length a) (length b)))
        (coalton-impl::coalton-bug "Invalid scc in typed-node-let:~%SCC: ~A~%BINDINGS: ~A~%" sorted-bindings b)))

    (compile-sccs sorted-bindings)))
