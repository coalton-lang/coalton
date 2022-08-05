(defpackage #:coalton-impl/codegen/compile-expression
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/codegen/ast
   #:coalton-impl/codegen/resolve-instance)
  (:import-from
   #:coalton-impl/codegen/typecheck-node
   #:typecheck-node)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:compile-toplevel
   #:compile-expression))

(in-package #:coalton-impl/codegen/compile-expression)

(defun compile-toplevel (inferred-type expr env &key (extra-context nil) (bare-abstraction nil))
  (declare (type tc:qualified-ty inferred-type)
           (type tc:typed-node expr)
           (type tc:environment env)
           (type pred-context extra-context)
           (values node &optional))

  (let* (;; Match the inferred-type against the node-type. This makes
         ;; the type variables in ctx match those in the ast node.
         (inferred-type-ty (tc:qualified-ty-type inferred-type))

         (inferred-type-preds (tc:qualified-ty-predicates inferred-type))

         (node-type (tc:qualified-ty-type (tc:fresh-inst (tc:typed-node-type expr))))

         (subs (tc:match inferred-type-ty node-type))

         (preds (tc:apply-substitution subs inferred-type-preds))

         (ctx
           (loop :for pred in preds
                 :collect (cons pred (gensym))))

         (full-ctx
           (append ctx extra-context))

         (node-abstraction_
           (if bare-abstraction
               #'make-node-bare-abstraction
               #'make-node-abstraction)))

    (let ((node
            (cond
              ;;
              ;; Reorder predicates to match definition order
              ;;
              ((tc:typed-node-abstraction-p expr)
               (let ((subnode (compile-expression (tc:typed-node-abstraction-subexpr expr) full-ctx env)))
                 (funcall node-abstraction_
                          :type (tc:make-function-type*
                                 (append
                                  (loop :for pred :in preds
                                        :collect (pred-type pred env))
                                  (loop :for (name . scheme) :in (tc:typed-node-abstraction-vars expr)
                                        :collect (tc:qualified-ty-type (tc:ty-scheme-type scheme))))
                                 (node-type subnode))
                          :vars (append
                                 (mapcar #'cdr ctx)
                                 (mapcar #'car (tc:typed-node-abstraction-vars expr)))
                          :subexpr subnode)))

              ;; Nodes that are not abstractions but are compiled to bare abstractions must be wrapped
              ((and bare-abstraction (tc:function-type-p inferred-type))
               (let* ((ty-args (tc:function-type-arguments inferred-type-ty))
                      (args (alexandria:make-gensym-list (length ty-args))))
                 (make-node-bare-abstraction
                  :type inferred-type-ty
                  :vars args
                  :subexpr (make-node-application
                            :type (tc:function-return-type inferred-type-ty)
                            :rator (compile-expression expr full-ctx env)
                            :rands (loop :for arg :in args
                                         :for ty :in (tc:function-type-arguments inferred-type-ty)
                                         :collect (make-node-variable :type ty
                                                                      :value arg))))))

              (ctx
               (let ((inner (compile-expression expr full-ctx env)))
                 (funcall node-abstraction_
                          :type (tc:make-function-type*
                                 (loop :for pred :in preds
                                       :collect (pred-type pred env))
                                 (node-type inner))
                          :vars (mapcar #'cdr ctx)
                          :subexpr inner)))

              (t
               (compile-expression expr full-ctx env)))))

      (typecheck-node node env)
      node)))

(defun apply-dicts (expr ctx env)
  (declare (type tc:typed-node expr)
           (type pred-context ctx)
           (type tc:environment env)
           (values node))
  (let* ((qual-ty (tc:fresh-inst (tc:typed-node-type expr)))

         (dicts (mapcar
                 (lambda (pred)
                   (resolve-dict pred ctx env))
                 (tc:qualified-ty-predicates qual-ty)))

         (dict-types (mapcar #'node-type dicts))

         (var-type (tc:make-function-type*
                    dict-types
                    (tc:qualified-ty-type qual-ty)))

         (inner-node
           (typecase expr
             (tc:typed-node-variable (make-node-variable :type var-type
                                                         :value (tc:typed-node-variable-name expr)))
             (t (compile-expression expr ctx env)))))

    (if (null dicts)
        inner-node
        (make-node-application :type (tc:qualified-ty-type qual-ty)
                               :rator inner-node
                               :rands dicts))))

(defgeneric compile-expression (expr ctx env)
  (:method ((expr tc:typed-node-literal) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (let ((qual-ty (tc:fresh-inst (tc:typed-node-type expr))))
      (assert (null (tc:qualified-ty-predicates qual-ty)))
      (make-node-literal :type (tc:qualified-ty-type qual-ty)
                         :value (tc:typed-node-literal-value expr))))

  (:method ((expr tc:typed-node-variable) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node &optional))
    (apply-dicts expr ctx env))

  (:method ((expr tc:typed-node-application) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (let ((qual-ty (tc:fresh-inst (tc:typed-node-type expr))))
      (assert (null (tc:qualified-ty-predicates qual-ty)))
      (make-node-application :type (tc:qualified-ty-type qual-ty)
                             :rator (compile-expression (tc:typed-node-application-rator expr) ctx env)
                             :rands (mapcar
                                     (lambda (expr)
                                       (apply-dicts expr ctx env))
                                     (tc:typed-node-application-rands expr)))))

  (:method ((expr tc:typed-node-abstraction) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (let* ((qual-ty (tc:fresh-inst (tc:typed-node-type expr)))

           (preds (tc:qualified-ty-predicates qual-ty))

           (dict-var-names (loop :for pred :in preds
                                 :collect (gensym)))

           (dict-types (loop :for pred :in preds
                             :collect (pred-type pred env)))

           (ctx (append (loop :for pred :in preds
                              :for name :in dict-var-names
                              :collect (cons pred name))
                        ctx))

           (vars (append
                  dict-var-names
                  (loop :for (name . scheme) :in (tc:typed-node-abstraction-vars expr)
                        :collect
                        (let ((qual-ty (tc:fresh-inst scheme)))
                          (assert (null (tc:qualified-ty-predicates qual-ty)))
                          name)))))

      (assert (not (some #'tc:static-predicate-p preds)))
      (make-node-abstraction
       :type (tc:make-function-type* dict-types (tc:qualified-ty-type qual-ty))
       :vars vars
       :subexpr (compile-expression (tc:typed-node-abstraction-subexpr expr) ctx env))))

  (:method ((expr tc:typed-node-let) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (let ((qual-ty (tc:fresh-inst (tc:typed-node-type expr))))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-let
       :type (tc:qualified-ty-type qual-ty)
       :bindings (loop :for (name . node) :in (tc:typed-node-let-bindings expr)
                       :for explicit-type := (gethash name (tc:typed-node-let-explicit-types expr))
                       :if explicit-type
                         :collect (cons name (compile-toplevel explicit-type node env :extra-context ctx))
                       :else
                         :collect (cons name (compile-expression node ctx env)))
       :subexpr (compile-expression (tc:typed-node-let-subexpr expr) ctx env))))

  (:method ((expr tc:typed-node-lisp) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (let ((qual-ty (tc:fresh-inst (tc:typed-node-type expr))))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-lisp :type (tc:qualified-ty-type qual-ty)
                      :vars (tc:typed-node-lisp-variables expr)
                      :form (tc:typed-node-lisp-form expr))))

  (:method ((expr tc:typed-match-branch) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values match-branch))
    (make-match-branch
     :pattern (tc:typed-match-branch-pattern expr)
     :bindings (loop :for (name . scheme) :in (tc:typed-match-branch-bindings expr)
                     :collect
                     (let ((qual-ty (tc:fresh-inst scheme)))
                       (assert (null (tc:qualified-ty-predicates qual-ty)))
                       (cons name (tc:qualified-ty-type qual-ty))))
     :body (compile-expression (tc:typed-match-branch-subexpr expr) ctx env)))

  (:method ((expr tc:typed-node-match) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node-match))
    (let ((qual-ty (tc:fresh-inst (tc:typed-node-type expr))))
      (assert (null (tc:qualified-ty-predicates qual-ty)))
      (make-node-match
       :type (tc:qualified-ty-type qual-ty)
       :expr (compile-expression (tc:typed-node-match-expr expr) ctx env)
       :branches (mapcar
                  (lambda (branch)
                    (compile-expression branch ctx env))
                  (tc:typed-node-match-branches expr)))))

  (:method ((expr tc:typed-node-seq) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (assert (not (null (tc:typed-node-seq-subnodes expr))))
    (let ((qual-ty (tc:fresh-inst (tc:typed-node-type expr))))
      (assert (null (tc:qualified-ty-predicates qual-ty)))
      (make-node-seq
       :type (tc:qualified-ty-type qual-ty)
       :nodes (mapcar
               (lambda (node)
                 (compile-expression node ctx env))
               (tc:typed-node-seq-subnodes expr)))))

  (:method ((expr tc:typed-node-return) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (let ((qual-ty (tc:fresh-inst (tc:typed-node-type expr))))
      (assert (null (tc:qualified-ty-predicates qual-ty)))
      (make-node-return
       :type (tc:qualified-ty-type qual-ty)
       :expr (compile-expression (tc:typed-node-return-expr expr) ctx env))))

  (:method ((expr tc:typed-node-bind) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (let ((qual-ty (tc:fresh-inst (tc:typed-node-type expr))))
      (assert (null (tc:qualified-ty-predicates qual-ty)))
      (make-node-bind
       :type (tc:qualified-ty-type qual-ty)
       :name (tc:typed-node-bind-name expr)
       :expr (compile-expression (tc:typed-node-bind-expr expr) ctx env)
       :body (compile-expression (tc:typed-node-bind-body expr) ctx env)))))

