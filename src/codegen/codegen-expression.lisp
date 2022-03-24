(defpackage #:coalton-impl/codegen/codegen-expression
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/lisp-type
   #:lisp-type)
  (:import-from
   #:coalton-impl/codegen/function-entry
   #:construct-function-entry
   #:*function-application-functions*
   #:*function-constructor-functions*)
  (:import-from
   #:coalton-impl/codegen/codegen-pattern
   #:codegen-pattern)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:ast #:coalton-impl/ast))
  (:export
   #:codegen-expression
   #:*emit-type-annotations*))

(in-package #:coalton-impl/codegen/codegen-expression)

(defvar *emit-type-annotations* t)

(defgeneric codegen-expression (node env)
  (:method ((node node-literal) env)
    (declare (type tc:environment env)
             (ignore env))
    (node-literal-value node))

  (:method ((node node-variable) env)
    (declare (type tc:environment env)
             (ignore env))
    (node-variable-value node))

  (:method ((node node-application) env)
    (declare (type tc:environment env))
    (let* ((arity (length (node-application-rands node)))

           (function-applicator
             (gethash arity *function-application-functions*)))

      `(,function-applicator
        ,(codegen-expression (node-application-rator node) env)
        ,@(mapcar
           (lambda (node)
             (codegen-expression node env))
           (node-application-rands node)))))

  (:method ((node node-direct-application) env)
    (declare (type tc:environment env))
    `(,(node-direct-application-rator node)
      ,@(mapcar
         (lambda (node)
           (codegen-expression node env))
         (node-direct-application-rands node))))

  (:method ((expr node-abstraction) env)
    (declare (type tc:environment env))
    (let* ((var-names (node-abstraction-vars expr))

           (arity (length var-names))

           (function-constructor
             (gethash arity *function-constructor-functions*))

           (type-decs
             (when *emit-type-annotations*
               (append
                (loop :for name :in (node-abstraction-vars expr)
                      :for i :from 0
                      :for arg-ty := (nth i (tc:function-type-arguments (node-type expr)))
                      :collect `(type ,(lisp-type arg-ty env) ,name))
                (list `(values ,(lisp-type (node-type (node-abstraction-subexpr expr)) env) &optional))))))

      `(,function-constructor
        (lambda ,var-names
          (declare ,@type-decs
                   (ignorable ,@var-names))
          ,(codegen-expression (node-abstraction-subexpr expr) env)))))

  (:method ((expr node-let) env)
    (declare (type tc:environment env))
    (let ((sccs (node-binding-sccs (node-let-bindings expr))))
      (codegen-let
       expr
       sccs
       (node-variables expr :variable-namespace-only t)
       env)))

  (:method ((expr node-lisp) env)
    (let ((inner
            `(let ,(mapcar
                    (lambda (var)
                      (list (car var) (cdr var)))
                    (node-lisp-vars expr))
               ,@(node-lisp-form expr))))

      (if *emit-type-annotations*
          `(the (values ,(lisp-type (node-type expr) env) &optional)
                ,inner)
          inner)))

  (:method ((expr node-match) env)
    (declare (type tc:environment env))

    ;; If possible codegen a cl:if instead of a trivia:match
    (when (and (equalp (node-type (node-match-expr expr)) tc:*boolean-type*)
               (= 2 (length (node-match-branches expr)))
               (equalp (match-branch-pattern (first (node-match-branches expr)))
                       (ast:pattern-constructor 'coalton:True nil))
               (equalp (match-branch-pattern (second (node-match-branches expr)))
                       (ast:pattern-constructor 'coalton:False nil)))
      (return-from codegen-expression
        `(if ,(codegen-expression (node-match-expr expr) env)
             ,(codegen-expression (match-branch-body (first (node-match-branches expr))) env)
             ,(codegen-expression (match-branch-body (second (node-match-branches expr))) env))))


    (let* ((subexpr (codegen-expression (node-match-expr expr) env))

           (branches
             (mapcar
              (lambda (b)
                `(,(codegen-pattern (match-branch-pattern b) env)
                  ,(codegen-expression (match-branch-body b) env)))
              (node-match-branches expr))))
      `(trivia:match (the ,(lisp-type (node-type (node-match-expr expr)) env) ,subexpr)
         ,@branches
         (_ (error "Pattern match not exaustive error")))))

  (:method ((expr node-seq) env)
    (declare (type tc:environment env))
    `(progn
       ,@(mapcar
          (lambda (node)
            (codegen-expression node env))
          (node-seq-nodes expr)))))

(defun codegen-let (node sccs local-vars env)
  (declare (type node-let node)
           (type list sccs)
           (type symbol-list local-vars)
           (type tc:environment env))

  (when (null sccs)
    (return-from codegen-let (codegen-expression (node-let-subexpr node) env)))

  (let* ((scc (car sccs))
         (scc-bindings
           (remove-if-not
            (lambda (binding)
              (find (car binding) scc))
            (node-let-bindings node))))

    (cond
      ;; Function binding group
      ((every #'node-abstraction-p (mapcar #'cdr scc-bindings))
       (let* ( ;; functions in this scc referenced in the variable namespace
              (binding-names-vars (intersection (mapcar #'car scc-bindings) local-vars))

              (inner (codegen-let node (cdr sccs) local-vars env))

              (inner
                (if binding-names-vars
                    (append
                     (loop :for (name . node) :in scc-bindings
                           :for arity := (length (node-abstraction-vars node))
                           :for function-constructor := (gethash arity *function-constructor-functions*)
                           :if (find name binding-names-vars :test #'equalp)
                             :collect `(setf
                                        ,name
                                        (,function-constructor #',name)))
                     (list inner))
                    (list inner)))

              (inner
                `(labels ,(loop :for (name . node) :in scc-bindings
                                :collect `(,name
                                           ,(node-abstraction-vars node)
                                           (declare (ignorable ,@(node-abstraction-vars node)))
                                           ,(codegen-expression (node-abstraction-subexpr node) env)))
                   ,@inner))

              (node
                (if binding-names-vars
                    `(let
                         ,(loop :for (name . node) :in scc-bindings
                                :if (find name binding-names-vars :test #'equalp)
                                  :collect name)
                       ,inner)
                    inner)))
         node))

      ;; Single variable binding
      ((= 1 (length scc-bindings))
       (let ((name (car (first scc-bindings)))
             (node_ (cdr (first scc-bindings))))

         `(let ((,name ,(codegen-expression node_ env)))
            ,(codegen-let node (cdr sccs) local-vars env))))

      (t (error "Invalid scc binding group. This should have been detected during typechecking.")))))
