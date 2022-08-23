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
  (:import-from
   #:coalton-impl/codegen/codegen-type-definition
   #:constructor-slot-name)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:ast #:coalton-impl/ast))
  (:export
   #:codegen-expression))

(in-package #:coalton-impl/codegen/codegen-expression)

(defgeneric codegen-expression (node current-function env)
  (:method ((node node-literal) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function)
             (ignore current-function env))
    (node-literal-value node))

  (:method ((node node-variable) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function)
             (ignore current-function env))
    (node-variable-value node))

  (:method ((node node-application) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    (let* ((arity (length (node-application-rands node)))

           (function-applicator
             (aref *function-application-functions* arity)))
      `(,function-applicator
        ,(codegen-expression (node-application-rator node) current-function env)
        ,@(mapcar
           (lambda (node)
             (codegen-expression node current-function env))
           (node-application-rands node)))))

  (:method ((node node-direct-application) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    `(,(node-direct-application-rator node)
      ,@(mapcar
         (lambda (node)
           (codegen-expression node current-function env))
         (node-direct-application-rands node))))

  (:method ((expr node-abstraction) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    (let* ((var-names (node-abstraction-vars expr))

           (arity (length var-names))

           (function-constructor
             (aref *function-constructor-functions* arity))

           (type-decs
             (when coalton-impl:*emit-type-annotations*
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
          (block @@local
            ,(codegen-expression (node-abstraction-subexpr expr) '@@local env))))))

  (:method ((expr node-bare-abstraction) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    (let* ((var-names (node-bare-abstraction-vars expr))

           (type-decs
             (when coalton-impl:*emit-type-annotations*
               (append
                (loop :for name :in (node-bare-abstraction-vars expr)
                      :for i :from 0
                      :for arg-ty := (nth i (tc:function-type-arguments (node-type expr)))
                      :collect `(type ,(lisp-type arg-ty env) ,name))
                (list `(values ,(lisp-type (node-type (node-bare-abstraction-subexpr expr)) env) &optional))))))

      `(lambda ,var-names
         (declare ,@type-decs
                  (ignorable ,@var-names))
         (block @@local
           ,(codegen-expression (node-bare-abstraction-subexpr expr) '@@local env)))))

  (:method ((expr node-let) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    (let ((sccs (node-binding-sccs (node-let-bindings expr))))
      (codegen-let
       expr
       sccs
       current-function
       (node-variables expr :variable-namespace-only t)
       env)))

  (:method ((expr node-lisp) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    (let ((inner
            `(let ,(mapcar
                    (lambda (var)
                      (list (car var) (cdr var)))
                    (node-lisp-vars expr))
               ,@(node-lisp-form expr))))

      (if coalton-impl:*emit-type-annotations*
          `(the (values ,(lisp-type (node-type expr) env) &optional)
                ,inner)
          inner)))

  (:method ((expr node-match) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))

    ;; If possible codegen a cl:if instead of a trivia:match
    (when (and (equalp (node-type (node-match-expr expr)) tc:*boolean-type*)
               (= 2 (length (node-match-branches expr)))
               (equalp (match-branch-pattern (first (node-match-branches expr)))
                       (ast:make-pattern-constructor :name 'coalton:True :patterns nil))
               (equalp (match-branch-pattern (second (node-match-branches expr)))
                       (ast:make-pattern-constructor :name 'coalton:False :patterns nil)))
      (return-from codegen-expression
        `(if ,(codegen-expression (node-match-expr expr) current-function env)
             ,(codegen-expression (match-branch-body (first (node-match-branches expr))) current-function env)
             ,(codegen-expression (match-branch-body (second (node-match-branches expr))) current-function env))))


    (let* ((subexpr (codegen-expression (node-match-expr expr) current-function env))

           (branches
             (mapcar
              (lambda (b)
                `(,(codegen-pattern (match-branch-pattern b) env)
                  ,(codegen-expression (match-branch-body b) current-function env)))
              (node-match-branches expr))))
      `(trivia:match
           ,(if coalton-impl:*emit-type-annotations*
             `(the ,(lisp-type (node-type (node-match-expr expr)) env) ,subexpr)
             subexpr)
         ,@branches
         (_ (error "Pattern match not exaustive error")))))

  (:method ((expr node-seq) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    `(progn
       ,@(mapcar
          (lambda (node)
            (codegen-expression node current-function env))
          (node-seq-nodes expr))))

  (:method ((expr node-return) current-function env)
    (assert (not (null current-function)))
    `(return-from ,current-function ,(codegen-expression (node-return-expr expr) current-function env)))

  (:method ((expr node-field) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    `(,(node-field-name expr)
      ,(codegen-expression (node-field-dict expr) current-function env)))

  (:method ((expr node-dynamic-extent) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    `(let ((,(node-dynamic-extent-name expr)
             ,(codegen-expression (node-dynamic-extent-node expr) current-function env)))
       (declare (dynamic-extent ,(node-dynamic-extent-name expr)))
       ,(codegen-expression (node-dynamic-extent-body expr) current-function env)))

  (:method ((expr node-bind) current-function env)
    (let ((name (node-bind-name expr)))
      (cond
        ((and (node-abstraction-p (node-bind-expr expr))
              (find name (node-variables (node-bind-body expr) :variable-namespace-only t)))
         (let* ((arity (length (node-abstraction-vars (node-bind-expr expr))))

                (function-constructor (aref *function-constructor-functions* arity)))
           `(let ((,name))
              (declare (ignorable ,name))
              (labels ((,name
                           ,(node-abstraction-vars (node-bind-expr expr))
                         ;; TODO: add type annotations
                         (declare (ignorable ,@(node-abstraction-vars (node-bind-expr expr))))
                         ,(codegen-expression (node-abstraction-subexpr (node-bind-expr expr)) name env)))
                (setf ,name (,function-constructor #',name))
                ,(codegen-expression (node-bind-body expr) current-function env)))))

        ((node-abstraction-p (node-bind-expr expr))
         `(labels ((,name
                       ,(node-abstraction-vars (node-bind-expr expr))
                     ;; TODO: add type annotations
                     (declare (ignorable ,@(node-abstraction-vars (node-bind-expr expr))))
                     ,(codegen-expression (node-abstraction-subexpr (node-bind-expr expr)) name env)))
            ,(codegen-expression (node-bind-body expr) current-function env)))

        (t
         `(let ((,name ,(codegen-expression (node-bind-expr expr) current-function env)))
            (declare (ignorable ,name))
            ,(codegen-expression (node-bind-body expr) current-function env)))))))

(defun find-constructor (initform env)
  (if (or (node-application-p initform) (node-direct-application-p initform))
      (and (node-rator-name initform)
           (tc:lookup-constructor env (node-rator-name initform) :no-error t))
      nil))

(defun data-letrec-able-p (initform env)
  (let ((ctor-ent (find-constructor initform env)))
    (and ctor-ent
         (let* ((type-name (tc:constructor-entry-constructs ctor-ent))
                (type-ent (tc:lookup-type env type-name)))
           (not (or (tc:type-entry-enum-repr type-ent)
                    (tc:type-entry-newtype type-ent)))))))

(declaim (ftype (function (tc:constructor-entry unsigned-byte symbol)
                          (values list &optional))
                setf-accessor))
(defun setf-accessor (ctor-ent nth-slot instance)
  (if (eq (tc:constructor-entry-name ctor-ent) 'coalton:Cons)
      (ecase nth-slot
        (0 `(car ,instance))
        (1 `(cdr ,instance)))
      `(slot-value ,instance ',(constructor-slot-name ctor-ent nth-slot))))

(defun codegen-let (node sccs current-function local-vars env)
  (declare (type node-let node)
           (type list sccs)
           (type (or null symbol) current-function)
           (type symbol-list local-vars)
           (type tc:environment env))

  (when (null sccs)
    (return-from codegen-let (codegen-expression (node-let-subexpr node) current-function env)))

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

              (inner (codegen-let node (cdr sccs) current-function local-vars env))

              (inner
                (if binding-names-vars
                    (append
                     (loop :for (name . node) :in scc-bindings
                           :for arity := (length (node-abstraction-vars node))
                           :for function-constructor := (aref *function-constructor-functions* arity)
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
                                           ;; TODO: add type annotations
                                           (declare (ignorable ,@(node-abstraction-vars node)))
                                           ,(codegen-expression (node-abstraction-subexpr node) name env)))
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

      ((every (lambda (pair)
                (data-letrec-able-p (cdr pair)
                                                env))
              scc-bindings)
       (let* ((inner (codegen-let node (cdr sccs) current-function local-vars env))
              (assignments (loop :for (name . initform) :in scc-bindings
                                 :for ctor-info := (find-constructor initform env)
                                 :appending (loop :for arg :in (node-rands initform)
                                                  :for i :from 0
                                                  :collect `(setf ,(setf-accessor ctor-info i name)
                                                                  ,(codegen-expression arg current-function env)))))
              (allocations (loop :for (name . initform) :in scc-bindings
                                 :for ctor-info := (find-constructor initform env)
                                 :collect `(,(node-rator-name initform)
                                            ,@(mapcar (constantly nil) (node-direct-application-rands initform))))))
         `(let ,(mapcar (lambda (scc-binding allocation)
                          (list (car scc-binding) allocation))
                        scc-bindings allocations)
            ,@assignments
            ,inner)
         ))

      ;; Single variable binding
      ((= 1 (length scc-bindings))
       (let ((name (car (first scc-bindings)))
             (node_ (cdr (first scc-bindings))))

         `(let ((,name ,(codegen-expression node_ current-function env)))
            ,(codegen-let node (cdr sccs) current-function local-vars env))))

      (t (error "Invalid scc binding group. This should have been detected during typechecking.")))))
