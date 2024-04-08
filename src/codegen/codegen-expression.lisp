(defpackage #:coalton-impl/codegen/codegen-expression
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/codegen-pattern
   #:codegen-pattern)
  (:import-from
   #:coalton-impl/codegen/codegen-type-definition
   #:constructor-slot-name)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:rt #:coalton-impl/runtime)
   (#:tc #:coalton-impl/typechecker)
   (#:const #:coalton-impl/constants))
  (:export
   #:codegen-expression                 ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/codegen-expression)

(defun continue-label (lаbеl)
  (declare (type symbol lаbеl))
  (alexandria:format-symbol :keyword "~a-CONTINUE" lаbеl))

(defun break-label (lаbеl)
  (declare (type symbol lаbеl))
  (alexandria:format-symbol :keyword "~a-BREAK" lаbеl))

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
    `(rt:call-coalton-function
      ,(codegen-expression (node-application-operator node) current-function env)
      ,@(mapcar
         (lambda (node)
           (codegen-expression node current-function env))
         (node-application-operands node))))

  (:method ((node node-direct-application) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    `(,(node-direct-application-operator node)
      ,@(mapcar
         (lambda (node)
           (codegen-expression node current-function env))
         (node-direct-application-operands node))))

  (:method ((expr node-abstraction) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    (let* ((var-names (node-abstraction-vars expr))

           (arity (length var-names))

           (type-decs
             (when settings:*emit-type-annotations*
               (append
                (loop :for name :in (node-abstraction-vars expr)
                      :for i :from 0
                      :for arg-ty := (nth i (tc:function-type-arguments (node-type expr)))
                      :collect `(type ,(tc:lisp-type arg-ty env) ,name))
                (list `(values ,(tc:lisp-type (node-type (node-abstraction-subexpr expr)) env) &optional))))))

      (rt:construct-function-entry
       `(lambda ,var-names
          (declare ,@type-decs
                   (ignorable ,@var-names))
          (block @@local
            ,(codegen-expression (node-abstraction-subexpr expr) '@@local env)))
       arity)))

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
    (let* ((inner `(values ,(car (last (node-lisp-form expr)))))

           (inner
             (if (node-lisp-vars expr)
                 `(let ,(mapcar
                         (lambda (var)
                           (list (car var) (cdr var)))
                         (node-lisp-vars expr))
                    ,@(butlast (node-lisp-form expr))
                    (values ,(car (last (node-lisp-form expr)))))
                 inner)))

      (if settings:*emit-type-annotations*
          `(the (values ,(tc:lisp-type (node-type expr) env) &optional)
                ,inner)
          inner)))

  (:method ((expr node-while) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))

    (let ((pred-expr (codegen-expression (node-while-expr expr) current-function env))
          (body-expr (codegen-expression (node-while-body expr) current-function env))
          (label (node-while-label expr)))
      `(loop
         :named ,(break-label label)
         :while ,pred-expr
         :do
            (block ,(continue-label label) ,body-expr))))

  (:method ((expr node-while-let) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))

    (let ((match-expr (codegen-expression (node-while-let-expr expr) current-function env))
          (body-expr (codegen-expression (node-while-let-body expr) current-function env))
          (label (node-while-let-label expr))
          (match-var (gensym "MATCH")))

      (multiple-value-bind (pred bindings)
          (codegen-pattern (node-while-let-pattern expr) match-var env)
        `(loop
           :named ,(break-label label)
           :for ,match-var
             := ,(if settings:*emit-type-annotations*
                     `(the ,(tc:lisp-type (node-type (node-while-let-expr expr)) env) ,match-expr)
                     match-expr)
           :while ,pred
           :do (block ,(continue-label label)
                 ,(cond ((null bindings) body-expr)
                        (t `(let ,bindings
                              (declare (ignorable ,@(mapcar #'car bindings)))
                              ,body-expr))))))))

  (:method ((expr node-loop) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    (let ((body-expr (codegen-expression (node-loop-body expr) current-function env))
          (label (node-loop-label expr)))
      `(loop :named  ,(break-label label)
             :do (block ,(continue-label label)
                   ,body-expr))))

  (:method ((expr node-break) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    `(return-from ,(break-label (node-break-label expr))))

  (:method ((expr node-continue) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    `(return-from ,(continue-label (node-continue-label expr))))

  (:method ((expr node-match) current-function env)
    (declare (type tc:environment env)
             (type (or null symbol) current-function))
    ;; If possible codegen a cl:if instead of a trivia:match
    (when (and (equalp (node-type (node-match-expr expr)) tc:*boolean-type*)
               (= 2 (length (node-match-branches expr)))
               (equalp (match-branch-pattern (first (node-match-branches expr)))
                       (make-pattern-constructor :type tc:*boolean-type* :name 'coalton:True :patterns nil))
               (equalp (match-branch-pattern (second (node-match-branches expr)))
                       (make-pattern-constructor :type tc:*boolean-type* :name 'coalton:False :patterns nil)))
      (return-from codegen-expression
        `(if ,(codegen-expression (node-match-expr expr) current-function env)
             ,(codegen-expression (match-branch-body (first (node-match-branches expr))) current-function env)
             ,(codegen-expression (match-branch-body (second (node-match-branches expr))) current-function env))))

    ;; Otherwise do the thing
    (let ((subexpr (codegen-expression (node-match-expr expr) current-function env))
          (match-var (gensym "MATCH")))
      `(let ((,match-var
               ,(if settings:*emit-type-annotations*
                    `(the ,(tc:lisp-type (node-type (node-match-expr expr)) env) ,subexpr)
                    subexpr)))

         (declare (ignorable ,match-var))
         (locally
             #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
             (cond
               ,@(loop :for branch :in (node-match-branches expr)
                       :for pattern := (match-branch-pattern branch)
                       :for expr := (codegen-expression (match-branch-body branch) current-function env)
                       :collect
                       (multiple-value-bind (pred bindings)
                           (codegen-pattern pattern match-var env)
                         `(,pred
                           ,(cond
                              ((null bindings)
                               expr)
                              (t
                               `(let ,bindings
                                  (declare (ignorable ,@(mapcar #'car bindings)))
                                  ,expr))))))

               ;; Only emit a fallback if there is not a catch-all clause.
               ,@(unless (member-if (lambda (pat)
                                      (or (pattern-wildcard-p pat)
                                          (pattern-var-p pat)))
                                    (node-match-branches expr)
                                    :key #'match-branch-pattern)
                   `((t
                      (error "Pattern match not exhaustive error")))))))))

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
         (let ((arity (length (node-abstraction-vars (node-bind-expr expr)))))
           `(let ((,name))
              (declare (ignorable ,name))
              (flet ((,name
                         ,(node-abstraction-vars (node-bind-expr expr))
                       (declare (ignorable ,@(node-abstraction-vars (node-bind-expr expr)))
                                ,@(argument-types (node-bind-expr expr) env)
                                (values ,(tc:lisp-type (tc:function-return-type (node-type (node-bind-expr expr))) env) &optional))
                       ,(codegen-expression (node-abstraction-subexpr (node-bind-expr expr)) name env)))
                (setf ,name ,(rt:construct-function-entry `#',name arity))
                ,(codegen-expression (node-bind-body expr) current-function env)))))

        ((node-abstraction-p (node-bind-expr expr))
         `(flet ((,name
                     ,(node-abstraction-vars (node-bind-expr expr))
                   (declare (ignorable ,@(node-abstraction-vars (node-bind-expr expr)))
                            ,@(argument-types (node-bind-expr expr) env)
                            (values ,(tc:lisp-type (tc:function-return-type (node-type (node-bind-expr expr))) env) &optional))
                   ,(codegen-expression (node-abstraction-subexpr (node-bind-expr expr)) name env)))
            ,(codegen-expression (node-bind-body expr) current-function env)))

        (t
         `(let ((,name ,(codegen-expression (node-bind-expr expr) current-function env)))
            (declare (ignorable ,name))
            ,(codegen-expression (node-bind-body expr) current-function env)))))))

(defun find-constructor (initform env)
  (if (or (node-application-p initform) (node-direct-application-p initform))
      (and (node-operator-name initform)
           (tc:lookup-constructor env (node-operator-name initform) :no-error t))
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
           (type util:symbol-list local-vars)
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
       (let* (;; functions in this scc referenced in the variable namespace
              (binding-names-vars (intersection (mapcar #'car scc-bindings) local-vars))

              (inner (codegen-let node (cdr sccs) current-function local-vars env))

              (inner
                (if binding-names-vars
                    (append
                     (loop :for (name . node) :in scc-bindings
                           :for arity := (length (node-abstraction-vars node))
                           :if (find name binding-names-vars :test #'equalp)
                             :collect `(setf
                                        ,name
                                        ,(rt:construct-function-entry `#',name arity)))
                     (list inner))
                    (list inner)))

              (inner
                `(labels ,(loop :for (name . node) :in scc-bindings
                                :collect `(,name
                                           ,(node-abstraction-vars node)
                                           (declare (ignorable ,@(node-abstraction-vars node))
                                                    ,@(argument-types node env)
                                                    (values ,(tc:lisp-type
                                                              (tc:function-return-type (node-type node))
                                                              env)
                                                            &optional))
                                           ,(codegen-expression (node-abstraction-subexpr node) name env)))
                   ,@inner))

              (node
                (if binding-names-vars
                    `(let
                         ,(loop :for (name . node) :in scc-bindings
                                :if (find name binding-names-vars :test #'equalp)
                                  :collect name)
                       (declare (ignorable ,@(mapcar #'car scc-bindings)))
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
                                 :appending (loop :for arg :in (node-operands initform)
                                                  :for i :from 0
                                                  :collect `(setf ,(setf-accessor ctor-info i name)
                                                                  ,(codegen-expression arg current-function env)))))
              (allocations (loop :for (name . initform) :in scc-bindings
                                 :for ctor-info := (find-constructor initform env)
                                 :for ctor-classname := (tc:constructor-entry-classname ctor-info)
                                 :collect
                                 (if ctor-classname
                                     `(allocate-instance (find-class ',ctor-classname))
                                     `(,(node-operator-name initform)
                                       ,@(mapcar (constantly nil) (node-direct-application-operands initform)))))))
         `(let ,(mapcar (lambda (scc-binding allocation)
                          (list (car scc-binding) allocation))
                 scc-bindings allocations)
            (declare (ignorable ,@(mapcar #'car scc-bindings)))
            ,@assignments
            ,inner)))

      ;; Single variable binding
      ((= 1 (length scc-bindings))
       (let ((name (car (first scc-bindings)))
             (node_ (cdr (first scc-bindings))))

         `(let ((,name ,(codegen-expression node_ current-function env)))
            (declare (ignorable ,name))
            ,(codegen-let node (cdr sccs) current-function local-vars env))))

      (t (error "Invalid scc binding group. This should have been detected during typechecking.")))))


(defun argument-types (node env)
  (declare (type node-abstraction node)
           (type tc:environment env))
  (loop :for var :in (node-abstraction-vars node)
        :for ty :in (tc:function-type-arguments (node-type node))
        :collect `(type ,(tc:lisp-type ty env) ,var)))
