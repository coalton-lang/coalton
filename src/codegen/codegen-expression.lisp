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
  (:import-from
   #:coalton-impl/codegen/transformations
   #:node-variables)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:rt #:coalton-impl/runtime)
   (#:tc #:coalton-impl/typechecker)
   (#:const #:coalton-impl/constants))
  (:export
   #:codegen-expression                 ; FUNCTION
   #:function-declarations              ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/codegen-expression)

(defun continue-label (label)
  (declare (type symbol label))
  (alexandria:format-symbol :keyword "~a-CONTINUE" label))

(defun break-label (label)
  (declare (type symbol label))
  (alexandria:format-symbol :keyword "~a-BREAK" label))

(defun block-label (label)
  (declare (type symbol label))
  (alexandria:format-symbol :keyword "~a-BLOCK" label))

(defgeneric codegen-expression (node env)
  (:method ((node node-literal) env)
    (declare (type tc:environment env)
             (ignore env))
    (node-literal-value node))

  (:method ((node node-variable) env)
    (declare (type tc:environment env)
             (ignore env))
    (let ((value (node-variable-value node)))
      (case value
        ;; HACK: generate efficient code for known constants.
        ((coalton:True coalton:False coalton:Nil coalton:Unit)
         `(quote ,(eval value)))
        ;; General case: Emit the symbol itself.
        (otherwise
         value))))

  (:method ((node node-application) env)
    (declare (type tc:environment env))
    `(rt:call-coalton-function
      ,(codegen-expression (node-application-rator node) env)
      ,@(mapcar
         (lambda (node)
           (codegen-expression node env))
         (node-application-rands node))))

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

           (arity (length var-names)))

      (rt:construct-function-entry
       `(lambda ,var-names
          ,(function-declarations expr env)
          ,(codegen-expression (node-abstraction-subexpr expr) env))
       arity)))

  (:method ((expr node-let) env)
    (declare (type tc:environment env))
    (let ((sccs (node-binding-sccs (node-let-bindings expr))))
      (codegen-let
       expr
       sccs
       (node-variables expr :variable-namespace-only t)
       env)))

  (:method ((expr node-lisp) env)
    (declare (type tc:environment env))
    (let* ((inner `(progn ,@(butlast (node-lisp-form expr))
                          (values ,(car (last (node-lisp-form expr))))))
           (inner
             (if (node-lisp-vars expr)
                 `(let ,(mapcar
                         (lambda (var)
                           (list (car var) (cdr var)))
                         (node-lisp-vars expr))
                    ,@(butlast (node-lisp-form expr))
                    (values ,(car (last (node-lisp-form expr)))))
                 inner)))

      `(locally (declare #+sbcl (optimize (sb-c::type-check 1)))
                ,(if settings:*emit-type-annotations*
                     `(the (values ,(tc:lisp-type (node-type expr) env) &optional)
                           ,inner)
                     inner))))

  (:method ((expr node-while) env)
    (declare (type tc:environment env))

    (let ((pred-expr (codegen-expression (node-while-expr expr) env))
          (body-expr (codegen-expression (node-while-body expr) env))
          (label (node-while-label expr)))
      `(loop
         :named ,(break-label label)
         :while ,pred-expr
         :do (block ,(continue-label label) ,body-expr)
         :finally (return-from ,(break-label label) coalton:Unit))))

  (:method ((expr node-while-let) env)
    (declare (type tc:environment env))

    (let ((match-expr-type (node-type (node-while-let-expr expr)))
          (match-expr (codegen-expression (node-while-let-expr expr) env))
          (body-expr (codegen-expression (node-while-let-body expr) env))
          (label (node-while-let-label expr))
          (match-var (gensym "MATCH")))

      (multiple-value-bind (pred bindings types)
          (codegen-pattern (node-while-let-pattern expr) match-var match-expr-type env)
        `(loop
           :named ,(break-label label)
           :for ,match-var ,@(if settings:*emit-type-annotations*
                                 `(:of-type ,(tc:lisp-type match-expr-type env))
                                 nil)
             := ,match-expr
           :while ,pred
           :do (block ,(continue-label label)
                 ,(cond ((null bindings) body-expr)
                        (t `(let ,bindings
                              (declare (ignorable ,@(mapcar #'car bindings))
                                       ,@(cond
                                           (settings:*emit-type-annotations*
                                            (loop :for binding :in bindings
                                                  :for var := (car binding)
                                                  :for type :in types
                                                  :collect `(type ,type ,var)))
                                           (t
                                            nil)))
                              ,body-expr))))
           :finally (return-from ,(break-label label) coalton:Unit)))))

  (:method ((expr node-loop) env)
    (declare (type tc:environment env))
    (let ((body-expr (codegen-expression (node-loop-body expr) env))
          (label (node-loop-label expr)))
      `(loop :named ,(break-label label)
             :do (block ,(continue-label label)
                   ,body-expr)
             :finally (return-from ,(break-label label) coalton:Unit))))

  (:method ((expr node-break) env)
    (declare (type tc:environment env))
    `(return-from ,(break-label (node-break-label expr)) coalton:Unit))

  (:method ((expr node-continue) env)
    (declare (type tc:environment env))
    `(return-from ,(continue-label (node-continue-label expr))))

  (:method ((expr node-match) env)
    (declare (type tc:environment env))
    ;; If possible codegen a cl:if instead of a trivia:match
    (when (and (equalp (node-type (node-match-expr expr)) tc:*boolean-type*)
               (= 2 (length (node-match-branches expr)))
               (equalp (match-branch-pattern (first (node-match-branches expr)))
                       (make-pattern-constructor :type tc:*boolean-type* :name 'coalton:True :patterns nil))
               (equalp (match-branch-pattern (second (node-match-branches expr)))
                       (make-pattern-constructor :type tc:*boolean-type* :name 'coalton:False :patterns nil)))
      (return-from codegen-expression
        `(if ,(codegen-expression (node-match-expr expr) env)
             ,(codegen-expression (match-branch-body (first (node-match-branches expr))) env)
             ,(codegen-expression (match-branch-body (second (node-match-branches expr))) env))))

    ;; Otherwise do the thing
    (let ((subexpr (codegen-expression (node-match-expr expr) env))
          (match-expr-type (node-type (node-match-expr expr)))
          (match-var (gensym "MATCH")))
      `(let ((,match-var ,subexpr))
         (declare ,@(list*
                     `(ignorable ,match-var)
                     (if settings:*emit-type-annotations*
                         (list `(type ,(tc:lisp-type match-expr-type env) ,match-var))
                         nil)))
         (locally
             #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
             (cond
               ,@(loop :for branch :in (node-match-branches expr)
                       :for pattern := (match-branch-pattern branch)
                       :for expr := (codegen-expression (match-branch-body branch) env)
                       :collect
                       (multiple-value-bind (pred bindings types)
                           (codegen-pattern pattern match-var match-expr-type env)
                         `(,pred
                           ,(cond
                              ((null bindings)
                               expr)
                              (t
                               `(let ,bindings
                                  (declare (ignorable ,@(mapcar #'car bindings))
                                           ,@(cond
                                               (settings:*emit-type-annotations*
                                                (loop :for binding :in bindings
                                                      :for var := (car binding)
                                                      :for type :in types
                                                      :collect `(type ,type ,var)))
                                               (t
                                                nil)))
                                  ,expr))))))

               ;; Only emit a fallback if there is not a catch-all clause.
               ,@(unless (or (member-if (lambda (pat)
                                          (or (pattern-wildcard-p pat)
                                              (pattern-var-p pat)))
                                        (node-match-branches expr)
                                        :key #'match-branch-pattern)
                             (and (settings:coalton-release-p)
                                  (patterns-exhaustive-p
                                   (mapcar #'match-branch-pattern (node-match-branches expr))
                                   (node-type (node-match-expr expr))
                                   env)))
                   `((t
                      (error "Pattern match not exhaustive error")))))))))

  (:method ((expr node-seq) env)
    (declare (type tc:environment env))
    `(progn
       ,@(mapcar
          (lambda (node)
            (codegen-expression node env))
          (node-seq-nodes expr))))

  (:method ((expr node-return-from) env)
    `(return-from ,(block-label (node-return-from-name expr))
       ,(codegen-expression (node-return-from-expr expr) env)))

  (:method ((expr node-block) env)
    `(block ,(block-label (node-block-name expr))
       ,(codegen-expression (node-block-body expr) env)))

  (:method ((expr node-field) env)
    (declare (type tc:environment env))
    `(,(node-field-name expr)
      ,(codegen-expression (node-field-dict expr) env)))

  (:method ((expr node-dynamic-extent) env)
    (declare (type tc:environment env))
    `(let ((,(node-dynamic-extent-name expr)
             ,(codegen-expression (node-dynamic-extent-node expr) env)))
       (declare (dynamic-extent ,(node-dynamic-extent-name expr)))
       ,(codegen-expression (node-dynamic-extent-body expr) env)))

  (:method ((expr node-bind) env)
    (let ((name (node-bind-name expr)))
      (cond
        ((and (node-abstraction-p (node-bind-expr expr))
              (find name (node-variables (node-bind-body expr) :variable-namespace-only t)))
         (let ((arity (length (node-abstraction-vars (node-bind-expr expr)))))
           `(let ((,name))
              (declare (ignorable ,name))
              (flet ((,name
                         ,(node-abstraction-vars (node-bind-expr expr))
                       ,(function-declarations (node-bind-expr expr) env)
                       ,(codegen-expression (node-abstraction-subexpr (node-bind-expr expr)) env)))
                (setf ,name ,(rt:construct-function-entry `#',name arity))
                ,(codegen-expression (node-bind-body expr) env)))))

        ((node-abstraction-p (node-bind-expr expr))
         `(flet ((,name
                     ,(node-abstraction-vars (node-bind-expr expr))
                   ,(function-declarations (node-bind-expr expr) env)
                   ,(codegen-expression (node-abstraction-subexpr (node-bind-expr expr)) env)))
            ,(codegen-expression (node-bind-body expr) env)))

        (t
         `(let ((,name ,(codegen-expression (node-bind-expr expr) env)))
            (declare (ignorable ,name))
            ,(codegen-expression (node-bind-body expr) env)))))))

(defun find-constructor (initform env)
  (if (or (node-application-p initform) (node-direct-application-p initform))
      (and (node-rator-name initform)
           (tc:lookup-constructor env (node-rator-name initform) :no-error t))
      nil))

(defun data-letrec-able-p (initform env)
  (let ((ctor-ent (find-constructor initform env)))
    (and (node-direct-application-p initform)
         ctor-ent
         (let* ((type-name (tc:constructor-entry-constructs ctor-ent))
                (type-ent (tc:lookup-type env type-name)))
           (not (or (tc:type-entry-enum-repr type-ent)
                    (tc:type-entry-newtype type-ent)))))))

(declaim (ftype (function (tc:constructor-entry unsigned-byte symbol)
                          (values list &optional))
                setf-accessor))
(defun setf-accessor (ctor-ent nth-slot instance)
  (case (tc:constructor-entry-name ctor-ent)
    ((coalton:Cons)
     (ecase nth-slot
       (0 `(car ,instance))
       (1 `(cdr ,instance))))
    ((coalton:Some)
     (ecase nth-slot
       (0 `(rt:unwrap-cl-some ,instance))))
    (otherwise
     `(slot-value ,instance ',(constructor-slot-name ctor-ent nth-slot)))))

(defun codegen-let (node sccs local-vars env)
  (declare (type node-let node)
           (type list sccs)
           (type util:symbol-list local-vars)
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
       (let* (;; functions in this scc referenced in the variable namespace
              (binding-names-vars (intersection (mapcar #'car scc-bindings) local-vars))

              (inner (codegen-let node (cdr sccs) local-vars env))

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
                                           ,(function-declarations node env)
                                           ,(codegen-expression (node-abstraction-subexpr node) env)))
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
       (let* ((inner (codegen-let node (cdr sccs) local-vars env))
              (assignments (loop :for (name . initform) :in scc-bindings
                                 :for ctor-info := (find-constructor initform env)
                                 :appending (loop :for arg :in (node-rands initform)
                                                  :for i :from 0
                                                  :collect `(setf ,(setf-accessor ctor-info i name)
                                                                  ,(codegen-expression arg env)))))
              (allocations (loop :for (name . initform) :in scc-bindings
                                 :for ctor-info := (find-constructor initform env)
                                 :for ctor-classname := (tc:constructor-entry-classname ctor-info)
                                 :collect
                                 (if ctor-classname
                                     `(allocate-instance (find-class ',ctor-classname))
                                     `(,(node-rator-name initform)
                                       ,@(mapcar (constantly nil) (node-direct-application-rands initform)))))))
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

         `(let ((,name ,(codegen-expression node_ env)))
            (declare (ignorable ,name))
            ,(codegen-let node (cdr sccs) local-vars env))))

      (t (error "Invalid scc binding group. This should have been detected during typechecking.")))))


(defun function-declarations (node env)
  (declare (type node-abstraction node)
           (type tc:environment env))
  `(declare (ignorable ,@(node-abstraction-vars node))
            ,@(when settings:*emit-type-annotations*
                `(,@(loop :for var :in (node-abstraction-vars node)
                          :for ty :in (tc:function-type-arguments (node-type node))
                          :collect `(type ,(tc:lisp-type ty env) ,var))
                  (values ,(tc:lisp-type (node-type (node-abstraction-subexpr node)) env) &optional)))))
