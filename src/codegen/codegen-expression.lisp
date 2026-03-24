(defpackage #:coalton-impl/codegen/codegen-expression
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast
   #:coalton-impl/codegen/codegen-match)
  (:import-from
   #:coalton-impl/codegen/codegen-pattern
   #:codegen-pattern)
  (:import-from
   #:coalton-impl/codegen/codegen-type-definition
   #:constructor-slot-name)
  (:import-from
   #:coalton-impl/codegen/transformations
   #:node-variables)
  (:import-from
   #:coalton-impl/codegen/typecheck-node
   #:explicit-nullary-callable-p)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:rt #:coalton-impl/runtime)
   (#:tc #:coalton-impl/typechecker)
   (#:const #:coalton-impl/constants))
  (:export
   #:codegen-expression                 ; FUNCTION
   #:abstraction-lambda-list            ; FUNCTION
   #:function-declarations              ; FUNCTION
   #:annotate-function-body             ; FUNCTION
   #:node-output-lisp-types             ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/codegen-expression)

(defvar *emit-lisp-type-checks-p* t
  "When false, embedded `lisp` forms inherit the surrounding type-check policy.")

(defun continue-label (label)
  (declare (type symbol label))
  (alexandria:format-symbol :keyword "~a-CONTINUE" label))

(defun break-label (label)
  (declare (type symbol label))
  (alexandria:format-symbol :keyword "~a-BREAK" label))

(defun block-label (label)
  (declare (type symbol label))
  (alexandria:format-symbol :keyword "~a-BLOCK" label))

(defun values-binding-decls (vars types)
  (declare (type list vars)
           (type list types)
           (values t &optional))
  `(declare
    (ignorable ,@vars)
    ,@(if (not settings:*emit-type-annotations*)
          nil
          (loop :for var :in vars
                :for type :in types
                :collect `(type ,type ,var)))))

(defun loop-binding-declaration-table (bindings env)
  (declare (type node-for-binding-list bindings)
           (type tc:environment env)
           (values hash-table &optional))
  (let ((table (make-hash-table :test #'eq)))
    (when settings:*emit-type-annotations*
      (loop :for binding :in bindings
            :for name := (node-for-binding-name binding)
            :do (setf (gethash name table)
                      (list `(type ,(tc:lisp-type (node-for-binding-type binding) env)
                                   ,name)))))
    table))

(defun loop-step-form (bindings env &key sequential-p)
  (declare (type node-for-binding-list bindings)
           (type tc:environment env)
           (values (or null list) &optional))
  (let ((assignments
          (loop :for binding :in bindings
                :when (node-for-binding-step binding)
                  :collect (list (node-for-binding-name binding)
                                 (codegen-expression (node-for-binding-step binding) env)))))
    (when assignments
      (if sequential-p
          `(setq
            ,@(loop :for (name expr) :in assignments
                    :append (list name expr)))
          `(psetq
            ,@(loop :for (name expr) :in assignments
                    :append (list name expr)))))))

(defun codegen-sequential-loop-bindings (bindings env inner-form &optional binding-decl-table)
  (declare (type node-for-binding-list bindings)
           (type tc:environment env)
           (type (or null hash-table) binding-decl-table)
           (values t &optional))
  (if (endp bindings)
      inner-form
      `(let* ,(loop :for binding :in bindings
                    :collect (list (node-for-binding-name binding)
                                   (codegen-expression (node-for-binding-init binding) env)))
         ,@(let ((decls
                   (when binding-decl-table
                     (loop :for binding :in bindings
                           :append (copy-list (gethash (node-for-binding-name binding)
                                                       binding-decl-table))))))
             (when decls
               `((declare ,@decls))))
         ,inner-form)))

(defun abstraction-lambda-list (node)
  (declare (type node-abstraction node)
           (values list &optional))
  (append
   (copy-list (node-abstraction-vars node))
   (when (node-abstraction-keyword-params node)
     (list* '&key
            (loop :for param :in (node-abstraction-keyword-params node)
                  :collect `((,(keyword-param-keyword param)
                               ,(keyword-param-var param))
                              nil
                              ,(keyword-param-supplied-p-var param)))))))

(defun abstraction-bound-vars (node)
  (declare (type node-abstraction node)
           (values list &optional))
  (append
   (copy-list (node-abstraction-vars node))
   (loop :for param :in (node-abstraction-keyword-params node)
         :append (list (keyword-param-var param)
                       (keyword-param-supplied-p-var param)))))

(defun keyword-tail-forms (keyword-rands env)
  (declare (type keyword-arg-list keyword-rands)
           (type tc:environment env)
           (values list boolean &optional))
  (let ((dynamic-p nil))
    (values
     (loop :for arg :in keyword-rands
           :collect
             (let ((keyword (node-application-keyword-arg-keyword arg))
                   (value (codegen-expression (node-application-keyword-arg-value arg) env))
                   (supplied-p (node-application-keyword-arg-supplied-p arg)))
               (if supplied-p
                   (progn
                     (setf dynamic-p t)
                     `(if ,(codegen-expression supplied-p env)
                          (list ,keyword ,value)
                          '()))
                   `(list ,keyword ,value))))
     dynamic-p)))

(defun keyword-call-args-list-form (positional-rands keyword-rands env)
  (declare (type node-list positional-rands)
           (type keyword-arg-list keyword-rands)
           (type tc:environment env)
           (values t &optional))
  `(append
    (list ,@(mapcar (lambda (subnode)
                      (codegen-expression subnode env))
                    positional-rands))
    ,@(nth-value 0 (keyword-tail-forms keyword-rands env))))

(defgeneric node-output-lisp-types (node env)
  (:documentation "Return the physical Common Lisp output types for NODE.")
  (:method ((node node-abstraction) env)
    (declare (type tc:environment env)
             (values list &optional))
    (mapcar (lambda (ty) (tc:lisp-type ty env))
            (tc:function-output-types (node-type node))))
  (:method ((node node-lisp) env)
    (declare (type tc:environment env)
             (values list &optional))
    (mapcar (lambda (ty) (tc:lisp-type ty env))
            (tc:multiple-value-output-types (node-type node))))
  (:method ((node node) env)
    (declare (type tc:environment env)
             (values list &optional))
    (list (tc:lisp-type (node-type node) env))))

(defgeneric codegen-expression (node env)
  (:method ((node node-literal) env)
    (declare (type tc:environment env)
             (ignore env))
    (node-literal-value node))

  (:method ((node node-variable) env)
    (declare (type tc:environment env))
    (let ((value (node-variable-value node)))
      (case value
        ;; HACK: generate efficient code for known constants.
        ((coalton:True coalton:False coalton:Nil coalton:Unit)
         `(quote ,(eval value)))
        ;; General case: Emit the symbol itself.
        (otherwise
         (alexandria:if-let ((entry (and (not (util:dynamic-variable-name-p value))
                                         (tc:lookup-function env value :no-error t))))
           (if (and (not (tc:function-type-p (node-type node)))
                    (zerop (tc:function-env-entry-arity entry)))
               `(rt:exact-call ,value)
               value)
           value)))))

  ;; Keyword dispatch for indirect calls (through function-entry).
  ;; The parallel logic for direct calls is in the node-direct-application
  ;; method below; both share keyword-tail-forms/keyword-call-args-list-form
  ;; for the dynamic-keyword path.
  (:method ((node node-application) env)
    (declare (type tc:environment env))
    (let ((rator (codegen-expression (node-application-rator node) env))
          (rands (node-application-rands node))
          (keyword-rands (node-application-keyword-rands node)))
      (cond
        ((and (null rands) (null keyword-rands))
         `(rt:exact-call ,rator))
        ((null keyword-rands)
         `(rt:exact-call
           ,rator
           ,@(mapcar (lambda (subnode)
                       (codegen-expression subnode env))
                     rands)))
        ((every (lambda (arg)
                  (null (node-application-keyword-arg-supplied-p arg)))
                keyword-rands)
         `(rt:call-coalton-function
           ,rator
           ,@(mapcar (lambda (subnode)
                       (codegen-expression subnode env))
                     rands)
           ,@(loop :for arg :in keyword-rands
                   :append (list (node-application-keyword-arg-keyword arg)
                                 (codegen-expression (node-application-keyword-arg-value arg) env)))))
        (t
         `(apply #'rt:call-coalton-function
                 ,rator
                 ,(keyword-call-args-list-form rands keyword-rands env))))))

  ;; Keyword dispatch for direct calls (known callee).
  ;; Parallel to node-application above but uses the function name directly.
  (:method ((node node-direct-application) env)
    (declare (type tc:environment env))
    (let* ((positional-rands (node-direct-application-rands node))
           (keyword-rands (node-direct-application-keyword-rands node))
           (dynamic-keywords-p
             (some #'node-application-keyword-arg-supplied-p keyword-rands))
           (call (if dynamic-keywords-p
                     `(apply #',(node-direct-application-rator node)
                             ,(keyword-call-args-list-form positional-rands keyword-rands env))
                     `(,(node-direct-application-rator node)
                       ,@(mapcar (lambda (subnode)
                                   (codegen-expression subnode env))
                                 positional-rands)
                       ,@(loop :for arg :in keyword-rands
                               :append (list (node-application-keyword-arg-keyword arg)
                                             (codegen-expression (node-application-keyword-arg-value arg) env))))))
           (residual-type
             (tc:function-remove-arguments
              (node-direct-application-rator-type node)
              (length positional-rands))))
      (if (and (tc:function-type-p residual-type)
               (explicit-nullary-callable-p residual-type)
               (not (tc:function-type-p (node-type node))))
          `(rt:exact-call ,call)
          call)))

  (:method ((node node-values) env)
    (declare (type tc:environment env))
    `(values
      ,@(mapcar
         (lambda (sub)
           (codegen-expression sub env))
         (node-values-nodes node))))

  (:method ((node node-values-bind) env)
    (declare (type tc:environment env))
    (let* ((vars (node-values-bind-vars node))
           (expr (codegen-expression (node-values-bind-expr node) env))
           (body (codegen-expression (node-values-bind-body node) env))
           (expr-type (node-type (node-values-bind-expr node)))
           (types (tc:multiple-value-output-types expr-type)))
      `(multiple-value-bind ,vars ,expr
         ,(values-binding-decls vars
                                (mapcar (lambda (ty) (tc:lisp-type ty env)) types))
         ,body)))

  (:method ((expr node-abstraction) env)
    (declare (type tc:environment env))
    (let* ((var-names (node-abstraction-vars expr))

           (arity (length var-names)))

      (rt:construct-function-entry
       `(lambda ,(abstraction-lambda-list expr)
          ,(function-declarations expr env)
          ,(annotate-function-body
            expr
            (codegen-expression (node-abstraction-subexpr expr) env)
            env))
       arity)))

  (:method ((expr node-let) env)
    (declare (type tc:environment env))
    (let ((sccs (node-binding-sccs (node-let-bindings expr))))
      (codegen-let
       expr
       sccs
       (node-variables expr :variable-namespace-only t)
       env)))

  (:method ((expr node-dynamic-let) env)
    (declare (type tc:environment env))
    `(let ,(loop :for binding :in (node-dynamic-let-bindings expr)
                 :collect (list (node-dynamic-binding-name binding)
                                (codegen-expression (node-dynamic-binding-value binding) env)))
       ,(codegen-expression (node-dynamic-let-subexpr expr) env)))

  (:method ((expr node-locally) env)
    (declare (type tc:environment env))
    (let* ((type-check (node-locally-type-check expr))
           (subexpr
             (let ((*emit-lisp-type-checks-p*
                     (if (eql type-check 0)
                         nil
                         *emit-lisp-type-checks-p*)))
               (codegen-expression (node-locally-subexpr expr) env)))
           (declarations nil))
      (when (node-locally-noinline-functions expr)
        (push `(notinline ,@(node-locally-noinline-functions expr)) declarations))
      (when type-check
        (if (eql 0 type-check)
            (push '(optimize (safety 0) #+sbcl (sb-c::type-check 0))
                  declarations)
            (push `(optimize #+sbcl (sb-c::type-check ,type-check))
                  declarations)))
      `(locally
         ,@(when declarations
             `((declare ,@(nreverse declarations))))
         ,subexpr)))

  (:method ((expr node-lisp) env)
    (declare (type tc:environment env))
    (let* ((forms (node-lisp-form expr))
           (prefix-forms (butlast forms))
           (last-form (car (last forms)))
           (output-arity (tc:multiple-value-output-arity (node-type expr)))
           (tail-forms
             (case output-arity
               (0
                (list last-form '(values)))
               (1
                (list `(values ,last-form)))
               (otherwise
                (list last-form))))
           (inner
             (if (node-lisp-vars expr)
                 `(let ,(mapcar
                         (lambda (var)
                           (list (car var) (cdr var)))
                         (node-lisp-vars expr))
                    ,@prefix-forms
                    ,@tail-forms)
                 `(progn ,@prefix-forms
                         ,@tail-forms)))
           (return-types (node-output-lisp-types expr env)))
      (let ((body
              (if settings:*emit-type-annotations*
                  `(the (values ,@return-types &optional)
                        ,inner)
                  inner)))
        (if *emit-lisp-type-checks-p*
            `(locally (declare #+sbcl (optimize (sb-c::type-check 1)))
               ,body)
            body))))

  (:method ((expr node-for) env)
    (declare (type tc:environment env))
    (let* ((bindings (node-for-bindings expr))
           (sequential-p (node-for-sequential-p expr))
           (label (node-for-label expr))
           (start-tag (gensym "LOOP-START-"))
           (done-tag (gensym "LOOP-DONE-"))
           (break-target (break-label label))
           (continue-target (continue-label label))
           (binding-decl-table (loop-binding-declaration-table bindings env))
           (init-bindings
             (loop :for binding :in bindings
                   :collect (cons (node-for-binding-name binding)
                                  (node-for-binding-init binding))))
           (termination-kind (node-for-termination-kind expr))
           (termination-expr (and (node-for-termination-expr expr)
                                  (codegen-expression (node-for-termination-expr expr) env)))
           (body-expr (codegen-expression (node-for-body expr) env))
           (step-form (loop-step-form bindings env :sequential-p sequential-p))
           (returns-expr (and (node-for-returns expr)
                              (codegen-expression (node-for-returns expr) env))))
      (let* ((loop-expr
               (case termination-kind
                 (:repeat
                  (let ((repeat-var (gensym "LOOP-REPEAT-")))
                    `(let ((,repeat-var ,termination-expr))
                       ,@(when settings:*emit-type-annotations*
                           `((declare (type ,(tc:lisp-type tc:*ufix-type* env) ,repeat-var))))
                       (block ,break-target
                         (tagbody
                          ,start-tag
                          (when (zerop ,repeat-var)
                            (go ,done-tag))
                          (block ,continue-target
                            ,body-expr)
                          ,@(when step-form
                              (list step-form))
                          (setq ,repeat-var (1- ,repeat-var))
                          (go ,start-tag)
                          ,done-tag)))))
                 (t
                  `(block ,break-target
                     (tagbody
                      ,start-tag
                      ,@(case termination-kind
                          (:while `((unless ,termination-expr
                                      (go ,done-tag))))
                          (:until `((when ,termination-expr
                                      (go ,done-tag))))
                          (otherwise nil))
                      (block ,continue-target
                        ,body-expr)
                      ,@(when step-form
                          (list step-form))
                      (go ,start-tag)
                      ,done-tag)))))
             (loop-form
               `(progn
                  ,loop-expr
                  ,(or returns-expr '(values))))
             (loop-body loop-form))
        (if sequential-p
            (codegen-sequential-loop-bindings bindings env loop-body binding-decl-table)
            (codegen-recursive-bindings
             init-bindings
             (node-binding-sccs init-bindings)
             (node-variables expr :variable-namespace-only t)
             env
             loop-body
             binding-decl-table)))))

  (:method ((expr node-break) env)
    (declare (type tc:environment env))
    `(return-from ,(break-label (node-break-label expr)) (values)))

  (:method ((expr node-continue) env)
    (declare (type tc:environment env))
    `(return-from ,(continue-label (node-continue-label expr)) (values)))

  (:method ((node node-catch) env)
    (declare (type tc:environment env))
    (let* ((block-label   (gensym "CATCH-BLOCK"))
           (handler-cases
             (loop
               :for branch :in (node-catch-branches node)
               :for pattern
                 := (catch-branch-pattern branch)
               ;; wildcard and variable patterns catch all 'error cases
               :for exception-name
                 := (etypecase pattern
                      (pattern-constructor
                       (let* ((ctor-name              (pattern-constructor-name pattern))
                              (ctor                   (tc::lookup-constructor env ctor-name)))
                         (tc:constructor-entry-classname ctor)))
                      ((or pattern-wildcard pattern-var)
                       'error))
               :for case-body
                 := (codegen-expression (catch-branch-body branch) env)
               :for lambda-var
                 := (gensym (symbol-name exception-name))
               :for bindings
                 := (nth-value 1 (codegen-pattern pattern lambda-var (pattern-type pattern) env))
               ;; NB: if CASE-BODY invokes a restart then control will
               ;; be transferred before the transfer due to
               ;; return-from.
               :for inner-body
                 := `(return-from ,block-label ,case-body)
               :collect `(,exception-name
                          (lambda (,lambda-var)
                            (declare (ignorable ,lambda-var))
                            (let ,bindings
                              (declare (ignorable ,@(mapcar #'car bindings)))
                              ,inner-body))))))
      `(block ,block-label
         (handler-bind ,handler-cases
           ,(codegen-expression (node-catch-expr node) env)))))

  (:method ((node node-resumable) env)
    (declare (type tc:environment env))
    (let* ((clauses
             (loop
               :for branch :in (node-resumable-branches node)
               :for pattern
                 := (resumable-branch-pattern branch)
               :for restart-name
                 := (tc:lisp-type (pattern-type pattern) env)
               :for resumption-constructor-arity
                 := (tc:constructor-entry-arity
                     (tc:lookup-constructor env restart-name))
               :for restart-var
                 := (gensym (symbol-name restart-name))
               :for bindings
                 := (nth-value 1 (codegen-pattern pattern restart-var (pattern-type pattern) env))
               :for inner-body := (codegen-expression (resumable-branch-body branch) env)

               :when (plusp resumption-constructor-arity)
                 :collect `(,restart-name (,restart-var)
                                          (declare (ignorable ,restart-var))
                                          (let ,bindings ,inner-body))
               :else
                 :collect `(,restart-name () ,inner-body))))
      `(restart-case ,(codegen-expression (node-resumable-expr node) env)
         ,@clauses)))

  (:method ((expr node-match) env)
    (declare (type tc:environment env))

    (flet ((codegen-branches (subexpr-var subexpr-type jumptablep)
             (loop :for branch :in (node-match-branches expr)
                   :for pattern := (match-branch-pattern branch)
                   :for code := (codegen-expression (match-branch-body branch) env)
                   :collect (codegen-match-branch code
                                                  pattern
                                                  subexpr-var
                                                  subexpr-type
                                                  env
                                                  jumptablep))))

      (let ((subexpr (codegen-expression (node-match-expr expr) env))
            (subexpr-type (node-type (node-match-expr expr)))
            (match-var (gensym "MATCH"))
            (jumptablep (match-emit-jumptable-p expr env))
            (fallbackp (match-emit-fallback-p expr env))
            (branchlessp (match-emit-branchless-p expr env)))

        (multiple-value-bind (emit-if-p then else)
            (match-emit-if-p expr)

          (cond
            ;; Case #1:
            ;;
            ;; Emit a `cl:if' expression on boolean matches
            ;; with exhaustive constructor pattern branches.
            (emit-if-p
             `(if ,(codegen-expression (node-match-expr expr) env)
                  ,(codegen-expression then env)
                  ,(codegen-expression else env)))

            ;; Case #2:
            ;;
            ;; All other situations are handled by `codegen-match',
            ;; including jumptables and branchless matches.
            (t
             (codegen-match
              match-var
              subexpr
              subexpr-type
              (codegen-branches match-var subexpr-type jumptablep)
              env
              jumptablep
              fallbackp
              branchlessp)))))))

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

  (:method ((node node-throw) env)
    `(error ,(codegen-expression (node-throw-expr node) env)))

  (:method ((node node-resume-to) env)
    (let* ((restart-name
             (tc:lisp-type (node-type (node-resume-to-expr node)) env))
           (resumption-constructor-arity
             (tc:constructor-entry-arity
              (tc:lookup-constructor env restart-name))))

      `(invoke-restart ',restart-name
                       ,@(if (zerop resumption-constructor-arity)
                             nil
                             (list (codegen-expression (node-resume-to-expr node) env))))))

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
    (declare (type tc:environment env))
    (let ((name (node-bind-name expr))
          (bound-expr (node-bind-expr expr))
          (body (node-bind-body expr)))
      (cond
        ((and (node-abstraction-p bound-expr)
              (find name (node-variables body :variable-namespace-only t)))
         (let ((arity (length (node-abstraction-vars bound-expr))))
           `(let ((,name))
              (declare (ignorable ,name))
              (flet ((,name
                         ,(abstraction-lambda-list bound-expr)
                       ,(function-declarations bound-expr env)
                       ,(annotate-function-body
                         bound-expr
                         (codegen-expression (node-abstraction-subexpr bound-expr) env)
                         env)))
                (setf ,name ,(rt:construct-function-entry `#',name arity))
                ,(codegen-expression body env)))))
        ((node-abstraction-p bound-expr)
         `(flet ((,name
                     ,(abstraction-lambda-list bound-expr)
                   ,(function-declarations bound-expr env)
                   ,(annotate-function-body
                     bound-expr
                     (codegen-expression (node-abstraction-subexpr bound-expr) env)
                     env)))
            ,(codegen-expression body env)))
        (t
         `(let ((,name ,(codegen-expression bound-expr env)))
            (declare (ignorable ,name))
            ,(codegen-expression body env))))))

  )

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

(defun recursive-binding-declarations (names binding-decl-table)
  (declare (type util:symbol-list names)
           (type (or null hash-table) binding-decl-table)
           (values list &optional))
  (append
   (if names
       (list `(ignorable ,@names))
       nil)
   (if binding-decl-table
       (loop :for name :in names
             :appending (copy-list (gethash name binding-decl-table)))
       nil)))

(defun codegen-recursive-bindings (bindings sccs local-vars env inner-form
                                    &optional binding-decl-table)
  "Emit recursive binding code for BINDINGS around INNER-FORM.

This is shared by `let` and the one-time initializer phase of `for` so both
forms follow the same binding semantics."
  (declare (type binding-list bindings)
           (type list sccs)
           (type util:symbol-list local-vars)
           (type tc:environment env)
           (type (or null hash-table) binding-decl-table))

  (when (null sccs)
    (return-from codegen-recursive-bindings inner-form))

  (let* ((scc (car sccs))
         (scc-bindings
           (remove-if-not
            (lambda (binding)
              (find (car binding) scc))
            bindings)))

    (cond
      ;; Function binding group
      ((every #'node-abstraction-p (mapcar #'cdr scc-bindings))
       (let* (;; functions in this scc referenced in the variable namespace
              (binding-names-vars (intersection (mapcar #'car scc-bindings) local-vars))

              (inner (codegen-recursive-bindings bindings (cdr sccs) local-vars env inner-form
                                                 binding-decl-table))

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
                                           ,(abstraction-lambda-list node)
                                           ,(function-declarations node env)
                                           ,(annotate-function-body
                                             node
                                             (codegen-expression (node-abstraction-subexpr node) env)
                                             env)))
                   ,@inner))

              (node
                (if binding-names-vars
                    `(let
                         ,(loop :for (name . node) :in scc-bindings
                                :if (find name binding-names-vars :test #'equalp)
                                  :collect name)
                       ,@(let ((decls (recursive-binding-declarations
                                       (mapcar #'car scc-bindings)
                                       binding-decl-table)))
                           (when decls
                             `((declare ,@decls))))
                       ,inner)
                    inner)))
         node))

      ((every (lambda (pair)
                (data-letrec-able-p (cdr pair)
                                    env))
              scc-bindings)
       (let* ((inner (codegen-recursive-bindings bindings (cdr sccs) local-vars env inner-form
                                                 binding-decl-table))
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
            ,@(let ((decls (recursive-binding-declarations
                            (mapcar #'car scc-bindings)
                            binding-decl-table)))
                (when decls
                  `((declare ,@decls))))
            ,@assignments
            ,inner)))

      ;; Single variable binding
      ((= 1 (length scc-bindings))
       (let ((name (car (first scc-bindings)))
             (node_ (cdr (first scc-bindings))))

         `(let ((,name ,(codegen-expression node_ env)))
            ,@(let ((decls (recursive-binding-declarations (list name) binding-decl-table)))
                (when decls
                  `((declare ,@decls))))
            ,(codegen-recursive-bindings bindings (cdr sccs) local-vars env inner-form
                                         binding-decl-table))))

      (t (error "Invalid scc binding group. This should have been detected during typechecking.")))))

(defun codegen-let (node sccs local-vars env)
  (declare (type node-let node)
           (type list sccs)
           (type util:symbol-list local-vars)
           (type tc:environment env))
  (codegen-recursive-bindings
   (node-let-bindings node)
   sccs
   local-vars
   env
   (codegen-expression (node-let-subexpr node) env)))


(defun function-declarations (node env)
  (declare (type node-abstraction node)
           (type tc:environment env))
  (let ((return-types (node-output-lisp-types node env)))
    `(declare (ignorable ,@(abstraction-bound-vars node))
              ,@(when settings:*emit-type-annotations*
                  `(,@(loop :for var :in (node-abstraction-vars node)
                            :for ty :in (tc:function-type-arguments (node-type node))
                            :collect `(type ,(tc:lisp-type ty env) ,var))
                    ,@(loop :for param :in (node-abstraction-keyword-params node)
                            :collect `(type boolean ,(keyword-param-supplied-p-var param)))
                    ,(if return-types
                         `(values ,@return-types &optional)
                         '(values)))))))

(defun annotate-function-body (node body env)
  (declare (type node-abstraction node)
           (type tc:environment env)
           (values t &optional))
  (let ((return-types (node-output-lisp-types node env)))
    (if settings:*emit-type-annotations*
        `(the ,(if return-types
                   `(values ,@return-types &optional)
                   '(values))
              ,body)
        body)))
