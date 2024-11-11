;;;
;;; This file provides functions for translating from the typechecker
;;; AST to the codegen AST.
;;;

(defpackage #:coalton-impl/codegen/translate-expression
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast
   #:coalton-impl/codegen/resolve-instance)
  (:import-from
   #:coalton-impl/codegen/typecheck-node
   #:typecheck-node)
  (:import-from
   #:coalton-impl/codegen/traverse
   #:action
   #:*traverse*
   #:traverse)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:translate-toplevel                   ; FUNCTION
   #:translate-expression                 ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/translate-expression)

(defvar *current-function-name*)
(setf (documentation '*current-function* 'variable)
      "The symbol name of the function currently being translated.
This will be bound for the extent of any TRANSLATE-TOPLEVEL call, or
TRANSLATE-EXPRESSION when an abstraction is being translated.")

(defun translate-toplevel (binding env name &key extra-context)
  (declare (type tc:binding-type binding)
           (type tc:environment env)
           (type symbol name)
           (type pred-context extra-context))

  (let* ((qual-ty (tc:binding-type binding))

         (preds (tc:qualified-ty-predicates qual-ty))

         (ctx
           (loop :for pred :in preds
                 :collect (cons pred (gensym "DICT"))))

         (full-ctx (append ctx extra-context))

         (pattern-params nil))

    (cond
      ;; If the binding does not have parameters, and the
      ;; body is a single lambda then generate a function
      ;; to match the declared type and then translate the
      ;; lambda.
      ((and (null (tc:binding-parameters binding)) (tc:binding-restricted-p binding))
       (make-node-abstraction
        :type (tc:make-function-type*
               (append
                (loop :for pred :in preds
                      :collect (pred-type pred env))
                (loop :for param :in (tc:node-abstraction-params (tc:binding-last-node binding))
                      :collect (tc:qualified-ty-type (tc:pattern-type param))))
               (tc:function-remove-arguments
                (tc:qualified-ty-type
                 (tc:node-type (tc:binding-last-node binding)))
                (length (tc:node-abstraction-params (tc:binding-last-node binding)))))

        :vars (append
               (loop :for (pred . name) :in ctx
                     :collect name)
               (loop :for param :in (tc:node-abstraction-params (tc:binding-last-node binding))
                     :if (tc:pattern-var-p param)
                       :collect (tc:pattern-var-name param)
                     :else :if (tc:pattern-wildcard-p param)
                             :collect (gensym "_")
                     :else
                       :collect (let ((name (gensym)))
                                  (push (cons name param) pattern-params)
                                  name)))

        :subexpr (let ((*current-function-name* name))
                   (wrap-in-block
                    (wrap-with-pattern-params
                     pattern-params
                     (translate-expression (tc:node-abstraction-body (tc:binding-last-node binding)) full-ctx env))))))

      ;; If the binding has parameters and/or predicates then wrap the body in a lambda.
      ((or (tc:binding-parameters binding) preds)
       (make-node-abstraction
        :type (tc:make-function-type*
               (append
                (loop :for pred :in preds
                      :collect (pred-type pred env))
                (loop :for var :in (tc:binding-parameters binding)
                      :for qual-ty := (tc:pattern-type var)

                      :do (assert (null (tc:qualified-ty-predicates qual-ty)))
                      :collect (tc:qualified-ty-type qual-ty)))
               (tc:qualified-ty-type (tc:node-type (tc:binding-last-node binding))))

        :vars (append
               (loop :for (pred . name) :in ctx
                     :collect name)
               (loop :for param :in (tc:binding-parameters binding)
                     :if (tc:pattern-var-p param)
                       :collect (tc:pattern-var-name param)
                     :else :if (tc:pattern-wildcard-p param)
                             :collect (gensym "_")
                     :else
                       :collect (let ((name (gensym)))
                                  (push (cons name param) pattern-params)
                                  name)))

        :subexpr (let ((*current-function-name* name))
                   (wrap-in-block
                    (wrap-with-pattern-params
                     pattern-params
                     (translate-expression (tc:binding-value binding) full-ctx env))))))

      (t
       (translate-expression (tc:binding-value binding) full-ctx env)))))


(defgeneric translate-expression (expr ctx env)
  (:documentation "Translate typechecker AST node EXPR to the codegen AST.

CTX provides the current predicate context.

Returns a `node'.")

  (:method ((expr tc:node-literal) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-literal
       :type (tc:qualified-ty-type qual-ty)
       :value (tc:node-literal-value expr))))

  (:method ((expr tc:node-integer-literal) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (let* ((classes-package (util:find-package "COALTON-LIBRARY/CLASSES"))
             (num-class (util:find-symbol "NUM" classes-package))
             (num-pred (tc:make-ty-predicate :class num-class
                                             :types (list (tc:qualified-ty-type qual-ty))))
             (from-int-method (util:find-symbol "FROMINT" classes-package))
             (val (tc:node-integer-literal-value expr))
             (ty (tc:qualified-ty-type qual-ty)))
        (flet ((make-full-call ()
                 (make-node-application
                  :type (tc:qualified-ty-type qual-ty)
                  :rator (make-node-variable
                          :type (tc:make-function-type*
                                 (list
                                  (pred-type num-pred env)
                                  tc:*integer-type*)
                                 (tc:qualified-ty-type qual-ty))
                          :value from-int-method)
                  :rands (list
                          (resolve-dict num-pred ctx env)
                          (make-node-literal
                           :type tc:*integer-type*
                           :value val)))))

          ;; Avoid casting INTEGER literals with FROMINT at runtime
          ;; when the type is known, We can do that at comptime.  This
          ;; is a temporary hack to get an easy performance boost but
          ;; should be solved later with proper constant folding.
          (if (not (tc:tycon-p ty))
              (make-full-call)
              (case (tc:tycon-name ty)
                (coalton:Integer
                 (make-node-literal :type tc:*integer-type*
                                    :value val))
                (coalton:Ifix
                 (make-node-literal :type tc:*ifix-type*
                                    :value val))
                (coalton:Ufix
                 (make-node-literal :type tc:*ufix-type*
                                    :value val))
                (coalton:Single-Float
                 (make-node-literal :type tc:*single-float-type*
                                    :value (coerce val 'single-float)))
                (coalton:Double-Float
                 (make-node-literal :type tc:*double-float-type*
                                    :value (coerce val 'double-float)))
                (otherwise
                 (make-full-call))))))))

  (:method ((expr tc:node-variable) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node &optional))

    (apply-dicts expr ctx env))

  (:method ((expr tc:node-accessor) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((qual-ty (tc:node-type expr))
           (ty (tc:qualified-ty-type qual-ty)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (unless (tc:function-type-p ty)
        (util:coalton-bug "Invalid accessor type '~S'" ty))

      (let ((from-ty (tc:base-type (tc:function-type-from ty))))

        (unless (tc:tycon-p from-ty)
          (util:coalton-bug "Invalid accessor type '~S'" ty))

        (let* ((type-entry (tc:lookup-type env (tc:tycon-name from-ty)))

               (struct-entry (tc:lookup-struct env (tc:tycon-name from-ty)))

               (idx (tc:struct-field-index (tc:get-field struct-entry
                                                         (tc:node-accessor-name expr)))))

          (assert idx)

          (if (tc:type-entry-newtype type-entry)
              ;; If the struct is a newtype, then return 'id' as the accessor
              (make-node-variable
               :type ty
               :value (util:find-symbol "ID" "COALTON-LIBRARY/FUNCTIONS"))

              (make-node-variable
               :type ty
               :value (alexandria:format-symbol
                       (symbol-package (tc:tycon-name from-ty))
                       "~A/~A-_~D"
                       (tc:tycon-name from-ty)
                       (tc:tycon-name from-ty)
                       idx)))))))

  (:method ((expr tc:node-application) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-application
       :type (tc:qualified-ty-type qual-ty)
       :rator (translate-expression (tc:node-application-rator expr) ctx env)
       :rands (mapcar
               (lambda (expr)
                 (apply-dicts expr ctx env))
               (tc:node-application-rands expr)))))

  (:method ((expr tc:node-body) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (loop :with out-node := (translate-expression (tc:node-body-last-node expr) ctx env)
          :for body-node :in (reverse (tc:node-body-nodes expr)) :do
            (setf out-node
                  (etypecase body-node
                    (tc:node-bind
                     (let ((ty (node-type out-node)))

                       (let ((pattern (tc:node-bind-pattern body-node)))
                         (typecase (tc:node-bind-pattern body-node)
                           (tc:pattern-var
                            (make-node-bind
                             :type ty
                             :name (tc:pattern-var-name pattern)
                             :expr (translate-expression (tc:node-bind-expr body-node) ctx env)
                             :body out-node))
                           (t
                            (make-node-match
                             :type ty
                             :expr (translate-expression (tc:node-bind-expr body-node) ctx env)
                             :branches (list
                                        (make-match-branch
                                         :pattern (translate-pattern pattern)
                                         :body out-node))))))))

                    (tc:node
                     (make-node-seq
                      :type (node-type out-node)
                      :nodes (list (translate-expression body-node ctx env)
                                   out-node)))))
          :finally (return out-node)))

  (:method ((expr tc:node-abstraction) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((*current-function-name* '@@local)

           (qual-ty (tc:node-type expr))

           (preds (tc:qualified-ty-predicates qual-ty))

           (dict-var-names
             (loop :for pred :in preds
                   :collect (gensym "DICT")))

           (dict-types
             (loop :for pred :in preds
                   :collect (pred-type pred env)))

           (ctx (append
                 (loop :for pred :in preds
                       :for name :in dict-var-names
                       :collect (cons pred name))
                 ctx))

           (pattern-params nil)

           (vars (append
                  dict-var-names
                  (loop :for param :in (tc:node-abstraction-params expr)
                        :for qual-ty := (tc:pattern-type param)

                        :do (assert (null (tc:qualified-ty-predicates qual-ty)))

                        :if (tc:pattern-var-p param)
                          :collect (tc:pattern-var-name param)
                        :else
                          :collect (let ((name (gensym)))
                                     (push (cons name param) pattern-params)
                                     name)))))

      (assert (not (some #'tc:static-predicate-p preds)))

      (make-node-abstraction
       :type (tc:make-function-type* dict-types (tc:qualified-ty-type qual-ty))
       :vars vars
       :subexpr (loop :with inner := (translate-expression (tc:node-abstraction-body expr) ctx env)

                      :for (name . pattern) :in (reverse pattern-params)
                      :do (setf inner
                                (make-node-match
                                 :type (node-type inner)
                                 :expr (make-node-variable
                                        :type (tc:qualified-ty-type (tc:pattern-type pattern))
                                        :value name)
                                 :branches
                                 (list
                                  (make-match-branch
                                   :pattern (translate-pattern pattern)
                                   :body inner))))

                      :finally (return (wrap-in-block inner))))))

  (:method ((expr tc:node-let) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-let
       :type (tc:qualified-ty-type qual-ty)
       :bindings (loop :for binding :in (tc:node-let-bindings expr)
                       :for name := (tc:node-variable-name (tc:node-let-binding-name binding))
                       :for var := (tc:node-let-binding-name binding)

                       :collect (cons name (translate-toplevel binding env name :extra-context ctx)))
       :subexpr (translate-expression (tc:node-let-body expr) ctx env))))

  (:method ((expr tc:node-lisp) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-lisp
       :type (tc:qualified-ty-type qual-ty)
       :vars (loop :for var :in (tc:node-lisp-vars expr)
                   :for var-name :in (tc:node-lisp-var-names expr)
                   :collect (cons var-name (tc:node-variable-name var)))
       :form (tc:node-lisp-body expr))))

  (:method ((expr tc:node-match) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-match
       :type (tc:qualified-ty-type qual-ty)
       :expr (translate-expression (tc:node-match-expr expr) ctx env)
       :branches (mapcar
                  (lambda (branch)
                    (make-match-branch
                     :pattern (translate-pattern (tc:node-match-branch-pattern branch))
                     :body (translate-expression (tc:node-match-branch-body branch) ctx env)))
                  (tc:node-match-branches expr)))))

  (:method ((expr tc:node-handle) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-handle
       :type (tc:qualified-ty-type qual-ty)
       :expr (translate-expression (tc:node-handle-expr expr) ctx env)
       :branches (mapcar
                  (lambda (branch)
                    (make-match-branch
                     :pattern (translate-pattern (tc:node-match-branch-pattern branch))
                     :body (translate-expression (tc:node-match-branch-body branch) ctx env)))
                  (tc:node-handle-branches expr)))))

  (:method ((expr tc:node-progn) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-seq
       :type (tc:qualified-ty-type qual-ty)
       :nodes (list (translate-expression (tc:node-progn-body expr) ctx env)))))

  (:method ((expr tc:node-return) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (let ((unit-value (util:find-symbol "UNIT" "COALTON")))

        (make-node-return-from
         :type (tc:qualified-ty-type qual-ty)
         :name *current-function-name*
         :expr (if (tc:node-return-expr expr)
                   (translate-expression (tc:node-return-expr expr) ctx env)
                   (make-node-variable
                    :type tc:*unit-type*
                    :value unit-value))))))

  (:method ((expr tc:node-or) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((coalton-package (util:find-package "COALTON"))
           (true-value (util:find-symbol "TRUE" coalton-package))
           (false-value (util:find-symbol "FALSE" coalton-package)))

      (loop :with out-node := (make-node-variable
                               :type tc:*boolean-type*
                               :value false-value)
            :for body-node :in (reverse (tc:node-or-nodes expr)) :do
              (setf out-node
                    (make-node-match
                     :type tc:*boolean-type*
                     :expr (translate-expression body-node ctx env)
                     :branches (list
                                (make-match-branch
                                 :pattern (make-pattern-constructor
                                           :type tc:*boolean-type*
                                           :name true-value
                                           :patterns nil)
                                 :body (make-node-variable
                                        :type tc:*boolean-type*
                                        :value true-value))
                                (make-match-branch
                                 :pattern (make-pattern-constructor
                                           :type tc:*boolean-type*
                                           :name false-value
                                           :patterns nil)
                                 :body out-node))))
            :finally (return out-node))))

  (:method ((expr tc:node-and) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((coalton-package (util:find-package "COALTON"))
           (true-value (util:find-symbol "TRUE" coalton-package))
           (false-value (util:find-symbol "FALSE" coalton-package)))

      (loop :with out-node := (make-node-variable
                               :type tc:*boolean-type*
                               :value true-value)
            :for body-node :in (reverse (tc:node-and-nodes expr)) :do
              (setf out-node
                    (make-node-match
                     :type tc:*boolean-type*
                     :expr (translate-expression body-node ctx env)
                     :branches (list
                                (make-match-branch
                                 :pattern (make-pattern-constructor
                                           :type tc:*boolean-type*
                                           :name false-value
                                           :patterns nil)
                                 :body (make-node-variable
                                        :type tc:*boolean-type*
                                        :value false-value))
                                (make-match-branch
                                 :pattern (make-pattern-constructor
                                           :type tc:*boolean-type*
                                           :name true-value
                                           :patterns nil)
                                 :body out-node))))
            :finally (return out-node))))

  (:method ((expr tc:node-if) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((coalton-package (util:find-package "COALTON"))
           (true-value (util:find-symbol "TRUE" coalton-package))
           (false-value (util:find-symbol "FALSE" coalton-package))

           (qual-ty (tc:node-type expr)))

      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-match
       :type (tc:qualified-ty-type qual-ty)
       :expr (translate-expression (tc:node-if-expr expr) ctx env)
       :branches (list
                  (make-match-branch
                   :pattern (make-pattern-constructor
                             :type tc:*boolean-type*
                             :name true-value
                             :patterns nil)
                   :body (translate-expression (tc:node-if-then expr) ctx env))
                  (make-match-branch
                   :pattern (make-pattern-constructor
                             :type tc:*boolean-type*
                             :name false-value
                             :patterns nil)
                   :body (translate-expression (tc:node-if-else expr) ctx env))))))

  (:method ((expr tc:node-when) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((coalton-package (util:find-package "COALTON"))
           (true-value (util:find-symbol "TRUE" coalton-package))
           (false-value (util:find-symbol "FALSE" coalton-package))
           (unit-value (util:find-symbol "UNIT" coalton-package)))

      (make-node-match
       :type tc:*unit-type*
       :expr (translate-expression (tc:node-when-expr expr) ctx env)
       :branches (list
                  (make-match-branch
                   :pattern (make-pattern-constructor
                             :type tc:*boolean-type*
                             :name true-value
                             :patterns nil)
                   :body (translate-expression (tc:node-when-body expr) ctx env))
                  (make-match-branch
                   :pattern (make-pattern-constructor
                             :type tc:*boolean-type*
                             :name false-value
                             :patterns nil)
                   :body (make-node-variable
                          :type tc:*unit-type*
                          :value unit-value))))))

  (:method ((expr tc:node-unless) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((coalton-package (util:find-package "COALTON"))
           (true-value (util:find-symbol "TRUE" coalton-package))
           (false-value (util:find-symbol "FALSE" coalton-package))
           (unit-value (util:find-symbol "UNIT" coalton-package)))

      (make-node-match
       :type tc:*unit-type*
       :expr (translate-expression (tc:node-unless-expr expr) ctx env)
       :branches (list
                  (make-match-branch
                   :pattern (make-pattern-constructor
                             :type tc:*boolean-type*
                             :name false-value
                             :patterns nil)
                   :body (translate-expression (tc:node-unless-body expr) ctx env))
                  (make-match-branch
                   :pattern (make-pattern-constructor
                             :type tc:*boolean-type*
                             :name true-value
                             :patterns nil)
                   :body (make-node-variable
                          :type tc:*unit-type*
                          :value unit-value))))))

  (:method ((expr tc:node-while) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (make-node-while
     :type tc:*unit-type*
     :label (tc:node-while-label expr)
     :expr (translate-expression (tc:node-while-expr expr) ctx env)
     :body (translate-expression (tc:node-while-body expr) ctx env)))

  (:method ((expr tc:node-while-let) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (make-node-while-let
     :type tc:*unit-type*
     :pattern (translate-pattern (tc:node-while-let-pattern expr))
     :label (tc:node-while-let-label expr)
     :expr (translate-expression (tc:node-while-let-expr expr) ctx env)
     :body (translate-expression (tc:node-while-let-body expr) ctx env)))

  (:method ((expr tc:node-for) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (let* ((pat-arg
             (translate-pattern (tc:node-for-pattern expr)))

           (pat-arg-ty
             (pattern-type pat-arg))

           (classes-package
             (util:find-package "COALTON-LIBRARY/CLASSES"))

           (some
             (util:find-symbol "SOME" classes-package))

           (optional
             (util:find-symbol "OPTIONAL" classes-package))

           (optional-pat-arg-ty
             (tc:apply-type-argument
              (tc:make-tycon :name optional
                             :kind (tc:make-kfun :from tc:+kstar+ :to tc:+kstar+))
              pat-arg-ty))

           (some-pattern
             (make-pattern-constructor
              :type optional-pat-arg-ty
              :name some
              :patterns (list pat-arg)))

           (into-iter-arg
             (translate-expression (tc:node-for-expr expr) ctx env))

           (into-iter-arg-ty
             (node-type into-iter-arg))

           (iterator-package
             (util:find-package "COALTON-LIBRARY/ITERATOR"))

           (into-iter-method
             (util:find-symbol "INTO-ITER" iterator-package))

           (intoiter-class
             (util:find-symbol "INTOITERATOR" iterator-package))

           (iterator
             (util:find-symbol "ITERATOR" iterator-package))

           (iter-ty
             (tc:apply-type-argument
              (tc:make-tycon :name iterator
                             :kind (tc:make-kfun :from tc:+kstar+ :to tc:+kstar+))
              pat-arg-ty))

           (intoiterator-pred
             (tc:make-ty-predicate
              :class intoiter-class
              :types (list into-iter-arg-ty pat-arg-ty)
              :location (source:location expr)))

           (into-iter-node
             (make-node-application
              :type iter-ty
              :rator (make-node-variable
                      :type (tc:make-function-type*
                             (list (pred-type intoiterator-pred env)
                                   (node-type into-iter-arg))
                             iter-ty)
                      :value into-iter-method)
              :rands (list
                      (resolve-dict intoiterator-pred ctx env)
                      into-iter-arg)))

           (next-method
             (util:find-symbol "NEXT!" iterator-package))

           (into-iter-binding-var-name
             (gensym "ITER-"))

           (iter-next-node
             (make-node-application
              :type optional-pat-arg-ty
              :rator (make-node-variable
                      :type (tc:make-function-type*
                             (list iter-ty)
                             optional-pat-arg-ty)
                      :value next-method)
              :rands (list (make-node-variable
                            :type iter-ty
                            :value  into-iter-binding-var-name))))

           (while-let-node
             (make-node-while-let
              :type tc:*unit-type*
              :label (tc:node-for-label expr)
              :pattern some-pattern
              :expr iter-next-node
              :body (translate-expression (tc:node-for-body expr) ctx env))))

      (make-node-bind
       :type tc:*unit-type*
       :name into-iter-binding-var-name
       :expr into-iter-node
       :body while-let-node)))

  (:method ((expr tc:node-loop) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (make-node-loop
     :type tc:*unit-type*
     :label (tc:node-loop-label expr)
     :body (translate-expression (tc:node-loop-body expr) ctx env)))

  (:method ((expr tc:node-break) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (make-node-break
     :type tc:*unit-type*
     :label (tc:node-break-label expr)))

  (:method ((expr tc:node-continue) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (make-node-continue
     :type tc:*unit-type*
     :label (tc:node-continue-label expr)))

  (:method ((expr tc:node-cond) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((coalton-package (util:find-package "COALTON"))
           (true-value (util:find-symbol "TRUE" coalton-package))
           (false-value (util:find-symbol "FALSE" coalton-package))

           (qual-ty (tc:node-type expr)))

      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (loop :with out-node := (make-node-lisp
                               :type (tc:qualified-ty-type qual-ty)
                               :vars nil
                               :form '((cl:error "Non-exhaustive COND.")))
            :for clause :in (reverse (tc:node-cond-clauses expr)) :do
              (setf out-node
                    (make-node-match
                     :type (tc:qualified-ty-type qual-ty)
                     :expr (translate-expression (tc:node-cond-clause-expr clause) ctx env)
                     :branches (list
                                (make-match-branch
                                 :pattern (make-pattern-constructor
                                           :type tc:*boolean-type*
                                           :name true-value
                                           :patterns nil)
                                 :body (translate-expression (tc:node-cond-clause-body clause) ctx env))
                                (make-match-branch
                                 :pattern (make-pattern-constructor
                                           :type tc:*boolean-type*
                                           :name false-value
                                           :patterns nil)
                                 :body out-node))))
            :finally (return out-node))))

  (:method ((node tc:node-do) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((classes-package (util:find-package "COALTON-LIBRARY/CLASSES"))

           (monad-symbol (util:find-symbol "MONAD" classes-package))

           (bind-symbol (util:find-symbol ">>=" classes-package))

           (m-type (tc:tapp-from (tc:qualified-ty-type (tc:node-type node))))

           (pred (tc:make-ty-predicate
                  :class monad-symbol
                  :types (list m-type)
                  :location (source:location node))))

      (loop :with out-node := (translate-expression (tc:node-do-last-node node) ctx env)

            :for elem :in (reverse (tc:node-do-nodes node))
            :do (setf out-node
                      (etypecase elem
                        (tc:node-bind
                         (let ((pattern (tc:node-bind-pattern elem)))
                           (typecase (tc:node-bind-pattern elem)
                             (tc:pattern-var
                              (make-node-bind
                               :type (node-type out-node)
                               :name (tc:pattern-var-name pattern)
                               :expr (translate-expression (tc:node-bind-expr elem) ctx env)
                               :body out-node))
                             (t
                              (make-node-match
                               :type (node-type out-node)
                               :expr (translate-expression (tc:node-bind-expr elem) ctx env)
                               :branches (list
                                          (make-match-branch
                                           :pattern (translate-pattern pattern)
                                           :body out-node)))))))


                        ;; *sad burrito noises*
                        (tc:node-do-bind
                         (let* ((var-type (tc:qualified-ty-type (tc:pattern-type (tc:node-do-bind-pattern elem))))

                                (callback-ty (tc:make-function-type var-type (node-type out-node)))

                                (var-name (gensym)))

                           (make-node-application
                            :type (node-type out-node)
                            :rator (make-node-variable
                                    :type (tc:make-function-type* ; (Monad :m => m :a -> (:a -> :m :b) -> :m :b)
                                           (list (pred-type pred env)
                                                 (tc:qualified-ty-type (tc:node-type (tc:node-do-bind-expr elem)))
                                                 callback-ty)
                                           (node-type out-node))
                                    :value bind-symbol)
                            :rands (list
                                    (resolve-dict pred ctx env)
                                    (translate-expression (tc:node-do-bind-expr elem) ctx env)
                                    (make-node-abstraction
                                     :type callback-ty
                                     :vars (list var-name)
                                     :subexpr (make-node-match
                                               :type (node-type out-node)
                                               :expr (make-node-variable
                                                      :type var-type
                                                      :value var-name)
                                               :branches (list
                                                          (make-match-branch
                                                           :pattern (translate-pattern
                                                                     (tc:node-do-bind-pattern elem))
                                                           :body out-node))))))))

                        ;; Same as node-do-bind but without binding
                        ;; the result of the computation to a
                        ;; variable.
                        (tc:node
                         (let* ((var-type (tc:tapp-to (tc:qualified-ty-type (tc:node-type elem))))

                                (callback-ty (tc:make-function-type var-type (node-type out-node)))

                                (var-name (gensym)))

                           (make-node-application
                            :type (node-type out-node)
                            :rator (make-node-variable
                                    :type (tc:make-function-type* ; (Monad :m => m :a -> (:a -> :m :b) -> :m :b)
                                           (list (pred-type pred env)
                                                 (tc:qualified-ty-type (tc:node-type elem))
                                                 callback-ty)
                                           (node-type out-node))
                                    :value bind-symbol)
                            :rands (list
                                    (resolve-dict pred ctx env)
                                    (translate-expression elem ctx env)
                                    (make-node-abstraction
                                     :type callback-ty
                                     :vars (list var-name)
                                     :subexpr out-node)))))))

            :finally (return out-node)))))

(defgeneric translate-pattern (pat)
  (:documentation "Translate the typechecker AST pattern to the codegen AST.")
  (:method ((pat tc:pattern-var))
    (let ((qual-ty (tc:pattern-type pat)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-pattern-var
       :type (tc:qualified-ty-type qual-ty)
       :name (tc:pattern-var-name pat))))

  (:method ((pat tc:pattern-literal))
    (let ((qual-ty (tc:pattern-type pat)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-pattern-literal
       :type (tc:qualified-ty-type qual-ty)
       :value (tc:pattern-literal-value pat))))

  (:method ((pat tc:pattern-wildcard))
    (let ((qual-ty (tc:pattern-type pat)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-pattern-wildcard
       :type (tc:qualified-ty-type qual-ty))))

  (:method ((pat tc:pattern-constructor))
    (let ((qual-ty (tc:pattern-type pat)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-pattern-constructor
       :type (tc:qualified-ty-type qual-ty)
       :name (tc:pattern-constructor-name pat)
       :patterns (mapcar #'translate-pattern (tc:pattern-constructor-patterns pat))))))


(defun apply-dicts (expr ctx env)
  "If there are predicates on EXPR, then find the typeclass dictionaries
in the environment and create a NODE-APPLICATION with all required
dictionaries applied."
  (declare (type tc:node expr)
           (type pred-context ctx)
           (type tc:environment env)
           (values node))
  (let* ((qual-ty (tc:node-type expr))

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
             (tc:node-variable
              (make-node-variable
               :type var-type
               :value (tc:node-variable-name expr)))
             (t
              (translate-expression expr ctx env)))))
    (cond
      ((null dicts)
       inner-node)
      (t
       (make-node-application
        :type (tc:qualified-ty-type qual-ty)
        :rator inner-node
        :rands dicts)))))

(defun wrap-with-pattern-params (pattern-params inner)
  "Wrap INNER in nested `NODE-MATCH' expressions to pattern match on PATTERN-PARAMS"
  (declare (type list pattern-params)
           (type node inner)
           (values node))
  (loop :for (name . pattern) :in (reverse pattern-params)

        :do (setf inner (make-node-match
                         :type (node-type inner)
                         :expr (make-node-variable
                                :type (tc:qualified-ty-type (tc:pattern-type pattern))
                                :value name)
                         :branches
                         (list
                          (make-match-branch
                           :pattern (translate-pattern pattern)
                           :body inner))))

        :finally (return inner)))

(defun wrap-in-block (inner)
  "Check whether INNER contains any return statements which need a
block. If so, wrap INNER in a block named for the current function.
Otherwise, return INNER unchanged.

There is currently no way for any return node to jump outside its
function's block, but this traversal handles the most general case by
checking for any return nodes targeting *CURRENT-FUNCTION-NAME* which
don't occur in an already existing block with a matching label."
  (declare (type node inner)
           (values node &optional))
  (traverse
   inner
   (list
    (action (:traverse node-block node)
      (unless (eq *current-function-name* (node-block-name node))
        (funcall *traverse* (node-block-body node)))
      (values))
    (action (:before node-return-from node)
      (when (eq *current-function-name* (node-return-from-name node))
        (return-from wrap-in-block
          (make-node-block
           :type (node-type inner)
           :name *current-function-name*
           :body inner))))))
  inner)
