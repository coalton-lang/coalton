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
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:translate-toplevel                   ; FUNCTION
   #:translate-expression                 ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/translate-expression)

(defun translate-toplevel (binding env &key extra-context)
  (declare (type tc:binding-type binding)
           (type tc:environment env)
           (type pred-context extra-context))

  (let* ((qual-ty (tc:binding-type binding))

         (preds (tc:qualified-ty-predicates qual-ty))

         (ctx
           (loop :for pred :in preds
                 :collect (cons pred (gensym "DICT"))))

         (full-ctx (append ctx extra-context)))

    (let ((node (cond
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
                            (loop :for var :in (tc:node-abstraction-vars (tc:binding-last-node binding))
                                  :for qual-ty := (tc:node-type var)
                                  :do (assert (null (tc:qualified-ty-predicates qual-ty)))
                                  :collect (tc:qualified-ty-type qual-ty)))
                           (tc:function-remove-arguments 
                            (tc:qualified-ty-type
                             (tc:node-type (tc:binding-last-node binding)))
                            (length (tc:node-abstraction-vars (tc:binding-last-node binding)))))

                    :vars (append
                           (loop :for (pred . name) :in ctx
                                 :collect name)
                           (mapcar #'tc:node-variable-name (tc:node-abstraction-vars (tc:binding-last-node binding))))

                    :subexpr (translate-expression (tc:node-abstraction-body (tc:binding-last-node binding)) full-ctx env)))

                  ;; If the binding has parameters and/or predicates then wrap the body in a lambda.
                  ((or (tc:binding-parameters binding) preds)
                   (make-node-abstraction
                    :type (tc:make-function-type*
                           (append
                            (loop :for pred :in preds
                                  :collect (pred-type pred env))
                            (loop :for var :in (tc:binding-parameters binding)
                                  :for qual-ty := (tc:node-type var)

                                  :do (assert (null (tc:qualified-ty-predicates qual-ty)))
                                  :collect (tc:qualified-ty-type qual-ty)))
                           (tc:qualified-ty-type (tc:node-type (tc:binding-last-node binding))))

                    :vars (append
                           (loop :for (pred . name) :in ctx
                                 :collect name)
                           (mapcar #'tc:node-variable-name (tc:binding-parameters binding)))

                    :subexpr (translate-expression (tc:binding-value binding) full-ctx env)))

                  (t
                   (translate-expression (tc:binding-value binding) full-ctx env)))))

      node)))

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
      
      (let* ((classes-package (find-package "COALTON-LIBRARY/CLASSES"))

             (num-class (util:find-symbol "NUM" classes-package))

             (num-pred (tc:make-ty-predicate :class num-class :types (list (tc:qualified-ty-type qual-ty))))

             (from-int-method (util:find-symbol "FROMINT" classes-package)))
        
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
                  :value (tc:node-integer-literal-value expr)))))))

  (:method ((expr tc:node-variable) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node &optional))

    (apply-dicts expr ctx env))

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

    (let* ((qual-ty (tc:node-type expr))

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

           (vars (append
                  dict-var-names
                  (loop :for var :in (tc:node-abstraction-vars expr)
                        :for name := (tc:node-variable-name var)
                        :for qual-ty := (tc:node-type var)
                        
                        :do (assert (null (tc:qualified-ty-predicates qual-ty)))
                            
                        :collect name))))

      (assert (not (some #'tc:static-predicate-p preds)))
      
      (make-node-abstraction
       :type (tc:make-function-type* dict-types (tc:qualified-ty-type qual-ty))
       :vars vars
       :subexpr (translate-expression (tc:node-abstraction-body expr) ctx env))))

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
                       
                       :collect (cons name (translate-toplevel binding env :extra-context ctx)))
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

      (let* ((coalton-package (find-package "COALTON"))

             (unit-value (util:find-symbol "UNIT" coalton-package)))
        
        (make-node-return
         :type (tc:qualified-ty-type qual-ty)
         :expr (if (tc:node-return-expr expr)
                   (translate-expression (tc:node-return-expr expr) ctx env)
                   (make-node-variable
                    :type tc:*unit-type*
                    :value unit-value))))))

  (:method ((expr tc:node-or) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((coalton-package (find-package "COALTON"))
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

    (let* ((coalton-package (find-package "COALTON"))
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

    (let* ((coalton-package (find-package "COALTON"))
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

    (let* ((coalton-package (find-package "COALTON"))
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

    (let* ((coalton-package (find-package "COALTON"))
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

  (:method ((expr tc:node-cond) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((coalton-package (find-package "COALTON"))
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

    (let* ((classes-package (find-package "COALTON-LIBRARY/CLASSES"))

           (monad-symbol (util:find-symbol "MONAD" classes-package))

           (bind-symbol (util:find-symbol ">>=" classes-package))

           (m-type (tc:tapp-from (tc:qualified-ty-type (tc:node-type node))))

           (pred (tc:make-ty-predicate
                  :class monad-symbol
                  :types (list m-type)
                  :source (tc:node-source node))))

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

                                (var-name (gentemp)))

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

                                (var-name (gentemp)))

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
