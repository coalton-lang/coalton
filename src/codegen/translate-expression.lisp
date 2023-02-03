;;;
;;; This file provides functions for translating from the typechecker
;;; AST to the codegen AST.
;;;

(defpackage #:coalton-impl/codegen/translate-expression
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/codegen/ast
   #:coalton-impl/codegen/resolve-instance)
  (:import-from
   #:coalton-impl/codegen/typecheck-node
   #:typecheck-node)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:error #:coalton-impl/error))
  (:export
   #:translate-toplevel                   ; FUNCTION
   #:translate-expression                 ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/translate-expression)

(defun translate-toplevel (inferred-type expr env &key extra-context)
  "Translate a toplevel typechecker AST node EXPR to the codegen AST,
ensuring attached type dictionaries match the declared predicate
order.

EXTRA-CONTEXT provides the current predicate context.

Returns a COALTON-IMPL/CODEGEN/AST:NODE."
  (declare (type tc:qualified-ty inferred-type)
           ;(type tc:node expr) ;; TODO
           (type tc:environment env)
           (type pred-context extra-context)
           (values node &optional))

  (let* (;; Match the inferred-type against the node-type. This makes
         ;; the type variables in ctx match those in the ast node.
         (inferred-type-ty (tc:qualified-ty-type inferred-type))

         (inferred-type-preds (tc:qualified-ty-predicates inferred-type))

         (node-type (tc:qualified-ty-type (tc:node-type expr)))

         (subs (tc:match inferred-type-ty node-type))

         (preds (tc:apply-substitution subs inferred-type-preds))

         (ctx (loop :for pred :in preds
                    :collect (cons pred (gensym "DICT"))))

         (full-ctx (append ctx extra-context))

         (node
           (cond
             ;;
             ;; Reorder predicates to match definition order
             ;;
             ((tc:node-abstraction-p expr)
              (let ((subnode (translate-expression (tc:node-abstraction-body expr) full-ctx env)))
                (make-node-abstraction
                 :type (tc:make-function-type*
                        (append
                         (loop :for pred :in preds
                               :collect (pred-type pred env))
                         (loop :for var :in (tc:node-abstraction-vars expr)
                               :for name := (tc:node-variable-name var)
                               :for type := (tc:node-type var)
                               ;; TODO: I'm only about 50% sure this
                               ;; should be here. We will see when the
                               ;; tests run.
                               :do (assert (null (tc:qualified-ty-predicates type)))
                               
                               :collect (tc:qualified-ty-type type)))
                        (node-type subnode))
                 :vars (append
                        (mapcar #'cdr ctx)
                        (mapcar #'tc:node-variable-name (tc:node-abstraction-vars expr)))
                 :subexpr subnode)))

             (ctx
              (let ((inner (translate-expression expr full-ctx env)))
                (make-node-abstraction
                 :type (tc:make-function-type*
                        (loop :for pred :in preds
                              :collect (pred-type pred env))
                        (node-type inner))
                 :vars (mapcar #'cdr ctx)
                 :subexpr inner)))

             (t
              (translate-expression expr full-ctx env)))))

    (typecheck-node node env)

    node))

(defgeneric translate-expression (expr ctx env)
  (:documentation "Translate typechecker AST node EXPR to the codegen AST.

CTX provides the current predicate context.

Returns a COALTON-IMPL/CODEGEN/AST:NODE.")

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
      (assert (= 1 (length (tc:qualified-ty-predicates qual-ty))))
      
      (let* ((num-pred (first (tc:qualified-ty-predicates qual-ty)))

             (classes-package (find-package "COALTON-LIBRARY/CLASSES"))

             (num-class (find-symbol "NUM" classes-package))

             (from-int-method (find-symbol "FROMINT" classes-package)))
        
        (assert (eq (tc:ty-predicate-class num-pred) num-class))

        (make-node-application
         :type (tc:qualified-ty-type qual-ty)
         :rator (make-node-variable
                 :type (tc:make-function-type
                        tc:*integer-type*
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
                     (let ((qual-ty (tc:node-type (tc:node-bind-expr body-node))))
                       (assert (null (tc:qualified-ty-predicates qual-ty)))

                       (let ((pattern (tc:node-bind-pattern body-node)))
                         (typecase (tc:node-bind-pattern body-node)
                           (tc:pattern-var
                            (make-node-bind
                             :type (tc:qualified-ty-type qual-ty)
                             :name (tc:pattern-var-name pattern)
                             :expr (translate-expression (tc:node-bind-expr body-node) ctx env)
                             :body out-node))
                           (t
                            (make-node-match
                             :type (tc:qualified-ty-type qual-ty)
                             :expr (translate-expression (tc:node-bind-expr body-node) ctx env)
                             :branches (make-match-branch
                                        :pattern (translate-pattern (tc:pattern-type pattern))
                                        :body out-node)))))))
                    
                    (tc:node
                     (let ((qual-ty (tc:node-type body-node)))
                       (assert (null (tc:qualified-ty-predicates qual-ty)))

                       (make-node-seq
                        :type (tc:qualified-ty-type qual-ty)
                        :nodes (list body-node out-node))))))
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
                       :for var := (tc:node-let-binding-name binding)
                       
                       :for name := (tc:node-variable-name var)
                       :for node := (tc:node-let-binding-value binding)
                       :for explicit-type := (tc:node-let-binding-explicit-type binding)

                       :if explicit-type
                         :collect (cons name (translate-toplevel explicit-type node env :extra-context ctx))
                       :else
                         :collect (cons name (translate-expression node ctx env)))
       :subexpr (translate-expression (tc:node-let-body expr) ctx env))))

  (:method ((expr tc:node-lisp) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-lisp
       :type (tc:qualified-ty-type qual-ty)
       :vars (tc:node-lisp-vars expr)
       :form (cst:raw (tc:node-lisp-body expr)))))

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

             (unit-value (find-symbol "UNIT" coalton-package)))
        
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
           (true-value (find-symbol "TRUE" coalton-package))
           (false-value (find-symbol "FALSE" coalton-package)))

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
           (true-value (find-symbol "TRUE" coalton-package))
           (false-value (find-symbol "FALSE" coalton-package)))

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
           (true-value (find-symbol "TRUE" coalton-package))
           (false-value (find-symbol "FALSE" coalton-package)))

      (make-node-match
       :type tc:*boolean-type*
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
           (true-value (find-symbol "TRUE" coalton-package))
           (false-value (find-symbol "FALSE" coalton-package))
           (unit-value (find-symbol "UNIT" coalton-package)))

      (make-node-match
       :type tc:*boolean-type*
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
           (true-value (find-symbol "TRUE" coalton-package))
           (false-value (find-symbol "FALSE" coalton-package))
           (unit-value (find-symbol "UNIT" coalton-package)))

      (make-node-match
       :type tc:*boolean-type*
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
           (true-value (find-symbol "TRUE" coalton-package))
           (false-value (find-symbol "FALSE" coalton-package))

           (qual-ty (tc:node-type expr)))

      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (loop :with out-node := (make-node-lisp
                               :type (tc:qualified-ty-type qual-ty)
                               :vars nil
                               :form '(cl:error "Non-exhaustive COND."))
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

  ;; TODO: node-do
  )

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
