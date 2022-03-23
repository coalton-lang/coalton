(defpackage #:coalton-impl/codegen/compile-instance
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker))
  (:import-from
   #:coalton-impl/codegen/compile-expression
   #:compile-expression)
  (:import-from
   #:coalton-impl/codegen/typecheck-node
   #:typecheck-node)
  (:import-from
   #:coalton-impl/codegen/resolve-instance
   #:pred-type
   #:resolve-dict))

(in-package #:coalton-impl/codegen/compile-instance)

(defun compile-instance (instance add-inline env)
  "Generate a series of bindings from INSTANCE"
  (declare (type tc:instance-definition instance)
           (type tc:environment env))

  (let* ((class (tc:lookup-class
                 env
                 (tc:instance-definition-class-name instance)))

         (method-codegen-syms
           (tc:instance-definition-method-codegen-syms instance))

         (method-nodes
           (tc:instance-definition-methods instance))

         ;; Build a context for the instance
         (ctx
           (loop :for pred :in (tc:instance-definition-context instance)
                 :collect (cons pred (gensym))))
         (ctx-ty
           (loop :for pred :in (tc:instance-definition-context instance)
                 :collect (pred-type pred env)))

         ;; Iterate through methods in the order they were defined on the class
         ;; building a creating a binding for each method 
         (method-definitions
           (loop :for (method-name . type) :in (tc:ty-class-unqualified-methods class)
                 :for method-node := (gethash method-name method-nodes)
                 :for codegen-name := (gethash method-name method-codegen-syms)

                 :for method-qual-ty := (tc:ty-scheme-type (tc:typed-node-type method-node))

                 :for method-ctx
                   := (append
                       ctx
                       (loop :for pred :in (remove (tc:instance-definition-predicate instance)
                                                   (tc:qualified-ty-predicates method-qual-ty)
                                                   :test #'equalp)
                             :collect (cons pred (gensym))))

                 :for node
                   := (cond
                        ;; If the method has context but is not a
                        ;; function then wrap it in function
                        ((and method-ctx (not (tc:typed-node-abstraction-p method-node)))
                         (let ((inner (compile-expression method-node method-ctx env)))
                           (node-abstraction
                            (tc:make-function-type*
                             (loop :for (pred . name) :in method-ctx
                                   :collect (pred-type pred env))
                             (node-type inner))
                            (mapcar #'cdr method-ctx)
                            inner)))

                        ;; Otherwise compile the method normally
                        (t (compile-expression method-node nil env)))

                 ;; Don't inline recursive methods
                 :do (unless (find method-name (node-variables node))
                       (funcall add-inline codegen-name))

                 :collect (cons codegen-name node)))

         ;; Iterate through the methods creating a node for each.
         ;; The instance predicate is removed from individual methods context
         ;; because it will be closed over
         (unqualified-method-definitions
           (loop :for (method-name . type) :in (tc:ty-class-unqualified-methods class)
                 :for method-node := (gethash method-name method-nodes)
                 :for method-qual := (tc:ty-scheme-type (tc:typed-node-type method-node))

                 :for method-preds := (remove-if
                                       (lambda (pred)
                                         (member pred (tc:instance-definition-context instance) :test #'equalp))
                                       (tc:qualified-ty-predicates method-qual))

                 :for method-ctx
                   := (append
                       ctx
                       (loop :for pred :in method-preds
                             :collect (cons pred (gensym))))

                 :for unqualified-scheme := (tc:to-scheme
                                             (tc:qualify
                                              method-preds
                                              (tc:qualified-ty-type method-qual)))

                 :for unqualified-node := (tc:replace-node-type method-node unqualified-scheme)

                 :for node
                   := (cond
                        ((and method-ctx (not (tc:typed-node-abstraction-p unqualified-node)))
                         (let ((inner (compile-expression unqualified-node method-ctx env)))
                           (node-abstraction
                            (tc:make-function-type*
                             (loop :for (pred . name) :in method-ctx
                                   :collect (pred-type pred env))
                             (node-type inner))
                            (mapcar #'cdr method-ctx)
                            inner)))

                        (t (compile-expression unqualified-node method-ctx env)))

                 :collect node))

         (method-ty (mapcar #'node-type unqualified-method-definitions))

         (subs (tc:predicate-match (tc:ty-class-predicate class) (tc:instance-definition-predicate instance)))

         (superclass-preds (tc:apply-substitution subs (mapcar #'car (tc:ty-class-superclass-dict class))))

         (superclass-ty
           (loop :for pred :in superclass-preds
                 :collect (pred-type pred env)))

         ;; Initial node
         (var-node
           (node-variable
            (tc:make-function-type*
             (append
              superclass-ty
              method-ty)
             (pred-type (tc:instance-definition-predicate instance) env))
            (tc:ty-class-codegen-sym class)))

         ;; If the instance has methods then apply them
         (app-node
           (if unqualified-method-definitions
               (node-application
                (pred-type (tc:instance-definition-predicate instance) env)
                var-node
                (append
                 (loop :for pred :in superclass-preds
                       :collect (resolve-dict pred ctx env))
                 unqualified-method-definitions))
               var-node))

         ;; If the instances has context then wrap it in a function
         (dict-node
           (if ctx
               (node-abstraction
                (tc:make-function-type*
                 ctx-ty
                 (pred-type (tc:instance-definition-predicate instance) env))
                (mapcar #'cdr ctx)
                app-node)
               app-node))

         (bindings
           (cons
            (cons
             (tc:instance-definition-codegen-sym instance)
             dict-node)
            method-definitions)))

    (loop :for (name . node) :in bindings
          :do (typecheck-node node env))

    bindings))
