(defpackage #:coalton-impl/codegen/translate-instance
  (:use
   #:cl
   #:coalton-impl/codegen/ast
   #:coalton-impl/codegen/resolve-instance)
  (:import-from
   #:coalton-impl/codegen/typecheck-node
   #:typecheck-node)
  (:import-from
   #:coalton-impl/codegen/resolve-instance
   #:pred-type
   #:resolve-dict)
  (:import-from
   #:coalton-impl/codegen/translate-expression
   #:translate-toplevel)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:translate-instance                   ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/translate-instance)

(defun translate-instance (instance env)
  (declare (type tc:toplevel-define-instance instance)
           (type tc:environment env))

  (let* ((pred (tc:toplevel-define-instance-pred instance))

         (instance-entry (tc:lookup-class-instance env pred))

         (method-codegen-syms
           (tc:ty-class-instance-method-codegen-syms instance-entry))

         (class-name (tc:ty-predicate-class pred))

         (class (tc:lookup-class env class-name))

         ;; Build a context for the instance
         (ctx
           (loop :for pred :in (tc:toplevel-define-instance-context instance)
                 :collect (cons pred (gensym))))

         (ctx-ty
           (loop :for pred :in (tc:toplevel-define-instance-context instance)
                 :collect (pred-type pred env)))

         (subs (tc:predicate-match (tc:ty-class-predicate class) pred))

         (superclass-preds (tc:apply-substitution subs (mapcar #'car (tc:ty-class-superclass-dict class))))

         ;; These will be methods with the dict arguments
         (method-definitions
           (loop :for method :in (tc:ty-class-unqualified-methods class)
                 :for method-name := (tc:ty-class-method-name method)
                 :for binding := (gethash method-name (tc:toplevel-define-instance-methods instance))
                 :for codegen-sym :in method-codegen-syms
                 :collect (cons codegen-sym (translate-toplevel binding env method-name))))

         ;; These will be methods without the dict arguments
         (unqualified-method-definitions
           (loop :for method :in (tc:ty-class-unqualified-methods class)
                 :for method-name := (tc:ty-class-method-name method)
                 :for method-type := (tc:apply-substitution subs (tc:ty-class-method-type method))
                 :for binding := (gethash method-name (tc:toplevel-define-instance-methods instance))
                 :collect (translate-toplevel (tc:attach-explicit-binding-type binding (tc:fresh-inst method-type))
                                              env
                                              method-name
                                              :extra-context ctx)))

         (method-ty (mapcar #'node-type unqualified-method-definitions))

         (superclass-ty
           (loop :for pred :in superclass-preds
                 :collect (pred-type pred env)))

         ;; Initial node
         (var-node
           (make-node-variable
            :type (tc:make-function-type*
                   (append
                    superclass-ty
                    method-ty)
                   (pred-type pred env))
            :value (tc:ty-class-codegen-sym class)))

         ;; If the instance has methods then apply them
         (app-node
           (if (and (endp unqualified-method-definitions) (endp superclass-preds))
               var-node
               (make-node-application
                :type (pred-type pred env)
                :properties '()
                :rator var-node
                :rands (append
                        ;; Superclass dictionary arguments
                        (loop :for pred :in superclass-preds
                              :collect (resolve-dict pred ctx env))

                        ;; The COND below could be replaced simply
                        ;; with
                        ;;
                        ;;     unqualified-method-definitions
                        ;;
                        ;; however, this will duplicate the method
                        ;; bodies in the final codegen output. As
                        ;; such, instead, we want to look up the
                        ;; symbols that these methods are bound to
                        ;; (METHOD-CODEGEN-SYMS) and apply them to the
                        ;; context variables as needed.
                        (cond
                          ;; In this case, there is no context, so we
                          ;; can just refer to the method symbols
                          ;; directly.
                          ((null ctx)
                           (loop :for sym :in method-codegen-syms
                                 :for ty :in method-ty
                                 :collect (make-node-variable
                                           :type ty
                                           :value sym)))

                          ;; In this case, we have to take a method symbol M and construct an appliaction
                          ;;
                          ;;    (M CTX1 CTX2 ... CTXn).
                          ;;
                          ;; This in turns transforms a qualified type
                          ;; (of M) to an unqualified type (of the
                          ;; application of M).
                          (t
                           (loop :for qual-sym :in method-codegen-syms
                                 :for unqual-method :in unqualified-method-definitions
                                 :for unqual-type :in method-ty
                                 :for qual-method :in (mapcar #'cdr method-definitions)
                                 :for qual-type := (node-type qual-method)
                                 :collect (make-node-application
                                           :type unqual-type
                                           :properties '()
                                           :rator (make-node-variable
                                                   :type qual-type
                                                   :value qual-sym)
                                           :rands (loop :for (pred . ctx-var) :in ctx
                                                        :for ctx-ty := (pred-type pred env)
                                                        :collect (make-node-variable
                                                                  :type ctx-ty
                                                                  :value ctx-var))))))))))

         (dict-node
           (if ctx
               (make-node-abstraction
                :type (tc:make-function-type*
                       ctx-ty
                       (node-type app-node))
                :vars (mapcar #'cdr ctx)
                :subexpr app-node)
               app-node))

         (bindings
           (acons
            (tc:ty-class-instance-codegen-sym instance-entry)
            dict-node
            method-definitions)))

    (loop :for (name . node) :in bindings
          :do (typecheck-node node env))

    bindings))
