(defpackage #:coalton-impl/codegen/translate-instance
  (:use
   #:cl
   #:coalton-impl/util
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
   (#:tc #:coalton-impl/typechecker)
   (#:error #:coalton-impl/error))
  (:export
   #:translate-instance                   ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/translate-instance)

;;; TODO: think about inlining

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

         (method-definitions
           (loop :for (method-name . type) :in (tc:ty-class-unqualified-methods class)
                 :for binding := (gethash method-name (tc:toplevel-define-instance-methods instance))
                 :for codegen-sym := (gethash method-name method-codegen-syms)

                 :collect (cons codegen-sym (translate-toplevel binding env))))

         (unqualified-method-definitions
           (loop :for (method-name . type) :in (tc:ty-class-unqualified-methods class)
                 :for binding := (gethash method-name (tc:toplevel-define-instance-methods instance))
                 :collect (translate-toplevel (tc:attach-explicit-binding-type binding (tc:fresh-inst type))
                                              env
                                              :extra-context ctx)))

         (method-ty (mapcar #'node-type unqualified-method-definitions))

         (subs (tc:predicate-match (tc:ty-class-predicate class) pred))

         (superclass-preds (tc:apply-substitution subs (mapcar #'car (tc:ty-class-superclass-dict class))))

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
           ;; TODO: should this be (or unqualified-method-definitions superclass-preds)
           (if unqualified-method-definitions 
               (make-node-application
                :type (pred-type pred env)
                :rator var-node
                :rands (append
                        (loop :for pred :in superclass-preds
                              :collect (resolve-dict pred ctx env))
                        unqualified-method-definitions))))

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
           (cons
            (cons
             (tc:ty-class-instance-codegen-sym instance-entry)
             dict-node)
            method-definitions)))

    (loop :for (name . node) :in bindings
          :do (typecheck-node node env))

    bindings))
 
