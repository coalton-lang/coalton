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
   #:bind-hidden-function-arguments
   #:translate-toplevel
   #:physical-callable-type)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:translate-instance                   ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/translate-instance)

(defun translate-qualified-method-value (qual-sym qual-type visible-type dict-nodes)
  (declare (type symbol qual-sym)
           (type tc:ty qual-type visible-type)
           (type list dict-nodes)
           (values node &optional))
  (let ((rator (make-node-variable
                :type qual-type
                :value qual-sym)))
    (if (tc:function-type-p visible-type)
        (bind-hidden-function-arguments
         rator
         (physical-callable-type visible-type)
         dict-nodes)
        (make-node-application
         :type visible-type
         :properties '()
         :rator rator
         :rands dict-nodes
         :keyword-rands nil))))

(defun strip-bound-context-predicates (qual-ty context-preds)
  (declare (type tc:qualified-ty qual-ty)
           (type tc:ty-predicate-list context-preds)
           (values tc:qualified-ty &optional))
  (let ((preds (tc:qualified-ty-predicates qual-ty)))
    (unless (<= (length context-preds) (length preds))
      (util:coalton-bug "Cannot strip ~S from qualified type ~S"
                        context-preds
                        qual-ty))
    (loop :for expected :in context-preds
          :for actual :in preds
          :unless (tc:type-predicate= actual expected)
            :do (util:coalton-bug
                 "Expected instance context predicate ~S at the front of ~S"
                 expected
                 qual-ty))
    (tc:make-qualified-ty
     :predicates (nthcdr (length context-preds) preds)
     :type (tc:qualified-ty-type qual-ty))))

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

         (ctx-nodes
           (loop :for (pred . ctx-var) :in ctx
                 :for ctx-ty := (pred-type pred env)
                 :collect (make-node-variable
                           :type ctx-ty
                           :value ctx-var)))

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
                 :for binding := (gethash method-name (tc:toplevel-define-instance-methods instance))
                 :for method-type := (strip-bound-context-predicates
                                      (tc:binding-type binding)
                                      (mapcar #'car ctx))
                 :collect (translate-toplevel (tc:attach-explicit-binding-type binding method-type)
                                              env
                                              method-name
                                              :extra-context ctx)))

         (method-ty (mapcar #'node-type unqualified-method-definitions))

         (superclass-ty
           (loop :for pred :in superclass-preds
                 :collect (pred-type pred env)))

         (superclass-dict-nodes
           (loop :for pred :in superclass-preds
                 :collect (resolve-dict pred ctx env)))

         (method-arg-nodes
           (cond
             ((null ctx)
              (loop :for sym :in method-codegen-syms
                    :for ty :in method-ty
                    :collect (make-node-variable
                              :type ty
                              :value sym)))
             (t
              (loop :for qual-sym :in method-codegen-syms
                    :for unqual-type :in method-ty
                    :for qual-method :in (mapcar #'cdr method-definitions)
                    :for qual-type := (node-type qual-method)
                    :collect (translate-qualified-method-value
                              qual-sym
                              qual-type
                              unqual-type
                              ctx-nodes)))))

         ;; Initial node
         (var-node
           (make-node-variable
            :type (tc:merge-function-input-types
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
                :rands (append superclass-dict-nodes method-arg-nodes))))

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
          :do (handler-case
                  (typecheck-node node env)
                (tc:coalton-internal-type-error (e)
                  (error e))))

    bindings))
