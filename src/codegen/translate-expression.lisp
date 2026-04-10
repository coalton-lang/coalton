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
   #:make-keyword-forwarders             ; FUNCTION
   #:bind-hidden-function-arguments      ; FUNCTION
   #:physical-callable-type              ; FUNCTION
   #:physical-callable-argument-types    ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/translate-expression)

(defun physical-callable-type (type)
  (declare (type tc:ty type)
           (values tc:ty &optional))
  type)

(defun physical-callable-argument-types (type)
  (declare (type tc:ty type)
           (values tc:ty-list &optional))
  (tc:function-type-arguments type))

(defun prepend-codegen-hidden-input-types (hidden-input-types type)
  "Prepend HIDDEN-INPUT-TYPES to the callable TYPE used by codegen.

Hidden dictionary arguments are part of the same generated callable as the
source-level parameters, even when the visible Coalton type is explicit-nullary.
That differs from generic type-level input merging, which sometimes needs to
preserve a nested function-valued result."
  (declare (type tc:ty-list hidden-input-types)
           (type tc:ty type)
           (values tc:ty &optional))
  (if (null hidden-input-types)
      type
      (typecase type
        (tc:function-ty
         (tc:make-function-ty
          :alias (tc:ty-alias type)
          :positional-input-types
          (append hidden-input-types
                  (tc:function-ty-positional-input-types type))
          :keyword-input-types (tc:function-ty-keyword-input-types type)
          :keyword-open-p (tc:function-ty-keyword-open-p type)
          :output-types (tc:function-ty-output-types type)))
        (t
         (tc:make-function-type* hidden-input-types type)))))

(defun make-keyword-forwarders (function-type)
  (declare (type tc:function-ty function-type)
           (values keyword-param-list keyword-arg-list &optional))
  (let ((params nil)
        (args nil))
    (dolist (entry (tc:function-ty-keyword-input-types function-type))
      (let* ((keyword (tc:keyword-ty-entry-keyword entry))
             (value-var (gensym "KEYWORD-ARG-"))
             (supplied-p-var (gensym "KEYWORD-SUPPLIED-P-"))
             (value-node (make-node-variable
                          :type (tc:keyword-ty-entry-type entry)
                          :value value-var))
             (supplied-p-node (make-node-variable
                               :type tc:*boolean-type*
                               :value supplied-p-var)))
        (push (make-keyword-param
               :keyword keyword
               :var value-var
               :supplied-p-var supplied-p-var)
              params)
        (push (make-node-application-keyword-arg
               :keyword keyword
               :value value-node
               :supplied-p supplied-p-node)
              args)))
    (values (nreverse params) (nreverse args))))

(defun bind-hidden-function-arguments (inner-node visible-type hidden-argument-nodes)
  "Bind HIDDEN-ARGUMENT-NODES onto INNER-NODE when it denotes a function value."
  (declare (type node inner-node)
           (type tc:ty visible-type)
           (type node-list hidden-argument-nodes)
           (values node))
  (if (or (null hidden-argument-nodes)
          (not (tc:function-type-p visible-type)))
      inner-node
      (let* ((function-var (gensym "FUNCTION-ENTRY-"))
             (hidden-bindings
               (loop :for hidden-node :in hidden-argument-nodes
                     :for i :from 0
                     :collect (cons (gensym (format nil "HIDDEN-~D-" i))
                                    hidden-node)))
             (bound-entry
               (make-node-lisp
                :type visible-type
                :vars (cons (cons function-var function-var)
                            (loop :for binding :in hidden-bindings
                                  :collect (cons (car binding) (car binding))))
                :form `((coalton-impl/runtime:bind-function-entry-hidden-arguments
                         ,function-var
                         ,@(mapcar #'car hidden-bindings))))))
        (loop :with inner := bound-entry
              :for (var . hidden-node) :in (reverse hidden-bindings)
              :do (setf inner
                        (make-node-bind
                         :type visible-type
                         :name var
                         :expr hidden-node
                         :body inner))
              :finally
                 (return
                   (make-node-bind
                    :type visible-type
                    :name function-var
                    :expr inner-node
                    :body inner))))))

(defun translate-keyword-params (keyword-params)
  (declare (type tc:keyword-param-list keyword-params)
           (values keyword-param-list &optional))
  (mapcar (lambda (param)
            (make-keyword-param
             :keyword (tc:keyword-param-keyword param)
             :var (tc:keyword-param-value-var param)
             :supplied-p-var (tc:keyword-param-supplied-p-var param)))
          keyword-params))

(defun translate-keyword-rands (keyword-rands ctx env)
  (declare (type tc:node-application-keyword-arg-list keyword-rands)
           (type pred-context ctx)
           (type tc:environment env)
           (values keyword-arg-list &optional))
  (mapcar
   (lambda (arg)
     (make-node-application-keyword-arg
     :keyword (tc:node-application-keyword-arg-keyword arg)
      :value (apply-dicts (tc:node-application-keyword-arg-value arg) ctx env)))
   keyword-rands))

(defun make-qualified-variable-application (expr qual-ty result-type rands keyword-rands ctx env)
  (declare (type tc:node-variable expr)
           (type tc:qualified-ty qual-ty)
           (type tc:ty result-type)
           (type node-list rands)
           (type keyword-arg-list keyword-rands)
           (type pred-context ctx)
           (type tc:environment env)
           (values node))
  (let* ((dicts
           (mapcar
            (lambda (pred)
              (resolve-dict pred ctx env))
            (tc:qualified-ty-predicates qual-ty)))
         (dict-types (mapcar #'node-type dicts)))
    (make-node-application
     :type result-type
     :properties '()
     :rator (make-node-variable
             :type (physical-callable-type
                    (prepend-codegen-hidden-input-types
                     dict-types
                     (tc:qualified-ty-type qual-ty)))
             :value (tc:node-variable-name expr))
     :rands (append dicts rands)
     :keyword-rands keyword-rands)))

(defun binding-codegen-abstraction-type (binding qual-ty preds env last-node)
  "Build the codegen function type for a translated toplevel binding.

Codegen abstractions bind explicit predicate dictionaries before the
source-level parameters, so their node type must include those extra
inputs. The typechecker stores predicates separately on QUAL-TY, so we
reconstruct the full codegen-visible function type here."
  (declare (type tc:binding-type binding)
           (type tc:qualified-ty qual-ty)
           (type tc:ty-predicate-list preds)
           (type tc:environment env)
           (type tc:node last-node)
           (values tc:ty))
  (declare (ignore last-node))
  (let ((dict-types
          (loop :for pred :in preds
                :collect (pred-type pred env)))
        (visible-type (tc:qualified-ty-type qual-ty)))
    (physical-callable-type
     (if (or (tc:binding-parameters binding)
             (tc:binding-keyword-parameters binding)
             (tc:binding-restricted-p binding))
         (prepend-codegen-hidden-input-types
          dict-types
          visible-type)
         (tc:merge-function-input-types
          dict-types
          visible-type)))))

(defun values-bind-vars (patterns)
  (declare (type tc:pattern-list patterns)
           (values list &optional))
  (loop :for subpattern :in patterns
        :collect (typecase subpattern
                   (tc:pattern-var
                    (tc:pattern-var-name subpattern))
                   (tc:pattern-wildcard
                    (gensym "_"))
                   (t
                    (util:coalton-bug "Invalid values binding subpattern ~S" subpattern)))))

(defun zero-values-node ()
  (declare (values node &optional))
  (make-node-values
   :type (tc:output-types-result-type nil)
   :nodes nil))

(defun translate-body-elements-into (body-nodes tail-node ctx env)
  "Translate BODY-NODES and sequence them in front of TAIL-NODE.

The resulting codegen node preserves the original let-style and
values-bind scoping of BODY-NODES while reusing TAIL-NODE as the final
expression."
  (declare (type tc:node-body-element-list body-nodes)
           (type node tail-node)
           (type pred-context ctx)
           (type tc:environment env)
           (values node &optional))
  (loop :with out-node := tail-node
        :for body-node :in (reverse body-nodes) :do
          (setf out-node
                (etypecase body-node
                  (tc:node-values-bind
                   (make-node-values-bind
                    :type (node-type out-node)
                    :vars (values-bind-vars (tc:node-values-bind-patterns body-node))
                    :expr (translate-expression (tc:node-values-bind-expr body-node) ctx env)
                    :body out-node))
                  (tc:node-bind
                   (let ((ty (node-type out-node))
                         (pattern (tc:node-bind-pattern body-node)))
                     (typecase pattern
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
                                     :body out-node)))))))
                  (tc:node
                   (make-node-seq
                    :type (node-type out-node)
                    :nodes (list (translate-expression body-node ctx env)
                                 out-node)))))
        :finally (return out-node)))

(defun translate-discarding-body-elements (body-nodes ctx env)
  "Translate BODY-NODES as a body whose final value is discarded.

This is used for loop bodies and similar contexts where the user forms
are sequenced for effect and the translated body should end in zero
values."
  (declare (type tc:node-body-element-list body-nodes)
           (type pred-context ctx)
           (type tc:environment env)
           (values node &optional))
  (translate-body-elements-into body-nodes (zero-values-node) ctx env))

(defun reachable-context-predicates (pred env)
  "Return PRED plus any predicates reachable from it through superclasses."
  (declare (type tc:ty-predicate pred)
           (type tc:environment env)
           (values tc:ty-predicate-list &optional))
  (labels ((walk (pred)
             (let* ((class (tc:lookup-class env (tc:ty-predicate-class pred)))
                    (subs (tc:predicate-match (tc:ty-class-predicate class) pred))
                    (supers (tc:apply-substitution subs (tc:ty-class-superclasses class))))
               (cons pred
                     (mapcan #'walk supers)))))
    (walk pred)))

(defun specialize-qual-type-to-context (qual-ty ctx env)
  "Specialize QUAL-TY to any uniquely matching predicates in CTX.

This matters for overloaded values that are captured without an
application site, such as variables passed into `lisp` forms."
  (declare (type tc:qualified-ty qual-ty)
           (type pred-context ctx)
           (type tc:environment env)
           (values tc:qualified-ty &optional))
  (let ((subs nil))
    (dolist (pred (tc:qualified-ty-predicates qual-ty))
      (let ((matches
              (remove-duplicates
               (loop :for (ctx-pred . nil) :in ctx
                     :append (loop :for reachable-pred :in (reachable-context-predicates ctx-pred env)
                                   :for match
                                     := (and (eq (tc:ty-predicate-class pred)
                                                 (tc:ty-predicate-class reachable-pred))
                                             (handler-case
                                                 (tc:predicate-match pred reachable-pred subs)
                                               (tc:predicate-unification-error ()
                                                 nil)
                                               (tc:coalton-internal-type-error ()
                                                 nil)))
                                   :when match
                                     :collect (cons (tc:apply-substitution match pred)
                                                    match)))
               :test #'tc:type-predicate=
               :key #'car)))
        (when (= 1 (length matches))
          (setf subs
                (tc:compose-substitution-lists
                 (cdar matches)
                 subs)))))
    (if subs
        (tc:apply-substitution subs qual-ty)
        qual-ty)))

(defun binding-eta-argument-types (binding qual-ty)
  "Return extra positional argument types needed to eta-expand BINDING.

Qualified value aliases like `(define lift-to lift)` must still compile to a
runtime function that accepts the source-level positional inputs after any
dictionary arguments. When the RHS is not already a lambda, translate-toplevel
needs to synthesize those trailing parameters explicitly."
  (declare (type tc:binding-type binding)
           (type tc:qualified-ty qual-ty)
           (values tc:ty-list &optional))
  (let ((type (tc:qualified-ty-type qual-ty)))
    (if (and (tc:qualified-ty-predicates qual-ty)
             (not (tc:binding-restricted-p binding))
             (null (tc:binding-parameters binding))
             (tc:function-type-p type)
             (plusp (length (physical-callable-argument-types type))))
        (physical-callable-argument-types type)
        nil)))

(defun binding-eta-keyword-forwarders (binding qual-ty)
  (declare (type tc:binding-type binding)
           (type tc:qualified-ty qual-ty)
           (values keyword-param-list keyword-arg-list &optional))
  (let ((type (tc:qualified-ty-type qual-ty)))
    (if (and (tc:qualified-ty-predicates qual-ty)
             (not (tc:binding-restricted-p binding))
             (null (tc:binding-parameters binding))
             (null (tc:binding-keyword-parameters binding))
             (tc:function-type-p type))
        (make-keyword-forwarders type)
        (values nil nil))))

(defun maybe-eta-expand-binding-body (binding qual-ty translated-body eta-vars eta-arg-types eta-keyword-rands ctx env)
  (declare (type tc:binding-type binding)
           (type tc:qualified-ty qual-ty)
           (type node translated-body)
           (type list eta-vars)
           (type tc:ty-list eta-arg-types)
           (type keyword-arg-list eta-keyword-rands)
           (type pred-context ctx)
           (type tc:environment env)
           (values node &optional))
  (let* ((binding-value (tc:binding-value binding))
         (eta-rands
           (loop :for var :in eta-vars
                 :for ty :in eta-arg-types
                 :collect (make-node-variable
                           :type ty
                           :value var))))
    (cond
      ((and (typep binding-value 'tc:node-variable)
            (or eta-rands eta-keyword-rands))
       (make-qualified-variable-application
        binding-value
        (specialize-qual-type-to-context (tc:node-type binding-value) ctx env)
        (tc:function-return-type (tc:qualified-ty-type qual-ty))
        eta-rands
        eta-keyword-rands
        ctx
        env))
      ((and (null eta-rands)
            (null eta-keyword-rands))
       translated-body)
      (t
       (make-node-application
        :type (tc:function-return-type (tc:qualified-ty-type qual-ty))
        :properties '()
        :rator translated-body
        :rands eta-rands
        :keyword-rands eta-keyword-rands)))))

(defun translate-toplevel (binding env name &key extra-context)
  (declare (type tc:binding-type binding)
           (type tc:environment env)
           (type symbol name)
           (ignorable name)
           (type pred-context extra-context))

  (let* ((qual-ty (tc:binding-type binding))

         (preds (tc:qualified-ty-predicates qual-ty))

         (ctx
           (loop :for pred :in preds
                 :collect (cons pred (gensym "DICT"))))

         (full-ctx (append ctx extra-context))

         (pattern-params nil)
         (last-node (tc:binding-last-node binding))
         (eta-arg-types (binding-eta-argument-types binding qual-ty))
         (eta-vars (loop :repeat (length eta-arg-types)
                         :collect (gensym "ARG"))))

    (multiple-value-bind (eta-keyword-params eta-keyword-rands)
        (binding-eta-keyword-forwarders binding qual-ty)

      (labels ((wrap-qualified-nullary-result (node outer-vars outer-keyword-params)
                 (if (and outer-vars
                          (null outer-keyword-params)
                          (null (tc:binding-parameters binding))
                          (not (tc:binding-restricted-p binding))
                          (tc:function-type-p (tc:qualified-ty-type qual-ty))
                          (null (physical-callable-argument-types
                                 (tc:qualified-ty-type qual-ty))))
                     (make-node-abstraction
                      :type (tc:qualified-ty-type qual-ty)
                      :vars nil
                      :keyword-params outer-keyword-params
                      :subexpr node)
                     node)))

        (cond
          ;; If the binding does not have parameters, and the
          ;; body is a single lambda then generate a function
          ;; to match the declared type and then translate the
          ;; lambda.
          ((and (null (tc:binding-parameters binding))
                (tc:binding-restricted-p binding)
                (typep last-node 'tc:node-abstraction))
           (let ((vars (append
                        (loop :for (pred . name) :in ctx
                              :collect name)
                        (loop :for param :in (tc:node-abstraction-params last-node)
                              :if (tc:pattern-var-p param)
                                :collect (tc:pattern-var-name param)
                              :else :if (tc:pattern-wildcard-p param)
                                      :collect (gensym "_")
                              :else
                                :collect (let ((name (gensym)))
                                           (push (cons name param) pattern-params)
                                           name)))))
             (make-node-abstraction
              :type (binding-codegen-abstraction-type binding qual-ty preds env last-node)
              :vars vars
              :keyword-params (translate-keyword-params
                               (tc:node-abstraction-keyword-params last-node))
              :subexpr (wrap-qualified-nullary-result
                        (wrap-with-pattern-params
                         pattern-params
                         (translate-expression
                          (tc:node-abstraction-body last-node)
                          full-ctx
                          env))
                        vars
                        (translate-keyword-params
                         (tc:node-abstraction-keyword-params last-node))))))

          ;; Function-syntax bindings and constrained bindings need an outer lambda.
          ((or (tc:binding-parameters binding) preds (tc:binding-restricted-p binding))
           (let ((vars (append
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
                                           name)))))
             (make-node-abstraction
              :type (binding-codegen-abstraction-type binding qual-ty preds env last-node)
              :vars (append vars eta-vars)
              :keyword-params (append (translate-keyword-params
                                       (tc:binding-keyword-parameters binding))
                                      eta-keyword-params)
              :subexpr (wrap-qualified-nullary-result
                        (wrap-with-pattern-params
                         pattern-params
                         (maybe-eta-expand-binding-body
                          binding
                          qual-ty
                          (translate-expression (tc:binding-value binding) full-ctx env)
                          eta-vars
                          eta-arg-types
                          eta-keyword-rands
                          full-ctx
                          env))
                        (append vars eta-vars)
                        (append (translate-keyword-params
                                 (tc:binding-keyword-parameters binding))
                                eta-keyword-params)))))

          (t
           (translate-expression (tc:binding-value binding) full-ctx env)))))))

(defun translate-variable-application (expr result-type rands keyword-rands ctx env)
  "Translate the immediate application of variable EXPR to RANDS.

Qualified variable references used as first-class values still need the
eta-expansion in APPLY-DICTS so they keep the source-visible arity.
When the variable is in operator position, however, we can apply any
resolved dictionaries directly in the same call node. This preserves the
direct-call shape needed by later tail-call and direct-application passes."
  (declare (type tc:node-variable expr)
           (type tc:ty result-type)
           (type tc:node-list rands)
           (type list keyword-rands)
           (type pred-context ctx)
           (type tc:environment env)
           (values node &optional))
  (let* ((qual-ty (specialize-qual-type-to-context
                   (tc:node-type expr)
                   ctx
                   env))
         (translated-rands
           (mapcar
            (lambda (rand)
              (apply-dicts rand ctx env))
            rands)))
    (make-qualified-variable-application
     expr
     qual-ty
     result-type
     translated-rands
     (translate-keyword-rands keyword-rands ctx env)
     ctx
     env)))

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

      (let* ((classes-package (util:find-package "COALTON/CLASSES"))
             (num-class (util:find-symbol "NUM" classes-package))
             (num-pred (tc:make-ty-predicate :class num-class
                                             :types (list (tc:qualified-ty-type qual-ty))))
             (from-int-method (util:find-symbol "FROMINT" classes-package))
             (val (tc:node-integer-literal-value expr))
             (ty (tc:qualified-ty-type qual-ty)))
        (flet ((make-full-call ()
                 (make-node-application
                  :type (tc:qualified-ty-type qual-ty)
                  :properties '()
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
                (coalton:IFix
                 (make-node-literal :type tc:*ifix-type*
                                    :value val))
                (coalton:UFix
                 (make-node-literal :type tc:*ufix-type*
                                    :value val))
                (coalton:F32
                 (make-node-literal :type tc:*single-float-type*
                                    :value (coerce val 'single-float)))
                (coalton:F64
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
               :value (util:find-symbol "ID" "COALTON/FUNCTIONS"))

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

      (let ((rator-expr (tc:node-application-rator expr)))
        (typecase rator-expr
          (tc:node-variable
           (if (util:dynamic-variable-name-p (tc:node-variable-name rator-expr))
               (make-node-application
                :type (tc:qualified-ty-type qual-ty)
                :properties '()
                :rator (translate-expression rator-expr ctx env)
                :rands (mapcar
                        (lambda (rand)
                          (apply-dicts rand ctx env))
                        (tc:node-application-rands expr))
                :keyword-rands (translate-keyword-rands
                                (tc:node-application-keyword-rands expr)
                                ctx
                                env))
               (translate-variable-application
                rator-expr
                (tc:qualified-ty-type qual-ty)
                (tc:node-application-rands expr)
                (tc:node-application-keyword-rands expr)
                ctx
                env)))
          (t
           (make-node-application
            :type (tc:qualified-ty-type qual-ty)
            :properties '()
            :rator (translate-expression rator-expr ctx env)
            :rands (mapcar
                    (lambda (rand)
                      (apply-dicts rand ctx env))
                    (tc:node-application-rands expr))
            :keyword-rands (translate-keyword-rands
                            (tc:node-application-keyword-rands expr)
                            ctx
                            env)))))))

  (:method ((expr tc:node-body) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (translate-body-elements-into
     (tc:node-body-nodes expr)
     (translate-expression (tc:node-body-last-node expr) ctx env)
     ctx
     env))

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

      (let* ((visible-type (tc:qualified-ty-type qual-ty))
             (translated-keyword-params
               (mapcar (lambda (param)
                         (make-keyword-param
                          :keyword (tc:keyword-param-keyword param)
                          :var (tc:keyword-param-value-var param)
                          :supplied-p-var (tc:keyword-param-supplied-p-var param)))
                       (tc:node-abstraction-keyword-params expr)))
             (abstraction-type
               (physical-callable-type
                (tc:merge-function-input-types
                 dict-types
                 visible-type))))
        (let ((inner (translate-expression (tc:node-abstraction-body expr) ctx env)))
          (loop :for (name . pattern) :in (reverse pattern-params)
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
                            :body inner)))))
          (when (and dict-var-names
                     (tc:function-type-p visible-type)
                     (null (tc:function-ty-positional-input-types visible-type))
                     (null (tc:function-ty-keyword-input-types visible-type))
                     (null translated-keyword-params))
            (setf inner
                  (make-node-abstraction
                   :type visible-type
                   :vars nil
                   :keyword-params nil
                   :subexpr inner)))
          (make-node-abstraction
           :type abstraction-type
           :vars vars
           :keyword-params translated-keyword-params
           :subexpr inner)))))

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

                       :collect
                         (cons name
                               (translate-toplevel
                                binding
                                env
                                name
                                :extra-context ctx)))
       :subexpr (translate-expression (tc:node-let-body expr) ctx env))))

  (:method ((expr tc:node-dynamic-let) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-dynamic-let
       :type (tc:qualified-ty-type qual-ty)
       :bindings (loop :for binding :in (tc:node-dynamic-let-bindings expr)
                       :collect (make-node-dynamic-binding
                                 :name (tc:node-variable-name (tc:node-dynamic-binding-name binding))
                                 :value (translate-expression
                                         (tc:node-dynamic-binding-value binding)
                                         ctx
                                         env)))
       :subexpr (translate-expression (tc:node-dynamic-let-subexpr expr) ctx env))))

  (:method ((expr tc:node-lisp) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))
      (let ((bindings nil)
            (lisp-vars nil))
        (loop :for var :in (tc:node-lisp-vars expr)
              :for var-name :in (tc:node-lisp-var-names expr)
              :do (let ((translated-var (apply-dicts var ctx env)))
                    (typecase translated-var
                      (node-variable
                        (push (cons var-name (node-variable-value translated-var))
                              lisp-vars))
                      (t
                        (let ((temp-name (gensym (format nil "~A-" var-name))))
                          (push (cons temp-name translated-var) bindings)
                          (push (cons var-name temp-name) lisp-vars))))))
        (let ((node-lisp
                (make-node-lisp
                 :type (tc:qualified-ty-type qual-ty)
                 :vars (nreverse lisp-vars)
                 :form (tc:node-lisp-body expr))))
          (if bindings
              (make-node-let
               :type (tc:qualified-ty-type qual-ty)
               :bindings (nreverse bindings)
               :subexpr node-lisp)
              node-lisp)))))

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

  (:method ((expr tc:node-catch) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-catch
       :type (tc:qualified-ty-type qual-ty)
       :expr (translate-expression (tc:node-catch-expr expr) ctx env)
       :branches (mapcar
                  (lambda (branch)
                    (make-catch-branch
                     :pattern (translate-pattern (tc:node-catch-branch-pattern branch))
                     :body (translate-expression (tc:node-catch-branch-body branch) ctx env)))
                  (tc:node-catch-branches expr)))))

  (:method ((expr tc:node-resumable) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-resumable
       :type (tc:qualified-ty-type qual-ty)
       :expr (translate-expression (tc:node-resumable-expr expr) ctx env)
       :branches (mapcar
                  (lambda (branch)
                    (make-resumable-branch
                     :pattern (translate-pattern (tc:node-resumable-branch-pattern branch))
                     :body (translate-expression (tc:node-resumable-branch-body branch) ctx env)))
                  (tc:node-resumable-branches expr)))))

  (:method ((expr tc:node-progn) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-seq
       :type (tc:qualified-ty-type qual-ty)
       :nodes (list (translate-expression (tc:node-progn-body expr) ctx env)))))

  (:method ((expr tc:node-unsafe) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-locally
       :type (tc:qualified-ty-type qual-ty)
       :noinline-functions nil
       :type-check 0
       :subexpr (translate-expression (tc:node-unsafe-body expr) ctx env))))

  (:method ((expr tc:node-block) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (let ((body (translate-expression (tc:node-block-body expr) ctx env)))
        (if (block-return-target-used-p body (tc:node-block-name expr))
            (make-node-block
             :type (tc:qualified-ty-type qual-ty)
             :name (tc:node-block-name expr)
             :body body)
            body))))

  (:method ((expr tc:node-return-from) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-return-from
       :type (tc:qualified-ty-type qual-ty)
       :name (tc:node-return-from-name expr)
       :expr (translate-expression (tc:node-return-from-expr expr) ctx env))))

  (:method ((expr tc:node-values) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))
      (make-node-values
       :type (tc:qualified-ty-type qual-ty)
       :nodes (mapcar (lambda (subexpr)
                        (translate-expression subexpr ctx env))
                      (tc:node-values-nodes expr)))))

  (:method ((expr tc:node-throw) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-throw
       :type (tc:qualified-ty-type qual-ty)
       :expr (translate-expression (tc:node-throw-expr expr) ctx env))))

  (:method ((expr tc:node-resume-to) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let ((qual-ty (tc:node-type expr)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-node-resume-to
       :type (tc:qualified-ty-type qual-ty)
       :expr (translate-expression (tc:node-resume-to-expr expr) ctx env))))


  (:method ((expr tc:node-or) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((coalton-package (util:find-package "COALTON"))
           (true-value (util:find-symbol "TRUE" coalton-package))
           (false-value (util:find-symbol "FALSE" coalton-package))
           (rev-children (reverse (tc:node-or-nodes expr))))

      (if (null rev-children)
          (make-node-variable
           :type tc:*boolean-type*
           :value false-value)
          (loop :with out-node := (translate-expression (car rev-children)
                                                        ctx env)
                :for body-node :in (cdr rev-children) :do
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
                :finally (return out-node)))))

  (:method ((expr tc:node-and) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((coalton-package (util:find-package "COALTON"))
           (true-value (util:find-symbol "TRUE" coalton-package))
           (false-value (util:find-symbol "FALSE" coalton-package))
           (rev-children (reverse (tc:node-and-nodes expr))))

      (if (null rev-children)
          (make-node-variable
           :type tc:*boolean-type*
           :value true-value)
          (loop :with out-node := (translate-expression (car rev-children)
                                                        ctx env)
                :for body-node :in (cdr rev-children) :do
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
                :finally (return out-node)))))

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

    (let* ((qual-ty (tc:node-type expr))
           (result-ty (tc:qualified-ty-type qual-ty))
           (coalton-package (util:find-package "COALTON"))
           (true-value (util:find-symbol "TRUE" coalton-package))
           (false-value (util:find-symbol "FALSE" coalton-package)))

      (make-node-match
       :type result-ty
       :expr (translate-expression (tc:node-when-expr expr) ctx env)
       :branches (list
                  (make-match-branch
                   :pattern (make-pattern-constructor
                             :type tc:*boolean-type*
                             :name true-value
                             :patterns nil)
                   :body (make-node-seq
                          :type result-ty
                          :nodes (list (translate-expression (tc:node-when-body expr) ctx env)
                                       (zero-values-node))))
                  (make-match-branch
                   :pattern (make-pattern-constructor
                             :type tc:*boolean-type*
                             :name false-value
                             :patterns nil)
                   :body (zero-values-node))))))

  (:method ((expr tc:node-unless) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((qual-ty (tc:node-type expr))
           (result-ty (tc:qualified-ty-type qual-ty))
           (coalton-package (util:find-package "COALTON"))
           (true-value (util:find-symbol "TRUE" coalton-package))
           (false-value (util:find-symbol "FALSE" coalton-package)))

      (make-node-match
       :type result-ty
       :expr (translate-expression (tc:node-unless-expr expr) ctx env)
       :branches (list
                  (make-match-branch
                   :pattern (make-pattern-constructor
                             :type tc:*boolean-type*
                             :name true-value
                             :patterns nil)
                   :body (zero-values-node))
                  (make-match-branch
                   :pattern (make-pattern-constructor
                             :type tc:*boolean-type*
                             :name false-value
                             :patterns nil)
                   :body (make-node-seq
                          :type result-ty
                          :nodes (list (translate-expression (tc:node-unless-body expr) ctx env)
                                       (zero-values-node))))))))

  (:method ((expr tc:node-for) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (make-node-for
     :type (tc:qualified-ty-type (tc:node-type expr))
     :label (tc:node-for-label expr)
     :bindings
     (loop :for binding :in (tc:node-for-bindings expr)
           :collect (make-node-for-binding
                     :name (tc:node-variable-name (tc:node-for-binding-name binding))
                     :type (tc:qualified-ty-type (tc:node-type (tc:node-for-binding-name binding)))
                     :init (translate-expression (tc:node-for-binding-init binding) ctx env)
                     :step (and (tc:node-for-binding-step binding)
                                (translate-expression (tc:node-for-binding-step binding) ctx env))))
     :sequential-p (tc:node-for-sequential-p expr)
     :returns (and (tc:node-for-returns expr)
                   (translate-expression (tc:node-for-returns expr) ctx env))
     :termination-kind (tc:node-for-termination-kind expr)
     :termination-expr (and (tc:node-for-termination-expr expr)
                            (translate-expression (tc:node-for-termination-expr expr) ctx env))
     :body (translate-expression (tc:node-for-body expr) ctx env)))

  (:method ((expr tc:node-break) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (make-node-break
     :type (tc:qualified-ty-type (tc:node-type expr))
     :label (tc:node-break-label expr)))

  (:method ((expr tc:node-continue) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))
    (make-node-continue
     :type (tc:qualified-ty-type (tc:node-type expr))
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

      (flet ((catchall-clause-p (clause)
               "Is `clause` the catchall clause, 'coalton:True?"
               (let ((expr (tc:node-cond-clause-expr clause)))
                 (and (typep expr 'tc:node-variable)
                      (and (tc:ty= tc:*boolean-type*  (tc:qualified-ty-type (tc:node-type expr)))
                           (eq 'coalton:True (tc:node-variable-name expr)))))))
        (let* ((reverse-clauses (reverse (tc:node-cond-clauses expr)))
               (has-catchall-clause-p (some #'catchall-clause-p reverse-clauses)))
          (when has-catchall-clause-p
            ;; remove all the clauses after the catchall clause.
            (loop :while (or (not (catchall-clause-p (first reverse-clauses)))
                             ;; make sure to find the very first catchall clause
                             (some #'catchall-clause-p (rest reverse-clauses)))
                  :do (pop reverse-clauses)))

          (loop :with out-node
                  := (cond
                       (has-catchall-clause-p
                        (translate-expression (tc:node-cond-clause-body (first reverse-clauses)) ctx env))
                       (t
                        (make-node-lisp
                         :type (tc:qualified-ty-type qual-ty)
                         :vars nil
                         :form '((cl:error "Non-exhaustive COND.")))))
                :for clause :in (if has-catchall-clause-p (rest reverse-clauses) reverse-clauses) :do
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
                :finally (return out-node))))))

  (:method ((node tc:node-do) ctx env)
    (declare (type pred-context ctx)
             (type tc:environment env)
             (values node))

    (let* ((classes-package (util:find-package "COALTON/CLASSES"))

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

                        (tc:node-values-bind
                         (make-node-values-bind
                          :type (node-type out-node)
                          :vars (values-bind-vars (tc:node-values-bind-patterns elem))
                          :expr (translate-expression (tc:node-values-bind-expr elem) ctx env)
                          :body out-node))


                        ;; *sad burrito noises*
                        (tc:node-do-bind
                         (let* ((var-type (tc:qualified-ty-type (tc:pattern-type (tc:node-do-bind-pattern elem))))

                                (callback-ty (tc:make-function-type var-type (node-type out-node)))

                                (var-name (gensym)))

                           (make-node-application
                            :type (node-type out-node)
                            :properties '()
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
                            :properties '()
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

  (:method ((pat tc:pattern-binding))
    (let ((qual-ty (tc:pattern-type pat)))
      (assert (null (tc:qualified-ty-predicates qual-ty)))

      (make-pattern-binding
       :type (tc:qualified-ty-type qual-ty)
       :var (translate-pattern (tc:pattern-binding-var pat))
       :pattern (translate-pattern (tc:pattern-binding-pattern pat)))))

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
  (let* ((qual-ty (specialize-qual-type-to-context (tc:node-type expr) ctx env))

         (dicts (mapcar
                 (lambda (pred)
                   (resolve-dict pred ctx env))
                 (tc:qualified-ty-predicates qual-ty)))

         (dict-types (mapcar #'node-type dicts))

         (var-type (prepend-codegen-hidden-input-types
                    dict-types
                    (physical-callable-type (tc:qualified-ty-type qual-ty))))

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
       (if (and (typep expr 'tc:node-variable)
                (not (util:dynamic-variable-name-p (tc:node-variable-name expr)))
                (not (tc:function-type-p (tc:qualified-ty-type qual-ty))))
           (alexandria:if-let ((entry (tc:lookup-function env (tc:node-variable-name expr) :no-error t)))
             (if (zerop (tc:function-env-entry-arity entry))
                 (make-node-application
                  :type (tc:qualified-ty-type qual-ty)
                  :properties '()
                  :rator (make-node-variable
                          :type (tc:make-function-type* nil (tc:qualified-ty-type qual-ty))
                          :value (tc:node-variable-name expr))
                  :rands nil
                  :keyword-rands nil)
                 inner-node)
             inner-node)
           inner-node))
      ((tc:function-type-p (tc:qualified-ty-type qual-ty))
       (bind-hidden-function-arguments
        inner-node
        (physical-callable-type (tc:qualified-ty-type qual-ty))
        dicts))
	      (t
	       (make-node-application
	        :type (tc:qualified-ty-type qual-ty)
	        :properties '()
        :rator inner-node
        :rands dicts
        :keyword-rands nil)))))

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

(defun block-return-target-used-p (inner target)
  "Return true when INNER contains a return targeting TARGET outside a matching block."
  (declare (type node inner)
           (type symbol target)
           (values boolean &optional))
  (traverse
   inner
   (list
    (action (:traverse node-block node)
      (unless (eq target (node-block-name node))
        (funcall *traverse* (node-block-body node)))
      (values))
    (action (:before node-return-from node)
      (when (eq target (node-return-from-name node))
        (return-from block-return-target-used-p t)))))
  nil)
