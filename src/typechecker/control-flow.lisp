;;;; Convert `return` forms to explicit, labeled "return-from" nodes.

(defpackage #:coalton-impl/typechecker/control-flow
  (:use
   #:cl
   #:coalton-impl/typechecker/base)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util))
  (:export
   #:resolve-control-flow))

(in-package #:coalton-impl/typechecker/control-flow)

(defstruct (control-context
            (:constructor make-control-context (&key return-target return-status)))
  "Lexical context for resolving surface RETURN forms."
  (return-target nil        :type (or null symbol)             :read-only t)
  ;; RETURN-STATUS controls whether RETURN is allowed in the current
  ;; context: :TOPLEVEL rejects it, :LAMBDA resolves it to
  ;; RETURN-TARGET, and :DO rejects it inside a do-bound expression.
  (return-status :toplevel  :type (member :toplevel :lambda :do) :read-only t))

(defun binding-introduces-return-scope-p (binding)
  (etypecase binding
    (parser:toplevel-define
     (or (parser:toplevel-define-function-syntax-p binding)
         (parser:toplevel-define-params binding)
         (parser:toplevel-define-keyword-params binding)))
    (parser:instance-method-definition
     (or (parser:instance-method-definition-function-syntax-p binding)
         (parser:instance-method-definition-params binding)
         (parser:instance-method-definition-keyword-params binding)))))

(defun wrap-body-in-return-block (body label location)
  "Wrap BODY in an outer NODE-BODY whose last node is a named NODE-BLOCK.

The outer NODE-BODY is required because bindings and lambdas contain
NODE-BODY values, not arbitrary nodes."
  (declare (type parser:node-body body)
           (type symbol label)
           (type source:location location)
           (values parser:node-body &optional))
  (parser:make-node-body
   :nodes nil
   :last-node (parser:make-node-block
               :name label
               :body body
               :location location)))

(defun resolve-control-flow (form)
  "Rewrite surface RETURN forms in FORM to explicit RETURN-FROM targets.

FORM must be a renamed parser AST fragment."
  (labels
      ((resolve-keyword-param (param ctx)
         (declare (type parser:keyword-param param)
                  (type control-context ctx)
                  (values parser:keyword-param &optional))
         (parser:make-keyword-param
          :keyword (parser:keyword-param-keyword param)
          :binder (parser:keyword-param-binder param)
          :default (resolve-node (parser:keyword-param-default param) ctx)
          :location (source:location param)))

       (resolve-keyword-params (params ctx)
         (declare (type parser:keyword-param-list params)
                  (type control-context ctx)
                  (values parser:keyword-param-list &optional))
         (mapcar (lambda (param)
                   (resolve-keyword-param param ctx))
                 params))

       (resolve-body-element (elem ctx)
         (declare (type parser:node-body-element elem)
                  (type control-context ctx)
                  (values parser:node-body-element &optional))
         (typecase elem
           (parser:node-bind
            (parser:make-node-bind
             :pattern (parser:node-bind-pattern elem)
             :expr (resolve-node (parser:node-bind-expr elem) ctx)
             :location (source:location elem)))
           (parser:node-values-bind
            (parser:make-node-values-bind
             :patterns (parser:node-values-bind-patterns elem)
             :expr (resolve-node (parser:node-values-bind-expr elem) ctx)
             :location (source:location elem)))
           (parser:node
            (resolve-node elem ctx))))

       (resolve-body (body ctx)
         (declare (type parser:node-body body)
                  (type control-context ctx)
                  (values parser:node-body &optional))
         (parser:make-node-body
          :nodes (mapcar (lambda (elem)
                           (resolve-body-element elem ctx))
                         (parser:node-body-nodes body))
          :last-node (resolve-node (parser:node-body-last-node body) ctx)))

       (resolve-association-entry (entry ctx)
         (declare (type parser:association-entry entry)
                  (type control-context ctx)
                  (values parser:association-entry &optional))
         (parser:make-association-entry
          :key (resolve-node (parser:association-entry-key entry) ctx)
          :value (resolve-node (parser:association-entry-value entry) ctx)
          :location (source:location entry)))

       (resolve-builder-clause (clause ctx)
         (declare (type parser:builder-clause clause)
                  (type control-context ctx)
                  (values parser:builder-clause &optional))
         (etypecase clause
           (parser:builder-with-clause
            (parser:make-builder-with-clause
             :binder (parser:builder-with-clause-binder clause)
             :expr (resolve-node (parser:builder-with-clause-expr clause) ctx)
             :location (source:location clause)))
           (parser:builder-for-clause
            (parser:make-builder-for-clause
             :binder (parser:builder-for-clause-binder clause)
             :expr (resolve-node (parser:builder-for-clause-expr clause) ctx)
             :location (source:location clause)))
           (parser:builder-below-clause
            (parser:make-builder-below-clause
             :binder (parser:builder-below-clause-binder clause)
             :expr (resolve-node (parser:builder-below-clause-expr clause) ctx)
             :location (source:location clause)))
           (parser:builder-when-clause
            (parser:make-builder-when-clause
             :expr (resolve-node (parser:builder-when-clause-expr clause) ctx)
             :location (source:location clause)))))

       (resolve-binding (binding ctx)
         (declare (type (or parser:toplevel-define parser:instance-method-definition) binding)
                  (type control-context ctx))
         (if (binding-introduces-return-scope-p binding)
             (let* ((label (gensym "RETURN-BLOCK-"))
                    (scope-ctx (make-control-context :return-target label
                                                     :return-status :lambda))
                    (resolved-keyword-params
                      (resolve-keyword-params
                       (parser:binding-keyword-parameters binding)
                       scope-ctx))
                    (resolved-body
                      (wrap-body-in-return-block
                       (resolve-body (parser:binding-value binding) scope-ctx)
                       label
                       (source:location binding))))
               (etypecase binding
                 (parser:toplevel-define
                  (parser:make-toplevel-define
                   :name (parser:toplevel-define-name binding)
                   :params (parser:toplevel-define-params binding)
                   :keyword-params resolved-keyword-params
                   :function-syntax-p (parser:toplevel-define-function-syntax-p binding)
                   :orig-params (parser:toplevel-define-orig-params binding)
                   :orig-keyword-params (parser:toplevel-define-orig-keyword-params binding)
                   :docstring (source:docstring binding)
                   :body resolved-body
                   :location (source:location binding)
                   :monomorphize (parser:toplevel-define-monomorphize binding)
                   :inline (parser:toplevel-define-inline binding)))
                 (parser:instance-method-definition
                  (parser:make-instance-method-definition
                   :name (parser:instance-method-definition-name binding)
                   :params (parser:instance-method-definition-params binding)
                   :keyword-params resolved-keyword-params
                   :function-syntax-p (parser:instance-method-definition-function-syntax-p binding)
                   :body resolved-body
                   :location (source:location binding)
                   :inline (parser:instance-method-definition-inline binding)))))
             (etypecase binding
               (parser:toplevel-define
                (parser:make-toplevel-define
                 :name (parser:toplevel-define-name binding)
                 :params (parser:toplevel-define-params binding)
                 :keyword-params
                 (resolve-keyword-params (parser:toplevel-define-keyword-params binding) ctx)
                 :function-syntax-p (parser:toplevel-define-function-syntax-p binding)
                 :orig-params (parser:toplevel-define-orig-params binding)
                 :orig-keyword-params (parser:toplevel-define-orig-keyword-params binding)
                 :docstring (source:docstring binding)
                 :body (resolve-body (parser:toplevel-define-body binding) ctx)
                 :location (source:location binding)
                 :monomorphize (parser:toplevel-define-monomorphize binding)
                 :inline (parser:toplevel-define-inline binding)))
               (parser:instance-method-definition
                (parser:make-instance-method-definition
                 :name (parser:instance-method-definition-name binding)
                 :params (parser:instance-method-definition-params binding)
                 :keyword-params
                 (resolve-keyword-params (parser:instance-method-definition-keyword-params binding) ctx)
                 :function-syntax-p (parser:instance-method-definition-function-syntax-p binding)
                 :body (resolve-body (parser:instance-method-definition-body binding) ctx)
                 :location (source:location binding)
                 :inline (parser:instance-method-definition-inline binding))))))

       (resolve-node (node ctx)
         (declare (type (or parser:node parser:node-do-bind) node)
                  (type control-context ctx)
                  (values t &optional))
         (typecase node
           ((or parser:node-variable
                parser:node-accessor
                parser:node-literal
                parser:node-integer-literal)
            node)

           (parser:node-type-of
            (parser:make-node-type-of
             :expr (resolve-node (parser:node-type-of-expr node) ctx)
             :location (source:location node)))

           (parser:node-abstraction
            (if (parser:node-abstraction-introduces-return-scope-p node)
                (let* ((label (gensym "RETURN-BLOCK-"))
                       (scope-ctx (make-control-context :return-target label
                                                        :return-status :lambda)))
                  (parser:make-node-abstraction
                   :params (parser:node-abstraction-params node)
                   :keyword-params
                   (resolve-keyword-params (parser:node-abstraction-keyword-params node) scope-ctx)
                   :body (wrap-body-in-return-block
                          (resolve-body (parser:node-abstraction-body node) scope-ctx)
                          label
                          (source:location node))
                   :location (source:location node)))
                (parser:make-node-abstraction
                 :params (parser:node-abstraction-params node)
                 :keyword-params
                 (resolve-keyword-params (parser:node-abstraction-keyword-params node) ctx)
                 :body (resolve-body (parser:node-abstraction-body node) ctx)
                 :introduces-return-scope-p nil
                 :location (source:location node))))

           (parser:node-let
            (parser:make-node-let
             :bindings (mapcar (lambda (binding)
                                 (parser:make-node-let-binding
                                  :name (parser:node-let-binding-name binding)
                                  :value (resolve-node (parser:node-let-binding-value binding) ctx)
                                  :location (source:location binding)))
                               (parser:node-let-bindings node))
             :declares (parser:node-let-declares node)
             :body (resolve-body (parser:node-let-body node) ctx)
             :location (source:location node)
             :sequential-p (parser:node-let-sequential-p node)))

           (parser:node-rec
            (parser:make-node-rec
             :name (parser:node-rec-name node)
             :bindings (mapcar (lambda (binding)
                                 (parser:make-node-let-binding
                                  :name (parser:node-let-binding-name binding)
                                  :value (resolve-node (parser:node-let-binding-value binding) ctx)
                                  :location (source:location binding)))
                               (parser:node-rec-bindings node))
             :declares (parser:node-rec-declares node)
             :params (parser:node-rec-params node)
             :call-args (parser:node-rec-call-args node)
             :body (resolve-body (parser:node-rec-body node) ctx)
             :location (source:location node)))

           (parser:node-dynamic-let
            (parser:make-node-dynamic-let
             :bindings (mapcar (lambda (binding)
                                 (parser:make-node-dynamic-binding
                                  :name (parser:node-dynamic-binding-name binding)
                                  :value (resolve-node (parser:node-dynamic-binding-value binding) ctx)
                                  :location (source:location binding)))
                               (parser:node-dynamic-let-bindings node))
             :subexpr (resolve-node (parser:node-dynamic-let-subexpr node) ctx)
             :location (source:location node)))

           (parser:node-lisp
            (parser:make-node-lisp
             :output-types (parser:node-lisp-output-types node)
             :vars (parser:node-lisp-vars node)
             :var-names (parser:node-lisp-var-names node)
             :body (parser:node-lisp-body node)
             :location (source:location node)))

           (parser:node-match
            (parser:make-node-match
             :expr (resolve-node (parser:node-match-expr node) ctx)
             :branches
             (mapcar (lambda (branch)
                       (parser:make-node-match-branch
                        :pattern (parser:node-match-branch-pattern branch)
                        :body (resolve-body (parser:node-match-branch-body branch) ctx)
                        :location (source:location branch)))
                     (parser:node-match-branches node))
             :location (source:location node)))

           (parser:node-catch
            (parser:make-node-catch
             :expr (resolve-node (parser:node-catch-expr node) ctx)
             :branches
             (mapcar (lambda (branch)
                       (parser:make-node-catch-branch
                        :pattern (parser:node-catch-branch-pattern branch)
                        :body (resolve-body (parser:node-catch-branch-body branch) ctx)
                        :location (source:location branch)))
                     (parser:node-catch-branches node))
             :location (source:location node)))

           (parser:node-resumable
            (parser:make-node-resumable
             :expr (resolve-node (parser:node-resumable-expr node) ctx)
             :branches
             (mapcar (lambda (branch)
                       (parser:make-node-resumable-branch
                        :pattern (parser:node-resumable-branch-pattern branch)
                        :body (resolve-body (parser:node-resumable-branch-body branch) ctx)
                        :location (source:location branch)))
                     (parser:node-resumable-branches node))
             :location (source:location node)))

           (parser:node-progn
            (parser:make-node-progn
             :body (resolve-body (parser:node-progn-body node) ctx)
             :location (source:location node)))

           (parser:node-unsafe
            (parser:make-node-unsafe
             :body (resolve-body (parser:node-unsafe-body node) ctx)
             :location (source:location node)))

           (parser:node-the
            (parser:make-node-the
             :type (parser:node-the-type node)
             :expr (resolve-node (parser:node-the-expr node) ctx)
             :location (source:location node)))

           (parser:node-collection-builder
            (parser:make-node-collection-builder
             :elements (mapcar (lambda (elem)
                                 (resolve-node elem ctx))
                               (parser:node-collection-builder-elements node))
             :location (source:location node)))

           (parser:node-association-builder
            (parser:make-node-association-builder
             :entries (mapcar (lambda (entry)
                                (resolve-association-entry entry ctx))
                              (parser:node-association-builder-entries node))
             :location (source:location node)))

           (parser:node-collection-comprehension
            (parser:make-node-collection-comprehension
             :head (resolve-node (parser:node-collection-comprehension-head node) ctx)
             :clauses (mapcar (lambda (clause)
                                (resolve-builder-clause clause ctx))
                              (parser:node-collection-comprehension-clauses node))
             :location (source:location node)))

           (parser:node-association-comprehension
            (parser:make-node-association-comprehension
             :key (resolve-node (parser:node-association-comprehension-key node) ctx)
             :value (resolve-node (parser:node-association-comprehension-value node) ctx)
             :clauses (mapcar (lambda (clause)
                                (resolve-builder-clause clause ctx))
                              (parser:node-association-comprehension-clauses node))
             :location (source:location node)))

           (parser:node-block
            (parser:make-node-block
             :name (parser:node-block-name node)
             :body (resolve-body (parser:node-block-body node) ctx)
             :location (source:location node)))

           (parser:node-return
            (case (control-context-return-status ctx)
              (:toplevel
               (tc-error "Unexpected return"
                         (tc-note node "returns must be inside a lambda")))
              (:do
               (tc-error "Invalid return"
                         (tc-note node "returns cannot be in a do expression"))))
            (unless (control-context-return-target ctx)
              (util:coalton-bug "Resolved return without a target"))
            (parser:make-node-return-from
             :name (control-context-return-target ctx)
             :expr (resolve-node (or (parser:node-return-expr node)
                                     (parser:make-node-values
                                      :nodes nil
                                      :location (source:location node)))
                                 ctx)
             :location (source:location node)))

           (parser:node-return-from
            (parser:make-node-return-from
             :name (parser:node-return-from-name node)
             :expr (resolve-node (parser:node-return-from-expr node) ctx)
             :location (source:location node)))

           (parser:node-values
            (parser:make-node-values
             :nodes (mapcar (lambda (subnode)
                              (resolve-node subnode ctx))
                            (parser:node-values-nodes node))
             :location (source:location node)))

           (parser:node-throw
            (parser:make-node-throw
             :expr (and (parser:node-throw-expr node)
                        (resolve-node (parser:node-throw-expr node) ctx))
             :location (source:location node)))

           (parser:node-resume-to
            (parser:make-node-resume-to
             :expr (and (parser:node-resume-to-expr node)
                        (resolve-node (parser:node-resume-to-expr node) ctx))
             :location (source:location node)))

           (parser:node-application
            (parser:make-node-application
             :rator (resolve-node (parser:node-application-rator node) ctx)
             :rands (mapcar (lambda (rand)
                              (resolve-node rand ctx))
                            (parser:node-application-rands node))
             :keyword-rands
             (mapcar (lambda (arg)
                       (parser:make-node-application-keyword-arg
                        :keyword (parser:node-application-keyword-arg-keyword arg)
                        :value (resolve-node (parser:node-application-keyword-arg-value arg) ctx)
                        :location (source:location arg)))
                     (parser:node-application-keyword-rands node))
             :location (source:location node)))

           (parser:node-or
            (parser:make-node-or
             :nodes (mapcar (lambda (subnode)
                              (resolve-node subnode ctx))
                            (parser:node-or-nodes node))
             :location (source:location node)))

           (parser:node-and
            (parser:make-node-and
             :nodes (mapcar (lambda (subnode)
                              (resolve-node subnode ctx))
                            (parser:node-and-nodes node))
             :location (source:location node)))

           (parser:node-if
            (parser:make-node-if
             :expr (resolve-node (parser:node-if-expr node) ctx)
             :then (resolve-node (parser:node-if-then node) ctx)
             :else (resolve-node (parser:node-if-else node) ctx)
             :location (source:location node)))

           (parser:node-when
            (parser:make-node-when
             :expr (resolve-node (parser:node-when-expr node) ctx)
             :body (resolve-body (parser:node-when-body node) ctx)
             :location (source:location node)))

           (parser:node-unless
            (parser:make-node-unless
             :expr (resolve-node (parser:node-unless-expr node) ctx)
             :body (resolve-body (parser:node-unless-body node) ctx)
             :location (source:location node)))

           (parser:node-cond
            (parser:make-node-cond
             :clauses (mapcar (lambda (clause)
                                (parser:make-node-cond-clause
                                 :expr (resolve-node (parser:node-cond-clause-expr clause) ctx)
                                 :body (resolve-body (parser:node-cond-clause-body clause) ctx)
                                 :location (source:location clause)))
                              (parser:node-cond-clauses node))
             :location (source:location node)))

           (parser:node-for
            (parser:make-node-for
             :label (parser:node-for-label node)
             :bindings
             (mapcar (lambda (binding)
                       (parser:make-node-for-binding
                        :name (parser:node-for-binding-name binding)
                        :init (resolve-node (parser:node-for-binding-init binding) ctx)
                        :step (and (parser:node-for-binding-step binding)
                                   (resolve-node (parser:node-for-binding-step binding) ctx))
                        :location (source:location binding)))
                     (parser:node-for-bindings node))
             :declares (parser:node-for-declares node)
             :returns (and (parser:node-for-returns node)
                           (resolve-node (parser:node-for-returns node) ctx))
             :termination-kind (parser:node-for-termination-kind node)
             :termination-expr (and (parser:node-for-termination-expr node)
                                    (resolve-node (parser:node-for-termination-expr node) ctx))
             :body (resolve-body (parser:node-for-body node) ctx)
             :location (source:location node)
             :sequential-p (parser:node-for-sequential-p node)))

           ((or parser:node-break parser:node-continue)
            node)

           (parser:node-do-bind
            (parser:make-node-do-bind
             :pattern (parser:node-do-bind-pattern node)
             :expr (resolve-node (parser:node-do-bind-expr node)
                                 (make-control-context
                                  :return-target (control-context-return-target ctx)
                                  :return-status :do))
             :location (source:location node)))

           (parser:node-do
            (parser:make-node-do
             :nodes (mapcar (lambda (elem)
                              (etypecase elem
                                (parser:node
                                 (resolve-node elem ctx))
                                (parser:node-bind
                                 (resolve-body-element elem ctx))
                                (parser:node-values-bind
                                 (resolve-body-element elem ctx))
                                (parser:node-do-bind
                                 (resolve-node elem ctx))))
                            (parser:node-do-nodes node))
             :last-node (resolve-node (parser:node-do-last-node node) ctx)
             :location (source:location node)))))

       (resolve-top-level (form ctx)
         (declare (type control-context ctx))
         (typecase form
           ((or parser:toplevel-define parser:instance-method-definition)
            (resolve-binding form ctx))
           (parser:toplevel-define-instance
            (parser:make-toplevel-define-instance
             :context (parser:toplevel-define-instance-context form)
             :pred (parser:toplevel-define-instance-pred form)
             :methods (mapcar (lambda (method)
                                (resolve-binding method ctx))
                              (parser:toplevel-define-instance-methods form))
             :docstring (source:docstring form)
             :head-location (parser:toplevel-define-instance-head-location form)
             :compiler-generated (parser:toplevel-define-instance-compiler-generated form)
             :location (source:location form)))
           (parser:node
            (resolve-node form ctx))
           (list
            (mapcar (lambda (elem)
                      (resolve-top-level elem ctx))
                    form))
           (t
            form))))

    (resolve-top-level form (make-control-context :return-target nil
                                                  :return-status :toplevel))))
