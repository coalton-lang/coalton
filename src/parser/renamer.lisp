(defpackage #:coalton-impl/parser/renamer
  (:use
   #:cl
   #:coalton-impl/parser/base
   #:coalton-impl/parser/types
   #:coalton-impl/parser/pattern
   #:coalton-impl/parser/expression
   #:coalton-impl/parser/parser)
  (:shadowing-import-from
   :coalton-impl/parser/base
   #:parse-error)
  (:import-from
   #:coalton-impl/parser/collect
   #:collect-type-variables)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm))
  (:export
   #:rename-variables                   ; FUNCTION
   ))

(in-package #:coalton-impl/parser/renamer)

(defun make-local-vars (vars &key (package *package*))
  (declare (type util:symbol-list vars))
  (loop :for var :in vars
        :collect (cons var (gentemp (concatenate 'string (symbol-name var) "-") package))))

(defun rename-variables (node)
  (rename-variables-generic% node (algo:make-immutable-map)))

(defgeneric rename-variables-generic% (node ctx)
  (:method ((node node-variable) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (let ((new-name (algo:immutable-map-lookup ctx (node-variable-name node))))

      (values
       (if new-name
           (make-node-variable
            :name new-name
            :source (node-source node))
           node)
       ctx)))

  (:method ((node node-literal) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values node ctx))

  (:method ((node node-integer-literal) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values node ctx))

  (:method ((node node-bind) ctx)
    (declare (type algo:immutable-map ctx)
             (values node-bind algo:immutable-map))

    (let* ((new-bindings (make-local-vars (pattern-variables (node-bind-pattern node))))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (values
       (make-node-bind
        :pattern (rename-variables-generic% (node-bind-pattern node) new-ctx)

        ;; ctx is used instead of new-ctx because bind creates non-recursive bindings
        :expr (rename-variables-generic% (node-bind-expr node) ctx)
        :source (node-bind-source node))
       new-ctx)))

  (:method ((node node-body) ctx)
    (declare (type algo:immutable-map ctx)
             (values node-body algo:immutable-map))

    (let ((new-ctx ctx))
      (values
       (make-node-body
        :nodes (loop :for elem :in (node-body-nodes node)
                     :collect (multiple-value-bind (elem new-ctx_)
                                  (rename-variables-generic% elem new-ctx)
                                (setf new-ctx new-ctx_)
                                elem))
        :last-node (rename-variables-generic% (node-body-last-node node) new-ctx))
       ctx)))

  (:method ((node node-abstraction) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (let* ((new-bindings (make-local-vars (mapcar #'node-variable-name (node-abstraction-vars node))))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (values
       (make-node-abstraction
        :vars (rename-variables-generic% (node-abstraction-vars node) new-ctx)
        :body (rename-variables-generic% (node-abstraction-body node) new-ctx)
        :source (node-source node))
       ctx)))

  (:method ((node node-let-binding) ctx)
    (declare (type algo:immutable-map ctx)
             (values node-let-binding algo:immutable-map))

    (values
     (make-node-let-binding
      :name (rename-variables-generic% (node-let-binding-name node) ctx)
      :value (rename-variables-generic% (node-let-binding-value node) ctx)
      :source (node-let-binding-source node))
     ctx))

  (:method ((node node-let-declare) ctx)
    (declare (type algo:immutable-map ctx)
             (values node-let-declare algo:immutable-map))

    (values
     (make-node-let-declare
      :name (rename-variables-generic% (node-let-declare-name node) ctx)
      :type (node-let-declare-type node)
      :source (node-let-declare-source node))
     ctx))

  (:method ((node node-let) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (let* ((new-bindings (make-local-vars (mapcar (alexandria:compose
                                                   #'node-variable-name
                                                   #'node-let-binding-name)
                                                  (node-let-bindings node))))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (values
       (make-node-let
        :bindings (rename-variables-generic% (node-let-bindings node) new-ctx)
        :declares (rename-variables-generic% (node-let-declares node) new-ctx)
        :body (rename-variables-generic% (node-let-body node) new-ctx)
        :source (node-source node))
       ctx)))

  ;; TOOD: think about how this works
  (:method ((node node-lisp) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values node ctx))

  (:method ((node node-match-branch) ctx)
    (declare (type algo:immutable-map ctx))

    (let* ((new-bindings (make-local-vars (pattern-variables (node-match-branch-pattern node))))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (values
       (make-node-match-branch
        :pattern (rename-variables-generic% (node-match-branch-pattern node) new-ctx)
        :body (rename-variables-generic% (node-match-branch-body node) new-ctx)
        :source (node-match-branch-source node))
       ctx)))

  (:method ((node node-match) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-match
      :expr (rename-variables-generic% (node-match-expr node) ctx)
      :branches (rename-variables-generic% (node-match-branches node) ctx)
      :source (node-source node))
     ctx))

  (:method ((node node-progn) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-progn
      :body (rename-variables-generic% (node-progn-body node) ctx)
      :source (node-source node))
     ctx))

  (:method ((node node-the) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-the
      :type (node-the-type node)
      :expr (rename-variables-generic% (node-the-expr node) ctx)
      :source (node-source node))
     ctx))

  (:method ((node node-return) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-return
      :expr (if (node-return-expr node)
                (rename-variables-generic% (node-return-expr node) ctx)
                nil)
      :source (node-source node))
     ctx))

  (:method ((node node-application) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-application
      :rator (rename-variables-generic% (node-application-rator node) ctx)
      :rands (rename-variables-generic% (node-application-rands node) ctx)
      :source (node-source node))
     ctx))

  (:method ((node node-or) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-or
      :nodes (rename-variables-generic% (node-or-nodes node) ctx)
      :source (node-source node))
     ctx))

  (:method ((node node-and) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-and
      :nodes (rename-variables-generic% (node-and-nodes node) ctx)
      :source (node-source node))
     ctx))

  (:method ((node node-if) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-if
      :expr (rename-variables-generic% (node-if-expr node) ctx)
      :then (rename-variables-generic% (node-if-then node) ctx)
      :else (rename-variables-generic% (node-if-else node) ctx)
      :source (node-source node))
     ctx))

  (:method ((node node-when) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-when
      :expr (rename-variables-generic% (node-when-expr node) ctx)
      :body (rename-variables-generic% (node-when-body node) ctx)
      :source (node-source node))
     ctx))

  (:method ((node node-unless) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-unless
      :expr (rename-variables-generic% (node-unless-expr node) ctx)
      :body (rename-variables-generic% (node-unless-body node) ctx)
      :source (node-source node))
     ctx))

  (:method ((node node-cond-clause) ctx)
    (declare (type algo:immutable-map ctx)
             (values node-cond-clause algo:immutable-map))

    (values
     (make-node-cond-clause
      :expr (rename-variables-generic% (node-cond-clause-expr node) ctx)
      :body (rename-variables-generic% (node-cond-clause-body node) ctx)
      :source (node-cond-clause-source node))
     ctx))

  (:method ((node node-cond) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-cond
      :clauses (rename-variables-generic% (node-cond-clauses node) ctx)
      :source (node-source node))
     ctx))

  (:method ((node node-do-bind) ctx)
    (declare (type algo:immutable-map ctx)
             (values node-do-bind algo:immutable-map))

    (let* ((new-bindings (make-local-vars (list (node-variable-name (node-do-bind-name node)))))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (values
       (make-node-do-bind
        :name (rename-variables-generic% (node-do-bind-name node) new-ctx)
        :expr (rename-variables-generic% (node-do-bind-expr node) ctx)
        :source (node-do-bind-source node))
       new-ctx)))

  (:method ((node node-do) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (let ((new-ctx ctx))
      (values
       (make-node-do
        :nodes (loop :for elem :in (node-do-nodes node)
                     :collect (multiple-value-bind (elem new-ctx_)
                                  (rename-variables-generic% elem new-ctx)
                                (setf new-ctx new-ctx_)
                                elem))
        :last-node (rename-variables-generic% (node-do-last-node node) new-ctx)
        :source (node-source node))
       ctx)))

  (:method ((pattern pattern-var) ctx)
    (declare (type algo:immutable-map ctx)
             (values pattern algo:immutable-map))

    (let ((new-name (algo:immutable-map-lookup ctx (pattern-var-name pattern))))
      (values
       (if new-name
           (make-pattern-var
            :name new-name
            :source (pattern-source pattern))
           pattern)
       ctx)))

  (:method ((pattern pattern-literal) ctx)
    (declare (type algo:immutable-map ctx)
             (values pattern algo:immutable-map))

    (values pattern ctx))

  (:method ((pattern pattern-wildcard) ctx)
    (declare (type algo:immutable-map ctx)
             (values pattern algo:immutable-map))

    (values pattern ctx))

  (:method ((pattern pattern-constructor) ctx)
    (declare (type algo:immutable-map ctx)
             (values pattern algo:immutable-map))

    (values
     (make-pattern-constructor
      :name (pattern-constructor-name pattern)
      :patterns (rename-variables-generic% (pattern-constructor-patterns pattern) ctx)
      :source (pattern-source pattern))
     ctx))

  (:method ((toplevel toplevel-define) ctx)
    (declare (type algo:immutable-map ctx)
             (values toplevel-define algo:immutable-map))

    (let* ((new-bindings (make-local-vars (mapcar #'node-variable-name (toplevel-define-vars toplevel))))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (values
       (make-toplevel-define
        :name (toplevel-define-name toplevel)
        :vars (rename-variables-generic% (toplevel-define-vars toplevel) new-ctx)
        :body (rename-variables-generic% (toplevel-define-body toplevel) new-ctx)
        :source (toplevel-define-source toplevel)
        :monomorphize (toplevel-define-monomorphize toplevel))
       ctx)))

  (:method ((method instance-method-definition) ctx)
    (declare (type algo:immutable-map ctx)
             (values instance-method-definition algo:immutable-map))

    (let* ((new-bindings (make-local-vars (mapcar #'node-variable-name (instance-method-definition-vars method))))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (values
       (make-instance-method-definition
        :name (instance-method-definition-name method)
        :vars (rename-variables-generic% (instance-method-definition-vars method) new-ctx)
        :body (rename-variables-generic% (instance-method-definition-body method) new-ctx)
        :source (instance-method-definition-source method))
       ctx)))

  (:method ((toplevel toplevel-define-instance) ctx)
    (declare (type algo:immutable-map ctx)
             (values toplevel-define-instance algo:immutable-map))

    (values
     (make-toplevel-define-instance
      :context (toplevel-define-instance-context toplevel)
      :pred (toplevel-define-instance-pred toplevel)
      :methods (rename-variables-generic% (toplevel-define-instance-methods toplevel) ctx)
      :source (toplevel-define-instance-source toplevel)
      :head-src (toplevel-define-instance-head-src toplevel))
     ctx))

  (:method ((program program) ctx)
    (declare (type algo:immutable-map ctx)
             (values program algo:immutable-map))

    (values
     (make-program
      :package (program-package program)
      :file (program-file program)
      :types (rename-type-variables (program-types program))
      :declares (program-declares program)
      :defines (rename-variables-generic% (program-defines program) ctx)
      :classes (rename-type-variables (program-classes program))
      :instances (rename-variables-generic% (program-instances program) ctx))
     ctx))

  (:method ((list list) ctx)
    (declare (type algo:immutable-map ctx)
             (values t algo:immutable-map))

    (values
     (mapcar
      (lambda (node)
        (rename-variables-generic% node ctx))
      list)
     ctx)))

(defun rename-type-variables (ty)
  (declare (type t ty))
  (rename-type-variables-generic% ty (algo:make-immutable-map)))

(defgeneric rename-type-variables-generic% (ty ctx)
  (:method ((ty tyvar) ctx)
    (declare (type algo:immutable-map ctx)
             (values tyvar))

    (let ((new-name (algo:immutable-map-lookup ctx (tyvar-name ty))))

      (if new-name
          (make-tyvar
           :name new-name
           :source (ty-source ty))
          ty)))

  (:method ((ty tycon) ctx)
    (declare (type algo:immutable-map ctx)
             (values tycon))

    ty)

  (:method ((ty tapp) ctx)
    (declare (type algo:immutable-map ctx)
             (values tapp))

    (make-tapp
     :from (rename-type-variables-generic% (tapp-from ty) ctx)
     :to (rename-type-variables-generic% (tapp-to ty) ctx)
     :source (ty-source ty)))

  (:method ((pred ty-predicate) ctx)
    (declare (type algo:immutable-map ctx)
             (values ty-predicate))

    (make-ty-predicate
     :class (ty-predicate-class pred)
     :types (rename-type-variables-generic% (ty-predicate-types pred) ctx)
     :source (ty-predicate-source pred)))

  (:method ((qual-ty qualified-ty) ctx)
    (declare (type algo:immutable-map ctx)
             (values qualified-ty))

    (make-qualified-ty
     :predicates (rename-type-variables-generic% (qualified-ty-predicates qual-ty) ctx) 
     :type (rename-type-variables-generic% (qualified-ty-type qual-ty) ctx)
     :source (qualified-ty-source qual-ty)))

  (:method ((ctor constructor) ctx)
    (declare (type algo:immutable-map ctx)
             (values constructor))
    (make-constructor
     :name (constructor-name ctor)
     :fields (rename-type-variables-generic% (constructor-fields ctor) ctx)
     :source (constructor-source ctor)))

  (:method ((keyword keyword-src) ctx)
    (declare (type algo:immutable-map ctx)
             (values keyword-src))

    (let ((new-name (algo:immutable-map-lookup ctx (keyword-src-name keyword))))

      (if new-name
          (make-keyword-src
           :name new-name
           :source (keyword-src-source keyword))
          keyword)))

  ;; TODO: is this instance working?
  (:method ((toplevel toplevel-define-type) ctx)
    (declare (type algo:immutable-map ctx)
             (values toplevel-define-type))

    (let* ((tvars (mapcar #'keyword-src-name (toplevel-define-type-vars toplevel)))

           (new-bindings (make-local-vars tvars :package util:+keyword-package+))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (make-toplevel-define-type
       :name (toplevel-define-type-name toplevel)
       :vars (rename-type-variables-generic% (toplevel-define-type-vars toplevel) new-ctx)
       :docstring (toplevel-define-type-docstring toplevel)
       :ctors (rename-type-variables-generic% (toplevel-define-type-ctors toplevel) new-ctx)
       :source (toplevel-define-type-source toplevel)
       :repr (toplevel-define-type-repr toplevel)
       :head-src (toplevel-define-type-head-src toplevel))))

  (:method ((fundep fundep) ctx)
    (declare (type algo:immutable-map ctx)
             (values fundep))

    (make-fundep
     :left (rename-type-variables-generic% (fundep-left fundep) ctx)
     :right (rename-type-variables-generic% (fundep-right fundep) ctx)
     :source (fundep-source fundep)))

  (:method ((method method-definition) ctx)
    (declare (type algo:immutable-map ctx)
             (values method-definition))

    (let* ((bound-variables (algo:immutable-map-keys ctx))

           (tvars (mapcar #'tyvar-name (collect-type-variables method)))

           (new-tvars (set-difference tvars bound-variables :test #'eq))

           (new-bindings (make-local-vars new-tvars :package util:+keyword-package+))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (make-method-definition
       :name (method-definition-name method)
       :type (rename-type-variables-generic% (method-definition-type method) new-ctx)
       :source (method-definition-source method))))

  (:method ((toplevel toplevel-define-class) ctx)
    (declare (type algo:immutable-map ctx)
             (values toplevel-define-class))

    (let* ((tvars (mapcar #'keyword-src-name (toplevel-define-class-vars toplevel)))

           (new-bindings (make-local-vars tvars :package util:+keyword-package+))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (make-toplevel-define-class
       :name (toplevel-define-class-name toplevel)
       :vars (rename-type-variables-generic% (toplevel-define-class-vars toplevel) new-ctx)
       :preds (rename-type-variables-generic% (toplevel-define-class-preds toplevel) new-ctx)
       :fundeps (rename-type-variables-generic% (toplevel-define-class-fundeps toplevel) new-ctx)
       :methods (rename-type-variables-generic% (toplevel-define-class-methods toplevel) new-ctx)
       :source (toplevel-define-class-source toplevel)
       :head-src (toplevel-define-class-head-src toplevel))))

  (:method ((list list) ctx)
    (mapcar
     (lambda (ty)
       (rename-type-variables-generic% ty ctx))
     list)))
