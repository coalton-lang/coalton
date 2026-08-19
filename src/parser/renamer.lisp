(defpackage #:coalton-impl/parser/renamer
  (:use
   #:cl
   #:coalton-impl/parser/base
   #:coalton-impl/parser/types
   #:coalton-impl/parser/pattern
   #:coalton-impl/parser/expression
   #:coalton-impl/parser/toplevel)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:import-from
   #:coalton-impl/parser/collect
   #:collect-type-variables)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:source #:coalton-impl/source)
   (#:algo #:coalton-impl/algorithm))
  (:export
   #:rename-variables                   ; FUNCTION
   #:rename-type-variables              ; FUNCTION
   ))

(in-package #:coalton-impl/parser/renamer)

(defun make-local-var (var &key (package *package*))
  (declare (type symbol var)
           (values symbol &optional))
  (gentemp (concatenate 'string (symbol-name var) "-") package))

(defun make-local-vars (vars &key (package *package*))
  (declare (type util:symbol-list vars))
  (loop :for var :in vars
        :collect (cons var (make-local-var var :package package))))

(defun rename-builder-binder (binder ctx)
  (declare (type node-variable binder)
           (type algo:immutable-map ctx)
           (values node-variable algo:immutable-map &optional))
  (let* ((new-name (make-local-var (node-variable-name binder)))
         (renamed (make-node-variable
                   :name new-name
                   :location (source:location binder))))
    (values renamed
            (algo:immutable-map-set ctx (node-variable-name binder) new-name))))

(defun rename-builder-clauses-sequentially (clauses ctx)
  (declare (type builder-clause-list clauses)
           (type algo:immutable-map ctx)
           (values builder-clause-list algo:immutable-map &optional))
  (loop :with renamed := nil
        :with current-ctx := ctx
        :for clause :in clauses
        :do (multiple-value-bind (renamed-clause next-ctx)
                (rename-variables-generic% clause current-ctx)
              (push renamed-clause renamed)
              (setf current-ctx next-ctx))
        :finally (return (values (nreverse renamed) current-ctx))))

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
            :location (source:location node))
           node)
       ctx)))

  (:method ((node node-accessor) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     node
     ctx))

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

    (let* ((new-bindings (make-local-vars (mapcar #'pattern-var-name (pattern-variables (node-bind-pattern node)))))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (values
       (make-node-bind
        :pattern (rename-variables-generic% (node-bind-pattern node) new-ctx)

        ;; ctx is used instead of new-ctx because bind creates non-recursive bindings
        :expr (rename-variables-generic% (node-bind-expr node) ctx)
        :location (source:location node))
       new-ctx)))

  (:method ((node node-values-bind) ctx)
    (declare (type algo:immutable-map ctx)
             (values node-values-bind algo:immutable-map))

    (let* ((new-bindings (make-local-vars (mapcar #'pattern-var-name
                                                  (pattern-variables (node-values-bind-patterns node)))))
           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))
      (values
       (make-node-values-bind
        :patterns (rename-variables-generic% (node-values-bind-patterns node) new-ctx)
        :expr (rename-variables-generic% (node-values-bind-expr node) ctx)
        :location (source:location node))
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

    (labels ((rename-keyword-params (keyword-params base-ctx)
               (loop :with renamed := nil
                     :with cur-ctx := base-ctx
                     :for param :in keyword-params
                     :for binder := (keyword-param-binder param)
                     :for new-name := (make-local-var (node-variable-name binder))
                     :for renamed-binder := (make-node-variable
                                             :name new-name
                                             :location (source:location binder))
                     :for renamed-default := (rename-variables-generic% (keyword-param-default param)
                                                                        cur-ctx)
                     :do (push (make-keyword-param
                                :keyword (keyword-param-keyword param)
                                :binder renamed-binder
                                :default renamed-default
                                :location (source:location param))
                               renamed)
                         (setf cur-ctx (algo:immutable-map-set cur-ctx
                                                               (node-variable-name binder)
                                                               new-name))
                     :finally (return (values (nreverse renamed) cur-ctx)))))
      (let* ((positional-bindings
               (make-local-vars
                (mapcar #'pattern-var-name
                        (pattern-variables (node-abstraction-params node)))))
             (positional-ctx (algo:immutable-map-set-multiple ctx positional-bindings)))
        (multiple-value-bind (keyword-params body-ctx)
            (rename-keyword-params (node-abstraction-keyword-params node) positional-ctx)
          (values
           (make-node-abstraction
            :params (rename-variables-generic% (node-abstraction-params node) positional-ctx)
            :keyword-params keyword-params
            :body (rename-variables-generic% (node-abstraction-body node) body-ctx)
            :introduces-return-scope-p (node-abstraction-introduces-return-scope-p node)
            :location (source:location node))
           ctx)))))

  (:method ((param keyword-param) ctx)
    (declare (type algo:immutable-map ctx)
             (values keyword-param algo:immutable-map))
    (values
     (make-keyword-param
      :keyword (keyword-param-keyword param)
      :binder (rename-variables-generic% (keyword-param-binder param) ctx)
      :default (rename-variables-generic% (keyword-param-default param) ctx)
      :location (source:location param))
     ctx))

  (:method ((node node-let-binding) ctx)
    (declare (type algo:immutable-map ctx)
             (values node-let-binding algo:immutable-map))

    (values
     (make-node-let-binding
      :name (rename-variables-generic% (node-let-binding-name node) ctx)
      :value (rename-variables-generic% (node-let-binding-value node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-dynamic-binding) ctx)
    (declare (type algo:immutable-map ctx)
             (values node-dynamic-binding algo:immutable-map))

    (values
     (make-node-dynamic-binding
      :name (node-dynamic-binding-name node)
      :value (rename-variables-generic% (node-dynamic-binding-value node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-let-declare) ctx)
    (declare (type algo:immutable-map ctx)
             (values node-let-declare algo:immutable-map))

    (values
     (make-node-let-declare
      :name (rename-variables-generic% (node-let-declare-name node) ctx)
      :type (node-let-declare-type node)
      :location (source:location node))
     ctx))

  (:method ((node node-let) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (cond
      ((node-let-sequential-p node)
       (let ((current-ctx ctx)
             (new-bindings nil))
         (dolist (binding (node-let-bindings node))
           (multiple-value-bind (value _)
               (rename-variables-generic% (node-let-binding-value binding) current-ctx)
             (declare (ignore _))
             (let* ((name-node (node-let-binding-name binding))
                    (name (node-variable-name name-node))
                    (new-name (make-local-var name))
                    (next-ctx (algo:immutable-map-set current-ctx name new-name)))
               (push (make-node-let-binding
                      :name (make-node-variable
                             :name new-name
                             :location (source:location name-node))
                      :value value
                      :location (source:location binding))
                     new-bindings)
               (setf current-ctx next-ctx))))
         (values
          (make-node-let
           :bindings (nreverse new-bindings)
           :declares (rename-variables-generic% (node-let-declares node) current-ctx)
           :body (rename-variables-generic% (node-let-body node) current-ctx)
           :sequential-p t
           :location (source:location node))
          ctx)))
      (t
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
           :location (source:location node))
          ctx)))))

  (:method ((node node-rec) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (let* ((outer-bindings
             (make-local-vars
              (mapcar (alexandria:compose #'node-variable-name
                                          #'node-let-binding-name)
                      (node-rec-bindings node))))
           (outer-ctx
             (algo:immutable-map-set-multiple ctx outer-bindings))
           (rec-name
             (make-local-var (node-variable-name (node-rec-name node))))
           (param-bindings
             (make-local-vars
              (mapcar #'pattern-var-name
                      (pattern-variables (node-rec-params node)))))
           (body-ctx
             (algo:immutable-map-set-multiple
              (algo:immutable-map-set ctx
                                      (node-variable-name (node-rec-name node))
                                      rec-name)
              param-bindings)))
      (values
       (make-node-rec
        :name (make-node-variable
               :name rec-name
               :location (source:location (node-rec-name node)))
        :bindings (rename-variables-generic% (node-rec-bindings node) outer-ctx)
        :declares (rename-variables-generic% (node-rec-declares node) outer-ctx)
        :params (rename-variables-generic% (node-rec-params node) body-ctx)
        :call-args (rename-variables-generic% (node-rec-call-args node) outer-ctx)
        :body (rename-variables-generic% (node-rec-body node) body-ctx)
        :location (source:location node))
       ctx)))

  (:method ((node node-dynamic-let) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-dynamic-let
      :bindings (rename-variables-generic% (node-dynamic-let-bindings node) ctx)
      :subexpr (rename-variables-generic% (node-dynamic-let-subexpr node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-for) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (cond
      ((node-for-sequential-p node)
       (let ((current-ctx ctx)
             (renamed-bindings nil))
         (dolist (binding (node-for-bindings node))
           (multiple-value-bind (init _)
               (rename-variables-generic% (node-for-binding-init binding) current-ctx)
             (declare (ignore _))
             (let* ((name-node (node-for-binding-name binding))
                    (name (node-variable-name name-node))
                    (new-name (make-local-var name))
                    (next-ctx (algo:immutable-map-set current-ctx name new-name)))
               (push (list binding new-name init) renamed-bindings)
               (setf current-ctx next-ctx))))
         (values
          (make-node-for
           :location (source:location node)
           :label (node-for-label node)
           :bindings
           (loop :for (binding new-name init) :in (nreverse renamed-bindings)
                 :collect (make-node-for-binding
                           :name (make-node-variable
                                  :name new-name
                                  :location (source:location (node-for-binding-name binding)))
                           :init init
                           :step (and (node-for-binding-step binding)
                                      (rename-variables-generic%
                                       (node-for-binding-step binding)
                                       current-ctx))
                           :location (source:location binding)))
           :declares (rename-variables-generic% (node-for-declares node) current-ctx)
           :returns (and (node-for-returns node)
                         (rename-variables-generic% (node-for-returns node) current-ctx))
           :termination-kind (node-for-termination-kind node)
           :termination-expr (and (node-for-termination-expr node)
                                  (rename-variables-generic%
                                   (node-for-termination-expr node)
                                   current-ctx))
           :body (rename-variables-generic% (node-for-body node) current-ctx)
           :sequential-p t)
          ctx)))
      (t
       (let* ((new-bindings
                (make-local-vars
                 (mapcar (alexandria:compose
                          #'node-variable-name
                          #'node-for-binding-name)
                         (node-for-bindings node))))
              (new-ctx
                (algo:immutable-map-set-multiple ctx new-bindings)))

         (values
          (make-node-for
           :location (source:location node)
           :label (node-for-label node)
           :bindings
           (loop :for binding :in (node-for-bindings node)
                 :collect (make-node-for-binding
                           :name (rename-variables-generic% (node-for-binding-name binding) new-ctx)
                           :init (rename-variables-generic% (node-for-binding-init binding) new-ctx)
                           :step (and (node-for-binding-step binding)
                                      (rename-variables-generic%
                                       (node-for-binding-step binding)
                                       new-ctx))
                           :location (source:location binding)))
           :declares (rename-variables-generic% (node-for-declares node) new-ctx)
           :returns (and (node-for-returns node)
                         (rename-variables-generic% (node-for-returns node) new-ctx))
           :termination-kind (node-for-termination-kind node)
           :termination-expr (and (node-for-termination-expr node)
                                  (rename-variables-generic%
                                   (node-for-termination-expr node)
                                   new-ctx))
           :body (rename-variables-generic% (node-for-body node) new-ctx))
          ctx)))))

  (:method ((node node-lisp) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-lisp
      :location (source:location node)
      :output-types (node-lisp-output-types node)
      :vars (rename-variables-generic% (node-lisp-vars node) ctx)
      :var-names (node-lisp-var-names node)
      :body (node-lisp-body node))
     ctx))

  (:method ((node node-match-branch) ctx)
    (declare (type algo:immutable-map ctx))

    (let* ((new-bindings (make-local-vars (mapcar #'pattern-var-name
                                                  (pattern-variables (node-match-branch-pattern node)))))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (values
       (make-node-match-branch
        :pattern (rename-variables-generic% (node-match-branch-pattern node) new-ctx)
        :body (rename-variables-generic% (node-match-branch-body node) new-ctx)
        :location (source:location node))
       ctx)))

  (:method ((node node-match) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-match
      :expr (rename-variables-generic% (node-match-expr node) ctx)
      :branches (rename-variables-generic% (node-match-branches node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-resumable-branch) ctx)
    (declare (type algo:immutable-map ctx))

    (let* ((new-bindings (make-local-vars (mapcar #'pattern-var-name
                                                  (pattern-variables (node-resumable-branch-pattern node)))))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (values
       (make-node-resumable-branch
        :pattern (rename-variables-generic% (node-resumable-branch-pattern node) new-ctx)
        :body (rename-variables-generic% (node-resumable-branch-body node) new-ctx)
        :location (source:location node))
       ctx)))

  (:method ((node node-resumable) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-resumable
      :expr (rename-variables-generic% (node-resumable-expr node) ctx)
      :branches (rename-variables-generic% (node-resumable-branches node) ctx)
      :location (source:location node))
     ctx))



  (:method ((node node-catch-branch) ctx)
    (declare (type algo:immutable-map ctx))

    (let* ((new-bindings (make-local-vars (mapcar #'pattern-var-name
                                                  (pattern-variables (node-catch-branch-pattern node)))))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (values
       (make-node-catch-branch
        :pattern (rename-variables-generic% (node-catch-branch-pattern node) new-ctx)
        :body (rename-variables-generic% (node-catch-branch-body node) new-ctx)
        :location (source:location node))
       ctx)))

  (:method ((node node-catch) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-catch
      :expr (rename-variables-generic% (node-catch-expr node) ctx)
      :branches (rename-variables-generic% (node-catch-branches node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-progn) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-progn
      :body (rename-variables-generic% (node-progn-body node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-type-of) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-type-of
      :expr (rename-variables-generic% (node-type-of-expr node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-unsafe) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-unsafe
      :body (rename-variables-generic% (node-unsafe-body node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-the) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-the
      :type (node-the-type node)
      :expr (rename-variables-generic% (node-the-expr node) ctx)
     :location (source:location node))
     ctx))

  (:method ((node node-collection-builder) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))
    (values
     (make-node-collection-builder
      :elements (rename-variables-generic% (node-collection-builder-elements node) ctx)
      :location (source:location node))
     ctx))

  (:method ((entry association-entry) ctx)
    (declare (type algo:immutable-map ctx)
             (values association-entry algo:immutable-map))
    (values
     (make-association-entry
      :key (rename-variables-generic% (association-entry-key entry) ctx)
      :value (rename-variables-generic% (association-entry-value entry) ctx)
      :location (source:location entry))
     ctx))

  (:method ((node node-association-builder) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))
    (values
     (make-node-association-builder
      :entries (rename-variables-generic% (node-association-builder-entries node) ctx)
      :location (source:location node))
     ctx))

  (:method ((clause builder-with-clause) ctx)
    (declare (type algo:immutable-map ctx)
             (values builder-with-clause algo:immutable-map))
    (multiple-value-bind (binder next-ctx)
        (rename-builder-binder (builder-with-clause-binder clause) ctx)
      (values
       (make-builder-with-clause
        :binder binder
        :expr (rename-variables-generic% (builder-with-clause-expr clause) ctx)
        :location (source:location clause))
       next-ctx)))

  (:method ((clause builder-for-clause) ctx)
    (declare (type algo:immutable-map ctx)
             (values builder-for-clause algo:immutable-map))
    (multiple-value-bind (binder next-ctx)
        (rename-builder-binder (builder-for-clause-binder clause) ctx)
      (values
       (make-builder-for-clause
        :binder binder
        :expr (rename-variables-generic% (builder-for-clause-expr clause) ctx)
        :location (source:location clause))
       next-ctx)))

  (:method ((clause builder-below-clause) ctx)
    (declare (type algo:immutable-map ctx)
             (values builder-below-clause algo:immutable-map))
    (multiple-value-bind (binder next-ctx)
        (rename-builder-binder (builder-below-clause-binder clause) ctx)
      (values
       (make-builder-below-clause
        :binder binder
        :expr (rename-variables-generic% (builder-below-clause-expr clause) ctx)
        :location (source:location clause))
       next-ctx)))

  (:method ((clause builder-when-clause) ctx)
    (declare (type algo:immutable-map ctx)
             (values builder-when-clause algo:immutable-map))
    (values
     (make-builder-when-clause
      :expr (rename-variables-generic% (builder-when-clause-expr clause) ctx)
      :location (source:location clause))
     ctx))

  (:method ((node node-collection-comprehension) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))
    (multiple-value-bind (clauses final-ctx)
        (rename-builder-clauses-sequentially (node-collection-comprehension-clauses node) ctx)
      (values
       (make-node-collection-comprehension
        :head (rename-variables-generic% (node-collection-comprehension-head node) final-ctx)
        :clauses clauses
        :location (source:location node))
       ctx)))

  (:method ((node node-association-comprehension) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))
    (multiple-value-bind (clauses final-ctx)
        (rename-builder-clauses-sequentially (node-association-comprehension-clauses node) ctx)
      (values
       (make-node-association-comprehension
        :key (rename-variables-generic% (node-association-comprehension-key node) final-ctx)
        :value (rename-variables-generic% (node-association-comprehension-value node) final-ctx)
        :clauses clauses
        :location (source:location node))
       ctx)))

  (:method ((node node-return) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-return
      :expr (if (node-return-expr node)
                (rename-variables-generic% (node-return-expr node) ctx)
                nil)
      :location (source:location node))
     ctx))

  (:method ((node node-values) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-values
      :nodes (rename-variables-generic% (node-values-nodes node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-throw) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-throw
      :expr (if (node-throw-expr node)
                (rename-variables-generic% (node-throw-expr node) ctx)
                nil)
      :location (source:location node))
     ctx))

  (:method ((node node-resume-to) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-resume-to
      :expr (if (node-resume-to-expr node)
                (rename-variables-generic% (node-resume-to-expr node) ctx)
                nil)
      :location (source:location node))
     ctx))

  (:method ((node node-application) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-application
      :rator (rename-variables-generic% (node-application-rator node) ctx)
      :rands (rename-variables-generic% (node-application-rands node) ctx)
      :keyword-rands (rename-variables-generic% (node-application-keyword-rands node) ctx)
      :location (source:location node))
     ctx))

  (:method ((arg node-application-keyword-arg) ctx)
    (declare (type algo:immutable-map ctx)
             (values node-application-keyword-arg algo:immutable-map))
    (values
     (make-node-application-keyword-arg
      :keyword (node-application-keyword-arg-keyword arg)
      :value (rename-variables-generic% (node-application-keyword-arg-value arg) ctx)
      :location (source:location arg))
     ctx))

  (:method ((node node-or) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-or
      :nodes (rename-variables-generic% (node-or-nodes node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-and) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-and
      :nodes (rename-variables-generic% (node-and-nodes node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-if) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-if
      :expr (rename-variables-generic% (node-if-expr node) ctx)
      :then (rename-variables-generic% (node-if-then node) ctx)
      :else (rename-variables-generic% (node-if-else node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-when) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-when
      :expr (rename-variables-generic% (node-when-expr node) ctx)
      :body (rename-variables-generic% (node-when-body node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-break) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))
    (values
     node
     ctx))

  (:method ((node node-continue) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))
    (values
     node
     ctx))
  
  (:method ((node node-unless) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-unless
      :expr (rename-variables-generic% (node-unless-expr node) ctx)
      :body (rename-variables-generic% (node-unless-body node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-cond-clause) ctx)
    (declare (type algo:immutable-map ctx)
             (values node-cond-clause algo:immutable-map))

    (values
     (make-node-cond-clause
      :expr (rename-variables-generic% (node-cond-clause-expr node) ctx)
      :body (rename-variables-generic% (node-cond-clause-body node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-cond) ctx)
    (declare (type algo:immutable-map ctx)
             (values node algo:immutable-map))

    (values
     (make-node-cond
      :clauses (rename-variables-generic% (node-cond-clauses node) ctx)
      :location (source:location node))
     ctx))

  (:method ((node node-do-bind) ctx)
    (declare (type algo:immutable-map ctx)
             (values node-do-bind algo:immutable-map))

    (let* ((new-bindings
             (make-local-vars
              (mapcar #'pattern-var-name
                      (pattern-variables (node-do-bind-pattern node)))))
           (new-ctx
             (algo:immutable-map-set-multiple ctx new-bindings)))

      (values
       (make-node-do-bind
        :pattern (rename-variables-generic% (node-do-bind-pattern node) new-ctx)
        :expr (rename-variables-generic% (node-do-bind-expr node) ctx)
        :location (source:location node))
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
        :location (source:location node))
       ctx)))

  (:method ((pattern pattern-binding) ctx)
    (declare (type algo:immutable-map ctx)
             (values pattern algo:immutable-map))
    (multiple-value-bind (newvar ctx)
        (rename-variables-generic% (pattern-binding-var pattern) ctx)
      (multiple-value-bind (new-pattern ctx)
          (rename-variables-generic% (pattern-binding-pattern pattern) ctx)
        (values
         (make-pattern-binding
          :location (source:location pattern)
          :var newvar
          :pattern new-pattern)
         ctx))))

  (:method ((pattern pattern-var) ctx)
    (declare (type algo:immutable-map ctx)
             (values pattern algo:immutable-map))

    (let ((new-name (algo:immutable-map-lookup ctx (pattern-var-name pattern))))
      (values
       (if new-name
           (make-pattern-var
            :name new-name
            :orig-name (pattern-var-orig-name pattern)
            :location (source:location pattern))
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
      :location (source:location pattern))
     ctx))

  (:method ((toplevel toplevel-define) ctx)
    (declare (type algo:immutable-map ctx)
             (values toplevel-define algo:immutable-map))

    (labels ((rename-keyword-params (keyword-params base-ctx)
               (loop :with renamed := nil
                     :with cur-ctx := base-ctx
                     :for param :in keyword-params
                     :for binder := (keyword-param-binder param)
                     :for new-name := (make-local-var (node-variable-name binder))
                     :for renamed-binder := (make-node-variable
                                             :name new-name
                                             :location (source:location binder))
                     :for renamed-default := (rename-variables-generic% (keyword-param-default param)
                                                                        cur-ctx)
                     :do (push (make-keyword-param
                                :keyword (keyword-param-keyword param)
                                :binder renamed-binder
                                :default renamed-default
                                :location (source:location param))
                               renamed)
                         (setf cur-ctx (algo:immutable-map-set cur-ctx
                                                               (node-variable-name binder)
                                                               new-name))
                     :finally (return (values (nreverse renamed) cur-ctx)))))
      (let* ((positional-bindings
               (make-local-vars
                (mapcar #'pattern-var-name
                        (pattern-variables (toplevel-define-params toplevel)))))
             (positional-ctx
               (algo:immutable-map-set-multiple ctx positional-bindings)))
        (multiple-value-bind (keyword-params body-ctx)
            (rename-keyword-params (toplevel-define-keyword-params toplevel) positional-ctx)
          (values
           (make-toplevel-define
            :name (toplevel-define-name toplevel)
            :params (rename-variables-generic% (toplevel-define-params toplevel) positional-ctx)
            :keyword-params keyword-params
            :function-syntax-p (toplevel-define-function-syntax-p toplevel)
            :orig-params (toplevel-define-orig-params toplevel)
            :orig-keyword-params (toplevel-define-orig-keyword-params toplevel)
            :docstring (source:docstring toplevel)
            :body (rename-variables-generic% (toplevel-define-body toplevel) body-ctx)
            :location (source:location toplevel)
            :monomorphize (toplevel-define-monomorphize toplevel)
            :inline (toplevel-define-inline toplevel))
           ctx)))))

  (:method ((method instance-method-definition) ctx)
    (declare (type algo:immutable-map ctx)
             (values instance-method-definition algo:immutable-map))

    (labels ((rename-keyword-params (keyword-params base-ctx)
               (loop :with renamed := nil
                     :with cur-ctx := base-ctx
                     :for param :in keyword-params
                     :for binder := (keyword-param-binder param)
                     :for new-name := (make-local-var (node-variable-name binder))
                     :for renamed-binder := (make-node-variable
                                             :name new-name
                                             :location (source:location binder))
                     :for renamed-default := (rename-variables-generic% (keyword-param-default param)
                                                                        cur-ctx)
                     :do (push (make-keyword-param
                                :keyword (keyword-param-keyword param)
                                :binder renamed-binder
                                :default renamed-default
                                :location (source:location param))
                               renamed)
                         (setf cur-ctx (algo:immutable-map-set cur-ctx
                                                               (node-variable-name binder)
                                                               new-name))
                     :finally (return (values (nreverse renamed) cur-ctx)))))
      (let* ((positional-bindings
               (make-local-vars
                (mapcar #'pattern-var-name
                        (pattern-variables (instance-method-definition-params method)))))
             (positional-ctx (algo:immutable-map-set-multiple ctx positional-bindings)))
        (multiple-value-bind (keyword-params body-ctx)
            (rename-keyword-params (instance-method-definition-keyword-params method) positional-ctx)
          (values
           (make-instance-method-definition
            :name (instance-method-definition-name method)
            :params (rename-variables-generic% (instance-method-definition-params method) positional-ctx)
            :keyword-params keyword-params
            :function-syntax-p (instance-method-definition-function-syntax-p method)
            :body (rename-variables-generic% (instance-method-definition-body method) body-ctx)
            :location (source:location method)
            :inline (instance-method-definition-inline method))
           ctx)))))

  (:method ((toplevel toplevel-define-instance) ctx)
    (declare (type algo:immutable-map ctx)
             (values toplevel-define-instance algo:immutable-map))

    (values
     (make-toplevel-define-instance
      :context (toplevel-define-instance-context toplevel)
      :pred (toplevel-define-instance-pred toplevel)
      :methods (rename-variables-generic% (toplevel-define-instance-methods toplevel) ctx)
      :location (source:location toplevel)
      :head-location (toplevel-define-instance-head-location toplevel)
      :docstring (source:docstring toplevel)
      :compiler-generated (toplevel-define-instance-compiler-generated toplevel))
     ctx))

  (:method ((program program) ctx)
    (declare (type algo:immutable-map ctx)
             (values program algo:immutable-map))

    (values
     (make-program
      :package (program-package program)
      :types (rename-type-variables (program-types program))
      :type-aliases (rename-type-variables (program-type-aliases program))
      :structs (rename-type-variables (program-structs program))
      :declares (program-declares program)
      :defines (rename-variables-generic% (program-defines program) ctx)
      :classes (program-classes program) ; Class type variables are renamed during kind inference
      :instances (rename-variables-generic% (program-instances program) ctx)
      :lisp-forms (program-lisp-forms program)
      :specializations (program-specializations program) ; Renaming type variables in specializations is not valid
      )
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
           :source-name (tyvar-source-name ty)
           :location (source:location ty))
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
     :location (source:location ty)))

  (:method ((entry keyword-ty-entry) ctx)
    (declare (type algo:immutable-map ctx)
             (values keyword-ty-entry &optional))
    (make-keyword-ty-entry
     :keyword (keyword-ty-entry-keyword entry)
     :type (rename-type-variables-generic% (keyword-ty-entry-type entry) ctx)
     :location (source:location entry)))

  (:method ((ty function-ty) ctx)
    (declare (type algo:immutable-map ctx)
             (values function-ty &optional))
    (make-function-ty
     :positional-input-types
     (rename-type-variables-generic% (function-ty-positional-input-types ty) ctx)
     :keyword-input-types
     (rename-type-variables-generic% (function-ty-keyword-input-types ty) ctx)
     :output-types
     (rename-type-variables-generic% (function-ty-output-types ty) ctx)
     :location (source:location ty)))

  (:method ((ty result-ty) ctx)
    (declare (type algo:immutable-map ctx)
             (values result-ty &optional))
    (make-result-ty
     :output-types (rename-type-variables-generic% (result-ty-output-types ty) ctx)
     :location (source:location ty)))

  (:method ((pred ty-predicate) ctx)
    (declare (type algo:immutable-map ctx)
             (values ty-predicate))

    (make-ty-predicate
     :class (ty-predicate-class pred)
     :types (rename-type-variables-generic% (ty-predicate-types pred) ctx)
     :location (source:location pred)))

  (:method ((qual-ty qualified-ty) ctx)
    (declare (type algo:immutable-map ctx)
             (values qualified-ty))

    (make-qualified-ty
     :explicit-p (qualified-ty-explicit-p qual-ty)
     :explicit-variables (rename-type-variables-generic% (qualified-ty-explicit-variables qual-ty) ctx)
     :predicates (rename-type-variables-generic% (qualified-ty-predicates qual-ty) ctx)
     :type (rename-type-variables-generic% (qualified-ty-type qual-ty) ctx)
     :location (source:location qual-ty)))

  (:method ((ctor constructor) ctx)
    (declare (type algo:immutable-map ctx)
             (values constructor))
    (make-constructor
     :name (constructor-name ctor)
     :signature (rename-type-variables-generic% (constructor-signature ctor) ctx)
     :fields (rename-type-variables-generic% (constructor-fields ctor) ctx)
     :docstring (source:docstring ctor)
     :location (source:location ctor)))

  (:method ((keyword keyword-src) ctx)
    (declare (type algo:immutable-map ctx)
             (values keyword-src))

    (let ((new-name (algo:immutable-map-lookup ctx (keyword-src-name keyword))))

      (if new-name
          (make-keyword-src
           :name new-name
           :source-name (keyword-src-source-name keyword)
           :location (source:location keyword))
          keyword)))

  (:method ((toplevel toplevel-define-type) ctx)
    (declare (type algo:immutable-map ctx)
             (values toplevel-define-type))

    (let* ((tvars (mapcar #'keyword-src-name (toplevel-define-type-vars toplevel)))

           (new-bindings (make-local-vars tvars :package util:+keyword-package+))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (make-toplevel-define-type
       :name (toplevel-define-type-name toplevel)
       :vars (rename-type-variables-generic% (toplevel-define-type-vars toplevel) new-ctx)
       :docstring (source:docstring toplevel)
       :ctors (rename-type-variables-generic% (toplevel-define-type-ctors toplevel) new-ctx)
       :location (source:location toplevel)
       :repr (toplevel-define-type-repr toplevel)
       :derive (toplevel-define-type-derive toplevel)
       :head-location (toplevel-define-type-head-location toplevel)
       :exception-p (toplevel-define-type-exception-p toplevel)
       :resumption-p (toplevel-define-type-resumption-p toplevel))))

  (:method ((toplevel toplevel-define-type-alias) ctx)
    (declare (type algo:immutable-map ctx)
             (values toplevel-define-type-alias))

    (let* ((tvars (mapcar #'keyword-src-name (toplevel-define-type-alias-vars toplevel)))

           (new-bindings (make-local-vars tvars :package util:+keyword-package+))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (make-toplevel-define-type-alias
       :name (toplevel-define-type-alias-name toplevel)
       :vars (rename-type-variables-generic% (toplevel-define-type-alias-vars toplevel) new-ctx)
       :docstring (source:docstring toplevel)
       :type (rename-type-variables-generic% (toplevel-define-type-alias-type toplevel) new-ctx)
       :location (source:location toplevel)
       :head-location (toplevel-define-type-alias-head-location toplevel))))

  (:method ((field struct-field) ctx)
    (declare (type algo:immutable-map ctx)
             (values struct-field))

    (make-struct-field
     :name (struct-field-name field)
     :type (rename-type-variables-generic% (struct-field-type field) ctx)
     :docstring (source:docstring field)
     :location (source:location field)))

  (:method ((toplevel toplevel-define-struct) ctx)
    (declare (type algo:immutable-map ctx)
             (values toplevel-define-struct))

    (let* ((tvars (mapcar #'keyword-src-name (toplevel-define-struct-vars toplevel)))

           (new-bindings (make-local-vars tvars :package util:+keyword-package+))

           (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

      (make-toplevel-define-struct
       :name (toplevel-define-struct-name toplevel)
       :vars (rename-type-variables-generic% (toplevel-define-struct-vars toplevel) new-ctx)
       :docstring (source:docstring toplevel)
       :fields (rename-type-variables-generic% (toplevel-define-struct-fields toplevel) new-ctx)
       :location (source:location toplevel)
       :repr (toplevel-define-struct-repr toplevel)
       :derive (toplevel-define-struct-derive toplevel)
       :head-location (toplevel-define-struct-head-location toplevel))))

  (:method ((fundep fundep) ctx)
    (declare (type algo:immutable-map ctx)
             (values fundep))

    (make-fundep
     :left (rename-type-variables-generic% (fundep-left fundep) ctx)
     :right (rename-type-variables-generic% (fundep-right fundep) ctx)
     :location (source:location fundep)))

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
       :docstring (source:docstring method)
       :location (source:location method))))

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
       :docstring (source:docstring toplevel)
       :methods (rename-type-variables-generic% (toplevel-define-class-methods toplevel) new-ctx)
       :location (source:location toplevel)
       :head-location (toplevel-define-class-head-location toplevel))))

  (:method ((list list) ctx)
    (mapcar
     (lambda (ty)
       (rename-type-variables-generic% ty ctx))
     list)))
