;;;; toplevel-define.lisp

(in-package #:coalton-impl)

;;; Handling of top-level COALTON:DEFINE.



(defun process-toplevel-value-definitions (def-forms declared-types package env)
  "Parse all coalton DEFINE forms in DEF-FORMS, optionally with declared types

Returns new environment, binding list of declared nodes, and a DAG of dependencies"
  (declare (type package package)
           (values environment typed-binding-list list list))

  (let* ((docstrings nil)
         (parsed (loop :for form :in def-forms
                       :collect (multiple-value-bind (name node docstring)
                                    (tc:parse-define-form form package env)
                                  (push (list name docstring) docstrings)
                                  (cons name node))))
         (expl-names (alexandria:hash-table-keys declared-types))
         (impl-bindings nil)
         (expl-bindings nil)
         (name-table (make-hash-table)))

    (coalton-impl/typechecker::with-type-context ("COALTON-TOPLEVEL")
      (loop :for (name . node) :in parsed
            :do (progn
                  (when (gethash name name-table)
                    (error 'duplicate-definition
                           :name name))
                  (setf (gethash name name-table) t))))

    ;; Sort our bindings into implicit and explicit
    (loop :for binding :in parsed
          :do
             (if (member (car binding) expl-names :test #'eql)
                 (push binding expl-bindings)
                 (push binding impl-bindings)))

    ;; Assert that there are no orphan declares
    (loop :for name :in expl-names :do
      (assert (member name expl-bindings :key #'car)
          () "Orphan type declaration for variable ~A" name))

    (coalton-impl/typechecker::with-type-context ("COALTON-TOPLEVEL")
      (multiple-value-bind (typed-bindings preds new-env subs returns)
          (coalton-impl/typechecker::derive-bindings-type
           impl-bindings expl-bindings declared-types env nil nil
           :allow-deferred-predicates nil
           :allow-returns nil)
        (when preds
          (coalton-bug "Preds not expected. ~A" preds))
        (when returns
          (coalton-bug "Returns not expected. ~A" returns))

        ;; Apply output substitutions
        (setf typed-bindings
              (mapcar (lambda (binding)
                        (cons
                         (car binding)
                         (coalton-impl/typechecker::apply-substitution subs (cdr binding))))
                      typed-bindings))

        ;; Update the current environment with any updated types
        (setf env (coalton-impl/typechecker::apply-substitution subs new-env))

        (loop :for (name . node) :in typed-bindings :do
          (progn
            (setf env
                  (set-name env name
                            (make-name-entry :name name
                                             :type :value
                                             :docstring (second (find name docstrings :key #'car))
                                             :location (or *compile-file-pathname* *load-truename*))))
            (when (typed-node-abstraction-p node)
              ;; for functions, stash the original parameter names so documentation generation can get at them
              (setf env
                    (set-function-source-parameter-names env
                                                         name
                                                         (typed-node-abstraction-source-parameter-names node))))))

        (values
         env
         typed-bindings)))))
