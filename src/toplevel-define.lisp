;;;; toplevel-define.lisp

(in-package #:coalton-impl)

;;; Handling of top-level COALTON:DEFINE.

(defun parse-define-form (form package env &key (skip-inherited-symbol-checks nil))
  "Parse a COALTON:DEFINE form."
  (declare (type list form)
           (type package package)
           (type environment env)
           (values symbol node (or null string) &optional))
  (unless (and (eql (first form) 'coalton:define)
               (or (<= 3 (length form))   ; Without docstring
                    )) ; With docstring
    (error-parsing form "malformed DEFINE form"))

  ;; Defines either define a value or a function. Values and functions
  ;; in Coalton occupy the namespace, but the intent of the user can
  ;; be distinguished. A definition either looks like:
  ;;
  ;;     (DEFINE <var> <val>)
  ;;
  ;; or
  ;;
  ;;     (DEFINE (<fvar> <arg>*) <val>+)
  ;;
  ;; The former defines a variable, the latter defines a function.

  (let ((docstring nil)
        (var-thing (second form)))

    ;; Grab the docstring if it exists
    (when (and (> (length form) 3) (typep (third form) 'string))
      (setf docstring (third form)))


    (cond
      ;; (define () 5) is invalid
      ((null var-thing)
       (error-parsing form "Found a null value where a symbol or function was expected"))


    ;; Parse a variable declaration
    ((symbolp var-thing)
     (parse-define-form-variable
      var-thing
      (if docstring
          (fourth form)
          (third form))
      docstring
      package
      env
      :skip-inherited-symbol-checks skip-inherited-symbol-checks))

    ((and (listp var-thing)
          (every #'symbolp var-thing))
     (parse-define-form-function
      (first var-thing)
      (rest var-thing)
      (if docstring
          (nthcdr 3 form)
          (nthcdr 2 form))
      docstring
      package
      env
      :skip-inherited-symbol-checks skip-inherited-symbol-checks))

    (t
     (error-parsing form "Invalid define form.")))))

(defun parse-define-form-variable (var val docstring package env &key (skip-inherited-symbol-checks nil))
  (declare (type symbol var)
           (type t val)
           (type package package)
           (type environment env)
           (ignore env)
           (values symbol node (or null string)))
  ;; The (DEFINE <var> <val>) case.
  ;; XXX: Should this be LETREC too? Probably for something like F = x => ... F.
  (unless (or skip-inherited-symbol-checks (equalp package (symbol-package var)))
    (error-inherited-symbol
     var
     package))
  (values var
          (parse-form val (make-immutable-map) package)
          docstring))

(defun parse-define-form-function (fvar args forms docstring package env &key (skip-inherited-symbol-checks nil))
  (declare (type symbol fvar)
           (type list args)
           (type list forms)
           (type package package)
           (type environment env)
           (ignore env)
           (values symbol node (or null string)))
  ;; The (DEFINE (<fvar> <arg>*) <val>+) case.
  (unless (or skip-inherited-symbol-checks (equalp package (symbol-package fvar)))
    (error-inherited-symbol
     fvar
     package))
  (values fvar
          (parse-form `(coalton:fn ,args ,@forms) (make-immutable-map) package)
          docstring))

(defun process-toplevel-value-definitions (def-forms declared-types package env)
  "Parse all coalton DEFINE forms in DEF-FORMS, optionally with declared types

Returns new environment, binding list of declared nodes, and a DAG of dependencies"
  (declare (type package package)
           (values environment typed-binding-list list list))

  (let* ((docstrings nil)
         (parsed (loop :for form :in def-forms
                       :collect (multiple-value-bind (name node docstring)
                                    (parse-define-form form package env)
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
           :disable-monomorphism-restriction t
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

        ;; Checks for monomorphism restriction for top level bindings
        (dolist (b typed-bindings)
          (with-type-context ("definition of ~A" (car b))
            (let* ((type (coalton-impl/typechecker::fresh-inst (lookup-value-type env (car b))))

                   (preds (reduce-context env (coalton-impl/typechecker::qualified-ty-predicates type) subs
                                          :allow-deferred-predicates nil)))
              (when (and (not (gethash (car b) declared-types))
                         (not (coalton-impl/typechecker::typed-node-abstraction-p (cdr b)))
                         (not (null preds)))
                (error 'toplevel-monomorphism-restriction
                       :type type
                       :name (car b))))))

        (loop :for (name . node) :in typed-bindings :do
          (progn
            (setf env
                  (set-name env name
                            (make-name-entry
                             :name name
                             :type :value
                             :docstring (second (find name docstrings :key #'car))
                             :location (or *compile-file-pathname* *load-truename*))))))

        (values
         env
         typed-bindings)))))
