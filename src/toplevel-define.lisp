;;;; toplevel-define.lisp

(in-package #:coalton-impl)

;;; Handling of top-level COALTON:DEFINE.

(defun parse-define-form (form)
  "Parse a COALTON:DEFINE form."
  (declare (type list form)
           (values symbol node symbol list (or null string) &optional))
  (assert (and (eql (first form) 'coalton:define)
               (or (= 3 (length form))   ; Without docstring
                   (= 4 (length form)))) ; With docstring
          () "Malformed DEFINE form ~A" form)
  ;; Defines either define a value or a function. Values and functions
  ;; in Coalton occupy the namespace, but the intent of the user can
  ;; be distinguished. A definition either looks like:
  ;;
  ;;     (DEFINE <var> <val>)
  ;;
  ;; or
  ;;
  ;;     (DEFINE (<fvar> <arg>*) <val>)
  ;;
  ;; The former defines a variable, the latter defines a function.
  (let* ((parse-docstring? (= 4 (length form)))
         (var-thing (second form))
         (val (if parse-docstring?
                  (fourth form)
                  (third form)))
         (docstring (when parse-docstring?
                      (third form))))
    (cond
      ((null var-thing)
       (error-parsing form "Found a null value where a symbol or function ~
                            was expected."))
      ((symbolp var-thing)
       (parse-define-form-variable var-thing val docstring))
      ((and (listp var-thing)
            (every #'symbolp var-thing))
       ;; Disallow zero arity functions
       (unless (>= (length var-thing) 2)
         (error-parsing form "Unable to define function with arity ~A" (1- (length var-thing))))
       (parse-define-form-function (first var-thing) (rest var-thing) val docstring))
      (t
       (error-parsing form "Invalid second argument.")))))

(defun parse-define-form-variable (var val docstring)
  (declare (type symbol var)
           (type t val)
           (values symbol node symbol list (or null string)))
  ;; The (DEFINE <var> <val>) case.
  ;; XXX: Should this be LETREC too? Probably for something like F = x => ... F.
  (values var
          (parse-form val (make-shadow-realm) (symbol-package var))
          ':variable
          nil
          docstring))

(defun parse-define-form-function (fvar args val docstring)
  (declare (type symbol fvar)
           (type list args)
           (type t val)
           (values symbol node symbol list (or null string)))
  ;; The (DEFINE (<fvar> . <args>) <val>) case.
  (values fvar
          (parse-form `(coalton:fn ,args ,val) (make-shadow-realm) (symbol-package fvar))
          ':function
          args
          docstring))

(defun process-toplevel-value-definitions (def-forms declared-types env)
  "Parse all coalton DEFINE forms in DEF-FORMS, optionally with declared types

Returns new environment, binding list of declared nodes, a DAG of dependencies, and docstring alist"
  (declare (values environment typed-binding-list list list))

  (let* ((docstrings nil)
         (parsed (loop :for form :in def-forms
                       :collect (multiple-value-bind (name node type args docstring)
                                    (parse-define-form form)
                                  (push (list name docstring type) docstrings)
                                  (list name node type args))))
         (expl-names (alexandria:hash-table-keys declared-types))
         (impl-bindings nil)
         (expl-bindings nil))

    ;; Sort our bindings into implicit and explicit
    (loop :for binding :in parsed
          :do
             (if (member (car binding) expl-names :test #'eql)
                 (push (cons (first binding) (second binding)) expl-bindings)
                 (push (cons (first binding) (second binding)) impl-bindings)))

    ;; Assert that there are no orphan declares
    (mapcar (lambda (name)
              (assert (member name expl-bindings :key #'car)
                      () "Orphan type declaration for variable ~A" name))
            expl-names)

    (coalton-impl/typechecker::with-type-context ("COALTON-TOPLEVEL")
      (multiple-value-bind (typed-bindings preds new-env subs)
          (coalton-impl/typechecker::derive-bindings-type
           impl-bindings expl-bindings declared-types env nil nil)
        (when preds
          (error "Preds not expected. ~A" preds))

        (setf typed-bindings
              (mapcar (lambda (binding)
                        (cons
                         (car binding)
                         (coalton-impl/typechecker::apply-substitution subs (cdr binding))))
                      typed-bindings))

        (setf env (coalton-impl/typechecker::apply-substitution subs new-env))

        (loop :for (name . node) :in typed-bindings :do
          (setf env (set-name env name (make-name-entry :name name
                                                        :type :value
                                                        :docstring (second (find name docstrings :key #'car))
                                                        :location (or *compile-file-pathname* *load-truename*)))))

        (values
         env
         typed-bindings
         (reverse
          (tarjan-scc (bindings-to-dag (append impl-bindings expl-bindings))))
         docstrings)))))
