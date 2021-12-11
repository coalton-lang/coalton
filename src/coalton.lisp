(in-package #:coalton-impl)

;;; # Compiler
;;;
;;; The compiler is a combination of a code analyzer and code
;;; generator. The main analysis to be done is type checking. The code
;;; generator just generates valid Common Lisp, to be further
;;; processed by the Common Lisp compiler. Generally, this code will
;;; be generated at macroexpansion time of the ambient Common Lisp
;;; compiler. See the COALTON macro.

(define-global-var **toplevel-operators** '(coalton:coalton-toplevel))
(define-global-var **special-operators** `(,@**toplevel-operators**
                                           coalton:define
                                           coalton:define-type
                                           coalton:declare
                                           coalton:define-class
                                           coalton:define-instance
                                           coalton:repr))


;;; Entry Point

(defun collect-toplevel-forms (forms)
  "Walk through the top-level forms and sort them out. Return three values:

    1. All DEFINE-TYPE forms

    2. All DECLARE forms

    3. All DEFINE forms

    4. All DEFINE-CLASS forms

    5. All DEFINE-INSTANCE forms
"
  (let ((deftypes nil)
        (declares nil)
        (defines nil)
        (defclasses nil)
        (definstances nil)
        (repr-table (make-hash-table)))
    (labels ((flatten (forms)
               (loop :for form :in forms
                     :append (cond
                               ((atom form) (list form))
                               ((member (first form) **toplevel-operators**)
                                (flatten (rest form)))
                               (t (list form)))))
             (walk (forms)
               (let ((next-form (first forms)))
                 (cond
                   ((endp forms)
                    (values
                     (nreverse deftypes)
                     (nreverse declares)
                     (nreverse defines)
                     (nreverse defclasses)
                     (nreverse definstances)
                     repr-table))

                   ((or (atom next-form)
                        (not (member (first next-form) **special-operators**)))
                    (error-parsing next-form "This can't show up at the top-level."))

                   ((eql 'coalton:define-type (first next-form))
                    (push next-form deftypes)
                    (walk (rest forms)))

                   ((eql 'coalton:declare (first next-form))
                    (push next-form declares)
                    (walk (rest forms)))

                   ((eql 'coalton:define (first next-form))
                    (push next-form defines)
                    (walk (rest forms)))

                   ((eql 'coalton:define-class (first next-form))
                    (push next-form defclasses)
                    (walk (rest forms)))

                   ((eql 'coalton:define-instance (first next-form))
                    (push next-form definstances)
                    (walk (rest forms)))

                   ((eql 'coalton:repr (first next-form))
                    ;; Repr must immediatly precede a type definition
                    (unless (eql 'coalton:define-type (first (second forms)))
                      (error-parsing next-form "Orphan repr instance."))

                    ;; Repr must have only two parts
                    (unless (= 2 (length next-form))
                      (error-parsing next-form "Invalid repr form."))

                    (let ((repr-dec (second next-form)))

                      (unless (eql :lisp repr-dec)
                        (error-parsing next-form "Unknown repr ~A." repr-dec))

                      (setf (gethash (second (second forms)) repr-table) repr-dec)
                      (walk (rest forms))))

                   (t
                    (assert nil () "Unreachable."))))))
      (walk (flatten forms)))))

(defparameter *global-environment* (make-default-environment))

(defparameter *initial-environment* nil)

;;; Coalton Macros
(defmacro coalton:coalton-toplevel (&body toplevel-forms)
  "Top-level definitions for use within Coalton."
  (multiple-value-bind (form env)
      (process-coalton-toplevel toplevel-forms *global-environment*)
    (setf *global-environment* env)
    form))

(defmacro coalton:coalton-codegen (&body toplevel-forms)
  "Returns the lisp code generated from coalton code. Intended for debugging."
  `(let ((*emit-type-annotations* nil))
     (process-coalton-toplevel ',toplevel-forms *global-environment*)))

(defmacro coalton:coalton-codegen-types (&body toplevel-forms)
  "Returns the lisp code generated from coalton code with lisp type annotations. Intended for debugging."
  `(let ((*emit-type-annotations* t))
     (process-coalton-toplevel ',toplevel-forms *global-environment*)))

(defmacro coalton:coalton (form)
  (let ((parsed-form (parse-form form (make-immutable-map) *package*)))
    (coalton-impl/typechecker::with-type-context ("COALTON")
      (multiple-value-bind (type preds typed-node substs)
          (derive-expression-type parsed-form *global-environment* nil)
        (let* ((env (coalton-impl/typechecker::apply-substitution substs *global-environment*))
               (preds (coalton-impl/typechecker::apply-substitution substs preds))
               (preds (coalton-impl/typechecker::reduce-context env preds substs))
               (typed-node (coalton-impl/typechecker::apply-substitution substs typed-node))
               (type (coalton-impl/typechecker::apply-substitution substs type))
               (qual-type (coalton-impl/typechecker::qualify preds type))
               (scheme (coalton-impl/typechecker::quantify (coalton-impl/typechecker::type-variables qual-type) qual-type)))

          (if (null preds)
              (progn
                (setf *global-environment* env)
                (coalton-impl/codegen::compile-expression typed-node nil *global-environment*))
              (progn
                (coalton-impl/typechecker::with-pprint-variable-context ()
                  (let* ((tvars (loop :for i :to (coalton-impl/typechecker::kind-arity
                                                     (coalton-impl/typechecker::kind-of type))
                                      :collect (coalton-impl/typechecker::make-variable)))
                         (qual-type (coalton-impl/typechecker::instantiate
                                     tvars
                                     (coalton-impl/typechecker::ty-scheme-type scheme))))
                    (format t "Expression ~A~%    of type ~A~{ ~A~}. ~A => ~A~%    has unresolved constraint~A ~A~%    add a type assertion with THE to resolve it"
                            form
                            (if *coalton-print-unicode*
                                "∀"
                                "FORALL")
                            tvars
                            (coalton-impl/typechecker::qualified-ty-predicates qual-type)
                            (coalton-impl/typechecker::qualified-ty-type qual-type)
                            (if (= (length (coalton-impl/typechecker::qualified-ty-predicates qual-type)) 1)
                                ""
                                "s")
                            (coalton-impl/typechecker::qualified-ty-predicates qual-type))))
                (values))))))))

(defun process-coalton-toplevel (toplevel-forms &optional (env *global-environment*))
  "Top-level definitions for use within Coalton."

  (multiple-value-bind (type-defines declares defines class-defines instance-defines repr-table)
      (collect-toplevel-forms toplevel-forms)

    (multiple-value-bind (defined-types env type-docstrings)
        (process-toplevel-type-definitions type-defines repr-table env)

      ;; Class definitions must be checked after types are defined
      ;; but before values are typechecked.

      (multiple-value-bind (classes env)
          (parse-class-definitions class-defines env)

        ;; Methods need to be added to the environment before we can
        ;; check value types.
        (setf env (predeclare-toplevel-instance-definitions instance-defines env))

        (let ((declared-types (process-toplevel-declarations declares env)))
          (multiple-value-bind (env toplevel-bindings dag value-docstrings)
              (process-toplevel-value-definitions defines declared-types env)

            ;; Methods must be typechecker after the types of values
            ;; are determined since instances may reference them.
            (let ((instance-definitions (process-toplevel-instance-definitions instance-defines env)))

              (let* ((env-diff (environment-diff env *global-environment*))
                     (env (update-function-env toplevel-bindings env))
                     (update (generate-environment-update
                              env-diff
                              '*global-environment*))
                     (program (codegen-program
                               defined-types
                               toplevel-bindings
                               dag
                               classes
                               instance-definitions
                               (append type-docstrings
                                       value-docstrings)
                               env)))
                (values
                 ;; Only generate an update block if there are environment updates
                 (if (not (equalp update `(eval-when (:load-toplevel))))
                     `(progn
                        ,update
                        ,program)
                     program)
                 env)))))))))
