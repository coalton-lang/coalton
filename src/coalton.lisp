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
                                           coalton:define-instance))

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
        (definstances nil))
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
                     (nreverse definstances)))

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
  `(let ((old ,*emit-type-annotations*))
    (setf *emit-type-annotations* nil)
    (let ((form (process-coalton-toplevel ',toplevel-forms *global-environment*)))
      (setf *emit-type-annotations* old)
      form)))

(defun process-coalton-toplevel (toplevel-forms &optional (env *global-environment*))
  "Top-level definitions for use within Coalton."

  (multiple-value-bind (type-defines declares defines class-defines instance-defines)
      (collect-toplevel-forms toplevel-forms)

    (multiple-value-bind (defined-types env type-docstrings)
	(process-toplevel-type-definitions type-defines env)

      ;; Class definitions must be checked after types are defined
      ;; but before values are typechecked.

      (multiple-value-bind (classes env)
	  (process-toplevel-class-definitions class-defines env)

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
