(in-package #:coalton-impl)

;;; # Compiler
;;;
;;; The compiler is a combination of a code analyzer and code
;;; generator. The main analysis to be done is type checking. The code
;;; generator just generates valid Common Lisp, to be further
;;; processed by the Common Lisp compiler. Generally, this code will
;;; be generated at macroexpansion time of the ambient Common Lisp
;;; compiler. See the COALTON macro.

(define-global-var **repr-specifiers** '(:lisp :transparent :native :enum)
  "(repr ...) specifiers that the compiler is known to understand.")

(defmacro install-operator-metadata (&rest directives)
  "Associate metadata with symbols as described by DIRECTIVES.

For each directive of the form
  (SYMBOL PROPERTY*)
insert each PROPERTY into the symbol-plist of SYMBOL. A property can be given as
either (INDICATOR VALUE) or just INDICATOR; the short form means (INDICATOR T)."
  `(dolist (directive ',directives)
     (let ((symbol (car directive)))
       (dolist (property (cdr directive))
         (let* ((indicator (if (listp property) (car property) property))
                (value (if (listp property) (cadr property) t)))
           ;; Set properties individually instead of appending, so that using
           ;; the macro twice on the same operator does the reasonable thing.
           (setf (get symbol indicator) value))))))

(install-operator-metadata
 (coalton:coalton-toplevel  :toplevel-container)

 (coalton:declare           :toplevel)
 (coalton:define            :toplevel)
 (coalton:define-type       :toplevel)
 (coalton:define-class      :toplevel)
 (coalton:define-instance   :toplevel)

 (coalton:repr              :toplevel
                            (:must-precede-one-of (coalton:define-type)))

 (coalton:monomorphize      :toplevel
                            (:must-precede-one-of (coalton:declare
                                                   coalton:define)))
 (coalton:specialize :toplevel))

;;; Entry Point

(defun collect-toplevel-forms (forms)
  "Return an organized representation of FORMS, a sequence of toplevel forms.

Signal an error if FORMS is not a valid container for toplevel forms, or if any
subform of FORMS is not a valid toplevel form.

The return value is a plist containing (1) a hash table of reprs associated with
types defined in FORMS; (2) for every toplevel operator, a list of the subforms
in FORMS that begin with that operator."
  (let ((plist
          (list 'repr-table (make-hash-table)
                'attr-table (make-hash-table))))
    (labels
        ((operator (form)
           (handler-case
               (prog1
                   (car form)
                 (assert (symbolp (car form))))
             (type-error () (error-parsing form "Non-list form at toplevel"))
             (simple-error () (error-parsing form "A toplevel form must begin ~
                                                   with a symbol."))))
         (establish-repr (specifier type arg)
           (unless (member specifier **repr-specifiers**)
             (error
              "The compiler does not understand (repr ~S)."
              specifier))

           (when (listp type)
             (setf type (car type)))

           (setf (gethash type (getf plist 'repr-table)) (cons specifier arg)))

         (handle-monomorphize (definition-name)
           (push :monomorphize (gethash definition-name (getf plist 'attr-table))))

         (walk (forms)
           (loop
             :until (null forms)
             :for form := (pop forms)
             :for next-form := (first forms)
             :for op := (operator form)
             :for must-precede-list := (get op :must-precede-one-of)

             :unless (get op :toplevel)
               :do (error-parsing form
                                  "The form (~A ...) is not valid at toplevel."
                                  op)

             :when must-precede-list
               :unless (member (operator next-form) must-precede-list)
                 :do (error-parsing form
                                    "The ~A form must precede one of: ~
                                     ~{~A~^, ~}."
                                    op must-precede-list)

             :do (push form (getf plist op))
                 ;; Specific behaviors for particular operators
                 (case op
                   (coalton:repr
                    (unless (or (= (length form) 2) (= 3 (length form)))
                      (error-parsing form "Wrong number of arguments"))

                    (when (and (not (eq (cadr form) :native)) (caddr form))
                      (error-parsing form "Wrong number of arguments"))

                    (establish-repr (cadr form) (cadr next-form) (caddr form)))

                   (coalton:monomorphize
                    (unless (= (length form) 1)
                      (error-parsing form "Wrong number of arguments"))

                    (if (listp (cadr next-form))
                        (handle-monomorphize (first (cadr next-form)))
                        (handle-monomorphize (cadr next-form))))))))
      ;; Populate PLIST...
      (walk forms)
      ;; ...and return it, with its values reversed to reflect the order that
      ;; the forms appeared.
      (mapcar (lambda (element)
                (if (listp element)
                    (nreverse element)
                    element))
              plist))))

(defparameter *global-environment* (make-default-environment))


;;; Coalton Macros
(defmacro coalton:coalton-toplevel (&body toplevel-forms)
  "Top-level definitions for use within Coalton."
  (multiple-value-bind (form env)
      (process-coalton-toplevel toplevel-forms *package* *global-environment*)
    (setf *global-environment* env)
    form))

(defmacro coalton:coalton-codegen (&body toplevel-forms)
  "Returns the lisp code generated from coalton code. Intended for debugging."
  `(let ((*emit-type-annotations* nil)
         (*coalton-skip-update* t))
     (process-coalton-toplevel ',toplevel-forms *package* *global-environment*)))

(defmacro coalton:coalton-codegen-types (&body toplevel-forms)
  "Returns the lisp code generated from coalton code with lisp type annotations. Intended for debugging."
  `(let ((*emit-type-annotations* t))
     (process-coalton-toplevel ',toplevel-forms *package* *global-environment*)))

(defmacro coalton:coalton-codegen-ast (&body toplevel-forms)
  "Prints the AST of the typechecked coalton code. Intended for debugging."
  `(let ((*coalton-dump-ast* t))
     (process-coalton-toplevel ',toplevel-forms *package* *global-environment*)
     (values)))

(defmacro coalton:coalton (form)
  (let ((parsed-form (parse-form form (make-immutable-map) *package*)))
    (coalton-impl/typechecker::with-type-context ("COALTON")
      (multiple-value-bind (type preds typed-node substs)
          (derive-expression-type parsed-form *global-environment* nil)

        (let* ((env (coalton-impl/typechecker::apply-substitution substs *global-environment*))
               (preds (coalton-impl/typechecker::reduce-context env preds substs))
               (substs (coalton-impl/typechecker::pred-defaults preds substs))
               (preds (coalton-impl/typechecker::reduce-context env preds substs))
               (typed-node (coalton-impl/typechecker::remove-static-preds
                            (coalton-impl/typechecker::apply-substitution substs typed-node)))
               (type (coalton-impl/typechecker::apply-substitution substs type))
               (qual-type (coalton-impl/typechecker::qualify preds type))
               (scheme (coalton-impl/typechecker::quantify (coalton-impl/typechecker::type-variables qual-type) qual-type)))

          (cond
            ((null preds)
             (setf *global-environment* env)
             (values
              (coalton-impl/codegen::codegen-expression
               (coalton-impl/codegen::optimize-node
                (coalton-impl/codegen::compile-expression
                 typed-node
                 nil
                 *global-environment*)
                *global-environment*)
               nil
               *global-environment*)))

            (t
             (coalton-impl/typechecker::with-pprint-variable-context ()
               (let* ((tvars (loop :for i :to (1- (length (remove-duplicates (coalton-impl/typechecker::type-variables qual-type)
                                                                             :test #'equalp)))
                                   :collect (coalton-impl/typechecker::make-variable)))
                      (qual-type (coalton-impl/typechecker::instantiate
                                  tvars
                                  (coalton-impl/typechecker::ty-scheme-type scheme))))
                 (warn "The expression ~A~%    of type ~A~{ ~A~}. ~A => ~A~%    has unresolved constraint~A ~A~%    add a type assertion with THE to resolve it"
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
             ''coalton::unable-to-codegen)))))))

(defun process-coalton-toplevel (toplevel-forms package &optional (env *global-environment*))
  "Top-level definitions for use within Coalton."
  (declare (type package package)
           (values t environment))
  (destructuring-bind (&key
                         ((coalton:declare declares))
                         ((coalton:define defines))
                         ((coalton:define-type type-defines))
                         ((coalton:define-class class-defines))
                         ((coalton:define-instance instance-defines))
                         ((repr-table repr-table))
                         ((attr-table attr-table))
                         ((coalton:specialize specializations))
                       &allow-other-keys)
      (collect-toplevel-forms toplevel-forms)

    (multiple-value-bind (defined-types env)
        (process-toplevel-type-definitions type-defines repr-table env)

      ;; Class definitions must be checked after types are defined
      ;; but before values are typechecked.

      (multiple-value-bind (classes env)
          (parse-class-definitions class-defines env)

        ;; Methods need to be added to the environment before we can
        ;; check value types.
        (setf env (predeclare-toplevel-instance-definitions instance-defines package env))

        (let ((declared-types (process-toplevel-declarations declares env)))
          (multiple-value-bind (env toplevel-bindings)
              (process-toplevel-value-definitions defines declared-types package env)

            ;; Methods must be typechecked after the types of values
            ;; are determined since instances may reference them.
            (let ((instance-definitions
                    (process-toplevel-instance-definitions
                     instance-defines
                     package
                     env)))

              (multiple-value-bind (specializations env)
                  (process-toplevel-specializations specializations env)


                (let ((translation-unit
                        (make-translation-unit
                         :types defined-types
                         :definitions toplevel-bindings
                         :instances instance-definitions
                         :classes classes
                         :attr-table (or attr-table (make-hash-table)) ; "fix" style warning
                         :package package
                         :specializations specializations)))

                  (multiple-value-bind (program env)
                      (coalton-impl/codegen:compile-translation-unit
                       translation-unit
                       env)

                    (values
                     (if *coalton-skip-update*
                         program
                         `(progn
                            (eval-when (:load-toplevel)
                              (unless (eq (coalton-release-p) ,(coalton-release-p))
                                ,(if (coalton-release-p)
                                     `(error "~A was compiled in release mode but loaded in development." ,(or *compile-file-pathname* *load-truename*))
                                     `(error "~A was compiled in development mode but loaded in release." ,(or *compile-file-pathname* *load-truename*)))))
                            ,(coalton-impl/typechecker::generate-diff
                              translation-unit
                              env
                              '*global-environment*)
                            ,program))
                     env)))))))))))
