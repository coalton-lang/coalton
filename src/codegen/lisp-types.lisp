(in-package #:coalton-impl/codegen)

;;;
;;; Lisp types for coalton types
;;;

(defgeneric lisp-type (ty env)
  (:documentation "Returns the corresponding lisp type for the type of the given node")

  (:method ((ty coalton-impl/typechecker::tvar) env)
    (declare (ignore env))
    ;; Since lisp does not have the notion of type variables, emit a top type
    't)

  (:method ((ty coalton-impl/typechecker::tcon) env)
    (let*
        ((tcon-name
           (coalton-impl/typechecker::tycon-name
            (coalton-impl/typechecker::tcon-tycon ty)))
         (type-entry (coalton-impl/typechecker::lookup-type env tcon-name)))

      ;; If the type is a newtype then resolve the type runtime type
      (if (type-entry-newtype type-entry)
          (lisp-type (type-entry-runtime-type type-entry) env)
          (type-entry-runtime-type type-entry))))

  (:method ((ty coalton-impl/typechecker::tapp) env)
    (cond
      ;; If we are a function, emit a function type
      ((function-type-p ty)
       `(or function-entry (function (,(lisp-type (function-type-from ty) env))
                                     ,(lisp-type (function-type-to ty) env))))

      ;; Otherwise, emit the applied type
      (t
       ;; Our underlying representation of types does not have any
       ;; parameterization on coalton type paramters so we can just
       ;; recurse down to the base tycon.
       (lisp-type (coalton-impl/typechecker::tapp-from ty) env))))

  (:method ((ty ty) env)
    (declare (ignore env))
    (coalton-impl::coalton-bug "Unable to produce lisp type for ~A" ty))

  ;; Allow for calling with other types
  (:method ((ty ty-scheme) env)
    (lisp-type (coalton-impl/typechecker::fresh-inst ty) env))
  (:method ((ty qualified-ty) env)
    (lisp-type (coalton-impl/typechecker::qualified-ty-type ty) env))
  (:method ((ty typed-node) env)
    (lisp-type (typed-node-type ty) env)))

