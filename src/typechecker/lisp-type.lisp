(defpackage #:coalton-impl/typechecker/lisp-type
  (:use
   #:cl
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/predicate
   #:coalton-impl/typechecker/scheme
   #:coalton-impl/typechecker/environment)
  (:import-from
   #:coalton-impl/runtime
   #:function-entry)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:lisp-type))

(in-package #:coalton-impl/typechecker/lisp-type)

;;;
;;; Lisp types for coalton types
;;;

(defgeneric lisp-type (ty env)
  (:documentation "Returns the corresponding lisp type for the type of the given node.

USE-FUNCTION-ENTRIES specifies whether to emit FUNCTION-ENTRY for functions, emitting FUNCTION when NIL. Defaults to T.")

  (:method ((ty tyvar) env)
    (declare (ignore env))
    ;; Since lisp does not have the notion of type variables, emit a top type
    't)

  (:method ((ty tycon) env)
    (declare (ignore))
    (let*
        ((tcon-name (tycon-name ty))
         (type-entry (lookup-type env tcon-name :no-error t)))

      (cond
        ;; If the type is unknown then assume it exists at runtime
        ((null type-entry)
         tcon-name)

        (t
         (type-entry-runtime-type type-entry)))))

  (:method ((ty tapp) env)
    (cond
      ;; If we are a function, emit a function type
      ((function-type-p ty)
       'function-entry)

      ;; Otherwise, emit the applied type
      (t
       ;; Our underlying representation of types does not have any
       ;; parameterization on coalton type paramters so we can just
       ;; recurse down to the base tycon.
       (lisp-type (tapp-from ty) env))))

  (:method ((ty ty) env)
    (declare (ignore env))
    (util:coalton-bug "Unable to produce lisp type for ~A" ty))

  ;; Allow for calling with other types
  (:method ((ty ty-scheme) env)
    (lisp-type (fresh-inst ty) env))
  (:method ((ty qualified-ty) env)
    (lisp-type (qualified-ty-type ty) env)))

