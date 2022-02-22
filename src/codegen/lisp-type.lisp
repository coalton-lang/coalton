(defpackage #:coalton-impl/codegen/lisp-type
  (:use
   #:cl
   #:coalton-impl/util)
  (:import-from
   #:coalton-impl/codegen/function-entry
   #:function-entry)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)))

(in-package #:coalton-impl/codegen/lisp-type)

;;;
;;; Lisp types for coalton types
;;;

(defgeneric lisp-type (ty env)
  (:documentation "Returns the corresponding lisp type for the type of the given node")

  (:method ((ty tc:tvar) env)
    (declare (ignore env))
    ;; Since lisp does not have the notion of type variables, emit a top type
    't)

  (:method ((ty tc:tcon) env)
    (let*
        ((tcon-name (tc:tycon-name (tc:tcon-tycon ty)))
         (type-entry (tc:lookup-type env tcon-name :no-error t)))

      (cond
        ;; If the type is unknown then assume it exists at runtime
        ((null type-entry)
         tcon-name)

        ;; If the type is a newtype then resolve the type runtime type
        ((tc:type-entry-newtype type-entry)
         (lisp-type (tc:type-entry-runtime-type type-entry) env))

        (t
         (tc:type-entry-runtime-type type-entry)))))

  (:method ((ty tc:tapp) env)
    (cond
      ;; If we are a function, emit a function type
      ((tc:function-type-p ty)
       `(or function-entry (function (,(lisp-type (tc:function-type-from ty) env))
                                     ,(lisp-type (tc:function-type-to ty) env))))

      ;; Otherwise, emit the applied type
      (t
       ;; Our underlying representation of types does not have any
       ;; parameterization on coalton type paramters so we can just
       ;; recurse down to the base tycon.
       (lisp-type (coalton-impl/typechecker::tapp-from ty) env))))

  (:method ((ty tc:ty) env)
    (declare (ignore env))
    (coalton-impl::coalton-bug "Unable to produce lisp type for ~A" ty))

  ;; Allow for calling with other types
  (:method ((ty tc:ty-scheme) env)
    (lisp-type (tc:fresh-inst ty) env))
  (:method ((ty tc:qualified-ty) env)
    (lisp-type (tc:qualified-ty-type ty) env)))

