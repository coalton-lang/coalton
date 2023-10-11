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

(defun lisp-type= (x y)
  (and (subtypep x y)
       (subtypep y x)))

;;; This is a list of Coalton tycon names that (1) have a finite
;;; number of specialized representations, and (2) won't yet exist
;;; when this file is first compiled.

;;; Complex
(defun complex-tycon-p (tycon)
  (if (find-package "COALTON-LIBRARY/MATH/COMPLEX")
      (eq tycon (find-symbol "COMPLEX" "COALTON-LIBRARY/MATH/COMPLEX"))
      nil))

(defvar *specialized-complex-part-types-considered*
  '(cl:single-float
    cl:double-float))

;;; Simple Arrays
(defun lisparray-tycon-p (tycon)
  (if (find-package "COALTON-LIBRARY/LISPARRAY")
      (eq tycon (find-symbol "LISPARRAY" "COALTON-LIBRARY/LISPARRAY"))
      nil))


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

      ((typep (tapp-from ty) 'tycon)
       (let ((from (tycon-name (tapp-from ty)))
             (to   (tapp-to ty)))
         (cond
           ;; First we deal with specialized parametric types.
           ;;
           ;; (Complex :t)
           ((complex-tycon-p from)
            (let ((lisp-to (lisp-type to env)))
              (cond
                ((member lisp-to
                         *specialized-complex-part-types-considered*
                         :test #'lisp-type=)
                 `(cl:complex ,lisp-to))
                (t
                 ;; We can use FROM directly since it'll be the name
                 ;; of the Complex tycon, which matches the Lisp type
                 ;; exactly.
                 `(cl:or cl:number ,from)))))

           ;; (LispArray :t)
           ((lisparray-tycon-p from)
            ;; Subtle observation: We don't want to recursively call
            ;; LISP-TYPE on a TYVAR, because that will resolve to
            ;; `cl:t`, which isn't the correct type of
            ;; arbitrarily-typed arrays.
            (if (typep to 'tyvar)
                `(cl:simple-array cl:* (cl:*))
                `(cl:simple-array ,(lisp-type to env) (cl:*))))
           
           ;; Otherwise we fall back.
           (t
            (lisp-type (tapp-from ty) env)))))

      ;; Otherwise, emit the applied type
      (t
       ;; Our underlying representation of types does not have any
       ;; parameterization on coalton type paramters so we can just
       ;; recurse down to the base tycon.
       (lisp-type (tapp-from ty) env))))

  (:method ((ty ty) env)
    (declare (ignore env))
    (util:coalton-bug "Unable to produce lisp type for ~S" ty))

  ;; Allow for calling with other types
  (:method ((ty ty-scheme) env)
    (lisp-type (fresh-inst ty) env))
  (:method ((ty qualified-ty) env)
    (lisp-type (qualified-ty-type ty) env)))

