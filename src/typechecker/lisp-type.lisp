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
            (cond
              ((or
                ;; (LispArray :t)
                (typep to 'tyvar)
                ;; (LispArray (Complex :t))
                (and (typep to 'tapp)
                     (complex-tycon-p (tycon-name (tapp-from to)))
                     (typep (tapp-to to) 'tyvar)))
               `(cl:simple-array cl:* (cl:*)))
              (t
               `(cl:simple-array ,(lisp-type to env) (cl:*)))))

           ;; When FROM is a transparent type, and we got a specialized type,
           ;; we propagate parameterization to the inner type.
           ((and (not (typep to 'tyvar))
                 (try-recurse-transparent-type from to env)))


           ;; Otherwise we fall back.
           (t
            (lisp-type (tapp-from ty) env)))))

      ;; Otherwise, emit the applied type
      (t
       ;; Our underlying representation of types does not have any
       ;; parameterization on coalton type parameters so we can just
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


(defun try-recurse-transparent-type (from parameter env)
  "Called when taking a lisp-type of a parameterized type, where a
concrete type is given to the parameter.  For example, a type `(Foo
:t)` is defined, and we try to take a lisp-type of `(Foo UFix)`.  The
`FROM` argument gets the type `Foo` and the `parameter` argument gets
the type `UFix`.

There is a special opportunity of optimization when `Foo` is defined
as a transparent type, e.g. `(repr :transparent) (define-type (Foo
:t) (Content (Lisparray :t)))`.  In that case, `(Foo UFix)` can have a
Lisp type `(cl:simple-array cl:fixnum)`, which allows Lisp compiler to
optimize further.  We check the conditions and returns a specialized
Lisp type if possible.

If we can't get a specialized Lisp type, this returns nil, and let the
fallback branch take care of it.
"
  (labels ((transparent-type? (from)
             (alexandria:when-let (entry (lookup-type env from :no-error t))
               (eq (type-entry-explicit-repr entry) ':transparent)))
           (reveal-underlying-type (from)
             "If `from` type satisfies the optimizable condition, returns two
values: the 'inner' type (wrappee type) and the 'outer' type (wrapper
type)."
             (let ((v (lookup-value-type env from :no-error t)))
               (when (typep v 'ty-scheme)
                 (let ((qt (qualified-ty-type (ty-scheme-type v))))
                   ;; In transparent types, we have this in QT.
                   ;; (Arrow <innter-type>) -> <outer-type>
                   (when (function-type-p qt)
                     (values (function-type-from qt)
                             (function-type-to qt))))))))
    (and (transparent-type? from)
         (multiple-value-bind (inner outer) (reveal-underlying-type from)
           (cond ((and (typep inner 'tycon)
                       (typep outer 'tycon))
                  (lisp-type inner env))
                 ((and (typep inner 'tapp)
                       (typep outer 'tapp)
                       (typep (tapp-to inner) 'tgen)
                       (eq (tapp-to inner) (tapp-to outer)))
                  ;; Substitute inner type's type variable with the
                  ;; concrete type
                  (lisp-type (make-tapp :from (tapp-from inner)
                                        :to parameter)
                             env))
                 (t nil))))))
