(in-package #:coalton-impl/codegen)

;;;
;;; Lisp types for coalton types
;;;

(defgeneric lisp-type (ty)
  (:documentation "Returns the corresponding lisp type for the type of the given node")

  (:method ((ty coalton-impl/typechecker::tvar))
    ;; Since lisp does not have the notion of type variables, emit a top type
    't)

  (:method ((ty coalton-impl/typechecker::tcon))
    ;; Here, we will (for now) make a mapping from early types to lisp types.
    (let ((tcon-name (coalton-impl/typechecker::tycon-name (coalton-impl/typechecker::tcon-tycon ty))))
      (cond
        ((eql tcon-name 'coalton:Void)         'nil)
        ((eql tcon-name 'coalton:Char)         'character)
        ((eql tcon-name 'coalton:I32)          '(signed-byte 32))
        ((eql tcon-name 'coalton:I64)          '(signed-byte 64))
        ((eql tcon-name 'coalton:U8)           '(unsigned-byte 8))
        ((eql tcon-name 'coalton:U32)          '(unsigned-byte 32))
        ((eql tcon-name 'coalton:U64)          '(unsigned-byte 64))
        ((eql tcon-name 'coalton:Integer)      'integer)
        ((eql tcon-name 'coalton:Single-Float) 'single-float)
        ((eql tcon-name 'coalton:Double-Float) 'double-float)

        ((eql tcon-name 'coalton:String)       'simple-string)

        ;; LISP-OBJECT maps with this last case.
        (t tcon-name))))

  (:method ((ty coalton-impl/typechecker::tapp))
    (cond
      ;; If we are a function, emit a function type
      ((function-type-p ty)
       `(or function-entry (function (,(lisp-type (function-type-from ty)))
                                     ,(lisp-type (function-type-to ty)))))

      ;; Otherwise, emit the applied type
      (t
       ;; Our underlying representation of types does not have any
       ;; parameterization on coalton type paramters so we can just
       ;; recurse down to the base tycon.
       (lisp-type (coalton-impl/typechecker::tapp-from ty)))))

  (:method ((ty ty))
    (error "Unable to produce lisp type for ~A" ty))

  ;; Allow for calling with other types
  (:method ((ty ty-scheme))
    (lisp-type (coalton-impl/typechecker::fresh-inst ty)))
  (:method ((ty qualified-ty))
    (lisp-type (coalton-impl/typechecker::qualified-ty-type ty)))
  (:method ((ty typed-node))
    (lisp-type (typed-node-type ty))))

