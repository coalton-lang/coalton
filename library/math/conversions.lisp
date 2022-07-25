;;;; conversions.lisp
;;;;
;;;; Conversions between primitive numerical types

(coalton-library/utils:defstdlib-package #:coalton-library/math/conversions
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/functions
   #:coalton-library/math/bounded)
  (:export
   #:integer->double-float
   #:integer->single-float))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(in-package #:coalton-library/math/conversions)

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:defparameter *integer-types*
    '((U8 . (cl:unsigned-byte 8))
      (I8 . (cl:signed-byte 8))
      (U16 . (cl:unsigned-byte 16))
      (I16 . (cl:signed-byte 16))
      (U32 . (cl:unsigned-byte 32))
      (I32 . (cl:signed-byte 32))
      (U64 . (cl:unsigned-byte 64))
      (I64 . (cl:signed-byte 64))
      (UFix . (cl:and cl:fixnum cl:unsigned-byte))
      (IFix . cl:fixnum)
      (Integer . cl:integer))))

(coalton-toplevel
  (declare unsafe-cast (:any -> :other))
  (define (unsafe-cast x)
    "Both :ANY and :OTHER must be natively represented by a subtype of `cl:integer', and X must be a valid member of :OTHER."
    (lisp :other (x) x))

  (declare unify (:ty -> :ty -> :ty))
  (define (unify _ use)
    "Declare a constraint that two values are of the same type.

Used in `cast-if-inbounds' to force the type inference engine to read minBound and maxBound from the correct
`Bounded' instance."
    use)

  (declare cast-if-inbounds ((Ord :src) (Bounded :target) =>
                             :src -> (Result String :target)))
  (define (cast-if-inbounds x)
    "Cast X, minBound and maxBound to `Integer', and compare them. If X is within the bounds, `unsafe-cast' it to the result type."
    (let max-bound = maxBound)
    (let min-bound = minBound)
    (let int = (the Integer (unsafe-cast x)))
    (if (or (< (unsafe-cast min-bound) int) (> (unsafe-cast max-bound) int))
                (Err "value out of range")
                (Ok
                 ;; type hackery to get the minBound and maxBound from the Bounded instance of :target. if we
                 ;; removed the two `unfiy' calls, type inference would compute extra type variables for
                 ;; minBound and maxBound, each with the contstraints `Bounded _' and `Into _ Integer', but
                 ;; without unifying them with :target.
                 (unify max-bound (unify min-bound
                                         (unsafe-cast x)))))))

(cl:defmacro define-integer-conversions (from-type)
  "For each element of *INTEGER-TYPES* other than FROM-TYPE, define an `Into' or `TryInto' instance as appropriate."
  (cl:let* ((from-repr (cl:or (cl:cdr (cl:assoc from-type *integer-types*))
                              (cl:error "Attempt to define integer conversions for unknown type ~s" from-type))))
    (cl:flet ((repr-subtypep? (sub super)
                (cl:multiple-value-bind (subtypep determinedp)
                    (cl:subtypep sub super)
                  (cl:if determinedp
                         subtypep
                         (cl:error "Unable to determine subtype relationship between ~s and ~s" sub super)))))
      (cl:cons 'coalton-toplevel
               (cl:loop :for (into-type . into-repr) :in *integer-types*
                  :when (cl:not (cl:eq into-type from-type))
                    :collect (cl:if (repr-subtypep? from-repr into-repr)
                                    `(define-instance (Into ,from-type ,into-type)
                                       (define into unsafe-cast))
                                    `(define-instance (TryInto ,from-type ,into-type)
                                       (define tryInto cast-if-inbounds))))))))

(define-integer-conversions U8)
(define-integer-conversions I8)
(define-integer-conversions U16)
(define-integer-conversions I16)
(define-integer-conversions U32)
(define-integer-conversions I32)
(define-integer-conversions U64)
(define-integer-conversions I64)
(define-integer-conversions UFix)
(define-integer-conversions IFix)
(define-integer-conversions Integer)

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/CONVERSIONS")
