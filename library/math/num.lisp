;;;; num.lisp
;;;;
;;;; Instances for primitive numerical types

(coalton-library/utils:defstdlib-package #:coalton-library/math/num
  (:use
   #:coalton
   #:coalton-library/math/num-defining-macros)
  (:local-nicknames
   (#:cls #:coalton-library/classes))
  (:import-from
   #:coalton-library/hash
   #:define-sxhash-hasher))

(in-package #:coalton-library/math/num)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

;;;
;;; Eq Instances
;;;

  (define-eq Integer)
  (define-eq IFix)
  (define-eq UFix)
  (define-eq Bit)
  (define-eq I8)
  (define-eq U8)
  (define-eq I16)
  (define-eq U16)
  (define-eq I32)
  (define-eq U32)
  (define-eq I64)
  (define-eq U64)
  (define-eq F32)
  (define-eq F64)


;;;
;;; Ord Instances
;;;

  (define-ord Integer)
  (define-ord IFix)
  (define-ord UFix)
  (define-ord Bit)
  (define-ord I8)
  (define-ord U8)
  (define-ord I16)
  (define-ord U16)
  (define-ord I32)
  (define-ord U32)
  (define-ord I64)
  (define-ord U64)
  (define-ord F32)
  (define-ord F64)


;;;
;;; Integer Num instances
;;;

  (define-num-checked Integer cl:identity)

  (define-num-checked I8 %handle-8bit-overflow)
  (define-num-checked I16 %handle-16bit-overflow)
  (define-num-checked I32 %handle-32bit-overflow)
  (define-num-checked I64 %handle-64bit-overflow)
  (define-num-checked IFix %handle-fixnum-overflow)

  (define-num-wrapping U8 8)
  (define-num-wrapping U16 16)
  (define-num-wrapping U32 32)
  (define-num-wrapping U64 64)

  (define-instance (cls:Num Bit)
    (inline)
    (define (cls:+ a b)
      (lisp Bit (a b)
        (cl:logxor a b)))

    (inline)
    (define (cls:- a b)
      (lisp Bit (a b)
        (cl:logxor a b)))

    (inline)
    (define (cls:* a b)
      (lisp Bit (a b)
        (cl:logand a b)))

    (inline)
    (define (cls:fromInt x)
      "The integer `x` is converted to a `Bit` by its zeroth bit (i.e.,
whether it's even or odd)."
      (lisp Bit (x)
        (cl:ldb (cl:byte 1 0) x))))

  ;; UFixes are unsafe and depend on implementation.
  (define-instance (cls:Num UFix)
    (inline)
    (define (cls:+ a b)
      (lisp UFix (a b)
        (cl:locally (cl:declare (cl:optimize cl:speed (cl:safety 0)))
          (cl:+ a b))))

    (inline)
    (define (cls:- a b)
      (lisp UFix (a b)
        (cl:locally (cl:declare (cl:optimize cl:speed (cl:safety 0)))
          (cl:- a b))))

    (inline)
    (define (cls:* a b)
      (lisp UFix (a b)
        (cl:locally (cl:declare (cl:optimize cl:speed (cl:safety 0)))
          (cl:* a b))))

    (inline)
    (define (cls:fromInt x)
      (lisp UFix (x)
        (cl:mod x #.(cl:expt 2 +fixnum-bits+)))))


;;;
;;; Float Num instances
;;;

  (define-num-float F32 cl:single-float float-features:single-float-positive-infinity float-features:single-float-negative-infinity)
  (define-num-float F64 cl:double-float float-features:double-float-positive-infinity float-features:double-float-negative-infinity)


;;;
;;; Float to Fraction conversions
;;;

  (define-float-fraction-conversion F32)
  (define-float-fraction-conversion F64)


;;;
;;; Dividable and Reciprocable instances for floats
;;;

  (define-reciprocable-float F32)
  (define-reciprocable-float F64)

  (define-dividable-float F32 cl:single-float)
  (define-dividable-float F64 cl:double-float)


;;;
;;; Bits instances
;;;

  (define-bits-checked Integer cl:identity)

  (define-bits-checked I8 %handle-8bit-overflow)
  (define-bits-checked I16 %handle-16bit-overflow)
  (define-bits-checked I32 %handle-32bit-overflow)
  (define-bits-checked I64 %handle-64bit-overflow)
  (define-bits-checked IFix %handle-fixnum-overflow)

  (define-bits-wrapping Bit 1)
  (define-bits-wrapping U8 8)
  (define-bits-wrapping U16 16)
  (define-bits-wrapping U32 32)
  (define-bits-wrapping U64 64)
  (define-bits-wrapping UFix #.+unsigned-fixnum-bits+)


;;;
;;; Hash instances
;;;

  (define-sxhash-hasher Integer)
  (define-sxhash-hasher I8)
  (define-sxhash-hasher I16)
  (define-sxhash-hasher I32)
  (define-sxhash-hasher I64)
  (define-sxhash-hasher Bit)
  (define-sxhash-hasher U8)
  (define-sxhash-hasher U16)
  (define-sxhash-hasher U32)
  (define-sxhash-hasher U64)
  (define-sxhash-hasher IFix)
  (define-sxhash-hasher UFix)
  (define-sxhash-hasher F32)
  (define-sxhash-hasher F64)


;;;
;;; Default instances
;;;

  (define-default-num Bit)
  (define-default-num I8)
  (define-default-num U8)
  (define-default-num I16)
  (define-default-num I32)
  (define-default-num I64)
  (define-default-num U16)
  (define-default-num U32)
  (define-default-num U64)
  (define-default-num IFix)
  (define-default-num UFix)
  (define-default-num Integer)
  (define-default-num F32)
  (define-default-num F64))
