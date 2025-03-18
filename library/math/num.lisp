;;;; num.lisp
;;;;
;;;; Instances for primitive numerical types

(coalton-library/utils:defstdlib-package #:coalton-library/math/num
  (:use
   #:coalton
   #:coalton-library/math/num-defining-macros)
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
  (define-eq I8)
  (define-eq U8)
  (define-eq I16)
  (define-eq U16)
  (define-eq I32)
  (define-eq U32)
  (define-eq I64)
  (define-eq U64)
  (define-eq Single-Float)
  (define-eq Double-Float)


;;;
;;; Ord Instances
;;;

  (define-ord Integer)
  (define-ord IFix)
  (define-ord UFix)
  (define-ord I8)
  (define-ord U8)
  (define-ord I16)
  (define-ord U16)
  (define-ord I32)
  (define-ord U32)
  (define-ord I64)
  (define-ord U64)
  (define-ord Single-Float)
  (define-ord Double-Float)


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
  (define-num-wrapping UFix #.+unsigned-fixnum-bits+)


;;;
;;; Float Num instances
;;;

  (define-num-float Single-Float cl:single-float)
  (define-num-float Double-Float cl:double-float)


;;;
;;; Float to Fraction conversions
;;;

  (define-float-fraction-conversion Single-Float)
  (define-float-fraction-conversion Double-Float)


;;;
;;; Dividable and Reciprocable instances for floats
;;;

  (define-reciprocable-float Single-Float)
  (define-reciprocable-float Double-Float)

  (define-dividable-float Single-Float cl:single-float)
  (define-dividable-float Double-Float cl:double-float)


;;;
;;; Bits instances
;;;

  (define-bits-checked Integer cl:identity)

  (define-bits-checked I8 %handle-8bit-overflow)
  (define-bits-checked I16 %handle-16bit-overflow)
  (define-bits-checked I32 %handle-32bit-overflow)
  (define-bits-checked I64 %handle-64bit-overflow)
  (define-bits-checked IFix %handle-fixnum-overflow)

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
  (define-sxhash-hasher U8)
  (define-sxhash-hasher U16)
  (define-sxhash-hasher U32)
  (define-sxhash-hasher U64)
  (define-sxhash-hasher IFix)
  (define-sxhash-hasher UFix)
  (define-sxhash-hasher Single-Float)
  (define-sxhash-hasher Double-Float)


;;;
;;; Default instances
;;;

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
  (define-default-num Double-Float)
  (define-default-num Single-Float))
