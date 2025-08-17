;;;; rbit.lisp

(defpackage #:coalton-library/algorithms/rbit
  (:documentation "A Coalton package for efficient bit reversal with `RBIT`.

This package defines a type class, `RBIT`, with instances defined for `U8`, `U16`, `U32`, `UFix`, and `U64`. The `RBIT` type class defines the methods `(rbit x)` and `(rbitn n x)` for reversing the bits of `x` and the first `n` bits of `x`, respectively.")
  (:use
   #:coalton)
  (:export
   #:RBIT
   #:rbit
   #:rbitn))

(in-package #:coalton-library/algorithms/rbit)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Tell the compiler about the function, `%rbit`.
  (sb-c:defknown %rbit ((cl:unsigned-byte 64)) (cl:unsigned-byte 64) ()
    :overwrite-fndb-silently cl:t))

;;; Define the virtual operation for applying the RBIT assembly
;;; instruction in the SB-VM package.

(cl:in-package #:sb-vm) 

(define-vop (coalton-library/algorithms/rbit::%rbit)
  (:translate    coalton-library/algorithms/rbit::%rbit)
  (:policy       :fast-safe)
  (:args         (arg :scs (unsigned-reg) :target res))
  (:arg-types    unsigned-num)
  (:results      (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator    1 (inst rbit res arg)))

;;; In the RBIT package, define a type class for types to which the
;;; RBIT instruction can be applied and define instances on this type
;;; class for the relevant number types, U8, U16, U32, UFix, and U64.

(in-package #:coalton-library/algorithms/rbit)

(coalton-toplevel

  (define-class (RBIT :t)
    "A type class for number types that support bit reversal using the ARM64 assembly instruction, `RBIT`."
    (rbit
     "Reverse the bits of `x`."
     (:t -> :t))
    (rbitn
     "Reverse the first `n` bits of `x` and set the rest to 0."
     (UFix -> :t -> :t))))

;;; `RBIT` always reverses 64 bits. So, for smaller number types, the
;;; result of an application of `RBIT` has to be shifted to the right
;;; by `(- 64 nbits)` to recover the correct bit reversal.

(cl:defmacro define-rbit (type nbits)
  "Define an instance of `RBIT` for the Coalton type `type` of size `nbits`."
  (cl:let* ((shift (cl:- 64 (cl:the (cl:integer 0 64) (cl:eval nbits)))))
    `(coalton-toplevel

       (define-instance (RBIT ,type)
         (inline)
         (define (rbit x)
           (lisp ,type (x)
             (cl:ash (cl:the (cl:unsigned-byte 64) (%rbit x))
                     ,(cl:- shift))))
         (inline)
         (define (rbitn n x)
           (lisp ,type (n x)
             (cl:ash (cl:the (cl:unsigned-byte 64) (%rbit x))
                     (cl:the (cl:integer -63 0) (cl:- n 64)))))))))

(cl:defconstant +n-ufix-bits+ (cl:integer-length cl:most-positive-fixnum))

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0) (cl:compilation-speed 0)))

(define-rbit U8 8)
(define-rbit U16 16)
(define-rbit U32 32)
(define-rbit UFix +n-ufix-bits+)
(define-rbit U64 64)

