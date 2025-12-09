(coalton-library/utils::defstdlib-package #:coalton-library/bits
  (:shadow
     #:and
     #:or
     #:xor
     #:not)
  (:use
   #:coalton
   #:coalton-compatibility)
  (:local-nicknames
   (#:compat #:coalton-compatibility))
  (:import-from
   #:coalton-library/classes
   #:Num)
  (:import-from
   #:coalton-library/internal/rbit
   #:rbit)
  (:export
   #:Bits
   #:and
   #:or
   #:xor
   #:not
   #:shift
   #:dpb
   #:ldb
   #:ReverseBits
   #:reverse-bits
   #:reverse-n-bits))

(in-package #:coalton-library/bits)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define-class (Num :int => Bits :int)
    "Operations on the bits of twos-complement integers"
    (and   "The bitwise logical `and` of two integers"
           (:int -> :int -> :int))
    (or    "The bitwise logical `or` of two integers"
           (:int -> :int -> :int))
    (xor   "The bitwise logical exclusive `or` of two integers"
           (:int -> :int -> :int))
    (not   "The bitwise logical `not` of two integers"
           (:int -> :int))
    (shift "The arithmetic left-shift of an integer by an integer number of bits"
           (Integer -> :int -> :int)))

  (declare dpb (Bits :a => :a -> UFix -> UFix -> :a -> :a))
  (define (dpb newbyte size position bitstring)
    "Deposits a byte `newbyte` of size `size` into a bitstring `bitstring` at a position `position`."
    (lisp :a (newbyte bitstring size position)
      (cl:dpb newbyte (cl:byte size position) bitstring)))

  (declare ldb (Bits :a => UFix -> UFix -> :a -> :a))
  (define (ldb size position bitstring)
    "Deposits a byte of size `size` into a bitstring at a position `position`."
    (lisp :a (bitstring size position)
      (cl:ldb (cl:byte size position) bitstring))))

(coalton-toplevel

  (define-class (ReverseBits :t)
    "A type class for number types that support bit reversal."
    (reverse-bits
     "Reverse the bits of `x`."
     (:t -> :t))
    (reverse-n-bits
     "Reverse the first `n` bits of `x` and set the rest to 0."
     (UFix -> :t -> :t))))

(cl:defmacro define-reverse-bits (type nbits)
  "Define an instance of `ReverseBits` for the Coalton type `type` of size `nbits`."
  (cl:let* ((shift (cl:- 64 (cl:the (cl:integer 0 64) (cl:eval nbits)))))
    `(coalton-toplevel
       (define-instance (ReverseBits ,type)
         (inline)
         (define (reverse-bits x)
           (lisp ,type (x)
             (cl:ash (cl:the (cl:unsigned-byte 64) (rbit x))
                     ,(cl:- shift))))
         (inline)
         (define (reverse-n-bits n x)
           (lisp ,type (n x)
             (cl:ash (cl:the (cl:unsigned-byte 64) (rbit x))
                     (cl:the (cl:integer -63 0) (cl:- n 64)))))))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defconstant +n-ufix-bits+ (cl:integer-length cl:most-negative-fixnum)))

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0) (cl:compilation-speed 0)))

(define-reverse-bits U8 8)
(define-reverse-bits U16 16)
(define-reverse-bits U32 32)
(define-reverse-bits UFix +n-ufix-bits+)
(define-reverse-bits U64 64)

(compat:try-lock-package "COALTON-LIBRARY/BITS")

