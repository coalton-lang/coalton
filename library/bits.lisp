(coalton-library/utils::defstdlib-package #:coalton-library/bits
  (:shadow
     #:and
     #:or
     #:xor
     #:not)
  (:use
   #:coalton)
  (:import-from
   #:coalton-library/classes
   #:Num)
  (:export
   #:Bits
   #:and
   #:or
   #:xor
   #:not
   #:shift
   #:dpb
   #:ldb))

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

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/BITS")

