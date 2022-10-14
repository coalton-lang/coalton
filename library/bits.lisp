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
   #:shift))

(in-package #:coalton-library/bits)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define-class (Num :int => Bits :int)
    "Operations on the bits of twos-complement integers"
    (and (:int -> :int -> :int))
    (or (:int -> :int -> :int))
    (xor (:int -> :int -> :int))
    (not (:int -> :int))
    (shift (Integer -> :int -> :int))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/BITS")

