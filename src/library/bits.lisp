(coalton-library/utils::defstdlib-package #:coalton-library/bits
  (:shadow
     #:and
     #:or
     #:xor
     #:not)
  (:use
   #:coalton)
  (:local-nicknames
   (#:classes #:coalton-library/classes))
  (:export
   #:Bits
   #:and
   #:or
   #:xor
   #:not
   #:shift))

(cl:in-package #:coalton-library/bits)

(coalton-toplevel
  (define-class ((classes:Num :int) => (Bits :int))
    "Operations on the bits of twos-complement integers"
    (and (:int -> :int -> :int))
    (or (:int -> :int -> :int))
    (xor (:int -> :int -> :int))
    (not (:int -> :int))
    (shift (Integer -> :int -> :int))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/BITS")

