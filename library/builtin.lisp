(coalton-library/utils::defstdlib-package #:coalton-library/builtin
  (:use
   #:coalton
   #:coalton-library/classes)
  (:export
   #:undefined
   #:error ; re-export from classes
   #:not
   #:xor
   #:boolean-not
   #:boolean-or
   #:boolean-and
   #:boolean-xor))

(in-package #:coalton-library/builtin)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define (undefined x)
    "A function which can be used in place of any value, throwing an error at runtime."
    (error "Undefined"))

  (define not
    "Synonym for BOOLEAN-NOT."
    boolean-not)

  (define xor
    "Synonym for BOOLEAN-XOR."
    boolean-xor)

  (declare boolean-not (Boolean -> Boolean))
  (define (boolean-not x)
    "Is X False?"
    (match x
      ((True) False)
      ((False) True)))

  (declare boolean-or (Boolean -> Boolean -> Boolean))
  (define (boolean-or x y)
    "Is X or Y True? Note that this is a *function* which means both X and Y will be evaluated. Use the OR macro for short-circuiting behavior."
    (match x
      ((True) True)
      ((False) y)))

  (declare boolean-and (Boolean -> Boolean -> Boolean))
  (define (boolean-and x y)
    "Are X and Y True? Note that this is a *function* which means both X and Y will be evaluated. Use the AND macro for short-circuiting behavior."
    (match x
      ((True) y)
      ((False) False)))

  (declare boolean-xor (Boolean -> Boolean -> Boolean))
  (define (boolean-xor x y)
    "Are X or Y True, but not both?"
    (match x
      ((True) (boolean-not y))
      ((False) y))))


#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/BUILTIN")
