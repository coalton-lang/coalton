;;;; bounded.lisp
;;;;
;;;; Numerical types with fixed bounds

(coalton-library/utils:defstdlib-package #:coalton-library/math/bounded
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/functions)
  (:export
   #:Bounded #:minBound #:maxBound))

(in-package #:coalton-library/math/bounded)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define-class (Bounded :a)
    "Types which have a maximum and minumum bound."
    (minBound :a)
    (maxBound :a))

  (define-instance (Bounded U8)
    (define minBound 0)    ; 0
    (define maxBound 255)) ; 2^8-1
    
  (define-instance (Bounded I8)
    (define minBound -128) ; -1 * ceiling((2^8-1)/2)
    (define maxBound 127)) ; ceiling((2^8-1)/2)

  (define-instance (Bounded U16)
    (define minBound 0)      ; 0
    (define maxBound 65535)) ; 2^16-1

  (define-instance (Bounded I16)
    (define minBound -32768) ; -1 * floor((2^16-1)/2)
    (define maxBound 32767)) ; ceiling((2^16-1)/2)

  (define-instance (Bounded U32)
    (define minBound 0)           ; 0
    (define maxBound 4294967295)) ; 2^32-1

  (define-instance (Bounded I32)
    (define minBound -2147483648)  ; -1 * ceiling((2^32-1)/2)
    (define maxBound 2147483647))  ; floor((2^32-1)/2)
    
  (define-instance (Bounded U64)
    (define minBound 0)                     ; 0
    (define maxBound 18446744073709551615)) ; 2^64-1

  (define-instance (Bounded I64)
    (define minBound -9223372036854775808)  ; -1 * ceiling((2^64-1)/2)
    (define maxBound 9223372036854775807)) ; floor((2^32-1)/2)

  (define-instance (Bounded IFix)
    (define minBound
      (lisp IFix ()
        cl:most-positive-fixnum))
    (define maxBound
      (lisp IFix ()
        cl:most-negative-fixnum)))

  (define-instance (Bounded UFix)
    (define minBound 0)
    (define maxBound
      (lisp UFix ()
        cl:most-positive-fixnum))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/BOUNDED")
