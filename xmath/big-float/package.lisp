;;;; package.lisp
;;;;
;;;; Big float package for various implementations

(coalton/utils:defstdlib-package #:coalton/xmath/big-float
  (:documentation "Arbitrary-precision binary floating-point numbers (`BigFloat`) with configurable mantissa precision and IEEE-style directed and nearest rounding modes (`RoundingMode`).

Finite values are dyadic rationals $m \\cdot 2^e$; exact conversions can retain more bits than the active arithmetic precision. `with-precision` and `with-rounding` select the arithmetic format. Signed zero, infinities and NaN are also represented. General transcendental functions use guard bits, not certified directed enclosures. `approx` bounds absolute error with respect to the stored value (a lazy constant is resolved in the caller's context).")
  (:nicknames #:coalton/big-float
              #:coalton-library/big-float)
  (:shadow #:scale)
  (:use #:coalton
        #:coalton/builtin
        #:coalton/classes
        #:coalton/functions
        #:coalton/math)
  (:import-from #:coalton/xmath/dyadic #:Dyadic)
  (:local-nicknames
   (#:dyadic #:coalton/xmath/dyadic)
   (#:complex #:coalton/math/complex)
   (#:bits #:coalton/bits))
  (:export
   #:RoundingMode
   #:rndna
   #:rndn
   #:rndz
   #:rndu
   #:rndd
   #:rnda
   #:rndf
   #:set-rounding-mode!
   #:get-rounding-mode
   #:with-rounding

   #:set-precision!
   #:get-precision
   #:with-precision

   #:BigFloat
   #:with-precision-rounding
   #:round-to-f32
   #:round-to-f64

   #:bf-pi
   #:bf-ee))
