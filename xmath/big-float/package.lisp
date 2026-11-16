;;;; package.lisp
;;;;
;;;; Big float package for various implementations

(coalton/utils:defstdlib-package #:coalton/xmath/big-float
  (:documentation "Arbitrary-precision binary floating-point numbers (`BigFloat`) with configurable mantissa precision and IEEE-style directed and nearest rounding modes (`RoundingMode`).

Finite values are represented as normalized dyadic rationals $m \\cdot 2^e$ rounded to the active bit precision (`with-precision`, `set-precision!`) under the active `RoundingMode` (`with-rounding`, `set-rounding-mode!`), alongside signed zero and IEEE transfinite values ($\\pm\\infty$, $\\text{NaN}$). Elementary transcendental functions (`sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `exp`, `ln`, `pow`, `sqrt`, `nth-root`) and constants (`pi`, `ee`) are computed to the active precision via binary splitting and argument reduction.")
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
