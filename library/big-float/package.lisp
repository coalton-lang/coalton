;;;; package.lisp
;;;;
;;;; Big float package for various implementations

(coalton-library/utils:defstdlib-package #:coalton-library/big-float
  (:use #:coalton
        #:coalton-library/classes
        #:coalton-library/functions
        #:coalton-library/math
        #:coalton-library/math/integral)
  (:import-from #:coalton-library/math/dyadic #:Dyadic)
  (:local-nicknames
   (#:dyadic #:coalton-library/math/dyadic)
   (#:bits #:coalton-library/bits))
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

   #:MPFR-Float
   #:Big-Float
   #:with-precision-rounding

   #:bf-pi
   #:bf-ee))
