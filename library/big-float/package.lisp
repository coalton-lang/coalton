;;;; package.lisp
;;;;
;;;; Big float package for various implementations

(coalton/utils:defstdlib-package #:coalton/big-float
  (:use #:coalton
        #:coalton/classes
        #:coalton/functions
        #:coalton/math)
  (:import-from #:coalton/math/dyadic #:Dyadic)
  (:local-nicknames
   (#:dyadic #:coalton/math/dyadic)
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

   #:Big-Float
   #:with-precision-rounding

   #:bf-pi
   #:bf-ee))
