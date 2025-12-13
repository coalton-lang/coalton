(uiop:define-package #:coalton-library/math
  (:use
   #:coalton-compatibility)
  (:use-reexport
   #:coalton-library/math/arith
   #:coalton-library/math/num
   #:coalton-library/math/bounded
   #:coalton-library/math/conversions
   #:coalton-library/math/fraction
   #:coalton-library/math/integral
   #:coalton-library/math/real
   #:coalton-library/math/complex
   #:coalton-library/math/elementary
   #:coalton-library/math/dual))

(coalton-compatibility:try-lock-package "COALTON-LIBRARY/MATH")
