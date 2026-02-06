(uiop:define-package #:coalton/math
  (:nicknames
   #:coalton-library/math)
  (:use-reexport
   #:coalton/math/arith
   #:coalton/math/num
   #:coalton/math/bounded
   #:coalton/math/conversions
   #:coalton/math/fraction
   #:coalton/math/integral
   #:coalton/math/real
   #:coalton/math/complex
   #:coalton/math/elementary
   #:coalton/math/dual))

#+sb-package-locks
(sb-ext:lock-package "COALTON/MATH")
