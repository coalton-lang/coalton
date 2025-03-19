(uiop:define-package #:coalton-library/experimental
  (:use-reexport
   #:coalton-library/experimental/loops))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/EXPERIMENTAL")
