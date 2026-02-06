(uiop:define-package #:coalton/experimental
  (:nicknames
   #:coalton-library/experimental)
  (:use-reexport
   #:coalton/experimental/loops))

#+sb-package-locks
(sb-ext:lock-package "COALTON/EXPERIMENTAL")
