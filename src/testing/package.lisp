(uiop:define-package #:coalton-testing
  (:use-reexport
   #:coalton
   #:coalton-prelude)
  (:export
   #:coalton-fiasco-init
   #:coalton-is-assertion
   #:coalton-failed-assertion
   #:is))
