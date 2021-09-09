(in-package #:coalton-library)

(coalton-toplevel
  (define (undefined x)
    "A function which can be used in place of any value, throwing an error at runtime."
    (lisp :a () (cl:error "Undefined"))))
