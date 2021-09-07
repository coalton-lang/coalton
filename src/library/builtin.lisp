(in-package #:coalton-library)

(coalton-toplevel
  (define (undefined x)
    (lisp :a () (cl:error "Undefined"))))
