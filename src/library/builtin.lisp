(in-package #:coalton-library)

(coalton-toplevel
  (define (undefined x)
    "A function which can be used in place of any value, throwing an error at runtime."
    (lisp :a () (cl:error "Undefined")))

  (declare error (String -> :a))
  (define (error str)
    "Signal an error by calling CL:ERROR"
    (lisp :a (str) (cl:error str))))
