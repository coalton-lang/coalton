(coalton-library/utils:defstdlib-package #:coalton-library/char
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/builtin))

(cl:in-package #:coalton-library/char)

(coalton-toplevel
  (define-instance (Eq Char)
    (define (== x y)
      (lisp Boolean (x y) (to-boolean (cl:char= x y)))))

  (define-instance (Ord Char)
    (define (<=> x y)
      (if (== x y)
          EQ
          (if (lisp Boolean (x y) (to-boolean (cl:char> x y)))
              GT
              LT)))))

(define-sxhash-hasher Char)

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/CHAR")
