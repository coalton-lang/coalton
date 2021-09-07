(in-package #:coalton-library)

(coalton-toplevel
  (define-instance (Show Char)
    (define (show x)
      (lisp String (x)
        (cl:string x))))

  (define-instance (Eq Char)
    (define (== x y)
      (lisp Boolean (x y) (to-boolean (cl:char= x y))))
    (define (/= x y)
      (not (== x y))))

  (define-instance (Ord Char)
    (define (<=> x y)
      (if (== x y)
          EQ
          (if (lisp Boolean (x y) (to-boolean (cl:char> x y)))
              GT
              LT)))))
