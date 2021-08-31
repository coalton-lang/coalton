(in-package #:coalton-user)

(coalton-toplevel
  ;;
  ;; Built-in type instances
  ;;
  
  ;;
  ;; Int
  ;;
  
  (define-instance (Show Int)
    (define (show x)
      (lisp String (x) (cl:write-to-string x))))

  (define-instance (Eq Int)
    (define (== a b)
      (lisp Boolean (a b)
        (to-boolean (cl:= a b))))
    (define (/= a b)
      (not (== a b))))

  (define-instance (Ord Int)
      (define (<=> a b)
        (lisp Ord (a b)
          (cl:cond
            ((cl:< a b)
             LT)
            ((cl:> a b)
             GT)
            (cl:t
             EQ)))))

  (define-instance (Num Int)
    (define (+ a b)
      (lisp Int (a b) (cl:+ a b)))
    (define (- a b)
      (lisp Int (a b) (cl:- a b)))
    (define (* a b)
      (lisp Int (a b) (cl:* a b)))
    (define (fromInt x) x))

  (declare expt (Int -> Int -> Int))
  (define (expt base power)
    (lisp Int (base power) (cl:expt base power)))

  (declare mod (Int -> Int -> Int))
  (define (mod num base)
    (lisp Int (num base) (cl:mod num base)))

  (declare even (Int ->  Boolean))
  (define (even n)
    (lisp Boolean (n) (to-boolean (cl:evenp n))))

  (declare odd (Int -> Boolean))
  (define (odd n)
    (lisp Boolean (n) (to-boolean (cl:oddp n))))

  (declare gcd (Int -> Int -> Int))
  (define (gcd a b)
    (lisp Int (a b) (cl:gcd a b)))

  (declare lcm (Int -> Int -> Int))
  (define (lcm a b)
    (lisp Int (a b) (cl:lcm a b)))


  ;;
  ;; Char
  ;;

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
              LT))))


  ;;
  ;; String
  ;;

  (define-instance (Eq String)
    (define (== s1 s2)
      (lisp Boolean (s1 s2) (to-boolean (cl:string= s1 s2))))
    (define (/= s1 s2)
      (not (== s1 s2)))))
