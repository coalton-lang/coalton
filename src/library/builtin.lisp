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
      (lisp String (cl:write-to-string x))))

  (define-instance (Eq Int)
    (define (== a b)
      (lisp Boolean
        (to-boolean (cl:= a b))))
    (define (/= a b)
      (not (== a b))))

  (define-instance (Ord Int)
      (define (<=> a b)
        (lisp Ord
          (cl:cond
            ((cl:< a b)
             LT)
            ((cl:> a b)
             GT)
            (cl:t
             EQ)))))

  (define-instance (Num Int)
    (define (+ a b)
      (lisp Int (cl:+ a b)))
    (define (- a b)
      (lisp Int (cl:- a b)))
    (define (* a b)
      (lisp Int (cl:* a b)))
    (define (fromInt x) x))

  (declare expt (Int -> Int -> Int))
  (define (expt base power)
    (lisp Int (cl:expt base power)))

  (declare mod (Int -> Int -> Int))
  (define (mod num base)
    (lisp Int (cl:mod num base)))

  (declare even (Int ->  Boolean))
  (define (even n)
    (lisp Boolean (to-boolean (cl:evenp n))))

  (declare odd (Int -> Boolean))
  (define (odd n)
    (lisp Boolean (to-boolean (cl:oddp n))))

  (declare gcd (Int -> Int -> Int))
  (define (gcd a b)
    (lisp Int (cl:gcd a b)))

  (declare lcm (Int -> Int -> Int))
  (define (lcm a b)
    (lisp Int (cl:lcm a b)))


  ;;
  ;; Char
  ;;

  (define-instance (Eq Char)
    (define (== x y)
      (lisp Boolean (to-boolean (cl:char= x y))))
    (define (/= x y)
      (not (== x y))))

  (define-instance (Ord Char)
    (define (<=> x y)
      (if (== x y)
          EQ
          (if (lisp Boolean (to-boolean (cl:char> x y)))
              GT
              LT))))


  ;;
  ;; String
  ;;

  (define-instance (Eq String)
    (define (== s1 s2)
      (lisp Boolean (to-boolean (cl:string= s1 s2))))
    (define (/= s1 s2)
      (not (== s1 s2))))
  )
