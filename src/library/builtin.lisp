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
      (lisp String (x) (cl:prin1-to-string x))))

  (define-instance (Show Integer)
    (define (show x)
      (lisp String (x) (cl:prin1-to-string x))))

  (define-instance (Show Single-Float)
    (define (show x)
      (lisp String (x) (cl:prin1-to-string x))))

  (define-instance (Show Double-Float)
    (define (show x)
      (lisp String (x) (cl:prin1-to-string x))))

  (define-instance (Eq Int)
    (define (== a b)
      (lisp Boolean (a b)
        (to-boolean (cl:= a b))))
    (define (/= a b)
      (not (== a b))))

  (define-instance (Eq Integer)
    (define (== a b)
      (lisp Boolean (a b)
        (to-boolean (cl:= a b))))
    (define (/= a b)
      (not (== a b))))

  (define-instance (Eq Single-Float)
    (define (== a b)
      (lisp Boolean (a b)
        (to-boolean (cl:= a b))))
    (define (/= a b)
      (not (== a b))))

  (define-instance (Eq Double-Float)
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

  (define-instance (Ord Integer)
    (define (<=> a b)
      (lisp Ord (a b)
        (cl:cond
          ((cl:< a b)
           LT)
          ((cl:> a b)
           GT)
          (cl:t
           EQ)))))

  (define-instance (Ord Single-Float)
    (define (<=> a b)
      (lisp Ord (a b)
        (cl:cond
          ((cl:< a b)
           LT)
          ((cl:> a b)
           GT)
          (cl:t
           EQ)))))

  (define-instance (Ord Double-Float)
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

  (define-instance (Num Integer)
    (define (+ a b)
      (lisp Integer (a b) (cl:+ a b)))
    (define (- a b)
      (lisp Integer (a b) (cl:- a b)))
    (define (* a b)
      (lisp Integer (a b) (cl:* a b)))
    (define (fromInt x)
      (lisp Integer (x) x)))            ; Magic Coalton cast

  (define-instance (Num Single-Float)
    (define (+ a b)
      (lisp Single-Float (a b) (cl:+ a b)))
    (define (- a b)
      (lisp Single-Float (a b) (cl:- a b)))
    (define (* a b)
      (lisp Single-Float (a b) (cl:* a b)))
    (define (fromInt x)
      (lisp Single-Float (x) (cl:coerce x 'cl:single-float))))

  (define-instance (Num Double-Float)
    (define (+ a b)
      (lisp Double-Float (a b) (cl:+ a b)))
    (define (- a b)
      (lisp Double-Float (a b) (cl:- a b)))
    (define (* a b)
      (lisp Double-Float (a b) (cl:* a b)))
    (define (fromInt x)
      (lisp Double-Float (x) (cl:coerce x 'cl:double-float))))

  (define-instance (Into Int Integer)
    (define (into x) (fromInt x)))

  (define-instance (Into Integer Int)
    (define (into x)
      (lisp Int (x) (cl:coerce x '(cl:signed-byte 32)))))

  (define-instance (Into Int Single-Float)
    (define (into x) (fromInt x)))

  (define-instance (TryInto Single-Float Single-Float Int)
    (define (tryInto x)
      (lisp (Result Single-Float Int) (x)
	(cl:if (cl:or (float-features:float-infinity-p x)
		      (float-features:float-nan-p x))
	       (Err x)
	       (Ok (cl:coerce (cl:truncate x) '(cl:signed-byte 32)))))))

  (define-instance (TryInto Double-Float Double-Float Int)
    (define (tryInto x)
      (lisp (Result Double-Float Int) (x)
	(cl:if (cl:or (float-features:float-infinity-p x)
		      (float-features:float-nan-p x))
	       (Err x)
	       (Ok (cl:coerce (cl:truncate x) '(cl:signed-byte 32)))))))

  (define-instance (Into Int Double-Float)
    (define (into x) (fromInt x)))

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
