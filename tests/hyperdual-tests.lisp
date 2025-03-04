(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-instance (LooseCompare :t => LooseCompare (hyperdual:Hyperdual :t))
    (define (~ (hyperdual:Hyperdual a1 b1 c1 d1) (hyperdual:Hyperdual a2 b2 c2 d2))
      (and (~ a1 a2) (~ b1 b2) (~ c1 c2) (~ d1 d2)))))

(coalton-toplevel

  ;; Type Class Instance Test Functions

  ;; Num

  (define (f1 x)
    (* 3 (+ x (* x (- 5 x)))))

  (define (d0f1 x)
    (f1 x))
  (define (d1f1 x)
    (+ 18 (* -6 x)))
  (define (d2f1 _x)
    -6)

  ;; Reciprocable

  (define (f2 x)
    (/ 5 x))

  (define (d0f2 x)
    (f2 x))
  (define (d1f2 x)
    (/ -5 (* x x)))
  (define (d2f2 x)
    (/ 10 (* x (* x x))))

  ;; Trigonometric

  (define (sec x) (reciprocal (cos x)))
  (define (sec2 x) (^ (sec x) 2))

  (define (f3 x)
    (+ (sin (cos (tan x)))
       (+ (asin x)
          (+ (acos x)
             (atan x)))))

  (define (d0f3 x)
    (f3 x))
  (define (d1f3 x)
    (+ (* (cos (cos (tan x))) (* (negate (sin (tan x))) (sec2 x)))
       (reciprocal (1+ (* x x)))))
 (define (d2f3 x)
   (+ (+ (* (* (negate (sin (cos (tan x)))) (* (negate (sin (tan x))) (sec2 x)))
            (* (negate (sin (tan x))) (sec2 x)))
         (+ (* (* (negate (cos (tan x))) (sec2 x))
               (* (cos (cos (tan x))) (sec2 x)))
            (* (* 2 (* (sec2 x) (tan x)))
               (* (cos (cos (tan x))) (negate (sin (tan x)))))))
      (* -2 (/ x (^ (1+ (* x x)) 2)))))

  ;; Exponentiable
  
  (define (f4 x)
    (pow 2 (log x (ln (exp x)))))

  (define (d0f4 x)
    (f4 x))
  (define (d1f4 _x)
    0)
  (define (d2f4 _x)
    0)

  ;; Radical

  (define (f5 x)
    (sqrt (nth-root 5 x)))

  (define (d0f5 x)
    (f5 x))
  (define (d1f5 x)
    (/ (nth-root 10 x) (* 10 x)))
  (define (d2f5 x)
    (/ (* -9 (nth-root 10 x)) (* 100 (* x x))))

  ;; Test

  (define (test-univariate %f f df ddf x)
    (let ((%x (hyperdual:Hyperdual x 1 1 0))
          (a (f x))
          (b (df x))
          (c b)
          (d (ddf x))
          (%fx (%f %x))
          (fx (hyperdual:Hyperdual a b c d)))
      (is (~ %fx fx))
      (or (~ %fx fx) (progn (print x) False)))))

(define-test univariate-hyperdual-test-1 ()
  (pipe (make-list -10.0 -4.0 0.0 1.0 1344.34)
        (all (test-univariate f1 d0f1 d1f1 d2f1))))

(define-test univariate-hyperdual-test-2 ()
  (pipe (make-list -10.0 -4.0 0.1 1.0 1344.34)
        (all (test-univariate f2 d0f2 d1f2 d2f2))))

(define-test univariate-hyperdual-test-3 ()
  (pipe (make-list 0.2 0.4 0.7 0.9)
        (all (test-univariate f3 d0f3 d1f3 d2f3))))

(define-test univariate-hyperdual-test-4 ()
  (pipe (make-list 0.1 0.2 0.3 0.4 10.0)
        (all (test-univariate f4 d0f4 d1f4 d2f4))))

(define-test univariate-hyperdual-test-5 ()
  (pipe (make-list 0.1 0.2 0.3 0.4 0.8)
        (all (test-univariate f4 d0f4 d1f4 d2f4))))

(coalton-toplevel

  ;; Functions for testing multivariate & convenience functions tests.

  (define (g x y)
    (+ (* 17 (* (^ x 3) (^ y 4))) (* 12 (* (^ x 2) y))))

  (define (dxg x y)
    (+ (* 51 (* (^ x 2) (^ y 4))) (* 24 (* x y))))

  (define (dxxg x y)
    (+ (* 102 (* x (^ y 4))) (* 24 y)))

  (define (dyg x y)
    (+ (* 68 (* (^ x 3) (^ y 3))) (* 12 (^ x 2))))

  (define (dyyg x y)
    (* 204 (* (^ x 3) (^ y 2))))

  (define (dxyg x y)
    (+ (* 204 (* (^ x 2) (^ y 3))) (* 24 x)))

  (define points (make-list (Tuple -10 3) (Tuple 0 0) (Tuple 1 47))))

(define-test hyperdual-d-x-test ()
  (for (Tuple x y) in points
    (is (== (dxg x y) (hyperdual:d-x (fn (%x) (g %x (into y))) x)))))

(define-test hyperdual-d-xx-test ()
  (for (Tuple x y) in points
    (is (== (dxxg x y) (hyperdual:d-xx (fn (%x) (g %x (into y))) x)))))

(define-test hyperdual-partial-x-test ()
  (for (Tuple x y) in points
    (is (== (dxg x y) (hyperdual:partial-x g x y)))))

(define-test hyperdual-partial-y-test ()
  (for (Tuple x y) in points
    (is (== (dyg x y) (hyperdual:partial-y g x y)))))

(define-test hyperdual-partial-xx-test ()
  (for (Tuple x y) in points
    (is (== (dxxg x y) (hyperdual:partial-xx g x y)))))

(define-test hyperdual-partial-xy-test ()
  (for (Tuple x y) in points
    (is (== (dxyg x y) (hyperdual:partial-xy g x y)))))

(define-test hyperdual-partial-yy-test ()
  (for (Tuple x y) in points
    (is (== (dyyg x y) (hyperdual:partial-yy g x y)))))

(define-test hyperdual-gradient-test ()
  (for (Tuple x y) in points
    (is (all (uncurry ==) (zip (make-list (dxg x y) (dyg x y))
                              (hyperdual:gradient g x y))))))

(define-test hyperdual-hessian-test ()
  (for (Tuple x y) in points
    (is (all (uncurry ==) (zip (make-list (dxxg x y) (dxyg x y) (dxyg x y) (dyyg x y))
                              (hyperdual:hessian g x y))))))

(define-test hyperdual-laplacian-test ()
  (for (Tuple x y) in points
    (is (== (+ (dxxg x y) (dyyg x y)) (hyperdual:laplacian g x y)))))

