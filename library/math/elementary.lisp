;;;; elementary.lisp
;;;;
;;;; Elementary/algebriac functions and trancsendental numbers

(coalton-library/utils::defstdlib-package #:coalton-library/math/elementary
    (:use
     #:coalton
     #:coalton-library/builtin
     #:coalton-library/classes
     #:coalton-library/functions
     #:coalton-library/utils
     #:coalton-library/math/arith
     #:coalton-library/math/real
     #:coalton-library/math/complex)
  (:local-nicknames (#:ff #:float-features))
  (:export
   #:Trigonometric
   #:sin #:cos #:tan
   #:sinh #:cosh #:tanh
   #:asin #:acos #:atan
   #:asinh #:acosh #:atanh
   #:sincos
   #:Radical
   #:sqrt
   #:nth-root
   #:Polar
   #:phase
   #:magnitude
   #:cis
   #:Exponentiable
   #:log #:pow #:exp #:ln
   #:Elementary
   #:pi #:ee
   #:atan2))

(in-package #:coalton-library/math/elementary)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define-class (Trigonometric :a)
    "Standard circular functions and their inverses."
    (sin   (:a -> :a))
    (cos   (:a -> :a))
    (tan   (:a -> :a))
    (asin  (:a -> :a))
    (acos  (:a -> :a))
    (atan  (:a -> :a)))

  (declare sincos (Trigonometric :a => :a -> (Tuple :a :a)))
  (define (sincos x)
    "Computes the sine and cosine of X."
    (Tuple (sin x) (cos x)))

  (define-class (Exponentiable :a)
    "Exponential maps obeying:

    (* (exp x) (exp y)) = (exp (+ x y))
    (exp (ln x)) = x = (ln (exp x))
    (log b x) = (/ (ln x) (ln b))
    (pow x y) = (exp (* y (ln x)))
"
    (exp (:a -> :a))
    (pow (:a -> :a -> :a))
    (ln  (:a -> :a))
    (log  (:a -> :a -> :a)))

  (define-class (Radical :a)
    "Obeys:

    (^ (sqrt x) 2) = x = (^^ (nth-root n x) n)"
    (nth-root (Integer -> :a -> :a))
    (sqrt (:a -> :a)))

  (define-class ((Complex :a) (Num :a) => Polar :a)
    "For a complex number `z = (complex x y)`, the following identities hold:

    z = (* (magnitude z) (exp (* ii (phase z))))
    (polar z) = (Tuple (magnitude z) (phase z))
    (phase z) = (atan2 y x)
"
    (phase ((Complex :a) -> :a))
    (polar ((Complex :a) -> (Tuple :a :a))))

  (define (magnitude z)
    "For `z = x + yi`,


    (magnitude z) = (sqrt (+ (^ x 2) (^ y 2)))"
    (sqrt (square-magnitude z)))

  (declare cis ((Trigonometric :a) (Complex :a) => :a -> (Complex :a)))
  (define (cis z)
    "A point on the complex unit circle:


    (cis z) := (exp (complex 0 z))
             = (complex (cos z) (sin z))
"
    (complex (cos z) (sin z)))

  (define-class
      ((Reciprocable :a) (Polar :a)
       (Trigonometric :a) (Exponentiable :a) (Radical :a)
       => Elementary :a)
    "Numbers that can be can be passed to elementary functions."
    (ee :a)
    (pi :a))

  ;; See http://clhs.lisp.se/Body/f_sinh_.htm

  (declare sinh ((Elementary :f) => :f -> :f))
  (define (sinh x)
    (/ (- (exp x) (exp (negate x))) 2))

  (declare cosh ((Elementary :f) => :f -> :f))
  (define (cosh x)
    (/ (+ (exp x) (exp (negate x))) 2))

  (declare tanh ((Elementary :f) => :f -> :f))
  (define (tanh x)
    (/ (sinh x) (cosh x)))

  (declare asinh ((Elementary :f) => :f -> :f))
  (define (asinh x)
    (ln (+ x (sqrt (+ 1 (pow x 2))))))

  (declare acosh ((Elementary :f) => :f -> :f))
  (define (acosh x)
    (* 2 (ln (+ (sqrt (/ (+ x 1) 2)) (sqrt (/ (- x 1) 2))))))

  (declare atanh ((Elementary :f) => :f -> :f))
  (define (atanh x)
    (/ (- (ln (+ 1 x)) (ln (- 1 x))) (fromInt 2)))

  (declare atan2 ((Ord :f) (Elementary :f) => :f -> :f -> :f))
  (define (atan2 y x)
    "Computes the two-argument arctangent of y and x, which is roughly the same
as (atan (/ y x)) when defined and accounting for the quadrant of the (x,y)."
    (match (Tuple (<=> x 0) (<=> y 0))
      ((Tuple (GT) _)    (atan (/ y x)))
      ((Tuple (LT) (LT)) (- (atan (/ y x)) pi))
      ((Tuple (LT) _)    (+ (atan (/ y x)) pi))
      ((Tuple (EQ) (GT)) (/ pi 2))
      ((Tuple (EQ) (LT)) (/ pi -2))
      ((Tuple (EQ) (EQ)) 0)))

  (define (canonical-nth-root n x)
    "By definition implementation of `nth-root` for reals"
    (pow x (reciprocal (fromInt n))))

  (define (canonical-polar z)
    "By definition implementation of `polar`"
    (let x = (real-part z))
    (let y = (imag-part z))
    (let r = (sqrt (+ (* x x) (* y y))))
    (let theta =
      (cond
        ;; y /= 0
        ((or (/= 0 (floor y)) (/= 0 (ceiling y)))
         (* 2 (atan (/ (- r x) y))))
        ;; x > 0
        ((> (floor x) 0) 0)
        ;; x < 0
        ((< (ceiling x) 0) pi)
        ;; x = 0, y = 0
        ;; Pretend principal angle of 0 is 0
        (True 0)))
    (Tuple r theta)))

(cl:defmacro %define-real-float-elementary (coalton-type underlying-type)
  "Defines the elmentary instances for a lisp floating-point type"
  `(coalton-toplevel
     (define-instance (Trigonometric ,coalton-type)
       (define (sin x)
         (cond
           ;; CCL signals errors when applying trigonometric functions to NaN and infinity
           #+ccl
           ((or (nan? x) (== x infinity) (== x (negate infinity)))
            nan)

           (True
            (lisp ,coalton-type (x)
              (ff:with-float-traps-masked cl:t
                (cl:sin x))))))

       (define (cos x)
         (cond
           ;; CCL signals errors when applying trigonometric functions to NaN and infinity
           #+ccl
           ((or (nan? x) (== x infinity) (== x (negate infinity)))
            nan)

           (True
            (lisp ,coalton-type (x)
              (ff:with-float-traps-masked cl:t
                (cl:cos x))))))

       (define (tan x)
         (cond
           ;; CCL signals errors when applying trigonometric functions to NaN and infinity
           #+ccl
           ((or (nan? x) (== x infinity) (== x (negate infinity)))
            nan)

           (True
            (lisp ,coalton-type (x)
              (ff:with-float-traps-masked cl:t
                (cl:tan x))))))

       (define (asin x)
         (cond
           ;; CCL signals errors when applying trigonometric functions to NaN and infinity
           #+ccl
           ((or (nan? x) (== x infinity) (== x (negate infinity)))
            nan)

           (True
            (if (or (nan? x) (> x 1) (< x -1))
                nan
                (lisp ,coalton-type (x)
                  (ff:with-float-traps-masked cl:t
                    (cl:asin x)))))))

       (define (acos x)
         (if (or (nan? x) (> x 1) (< x -1))
             nan
             (lisp ,coalton-type (x)
               (ff:with-float-traps-masked cl:t
                 (cl:acos x)))))

       (define (atan x)
         (cond
           ;; CCL signals errors when applying trigonometric functions to NaN and infinity
           #+ccl
           ((or (nan? x) (== x infinity) (== x (negate infinity)))
            nan)

           (True
            (lisp ,coalton-type (x)
              (ff:with-float-traps-masked cl:t
                (cl:atan x)))))))

     (define-instance (Polar ,coalton-type)
       (define (phase x)
         (lisp ,coalton-type (x)
           (ff:with-float-traps-masked cl:t
             (cl:phase x))))
       (define (polar x)
         (Tuple (magnitude x) (phase x))))

     (define-instance (Exponentiable ,coalton-type)
       (define (pow x y)
         (cond
           ((or (nan? x) (nan? y)) nan)
           ((and (== x 0) (== y 0)) nan)
           ((< x 0) nan)

           ;; Allegro signals overflow and underflow errors when using infinity in exponents
           #+allegro
           ((or (== x infinity)
                (== y infinity))
            infinity)
           #+allegro
           ((or (== x (negate infinity))
                (== y (negate infinity)))
            (negate infinity))

           (True
            (lisp ,coalton-type (x y)
              (ff:with-float-traps-masked cl:t
                (cl:expt x y))))))

       (define (exp x)
         (cond
           ;; Allegro signals overflow and underflow errors when using infinity in exponents
           #+allegro
           ((== x infinity)
            infinity)
           #+allegro
           ((== x (negate infinity))
            (negate infinity))

           (True
            (lisp ,coalton-type (x)
              (ff:with-float-traps-masked cl:t
                (cl:let ((res (cl:exp x)))
                  (cl:if (cl:complexp res)
                         (cl:realpart res)
                         res)))))))

       (define (log b x)
         (cond
           ((or (nan? b) (nan? x)) nan)
           ((== b 1)
            (cond
              ((> x 1) infinity)
              ((and (< x 1) (>= x 0)) negative-infinity)
              (True nan)))
           ((and (> b 0) (> x 0))
            (lisp ,coalton-type (b x)
              (ff:with-float-traps-masked cl:t
                (cl:log x b))))
           (True nan)))

       (define (ln x)
         (cond
           ((nan? x) nan)
           ((> x 0)
            (lisp ,coalton-type (x)
              (ff:with-float-traps-masked cl:t
                (cl:log x))))
           ((< x 0) nan)
           (True negative-infinity))))

     (define-instance (Radical ,coalton-type)
       (define (sqrt x)
         (if (or (nan? x) (< x 0))
             nan
             (lisp ,coalton-type (x)
               (ff:with-float-traps-masked cl:t
                 (cl:sqrt x)))))
       (define (nth-root n x)
         (canonical-nth-root n x)))

     (define-instance (Elementary ,coalton-type)
       (define ee
         (lisp ,coalton-type ()
           (cl:exp (cl:coerce 1 ',underlying-type))))
       (define pi
         (lisp ,coalton-type ()
           (cl:coerce cl:pi ',underlying-type))))))

(%define-real-float-elementary Single-Float cl:single-float)
(%define-real-float-elementary Double-Float cl:double-float)

(cl:defmacro %define-standard-complex-instances (type)
  `(coalton-toplevel
     (define-instance (Complex ,type)
       (define (complex a b)
         (%Complex a b))
       (define (real-part a)
         (match a
           ((%Complex a _) a)))
       (define (imag-part a)
         (match a
           ((%Complex _ b) b))))))

(coalton-toplevel
  (define-instance ((Elementary :a) => Exponentiable (Complex :a))
    (define (ln z)
      ;; The principal natural log of a complex number
      (match (polar z)
        ((Tuple r theta)
         ;; ln r + iθ
         (complex (ln r) theta))))
    (define (exp z)
      ;; The natural exponential map of a complex number
      (let x = (real-part z))
      (let y = (imag-part z))
      ;; exp(x + iy) = (exp x) * (exp iy) = (exp x) * (complex (cos y) (sin y))
      (let ex = (exp x))
      (complex (* ex (cos y)) (* ex (sin y))))
    (define (pow x y)
      (exp (* y (ln x))))
    (define (log b x)
      (/ (ln x) (ln b))))

  (define-instance ((Elementary :a) => Radical (Complex :a))
    (define (sqrt z)
      (match (polar z)
        ((Tuple r theta)
         (let sqrt-r = (sqrt r))
         (let phi = (/ theta 2))
         ;; sqrt(z) = sqrt(r) (cos theta/2 + i sin theta/2)
         (complex (* sqrt-r (cos phi)) (* sqrt-r (sin phi))))))
    (define (nth-root n z)
      ;; nth-root(z) = nth-root(r) * exp (i theta / n)
      (match (polar z)
        ((Tuple r theta)
         (let nth-root-r = (nth-root n r))
         (let phi = (/ theta (fromInt n)))
         ;; sqrt(z) = sqrt(r) (cos theta/2 + i sin theta/2)
         (complex (* nth-root-r (cos phi)) (* nth-root-r (sin phi)))))))

  (define-instance ((Elementary :a) => Trigonometric (Complex :a))
    (define (sin z)
      (let x = (real-part z))
      (let y = (imag-part z))
      (let c = (exp (complex y (negate x))))
      ;; 1/2 i e^(y - i x) - 1/2 i e^(-y + i x)
      (* (/ ii 2) (- c (/ 1 c))))
    (define (cos z)
      (let x = (real-part z))
      (let y = (imag-part z))
      (let c = (exp (complex y (negate x))))
      ;; 1/2 e^(-y + i x) + 1/2 e^(y - i x)
      (* (/ 1 2) (+  c (/ 1 c))))
    (define (tan z)
      (let x = (real-part z))
      (let y = (imag-part z))
      (let c = (exp (complex y (negate x))))
      (let recip-c = (/ 1 c))
      ;; (i (e^(y - i x) - e^(-y + i x)))
      ;; / (e^(-y + i x) + e^(y - i x))
      (/ (* ii (- c recip-c)) (+ c recip-c)))
    (define (asin z)
      ;; asin z = -i ln (sqrt (- 1 z^2) + iz)
      (* (complex 0 -1)
         (ln
          (+ (sqrt (- 1 (* z z))) (* ii z)))))
    (define (acos z)
      ;; acos z = -i ln (i sqrt (-1 z^2) + z)
      (* (complex 0 -1)
         (ln
          (+ (* ii (sqrt (- 1 (* z z)))) z))))
    (define (atan z)
      ;; atan = (- i/2 (ln (i - z)/(i+z))
      (* (complex 0 (/ -1 2))
         (ln (/ (- ii z)
                (+ ii z))))))

  ;; This doesn't have much mathematical meaning
  (define-instance ((Elementary :a) => Polar (Complex :a))
    (define (phase zz)
      (match (polar zz)
        ((Tuple _ p) p)))
    (define (polar zz)
      (let x = (real-part zz))
      (let y = (imag-part zz))
      (let r = (magnitude zz))
      (let p =
        (if (== zz 0)
            0
            (* 2 (atan (/ y (+ r x))))))
      (Tuple r p)))

  (define-instance ((Elementary :a) => Elementary (Complex :a))
    (define ee (complex ee 0))
    (define pi (complex pi 0))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/ELEMENTARY")
