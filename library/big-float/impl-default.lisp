;;;; big-float.lisp
;;;;
;;;; Arbitrary precision floats using pure Coalton.

(in-package #:coalton-library/big-float)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;; Rounding modes
  (repr :enum)
  (define-type RoundingMode
    RoundNearAway
    RoundEven
    RoundToZero
    RoundUp
    RoundDown
    RoundFromZero
    RoundFaithful
    RoundDisable))

(cl:defvar *bf-precision* 53)
(cl:defvar *bf-rounding-mode* RoundDown)

(coalton-toplevel

  (define rndna
    "RouND to Nearest Away."
    RoundNearAway)
  (define rndn
    "RouND to Nearest, with the even rounding rule."
    RoundEven)
  (define rndz
    "RouND toward Zero."
    RoundToZero)
  (define rndu
    "RouND Up, toward positive infinity."
    RoundUp)
  (define rndd
    "RouND Down, toward negative infinity."
    RoundDown)
  (define rnda
    "RouND Away from zero."
    RoundFromZero)
  (define rndf
    "Faithful rounding (experimental)."
    RoundFaithful)

  (define (round-from-zero x)
    "Rounds to an integer away from zero."
    (* (sign x) (ceiling (abs x))))

  (define (dyadic-round-half-away-zero x)
    "Rounds a dyadic to the nearest integer with ties going away from zero."
    (* (sign x) (floor (+ (abs x) (Dyadic 1 -1)))))

  (define (fraction-round-half-away-zero x)
    "Rounds a fraction to the nearest integer with ties going away from zero."
    (* (sign x) (floor (+ (abs x) -1/2))))

  (declare set-precision! (UFix -> Unit))
  (define (set-precision! prec-bits)
    "Set the precision of Big-Float arithmetic to PREC-BITS bits."
    (unless (> prec-bits 0)
      (error "Precision must be positive."))
    (lisp Unit (prec-bits)
      (cl:setf *bf-precision* prec-bits)
      Unit))

  (declare set-rounding-mode! (RoundingMode -> Unit))
  (define (set-rounding-mode! r)
    "Set the global rounding mode for Big-Float operations."
    (lisp Unit (r)
      (cl:setf *bf-rounding-mode* r)
      Unit))

  (declare get-precision (Unit -> UFix))
  (define (get-precision _)
    "Get the current global precision of Big-Float arithmetic"
    (lisp UFix () *bf-precision*))

  (declare get-rounding-mode (Unit -> RoundingMode))
  (define (get-rounding-mode _)
    "Get the current rounding-mode of Big-Float arithmetic."
    (lisp RoundingMode ()
      *bf-rounding-mode*))

  (declare with-precision-rounding
           (UFix -> RoundingMode -> (Unit -> :a) -> :a))
  (define (with-precision-rounding prec-bits rnd f)
    "Call F with a temporary Big-Float PREC-BITS precision and RND rounding-mode."
    (lisp :a (f prec-bits rnd)
      (cl:let ((*bf-precision* prec-bits)
               (*bf-rounding-mode* rnd))
        (call-coalton-function f Unit))))

  (declare with-precision (UFix -> (Unit -> :a) -> :a))
  (define (with-precision prec-bits f)
    "Call F with a temporary Big-Float precision PREC-BITS."
    (with-precision-rounding prec-bits (get-rounding-mode) f))

  (declare with-rounding (RoundingMode -> (Unit -> :a) -> :a))
  (define (with-rounding rnd f)
    "Call F with a temporary Big-Float rounding-mode RND."
    (with-precision-rounding (get-precision) rnd f))

  (declare polylog-prec (UFix -> UFix))
  (define (polylog-prec n)
    "Raises (ilog 2 (get-precision)) to a power N or return 1 if prec is 1."
    (let x = (get-precision))
    (max 1
         (- (lisp UFix (x n) (cl:expt (cl:integer-length x) n)) 1)))

  (declare bit-length (Integer -> Integer))
  (define (bit-length n)
    (lisp Integer (n)
      (cl:integer-length n)))

  (declare ilog2-abs (Integer -> Integer))
  (define (ilog2-abs x)
    (- (bit-length (abs x)) 1))

  (define-type Big-Float
    (BFValue Dyadic)
    BFNegZero
    (BFConst (Unit -> Big-Float))
    BFInf
    BFNegInf
    BFNaN)

  ;; Can compare non-normalized big-floats or big-floats of different precisions.
  (define-instance (Eq Big-Float)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (BFValue x) (BFValue y))
         (== x y))
        ((Tuple (BFConst f) b)
         (== (f) b))
        ((Tuple a (BFConst f))
         (== a (f)))
        ((Tuple (BFNegZero) a) (== 0 a))
        ((Tuple a (BFNegZero)) (== a 0))
        ((Tuple (BFInf) (BFInf)) True)
        ((Tuple (BFNegInf) (BFNegInf)) True)
        (_ False))))

  (define-instance (Ord Big-Float)
    (define (<=> a b)
      (match (Tuple a b)
        ((Tuple (BFValue x) (BFValue y))
         (<=> x y))
        ((Tuple (BFConst f) _)
         (<=> (f) b))
        ((Tuple _ (BFConst f))
         (<=> a (f)))
        ((Tuple (BFNegZero) _) (<=> 0 a))
        ((Tuple _ (BFNegZero)) (<=> a 0))
        ((Tuple (BFInf) (BFInf)) EQ)
        ((Tuple (BFNegInf) (BFNegInf)) EQ)
        ((Tuple (BFInf) (BFNegInf)) GT)
        ((Tuple (BFNegInf) (BFInf)) LT)
        ((Tuple (BFNaN) (BFNaN)) EQ)
        ;; When sorting, NaNs will go to the left
        ((Tuple (BFNaN) _) LT)
        ((Tuple _ (BFNaN)) GT)
        ((Tuple (BFInf) _) GT)
        ((Tuple (BFNegInf) _) LT)
        ((Tuple _ (BFInf)) LT)
        ((Tuple _ (BFNegInf)) GT))))

  (define (normalize a)
    "Normalizes a Big-Float using current rounding and precision."
    (let k = (get-precision))
    (match (Tuple a (get-rounding-mode))
      ;; normalize cannot use fromInt
      ((Tuple (BFValue (Dyadic 0 _)) _) (BFValue (Dyadic 0 0)))
      ((Tuple x (RoundDisable)) x)
      ((Tuple (BFValue (Dyadic m e)) rnd-mode)
       (let j =  (ilog2-abs m))
       (let delta = (- (toInteger k) j))
       (let rounder =
         (match rnd-mode
           ((RoundDown) floor)
           ((RoundToZero) truncate)
           ((RoundUp) ceiling)
           ((RoundEven) round)
           ((RoundNearAway) dyadic-round-half-away-zero)
           ((RoundFromZero) round-from-zero)
           ;; TODO: Implement round faithful.
           ((RoundFaithful) round)
           (_ (error "Current rounding mode not implemented"))))
       (if (>= delta 0)
           (BFValue (Dyadic (lsh m delta) (- e delta)))
           (BFValue
            (match (Dyadic (rounder (Dyadic m delta)) (- e delta))
              ((Dyadic 0 n)
               (Dyadic (sign m) (- n 1)))
              (d d)))))
      (_ a)))

  (specialize negate bf-negate (Big-Float -> Big-Float))
  (define (bf-negate x)
    "Negates a Big-Float"
    (match x
      ((BFValue (Dyadic n k))
       (if (== n 0)
           BFNegZero
           (BFValue (Dyadic (negate n) k))))
      ((BFConst f) (negate (f)))
      ((BFNegZero) 0)
      ((BFInf) BFNegInf)
      ((BFNegInf) BFInf)
      ((BFNaN) BFNaN)))

  (define-instance (Num Big-Float)
    (define (+ a b)
      (match (Tuple a b)
        ((Tuple (BFValue x) (BFValue y))
         (normalize (BFValue (+ x y))))
        ((Tuple (BFConst f) b)
         (+ (f) b))
        ((Tuple a (BFConst f))
         (+ a (f)))
        ((Tuple (BFNegZero) (BFNegZero)) BFNegZero)
        ((Tuple (BFNegZero) a) a)
        ((Tuple a (BFNegZero)) a)
        ((Tuple (BFInf) (BFInf)) BFInf)
        ((Tuple (BFNegInf) (BFNegInf)) BFNegInf)
        (_ BFNaN)))
    (define (- a b)
      (+ a (bf-negate b)))
    (define (* a b)
      (match (Tuple a b)
        ((Tuple (BFValue (Dyadic 0 _)) (BFValue b))
         (if (< b 0)
             BFNegZero
             0))
        ((Tuple (BFValue a) (BFValue (Dyadic 0 _)))
         (if (< a 0)
             BFNegZero
             0))
        ((Tuple (BFValue a) (BFValue b))
         (normalize (BFValue (* a b))))
        ((Tuple (BFConst f) b)
         (* (f) b))
        ((Tuple a (BFConst f))
         (* a (f)))
        ((Tuple (BFNegZero) (BFNegZero)) 0)
        ((Tuple (BFNegZero) a)
         (if (>= a 0) BFNegZero 0))
        ((Tuple a (BFNegZero))
         (if (>= a 0) BFNegZero 0))
        ((Tuple (BFInf) (BFInf)) BFInf)
        ((Tuple (BFNegInf) (BFNegInf)) BFInf)
        ((Tuple (BFInf) a)
         (cond
           ((nan? a) BFNaN)
           ((< a 0) BFNegInf)
           ((> a 0) BFInf)
           ((== a 0) BFNaN)))
        ((Tuple a (BFInf)) (* BFInf a))
        (_ BFNaN)))
    (define (fromInt x) (normalize (BFValue (fromInt x)))))

  (define-instance (Reciprocable Big-Float)
    (define (reciprocal x)
      (match x
        ((BFValue (Dyadic 0 _)) BFInf)
        ((BFNegZero) BFNegInf)
        ((BFValue (Dyadic m k))
         (let p = (+ 1 (toInteger (get-precision))))
         (normalize
          (BFValue (Dyadic (div (lsh 1 (- p k)) m) (negate p)))))
        ((BFInf) 0)
        ((BFNegInf) BFNegZero)
        ((BFConst f) (reciprocal (f)))
        ((BFNaN) BFNaN)))
    (define (/ a b)
      (when (== b 0)
        (return (* a (reciprocal b))))
      (match (Tuple a b)
        ((Tuple (BFValue (Dyadic m1 k1)) (BFValue (Dyadic m2 k2)))
         (let p = (+ 1 (toInteger (get-precision))))
         (normalize
          (BFValue
           (Dyadic
            (div (lsh m1 (+ p (- k1 k2))) m2) (negate p)))))
        (_ (* a (reciprocal b))))))

  (define-instance (Dividable Integer Big-Float)
    (define (general/ a b)
      (let p = (+ 1 (toInteger (get-precision))))
      (if (== b 0)
          (* (fromInt a) (reciprocal (fromInt b)))
          (normalize (BFValue (Dyadic (div (lsh a p) b) (negate p)))))))

  (define-instance (Into Single-Float Big-Float)
    (define (into a)
      (cond
        ((== a infinity) BFInf)
        ((== a negative-infinity) BFNegInf)
        ((nan? a) BFNaN)
        (True
         (normalize
          (lisp Big-Float (a)
            (cl:multiple-value-bind (n k s)
                (cl:integer-decode-float a)
              (cl:if (cl:and (cl:= n 0) (cl:= s 0))
                     BFNegZero
                     (BFValue (Dyadic (cl:* s n) k))))))))))

  (define-instance (Into Double-Float Big-Float)
    (define (into a)
      (cond
        ((== a infinity) BFInf)
        ((== a negative-infinity) BFNegInf)
        ((nan? a) BFNaN)
        (True
         (normalize
          (lisp Big-Float (a)
            (cl:multiple-value-bind (n k s)
                (cl:integer-decode-float a)
              (cl:if (cl:and (cl:= n 0) (cl:= s 0))
                     BFNegZero
                     (BFValue (Dyadic (cl:* s n) k))))))))))

  (define-instance (Into Integer Big-Float)
    (define (into a)
      (fromInt a)))

  (define-instance (Into Fraction Big-Float)
    (define (into a)
      (general/ (numerator a) (denominator a))))

  (define-instance (Real Big-Float)
    (define (real-approx _ x) (to-fraction x)))

  (define-instance (Rational Big-Float)
    (define (to-fraction x)
      (match x
        ((BFValue d)
         (to-fraction d))
        ((BFConst f)
         (best-approx (f)))
        ((BFNegZero) 0)
        ((BFInf) (error "Cannot rationalize Inf"))
        ((BFNegInf) (error "Cannot rationalize -Inf"))
        ((BFNaN) (error "Cannot rationalize NaN"))))
    (define (best-approx x)
      (coalton-library/math/real::rational-approx (get-precision) x)))

  (define-instance (Quantizable Big-Float)
    (define (proper x)
      (match x
        ((BFValue d)
         (match (proper d)
           ((Tuple n d)
            (Tuple n (BFValue d)))))
        ((BFConst f)
         (proper (f)))
        ((BFNegZero) (Tuple 0 BFNegZero))
        ((BFInf) (Tuple 0 BFInf))
        ((BFNegInf) (Tuple 0 BFNegInf))
        ((BFNaN) (Tuple 0 BFNaN))))
    (define (floor x)
      (match x
        ((BFValue d) (floor d))
        ((BFConst f) (floor (f)))
        ((BFNegZero) 0)
        ((BFInf) (error "Cannot floor Inf"))
        ((BFNegInf) (error "Cannot floor -Inf"))
        ((BFNaN) (error "Cannot floor NaN"))))
    (define (ceiling x)
      (match x
        ((BFValue d) (ceiling d))
        ((BFConst f) (ceiling (f)))
        ((BFNegZero) 0)
        ((BFInf) (error "Cannot ceiling Inf"))
        ((BFNegInf) (error "Cannot ceiling -Inf"))
        ((BFNaN) (error "Cannot ceiling NaN")))))

  (define-instance (Transfinite Big-Float)
    (define nan BFNaN)
    (define (nan? x)
      (match x
        ((BFNan) True)
        (_ False)))
    (define infinity BFInf)
    (define (infinite? x)
      (match x
        ((BFInf) True)
        ((BFNegInf) True)
        (_ False))))

  (specialize negative-infinity bf-neg-inf Big-Float)
  (define bf-neg-inf BFNegInf)

  (define (scale x n)
    "Scales a Big-Float X=a*2^k to a*2^(+ k n)."
    (match x
      ((BFValue d) (BFValue (dyadic:scale d n)))
      ((BFConst f) (scale (f) n))
      ((BFNegZero) BFNegZero)
      ((BFInf) BFInf)
      ((BFNegInf) BFNegInf)
      ((BFNaN) BFNaN)))

  (define (reciprocal-sqrt a)
    "Non-normalized reciprocal of the sqrt of a"
    (let prec = (get-precision))
    (match a
      ((BFValue (Dyadic m j))
       (cond
         ((< m 0) BFNaN)
         ((== m 0) BFInf)
         ((== a 1) 1)
         (True
          (let l = (bit-length m))
          (let ((newton-rec
                  (fn (x)
                    (let y = (dyadic:scale (* x (- 3 (* (Dyadic m j) (* x x)))) -1))
                    (let z = (dyadic:shift prec y))
                    (if (== z x)
                        z
                        (newton-rec z)))))
            (BFValue (newton-rec (Dyadic 1 (negate (div (+ j l) 2)))))))))
      ((BFConst f) (reciprocal-sqrt (f)))
      ((BFNegZero) BFNegInf)
      ((BFInf) 0)
      (_ BFNaN))))

(coalton-library/math/complex::%define-standard-complex-instances Big-Float)

(coalton-toplevel
  ;; SeriesSplit/SeriesResult could be extended to any ring (e.g. polynomials)
  (define-type SeriesSplit
    "An nth element of series (SeriesSplit a(n) p(n) b(n) q(n)).
See `binary-split` step 1."
    (SeriesSplit Integer Integer Integer Integer))

  (define-type SeriesResult
    "An intermediary result of binary splitting series (SeriesResult P Q B T).
See `binary-split` step 2."
    (SeriesResult Integer Integer Integer Integer))

  (define-instance (Semigroup SeriesResult)
    (define (<> a b)
      "Combine binary split results. See `binary-split` step 3."
      (match (Tuple a b)
        ((Tuple (SeriesResult ap aq ab at)
                (SeriesResult bp bq bb bt))
         (SeriesResult (* ap bp) (* aq bq) (* ab bb)
                       (+ (* (* bb bq) at) (* (* ab ap) bt)))))))

  (declare make-result (SeriesSplit -> SeriesResult))
  (define (make-result b)
    "Calculations the current intermediary result for a given element of the series.
See `binary-split`."
    (match b
      ((SeriesSplit a p q b)
       (SeriesResult p q b (* a p)))))

  (declare eval-result ((Dividable Integer :a) => SeriesResult -> :a))
  (define (eval-result r)
    "Evaluates the final result of a SeriesResult. See `binary-split` step 4."
    (match r
      ((SeriesResult _ q b t)
       (general/ t (* b q)))))

  (declare recip-result ((Dividable Integer :a) => SeriesResult -> :a))
  (define (recip-result r)
    "Evaluates the reciprocal of `make-result`."
    (match r
      ((SeriesResult _ q b t)
       (general/ (* b q) t))))

  (declare binary-split ((UFix -> SeriesSplit) -> UFix -> UFix -> SeriesResult))
  (define (binary-split s n1 n2)
    "Implements a binary splitting algorithm described by. In short,
 1) Given a series S(n) = Σ^n_i=0 a(i)p(0)...p(i)/b(i)q(0)...q(i)
 2) Split P(n1,n2)=p(n1)...p(n2−1), Q(n1,n2)=q(n1)...q(n2−1), B(n1,n2)=b(n1)...b(n2−1), and T=BQS
 3) Recombined as P(a,c)=P(a,b)P(b,c),Q(a,c)=Q(a,b)Q(b,c),B(a,c)=B(a,b)B(b,c)
                 T(a,c)=B(b,c)Q(b,c)T(a,b)+B(a,b)P(a,b)T(b,c)
 4) With S(n) = T(0,n)/(B(0,n)Q(0,n))

Source: https://www.ginac.de/CLN/binsplit.pdf
"
    (let n = (- n2 n1))
    (cond
      ((<= n 0) (make-result (s n1)))
      ((== n 1)
       (<> (make-result (s n1))
           (make-result (s (+ 1 n1)))))
      ((== n 2)
       (<> (make-result (s n1))
           (<> (make-result (s (+ 1 n1)))
               (make-result (s (+ 2 n1))))))
      ((== n 3)
       (<> (<> (make-result (s n1))
               (make-result (s (+ 1 n1))))
           (<> (make-result (s (+ 2 n1)))
               (make-result (s (+ 3 n1))))))
      ((== n 4)
       (<> (<>
            (<> (make-result (s n1))
                (make-result (s (+ 1 n1))))
            (<> (make-result (s (+ 2 n1)))
                (make-result (s (+ 3 n1)))))
           (make-result (s (+ 4 n1)))))
      ((== n 5)
       (<> (<>
            (<> (make-result (s n1))
                (make-result (s (+ 1 n1))))
            (<> (make-result (s (+ 2 n1)))
                (make-result (s (+ 3 n1)))))
           (<> (make-result (s (+ 4 n1)))
               (make-result (s (+ 5 n1))))))
      (True
       (let nm = (div n 2))
       (let n3 = (+ n1 nm))
       (<> (binary-split s n1 n3)
           (binary-split s (+ n3 1) n2)))))

  (declare eval-series (SeriesSplit -> (UFix -> SeriesSplit) -> (UFix -> SeriesResult)))
  (define (eval-series x f n)
    (<> (make-result x)
        (binary-split f 1 n)))

  (declare make-results ((Dividable Integer :a) => Ufix -> SeriesSplit -> (Integer -> SeriesSplit) -> :a))
  (define (make-results n start f)
    "For a binary-split series, given an initial SeriesSPlit X and a function that
returns the nth SeriesSplit, return the series evaluated to the Nth element."
    (eval-result
     (eval-series start (fn (m) (f (toInteger m))) n)))

  (declare make-recip-results ((Dividable Integer :a) => Ufix -> SeriesSplit -> (Integer -> SeriesSplit) -> :a))
  (define (make-recip-results n start f)
    "Reciprocal of `make-result`."
    (recip-result
     (eval-series start (fn (m) (f (toInteger m))) n)))

  ;; XXX: We can pre-compute some values here
  (define (bf-pi _)
    "Return the value of pi to the currently set precision."
    ;; Chudnovsky algorithm
    (progn
      (let a = (* 12 13591409))
      (let b = (* 12 545140134))
      (let c = 640320)
      (let c3 = (^ c 3))
      ;; d = c^3/24
      (let j = (div c3 24))
      ;; C^3/2 / 12pi
      ;;  = Σ_n (6n)!(545140134n + 13591409)/((3q)!(n!)^3(-262537412640768000)^n
      ;; a(n)=A+nB, b(n)=1, p(0)=1, q(0)=1,
      ;; p(n)=−(6n−5)(2n−1)(6n−1), q(n)=1
      (reciprocal
       (*
        (make-results
         (polylog-prec 2) (SeriesSplit a 1 1 1)
         (fn (n)
           (let 2n = (* 2 n))
           (let 6n = (* 3 2n))
           (let n2 = (* n n))
           (SeriesSplit
            (+ a (* b n))
            (negate (* (* (- 6n 5) (- 2n 1)) (- 6n 1)))
            (* j (* n2 n)) 1)))
        (reciprocal-sqrt (BFValue (dyadic:simplify-integer c3)))))))

  (declare bf-ln2 (Unit -> Big-Float))
  (define (bf-ln2)
    "Return the value of (ln 2) to the currently set precision."
    ;; 2/3 Σ_n 1/(9^n(2n+1))
    ;; a(n)=2, b(n)=(6n + 3), p(n)=1, q(0)=1, q(n)=9
    (make-results
     (polylog-prec 4) (SeriesSplit 2 1 1 3)
     (fn (n)
       (SeriesSplit 2 1 9 (+ (* 6 n) 3)))))

  (define-instance (Trigonometric Big-Float)
    (define (sin x)
      (let prec = (polylog-prec 4))
      ;; sin(x) = Σ_n ((-1)^n x^(1 + 2 n))/((1 + 2 n)!)
      ;; a(n)=1, b(n)=1, p(0)=u ,q(0)=v
      ;; p(n)= −u^2, q(n)=(2n)(2n+1)v^2
      (match x
        ((BFValue (Dyadic m k))
         (if (< k 0)
             (make-results
              prec (SeriesSplit 1 m (rsh 1 k) 1)
              (fn (n)
                (let 2n = (* 2 n))
                (SeriesSplit 1 (negate (* m m)) (rsh (* 2n (+ 2n 1)) (* 2 k)) 1)))
             (progn
               (let u = (lsh m k))
               (make-results
                prec (SeriesSplit 1 u 1 1)
                (fn (n)
                  (let 2n = (* 2 n))
                  (SeriesSplit 1 (negate (* u u)) (* 2n (+ 2n 1)) 1))))))
        ((BFConst f) (sin (f)))
        ((BFNegZero) BFNegZero)
        (_ BFNaN)))
    (define (cos x)
      (let prec = (polylog-prec 4))
      (match x
        ((BFValue (Dyadic m k))
         ;; cos(x) = Σ_n ((-1)^n x^(2 n))/((2 n)!)
         ;; a(n)=1, b(n)=1, p(0)=1 ,q(0)=v
         ;; p(n)= −u^2, q(n)=(2n)(2n+1)v^2
         (if (< k 0)
             (make-results
              prec (SeriesSplit 1 1 1 1)
              (fn (n)
                (let 2n = (* 2 n))
                (SeriesSplit 1 (negate (* m m)) (rsh (* 2n (- 2n 1)) (* 2 k)) 1)))
             (progn
               (let u = (lsh m k))
               (make-results
                prec (SeriesSplit 1 1 1 1)
                (fn (n)
                  (let 2n = (* 2 n))
                  (SeriesSplit 1 (negate (* u u)) (* 2n (- 2n 1)) 1))))))
        ((BFConst f) (cos (f)))
        ((BFNegZero) 1)
        (_ BFNaN)))
    (define (tan x)
      (match x
        ((BFValue d)
         (/ (sin (BFValue d)) (cos (BFValue d))))
        ((BFConst f) (tan (f)))
        ((BFNegZero) BFNegZero)
        (_ BFNaN)))
    (define (atan x)
      (let prec = (get-precision))
      ;; atan(x) =  Σ_n ((-1)^n x^(1 + 2 n))/(1 + 2 n) for abs(x)<1
      ;; a(n)=1, b(n)=2n+1, q(n)=v^2, p(0)=u,p(n)=−u^2
      (let compute-atan =
        (fn (u v)
          (make-results
           (polylog-prec 4)
           (SeriesSplit 1 u v 1)
           (fn (n)
             (SeriesSplit
              1 (negate (* u u)) (* v v) (+ (* 2 n) 1))))))
      (normalize
       (with-rounding RoundDisable
         (fn ()
           (match x
             ((BFValue (Dyadic m k))
              (cond
                ((< (abs x) 1)
                 (let f = (to-fraction (Dyadic m k)))
                 (compute-atan (numerator f) (denominator f)))
                ((< x -1)
                 (- (scale (negate (bf-pi)) -1)
                    (atan (reciprocal x))))
                ((> x 1)
                 (let f = (to-fraction (Dyadic m k)))
                 (- (scale (bf-pi) -1)
                    (atan (reciprocal x))))
                ((== x 1)
                 (scale (bf-pi) -2))
                ((== x -1)
                 (scale (negate (bf-pi)) -2))))
             ((BFConst f) (atan (f)))
             ((BFNegZero) BFNegZero)
             ((BFInf) (scale (bf-pi) -1))
             ((BFNegInf) (scale (negate (bf-pi)) -1))
             (_ BFNaN))))))
    (define (asin x)
      (atan (* x (reciprocal-sqrt (- 1 (* x x))))))
    (define (acos x)
      (- (scale (bf-pi) -1) (asin x))))

  (define-instance (Exponentiable Big-Float)
    (define (exp x)
      (let prec = (get-precision))
      (match x
        ((BFValue (Dyadic m k))
         ;; e^x = Σ_n x^n/(n!)
         ;; a(n)=1, b(n)=1, p(0)=q(0)=1, p(n)=u, q(n)=nv
         (if (< k 0)
             (make-results
              (polylog-prec 4) (SeriesSplit 1 1 1 1)
              (fn (n)
                (SeriesSplit 1 m (rsh n k) 1)))
             (make-results
              (polylog-prec 4) (SeriesSplit 1 1 1 1)
              (fn (n)
                (SeriesSplit 1 (lsh m k) n 1)))))
        ((BFConst f) (exp (f)))
        ((BFNegZero) 1)
        ((BFInf) BFInf)
        ((BFNegInf) 0)
        (_ BFNaN)))
    (define (ln x)
      (let prec = (polylog-prec 4))
      (match x
        ((BFValue (Dyadic m k))
         ;; ln(x) = - Σ_n ((-1)^n (-1 + x)^n)/n for abs(-1 + x)<1
         ;; a(n)=1, b(n)=n+1, q(n)=v, p(0)=u, p(n)=−u
         (cond
           ((< x 0) BFNaN)
           ((== x 0) BFNegInf)
           ((== x 1) 0)
           ((< x 2)
            (let f = (- (to-fraction x) 1))
            (let u = (numerator f))
            (let v = (denominator f))
            (make-results
             prec (SeriesSplit 1 u v 1)
             (fn (n)
               (SeriesSplit 1 (negate u) v (+ n 1)))))
           ((== x 2) (bf-ln2))
           (True
            (let (Dyadic m k) = (Dyadic m k))
            (let j = (if (< m 1)
                         1
                         (bit-length m)))
            (let y = (BFValue (Dyadic m (negate j))))
            (when (> y 2)
              (error "Preventing infinite loop in ln."))
            (let l =  (+ j (toInteger k)))
            ;; ln (u/v + 1) + l * ln 2
            (normalize
             (with-rounding RoundDisable
               (fn ()
                 (+ (ln y)
                    (* (if (< l 0)
                           (general/ 1 (negate l))
                           (fromInt l))
                       (bf-ln2)))))))))
        ((BFConst f) (ln (f)))
        ((BFNegZero) BFNegInf)
        ((BFInf) BFInf)
        (_ BFNaN)))
    (define (pow x y)
      (match (Tuple x y)
        ((Tuple (BFValue x) (BFValue y))
         (cond
           ((and (== x 0) (== y 0)) BFNaN)
           ((== x 0) 0)
           ((or (== x 1) (== y 0)) 1)
           ((< y 0)
            (let fl-x = (floor y))
            (if (== (fromInt fl-x) y)
                (reciprocal (BFValue (^ x (negate fl-x))))
                BFNaN))
           (True
            (let fl-x = (floor y))
            (if (== (fromInt fl-x) y)
                (normalize (BFValue (^ x fl-x)))
                (exp (* (BFValue y) (ln (BFValue x))))))))
        ((Tuple (BFConst f) y) (pow (f) y))
        ((Tuple x (BFConst f)) (pow x (f)))
        (_ (exp (* y (ln x))))))
    (define (log b x)
      (/ (ln x) (ln b))))

  (define-instance (Radical Big-Float)
    (define (sqrt x)
      (reciprocal (reciprocal-sqrt x)))
    (define (nth-root n x)
      (pow x (general/ 1 n))))

  (define-instance (Polar Big-Float)
    (define (phase z)
      (let prec = (get-precision))
      (let x = (real-part z))
      (let y = (imag-part z))
      (match (Tuple (<=> x 0) (<=> y 0))
        ((Tuple (GT) _)    (atan (/ y x)))
        ((Tuple (LT) (LT)) (- (atan (/ y x)) pi))
        ((Tuple (LT) _)    (+ (atan (/ y x)) pi))
        ((Tuple (EQ) (GT)) (/ pi 2))
        ((Tuple (EQ) (LT)) (/ pi -2))
        ((Tuple (EQ) (EQ)) 0)))
    (define (polar z)
      (Tuple (magnitude z) (phase z))))

  (declare bf-ee (Unit -> Big-Float))
  (define (bf-ee)
    "Return the value of ee = exp(1) to the currently set precision."
    (exp 1))

  (define-instance (Elementary Big-Float)
    (define pi (BFConst bf-pi))
    (define ee (BFConst bf-ee)))

  (define (big-float->string x)
    "Returns a Big-Float in scientific notation."
    (match x
      ((BFValue (Dyadic 0 _)) "0.0e+0")
      ((BFValue d)
       (let y = (to-fraction d))
       (let prec = (ilog 10 (lsh 2 (get-precision))))
       (let z = (round (* y (^ 10 prec))))
       (let powr = (ilog 10 (max 1 (abs z))))
       (lisp String (z powr prec)
         (cl:multiple-value-bind (t r) (cl:truncate z (cl:expt 10 powr))
           (cl:format
            nil (cl:concatenate
                 'cl:string "~d.~" ; Integer part and sign
                 (cl:princ-to-string powr) ; Left pad of zeros
                 ",'0de~@d") ; Decimal part and exponent.
            t (cl:abs r) (cl:- powr prec)))))
      ((BFConst f) (big-float->string (f)))
      ((BFNegZero) "-0.0e+0")
      ((BFInf) "Inf")
      ((BFNegInf) "-Inf")
      ((BFNaN) "NaN"))))

(cl:defmethod cl:print-object ((obj big-float/bfvalue) out)
  (cl:format out (big-float->string obj)))
(cl:defmethod cl:print-object ((obj big-float/bfnegzero) out)
  (cl:format out (big-float->string obj)))
(cl:defmethod cl:print-object ((obj big-float/bfconst) out)
  (cl:format out (big-float->string obj)))
(cl:defmethod cl:print-object ((obj big-float/bfinf) out)
  (cl:format out (big-float->string obj)))
(cl:defmethod cl:print-object ((obj big-float/bfneginf) out)
  (cl:format out (big-float->string obj)))
(cl:defmethod cl:print-object ((obj big-float/bfnan) out)
  (cl:format out (big-float->string obj)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/BIG-FLOAT")
