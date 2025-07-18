;;;; big-float.lisp
;;;;
;;;; Benchmarks for arbitrary precision floats

(cl:in-package #:benchmark-big-float)

(cl:defvar *big-float-bench-precision*
  #-coalton-portable-bigfloat 10000
  #+coalton-portable-bigfloat 100)
(cl:defvar *big-float-bench-iterations*
  #-coalton-portable-bigfloat 1000
  #+coalton-portable-bigfloat 10)

(define-benchmark big-trig ()
  "Benchmark at N precision big-float trigonometric functions."
  (declare (optimize speed))
  (loop :repeat  *big-float-bench-iterations*
        :do (with-benchmark-sampling
              (benchmark-big-float/native::big-trig
               *big-float-bench-precision*
               (* (- (random 2)) (random 100.0d0)))))
  (report trivial-benchmark::*current-timer*))

(define-benchmark big-inv-trig ()
  "Benchmark at N precision big-float inverse trigonometric functions."
  (declare (optimize speed))
  (loop :repeat *big-float-bench-iterations*
        :do (with-benchmark-sampling
              (benchmark-big-float/native::big-inv-trig
               *big-float-bench-precision*
               (* (- (random 2)) (random 1.0d0)))))
  (report trivial-benchmark::*current-timer*))

(define-benchmark big-ln-exp ()
  "Benchmark at N precision big-float ln and exp."
  (declare (optimize speed))
  (loop :repeat *big-float-bench-iterations*
        :do (with-benchmark-sampling
              (benchmark-big-float/native::big-ln-exp
               *big-float-bench-precision*
               (* (- (random 2)) (random 100.0d0)))))
  (report trivial-benchmark::*current-timer*))

(define-benchmark big-sqrt ()
  "Benchmark at N precision big-float square roots."
  (declare (optimize speed))
  (loop :repeat *big-float-bench-iterations*
        :do (with-benchmark-sampling
              (benchmark-big-float/native::big-sqrt
               *big-float-bench-precision*
               (random 100.0d0))))
  (report trivial-benchmark::*current-timer*))

(define-benchmark big-mult-constants ()
  "Benchmark at N precision big-float multiplication of pi and euler's number."
  (declare (optimize speed))
  (loop :repeat *big-float-bench-iterations*
        :do (with-benchmark-sampling
              (benchmark-big-float/native::big-sqrt
               *big-float-bench-precision*
               (* (- (random 2)) (random 100.0d0)))))
  (report trivial-benchmark::*current-timer*))

(cl:in-package #:benchmark-big-float/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 1)))

(coalton-toplevel
  (declare big-trig (UFix -> Double-Float -> Big-Float))
  (define (big-trig n x)
    (with-precision n
      (fn ()
        (let x = (into x))
        (tan (sin (cos x))))))

  (declare big-inv-trig (UFix -> Double-Float -> Big-Float))
  (define (big-inv-trig n x)
    (with-precision n
      (fn ()
        (let x = (into x))
        (atan (+ (asin x) (acos x))))))

  (declare big-ln-exp (UFix -> Double-Float -> Big-Float))
  (define (big-ln-exp n x)
    (with-precision n
      (fn ()
        (let x = (into x))
        (ln (exp x)))))

  (declare big-sqrt (UFix -> Double-Float -> Big-Float))
  (define (big-sqrt n x)
    (with-precision n
      (fn ()
        (let x = (into x))
        (sqrt x))))

  (define (big-mult-constants n x)
    (with-precision n
      (fn ()
        (let x = (into x))
        (* x (* pi ee))))))
