;;;; big-float.lisp
;;;;
;;;; Benchmarks for arbitrary precision floats

(defpackage #:coalton-benchmarks/big-float
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-benchmarking
   #:coalton-library/big-float)
  (:local-nicknames
   (#:math #:coalton-library/math))
  (:export
   #:*big-float-bench-precision*
   #:*big-float-bench-iterations*
   #:big-trig
   #:big-inv-trig
   #:big-ln-exp
   #:big-sqrt
   #:big-mult-constants))

(cl:in-package #:coalton-benchmarks/big-float)

(cl:defvar *big-float-bench-precision*
  #-coalton-portable-bigfloat 10000
  #+coalton-portable-bigfloat 100)
(cl:defvar *big-float-bench-iterations*
  #-coalton-portable-bigfloat 1000
  #+coalton-portable-bigfloat 10)

(coalton-toplevel

  (define (big-float-bench-precision)
    (lisp UFix ()
      *big-float-bench-precision*))

  (define (big-float-bench-iterations)
    (lisp UFix ()
      *big-float-bench-iterations*)))

(coalton-toplevel
  (define (random-double-float)
    (lisp Double-Float ()
      (cl:* (cl:- (cl:random 2)) (cl:random 100.0d0)))))

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

  (define (big-mult-const n x)
    (with-precision n
      (fn ()
        (let x = (into x))
        (* x (* math:pi math:ee))))))

(cl:defmacro define-big-float-benchmark (name)
  (cl:let ((func name)
           (name (cl:string name))
           (rand (cl:* (cl:- (cl:random 2)) (cl:random 100.0d0))))
    `(coalton (coalton-benchmarking/benchmarking::%define-benchmark ,name
                                                                    (big-float-bench-iterations)
                                                                    (fn ()
                                                                      (,func (big-float-bench-precision)
                                                                             ,rand)
                                                                      Unit)
                                                                    False))))


(define-big-float-benchmark big-trig)

(define-big-float-benchmark big-inv-trig)

(define-big-float-benchmark big-ln-exp)

(define-big-float-benchmark big-sqrt)

(define-big-float-benchmark big-mult-const)
