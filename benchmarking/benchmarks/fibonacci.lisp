;;;; fibonacci.lisp
;;;;
;;;; Benchmarks for different methods of generating fibonacci numbers

(defpackage #:coalton-benchmarks/fibonacci
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-benchmarking)
  (:local-nicknames
   (#:seq #:coalton-library/seq))
  (:export
   #:lisp-fib
   #:fib
   #:fib-generic
   #:fib-generic-wrapped
   #:fib-monomorphized
   #:fib-optional
   #:fib-monomorphized-optional))

(in-package #:coalton-benchmarks/fibonacci)

;;;
;;; Lisp fibonacci
;;;

(cl:defun lisp-fib (n)
  (cl:declare (cl:type cl:integer n)
              (cl:values cl:integer)
              (cl:optimize (cl:speed 3) (cl:safety 0)))
  (cl:when (cl:= n 0)
    (cl:return-from lisp-fib 0))

  (cl:when (cl:= n 1)
    (cl:return-from lisp-fib 1))

  (cl:+ (lisp-fib (cl:- n 1)) (lisp-fib (cl:- n 2))))

;;;
;;; Coalton fibonacci
;;;

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(coalton-toplevel

 (declare fib (Integer -> Integer))
 (define (fib n)
    (when (== n 0)
      (return 0))

    (when (== n 1)
      (return 1))

    (+ (fib (- n 1)) (fib (- n 2))))

  (declare fib-generic (Num :a => :a -> :a))
  (define (fib-generic n)
    (when (== n 0)
      (return 0))

    (when (== n 1)
      (return 1))

    (+ (fib-generic (- n 1)) (fib-generic (- n 2))))

  (declare fib-generic-wrapped (Integer -> Integer))
  (define (fib-generic-wrapped x)
    (fib-generic x))

  (monomorphize)
  (declare fib-monomorphized (Integer -> Integer))
  (define (fib-monomorphized x)
    (fib-generic x))

  (declare fib-generic-optional (Integer -> Optional Integer))
  (define (fib-generic-optional x)
    (fib-generic (Some x)))

  (monomorphize)
  (declare fib-monomorphized-optional (Integer -> Optional Integer))
  (define (fib-monomorphized-optional x)
    (fib-generic (Some x))))

;;;
;;; Benchmarks
;;;

(define-parameterized-benchmark rec-fib 1000
  (fn (x)
    (fib x)
    Unit)
  (seq:make 10 15 20 25))

(define-parameterized-benchmark rec-fib-generic 500
  (fn (x)
    (fib-generic-wrapped x)
    Unit)
  (seq:make 10 15 20 25)
  :detect-convergence? cl:t)

(define-benchmark rec-fib-lisp 1000
  (fn ()
    (lisp Unit ()
      (lisp-fib 20)
      Unit)))

(define-benchmark rec-fib-mono 1000
  (fn ()
    (lisp Unit ()
      (fib-monomorphized 20)
      Unit)))

;;
;; Benchmarks on optional are disabled by default because they compute the 10th
;; instead of the 20th fibonacci number. Computing the 20th was exhausting the heap.
;;

#+ignore
(define-benchmark recursive-fib-generic-optional ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (coalton-benchmarks/native:fib-generic-optional 10)))
  (report trivial-benchmark::*current-timer*))

#+ignore
(define-benchmark recursive-fib-monomorphized-optional ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (coalton-benchmarks/native:fib-monomorphized-optional 10)))
  (report trivial-benchmark::*current-timer*))
