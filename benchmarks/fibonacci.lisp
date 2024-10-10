;;;; fibonacci.lisp
;;;;
;;;; Benchmarks for different methods of generating fibonacci numbers

(cl:in-package #:coalton-benchmark-fibonacci)

(define-benchmark recursive-fib ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (native:fib 20)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark recursive-fib-generic ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (native:fib-generic-wrapped 20)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark recursive-fib-lisp ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (lisp-fib 20)))
  (report trivial-benchmark::*current-timer*))


(define-benchmark recursive-fib-monomorphized ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (native:fib-monomorphized 20)))
  (report trivial-benchmark::*current-timer*))

;;
;; Benchmarks on optional are disabled by default because they compute the 10th
;; instead of the 20th fibonacci number. Computing the 20th was exhausting the heap.
;;

#+ignore
(define-benchmark recursive-fib-generic-optional ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (native:fib-generic-optional 10)))
  (report trivial-benchmark::*current-timer*))

#+ignore
(define-benchmark recursive-fib-monomorphized-optional ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (native:fib-monomorphized-optional 10)))
  (report trivial-benchmark::*current-timer*))

(defun lisp-fib (n)
  (declare (type integer n)
           (values integer)
           (optimize (speed 3) (safety 0)))
  (when (= n 0)
    (return-from lisp-fib 0))

  (when (= n 1)
    (return-from lisp-fib 1))

  (+ (lisp-fib (- n 1)) (lisp-fib (- n 2))))

(cl:in-package #:coalton-benchmark-fibonacci/native)

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
