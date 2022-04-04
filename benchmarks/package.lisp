(benchmark:define-benchmark-package #:coalton-benchmarks
  (:export #:run-benchmarks
           #:run-benchmarks-ci))

(cl:defpackage #:coalton-benchmarks/native
  (:use
   #:coalton
   #:coalton-prelude)
  (:export
   #:fib
   #:fib-fixnum
   #:fib-generic-wrapped))

(cl:in-package #:coalton-benchmarks)

(defun run-benchmarks ()
  (run-package-benchmarks :package '#:coalton-benchmarks :verbose t))

(defun run-benchmarks-ci ()
  (let ((result (run-package-benchmarks :package '#:coalton-benchmarks :verbose t)))
    (with-open-file (out "bench.json" :direction :output :if-exists :supersede)
      (yason:encode
       (loop :for name :being :the :hash-keys :of result
             :for data :being :the :hash-values :of result
             :for real-time := (cdar data)
             :for value := (coerce  (cdr (find :total (alexandria:plist-alist real-time) :key #'car)) 'double-float)
             :collect (alexandria:plist-hash-table (list "name" (symbol-name name) "value" value "unit" "seconds")))
       out)
      (format out "~%"))
    (values)))

(define-benchmark recursive-fib ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (coalton-benchmarks/native:fib 20)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark recursive-fib-fixnum ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (coalton-benchmarks/native:fib-fixnum 20)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark recursive-fib-generic ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (coalton-benchmarks/native:fib-generic-wrapped 20)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark recursive-fib-lisp ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (lisp-fib 20)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark recursive-fib-lisp-fixnum ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (lisp-fixnum 20)))
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
 
(defun lisp-fixnum (n)
  (declare (type fixnum n)
           (values fixnum)
           (optimize (speed 3) (safety 0)))
  (when (= n 0)
    (return-from lisp-fixnum 0))

  (when (= n 1)
    (return-from lisp-fixnum 1))

  (+ (lisp-fixnum (- n 1)) (lisp-fixnum (- n 2))))

(cl:in-package #:coalton-benchmarks/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(coalton-toplevel
  (declare fib (Integer -> Integer))
  (define (fib n)
    (when (== n 0)
      (return 0))

    (when (== n 1)
      (return 1))

    (+ (fib (- n 1)) (fib (- n 2))))

  (declare fib-fixnum (IFix -> IFix))
  (define (fib-fixnum n)
    (when (== n 0)
      (return 0))

    (when (== n 1)
      (return 1))

    (+ (fib-fixnum (- n 1)) (fib-fixnum (- n 2))))

  (declare fib-generic (Num :a => :a -> :a))
  (define (fib-generic n)
    (when (== n 0)
      (return 0))

    (when (== n 1)
      (return 1))

    (+ (fib-generic (- n 1)) (fib-generic (- n 2))))

  (declare fib-generic-wrapped (Integer -> Integer))
  (define fib-generic-wrapped fib-generic))
