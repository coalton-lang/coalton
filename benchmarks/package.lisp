;;;; package.lisp
;;;;
;;;; Benchmarks packages and common functions
;;;;

;;; Each bechmark group are in separate packages, benchmark-NAME
;;; and benchmark-NAME/native.  The latter is used to define
;;; Coalton functions, and the former calls them with benchmarking macros.
;;;
;;; The coalton-benchmarks package defines functions that runs each
;;; individual benchmark group, or all benchmarks at once.

(cl:defpackage #:coalton-benchmarks
  (:use #:cl
        #:trivial-benchmark)
  (:export #:run-benchmark
           #:run-benchmarks
           #:run-benchmarks-ci))

(cl:in-package #:coalton-benchmarks)

(defparameter *all-benchmarks* '())

(defmacro define-coalton-benchmark (name
                                    benchmark-clauses
                                    &rest native-clauses)
  (let ((benchmark-package (intern (format nil "COALTON-BENCHMARK-~S" name) 'keyword))
        (native-package (intern (format nil "COALTON-BENCHMARK-~S/NATIVE" name) 'keyword)))
    `(progn
       (pushnew ',benchmark-package *all-benchmarks*)
       (defpackage ,native-package ,@native-clauses)
       (define-benchmark-package ,benchmark-package
         (:local-nicknames
          (#:native ,native-package))
         ,@benchmark-clauses))))

(define-coalton-benchmark big-float
    ()
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/big-float
   #:coalton-library/math)
  (:export
   #:fib
   #:fib-fixnum
   #:fib-generic-wrapped
   #:fib-monomorphized
   #:fib-generic-optional
   #:fib-monomorphized-optional))

(define-coalton-benchmark fibonacci
    ()
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/math)
  (:export
   #:fib
   #:fib-fixnum
   #:fib-generic-wrapped
   #:fib-monomorphized
   #:fib-generic-optional
   #:fib-monomorphized-optional))

(define-coalton-benchmark gabriel
    ()
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/math)
  (:local-nicknames (#:list #:coalton-library/list))
  (:export
   #:tak
   #:stak
   #:takl
   #:takr))

;;;
;;; Running benchmarks
;;;

(defun run-benchmark (package)
  "Run a single benchmark package.  Returns a hashtable of benchmark stats."
  (run-package-benchmarks :package package :verbose t))

(defun run-benchmarks ()
  "Run all benchmark packages.  Returns a hashtable of benchmark stats."
  (labels ((merge-hash-tables (a b)
             (loop :for b-key :being :the :hash-keys :of b
                   :do (setf (gethash b-key a) (gethash b-key b)))
             a))
    (reduce #'merge-hash-tables
            (mapcar (lambda (package)
                      (run-package-benchmarks :package package :verbose t))
                    (reverse *all-benchmarks*)))))

(defun run-benchmarks-ci ()
  "Run all benchmark packages, and dump the result in bench.json."
  (let ((result (run-benchmarks)))
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
