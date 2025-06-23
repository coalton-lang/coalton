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
  (let ((benchmark-package (intern (format nil "BENCHMARK-~S" name) 'keyword))
        (native-package (intern (format nil "BENCHMARK-~S/NATIVE" name) 'keyword)))
    `(progn
       (pushnew ',benchmark-package *all-benchmarks*)
       (define-benchmark-package ,benchmark-package ,@benchmark-clauses)
       (defpackage ,native-package ,@native-clauses))))

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
   #:takr)
  )

(define-coalton-benchmark pi
    ()
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/math)
  (:local-nicknames (#:list #:coalton-library/list)
                    (#:cell #:coalton-library/cell))
  (:export
   #:leibniz-rec
   #:leibniz-cell)
  )

(define-coalton-benchmark seq
    ()
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/math)
  (:local-nicknames (#:list #:coalton-library/list)
                    (#:cell #:coalton-library/cell)
                    (#:seq  #:coalton-library/seq)
                    (#:iter #:coalton-library/iterator))
  (:export
   #:cons-seq
   #:cons-list
   #:walk-seq
   #:walk-seq-generic
   #:walk-list)
  )

(define-coalton-benchmark matrix
    ()
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/math)
  (:local-nicknames (#:list #:coalton-library/list)
                    (#:cell #:coalton-library/cell)
                    (#:seq  #:coalton-library/seq)
                    (#:iter #:coalton-library/iterator)
                    (#:vec  #:coalton-library/vector)
                    (#:arr  #:coalton-library/lisparray))
  )

(define-coalton-benchmark mapping
    ((:local-nicknames
      (#:hashtable #:coalton-library/hashtable)
      (#:hashmap   #:coalton-library/hashmap)
      (#:map       #:coalton-library/ord-map)))
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/hash)
  (:local-nicknames (#:cell      #:coalton-library/cell)
                    (#:hashtable #:coalton-library/hashtable)
                    (#:hashmap   #:coalton-library/hashmap)
                    (#:iter      #:coalton-library/iterator)
                    (#:l         #:coalton-library/experimental/loops)
                    (#:map       #:coalton-library/ord-map))

  )

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
