;;;; package.lisp
;;;;
;;;; Benchmarks packages and common functions

(uiop:define-package #:coalton-benchmarks
  (:use #:coalton
        #:coalton-prelude
        #:coalton-benchmarking)
  (:mix-reexport
   #:coalton-benchmarks/fibonacci
   #:coalton-benchmarks/big-float
   #:coalton-benchmarks/gabriel)
  (:export
   #:run-coalton-benchmarks))

(in-package #:coalton-benchmarks)

(reexport-benchmarks
   "coalton-benchmarks/fibonacci"
   "coalton-benchmarks/big-float"
   "coalton-benchmarks/gabriel")

(cl:defun run-coalton-benchmarks ()
  (coalton (run-package-benchmarks "coalton-benchmarks")))

#+ig
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
