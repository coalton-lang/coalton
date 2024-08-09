;;;; package.lisp
;;;;
;;;; Benchmarks packages and common functions

(uiop:define-package #:coalton/benchmarks
  (:mix-reexport
   #:coalton/benchmarks/fibonacci
   #:coalton/benchmarks/big-float
   #:coalton/benchmarks/gabriel
   #:coalton/benchmarks/benchmarks-game))

;; (benchmark:define-benchmark-package #:coalton-benchmarks
;;   (:export #:run-benchmarks
;;            #:run-benchmarks-ci))

;; (cl:defpackage #:coalton-benchmarks/native
;;   (:use
;;    #:coalton
;;    #:coalton-prelude
;;    #:coalton-library/big-float
;;    #:coalton-library/math)
;;   (:local-nicknames (#:list #:coalton-library/list))
;;   (:export
;;    #:fib
;;    #:fib-fixnum
;;    #:fib-generic-wrapped
;;    #:fib-monomorphized
;;    #:fib-generic-optional
;;    #:fib-monomorphized-optional)

;;   ;; gabriel-benchmarks/
;;   (:export
;;    #:tak
;;    #:stak
;;    #:takl
;;    #:takr))

;; (cl:in-package #:coalton-benchmarks)

;; (defun run-benchmarks ()
;;   (run-package-benchmarks :package '#:coalton-benchmarks :verbose t))

;; (defun run-benchmarks-ci ()
;;   (let ((result (run-package-benchmarks :package '#:coalton-benchmarks :verbose t)))
;;     (with-open-file (out "bench.json" :direction :output :if-exists :supersede)
;;       (yason:encode
;;        (loop :for name :being :the :hash-keys :of result
;;              :for data :being :the :hash-values :of result
;;              :for real-time := (cdar data)
;;              :for value := (coerce  (cdr (find :total (alexandria:plist-alist real-time) :key #'car)) 'double-float)
;;              :collect (alexandria:plist-hash-table (list "name" (symbol-name name) "value" value "unit" "seconds")))
;;        out)
;;       (format out "~%"))
;;     (values)))
