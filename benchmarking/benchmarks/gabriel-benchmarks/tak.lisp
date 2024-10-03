;;;; gabriel-benchmarks/tak.lisp
;;;;
(defpackage #:coalton-benchmarks/gabriel/tak
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-benchmarking)
  (:export
   #:lisp-tak
   #:tak))

(in-package #:coalton-benchmarks/gabriel/tak)

;; Defining the lisp version
(cl:declaim (cl:ftype (cl:function (cl:fixnum cl:fixnum cl:fixnum) cl:fixnum) lisp-tak))
(cl:defun lisp-tak (x y z)
  (cl:declare (cl:optimize (cl:speed 3) (cl:safety 0)))
  (cl:if (cl:not (cl:< y x))
         z
         (lisp-tak (lisp-tak (cl:1- x) y z)
                   (lisp-tak (cl:1- y) z x)
                   (lisp-tak (cl:1- z) x y))))

;; Defining the Coalton version
(coalton-toplevel

  (declare tak (IFix -> IFix -> IFix -> IFix))
  (define (tak x y z)
    (if (not (< y x))
        z
        (tak (tak (1- x) y z)
             (tak (1- y) z x)
             (tak (1- z) x y)))))

;; Defining the Coalton benchmark
(define-benchmark tak 1000
  (fn ()
    (tak 18 12 6)
    Unit))

;; Defining the Lisp Benchmark
(define-benchmark lisp-tak 1000
  (fn ()
    (lisp Unit ()
      (lisp-tak 18 12 6)
      Unit)))
