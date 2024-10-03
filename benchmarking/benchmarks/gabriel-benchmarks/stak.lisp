;;;; gabriel-benchmarks/stak.lisp
;;;;
;;;;

(defpackage #:coalton-benchmarks/gabriel/stak
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-benchmarking)
  (:export
   #:lisp-stak
   #:stak))

(in-package #:coalton-benchmarks/gabriel/stak)

;;;
;;;
;;;


(cl:defvar x)
(cl:defvar y)
(cl:defvar z)

(cl:declaim (cl:ftype (cl:function () cl:fixnum) stak-aux))
(cl:defun stak-aux ()
  (cl:if (cl:not (cl:< y x))
      z
      (cl:let ((x (cl:let ((x (cl:1- x))
                     (y y)
                     (z z))
                 (stak-aux)))
            (y (cl:let ((x (cl:1- y))
                     (y z)
                     (z x))
                 (stak-aux)))
            (z (cl:let ((x (cl:1- z))
                     (y x)(z y))
                 (stak-aux))))
        (stak-aux))))

(cl:declaim (cl:ftype (cl:function (cl:fixnum cl:fixnum cl:fixnum) cl:fixnum) lisp-stak))
(cl:defun lisp-stak (x y z)
  (stak-aux))

;;;
;;;
;;;

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(coalton-toplevel

 (declare stak (IFix -> IFix -> IFix -> IFix))
 (define (stak x y z)
     (if (not (< y x))
         z
         (let ((x1 (let ((x2 (1- x))
                         (y2 y)
                         (z2 z))
                     (stak x2 y2 z2)))
               (y1 (let ((x2 (1- y))
                         (y2 z)
                         (z2 x))
                     (stak x2 y2 z2)))
               (z1 (let ((x2 (1- z))
                         (y2 x)
                         (z2 y))
                     (stak x2 y2 z2))))
           (stak x1 y1 z1)))))

;; Defining the Coalton benchmark
(define-benchmark stak 1000
  (fn ()
    (stak 18 12 6)
    Unit))

;; Defining the Lisp Benchmark
(define-benchmark lisp-stak 1000
  (fn ()
    (lisp Unit ()
      (lisp-stak 18 12 6)
      Unit)))
