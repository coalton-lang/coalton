;;;; gabriel-benchmarks/stak.lisp
;;;;
;;;;

(in-package #:coalton-benchmarks)

(define-benchmark stak ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (coalton-benchmarks/native:stak 18 12 6)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark stak-lisp ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (lisp-stak 18 12 6)))
  (report trivial-benchmark::*current-timer*))

;;;
;;;
;;;


(defvar x)
(defvar y)
(defvar z)

(declaim (ftype (function () fixnum) stak-aux))
(defun stak-aux ()
  (if (not (< y x))
      z
      (let ((x (let ((x (1- x))
                     (y y)
                     (z z))
                 (stak-aux)))
            (y (let ((x (1- y))
                     (y z)
                     (z x))
                 (stak-aux)))
            (z (let ((x (1- z))
                     (y x)(z y))
                 (stak-aux))))
        (stak-aux))))

(declaim (ftype (function (fixnum) fixnum) lisp-stak))
(defun lisp-stak (x y z)
  (stak-aux))

;;;
;;;
;;;


(cl:in-package #:coalton-benchmarks/native)

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
