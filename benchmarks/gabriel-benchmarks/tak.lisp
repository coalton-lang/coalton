;;;; gabriel-benchmarks/tak.lisp
;;;;
;;;; 

(defpackage #:coalton/benchmarks/gabriel/tak
  (:use #:coalton
        #:coalton-prelude)
  (:export
   #:tak
   #:tak-main))

(cl:in-package #:coalton/benchmarks/gabriel/tak)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(coalton-toplevel

  (declare tak (IFix -> IFix -> IFix -> IFix))
  (define (tak x y z)
    (if (not (< y x))
        z
        (tak (tak (1- x) y z)
             (tak (1- y) z x)
             (tak (1- z) x y))))

  (define (tak-main)
    (time (fn () (tak 18 12 6)))))


;; (cl:in-package #:coalton-benchmarks)

#+ig(define-benchmark tak ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (coalton:coalton (coalton-benchmarks/native:tak 18 12 6))))
  (report trivial-benchmark::*current-timer*))
#+ig
(define-benchmark tak-lisp ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (lisp-tak 18 12 6)))
  (report trivial-benchmark::*current-timer*))

;;(declaim (ftype (function (fixnum fixnum fixnum) fixnum) lisp-tak))
#+ig(defun lisp-tak (x y z)
  (if (not (< y x))
      z
      (lisp-tak (lisp-tak (1- x) y z)
                (lisp-tak (1- y) z x)
                (lisp-tak (1- z) x y))))
