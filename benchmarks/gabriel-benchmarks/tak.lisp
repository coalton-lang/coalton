;;;; gabriel-benchmarks/tak.lisp
;;;;
;;;;

(cl:in-package #:coalton-benchmarks)

(define-benchmark tak ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (coalton-benchmarks/native:tak 18 12 6)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark tak-lisp ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (lisp-tak 18 12 6)))
  (report trivial-benchmark::*current-timer*))

(declaim (ftype (function (fixnum fixnum fixnum) fixnum) lisp-tak))
(defun lisp-tak (x y z)
  (if (not (< y x))
      z
      (lisp-tak (lisp-tak (1- x) y z)
                (lisp-tak (1- y) z x)
                (lisp-tak (1- z) x y))))

(cl:in-package #:coalton-benchmarks/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(coalton-toplevel

  (declare tak (IFix -> IFix -> IFix -> IFix))
  (define (tak x y z)
    (if (not (< y x))
        z
        (tak (tak (1- x) y z)
             (tak (1- y) z x)
             (tak (1- z) x y)))))
