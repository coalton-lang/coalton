;;; A small benchmark between Lisp and Coalton.
;;;
;;; The purpose of this benchmark is to compare Lisp and Coalton that
;;; require the "same amount of keyboard typing", against a
;;; hand-optimized Lisp function that requires a lot of keyboard
;;; typing.
;;;
;;; Simply call
;;;
;;;     (microbench1:run)
;;;
;;; and observe the printed output.

(cl:in-package #:cl-user)

(defpackage #:lisp-microbench1
  (:use #:cl)
  (:export #:f
           #:f-hand-optimized))

(defpackage #:coalton-microbench1
  (:use #:coalton #:coalton-prelude)
  (:export #:f))

(defpackage #:microbench1
  (:use #:cl)
  (:export #:run))


(cl:in-package #:lisp-microbench1)

(declaim (optimize speed (safety 0) (debug 0)))

(defun f (x)
  (- (* x x) (* 2.0f0 x)))

(defun f-hand-optimized (x)
  (declare (type single-float x))
  (the single-float (- (the single-float (* x x))
                       (the single-float (* 2.0f0 x)))))

(cl:in-package #:coalton-microbench1)

(named-readtables:in-readtable coalton:coalton)

(cl:declaim (cl:optimize cl:speed (cl:safety 0) (cl:debug 0)))

(coalton-toplevel
  (declare f (Single-Float -> Single-Float))
  (define (f x)
    (- (* x x) (* 2.0f0 x))))



(cl:in-package #:microbench1)

(declaim (optimize (speed 1) (safety 1) (debug 1)))

(defun time-it (thunk repeat)
  "Run THUNK a total of REPEAT times, and return the number of
milliseconds it took."
  (declare (optimize (speed 0) safety debug))
  (let ((start (get-internal-run-time)))
    (loop :repeat repeat :do (funcall thunk))
    (round (* 1000 (- (get-internal-run-time) start))
           internal-time-units-per-second)))

(defun run (&optional (size (* 32 1024 1024))
                      (repeat 16))
  (let ((a (make-array size :element-type 'single-float :initial-element 0.0f0))
        (b (make-array size :element-type 'single-float :initial-element 0.0f0))
        (c (make-array size :element-type 'single-float :initial-element 0.0f0)))
    ;; Initialize arrays
    (dotimes (i size)
      (setf (aref a i) (random 1.0f0)))
    (replace b a)
    (replace c a)

    ;; Define the test functions
    (flet ((lisp-test ()
             (declare (optimize speed (safety 0) (debug 0)))
             (dotimes (i size)
               (declare (type fixnum i))
               (setf (aref a i) (lisp-microbench1:f (aref a i)))))
           (hand-optimized-lisp-test ()
             (declare (optimize speed (safety 0) (debug 0)))
             (dotimes (i size)
               (declare (type fixnum i))
               (setf (aref a i) (lisp-microbench1:f-hand-optimized (aref a i)))))
           (coalton-test ()
             (declare (optimize speed (safety 0) (debug 0)))
             (dotimes (i size)
               (declare (type fixnum i))
               (setf (aref a i) (coalton-microbench1:f (aref a i))))))
      
      (let* ((lisp-time (time-it #'lisp-test repeat))
             (coalton-time (time-it #'coalton-test repeat))
             (hand-optimized-time (time-it #'hand-optimized-lisp-test repeat))
             (delta (- lisp-time coalton-time)))
        (format t "Lisp took ~D ms~%" lisp-time)
        (format t "Coalton took ~D ms~%" coalton-time)
        (cond
          ((plusp delta)
           (format t "Coalton was faster by ~D ms (~3,2F%)~%"
                   delta
                   (* 100.0 (/ delta lisp-time))))
          ((minusp delta)
           (format t "Lisp was faster by ~D ms (~3,2F%)~%"
                   (abs delta)
                   (* 100.0 (/ (abs delta) coalton-time))))
          (t
           (format t "Both Lisp and Coalton equal~%")))
        (format t "The hand-optimized function took ~D ms~%" hand-optimized-time)
        (finish-output)))))
