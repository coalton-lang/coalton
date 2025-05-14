;;;; pi.lisp
;;;;
;;;; Benchmark for simple numeric computation & looping

(cl:in-package #:benchmark-pi)

(cl:defvar *leipniz-pi-num-terms* 1000000)
(cl:defvar *leipniz-pi-iterations* 10)

(define-benchmark leibniz-rec ()
  (declare (optimize speed))
  (loop :repeat *leipniz-pi-iterations*
        :do (with-benchmark-sampling
              (benchmark-pi/native::leibniz-rec *leipniz-pi-num-terms*)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark leibniz-cell ()
  (declare (optimize speed))
  (loop :repeat *leipniz-pi-iterations*
        :do (with-benchmark-sampling
              (benchmark-pi/native::leibniz-cell *leipniz-pi-num-terms*)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark leibniz-lisp ()
  (declare (optimize speed))
  (loop :repeat *leipniz-pi-iterations*
        :do (with-benchmark-sampling
              (lisp-leibniz *leipniz-pi-num-terms*)))
  (report trivial-benchmark::*current-timer*))

(defun lisp-leibniz (count)
  (declare (optimize speed)
           (type fixnum count))
  (* 4
     (loop :for k fixnum :from 0 :below count
           :sum (/ (the fixnum (if (oddp k) -1 1))
                   (coerce (+ (* 2 k) 1) 'double-float)))))

(cl:in-package #:benchmark-pi/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 1)))

(coalton-toplevel
  ;; Compute pi with Leibniz's formula

  ;; Using tail-recursion
  (declare leibniz-rec (UFix -> Double-Float))
  (define (leibniz-rec n)
    (let ((declare %rec (UFix -> UFix -> Double-Float -> Double-Float))
          (%rec (fn (n k r)
                 (if (== k n)
                     r
                     (%rec n (+ k 1)
                           (+ r (inexact/ (if (odd? k) -1 1)
                                          (tointeger (+ (* 2 k) 1)))))))))
      (* 4 (%rec n 0 0))))

  ;; Using mutable cell
  (declare leibniz-cell (UFix -> Double-Float))
  (define (leibniz-cell n)
    (let ((k (cell:new 0))
          (r (cell:new 0.0d0)))
      (while (< (cell:read k) n)
        (let ((term (inexact/ 1 (tointeger (+ (* 2 (cell:read k)) 1)))))
          (cell:update! (if (odd? (cell:read k))
                            (+ term)
                            (fn (v) (- v term)))
                        r))
        (cell:increment! k))
      (* 4 (cell:read r))))
  )
