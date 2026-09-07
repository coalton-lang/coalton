;;;; Load coalton/xmath first, then this file, and call
;;;; (realalgebraic-benchmark:run :iterations 1000).
;;;; See docs/internals/realalgebraic.md for methodology and reference results.

(defpackage #:realalgebraic-benchmark
  (:use #:cl)
  (:local-nicknames (#:ra #:coalton/xmath/realalgebraic))
  (:export #:run))

(in-package #:realalgebraic-benchmark)

(defun checked-root (coefficients lo hi)
  (let ((result (ra:root coefficients lo hi)))
    (coalton:coalton
      (coalton:match
          (coalton:lisp (coalton:-> (coalton-prelude:Result coalton:String ra:RealAlgebraic)) () result)
        ((coalton-prelude:Ok x) x)
        ((coalton-prelude:Err message) (coalton-prelude:error message))))))

(defun measure (label iterations warmup function &optional describe)
  "Measure a warmed batch; exclude setup, verification, and output from timing.
SBCL allocation is cumulative bytes allocated, not peak or retained memory."
  (dotimes (i warmup) (funcall function))
  #+sbcl (sb-ext:gc :full t)
  (let* ((bytes #+sbcl (sb-ext:get-bytes-consed) #-sbcl nil)
         (start (get-internal-real-time))
         (result (loop repeat iterations for value = (funcall function) finally (return value)))
         (seconds (/ (- (get-internal-real-time) start)
                     (float internal-time-units-per-second 1d0)))
         (allocated #+sbcl (- (sb-ext:get-bytes-consed) bytes) #-sbcl nil))
    (format t "~&~A: iterations=~D total=~,6Fs mean=~,6Fms allocation=~A bytes/op~%"
            label iterations seconds (* 1000 (/ seconds iterations))
            (if allocated (format nil "~,1F" (/ allocated iterations)) "unavailable"))
    (when describe (funcall describe result))
    (finish-output)
    result))

(defun describe-algebraic (x)
  (format t "  degree=~D polynomial=~S~%" (ra::degree x)
          (ra::coefficients (ra::defining-polynomial x))))

(defun run (&key (iterations 1000) (warmup 3) (extended t))
  "Benchmark independent repetitions of each operation on prebuilt operands.
For the old implementation, use :ITERATIONS 1 :WARMUP 0 :EXTENDED NIL.
Extended cases include formerly nonterminating or prohibitively slow inputs."
  (check-type iterations (integer 1 *))
  (check-type warmup (integer 0 *))
  (format t "~&~A ~A; mode=~A; timer-units/second=~D~%"
          (lisp-implementation-type) (lisp-implementation-version)
          (if (coalton-impl/settings:coalton-release-p) :release :development)
          internal-time-units-per-second)
  (let* ((s2 (checked-root '(1 0 -2) 1 2))
         (s3 (checked-root '(1 0 -3) 1 2))
         (sum (ra::add-certified s2 s3))
         (power sum))
    (dolist (exponent '(2 4 8 16))
      (let ((input power))
        (setf power
              (measure (format nil "s^~D by squaring s^~D" exponent (/ exponent 2))
                       iterations warmup (lambda () (ra::mul-certified input input))
                       #'describe-algebraic))))
    (let ((input (checked-root '(1 -3 1) 2 3)))
      (measure "sqrt((3+sqrt(5))/2)" iterations warmup
               (lambda () (ra::nth-root-certified-positive-index 2 input)) #'describe-algebraic))
    (let ((two (ra::from-integer 2)))
      (measure "11th root of 2" iterations warmup
               (lambda () (ra::nth-root-certified-positive-index 11 two)) #'describe-algebraic))
    (when extended
      (let* ((m (expt 10 20))
             (input (checked-root (list 1 0 (- (1+ (* m m)))) m (1+ m))))
        (assert (= m (measure "floor(sqrt(10^40+1))" iterations warmup
                              (lambda () (ra::floor-certified input))))))
      (let* ((den (+ (expt 10 20) 39))
             (q (/ 1 den))
             (input (checked-root (list den -1 (- (* 3 den)) 3) (- q 1/100) (+ q 1/100))))
        (assert (measure "rational? root of (B*t-1)*(t^2-3), B=10^20+39" iterations warmup
                         (lambda () (ra:rational? input)))))
      (let ((one (ra::from-integer 1)))
        (let ((result
                (measure "12 shared-generator involutions (x+1)/(x-1)" iterations warmup
                         (lambda ()
                           (loop repeat 12 for x = sum then next
                                 for next = (ra::div-certified (ra::add-certified x one)
                                                               (ra::sub-certified x one))
                                 finally (return next)))
                         #'describe-algebraic)))
          (assert (eq (ra::compare-certified result sum)
                      (ra::compare-certified sum sum)))))))
  (values))
