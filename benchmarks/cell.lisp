;;;; cell.lisp
;;;;
;;;; Micro benchmarks of cell operations

(cl:in-package #:benchmark-cell)

(cl:defparameter *cell-samples* 150)
(cl:defparameter *cell-iterations* 2000000)

(define-benchmark cl-lexical-var ()
  (declare (optimize speed))
  (loop :repeat *cell-samples*
        :do (with-benchmark-sampling
              (benchmark-cell/native::cl-lexical-var *cell-iterations*)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark coalton-local-cell ()
  (declare (optimize speed))
  (loop :repeat *cell-samples*
        :do (with-benchmark-sampling
              (benchmark-cell/native::coalton-local-cell *cell-iterations*)))
  (report trivial-benchmark::*current-timer*))

(cl:in-package #:benchmark-cell/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 1)))

(coalton-toplevel
  (declare calculate (UFix -> UFix))
  (define (calculate x)
    (mod (^ (1+ x) 2) 123456789)))

;; Benchmark reading and mutating a Common Lisp lexical variable
(cl:defun cl-lexical-var (iterations)
  (cl:declare (cl:optimize (cl:speed 3) (cl:safety 1)))
  (cl:declare (cl:fixnum iterations))
  (cl:let ((value 0))
    (cl:dotimes (_ iterations)
      (cl:setf value (calculate value)))))

;; Benchmark reading and mutating a Coalton cell. To guarantee apples-to-apples
;; comparison, using CL to use the same loop mechanism as the CL benchamrk.
(cl:defun coalton-local-cell (iterations)
  (cl:declare (cl:optimize (cl:speed 3) (cl:safety 1)))
  (cl:declare (cl:fixnum iterations))
  (cl:let ((cell (coalton (new 0))))
    (cl:dotimes (_ iterations)
      (coalton (write! (lisp :a () cell)
                       (calculate (read (lisp :a () cell))))))))
