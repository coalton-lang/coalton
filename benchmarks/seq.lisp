;;;; seq.lisp
;;;;
;;;; Micro benchmarks of seq operations

(cl:in-package #:benchmark-seq)

(cl:defvar *cons-seq-size* 1000)
(cl:defvar *cons-seq-iterations* 10000)
(cl:defvar *walk-seq-iterations* 2000)

(define-benchmark cons-seq ()
  (declare (optimize speed))
  (loop :repeat *cons-seq-iterations*
        :do (with-benchmark-sampling
              (benchmark-seq/native::cons-seq *cons-seq-size*)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark cons-list ()
  (declare (optimize speed))
  (loop :repeat *cons-seq-iterations*
        :do (with-benchmark-sampling
              (benchmark-seq/native::cons-list *cons-seq-size*)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark walk-seq ()
  (declare (optimize speed))
  (loop :with seq = (benchmark-seq/native::cons-seq *cons-seq-size*)
        :repeat *walk-seq-iterations*
        :do (with-benchmark-sampling
              (benchmark-seq/native::walk-seq seq)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark walk-seq-generic ()
  (declare (optimize speed))
  (loop :with seq = (benchmark-seq/native::cons-seq *cons-seq-size*)
        :repeat *walk-seq-iterations*
        :do (with-benchmark-sampling
              (benchmark-seq/native::walk-seq-generic seq)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark walk-list ()
  (declare (optimize speed))
  (loop :with lis = (benchmark-seq/native::cons-list *cons-seq-size*)
        :repeat *walk-seq-iterations*
        :do (with-benchmark-sampling
              (benchmark-seq/native::walk-list lis)))
  (report trivial-benchmark::*current-timer*))

(cl:in-package #:benchmark-seq/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 1)))

(coalton-toplevel
  ;; Construction
  ;;  using seq:push
  (declare cons-seq (UFix -> (seq:Seq UFix)))
  (define (cons-seq size)
    (let ((declare %rec (UFix -> (seq:Seq UFix) -> (seq:Seq UFix)))
          (%rec (fn (k s)
                  (if (== k size)
                      s
                      (%rec (+ k 1) (seq:push s k))))))
      (%rec 0 (seq:new))))

  ;;  for baseline
  (declare cons-list (UFix -> (List UFix)))
  (define (cons-list size)
    (let ((declare %rec (UFix -> (List UFix) -> (List UFix)))
          (%rec (fn (k s)
                  (if (== k size)
                      s
                      (%rec (+ k 1) (cons k s))))))
      (%rec 0 Nil)))

  ;; Scanning
  (declare walk-seq ((seq:Seq UFix) -> UFix))
  (define (walk-seq seq)
    (let ((declare %rec ((seq:Seq UFix) -> UFix -> UFix -> UFix))
          (%rec (fn (s i r)
                  (if (== i (seq:size seq))
                      r
                      (%rec s (+ i 1)
                            (+ r (from-some "" (seq:get s i))))))))
      (%rec seq 0 0)))

  ;;  generic iterator + fold!
  (declare walk-seq-generic ((seq:Seq UFix) -> UFix))
  (define (walk-seq-generic seq)
    (iter:fold! + 0 (iter:into-iter seq)))

  ;;  for baseline
  (declare walk-list ((List UFix) -> UFix))
  (define (walk-list lis)
    (reduce + 0 lis))
  )
