;;;; matrix.lisp
;;;;
;;;; Basic matrix multiplication in various ways

(cl:in-package #:benchmark-matrix)

(defvar *matrix-iterations* 500000)

(defun ary (&rest args)
  (make-array (list (length args))
              :initial-contents args
              :element-type 'double-float))

(defparameter *matrix-a*
  (ary 0.8147236863931789d0 0.9057919370756192d0 0.12698681629350606d0
       0.9133758561390194d0 0.6323592462254095d0 0.09754040499940952d0
       0.2784982188670484d0 0.5468815192049838d0 0.9575068354342976d0
       0.9648885351992765d0 0.1576130816775482d0 0.9705927817606157d0
       0.9571669482429456d0 0.4853756487228412d0 0.8002804688888001d0
       0.14188633862721534d0))

(defparameter *matrix-b*
  (ary 0.9157355251890671d0 0.7922073295595544d0 0.959492426392903d0
       0.6557406991565868d0 0.035711678574189554d0 0.8491293058687771d0
       0.9339932477575505d0 0.6787351548577735d0 0.7577401305783334d0
       0.7431324681249162d0 0.39222701953416816d0 0.6554778901775566d0
       0.17118668781156177d0 0.7060460880196088d0 0.031832846377420676d0
       0.3489284681249162d0))

(define-benchmark matrix-unroll ()
  (declare (optimize speed))
  (loop :repeat *matrix-iterations*
        :do (with-benchmark-sampling
              (benchmark-matrix/native::matmul-unroll *matrix-a* *matrix-b*)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark matrix-rec ()
  (declare (optimize speed))
  (loop :repeat *matrix-iterations*
        :do (with-benchmark-sampling
              (benchmark-matrix/native::matmul-rec *matrix-a* *matrix-b*)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark lisp-matrix ()
  (declare (optimize speed))
  (loop :repeat *matrix-iterations*
        :do (with-benchmark-sampling
              (lisp-matmul-1 *matrix-a* *matrix-b*)))
  (report trivial-benchmark::*current-timer*))

(defun lisp-matmul-1 (a b)
  (let ((r (make-array '(16) :element-type 'cl:Double-Float)))
    (loop :for i :from 0 :below 4
          :do (loop :for j :from 0 :below 4
                    :do (setf (aref r (+ (* i 4) j))
                              (loop :for k :from 0 :below 4
                                    :summing (* (aref a (+ (* i 4) k))
                                                (aref b (+ (* k 4) j)))))))))

(cl:in-package #:benchmark-matrix/native)

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:defun Ix (i j) (cl:+ (cl:* i 4) j)))

(cl:defmacro m+ (cl:&rest args)
  (cl:cond ((cl:null args) 0)
           ((cl:null (cl:cdr args)) (cl:car args))
           ((cl:null (cl:cddr args)) `(+ ,(cl:car args) ,(cl:cadr args)))
           (cl:t `(+ ,(cl:car args) (m+ ,@(cl:cdr args))))))

(cl:defmacro matmul (a b c)
  `(let ()
     ,@(cl:loop :for i :from 0 :below 4
          :append (cl:loop :for j :from 0 :below 4
                     :collect `(arr:set! ,c
                                         ,(Ix i j)
                                         (m+ ,@(cl:loop :for k :from 0 :below 4
                                                  :collect `(* (arr:aref ,a ,(Ix i k))
                                                               (arr:aref ,b ,(Ix k j))))))))))


(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 1)))

(coalton-toplevel

  (declare matmul-unroll ((arr:LispArray Double-Float)
                          -> (arr:LispArray Double-Float)
                          -> (arr:LispArray Double-Float)))
  (define (matmul-unroll a b)
    (let ((r (lisp (arr:LispArray Double-Float) ()
               (cl:make-array '(16) :element-type 'cl:Double-Float))))
      (matmul a b r)
      r))

  (declare matmul-rec ((arr:LispArray Double-Float)
                       -> (arr:LispArray Double-Float)
                       -> (arr:LispArray Double-Float)))
  (define (matmul-rec a b)
    (let ((r (lisp (arr:LispArray Double-Float) ()
               (cl:make-array '(16) :element-type 'cl:Double-Float)))
          (declare ix (UFix -> UFix -> Ufix))
          (ix (fn (i j) (+ (* i 4) j)))
          (declare i-loop (UFix -> Unit))
          (i-loop (fn (i)
                    (if (== i 4)
                        Unit
                        (progn (j-loop i 0) (i-loop (+ i 1))))))
          (declare j-loop (UFix -> UFix -> Unit))
          (j-loop (fn (i j)
                    (if (== j 4)
                        Unit
                        (progn
                          (arr:set! r (ix i j) (k-loop i j 0 0d0))
                          (j-loop i (+ j 1))))))
          (declare k-loop (UFix -> UFix -> UFix -> Double-Float -> Double-Float))
          (k-loop (fn (i j k v)
                    (if (== k 4)
                        v
                        (progn
                          (+ v (* (arr:aref a (ix i k))
                                  (arr:aref b (ix k j)))))))))
      (i-loop 0)
      r))
  )
