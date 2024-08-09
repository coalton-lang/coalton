;;;; gabriel-benchmarks/takl.lisp
;;;;
;;;;

(defpackage #:coalton/benchmarks/gabriel/takl
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames (#:list #:coalton-library/list))
  (:export
   #:takl
   #:takl-main))

(in-package #:coalton/benchmarks/gabriel/takl)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(coalton-toplevel

  (declare listn (UFix -> (List UFix)))
  (define (listn n)
    (if (not (== n 0))
        (Cons n (listn (1- n)))
        Nil))

  (declare shorterp ((List UFix) -> (List UFix) -> Boolean))
  (define (shorterp x y)
    (and (not (list:null? y))
         (or (list:null? x)
             (shorterp (list:cdr x)
                       (list:cdr y)))))

  (declare mas ((List UFix) -> (List UFix) -> (List UFix) -> (List UFix)))
  (define (mas x y z)
    (if (not (shorterp y x))
        z
        (mas (mas (list:cdr x)
                  y z)
             (mas (list:cdr y)
                  z x)
             (mas (list:cdr z)
                  x y))))

  (declare takl (UFix -> UFix -> UFix -> (List UFix)))
  (define (takl x y z)
    (mas (listn x) (listn y) (listn z)))

  (define (takl-main)
    (time (fn () (takl 18 12 6)))))

;; (cl:in-package #:coalton-benchmarks)

#+ig(define-benchmark takl ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (coalton-benchmarks/native:takl 18 12 6)))
  (report trivial-benchmark::*current-timer*))

#+ig(define-benchmark takl-lisp ()
  (declare (optimize speed))
  (loop :repeat 1000
        :do (with-benchmark-sampling
              (lisp-takl 18 12 6)))
  (report trivial-benchmark::*current-timer*))

;;;
;;;
;;;

;;(declaim (ftype (function (fixnum) list) listn))
#+ig(defun listn (n)
  (if (not (= 0 n))
      (cons n (listn (1- n)))))

;;(declaim (ftype (function (list list) boolean)))
#+ig(defun shorterp (x y)
  (and y (or (null x)
             (shorterp (cdr x)
                       (cdr y)))))

;; (declaim (ftype (function (list list list) list)))
#+ig(defun mas (x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x)
                y z)
           (mas (cdr y)
                z x)
           (mas (cdr z)
                x y))))

;;(declaim (ftype (function (fixnum fixnum fixnum) list)))
#+ig(defun lisp-takl (x y z)
  (mas (listn x) (listn y) (listn z)))

;;;
;;;
;;;
