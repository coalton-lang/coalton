;;;; gabriel-benchmarks/takl.lisp
;;;;
;;;;

(defpackage #:coalton-benchmarks/gabriel/takl
  (:use #:coalton
        #:coalton-prelude
        #:coalton-benchmarking)
  (:local-nicknames
   (#:list #:Coalton-library/list)))

(in-package #:coalton-benchmarks/gabriel/takl)

;;;
;;;
;;;

(cl:declaim (cl:ftype (cl:function (cl:fixnum) cl:list) lisp-listn))
(cl:defun lisp-listn (n)
  (cl:if (cl:not (cl:= 0 n))
         (cl:cons n (lisp-listn (cl:1- n)))))

(cl:declaim (cl:ftype (cl:function (cl:list cl:list) cl:boolean) lisp-shorterp))
(cl:defun lisp-shorterp (x y)
  (cl:and y (cl:or (cl:null x)
                   (lisp-shorterp (cl:cdr x)
                                  (cl:cdr y)))))

(cl:declaim (cl:ftype (cl:function (cl:list cl:list cl:list) cl:list) lisp-mas))
(cl:defun lisp-mas (x y z)
  (cl:if (cl:not (lisp-shorterp y x))
      z
      (lisp-mas (lisp-mas (cl:cdr x)
                y z)
           (lisp-mas (cl:cdr y)
                z x)
           (lisp-mas (cl:cdr z)
                x y))))

(cl:declaim (cl:ftype (cl:function (cl:fixnum cl:fixnum cl:fixnum) cl:list) lisp-takl))
(cl:defun lisp-takl (x y z)
  (lisp-mas (lisp-listn x) (lisp-listn y) (lisp-listn z)))

;;;
;;;
;;;

#+ig(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

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
    (mas (listn x) (listn y) (listn z))))

(define-benchmark takl 500
  (fn ()
    (takl 18 12 6)
    Unit)
  :comprehensive? cl:t)

(define-benchmark lisp-takl 500
  (fn ()
    (takl 18 12 6)
    Unit)
  :comprehensive? cl:t)
