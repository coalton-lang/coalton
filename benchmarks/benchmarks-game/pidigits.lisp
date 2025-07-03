;;;; pidigits.lisp
;;;;
;;;; From the Benchmarks Game
;;;;
;;;; https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/pidigits.html#pidigits

(defpackage #:benchmarks-game/pidigits
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:math #:coalton-library/math))
  (:export
   #:main
   #:pidigits))

(in-package #:benchmarks-game/pidigits)

(cl:declaim (cl:optimize cl:speed (cl:safety 0) (cl:debug 0)))

(coalton-toplevel
  (inline)
  (define (extract-digit idx num den acc)
    (math:div (+ acc (* num idx)) den))

  (define (eliminate-digit d i n k num den acc)
    (driver i n k
            (* 10 num)
            den
            (* 10 (- acc (* d den)))))

  (define (next-term i n k num den acc)
    (let ((2k+1 (into (1+ (* 2 k))))
          (acc′ (* 2k+1      (+ acc (* 2 num))))
          (num′ (* (into k)  num))
          (den′ (* 2k+1      den)))
      (cond
        ((> num′ acc′) (driver i n k num′ den′ acc′))
        (True          (extract i n k num′ den′ acc′)))))

  (define (extract i n k num den acc)
    (let ((d  (extract-digit 3 num den acc))
          (d′ (extract-digit 4 num den acc)))
      (cond
        ((/= d d′)
         (driver i n k num den acc))
        (True
         (let ((i′ (1+ i)))
           (lisp Unit (d i′)
             (cl:format cl:t "~D" d)
             (cl:when (cl:zerop (cl:mod i′ 10))
               (cl:format cl:t "~C:~D~%" #\Tab i′))
             Unit)

           (eliminate-digit d i′ n k num den acc))))))

  (declare driver (UFix -> UFix -> UFix -> Integer -> Integer -> Integer -> Unit))
  (define (driver i n k num den acc)
    (cond
      ((>= i n)
       Unit)
      (True
       (next-term i n (1+ k) num den acc))))

  (monomorphize)
  (declare pidigits (UFix -> Unit))
  (define (pidigits n)
    (driver 0 n 0
            1 1 0))

  (define (main)
    (pidigits 10000)))
