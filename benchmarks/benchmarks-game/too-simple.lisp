(defpackage #:coalton/benchmarks/benchmarks-game/too-simple
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames (#:sys #:coalton-library/system)
                    (#:iter #:coalton-library/iterator)
                    (#:cell #:coalton-library/cell)
                    (#:string #:coalton-library/string)
                    (#:list #:coalton-library/list))
  (:export
   #:too-simple-main
   #:too-simple-main2))

(in-package #:coalton/benchmarks/benchmarks-game/too-simple)

(cl:declaim (cl:optimize cl:speed (cl:safety 0) (cl:debug 0)))
(coalton-toplevel

  (define (too-simple-main n)
    (time (fn ()
            (let sum = (cell:new 0.0d0))
            (let flip = (cell:new -1.0d0))
            (for i in (map 1+ (iter:up-to n))
              (cell:update! negate flip)
              (cell:update! (fn (x) (+ x
                                        (/ (cell:read flip)
                                           (1- (* 2 i)))))
                            sum))
            (traceobject "Sum" (* (cell:read sum) 4)))))

  (define (too-simple-main2 n)
    (time (fn ()
            (let ((declare run (Double-Float -> Double-Float -> Double-Float -> Double-Float))
                  (run (fn (i sum flip)
                         (if (< n i)
                             (* 4 sum)
                             (run (1+ i)
                                  (+ sum
                                      (/ flip
                                         (1- (* 2 i))))
                                  (negate flip))))))
              (traceobject "Sum:" (run 1.0d0 0.0d0 1.0d0)))))))
