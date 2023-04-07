;;; A program to (inefficiently) generate an "infinite" stream of
;;; primes. See
;;;
;;;     https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
;;;
;;; for details.
;;;
;;; CL-USER> (in-package :small-coalton-programs/primes-native)
;;; #<COMMON-LISP:PACKAGE "SMALL-COALTON-PROGRAMS/PRIMES-NATIVE">
;;; SMALL-COALTON-PROGRAMS/PRIMES-NATIVE> (cl:time (coalton (head (extract 10000 primes))))
;;; Evaluation took:
;;;   20.987 seconds of real time
;;;   20.971166 seconds of total run time (18.733503 user, 2.237663 system)
;;;   [ Run times consist of 16.933 seconds GC time, and 4.039 seconds non-GC time. ]
;;;   99.92% CPU
;;;   50,369,738,320 processor cycles
;;;   6,450,511,424 bytes consed
;;;
;;; #.(SOME 2)
;;;
;;; SMALL-COALTON-PROGRAMS/PRIMES-NATIVE> (in-package :small-coalton-programs/primes-iterator)
;;; #<COMMON-LISP:PACKAGE "SMALL-COALTON-PROGRAMS/PRIMES-ITERATOR">
;;; SMALL-COALTON-PROGRAMS/PRIMES-ITERATOR> (cl:time (coalton (head (extract! 10000 (primes-iter)))))
;;; Evaluation took:
;;;   3.338 seconds of real time
;;;   3.341738 seconds of total run time (3.333064 user, 0.008674 system)
;;;   [ Run times consist of 0.039 seconds GC time, and 3.303 seconds non-GC time. ]
;;;   100.12% CPU
;;;   8,011,692,922 processor cycles
;;;   810,513,312 bytes consed
;;;
;;; #.(SOME 2)

(cl:defpackage #:small-coalton-programs.primes-native
  (:use #:coalton
        #:coalton-prelude)
  (:export #:extract
           #:primes))

(cl:in-package #:small-coalton-programs.primes-native)

(coalton-toplevel
  (define-type (LazyStream :t)
    (LCons :t (Unit -> LazyStream :t)))

  (define (extract n l)
    "Take `n` primes from the stream `l`."
    (if (<= n 0)
        Nil
        (match l
          ((LCons x xs) (Cons x (extract (- n 1) (xs)))))))

  (define (numbers-from n)
    "Produce a stream of ascending integers starting from `n`."
    (LCons n (fn () (numbers-from (+ n 1)))))

  (define (drop-if f l)
    "Filter the stream `l`, removing values that satisfy `f`."
    (match l
      ((LCons x xs) (if (f x)
                        (drop-if f (xs))
                        (LCons x (fn () (drop-if f (xs))))))))

  (define (multiple? m x)
    "Is `x` a multple of `m`?"
    (== 0 (mod x m)))

  (define primes
    "A stream of prime numbers."
    (let ((drop-multiples
            (compose drop-if multiple?))
          (sieve
            (fn (l)
              (match l
                ((LCons p xs)
                 (LCons p (fn () (sieve (drop-multiples p (xs))))))))))
      (sieve (numbers-from 2)))))


;;; This is like the above, but uses Coalton's iterators.

(cl:defpackage #:small-coalton-programs.primes-iterator
  (:use #:coalton
        #:coalton-prelude)
  (:export #:extract!
           #:primes-iter)
  (:local-nicknames (#:iter #:coalton-library/iterator)))

(cl:in-package #:small-coalton-programs.primes-iterator)

(coalton-toplevel
  (define (extract! n it)
    "Extract `n` elements from the iterator `it` into a list."
    (iter:collect! (iter:take! n it)))

  (define (numbers-from n)
    "Produce an iterator of ascending integers starting from `n`."
    (iter:recursive-iter 1+ (const False) n))

  (define (sieve-step! keeper init+it)
    "Produce a new iterator (and initial element) keeping elements
     satisfying `(keeper init)` from `it`."
    (match init+it
      ((Tuple init it)
       (let ((next-it (iter:filter! (keeper init) it)))
         (Tuple (unwrap (iter:next! next-it))
                next-it)))))

  (define (primes-iter)
    "Produce an iterator of all primes in ascending order."
    (map fst (iter:recursive-iter (sieve-step! (fn (m x) (/= 0 (mod x m))))
                                  (const False)
                                  (Tuple 2 (numbers-from 3))))))
