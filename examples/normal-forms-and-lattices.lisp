;;;; From the repository root, evaluate:
;;;;   (ql:quickload "coalton/xmath")
;;;;   (load "examples/normal-forms-and-lattices.lisp" :print t)

(cl:defpackage #:normal-form-examples
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:mat #:coalton/xmath/matrix)
                    (#:nf #:coalton/xmath/normal-form)
                    (#:lattice #:coalton/xmath/lattice)
                    (#:pslq #:coalton/xmath/pslq)
                    (#:ra #:coalton/xmath/real-algebraic)
                    (#:cr #:coalton/xmath/computable-reals)))
(cl:in-package #:normal-form-examples)
(named-readtables:in-readtable coalton:coalton)

;; The first two rows are dependent, but simply discarding the second
;; would lose lattice points: twice the first minus the second is (2,4).
(coalton-toplevel
  (define generators
    (the (mat:Matrix Integer) (mat:from-rows [[6 12] [10 20] [0 9]]))))

(coalton
  (progn
    (let (values h u pivots) = (nf:hermite-normal-form generators))
    (values (mat:to-rows h) pivots (== h (* u generators)))))
;; => ((2 4) (0 9) (0 0)), (0 1), True

;; LLL finds shorter generators rather than a canonical basis. Its witness
;; is also unimodular, so the change of basis is reversible over the integers.
(coalton
  (progn
    (let (values b u rank) = (lattice:lll generators :delta 3/4))
    (values (mat:to-rows b) rank (== b (* u generators)))))
;; => ((2 4) (-4 1) (0 0)), 2, True

;; For the map defined by A, the first two columns of A*V generate the
;; image; the last column of V generates its right kernel.
(coalton
  (progn
    (let a = (the (mat:Matrix Integer) (mat:from-rows [[6 0 6] [0 10 10]])))
    (let (values d u v factors) = (nf:smith-normal-form a))
    (values factors (== d (* (* u a) v)) (mat:mat-vec-mul a (mat:col v 2)))))
;; => (2 30), True, (0 0)

;; Find the polynomial 1 + 2x - x^2. The dot product can be checked in
;; the exact field of real algebraic numbers.
(coalton
  (let ((x (+ 1 (sqrt (the ra:RealAlgebraic 2)))))
    (pslq:pslq [1 x (* x x)] :bits 80 :max-coefficient 4 :max-iterations 100)))
;; => ExactRelation (1 2 -1)

;; For general computable reals, the independent higher-precision check
;; can bound the absolute residual but cannot prove it is zero.
(coalton
  (let ((a (sqrt (the cr:CReal 2)))
        (b (sqrt (the cr:CReal 3))))
    (pslq:pslq [a b (+ (* 5 a) (* 7 b))]
               :bits 96 :max-coefficient 7 :max-iterations 100)))
;; => RelationCandidate (5 7 -1) bound
