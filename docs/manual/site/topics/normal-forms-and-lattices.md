---
title: "Normal Forms and Lattices"
description: "Read integral matrix witnesses, reduce lattice bases, and distinguish PSLQ candidates from proven relations."
hideMeta: true
math: true
weight: 31
---

An integer matrix describes two different objects: a **map** on column
vectors, and a list of **generators** given by its rows. Smith form describes
the map; row Hermite form and LLL change the row generators without changing
the lattice they generate. PSLQ solves a different problem: given real
numbers, look for a short integer relation *between* them.

Load the extended math library and give the packages short names for these
examples:

```lisp
(ql:quickload "coalton/xmath")
(cl:defpackage #:lattice-examples
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:mat #:coalton/xmath/matrix)
                    (#:nf #:coalton/xmath/normal-form)
                    (#:lattice #:coalton/xmath/lattice)
                    (#:pslq #:coalton/xmath/pslq)
                    (#:ra #:coalton/xmath/real-algebraic)
                    (#:cr #:coalton/xmath/computable-reals)))
(cl:in-package #:lattice-examples)
(named-readtables:in-readtable coalton:coalton)
```

### Change generators without losing integer information

The rows $(6,12)$ and $(10,20)$ lie on the same line. Dropping the second
would lose lattice points: twice the first minus the second gives $(2,4)$.
Include a third generator $(0,9)$:

```lisp
(coalton-toplevel
  (define generators
    (the (mat:Matrix Integer) (mat:from-rows [[6 12] [10 20] [0 9]]))))

(coalton
  (progn
    (let (values h u pivots) = (nf:hermite-normal-form generators))
    (values (mat:to-rows h) pivots (== h (* u generators)))))
;; ((2 4) (0 9) (0 0)), (0 1), True

(coalton
  (progn
    (let (values b u rank) = (lattice:lll generators :delta 3/4))
    (values (mat:to-rows b) rank (== b (* u generators)))))
;; ((2 4) (-4 1) (0 0)), 2, True
```

Both calls return a square integer matrix $U$ with determinant $\pm1$, so
the transformation is reversible over the **integers**. Hermite form is
canonical: pivots are positive, entries above each pivot $p$ lie in
$[0,p)$, and the zero rows come last. For a column Hermite convention,
take the row Hermite form of the transpose and transpose the result.
For univariate polynomials over an exact field, pivots are monic and
residues above them have smaller degree.

LLL instead chooses a short, nearly orthogonal basis. Its output need not be
canonical or shortest possible. Nonzero rows come first; remaining rows are
zero, even for rectangular or dependent input. The parameter must satisfy
$1/4<\delta<1$. Rounding and the Lovász tests are exact, including ties.
When the rows are independent, their Gram data are initialized once and
updated after each elementary operation.

### Read the kernel and image from Smith form

```lisp
(coalton
  (progn
    (let a = (the (mat:Matrix Integer) (mat:from-rows [[6 0 6] [0 10 10]])))
    (let (values d u v factors) = (nf:smith-normal-form a))
    (values factors (== d (* (* u a) v)) (mat:mat-vec-mul a (mat:col v 2)))))
;; (2 30), True, (0 0)
```

The convention is $UAV=D$, **not** $A=UDV$. The nonzero entries $2,30$
are the invariant factors: their product is $60$ and the first divides the
second. The cokernel is $\mathbb Z/2\mathbb Z\oplus\mathbb Z/30\mathbb Z$.
For any $m\times n$ input of rank $r$:

- The last $n-r$ columns of $V$ are a basis of the right kernel. In the
  example this is the line of multiples of $(-1,-1,1)$.
- The first $r$ columns of $AV$ form an image basis. The image here contains
  $(6,0)$ and $(0,10)$, but not $(1,0)$, despite having full rational rank.
- The last $m-r$ rows of $U$ are a basis of the left kernel.

Use sized matrices for these operations; `(mat:matrix 0 3 [])` retains three
columns, unlike the unqualified empty row list.

### Search for an integer relation with PSLQ

If $x=1+\sqrt2$, what small integer polynomial vanishes at $x$? Search for
a relation between $1,x,x^2$:

```lisp
(coalton
  (let ((x (+ 1 (sqrt (the ra:RealAlgebraic 2)))))
    (pslq:pslq [1 x (* x x)] :bits 80 :max-coefficient 4 :max-iterations 100)))
;; ExactRelation (1 2 -1): 1 + 2x - x^2 = 0, checked in the exact field.

(coalton
  (let ((a (sqrt (the cr:CReal 2)))
        (b (sqrt (the cr:CReal 3))))
    (pslq:pslq [a b (+ (* 5 a) (* 7 b))]
               :bits 96 :max-coefficient 7 :max-iterations 100)))
;; RelationCandidate (5 7 -1) bound
```

The second search works with computable-real approximations. It does **not**
use approximate equality as an exact test. For a proposed $c$, it requests
fresh input approximations $r_i$ with absolute error at most $2^{-q}$ at
a higher precision. If the interval
$\sum c_i r_i \,\pm\, 2^{-q}\sum |c_i|$ excludes zero, it rejects the
proposal; otherwise `bound` is an absolute upper bound on $|\sum c_i x_i|$,
in the original units. Zero lying in a narrow interval is not a proof.
For `Integer`, `Fraction`, or `RealAlgebraic` inputs it checks the dot product
exactly instead.

`:bits` sets the binary working precision relative to the input scale
(default 128, minimum 16). Supply the original exact or refinable inputs:
increasing it cannot recover digits lost in an earlier conversion to `F64`.
`:max-coefficient` bounds each returned coefficient inclusively.
`PrecisionExhausted` means the input scale, a pivot, or a proposed relation
could not be resolved. `CoefficientLimit` and `IterationLimit` mean the
search stopped, **not** that no other relation exists. Increase the relevant
limit or rescale the inputs before retrying.
