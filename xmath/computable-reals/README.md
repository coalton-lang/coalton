# Computable reals

`CReal` represents a real number as a stored rational or an approximation
oracle, using [computable-reals](https://github.com/stylewarning/computable-reals).
Asked for $k$ binary places, the oracle returns an integer $a$ such that
$|2^k x-a|\leq 1$. Integer literals and conversions from `Fraction` are
exact. `(known-fraction x)` returns `Some` for a stored rational without
consulting an oracle. `None` does *not* mean the value is irrational.

### Comparing without guessing

`(compare-at bits x y)` returns an `Optional Ordering`. Stored rationals are
compared exactly even at zero bits. For an oracle, the comparison requests
one approximation at `bits` and uses its closed enclosure
$[(a-1)2^{-k},(a+1)2^{-k}]$, with $k$ equal to `bits`.
`Some LT` and `Some GT` certify strict separation; `Some EQ` certifies equal
stored rationals. If an oracle is involved, touching or overlapping enclosures
give `None`, **not** equality. A larger budget may separate distinct values;
it cannot decide whether two arbitrary oracles represent the same number.

For example, with `cr` as a nickname for `coalton/xmath/computable-reals`:

```lisp
(coalton (cr:compare-at 0 (into 1/3) (/ 2 6))) ; => (Some EQ)
(coalton (cr:compare-at 8 (sqrt 2) (sqrt 3)))  ; => (Some LT)
(coalton (let ((x (sqrt 2))) (cr:compare-at 8 x x))) ; => None
```

There is no `Eq`, `Ord`, `Num`, or `Hash` instance. Replace `==` and `<=>`
with an explicit `compare-at` and handle `None`; do not treat it as a tie for
sorting or use it to choose a branch of an exact calculation. The old
`comparison-threshold`, `set-comparison-threshold!`, and
`with-comparison-threshold` controls have been removed. Code that only
needs arithmetic and integer literals can require `Ring` rather than `Num`.

### Rounding and approximation

Use `floor-at`, `ceiling-at`, `truncate-at`, or `round-at` in place of
`Quantizable`. Each takes `bits` and a `CReal` and returns `Optional Integer`.
It succeeds when both enclosure endpoints round to the same integer; for
example, `floor-at` may remain undetermined at an opaque integer. Stored
rationals always give an exact rounding decision, including ties to even for
`round-at`. For `proper`'s remainder, subtract `(from-int n)` from the input
after `truncate-at` returns `(Some n)`.

`rationalize` chooses a simple fraction in a certified **inner** interval.
The result is within the requested absolute tolerance, but need not have
minimum denominator in the full interval: deciding whether a rational lies
exactly on its boundary is not computable for arbitrary oracles. A fixed-float
`approx` can likewise report that its bound could not be certified, including
at a boundary. For ordinary conversion, round a `rational-approx` using
`fromfrac`.

`Approximable` supports `Fraction`, `Dyadic`, `BigFloat`, `F32`, and `F64`.
Arithmetic, reciprocals on nonzero inputs, and the `Elementary` operations
do not require a comparison. In particular `asin` and `acos` work throughout
$[-1,1]$ without testing whether an oracle equals an endpoint.
`Complex CReal` still supports arithmetic, `cis`, `magnitude`, and componentwise
approximation. It has no equality, and there is no `Polar CReal` instance:
the principal phase cannot be chosen continuously across its branch cut.

