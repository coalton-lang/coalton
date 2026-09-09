# Certified real algebraic arithmetic

`coalton/xmath/realalgebraic` represents an exact real root, rather than a
floating-point approximation. Its public constructors and numeric instances
remain unchanged. Coefficients are in descending degree order.

## Certificates and root selection

A value carries a primitive, square-free polynomial `p` in `Z[t]`, with
positive leading coefficient, and rational endpoints `lo < hi`. Neither
endpoint is a root, and `(lo, hi)` contains exactly one real root of `p`.
A cached Sturm chain certifies this invariant. The polynomial may be
reducible; its degree is not necessarily the value's minimal algebraic degree.

For a square-free polynomial, let `V(x)` count Sturm sign variations, ignoring
zero entries. At a root of `p`, this is the right-hand variation. Consequently,

```
number of roots in (lo, hi) = V(lo) - V(hi) - [p(hi) = 0].
```

Arithmetic first constructs a nonzero polynomial that vanishes at the desired
result. Rational interval arithmetic gives an enclosure of that result.
If the enclosure does not yet certify one root, the operands are refined and
the enclosure is recomputed. Moving an endpoint inward without preserving
the desired result is unsound. Operand refinement terminates because widths
tend to zero and the finitely many distinct resultant roots are separated.
Division first certifies a nonzero denominator and refines until its interval
excludes zero. Powers and multiplication handle exact zero separately.

Once isolation is certified, a simple root changes the sign of `p`.
Refinement therefore only evaluates `p` at the lower endpoint and a nonroot
split. The midpoint is used unless it is the root, in which case the one-third
split cannot also be a root of an isolating interval. Each refinement shrinks
the width by at least a factor of `2/3`. At a rational point `u/v`, integer
Horner evaluation computes the sign of `v^degree(p) * p(u/v)`, avoiding
intermediate rational normalization.

## Rational recognition, quantization, and decimals

If a primitive integer polynomial has leading coefficient of magnitude `B`,
every rational root in lowest terms has denominator at most `B`. Two distinct
rationals with such denominators differ by at least `1/B^2`. The rationality
test refines to width at most `1/(2*B^2)`, finds the rational of minimum
denominator in the closed interval by continued fractions, and checks its
denominator, strict containment, and exact polynomial substitution.

If the represented root is rational, this candidate must be it: a second
candidate of denominator at most `B` would violate the separation bound.
If the candidate fails, the value is irrational. This argument also covers
polynomials with an irrelevant zero root. No integer factorization or divisor
enumeration is needed. Linear polynomials have a direct rational shortcut.

Floor refines until the endpoint floors agree, or until only one integer
boundary remains. Exact substitution at that boundary resolves an integer
root; otherwise continued refinement eventually puts the value on one side.
Ceiling and truncation follow from floor and one exact boundary check.
Quantization thus avoids the precision demanded by an unrelated rationality
decision. Nearly integral irrational inputs can still require fine intervals.

`decimal-string` rounds to the requested number of decimal places, with ties
away from zero. Agreement of rounded endpoints certifies the output. If the
endpoints disagree, exact substitution at a half-integer boundary after
scaling by `10^digits` resolves a tie. Otherwise refinement eventually resolves
the rounding. Extra guard digits alone cannot certify an exact tie.

## Controlling arithmetic growth

Each nonrational value also carries a private selected generator `alpha` and
a polynomial coordinate `f` such that the value equals `f(alpha)`. The
generator has its own certified polynomial and interval. Identity of the
generator object distinguishes separate root selections, including conjugates
of the same polynomial. Refinement preserves that identity.

For shared generators, addition, subtraction, and multiplication operate on
coordinates in `Q[t]/(p)`. Reduction after each operation keeps coordinates
below `degree(p)`. A defining polynomial for a coordinate `f = num/D` is

```
Res_t(p(t), D*z - num(t)).
```

Its degree in `z` is at most `degree(p)`: over a splitting field it is a
nonzero constant times the product of the linear factors
`D*z - num(alpha_i)`. Square-free normalization removes repeated image roots.
Unary powers use modular exponentiation and this image construction, so the
result's defining degree cannot exceed the operand's defining degree.
The built-in integral exponent types specialize both `^` and `^^`; negative
exponents of `^^` use an exact reciprocal.

The quotient algebra need not be a field because `p` need not be irreducible.
To invert a coordinate, extended Euclid may find a nonconstant gcd with `p`.
Its roots are irrelevant to the selected generator because the denominator
has already been certified nonzero there. Dividing out that gcd gives a
modulus on which the inverse exists. The resulting polynomial equals the
reciprocal at the selected root; it need not invert the denominator at every
other root of the original polynomial. Subsequent operations only require
equality at the selected generator.

Rational shifts and nonzero scales substitute directly into the defining
polynomial and transform interval endpoints. Reciprocals reverse coefficients,
discard the resulting leading zeros, and invert endpoints after excluding
zero. Both transformations preserve square-freeness and root isolation.
For independent operands, general resultants remain necessary. A quotient
resultant first removes an irrelevant factor of `t` from the denominator's
polynomial, preventing a common zero component from annihilating the whole
resultant.

An `n`th root is among the real roots of `p(t^n)`. Zero insertion constructs
this polynomial in time linear in its output length. Candidate selection
uses exact rational interval powers: a candidate's `n`th power is already
known to be a root of `p`, so inclusion in the input's isolating interval
proves it is the requested root. Disjointness rejects a candidate. Nonroot
input endpoints ensure termination. Even indices select the positive
candidate after the public zero and domain checks.

## Polynomial kernels and complexity

* Multiplication uses dense array accumulation: `O(m*n)` integer operations
  for coefficient-list lengths `m` and `n`, with linear output storage.
* Bareiss exact division uses integer long division and checks both coefficient
  integrality and zero remainder. The determinant takes `O(N^3)` operations
  in `Z[z]` on an `N` by `N` matrix, not `O(N^3)` bit operations.
* Gcds and Sturm chains use primitive integer remainder sequences. Each
  remainder is a positive rational multiple of the ordinary rational
  remainder; removing only positive content preserves Sturm signs even with
  a negative divisor leading coefficient.
* A refinement costs `O(d)` integer Horner steps rather than evaluating an
  entire Sturm chain, whose total degrees can be quadratic in `d`.
* Disjoint open enclosures settle comparison before any polynomial gcd.
* Modular exponentiation uses `O(log n)` reduced polynomial multiplications
  for exponent `n`, without computing an unused final square.

These are arithmetic-operation counts. Coefficient bit lengths, interval
precision, root separation, and the requested output size still matter.
Primitive remainder sequences are not a modular gcd or fast subresultant
implementation. Independent generators can still cause genuine product-degree
growth, and this module does not promise irreducible factorization or globally
minimal defining polynomials.

## Regression tests and reproducible measurements

`tests/realalgebraic-tests.ct` includes wrong-conjugate regressions, open Sturm
endpoint counts, zero-factor division and rational recognition, integer and
decimal boundary cases, deterministic rational and polynomial oracles, degree
bounds, shared-generator inversions on reducible polynomials, and a 101st root.

The independent Bareiss oracle tests 30 polynomial matrices of dimensions
2 through 6, including zero pivots and singular matrices. It compares against
rational Gaussian elimination at `2*N+1` distinct integers. Since each matrix
entry has degree at most 2 and each determinant has degree at most `2*N`, these
270 exact evaluations establish polynomial identity for all tested matrices.

To run the benchmark after making this checkout available to ASDF:

```lisp
(ql:quickload :coalton/xmath)
(load "scripts/benchmark-realalgebraic.lisp")
(realalgebraic-benchmark:run :iterations 1000 :warmup 3)
```

On systems without MPFR, enable `:coalton-portable-bigfloat` before loading
`coalton/xmath`. It does not alter real-algebraic arithmetic. Development and
release builds must use separate ASDF output caches. For an old implementation
comparison, use `:iterations 3 :warmup 1 :extended nil`; extended cases include
previously nonterminating or prohibitively slow inputs.

The harness builds operands before timing, repeats each operation on the same
immutable operands, excludes warmup and result printing, and reports elapsed
time and cumulative allocated bytes per operation. Allocation is not peak or
retained memory. Results are workload- and machine-dependent.

### Reference measurements

Measured on Windows with SBCL `2.6.7.202-5a569e62e` in development mode,
comparing the module at `553978a` with the algorithms at `f6e755c`. Both ran
through the same harness with the same core library and compiler. The old
module used 3 measured repetitions after 1 warmup; the new module used 1,000
after 3 warmups. The machine was shared, so elapsed-time ratios are approximate;
degree and allocation differences are the more stable evidence.
Let `s = sqrt(2) + sqrt(3)`; each power row times only the last squaring step.

| Operation | Mean before | Mean after | Defining degree before / after | Allocation before / after |
| --- | ---: | ---: | ---: | ---: |
| `s^2` | 0.637 ms | 0.160 ms | 6 / 2 | 0.375 MiB / 31.83 KiB |
| `s^4` | 3.052 ms | 0.0739 ms | 10 / 2 | 2.749 MiB / 16.79 KiB |
| `s^8` | 106.683 ms | 0.0680 ms | 18 / 2 | 88.022 MiB / 16.79 KiB |
| `s^16` | 7,001.947 ms | 0.0726 ms | 34 / 2 | 10,767.096 MiB / 16.96 KiB |
| `sqrt((3+sqrt(5))/2)` | 19.867 ms | 0.0837 ms | 4 / 4 | 12.120 MiB / 13.79 KiB |
| `11th-root(2)` | 217.463 ms | 0.0737 ms | 11 / 11 | 163.508 MiB / 14.49 KiB |

The sixteenth-power step is approximately 96,000 times faster in this run
and allocates approximately 650,000 times fewer bytes. This is principally
an algorithmic improvement from avoiding extraneous conjugate combinations,
not a constant-factor claim for arbitrary inputs. Every even power of `s`
lies in `Q(sqrt(6))`, and the new degree-2 certificates reflect that fact.
The radical improvements avoid algebraic powers and resultants during
candidate selection even where the final defining degree is unchanged.

Additional new-version means over 1,000 repetitions:

* `floor(sqrt(10^40+1))`: 0.00354 ms, returning exactly `10^20`; the original
  operation exceeded a 3-second timeout while enumerating coefficient divisors.
* Rational recognition for the selected root `1/B` of
  `(B*t-1)*(t^2-3)`, `B = 10^20+39`: 1.632 ms, returning true.
* Twelve iterations of `(x+1)/(x-1)` starting at `s`: 4.225 ms total per
  twelve-iteration chain, returning exactly `s` with defining degree at most 4.

Correctness regressions include these formerly incorrect results:

| Input | Before | After |
| --- | --- | --- |
| Zero roots of `t(t-4)` in `(-1,3)` and `t(t+2)` in `(-1,1)`, added | 2 | 0 |
| Difference of roots of `t^3-3t+1` selected by `(1,5)` and `(0,1)` | About 3.41147 | About 1.18479, certified in `(1,2)` |
| `(sqrt(2)+sqrt(2))/(sqrt(2)+sqrt(2))` | Zero-polynomial error | 1 |
| Rationality and integer quantization of root 1 of `t(t-1)` | False rationality; floor/ceiling did not terminate | Rational; exact fraction, floor, and ceiling all 1 |
| `decimal-string 2` of `1/8`, isolated by `(12499/100000,125001/1000000)` | `0.12` | `0.13` |
| Number of roots of `t(t-1)(t-2)` in `(0,3)` | 1 | 2 |
