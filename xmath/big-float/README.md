# Coalton `BigFloat`

`BigFloat` is a portable arbitrary-precision floating-point number written in
Coalton.

Dynamic precision and rounding are configured with scoped helpers. For example:

```lisp
(big-float:with-precision-rounding
  (fn ()
    ...)
  :precision 256
  :rounding big-float:rndn)
```

`approx` is distinct from changing the arithmetic precision: it bounds
absolute error relative to a finite stored value. An exact dyadic conversion
does not discard bits just because the current mantissa precision is small.
`round-to-f32` and `round-to-f64` perform ordinary nearest-even conversions
without requiring an absolute error bound.

## Directed rounding

For a single rounded operation, `rndu` returns the **least** representable
value greater than or equal to the exact result; `rndd` returns the
**greatest** value less than or equal to it. `rndz` selects the neighbor
toward zero and `rnda` the one away from zero. An exactly representable
answer is unchanged. This is stronger than merely returning a bound.
If the implementation cannot certify the result it signals the Common Lisp
arithmetic condition `big-float:uncertified-rounding`. NaNs, infinities,
and signed zeros still have their operation-specific meanings.

```lisp
(big-float:with-precision-rounding
  (fn () (pow (the big-float:BigFloat (into (the Integer 81))) (general/ 1 4)))
  :precision 4 :rounding big-float:rndu)  ; exactly 3, not 3.25
```

The inputs to an operation are **stored values**. Here `into` preserves 81;
`from-int` would round it at four bits before `pow` sees it. A lazy `pi` or
`ee` is rounded when resolved. For example, `sin` of a stored approximation
to $\pi$ is not the symbolic expression $\sin(\pi)$.

| Operation | Directed implementation |
| --- | --- |
| `+`, `-`, `*`, `/`, `recip`, `sqrt`, `hypot` (also `magnitude`) | Exact integer or guard-and-sticky arithmetic. `hypot` retains enough bits of the exact sum of squares to round the root once. |
| `pow` on a positive finite base | For reduced dyadic exponents $a/b$ with $\lvert a\rvert,b\le64$, round the root of an exact rational. Larger powers have exact-boundary shortcuts. Otherwise enclose the **whole** power using monotone square roots and integer powers, inverting the exact base first for negative exponents. No rounded logarithm or exponential is used. |
| `nth-root` | Exact integer comparisons for $1\le\lvert n\rvert\le256$, including negative indices and negative odd roots. Larger indices require a perfect dyadic root before taking a reciprocal if requested. |
| `pi`, `ee`, `exp`, `ln`, `log` | Closed integer enclosures: Machin's formula for $\pi$, Taylor series for $\exp$, and an $\operatorname{atanh}$ series for $\ln$. General-base `log` recognizes rational logarithms by checking the odd mantissas and powers of two; otherwise it encloses the ratio of both logarithms. |
| `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2` (also `phase`) | Enclose the quadrant reduction and the series or inverse-function formula before rounding. `sincos`, `cis`, and `polar` give the corresponding scalar results component by component. |
| `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh` | Enclose the whole scalar function or use an analytic shortcut with a known rounding cell. This holds through polymorphic calls too. |

The fixed-point kernels for `exp`, `sin`, `cos`, `tan`, `sinh`, `cosh`,
`tanh`, `asinh` and `acosh` accept arguments of magnitude at most 1024;
exact and saturation shortcuts can succeed outside this range. The general
power refinement accepts a simplified exponent $a2^k$ when the bit length
of $|a|$ plus $|k|$ is at most 1024. Refinement starts at 16 extra bits and
doubles that allowance up to 1024. It succeeds only when both endpoints of
a proven enclosure round to the same target value. A result very close to
zero or exactly on an unrecognized boundary can remain unresolved; the
limit is not a claim that 1024 guard bits suffice.

`^` and `^^` are **not** aliases for `pow`: they perform a sequence of
multiplications and, for a negative exponent, a reciprocal. At three bits
toward $+\infty$, `(^ x 3)` for stored $x=3$ gives $32$, whereas
`(pow x x)` gives $28$, the least representable upper neighbor of $27$.
The `canonical-` formulas, `square-magnitude`, user-written compositions, and generic complex
arithmetic do not promise one final rounding. Exact conversions, sign
changes, and binary scaling do not themselves discard mantissa bits.

Nearest-even (`rndn`), nearest with ties away (`rndna`), and experimental
faithful (`rndf`) keep their existing algorithms. The small exact-power
kernel handles their halfway cases too; general transcendental results in
these modes are working-precision approximations, not certified enclosures.
