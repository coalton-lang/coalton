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

For positive finite bases and dyadic exponents $a/b$ in lowest terms with
$|a|, b \le 64$, `pow` uses integer arithmetic to round the root of an exact
rational. This handles exact boundaries, neighboring inputs, and halfway
ties in every supported rounding mode. Larger exponents also have exact
shortcuts for powers of two and dyadic roots whose powers fit the target
mantissa or a halfway boundary. Other cases use working precision and guard
bits. The rounding mode on a transcendental result is **not** a certificate
that it bounds the exact mathematical value.
