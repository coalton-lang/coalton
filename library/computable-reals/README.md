# Computable Reals

This library is built on [computable-reals](https://github.com/stylewarning/computable-reals).

### Creal 

The primary type is a `Creal`, a wrapper on the computable-reals type of the same name.

As stated in the computable-reals README:

"Computable real numbers `x` are interpreted as (potentially) infinite fractions in base 2 that are specified through a rule for computation of an integer `a` with `|(2^k)*x - a| <= 1` for any `k>=0`."

### Instances:

`Eq` and `Ord` have been implemented in order to fulfill the needs of the `Num` typeclass, though they are not exact. Their definitions are shaky because they can only be verified up to an arbitrary, specified precision. The precision threshold for `Creal` can be accessed using the library function `comparison-threshold` and changed using `set-comparison-threshold!`. The default precision is `106`.

Other instances defined:

- `Num`
- `Reciprocable`
- `Exponentiable`
- `Radical`
- `Trigonometric`
- `Complex`
- `Polar`
- `Elementary`
- Typical `Into` instances


