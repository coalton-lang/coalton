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
