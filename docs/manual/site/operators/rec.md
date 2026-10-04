---
title: "rec"
description: "Named recursion form."
hideMeta: true
weight: 180
---

`rec` is Coalton's named-recursion form, similar to Scheme's "named-let".

## Syntax

```lisp
(rec ⟨name⟩ (⟨binding⟩...)
  ⟨body⟩...)

;; ⟨binding⟩ := ⟨declare-form⟩
;;            | (⟨var⟩ ⟨init⟩)
```

## Semantics

- `rec` introduces a local recursive function `⟨name⟩` and immediately calls it
  with the initial values from the binding list.
- `rec` is useful for iteration. The names `go` and `%` are common.
- Init bindings may be declared with `declare`, like in `let`, `let*`, `for`,
  and `for*`.
- Local `declare` forms in the binding list apply to init variables, which in
  turn constrain the recursive function's parameters.
- If you want to constrain the result type of a `rec` expression, wrap the
  whole form in `the`.
- Recursive uses of `⟨name⟩` must be direct tail calls.
- `rec` is intended for iteration. If you need more flexible local recursion,
  or want to pass the recursive function around as a value, use `let` together
  with `fn` instead.
- `⟨body⟩` has an implicit `progn`.

When the types of a `rec` loop are known, typeclass dictionaries supplied by
context-free global instances do not become parameters of its local recursive
function. For example, arithmetic on known `F64` values and a known `UFix`
counter can use the same specialized operations as `for`. Polymorphic loops
retain their unresolved dictionaries. Instances that still require dictionary
construction also retain them, because construction can evaluate method values;
this preserves their construction order and frequency. Ordinary instance
selection and development-mode method redefinition still apply.

## Example

```lisp
(the Integer
  (rec %
       ((declare i Integer)
        (declare acc Integer)
        (i n)
        (acc 0))
    (if (== i 0)
        acc
        (% (1- i) (+ acc i)))))
```
