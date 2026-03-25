---
title: "->"
description: "Function type arrow."
hideMeta: true
weight: 10
---

`->` denotes a function type. You use it in `declare` forms, class
method types, local declarations, and inline `lisp` type signatures.

## Syntax

```lisp
;; in types
⟨type-product⟩ [&key ...] -> ⟨type-product⟩

;; in Lisp forms

(lisp (-> ⟨type-product⟩) ...)

;; ⟨type-product⟩ := ⟨type⟩
;;                 | ⟨type-product⟩ * ⟨type⟩
```

## Semantics

- `A -> B` means a function from `A` to `B`.
- `A * B -> C` means a fixed-arity two-argument function.
- `Void -> T` is a true nullary function type.
- `T -> Void` is a function type that returns no values.
- In `lisp` forms, the arrow separates the output type list from the embedded
  Lisp variable list.
- Keyword argument lists can precede `->`.

## Options

- Put [`&key`](/manual/operators/keyword-tail/) between positional inputs and
  `->` to describe keyword arguments.
- Use explicit [`forall`](/manual/operators/forall/) when you want the type
  variables named in the signature to be in lexical scope inside the body.

## Example

```lisp
(coalton-toplevel
  (declare add2 (Integer -> Integer))
  (define (add2 x)
    (+ x 2)))
```
