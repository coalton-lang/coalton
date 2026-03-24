---
title: "values"
description: "Multiple-values form."
hideMeta: true
weight: 195
---

`values` produces multiple return values. It can only be used in
a binding or returning position.

`values` is also a pattern in `let`

## Syntax

```lisp
;; produce multiple values
(values ⟨expr⟩ ...)

;; destructure multiple values into variable bindings
(let (values ⟨var⟩...) = ⟨expr⟩)

;; or
(let (((values ⟨var⟩...) ⟨expr⟩)
      ...)
  ...)
```

## Semantics

- The number and types of returned values must match the surrounding expected
  type.
- Multiple values can be destructured with `let (values ...) = ...`.

## Example

```lisp
(define (sum-and-product x y)
  (values (+ x y) (* x y)))
```
