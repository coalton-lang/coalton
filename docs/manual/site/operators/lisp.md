---
title: "lisp"
description: "Inline Common Lisp form."
hideMeta: true
weight: 190
---

`lisp` embeds raw Common Lisp in a Coalton expression.

## Syntax

```lisp
(lisp (-> ⟨output-type⟩) (⟨coalton-var⟩...)
  ⟨lisp-form⟩...)
```

## Semantics

- `lisp` is the direct interop escape hatch.
- Coalton trusts the declared output type and does not analyze the Lisp body.
- It can return zero, one, or multiple values depending on the output type
  specification.

## Example

```lisp
(lisp (-> Integer) (n)
  (cl:random n))
```
