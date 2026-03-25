---
title: "type-of"
description: "Inspect the inferred type of an expression."
hideMeta: true
weight: 300
---

`type-of` inspects the inferred type of a Coalton expression.

## Syntax

```lisp
(type-of ⟨expr⟩)
```

## Semantics

- `type-of` is used inside a Coalton expression, often at the REPL through
  `coalton`.
- It returns a representation of the inferred type scheme for `⟨expr⟩`. One
  may use `show` to see a printed representation.
- This is useful when type inference succeeds but the exact inferred type is
  not obvious.
- For alias-oriented REPL output, see
  [`describe-type-of`](/manual/operators/describe-type-of/).

## Example

```lisp
(coalton-toplevel
  (define (fun x)
    (map (fn (y) (+ 2 y)) (str:parse-int x))))

(coalton (type-of fun))
```
