---
title: "coalton-toplevel"
description: "Top-level entry point for Coalton definitions."
hideMeta: true
weight: 10
---

`coalton-toplevel` is the main entry point for writing Coalton definitions in a
source file.

## Syntax

```lisp
(coalton-toplevel
  ⟨toplevel-form⟩ ...)
```

## Semantics

- The body contains top-level Coalton forms such as `declare`, `define`,
  `define-type`, and `define-class`.
- It is for definitions, not for arbitrary one-off expression evaluation.
- In practice, most Coalton code in a file lives inside one or more
  `coalton-toplevel` forms.
- Use [`coalton`](manual/operators/coalton/) to evaluate Coalton expressions at the REPL or within
  other Lisp code.

## Example

```lisp
(coalton-toplevel
  (define-type Manual
    Book
    Online)

  (declare add2 (Integer -> Integer))
  (define (add2 x)
    (+ x 2)))
```
