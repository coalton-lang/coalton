---
title: "pprint-coalton-codegen"
description: "Pretty-print generated Lisp for Coalton forms."
hideMeta: true
weight: 345
---

`pprint-coalton-codegen` is the readable-display wrapper around
[`coalton-codegen`](/manual/operators/coalton-codegen/).

## Syntax

```lisp
(pprint-coalton-codegen
  ⟨form⟩ ...)
```

## Semantics

- It pretty-prints generated Lisp to standard output.
- It temporarily imports the current package's symbols into a helper package so
  the printed output is easier to read.
- Use it when raw generated forms are too noisy to inspect directly.

## Example

```lisp
(pprint-coalton-codegen
  (define (double x)
    (+ x x)))
```
