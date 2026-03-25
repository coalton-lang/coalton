---
title: "specialize"
description: "Register a specialized implementation for a generic function."
hideMeta: true
weight: 95
---

`specialize` is a toplevel directive for type-matched call
rewriting.

## Syntax

```lisp
(specialize ⟨generic-fun⟩ ⟨specialized-fun⟩ ⟨specialized-ty⟩)
```

## Semantics

- `⟨generic-fun⟩` is the name of the general function callers normally use.
- `⟨specialized-fun⟩` is a more specific implementation with the declared
  `⟨specialized-ty⟩`.
- When a call site has matching known types at compile-time, Coalton may rewrite the call to
  use the specialized function.
- Specialization is not guaranteed, so specialized code must behave the same as
  the original definition.
- Specialization, when it exists, is automatically invoked during monomorphization.

## Example

```lisp
(coalton-toplevel
  (declare inc (Num :a => :a -> :a))
  (define (inc x)
    (+ x 1))

  (declare inc-int (Integer -> Integer))
  (define (inc-int x)
    (+ x 1))

  (specialize inc inc-int (Integer -> Integer)))

;; Calls to `inc` will be replaced with `inc-int`
;; when it is statically known to be operating on
;; `Integer`.
```
