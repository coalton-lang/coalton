---
title: "need"
description: "Take a value or return the failure early."
hideMeta: true
weight: 268
---

`need` unwraps a successful value from a `Fallible` container (such as `Result` or `Optional`), or returns the failure early from the enclosing function.

## Syntax

```lisp
(need ⟨fallible-expr⟩)
```

## Semantics

- `(need ⟨fallible-expr⟩)` is an expression macro that expands to:
  ```lisp
  (match (split-failure ⟨fallible-expr⟩)
    ((Ok value) value)
    ((Err failure) (return failure)))
  ```
  where `split-failure` is the method of the `Fallible :f` type class (`split-failure : (:f :a) -> (Result (:f :b) :a)`).
- Standard library instances of `Fallible`:
  - `Optional` (`Fallible Optional`): unwraps `x` from `(Some x)`, or returns `None` from the enclosing function (which must also return an `(Optional :b)`).
  - `(Result :e)` (`Fallible (Result :e)`): unwraps `x` from `(Ok x)`, or returns `(Err e)` from the enclosing function (which must return `(Result :e :b)` with the same error type `:e`).
- Because `need` expands to [`return`](/manual/operators/return/) on the failure branch, it must appear inside a function (and not inside the right-hand side of a `do` bind `<-`).
- `need` does not catch thrown exceptions; use `coalton/result:try` or [`catch`](/manual/operators/catch/) when handling exceptions.

## Examples

```lisp
(coalton-toplevel
  ;; Early-return propagation on `Result`
  (declare parse-and-divide (String * String -> (Result String Integer)))
  (define (parse-and-divide num-str den-str)
    (let n = (need (coalton/result:opt->result "invalid numerator"
                                               (coalton/string:parse-int num-str))))
    (let d = (need (coalton/result:opt->result "invalid denominator"
                                               (coalton/string:parse-int den-str))))
    (when (== d 0)
      (return (Err "division by zero")))
    (Ok (coalton/math:div n d)))

  ;; Early-return propagation on `Optional`
  (declare second-element ((List :a) -> (Optional :a)))
  (define (second-element xs)
    (let rest = (need (coalton/list:tail xs)))
    (coalton/list:head rest)))
```

## Other Notes

- Unlike [`do`](/manual/operators/do/) notation (which chains callbacks through `>>=`), `need` compiles to flat conditional branching and an immediate [`return`](/manual/operators/return/), so it works naturally inside [`for`](/manual/operators/for/) loops and preserves tail-call optimization in [`rec`](/manual/operators/rec/) loops.
- User-defined container types can support `need` by implementing `Fallible` from `coalton/classes`.

