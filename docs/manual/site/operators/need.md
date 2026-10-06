---
title: "need"
description: "Take a value or return the failure early."
hideMeta: true
weight: 268
---

`need` is a macro that takes the value held by a `Result` or `Optional`, or
returns its failure from the enclosing function.

## Syntax

```lisp
(need ⟨expr⟩)
```

## Semantics

- `⟨expr⟩` must have a type that is an instance of the `Fallible` class, such
  as `(Result :e :a)` or `(Optional :a)`.
- If `⟨expr⟩` holds a value, such as `(Ok x)` or `(Some x)`, the `need`
  expression evaluates to that value.
- Otherwise `need` returns the failure, such as `(Err e)` or `None`, from the
  nearest enclosing function, like [`return`](/manual/operators/return/). That
  function must therefore return a `Result` with the same error type, or an
  `Optional`.
- `need` does not catch exceptions. Use `coalton/result:try` to turn a thrown
  exception into a `Result` first.
- `(need ⟨expr⟩)` expands to
  `(match (split-failure ⟨expr⟩) ((Ok v) v) ((Err f) (return f)))`, where
  `split-failure` is the method of `Fallible`. Because `need` is a macro, it is
  not itself a function.

## Example

```lisp
(declare add-parsed (String * String -> (Result String Integer)))
(define (add-parsed a b)
  (let x = (need (parse-number a)))
  (let y = (need (parse-number b)))
  (Ok (+ x y)))
```
