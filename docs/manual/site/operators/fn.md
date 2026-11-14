---
title: "fn"
description: "Anonymous function form."
hideMeta: true
weight: 140
---

`fn` creates an anonymous function.

## Syntax

```lisp
(fn (⟨arg⟩ ...) ⟨body⟩)
```

Coalton also has short `fn` syntax for compact lambdas:
[`ƒ`](/manual/operators/short-fn/).

## Semantics

- `fn` is the standard way to write lambdas in Coalton.
- A closure is allocated when variables are captured. When a `fn` is bound to a local variable in `let` or `let*` and does not escape its lexical scope (i.e., it is only called directly and never returned, stored in a data structure, or captured by an escaping closure), the compiler declares it `dynamic-extent` so the underlying Lisp compiler can stack-allocate the closure.
- `fn` also supports keyword arguments with `&key`.
- `return` may be used to return a value from the function.

## Example

```lisp
(fn (x)
  (+ x 2))
```
