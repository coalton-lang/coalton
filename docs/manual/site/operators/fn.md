---
title: "fn"
description: "Anonymous function form."
hideMeta: true
weight: 140
---

`fn` constructs an anonymous first-class function (lambda).

## Syntax

```lisp
(fn (⟨arg⟩* [&key (⟨kw-var⟩ ⟨default-expr⟩)*])
  ⟨body-element⟩*
  ⟨expr⟩)

;; ⟨arg⟩ := ⟨var⟩ | _ | ⟨pattern⟩
```

Coalton also provides reader shorthand for compact lambdas: [`ƒ`](/manual/operators/short-fn/).

## Semantics

- `(fn (...) ...)` evaluates to a function value of type `(⟨inputs⟩ -> ⟨outputs⟩)`.
- Positional parameters `⟨arg⟩*` may be variables, `_`, or patterns (such as `(Tuple a b)`). All bound variable names within the positional parameters must be distinct, and all keyword parameter names must be distinct.
- `(fn () ...)` with zero parameters has type `(Void -> ⟨return-type⟩)` (or `(&key ... -> ⟨return-type⟩)` when keyword parameters are present).
- Optional keyword parameters follow [`&key`](/manual/operators/keyword-tail/); each `(⟨kw-var⟩ ⟨default-expr⟩)` requires a default expression evaluated sequentially left-to-right when omitted by the caller.
- The body is an implicit [`progn`](/manual/operators/progn/) (supporting multiple expressions and shorthand `let` bindings) and returns the value(s) of the final `⟨expr⟩` (which may be a single value, `Void`, or multiple values via [`values`](/manual/operators/values/)).
- Each `fn` introduces its own lexical [`return`](/manual/operators/return/) scope: `(return ...)` inside the body exits the `fn` rather than an outer `define`.
- [`break`](/manual/operators/break/) and [`continue`](/manual/operators/continue/) inside an `fn` cannot target a [`for`](/manual/operators/for/) or [`for*`](/manual/operators/for-star/) loop outside that `fn`.

## Examples

```lisp
;; Map an anonymous function over a list
(coalton
  (map (fn (x) (+ (* x x) 1))
       (the (List Integer) [1 2 3 4])))
; => (2 5 10 17)

;; Destructuring a Tuple parameter and using a keyword argument
(coalton
  (progn
    (let scale = (fn ((Tuple x y) &key (factor 2))
                   (Tuple (* x factor) (* y factor))))
    (Tuple (scale (Tuple 3 4))
           (scale (Tuple 3 4) :factor 10))))
; => #.(TUPLE #.(TUPLE 6 8) #.(TUPLE 30 40))
```

## Other notes

- When a `fn` is bound to a local variable in [`let`](/manual/operators/let/) or [`let*`](/manual/operators/let-star/) (or via shorthand `let`) and does not escape its lexical scope (i.e., it is only called directly and never returned, stored in a data structure, or captured by an escaping closure), the compiler emits a Common Lisp `flet`/`labels` binding declared `dynamic-extent` so the host Lisp compiler can stack-allocate the closure. See [Performance and Optimization](/manual/topics/performance-and-optimization/).
- Macro authors who generate internal `fn` wrappers and want [`return`](/manual/operators/return/) inside the macro's user body to exit the outer enclosing function can write `(fn :transparent-to-return (⟨arg⟩*) ⟨body⟩+)`.

