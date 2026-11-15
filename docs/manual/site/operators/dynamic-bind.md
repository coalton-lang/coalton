---
title: "dynamic-bind"
description: "Dynamically rebind one or more dynamic variables."
hideMeta: true
weight: 155
---

`dynamic-bind` temporarily rebinds one or more dynamic variables for the dynamic extent of its body.

## Syntax

```lisp
(dynamic-bind ((*⟨name⟩* ⟨init-expr⟩)*)
  ⟨body-element⟩*
  ⟨expr⟩)
```

## Semantics

- Every bound variable `*⟨name⟩*` must be an existing dynamic variable defined at top level with [`define`](/manual/operators/define/) (beginning and ending with `*` with at least one non-`*` character).
- Duplicate dynamic variable names in the same `dynamic-bind` binding list are rejected at compile time.
- Initializer expressions `⟨init-expr⟩` are evaluated in parallel in the outer dynamic environment (referencing `*⟨name⟩*` inside `⟨init-expr⟩` reads its previous dynamic value).
- Each `⟨init-expr⟩` must match the existing type scheme of `*⟨name⟩*`.
- The new bindings are visible to all code executed during the dynamic extent of the body, including called functions.
- When control leaves `dynamic-bind` (either normally or via non-local exit such as [`return`](/manual/operators/return/), [`break`](/manual/operators/break/), or [`throw`](/manual/operators/throw/)), the previous dynamic bindings are automatically restored.
- The body is an implicit [`progn`](/manual/operators/progn/) (supporting multiple expressions and shorthand `let`), and its final expression is in tail position when the `dynamic-bind` form is in tail position.

## Examples

```lisp
(coalton-toplevel
  (declare *indent* UFix)
  (define *indent* 0)

  (declare current-indent (Void -> UFix))
  (define (current-indent)
    *indent*))

(coalton
  (Tuple (current-indent)
         (dynamic-bind ((*indent* (+ *indent* 4)))
           (current-indent))))
; => #.(TUPLE 0 4)
```

## Other Notes

- Inside any expression body (`define`, `fn`, `progn`, `do`, etc.), you can also write the shorthand `(let *⟨name⟩* = ⟨init-expr⟩)`, which expands to a `dynamic-bind` over the remainder of that body.
- `dynamic-bind` compiles directly to a Common Lisp special-variable `cl:let` binding, which is thread-local per dynamic binding in multi-threaded Common Lisp implementations.

