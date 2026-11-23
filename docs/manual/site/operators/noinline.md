---
title: "noinline"
description: "Prevent a specific application from being inlined."
hideMeta: true
weight: 80
---

`noinline` is an expression-level compiler hint that prevents a specific function call from being inlined.

## Syntax

```lisp
(noinline (⟨fn-expr⟩ ⟨arg⟩*))
```

## Semantics

- `noinline` is a function of type `(:a -> :a)` that returns the value of its argument unchanged.
- Unlike [`inline`](/manual/operators/inline/), `noinline` is only an expression-level operator and cannot be used as a top-level attribute.
- When the argument to `noinline` is syntactically a function application `(f a1 ...)`, Coalton marks that application node `:noinline` in the codegen AST, preventing Coalton's AST inliner and scalar-replacement passes from inlining `f` at that call site.
- If the argument is not a syntactic function application, `(noinline expr)` simply evaluates to `expr`.

## Examples

```lisp
(coalton-toplevel
  (declare fallback-hash (U64 -> U64))
  (define (fallback-hash x)
    (* x 11400714819323198485))

  ;; Prevent Coalton's AST inliner from inlining `fallback-hash` at this call site
  (declare probe (Boolean * U64 -> U64))
  (define (probe hit? key)
    (if (likely hit?)
        key
        (noinline (fallback-hash key)))))
```

## Other notes

- `noinline` is useful for keeping cold error/fallback paths out of hot inner loops, inspecting call boundaries in [`pprint-coalton-codegen`](/manual/operators/pprint-coalton-codegen/) or `cl:disassemble`, and profiling individual calls.
- See [`inline`](/manual/operators/inline/) and [Performance and Optimization](/manual/topics/performance-and-optimization/).

