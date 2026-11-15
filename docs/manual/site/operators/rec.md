---
title: "rec"
description: "Named tail-recursion form."
hideMeta: true
weight: 180
---

`rec` defines a local tail-recursive function and immediately invokes it with initial arguments, similar to Scheme's "named `let`".

## Syntax

```lisp
(rec ⟨name⟩ (⟨binding-or-declare⟩*)
  ⟨body-element⟩*
  ⟨expr⟩)

;; ⟨binding-or-declare⟩ := (⟨var⟩ ⟨init-expr⟩)
;;                       | (declare ⟨var⟩ ⟨qualified-type⟩)
```

## Semantics

- `rec` binds `⟨name⟩` to a local recursive function whose positional parameters are the variables `⟨var⟩` from the binding list (in source order), and immediately calls `⟨name⟩` with the corresponding `⟨init-expr⟩` values.
- Duplicate binding variable names in the `rec` binding list are rejected.
- Optional `(declare ⟨var⟩ ⟨qualified-type⟩)` clauses in the binding list constrain the initial bindings and the corresponding parameters of `⟨name⟩`. To constrain the return type of the `rec` expression itself, wrap the `rec` form in [`the`](/manual/operators/the/).
- The body is an implicit [`progn`](/manual/operators/progn/) (supporting multiple expressions and shorthand [`let`](/manual/operators/let/) bindings), and may return a single value, `Void`, or multiple values.
- **Tail-call requirement**: Every reference to `⟨name⟩` inside the body must be a direct tail call `(⟨name⟩ ⟨arg⟩*)` in a tail position of the `rec` body (such as the final expression of `progn`, `let`, `let*`, `cond`, `when`, `unless`, `and`, `or`, `match`, `catch`, `resumable`, or branches of `if`). Non-tail calls to `⟨name⟩` or passing `⟨name⟩` as a first-class value are rejected at compile time.
- If you need non-tail recursion or need to pass the recursive function as a value, use [`let`](/manual/operators/let/) with [`fn`](/manual/operators/fn/) instead.

## Examples

```lisp
;; Tail-recursive factorial accumulator
(coalton
  (the Integer
    (rec % ((declare i Integer)
            (declare acc Integer)
            (i 5)
            (acc 1))
      (if (<= i 1)
          acc
          (% (- i 1) (* acc i))))))
; => 120

;; Tail-recursive list search using `match`
(coalton-toplevel
  (declare find-index (Eq :a => :a * (List :a) -> (Optional UFix)))
  (define (find-index target xs)
    (rec go ((rem xs)
             (idx 0))
      (match rem
        ((Nil) None)
        ((Cons x rest)
         (if (== x target)
             (Some idx)
             (go rest (+ idx 1))))))))
```

## Other Notes

- By convention, short names such as `%`, `go`, or `loop` are commonly used for `⟨name⟩`.
- When the types of a `rec` loop are known, typeclass dictionaries supplied by context-free global instances do not become runtime parameters of its local recursive function (`labels`), so numeric loops in `rec` compile to tight loops comparable to [`for`](/manual/operators/for/). Polymorphic loops and contextual dictionary constructors retain their dictionary parameters as needed. See [Performance and Optimization](/manual/topics/performance-and-optimization/).

