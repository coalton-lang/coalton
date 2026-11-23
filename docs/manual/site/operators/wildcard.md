---
title: "_"
description: "Wildcard pattern."
hideMeta: true
weight: 70
---

`_` is the wildcard pattern that matches any value without introducing a variable binding.

## Syntax

```lisp
;; In patterns (`match`, `define`, `fn`, `let`, `do`, `catch`, `handle`)
_

;; Explicitly discard an expression's result(s) in a body
(let _ = ⟨expr⟩)
(let (values _ ...) = ⟨expr⟩)

;; Ignored binder in a comprehension clause or short lambda
[... :for _ :below ⟨ufix⟩ ...]
[... :for _ :in ⟨iter⟩ ...]
[... :with _ = ⟨expr⟩ ...]
ƒ_.⟨expr⟩
```

## Semantics

- `_` matches any value of the expected type and discards it without binding a variable.
- Multiple `_` wildcards may appear in the same pattern, parameter list, `values` binding, or [`ƒ`](/manual/operators/short-fn/) parameter list without conflicting.
- `_` is not a valid expression or variable name and cannot be evaluated.
- **Discarding results with `(let _ = ⟨expr⟩)`**:
  - Unlike a named pattern variable, `(let _ = ⟨expr⟩)` accepts expressions returning `Void` (zero values) or multiple values as well as single values.
  - Using `(let _ = ⟨expr⟩)` suppresses compiler warnings when discarding a `Result` value, which would otherwise trigger a discarded-result warning.
- **In [`catch`](/manual/operators/catch/) and [`handle`](/manual/operators/handle/)**:
  - A branch `(_ ⟨body⟩+)` catches any Common Lisp `cl:error` (including all Coalton exceptions and panics).
  - A branch `((the ⟨exception-type⟩ _) ⟨body⟩+)` catches any exception of `⟨exception-type⟩` without binding it.

## Examples

```lisp
(coalton-toplevel
  ;; Ignore unused tuple fields and constructor payloads
  (declare first-or-zero ((Optional (Tuple Integer String)) -> Integer))
  (define (first-or-zero opt)
    (match opt
      ((Some (Tuple x _)) x)
      ((None)             0)))

  ;; Discard the second return value of a multi-value call
  (declare quotient-only (Integer * Integer -> Integer))
  (define (quotient-only a b)
    (let (values q _) = (values (coalton/math:div a b) (mod a b)))
    q))
```

## Other notes

- A variable name that *begins* with an underscore followed by additional characters, such as `_state` or `_unused`, is an ordinary bound variable (not a wildcard): it can still be referenced in the body, but the compiler suppresses unused-variable warnings if it is never read.
- See [`match`](/manual/operators/match/), [`let`](/manual/operators/let/), and [`=`](/manual/operators/pattern-bind/).

