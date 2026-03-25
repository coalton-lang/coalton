---
title: "match"
description: "Pattern matching form."
hideMeta: true
weight: 150
---

`match` finds the first matching pattern and returns the resulting expression.

## Syntax

```lisp
(match ⟨expr⟩
  (⟨pattern⟩ ⟨body⟩...)
  ...)
```

## Semantics

- Each branch pairs a pattern with a body.
- Patterns can destructure constructors, tuples, literals, and wildcards (both variables and `_`).
- Constructor patterns must *always* be wrapped in parentheses. `None` is a variable
  wildcard pattern while `(None)` is a constructor pattern
- `match` is the main way to consume algebraic data types.
- Non-exhaustive patterns warn at compile time. The behavior of falling through without
  a wildcard is undefined.
- `⟨body⟩` forms an implicit `progn`.

## Example

```lisp
(match opt
  ((Some x) x)
  ((None)   0))

;; Not intended, almost surely wrong:
(match opt
 ((Some x) x)
 (None     0))
```
