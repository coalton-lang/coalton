---
title: "rec"
description: "Named recursion form."
hideMeta: true
weight: 180
---

`rec` is Coalton's named-recursion form, similar to Scheme's "named-let".

## Syntax

```lisp
(rec ⟨name-spec⟩ ((⟨var⟩ ⟨init⟩) ...)
  ⟨body⟩)

;; ⟨name-spec⟩ := ⟨name⟩
;;              | (⟨name⟩ ⟨type⟩)
```

## Semantics

- `rec` is useful for iteration. `⟨name⟩` should name a
  function to be called tail recursively.
- The recursive name can optionally carry an explicit type.
- The names `go` or `%` are often used.
- Tail recursion is not enforced.
- `⟨body⟩` has an implicit `progn`.

## Example

```lisp
(rec % ((i n) (acc 0))
  (if (== i 0)
      acc
      (% (1- i) (+ acc i))))
```
