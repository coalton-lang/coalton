---
title: "&key"
description: "Keyword-argument tail syntax."
hideMeta: true
weight: 40
---

`&key` introduces keyword arguments. In definitions it separates positional
parameters from keyword parameters. In types it introduces the keyword tail
between positional inputs and [`->`](/manual/operators/arrow/).

## Syntax

```lisp
;; function definition with keyword arguments
(define (⟨name⟩ ⟨arg⟩... &key (⟨keyword⟩ ⟨default⟩)...)
  ⟨body⟩...)

;; function type with keyword arguments
(declare ⟨name⟩
  (⟨type⟩ &key (:⟨keyword⟩ ⟨type⟩)... -> ⟨result-type⟩))
```

## Semantics

- Positional arguments always come first.
- Keyword arguments are optional and use their initializer when omitted. An
  initializer is always required.
- The surface keyword name comes from the parameter name, so `width` is passed
  as `:width`.
- In a type declaration, each keyword is written as `(:name ⟨type⟩)`.

## Options

- A bare `&key` is allowed and is equivalent to omitting the keyword tail
  entirely.
- Explicit declarations are exact about which keywords exist.

## Example

```lisp
(define add-offset
  (fn (x &key (offset 1))
    (+ x offset)))
```
