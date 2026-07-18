---
title: "define"
description: "Top-level value, function, and dynamic variable definition form."
hideMeta: true
weight: 40
---

`define` introduces top-level values, functions, and dynamic variables.

## Syntax

```lisp
;; Ordinary top-level value
(define ⟨name⟩ ⟨docstring⟩? ⟨expr⟩)

;; Function
(define (⟨name⟩ ⟨arg⟩...)
  ⟨docstring⟩?
  ⟨body⟩...)

;; Dynamic variable
(define *⟨name⟩* ⟨docstring⟩? ⟨expr⟩)

;; ⟨arg⟩ := ⟨name⟩ | _ | ⟨pattern⟩
```

## Ordinary top-level values

- An ordinary name creates a top-level value binding.
- A local lexical binding may shadow a top-level value.

```lisp
(declare answer Integer)
(define answer 42)
```

## Functions

- Function syntax places the name and arguments in a list.
- Arguments may be names, `_`, or patterns.
- An unused named argument may begin with `_`, such as `_state`.
- A direct argument pattern may be used when its type has one constructor.
- The body is an implicit [`progn`](/manual/operators/progn/) and may use
  [`return`](/manual/operators/return/).
- A function may instead be defined as a value using
  [`fn`](/manual/operators/fn/).

```lisp
(declare add2 (Integer -> Integer))
(define (add2 x)
  (+ x 2))
```

## Dynamic variables

- A name beginning and ending with `*` defines a dynamic variable. It must
  contain at least one non-`*` character.
- Earmuffs are syntactic: ordinary parameters, patterns, and lexical bindings
  cannot use dynamic-variable names.
- The definition supplies the top-level value. Each reference reads the value
  from the current dynamic environment.
- [`dynamic-bind`](/manual/operators/dynamic-bind/) temporarily rebinds a
  dynamic variable. Called functions observe the rebinding.
- A rebinding must preserve the variable's type. Multiple bindings are
  parallel and their initializers are evaluated in the outer environment.
- Dynamic variables may have any Coalton type, including function types.
  Function-valued dynamic variables use value syntax with
  [`fn`](/manual/operators/fn/); function syntax cannot use a dynamic name.
- Dynamic variables cannot be marked [`inline`](/manual/operators/inline/).

```lisp
(declare *base* Integer)
(define *base* 10)

(define (base-value)
  *base*)

(dynamic-bind ((*base* 20))
  (base-value))              ; => 20
```

## Common semantics

- Types are inferred, but an explicit `declare` is recommended.
- Exported definitions without a matching declaration signal a
  `coalton:deprecation-warning`; this is planned to become an error.
- A docstring appears after the name or argument list and before the expression
  or body.
- Ordinary functions may use [`inline`](/manual/operators/inline/),
  [`monomorphize`](/manual/operators/monomorphize/), and
  [`specialize`](/manual/operators/specialize/) optimization facilities.
