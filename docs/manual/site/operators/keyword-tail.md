---
title: "&key"
description: "Keyword-argument tail syntax."
hideMeta: true
weight: 40
---

`&key` introduces optional keyword parameters in function definitions ([`define`](/manual/operators/define/) and [`fn`](/manual/operators/fn/)) and keyword parameter types before [`->`](/manual/operators/arrow/) in function types.

## Syntax

```lisp
;; Function definition with keyword parameters
(define (⟨name⟩ ⟨pos-arg⟩* &key (⟨kw-var⟩ ⟨default-expr⟩)*)
  ⟨docstring⟩?
  ⟨body⟩+)

;; Anonymous function with keyword parameters
(fn (⟨pos-arg⟩* &key (⟨kw-var⟩ ⟨default-expr⟩)*)
  ⟨body⟩+)

;; Function type with positional and keyword inputs
(⟨pos-input-types⟩ &key (:⟨kw⟩ ⟨type⟩)* -> ⟨output-spec⟩)

;; Function type with only keyword inputs (no positional inputs)
(&key (:⟨kw⟩ ⟨type⟩)+ -> ⟨output-spec⟩)

;; Function call supplying keyword arguments
(⟨fn-expr⟩ ⟨pos-arg⟩* :⟨kw-1⟩ ⟨expr-1⟩ :⟨kw-2⟩ ⟨expr-2⟩ ...)
```

## Semantics

- **In definitions (`define`, `fn`, and instance methods)**:
  - Positional parameters always precede `&key`.
  - Each keyword parameter is written `(⟨kw-var⟩ ⟨default-expr⟩)`, where `⟨kw-var⟩` is an ordinary variable name (not a pattern or dynamic variable) bound inside the body, and `:⟨kw-var⟩` is the keyword used by callers.
  - Every keyword parameter requires a `⟨default-expr⟩`. When a caller omits `:⟨kw-var⟩`, `⟨default-expr⟩` is evaluated at call time.
  - Keyword defaults are checked and evaluated sequentially left-to-right: a later `⟨default-expr⟩` may reference earlier positional parameters and earlier keyword parameters.
  - Duplicate keyword parameter names are rejected at compile time.
- **In function types**:
  - Each keyword entry is written `(:⟨kw⟩ ⟨type⟩)` between `&key` and `->`.
  - Keyword entries in a type may be written in any order (they are canonicalized by keyword name); duplicate keyword entries are rejected.
  - A function with zero positional parameters and one or more keyword parameters is written `(&key (:⟨kw⟩ ⟨type⟩)+ -> ⟨output-spec⟩)` without `Void`.
  - A bare `&key` with no entries is permitted and is equivalent to omitting `&key`.
- **At call sites**:
  - Keyword arguments `:⟨kw⟩ ⟨expr⟩` must appear after all positional arguments.
  - Callers may supply any subset of the declared keywords in any order; unknown or duplicate keywords at a call site are compile-time errors.
- **Higher-order compatibility**:
  - A function value that accepts optional keyword parameters can be passed to a higher-order context expecting a function type with a subset of those keywords (including no keywords); omitted keywords use their defaults.

## Examples

```lisp
(coalton-toplevel
  ;; Function with positional and sequential keyword defaults
  (declare make-range (Integer &key (:stop Integer) (:step Integer) -> (List Integer)))
  (define (make-range start &key (stop (+ start 10)) (step 1))
    (for ((i start (+ i step))
          (acc Nil (Cons i acc)))
      :returns (reverse acc)
      :while (< i stop)))

  ;; Passing `make-range` where a plain `(Integer -> (List Integer))` is expected
  (declare apply-to-zero ((Integer -> (List Integer)) -> (List Integer)))
  (define (apply-to-zero f)
    (f 0)))

(coalton
  (Tuple (make-range 0 :stop 6 :step 2)
         (apply-to-zero make-range)))
; => #.(TUPLE (0 2 4) (0 1 2 3 4 5 6 7 8 9))
```

## Other Notes

- Functions with keyword parameters are not inlined by Coalton's AST inliner or cloned by [`monomorphize`](/manual/operators/monomorphize/).
- See [`->`](/manual/operators/arrow/), [`define`](/manual/operators/define/), and [`fn`](/manual/operators/fn/).

