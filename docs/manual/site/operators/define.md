---
title: "define"
description: "Top-level value, function, and dynamic variable definition form."
hideMeta: true
weight: 40
---

`define` introduces top-level values, functions, and dynamic variables inside [`coalton-toplevel`](/manual/operators/coalton-toplevel/).

## Syntax

```lisp
;; Ordinary top-level value
⟨opt-attribute⟩*
(define ⟨name⟩
  ⟨docstring⟩?
  ⟨body-element⟩*
  ⟨expr⟩)

;; Function
⟨opt-attribute⟩*
(define (⟨name⟩ ⟨arg⟩* [&key (⟨kw-var⟩ ⟨default-expr⟩)*])
  ⟨docstring⟩?
  ⟨body-element⟩*
  ⟨expr⟩)

;; Dynamic variable
(define *⟨name⟩*
  ⟨docstring⟩?
  ⟨body-element⟩*
  ⟨expr⟩)

;; ⟨arg⟩           := ⟨var⟩ | _ | ⟨pattern⟩
;; ⟨opt-attribute⟩ := (inline) | (monomorphize) | (abi ⟨convention⟩)
;; ⟨convention⟩    := :auto | :lisp | :unpacked | :unboxed
```

## Semantics

### Ordinary top-level values

- `(define ⟨name⟩ ...)` binds `⟨name⟩` as a top-level value in the current package.
- Top-level value initializers are evaluated at load time in dependency order. Unlike local [`let`](/manual/operators/let/) bindings, top-level values cannot form recursive or mutually recursive cycles; cyclic definitions must be functions.
- Polymorphic top-level value definitions are subject to the value restriction and monomorphism restriction: expansive initializers (such as function calls that may allocate or have effects) cannot be generalized to polymorphic types, and unresolved typeclass constraints on values are defaulted at top level.
- A top-level value definition may evaluate to `Void` (for load-time side effects), though a standalone `Void` binding cannot have an explicit [`declare`](/manual/operators/declare/).
- Local lexical bindings (`let`, `let*`, `fn` parameters, pattern variables) may shadow a top-level value.

### Functions

- `(define (⟨name⟩ ...) ...)` defines a top-level function.
- Positional parameters `⟨arg⟩*` may be variable names, `_`, or patterns (such as `(Tuple a b)` for single-constructor types). An unused named parameter may begin with `_` (such as `_unused`) to suppress unused-variable warnings.
- `(define (⟨name⟩) ...)` with an empty parameter list defines a nullary function of type `(Void -> ⟨return-type⟩)`.
- Optional keyword parameters follow [`&key`](/manual/operators/keyword-tail/); each is written `(⟨kw-var⟩ ⟨default-expr⟩)`, where `⟨default-expr⟩` is evaluated when the caller omits `:⟨kw-var⟩` and may reference earlier positional and keyword parameters.
- The function body is an implicit [`progn`](/manual/operators/progn/) (supporting shorthand `let` bindings) and introduces a return scope for [`return`](/manual/operators/return/).
- Top-level functions in the same `coalton-toplevel` unit may be mutually recursive in any order.
- A function can also be defined using value syntax with [`fn`](/manual/operators/fn/), such as `(define f (fn (x) ...))`.

### Dynamic variables

- A name beginning and ending with `*` and containing at least one non-`*` character (such as `*config*`) defines a dynamically scoped variable.
- Earmuffs are a syntactic rule in Coalton: ordinary lexical variables, function parameters, patterns, and method names cannot use `*...*` names, and function definition syntax `(define (*f* x) ...)` is invalid.
- The `define` form establishes the default top-level binding. Each reference to `*⟨name⟩*` reads the value from the current dynamic environment.
- [`dynamic-bind`](/manual/operators/dynamic-bind/) or shorthand `(let *⟨name⟩* = ⟨expr⟩)` temporarily rebinds dynamic variables for the dynamic extent of a body; all functions called within that extent observe the new value.
- Dynamic variables may have any Coalton type, including function types (defined in value syntax with [`fn`](/manual/operators/fn/)).
- Dynamic variables cannot be annotated with [`inline`](/manual/operators/inline/) or [`abi`](/manual/operators/abi/).

### Declarations, docstrings, and attributes

- Types are inferred automatically, or may be specified with a matching [`declare`](/manual/operators/declare/) in the same `coalton-toplevel`.
- Exported definitions without a matching `declare` signal a `coalton:deprecation-warning`.
- A string literal immediately following the name or parameter list is recorded as the docstring when followed by at least one body expression. (In `(define greeting "hello")`, `"hello"` is the value expression rather than a docstring.)
- [`inline`](/manual/operators/inline/), [`monomorphize`](/manual/operators/monomorphize/), and [`abi`](/manual/operators/abi/) attributes may precede either `define` or its `declare`.

## Examples

```lisp
(coalton-toplevel
  ;; Top-level constant with docstring
  (declare answer Integer)
  (define answer
    "The ultimate answer."
    42)

  ;; Function with pattern destructuring and keyword parameter
  (declare scale-point ((Tuple F64 F64) &key (:factor F64) -> (Tuple F64 F64)))
  (define (scale-point (Tuple x y) &key (factor 1.0d0))
    (Tuple (* x factor) (* y factor)))

  ;; Dynamic variable and dynamic rebinding
  (declare *base* Integer)
  (define *base* 10)

  (declare in-base (Integer -> Integer))
  (define (in-base x)
    (* x *base*)))

(coalton
  (Tuple (in-base 3)
         (dynamic-bind ((*base* 16))
           (in-base 3))))
; => #.(TUPLE 30 48)
```

## Other notes

- Unconstrained top-level functions compile to Common Lisp `defun` definitions with the same symbol name and arity, plus a global lexical binding so the symbol can be used both as a direct call operator and as a first-class function value.
- Functions with typeclass constraints receive hidden dictionary parameters before their positional arguments.
- See [`declare`](/manual/operators/declare/), [`fn`](/manual/operators/fn/), [`abi`](/manual/operators/abi/), and [`dynamic-bind`](/manual/operators/dynamic-bind/).
