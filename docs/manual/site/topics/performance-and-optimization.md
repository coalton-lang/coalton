---
title: "Performance and Optimization"
description: "Compiler optimizations, unboxing, stack allocation, and writing allocation-free Coalton code."
hideMeta: true
weight: 62
---

Coalton compiles to Common Lisp with static type declarations and performs several optimization passes before handing code to the host Lisp compiler. Understanding these optimizations helps when writing allocation-free inner loops and numerical code.

Note that while Coalton's own AST rewrites (such as inlining, specialization, and scalar replacement) are portable across Common Lisp implementations, low-level code generation details—such as unboxed register allocation from `(declare (type ...))` and stack allocation from `(declare (dynamic-extent ...))`—depend on the host Lisp compiler. The discussion below assumes [SBCL](https://www.sbcl.org/), and not all low-level optimizations are guaranteed on every Common Lisp implementation.

{{<toc>}}

## Development vs. Release Mode

By default, Coalton compiles in **development mode**, which prioritizes interactive redefinition at the REPL (for example, compiling algebraic data types as CLOS classes so types and methods can be redefined).

Setting the environment variable `COALTON_ENV=release` before compiling Coalton (or configuring `(:compiler-mode "release")` via [`:coalton-config`](/manual/topics/configuring-coalton/)) enables **release mode**, which:

- Compiles algebraic data types and structs as `cl:defstruct` definitions with `sb-ext:freeze-type` on SBCL.
- Emits concrete Common Lisp `(declare (type ...))` annotations on function parameters, [`let`](/manual/operators/let/)/[`let*`](/manual/operators/let-star/) bindings, [`for`](/manual/operators/for/)/[`for*`](/manual/operators/for-star/) bindings, and [`match`](/manual/operators/match/) pattern variables.
- Makes the final branch of exhaustive `match` expressions unconditional so the Lisp compiler does not synthesize a dead `nil` return path, preserving tail-call optimization and concrete return types.
- Enables **scalar replacement** of non-escaping structs, single-constructor data types (such as `Tuple`), and branching sum types (such as `Optional` and `Result`), as well as **unboxing** of non-escaping `coalton/cell:Cell` bindings.
- Enables stack allocation (`dynamic-extent`) of freshly constructed `match` scrutinees whose pattern bindings do not escape.

## Local `Cell` Unboxing

Imperative accumulation inside [`for`](/manual/operators/for/) loops and local algorithms often uses `coalton/cell:Cell`. At runtime, an escaping `Cell :a` is a heap-allocated struct whose slot has Lisp type `t` (which requires boxing machine words such as `U64` or `F64`).

In release mode, after inlining, the compiler detects local bindings of `(cell:new init)` that **do not escape** their lexical scope and rewrites them into typed mutable Common Lisp lexical variables (`let` + `(declare (type ...))` + `setq`):

- `cell:read` lowers to a direct variable read.
- `cell:write!` lowers to `(setq var value)`.
- `cell:swap!` lowers to a temporary binding of the old value followed by `setq`.
- Inlined helpers built on them (`cell:update!`, `cell:update-swap!`, `cell:increment!`, `cell:decrement!`, `cell:push!`, and `cell:pop!`) are unboxed automatically.
- `Cell` bindings initialized across `if`, `cond`, or `match` branches (where every returning branch constructs a `Cell` with `cell:new`), as well as invariant `Cell` bindings in `for` and `for*` clauses (e.g., `(for ((acc (cell:new 0) acc) ...) ...)`), are also unboxed.

For example:

```lisp
(declare accumulate (U64 * F64 -> U64 * F64))
(define (accumulate n delta)
  (let count = (cell:new 0))
  (let total = (cell:new 0.0d0))
  (for ((declare i U64)
        (i 0 (+ i 1)))
    :while (< i n)
    (cell:increment! count)
    (cell:write! total (+ (cell:read total) delta))
    (cell:swap! count (+ (cell:read count) i)))
  (values (cell:read count) (cell:read total)))
```

In release mode, neither `count` nor `total` allocates a `cell-internal` struct. Instead, they are bound as local lexical variables declared `(type (unsigned-byte 64) ...)` and `(type double-float ...)`, allowing SBCL to keep both accumulators in CPU registers with zero heap allocation.

A `Cell` is considered non-escaping when it is only accessed through the operations above—including inside `for`/`for*` loops, `match`/`if`/`cond` branches, immediate anonymous lambda applications `((fn (...) ...) ...)`, and non-escaping local helper functions (`let`/`let*`). Non-returning control transfers in initializer branches (`return`, `break`, `continue`, `throw`, or `error`) are also permitted. If a `Cell` is returned, stored in an escaping data structure, passed to an uninlined function, or captured by an escaping closure, it retains its heap-allocated `cell-internal` representation.

## Scalar Replacement of Structs, Tuples, and Algebraic Data Types

### Structs and Single-Constructor Data Types

In release mode, locally constructed immutable structs ([`define-struct`](/manual/operators/define-struct/)) and single-constructor algebraic data types ([`define-type`](/manual/operators/define-type/), such as `Tuple`) that are consumed only through generated field readers (`.field`), irrefutable [`match`](/manual/operators/match/) destructuring (including nested patterns), or inlined helpers (such as `fst`, `snd`, `swap`, and `bimap`) are replaced by typed scalar local bindings for their fields:

```lisp
(define-struct Pair
  (left U64)
  (right F64))

(declare sum-pair (U64 * F64 -> F64))
(define (sum-pair n x)
  (let p = (Pair n x))
  (+ (into (.left p)) (.right p)))
```

All field initializers still evaluate once, in left-to-right order, before the body runs, without allocating the enclosing `Pair` or `Tuple` instance. This also applies when a single-constructor value is produced across local `if`, `cond`, or `match` branches—the branches return the unboxed field values via `cl:values` into a typed `cl:multiple-value-bind`.

### Branching Sum Types (`Optional`, `Result`, and Custom ADTs)

In release mode, when a local binding is initialized across [`if`](/manual/operators/if/), [`cond`](/manual/operators/cond/), or [`match`](/manual/operators/match/) branches by known constructors of a sum type (such as `Optional`, `Result`, `List`, or a user-defined `define-type`) and is only consumed by local `match` expressions, scalar replacement unboxes both the variant tag and the payload fields across multiple passes:

- A two-variant sum (such as `Some`/`None` or `Ok`/`Err`) uses an unboxed `Boolean` tag that compiles to `cl:if`, while sums with three or more variants use an unboxed `UFix` tag.
- Each distinct payload type is bound to a typed scalar local (e.g., `(type (unsigned-byte 64) ...)` or `(type double-float ...)`), eliminating both heap wrapper allocation (`Result`, `cl-optional`) and number boxing (`U64`, `F64`).
- Branches that transfer control with `return`, `break`, `continue`, `throw`, or `error` do not prevent unboxing of the remaining constructor branches.
- Even when an `Optional :a` value is not scalar-replaced (for example, when received as a function parameter), matching `(Some x)` when `:a` is statically disjoint from `Optional` binds `x` directly with its concrete Lisp type without calling a runtime unwrap helper:

```lisp
(declare step-value (U64 -> U64))
(define (step-value acc)
  (let ((opt (if (== (coalton/bits:and acc 1) 0)
                 (Some (+ acc 3))
                 None))
        (tup (if (== (coalton/bits:and acc 2) 0)
                 (Tuple (+ acc 5) 1)
                 (Tuple (+ acc 7) 2)))
        (res (match opt
               ((Some v) (Ok (+ v (coalton/tuple:fst tup))))
               ((None)   (Err (+ acc (coalton/tuple:snd tup)))))))
    (match res
      ((Ok good) (+ good 11))
      ((Err bad) (+ bad 11)))))
```

In release mode, `opt`, `tup`, and `res` are all scalar-replaced into unboxed `Boolean` tags and `(unsigned-byte 64)` locals with zero heap allocation.

## Stack Allocation (`dynamic-extent`)

### Non-Escaping Local Functions

When a local function ([`fn`](/manual/operators/fn/)) is bound in [`let`](/manual/operators/let/) or [`let*`](/manual/operators/let-star/) and does not escape its lexical scope, Coalton emits `(declare (dynamic-extent #'fn-name ...))` on the generated `flet` or `labels` binding:

```lisp
(declare sum-with-step (U64 -> U64))
(define (sum-with-step n)
  (let acc = (cell:new 0))
  (let ((step! (fn (x)
                 (cell:write! acc (+ (cell:read acc) x)))))
    (for ((declare i U64)
          (i 0 (+ i 1)))
      :while (< i n)
      (step! i)
      (step! 1))
    (cell:read acc)))
```

Because `step!` is only called directly and never returned or stored, `step!` is declared `dynamic-extent` and `acc` is unboxed into a typed `U64` lexical variable, so the entire function runs without heap allocation. Local functions passed as the callback to `cell:update!` or `cell:update-swap!`, or called only by other non-escaping local functions, also receive `dynamic-extent`.

### `match` Scrutinees and [`rec`](/manual/operators/rec/) Loops

- **Immediate `match` on constructors**: In release mode, when a `match` expression scrutinizes a freshly constructed algebraic data value and none of the pattern-bound variables escape the branch bodies, the scrutinee is bound with `(declare (dynamic-extent ...))`.
- **Tail-recursive `rec` loops**: [`rec`](/manual/operators/rec/) compiles to a local `labels` loop and automatically eliminates typeclass dictionary parameters that resolve to context-free global instances once the loop's types are concrete.
- **Multiple return values**: Returning multiple values with [`values`](/manual/operators/values/) and destructuring them with `(let (values a b) = ...)` uses Common Lisp multiple values (`cl:values` / `cl:multiple-value-bind`) rather than allocating a `Tuple`.

## Directives for Controlling Optimization

Coalton provides several directives for guiding the optimizer:

- [`inline`](/manual/operators/inline/) and [`noinline`](/manual/operators/noinline/): Control function inlining at definition sites or individual call sites. Inlining higher-order helpers and small constructors often exposes local `Cell`s and structs to scalar replacement.
- [`specialize`](/manual/operators/specialize/): Register type-directed rewrite rules that replace generic typeclass-constrained functions with specialized implementations when concrete types are known.
- [`monomorphize`](/manual/operators/monomorphize/): Inline and specialize a polymorphic definition at its call sites to eliminate runtime dictionary dispatch.
- [`repr`](/manual/operators/repr/): Choose the underlying runtime representation of a `define-type` (`:enum`, `:transparent`, `:lisp`, or `:native`).
- [`likely`](/manual/operators/likely/) and [`unlikely`](/manual/operators/unlikely/): Provide branch probability hints on conditionals.
- [`unsafe`](/manual/operators/unsafe/): Locally compile a body with `(optimize (safety 0) (sb-c::type-check 0))`.

## Inspecting Generated Code

To verify whether a function unboxes its locals or stack-allocates closures, use [`pprint-coalton-codegen`](/manual/operators/pprint-coalton-codegen/) to inspect the generated Common Lisp code, or `cl:disassemble` at the Lisp REPL (see [Debugging Operators](/manual/topics/debugging/)).
