---
title: "Coalton for Common Lispers"
description: "Why Coalton matters to Common Lisp programmers, key mental-model shifts, optimizations, and how to architect Coalton programs."
hideMeta: true
math: true
weight: 15
---

If you already use Common Lisp heavily, you have a fast native compiler, interactive development in Emacs, macros, `defstruct`, CLOS, conditions and restarts, and type declarations. Common Lisp is already an exceptionally productive language for exploratory programming, metaprogramming, and building large dynamic systems.

Coalton is not a wholesale replacement for Common Lisp. It is a statically typed language embedded *inside* Common Lisp that compiles down to ordinary Lisp functions and data structures in the same running image. It is designed for the parts of a Lisp system where Common Lisp's dynamic model starts to work against you: maintaining invariants across large refactors, modeling closed data variants without nullable slots, and writing generic, modular algorithms without giving up performance.

In this article, we give a preview of Coalton for the working Common Lisp programmer. When you are ready for a more complete tour of Coalton's syntax and features, continue to the [Coalton Language Guide](/manual/topics/language-guide/).

{{<toc>}}

## Where Common Lisp's Type System Stops

In Common Lisp, types are effectively predicates over objects checkable with `cl:typep`. Types can be used dynamically with `cl:typep` and `cl:check-type`, but they're also possible to put in variable, function, and slot declarations. Some compilers, such as SBCL, may use this information for local type inference, warnings, and representation selection; others may not use it at all.

Because Common Lisp types describe run-time values and storage layouts rather than static program-wide contracts, you eventually run into several limits:

1. **Type declarations can be freely ignored by the compiler**: Under the ANSI Common Lisp standard ([CLHS 3.3.1](https://www.lispworks.com/documentation/HyperSpec/Body/03_ca.htm)), `type` and `ftype` declarations are optional promises from the programmer to the compiler (with undefined consequences if violated) that a conforming implementation is free to ignore completely.
2. **Compound types that *look* precise often collapse to `t`**: In Common Lisp, `(vector person)` looks like a vector of `person` structs, but array element types are governed by storage layout (`upgraded-array-element-type`), not static type safety. Because any struct or class upgrades to `t`, `(vector person)` is actually the exact same type as `(vector t)`, meaning `(typep #("not-a-person" 42) '(vector person))` evaluates to `T`, and storing a string into a `(make-array 5 :element-type 'person)` succeeds.
3. **Built-in containers cannot be type-checked without expensive scans**: There is no built-in type specifier for "a list of integers" or "a hash table from strings to points," and checking one with `(satisfies ...)` requires walking the entire container at run time.
4. **Nullable values are implicit everywhere**: Ordinary Common Lisp code relies heavily on `nil` to mean "uninitialized", "no value", etc. Variables and slots are often `nil`, and it's easy to make the mistake of being imprecise, as in not using `(or null person)`, or not being careful to do run-time checks, as in forgetting `(null person)` checks before accessing the `person` object.
5. **Refactoring across files relies on run-time tests**: If you add a new variant to a data representation or change a function's return contract, Common Lisp will still compile callers that are now broken, waiting until that code path runs to signal a `type-error` or `no-applicable-method` (if sufficient `safety` is enabled) or suffer memory corruption or a hard crash.

Coalton adds a global, compile-time type checker and optimizer to the usual Lisp workflow. You load Coalton systems with ASDF, recompile top-level forms interactively from your editor, inspect stack traces in the standard Lisp debugger, and call Coalton functions from Common Lisp (or Common Lisp functions from Coalton) in the same image.

## Core Concepts in Common Lisp Terms

You do not need a background in category theory or functional programming to be productive in Coalton. Most of Coalton's core features solve problems you have already encountered in Common Lisp.

### Disentangling `nil`

In Common Lisp, the value `nil` is overloaded to mean at least four different things:

1. Boolean falsity (`nil` vs. `t`).
2. The empty list (`'()`).
3. Absence of a value ("not found" from `gethash` or `find`).
4. An empty result or placeholder value.

Every Common Lisper has written `(multiple-value-bind (val presentp) (gethash k table) ...)` because `nil` might be a legitimate value stored in the table.

Coalton separates these roles into distinct, non-overlapping types:

| Role in Common Lisp | Coalton Type | Coalton Values |
| :--- | :--- | :--- |
| Boolean | `Boolean` | `True` and `False` (`t` is an ordinary variable name!) |
| List | `List :a` | `Nil` (empty list) and `(Cons first rest)` |
| Optional / nullable value | `Optional :a` | `None` (absent) and `(Some value)` (present) |
| "Nothing interesting" | `Unit` | `Unit` |
| Zero return values | `Void` | `(values)` |

Because `Optional Integer` and `Integer` are different types, you can never accidentally pass a "not found" result into `+`. The type checker forces you to handle the `None` case first.

### Closed Variants and Pattern Matching

Suppose you are modeling the result of a payment request in Common Lisp. An outcome might be:

- Approved, with a transaction ID (`integer`) and timestamp (`integer`);
- Declined, with a reason string (`string`); or
- RetryLater, with a backoff delay in seconds (`integer`).

In Common Lisp, you typically choose between:

- A `defstruct` with slots `status`, `transaction-id`, `reason`, and `retry-seconds`, where half the slots are `nil` depending on `status`;
- Tagged lists like `(:approved 1042 1700000000)` or `(:declined "Card expired")`; or
- A CLOS abstract base class `payment-result` with three subclasses.

In Coalton, you express this directly with `define-type`:

```lisp
(coalton-toplevel
  (define-type PaymentResult
    (Approved Integer Integer)
    (Declined String)
    (RetryLater Integer)))
```

`PaymentResult` is a single type with three *constructors* (`Approved`, `Declined`, and `RetryLater`), each carrying only the fields that make sense for that case. Functions that work with `PaymentResult` discriminate between these constructors, like this function to describe a payment as a string:

```lisp
(coalton-toplevel
  (declare describe-payment (PaymentResult -> String))
  (define (describe-payment result)
    (match result
      ((Approved tx-id _timestamp)
       (<> "Approved transaction #" (into tx-id)))
      ((Declined reason)
       (<> "Declined: " reason))
      ((RetryLater seconds)
       (<> "Please retry in " (<> (into seconds) "s"))))))
```

Think of `define-type` + `match` as a combination of `defstruct`, `etypecase`, and `destructuring-bind` that is checked at compile time:

- You cannot read `reason` unless you are in the `Declined` branch.
- You cannot misspell a constructor name.
- If you later add a fourth constructor `(Cancelled)` to `PaymentResult`, the compiler will immediately flag `describe-payment` as non-exhaustive.
- All branches must return the same kind of object, in this case a `String`.

When a type really is just a single record with named fields, like a `defstruct`, one uses `define-struct` instead:

```lisp
(coalton-toplevel
  (define-struct Point
    (x F64)
    (y F64))

  (declare distance-squared (Point -> F64))
  (define (distance-squared p)
    (+ (* (.x p) (.x p))
       (* (.y p) (.y p)))))
```

### Type Variables: Connecting Inputs, Callbacks, and Outputs

Suppose you write a utility `find-where` that searches for an object in a custom tree data structure `tree` based on a given predicate, like

```lisp
(find-where #'evenp coin-denominations)
(find-where #'player-health-low-p (get-players))
(find-where (lambda (x) (string= "COBOL" x)) *language-tree*)
(find-where #'point-at-origin-p (graph-quadtree g))
```

Suppose a caller passes a predicate on `point` structs and a tree of `point` structs, with all of their types declared. The most precise `ftype` declaration you can write for `find-where` in Common Lisp is still:

```lisp
(function ((function (t) boolean) tree) t)
```

A Common Lisp compiler with good type inference still cannot ascertain from that signature that the objects in the tree have the same type as the argument to the callback, or that `find-where` returns an object of that same type.

Coalton expresses these relationships with *type variables*, written as keyword symbols (`:a`, `:b`, `:key`, `:val`), while concrete type names are capitalized (`Integer`, `String`, `List`, `Vector`). Given a tree data structure like

```lisp
(coalton-toplevel
  (define-type (Tree :a)
    (Node (Tree :a) (Tree :a))
    (Leaf :a)))
```

we can write `find-where` as:

```lisp
(coalton-toplevel
  (declare find-where ((:a -> Boolean) * Tree :a -> Optional :a))
  (define (find-where pred tree)
    (match tree
      ((Leaf x)
       (if (pred x)
           (Some x)
           None))
      ((Node left right)
       (match (find-where pred left)
         ((Some x) (Some x))
         ((None) (find-where pred right)))))))
```

Because `:a` appears in all three positions of `(:a -> Boolean) * Tree :a -> Optional :a`, the compiler enforces two things across every call site that `(function ((function (t) boolean) tree) t)` cannot:

- Passing a predicate of type `Point -> Boolean` together with a `Tree String` is rejected at compile time.
- Calling `find-where` on a `Tree Point` returns an `Optional Point`, so the caller still knows it has a `Point` without any extra type declarations.

### Protocols and Type Classes

In Common Lisp, when several unrelated types support the same operations, such as serialization, equality, or arithmetic, we usually define a *protocol*: a set of `defgeneric` functions that participating types implement with `defmethod`. Common Lisp leaves that grouping informal, though. There is no built-in way to name the protocol itself, require it in a function's type declaration, or check at compile time that a type implements all of its methods.

Coalton represents protocols directly as **type classes**:

- `define-class` declares a named protocol and the type signatures of its operations.
- `define-instance` implements that protocol for a particular type.

> [!NOTE]
> Notice the difference in terminology from CLOS: in Coalton, data types are defined with `define-type` or `define-struct`. The words "class" and "instance" in `define-class` and `define-instance` refer to a protocol and a type's implementation of that protocol. This terminology comes from Haskell and is standard in languages with type classes, where a "class" is a classification of types rather than a blueprint for objects.

Here is a protocol for outcomes that can be summarized as a string and checked for whether they can be retried:

```lisp
(coalton-toplevel
  ;; Similar to a group of DEFGENERIC declarations that form one contract:
  (define-class (Reportable :a)
    (summary (:a -> String))
    (recoverable? (:a -> Boolean)))

  ;; A DEFINE-INSTANCE must implement every method in the protocol:
  (define-instance (Reportable PaymentResult)
    (define (summary r)
      (describe-payment r))
    (define (recoverable? r)
      (match r
        ((RetryLater _) True)
        (_ False)))))
```

Functions that use `Reportable` operations state that requirement in their declaration before `=>`:

```lisp
(coalton-toplevel
  (declare format-report (Reportable :a => :a -> String))
  (define (format-report item)
    (if (recoverable? item)
        (<> "[retryable] " (summary item))
        (<> "[final] " (summary item)))))
```

Compared to CLOS generic functions, type classes behave differently in three ways that matter in practice:

1. **Completeness is checked at compile time**: Omitting `recoverable?` from `define-instance`, or calling `(format-report x)` on a type that does not implement `Reportable`, is a compile-time error rather than a run-time `no-applicable-method`.
2. **Methods can dispatch on return types**: A CLOS generic function can only dispatch on the run-time classes of the arguments passed into it; a zero-argument `(defgeneric default ())` cannot dispatch on anything, so in Common Lisp you have to pass a type symbol like `(default 'point)` or `(cl:coerce x 'list)`. In Coalton, dispatch is driven by static types, including the *return type* expected by the caller. For example, the built-in `Default` type class has a zero-argument method `(default)`:

   ```lisp
   (coalton-toplevel
     (define-instance (Default Point)
       (define (default)
         (Point 0.0d0 0.0d0)))

     ;; Because DISTANCE-SQUARED expects a Point, (default)
     ;; automatically selects the Point instance:
     (define origin-distance (distance-squared (default))))
   ```

3. **Static resolution and inlining**: When concrete types are known at compile time, the compiler can resolve the instance methods directly and inline them without run-time method dispatch.
4. **Fast run-time dispatch**: When a generic call is not inlined away, calling a type class method is just a structure slot read and a `funcall`.

## Writing Modular Code Without Giving Up Speed

Experienced Common Lisp programmers know how to make an inner loop fast: add `(declare (type double-float ...))` to every variable, avoid CLOS dispatch in hot paths, avoid allocating wrapper structs or `cons` cells for intermediate results, and write `defmacro` generators when the same numerical kernel needs to run on `single-float`, `double-float`, and `fixnum`.

The catch is that optimizing Common Lisp this way usually means dismantling your abstractions:

- Writing an algorithm with `defgeneric` (or unspecialized `cl:+` and `cl:*` across helper functions) incurs run-time dispatch and boxes intermediate numbers; avoiding that in Common Lisp means replacing ordinary functions with macro-generators that stamp out `foo-f32`, `foo-f64`, and `foo-fixnum`.
- A reusable struct like `(defstruct interval low high)` either leaves `low` and `high` as type `t` (boxing every float) or hard-codes `:type double-float` (duplicating the struct and its functions for every numeric type).
- Wrapping a `double-float` in a `defstruct` or `defclass` so you don't mix up `meters` and `seconds` boxes every number on the heap, whereas `deftype` is just an alias that doesn't catch mixing them up.
- Returning `nil` on failure from a function that otherwise produces a `double-float` gives the return value type `(or null double-float)`, which forces the float to be heap-boxed unless you rewrite callers around `(values 0.0d0 nil)` with dummy numbers and boolean flags.

Coalton avoids this trade-off for two reasons. First, it knows the static types of every expression across function boundaries *before* generating Common Lisp. Second, Coalton has an explicit **release mode**. In Common Lisp, the compiler must generally assume the program is never "done", because classes, structs, and functions might be redefined at the REPL at any time. Coalton's **development mode** keeps that interactive flexibility (for instance, compiling data types as CLOS classes so they can be redefined live), while **release mode** lets the compiler assume that types, instances, and functions are sealed and can be specialized and unboxed across the program.

### Fast Functions from Generic Functions

Suppose you want to evaluate a polynomial $c_0 + x(c_1 + x(c_2 + \dots))$ using Horner's method:

```lisp
(defun horner (x coeffs)
  (if (null coeffs)
      0
      (+ (car coeffs) (* x (horner x (cdr coeffs))))))
```

In Common Lisp, how do you make this function both **generic** (working on `double-float`, `single-float`, `rational`, and user-defined types like matrices or polynomials) and **fast** (running with unboxed `double-float` arithmetic when given `double-float`s)?

- `(declaim (inline horner))` cannot inline a recursive function away.
- The literal `0` returns an `integer`, not `0.0d0`, and `(car coeffs)` has type `t`, so every step goes through generic `cl:+` and `cl:*` and boxes floats.
- Built-in `cl:+` and `cl:*` cannot be extended to user-defined types anyway; if you replace them with CLOS generic functions `generic-+` and `generic-*`, every step of the recursion pays for CLOS dispatch.
- To get fast specialized versions in Common Lisp, you end up turning `horner` (and every helper function it calls) into a macro that stamps out `horner-f64`, `horner-f32`, and so on.

In Coalton, you write `horner` as an ordinary generic function once:

```lisp
(coalton-toplevel
  (declare horner (Num :a => :a * List :a -> :a))
  (define (horner x coeffs)
    (match coeffs
      ((Nil) 0)
      ((Cons c rest) (+ c (* x (horner x rest)))))))
```

Because `+`, `*`, and the literal `0` are all methods of the `Num` type class, `horner` works on `Integer`, `Fraction`, `F32`, `F64`, `Complex`, or your own matrix or polynomial type.

When you want a fast `F64` version, you do not rewrite `horner` as a macro. Instead, you ask the compiler to [`monomorphize`](/manual/operators/monomorphize/) a concrete entry point:

```lisp
(coalton-toplevel
  (monomorphize)
  (declare horner-f64 (F64 * List F64 -> F64))
  (define (horner-f64 x coeffs)
    (horner x coeffs)))
```

Coalton walks the call graph from `horner-f64` and generates a specialized Common Lisp clone of the recursive `horner` function for `F64`: the literal `0` becomes `0.0d0`, `+` and `*` become direct `double-float` operations, and every variable and list element `c` is declared `(type double-float ...)`. You keep a single generic function in your source code while getting the same Common Lisp output you would have written by hand or generated with a macro.

### Distinct Wrapper Types Without Heap Allocation

Suppose you want separate types for `Meters` and `Seconds` so you cannot accidentally add a distance to a duration. In Common Lisp, `deftype` does not make them distinct types, while `defstruct` allocates a heap object for every number.

In Coalton, attaching `(repr :transparent)` to a single-field type gives you a distinct compile-time type that erases to the underlying value at run time:

```lisp
(coalton-toplevel
  (repr :transparent)
  (define-type Meters
    (Meters F64))

  (repr :transparent)
  (define-type Seconds
    (Seconds F64))

  (declare speed (Meters * Seconds -> F64))
  (define (speed (Meters d) (Seconds t))
    (/ d t)))
```

Passing `(Seconds 5.0d0)` as the first argument to `speed` is a compile-time error, but at run time `Meters` and `Seconds` are just raw `double-float`s and `speed` compiles to a direct `double-float` division.

### Using `Optional`, `Result`, and Structs Without Boxing in Inner Loops

In performance-critical Common Lisp, programmers often avoid returning wrapper structs or `(or null double-float)` from helpers inside a loop because doing so allocates on the heap or boxes floats. Instead, they end up threading multiple values with dummy numbers and boolean flags, such as `(values 0.0d0 nil)`.

In Coalton's release mode, you can use ordinary structs, `Tuple`s, `Optional`, `Result`, and local mutable `Cell`s inside a function without paying for those intermediate allocations: when those values do not escape the function, the compiler rewrites them into plain, type-declared Common Lisp lexical variables automatically.

### Generating Type Declarations Automatically

When writing fast Common Lisp by hand, leaving a single `(declare (type ...))` off an intermediate `let` binding can quietly reintroduce generic dispatch and float boxing. Conversely, if you refactor a type and forget to update a hand-written `(the ...)` form under `(optimize (safety 0))`, you can corrupt memory.

In release mode, Coalton generates Common Lisp `(declare (type ...))` annotations on function parameters, `let` bindings, loop variables, and `match` variables automatically from the static types it has already proved.

See [Performance and Optimization](/manual/topics/performance-and-optimization/) for examples of the generated Common Lisp code.

## How Programming in Coalton Feels Different

Even though Coalton is a language within Common Lisp, many of the practices and patterns of Common Lisp don't apply directly. Adopting Coalton means adjusting how you model data, organize abstractions, and iterate at the REPL.

### Data Modeling Comes First, Not Last

In Common Lisp, it is common to start prototyping with nested lists, alists, or plists like `'(:name "Alice" :age 42 :roles (:admin :staff))` where a single list mixes strings, integers, and symbols, and to put off defining `defstruct` or `defclass` types until the design settles.

In Coalton, every collection is homogeneous: all elements of a list or vector must have the same type. You cannot pass around an untyped plist as a general-purpose record. Instead, you start by writing down the types of your domain data using structs and sum types. Because those definitions are only a few lines of code, sketching them up front is lightweight, and the compiler immediately helps check the functions you write against them.

### Variants and Protocols Instead of Inheritance Hierarchies

In CLOS, a class hierarchy often serves two purposes at once: it lets you store different kinds of objects together (such as a list of `shape` instances containing both circles and rectangles), and it lets different types share generic operations and slots.

Coalton has no subtyping and no class inheritance. A 32-bit integer is not a subtype of `Integer`, and one struct cannot inherit fields from another. Instead, you separate those two jobs:

- When you have a fixed set of cases that need to live in the same container, you define a single sum type (`define-type`) with a constructor for each case and branch on it with `match`.
- When you have an open set of types that share behavior, you define a type class protocol (`define-class`). Unlike a CLOS method specialized on a superclass, a generic function constrained by a type class still operates on one concrete type at a time: a list of drawable items is a list of circles or a list of rectangles, not a mixed list of both.

### Explicit Conversions Instead of Numeric Contagion

Common Lisp's numeric tower automatically promotes operands across `integer`, `ratio`, and `float` in expressions like `(+ 1 2.5d0 3/4)`.

In Coalton, distinct numeric types never convert into one another behind your back. Even though arithmetic operators are generic across all numeric types, both inputs and the output of `+` or `*` must be the exact same type. Combining an integer and a floating-point number requires an explicit conversion. Mixed-precision code takes a little more care to write, but accidental float contagion and silent loss of exactness cannot happen implicitly.

### Compiler-Guided Refactoring

In Common Lisp, when you change a function's signature or a type's layout with `C-c C-c`, the compiler accepts the new definition silently, and you typically discover which callers were affected when they fail at run time.

In Coalton, each `coalton-toplevel` form is type-checked before code is emitted, so a function with a type mismatch or an unhandled `match` case in its own body is caught immediately. And when you interactively redefine an existing function with an incompatible type signature, Coalton doesn't just silently leave callers broken—or force you to fix the entire program before compiling the new definition. Instead, it tracks dependencies across functions, reports the old and new types along with a list of every affected caller (and its source location), and offers Common Lisp restarts (`Continue anyway` or `Abort redefinition`, configurable via `:auto-continue-redefinition`) so you can install the new definition and work through the affected callers one by one.

## Quirks and Differences from Common Lisp

At the surface syntax level, keep these everyday differences in mind:

* Coalton is a Lisp-1: functions and variables share a single namespace, so you pass functions by name without `#'` and call function variables directly without `funcall`.
* Coalton uses `True` and `False` (not `t` and `nil`). As such, `t` may be used as an ordinary variable name.
* For testing equality, Coalton uses double-equals, `==`.
* Lists in Coalton must be homogeneous.
* To denote anonymous functions, Coalton uses `fn` (*not* `lambda`), with
  `ƒ` available as short `fn` syntax in Coalton source.
* Numerical operators like `+`, `-`, `*`, and `/` are binary (fixed 2-argument functions).
* Negation is done with `negate`; `-` is always binary subtraction.

For a fuller side-by-side table of Common Lisp and Coalton names, see the [glossary](https://github.com/coalton-lang/coalton/blob/main/docs/glossary.md).

## Structuring Coalton and Hybrid Lisp Systems

Because Coalton and Common Lisp share the same image, you do not have to commit an entire project to one or the other:

- **Use Common Lisp for dynamic systems and ecosystem glue**: Macro-heavy DSLs, run-time metaprogramming, dynamic code loading, and interfaces to existing Quicklisp libraries (networking, databases, CFFI, GUIs, web servers) are often easiest to write in plain Common Lisp.
- **Use Coalton for invariant-heavy cores and kernels**: Domain models, compilers and AST passes, state machines, numerical algorithms, and subsystems that undergo frequent refactoring benefit most from static types, exhaustive pattern matching, and release-mode optimization.
- **Keep the boundary between them narrow**: Coalton includes an inline `lisp` form for calling Common Lisp directly, where you state the Coalton type that the Lisp expression returns. Because Coalton trusts that annotation without type-checking the Lisp body inside it, it is good practice to keep raw `lisp` calls inside small wrapper functions that convert Lisp values and conditions into typed Coalton values at the boundary.
- **Keep interfaces immutable and internals pragmatic**: Coalton works best when functions accept and return immutable values at module boundaries, making components easy to test and compose. Inside a function body, however, you do not need to force everything into pure recursion: imperative loops, early returns, and local mutable accumulators are completely normal Coalton style.
- **Match error handling to the kind of failure**: Rather than signaling conditions for every failure or returning `nil`, Coalton code typically uses `Optional` when a lookup simply finds nothing, `Result` when an operation can fail in expected ways that the caller should handle explicitly, and exceptions or resumptions for rare failures across deep call stacks or interoperation with Common Lisp conditions.

Used this way, Coalton doesn't ask you to give up Common Lisp's interactive image, libraries, or macros. Instead, it gives you another tool inside the same Lisp environment for the parts of your program where static types, explicit data variants, and compiler-driven specialization pay off.

## Where to Go Next

- **[Introduction and Getting Started](/manual/topics/introduction/)**: Setting up a Common Lisp environment, Quicklisp, Emacs/SLIME, and your first `.asd` + `.ct` project.
- **[Coalton Language Guide](/manual/topics/language-guide/)**: The main tour of Coalton's syntax, types, collections, loops, and type classes.
- **[Advanced Coalton Programming](/manual/topics/advanced-coalton-programming/)**: Functional dependencies, custom collection builders, overlapping instances, `forall` and `Values` polymorphism, the value restriction, resumptions, and function specialization.
- **[Lisp Interop](/manual/topics/lisp-interop/)**: Calling Coalton from Common Lisp, calling Common Lisp from Coalton, and controlling data representations with `(repr ...)`.
- **[Performance and Optimization](/manual/topics/performance-and-optimization/)**: Development vs. release mode, `Cell` and ADT unboxing, stack allocation, `inline`, and `monomorphize`.
- **[Glossary of Terms](https://github.com/coalton-lang/coalton/blob/main/docs/glossary.md)**: A side-by-side lookup table of Common Lisp and Coalton terminology.
