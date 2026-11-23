---
title: "Coalton for Common Lispers"
description: "Why Coalton matters to Common Lisp programmers and how to think about programming in Coalton."
hideMeta: true
math: true
weight: 15
---

If you already use Common Lisp, you may wonder what another language in the same image buys you. You already have macros, `defstruct`, CLOS, conditions and restarts, type declarations, and an interactive compiler.

Coalton builds on that environment. It is a statically typed language embedded in Common Lisp that compiles to ordinary Lisp functions and data structures. Its type checker can connect the types of a container, a callback, and a function's result; check that a pattern match covers the possible cases; and provide the type information needed to specialize a generic numerical algorithm. You still develop in the Lisp image and use Common Lisp where its flexibility is useful.

This article introduces those ideas through familiar Lisp problems. For a more complete tour of Coalton's syntax and features, continue to the [Coalton Language Guide](/manual/topics/language-guide/).

{{<toc>}}

## What Coalton adds to Common Lisp's type system

In Common Lisp, types describe sets of objects whose membership you can test with `cl:typep`. You can check types at run time with `cl:typep` and `cl:check-type`, and state them in variable, function, and slot declarations. Compilers such as SBCL use this information for type inference, warnings, and representation selection.

Those declarations are useful, but they do not express all the relationships you might want a compiler to check:

1. **Declarations are promises, not required checks**: Under the ANSI Common Lisp standard ([CLHS 3.3.1](https://www.lispworks.com/documentation/HyperSpec/Body/03_ca.htm)), a conforming implementation may ignore `type` and `ftype` declarations. They are promises from the programmer, with undefined consequences if violated, rather than a requirement that the compiler prove the program respects them.
2. **Array element types describe storage**: `(vector person)` looks like a vector of `person` structs, but array element types are governed by storage layout (`upgraded-array-element-type`). The element type `person` upgrades to `t`, so `(vector person)` is the same type as `(vector t)`. For example, `(typep #("not-a-person" 42) '(vector person))` evaluates to `T`, and storing a string into a `(make-array 5 :element-type 'person)` succeeds.
3. **List and hash-table types do not describe their contents**: There is no built-in type specifier for "a list of integers" or "a hash table from strings to points." You can write a predicate and use `(satisfies ...)`, but checking every element this way requires scanning the container at run time.

Coalton adds a global, compile-time type checker and optimizer to the usual Lisp workflow. You load Coalton systems with ASDF, recompile top-level forms interactively from your editor, inspect stack traces in the standard Lisp debugger, and call Coalton functions from Common Lisp (or Common Lisp functions from Coalton) in the same image.

## Core concepts in Common Lisp terms

You can approach Coalton's types and protocols through ideas you already know from Common Lisp. The examples below start with ordinary Lisp problems and show what changes when the compiler can check the relationships between types.

### Disentangling `nil`

In Common Lisp, the value `nil` is overloaded to mean at least four different things:

1. Boolean falsity (`nil` vs. `t`).
2. The empty list (`'()`).
3. Absence of a value ("not found" from `gethash` or `find`).
4. An empty result or placeholder value.

If you've used `gethash`, you've probably needed its second return value: `nil` alone cannot tell you whether a key is missing or its stored value is `nil`.

Coalton separates these roles into distinct, non-overlapping types:

| Role in Common Lisp | Coalton Type | Coalton Values |
| :--- | :--- | :--- |
| Boolean | `Boolean` | `True` and `False` (`t` is an ordinary variable name!) |
| List | `List :a` | `Nil` (empty list) and `(Cons first rest)` |
| Optional / nullable value | `Optional :a` | `None` (absent) and `(Some value)` (present) |
| "Nothing interesting" | `Unit` | `Unit` |
| Zero return values | `Void` | `(values)` |

Because `Optional Integer` and `Integer` are different types, you cannot accidentally pass a "not found" result into `+`. You must obtain an `Integer` from the optional value before using it in arithmetic, deciding what to do when there is no value.

### Closed variants and pattern matching

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

`PaymentResult` is a single type with three *constructors* (`Approved`, `Declined`, and `RetryLater`), each carrying only the fields that make sense for that case. A function can use `match` to distinguish them and name their fields:

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
- Constructor patterns and their fields are checked against the type definition.
- If you later add a fourth constructor `(Cancelled)` to `PaymentResult`, recompiling `describe-payment` produces a warning that its match is non-exhaustive.
- All branches must return the same kind of object, in this case a `String`.

When you want a single record with named fields, like a `defstruct`, use `define-struct` instead:

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

### Connecting types across a function

Suppose you write a utility `find-where` that searches a custom tree data structure `tree` for an object satisfying a predicate:

```lisp
(find-where #'evenp coin-denominations)
(find-where #'player-health-low-p (get-players))
(find-where (lambda (x) (string= "COBOL" x)) *language-tree*)
(find-where #'point-at-origin-p (graph-quadtree g))
```

You want the same function to work on trees of integers, strings, and `point` structs. A general-purpose `ftype` declaration might be:

```lisp
(function ((function (t) boolean) tree) t)
```

Even if callers declare their types precisely, that signature does not say that the objects in the tree have the same type as the argument to the callback, or that a successful search returns an object of that same type.

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

The repeated `:a` connects the callback's argument, the tree's elements, and the optional result. This gives the compiler two useful checks at every call site:

- Passing a predicate of type `Point -> Boolean` together with a `Tree String` is rejected at compile time.
- Calling `find-where` on a `Tree Point` returns an `Optional Point`, so a value extracted from the result is known to be a `Point` without any extra type declarations.

### Protocols and type classes

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

Compared to CLOS generic functions, type classes have a few differences worth knowing:

1. **Completeness is checked at compile time**: Omitting `recoverable?` from `define-instance`, or calling `(format-report x)` on a type that does not implement `Reportable`, is a compile-time error rather than a run-time `no-applicable-method`.
2. **Methods can dispatch on return types**: CLOS selects methods using the run-time classes or identities (`eql` specializers) of arguments, not the expected return type. A zero-argument `(defgeneric default ())` has no arguments to distinguish methods, so a Lisp interface might instead take a type symbol, as in `(default 'point)` or `(cl:coerce x 'list)`. In Coalton, static types select the instance, including the *return type* expected by the caller. For example, the built-in `Default` type class has a zero-argument method `(default)`:

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

## Keeping generic code fast

Experienced Common Lisp programmers know the techniques for making an inner loop fast: give the compiler precise numeric types, avoid unnecessary dispatch in hot paths, and reduce allocation of wrapper structs and `cons` cells.

The difficulty is keeping those optimizations in reusable code. You may have a perfectly good generic algorithm, yet still maintain separate numerical versions to get the performance you need. Macro generators can help, but they leave you responsible for specializing the algorithm and its helpers.

Coalton can do that specialization for you because it knows the static types across function boundaries before generating Common Lisp. Its **development mode** preserves interactive redefinition, for example by compiling data types as CLOS classes. **Release mode** uses representations and calling conventions suited to optimization, including structs and unboxed entry points for eligible functions.

The examples below show what this buys you. Coalton performs its own specialization and allocation-elimination passes, but low-level results such as keeping floats unboxed in registers also depend on the host Lisp compiler.

### Specializing a generic function

Suppose you want to evaluate a polynomial $c_0 + x(c_1 + x(c_2 + \dots))$ using Horner's method:

```lisp
(defun horner (x coeffs)
  (if (null coeffs)
      0
      (+ (car coeffs) (* x (horner x (cdr coeffs))))))
```

How do you keep this function generic while getting unboxed `double-float` arithmetic when you need it? You might also want it to work on user-defined numeric types, such as matrices or polynomials.

An `inline` declaration alone cannot eliminate all recursive calls, and the definition leaves the element type of `coeffs` unspecified. Built-in `cl:+` and `cl:*` cannot be extended to your own types either. A macro that generates specialized versions is one approach.

In Coalton, you write `horner` as an ordinary generic function once:

```lisp
(coalton-toplevel
  (declare horner (Num :a => :a * List :a -> :a))
  (define (horner x coeffs)
    (match coeffs
      ((Nil) 0)
      ((Cons c rest) (+ c (* x (horner x rest)))))))
```

The operations `+` and `*` are methods of `Num`, and integer literals such as `0` are interpreted through its `fromInt` method. The same `horner` definition therefore works on any type with a `Num` instance, including `Integer`, `Fraction`, `F32`, `F64`, and suitable user-defined matrix or polynomial types.

When you want a fast `F64` version, you do not rewrite `horner` as a macro. Instead, you ask the compiler to [`monomorphize`](/manual/operators/monomorphize/) a concrete entry point:

```lisp
(coalton-toplevel
  (monomorphize)
  (declare horner-f64 (F64 * List F64 -> F64))
  (define (horner-f64 x coeffs)
    (horner x coeffs)))
```

Coalton specializes the recursive `horner` function for `F64`: `0` is interpreted as an `F64`, and `+` and `*` resolve to that type's arithmetic operations. The generated Common Lisp includes concrete type declarations for the numeric bindings, allowing the host compiler to optimize the arithmetic. You maintain one generic algorithm; the compiler produces the specialized version.

### Distinct wrapper types without heap allocation

Suppose you want separate types for `Meters` and `Seconds` so you cannot accidentally add a distance to a duration. In Common Lisp, `deftype` aliases for `double-float` do not distinguish them, while `defstruct` gives each value a wrapper object.

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

Passing `(Seconds 5.0d0)` as the first argument to `speed` is a compile-time error. At run time, however, `Meters` and `Seconds` use the underlying `double-float` representation: there is no additional wrapper to allocate or unpack.

### Structured values without extra allocation

A struct can make Lisp code easier to understand: its name and fields tell callers what they've received. But it can also allocate an object that exists only to pass information from one expression to another.

For example, suppose you want the squared distance from the origin to the midpoint of two points. A straightforward Common Lisp version computes a new `point` and passes it to a distance function:

```lisp
(defstruct point
  (x 0.0d0 :type double-float)
  (y 0.0d0 :type double-float))

(defun midpoint (p q)
  (declare (type point p q))
  (make-point :x (/ (+ (point-x p) (point-x q)) 2.0d0)
              :y (/ (+ (point-y p) (point-y q)) 2.0d0)))

(defun distance-squared (p)
  (declare (type point p))
  (+ (* (point-x p) (point-x p))
     (* (point-y p) (point-y p))))

(defun midpoint-distance-squared (p q)
  (declare (type point p q))
  (distance-squared (midpoint p q)))
```

The functions compose naturally because `midpoint` returns the same kind of object that `distance-squared` accepts. But each call constructs a temporary point just to read its coordinates. If profiling shows that these points account for too much allocation, you might write a version that returns coordinates as multiple values and adjust the caller:

```lisp
(declaim (inline midpoint-values))
(defun midpoint-values (p q)
  (declare (type point p q))
  (values (/ (+ (point-x p) (point-x q)) 2.0d0)
          (/ (+ (point-y p) (point-y q)) 2.0d0)))

(defun midpoint-distance-squared (p q)
  (declare (type point p q))
  (multiple-value-bind (x y) (midpoint-values p q)
    (declare (type double-float x y))
    (+ (* x x) (* y y))))
```

This avoids constructing the intermediate point, but the caller must know that `midpoint-values` returns the x coordinate followed by the y coordinate. It also computes the distance itself rather than reusing the function that accepts a point. In Coalton, you can keep the original composition, using the `Point` type and `distance-squared` function from earlier:

```lisp
(coalton-toplevel
  (inline)
  (declare midpoint (Point * Point -> Point))
  (define (midpoint p q)
    (Point (/ (+ (.x p) (.x q)) 2.0d0)
           (/ (+ (.y p) (.y q)) 2.0d0)))

  (declare midpoint-distance-squared (Point * Point -> F64))
  (define (midpoint-distance-squared p q)
    (let middle = (midpoint p q))
    (inline (distance-squared middle))))
```

Here, `midpoint` returns a `Point`, so its result works with other functions that accept points. In release mode, the inline hints expose the construction and field accesses of `middle` to Coalton's scalar-replacement pass. It replaces that intermediate point with two typed numeric bindings, so `midpoint-distance-squared` does not allocate a new `Point`. You keep the record-based interface and reuse `distance-squared` without manually unpacking and repacking coordinates.

The same pass handles eligible locally constructed `Tuple`s, `Optional`, `Result`, and mutable `Cell`s. The compiler must recognize their construction and use, and the values must not escape. Lisp compilers can eliminate some allocations too, but Coalton explicitly performs these transformations on its typed representations before generating Common Lisp.

### Generating type declarations automatically

Hand-written declarations need maintenance too. A missing declaration can leave the compiler without enough information to optimize a binding; a stale declaration can be dangerous under low-safety compilation.

In release mode, Coalton generates Common Lisp `(declare (type ...))` annotations on function parameters, `let` bindings, loop variables, and `match` variables from the types it has checked. When you change the Coalton code, those declarations change with it.

See [Performance and Optimization](/manual/topics/performance-and-optimization/) for examples of the generated Common Lisp code.

## How programming in Coalton feels different

Coalton shares the Lisp image, but not all of Common Lisp's programming conventions. A few adjustments to data modeling, generic operations, and interactive development will help you get comfortable with it.

### Starting with your data types

In Common Lisp, it is common to start prototyping with nested lists, alists, or plists like `'(:name "Alice" :age 42 :roles (:admin :staff))` where a single list mixes strings, integers, and symbols, and to put off defining `defstruct` or `defclass` types until the design settles.

In Coalton, lists and vectors are homogeneous: their elements must have the same type. An untyped plist therefore cannot serve as a general-purpose record. You usually start by sketching your data with structs and sum types. Those definitions are short, and they give the compiler something useful to check as you write the functions that work with them.

### Variants and protocols instead of inheritance hierarchies

In CLOS, a class hierarchy often serves two purposes at once: it lets you store different kinds of objects together (such as a list of `shape` instances containing both circles and rectangles), and it lets different types share generic operations and slots.

Coalton has no subtyping and no class inheritance. A 32-bit integer is not a subtype of `Integer`, and one struct cannot inherit fields from another. Instead, you separate those two jobs:

- When you have a fixed set of cases that need to live in the same container, you define a single sum type (`define-type`) with a constructor for each case and branch on it with `match`.
- When you have an open set of types that share behavior, you define a type class protocol (`define-class`). Unlike a CLOS method specialized on a superclass, a generic function constrained by a type class still operates on one concrete type at a time: a list of drawable items is a list of circles or a list of rectangles, not a mixed list of both.

### Explicit conversions instead of numeric contagion

Common Lisp's numeric tower automatically promotes operands across `integer`, `ratio`, and `float` in expressions like `(+ 1 2.5d0 3/4)`.

In Coalton, distinct numeric types do not implicitly convert into one another. The arithmetic operators are generic, but both inputs and the output of `+` or `*` must have the same type. Combining an `Integer` value and an `F64` value requires an explicit conversion. Integer literals are different: as in the Horner example, `1` can be interpreted directly as an `F64` through `fromInt`. Mixed-precision code takes a little more care to write, but you choose where conversions occur.

### Compiler-guided refactoring

When you recompile a Common Lisp function after changing its parameters or return type, a compiler such as SBCL may warn about incompatible callers. Common Lisp does not require implementations to detect all affected callers, though, so some problems may appear only when those callers run.

Coalton type-checks each `coalton-toplevel` form before emitting code. It rejects type mismatches and warns about non-exhaustive `match` expressions. When you interactively redefine a function with an incompatible type signature, it uses tracked dependencies to report the old and new types and the affected callers, including their source locations.

You can still work incrementally at the REPL. Common Lisp restarts let you choose `Continue anyway` or `Abort redefinition`, and `:auto-continue-redefinition` controls whether Coalton continues automatically with a warning. You can install the new definition and work through the callers one by one, with the diagnostics as your checklist.

## Familiar syntax, different rules

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

## Structuring hybrid Coalton/Lisp systems

Because Coalton and Common Lisp share the same image, you do not have to commit an entire project to one or the other:

- **Use Common Lisp for dynamic systems and ecosystem glue**: Macro-heavy DSLs, run-time metaprogramming, dynamic code loading, and interfaces to existing Quicklisp libraries (networking, databases, CFFI, GUIs, web servers) are often easiest to write in plain Common Lisp.
- **Use Coalton for typed data models and algorithms**: Domain models, compilers and AST passes, state machines, numerical algorithms, and subsystems that undergo frequent refactoring are good places to use static types, pattern-match checks, and specialization.
- **Keep the boundary between them narrow**: Coalton includes an inline `lisp` form for calling Common Lisp directly, where you state the Coalton type that the Lisp expression returns. Because Coalton trusts that annotation without type-checking the Lisp body inside it, it is good practice to keep raw `lisp` calls inside small wrapper functions that convert Lisp values and conditions into typed Coalton values at the boundary.
- **Keep interfaces immutable and internals pragmatic**: Coalton works best when functions accept and return immutable values at module boundaries, making components easy to test and compose. Inside a function body, however, you do not need to force everything into pure recursion: imperative loops, early returns, and local mutable accumulators are completely normal Coalton style.
- **Match error handling to the kind of failure**: Rather than signaling conditions for every failure or returning `nil`, Coalton code typically uses `Optional` when a lookup simply finds nothing, `Result` when an operation can fail in expected ways that the caller should handle explicitly, and exceptions or resumptions for rare failures across deep call stacks or interoperation with Common Lisp conditions.

You need not move a whole system into Coalton. A useful starting point is a component whose data shapes and interfaces are becoming difficult to keep consistent. Define those types in Coalton, keep the surrounding Lisp code, and let the compiler help with the next change.

## Where to go next

- **[Introduction and Getting Started](/manual/topics/introduction/)**: Setting up a Common Lisp environment, Quicklisp, Emacs/SLIME, and your first `.asd` + `.ct` project.
- **[Coalton Language Guide](/manual/topics/language-guide/)**: The main tour of Coalton's syntax, types, collections, loops, and type classes.
- **[Advanced Coalton Programming](/manual/topics/advanced-coalton-programming/)**: Functional dependencies, custom collection builders, overlapping instances, `forall` and `Values` polymorphism, the value restriction, resumptions, and function specialization.
- **[Lisp Interop](/manual/topics/lisp-interop/)**: Calling Coalton from Common Lisp, calling Common Lisp from Coalton, and controlling data representations with `(repr ...)`.
- **[Performance and Optimization](/manual/topics/performance-and-optimization/)**: Development vs. release mode, `Cell` and ADT unboxing, stack allocation, `inline`, and `monomorphize`.
- **[Glossary of Terms](https://github.com/coalton-lang/coalton/blob/main/docs/glossary.md)**: A side-by-side lookup table of Common Lisp and Coalton terminology.
