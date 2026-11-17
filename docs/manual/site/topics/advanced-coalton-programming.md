---
title: "Advanced Coalton Programming"
description: "Guide to advanced Coalton programming."
hideMeta: true
weight: 25
---

Coalton includes several features for designing expressive abstractions, working with rich static types, and controlling dispatch and performance. Before reading this guide, make sure you are comfortable with the material in the [Coalton Language Guide](/manual/topics/language-guide/), especially algebraic data types (`define-type`), polymorphic functions (`declare`), and type classes (`define-class` and `define-instance`).

{{<toc>}}

## Functional Dependencies

Multi-parameter type classes relate two or more types. However, without extra guidance, the type checker treats all parameters of a class as independent, which often leads to ambiguous types.

Suppose we want a type class `Collects` relating a collection type `:c` to its element type `:e`:

```lisp
(coalton-toplevel
  (define-class (Collects :c :e)
    (empty-collection (Void -> :c))
    (insert-element (:c * :e -> :c))))
```

If `:c` and `:e` are completely independent, a type like `(List Integer)` could theoretically implement both `(Collects (List Integer) Integer)` and `(Collects (List Integer) String)`. Because the compiler cannot assume that `:c` determines `:e`, calling methods or chaining operations on `:c` can leave `:e` unresolved unless the caller writes explicit type annotations everywhere.

*Functional dependencies* solve this by declaring that some class parameters uniquely determine others.

### Declaring Functional Dependencies

A class `C` can be given a functional dependency `(:a -> :b)` in its header:

```lisp
(define-class (C :a :b (:a -> :b)))
```

The clause `(:a -> :b)` reads: "for each type `:a`, there is at most one type `:b`," or equivalently, "the type `:b` is uniquely determined by `:a`."

With a functional dependency on `Collects`:

```lisp
(coalton-toplevel
  (define-class (Collects :c :e (:c -> :e))
    (empty-collection (Void -> :c))
    (insert-element (:c * :e -> :c))))
```

once the compiler knows the collection type `:c`, it can automatically infer the element type `:e`.

A class header can also include multiple functional dependency clauses, and each clause can list multiple type variables on either side of `->`:

```lisp
(coalton-toplevel
  ;; Multiple determining variables: :a and :b together determine :c
  (define-class (Mult :a :b :c (:a :b -> :c))
    (multiply (:a * :b -> :c)))

  ;; Multiple determined variables: :collection determines both :element and :builder
  (define-class (FromItemizedCollection :collection :element :builder
                                        (:collection -> :element :builder))
    ...)

  ;; Multiple dependency clauses (including mutually determining variables)
  (define-class (Iso :a :b (:a -> :b) (:b -> :a))
    (to (:a -> :b))
    (from (:b -> :a))))
```

### Instance Consistency Rules

Functional dependencies are enforced whenever instances are defined:

1. **Consistency across instances**: If `(C String Integer)` is defined, it is invalid to also define `(C String Char)`, because the same `:a` (`String`) would map to two different types for `:b` (`Integer` and `Char`).
2. **Consistency with an instance's own context**: An instance may not contradict its own prerequisites. For example, the instance

   ```lisp
   (define-instance (C :a :b => C :a (List :b)))
   ```

   is invalid: whenever it provides `C :a (List :b)`, its context also requires `C :a :b`, so the same `:a` would determine both `:b` and `(List :b)`.

## Extending Builder Syntax to User-Defined Types

As shown in the Language Guide, square-bracket syntax (`[1 2 3]`, `["a" => 1]`, and comprehensions) defaults to `Seq`, but can build any type that implements Coalton's builder type classes. These classes use functional dependencies to connect the target collection type, the element or entry types, and an intermediate builder state:

- `FromItemizedCollection` for `[a b c]`
- `FromItemizedAssociation` for `[k => v ...]`
- `FromCollectionComprehension` for `[expr :for ...]`
- `FromAssociationComprehension` for `[key => value :for ...]`

The itemized classes are used when elements or key-value entries are listed explicitly in source code. They receive the exact number of items up front and the zero-based index of each item as it is added. The comprehension classes are stream-oriented: they receive an advisory initial size hint, accumulate each emitted item or entry, and finalize the builder state into the target collection.

Here is a complete `FromItemizedCollection` instance for a custom wrapper around `List`. Its intermediate builder state is a temporary `List :a` that is reversed once at the end:

```lisp
(coalton-toplevel
  (define-type (Bag :a)
    (Bag (List :a)))

  (define-instance (FromItemizedCollection (Bag :a) :a (List :a))
    (define (begin-collection-builder _ _size)
      Nil)
    (define (adjoin-to-collection-builder _ builder _index item)
      (Cons item builder))
    (define (finalize-collection-builder _ builder)
      (Bag (reverse builder))))

  (define my-bag
    (the (Bag Integer) [1 2 3])))
```

In `(FromItemizedCollection (Bag :a) :a (List :a))`:

1. `(Bag :a)` is the final collection type.
2. `:a` is the element type (uniquely determined by `(Bag :a)`).
3. `(List :a)` is the intermediate accumulator type (also determined by `(Bag :a)`).

The first argument passed to each method is a `types:Proxy` for the target collection type, which disambiguates the instance even before the final collection value has been constructed.

To support `[x :for ...]` comprehensions for `Bag :a`, define a corresponding `FromCollectionComprehension` instance. Association builders work the same way through `FromItemizedAssociation` and `FromAssociationComprehension`.

## Overlapping Instances

By default, Coalton forbids two instances of the same type class from overlapping—that is, having instance heads that could match the same concrete types. This guarantees that instance resolution is globally unique regardless of where functions are compiled.

In some designs, however, it is useful to provide a general fallback instance while still allowing more specific instances to override it. Coalton supports this when every participating instance is explicitly marked with the [`overlap`](/manual/operators/overlap/) attribute:

```lisp
(coalton-toplevel
  (define-class (Label :a)
    (label (:a -> String)))

  (overlap)
  (define-instance (Label :a)
    (define (label _) "value"))

  (overlap)
  (define-instance (Label Integer)
    (define (label _) "integer")))

(coalton (label True))              ; ==> "value"
(coalton (label (the Integer 42)))  ; ==> "integer"
```

### Specificity and Polymorphic Callers

When resolving an overlapping constraint, Coalton compares only the instance heads:

- An instance head is **more specific** than another if it can be obtained by substituting types into the more general head, and not vice versa.
- Definition order does not matter; the unique most-specific matching head wins.
- If two matching heads are incomparable (neither is more specific than the other), a use that matches both is rejected as ambiguous unless a third `(overlap)` instance is defined for their intersection.

Because a more specific `(overlap)` instance can be defined later (even in another compilation unit), a polymorphic function that calls a method of an overlapping class cannot prematurely commit to the general instance. Instead, it retains the class constraint in its signature so the caller supplies the dictionary for the concrete type:

```lisp
(coalton-toplevel
  (declare describe-label (Label :a => :a -> String))
  (define (describe-label x)
    (label x)))

(coalton (describe-label True))              ; ==> "value"
(coalton (describe-label (the Integer 42)))  ; ==> "integer"
```

The standard library uses `(overlap)` for identity conversions (`Into :a :a`) alongside generic container conversions. See [`overlap`](/manual/operators/overlap/) for complete rules and examples.

## Instance Defaulting

Consider what happens when you evaluate a numeric expression at the REPL without any type annotations:

```
COALTON-USER> (coalton (+ 1 2))
3
COALTON-USER> (coalton (/ 1 2))
0.5d0
```

In Coalton, integer literals like `1` and `2` are polymorphic: they desugar via `from-int` to any type `:a` that implements `Ring :a` (or its subclass `Num :a`). Consequently:

- `(+ 1 2)` has the inferred type `Ring :a => :a`.
- `(/ 1 2)` has the inferred type `Reciprocable :a => :a` (since `Ring :a` is a superclass of `Reciprocable :a`).

At the top level of a `(coalton ...)` form—or in an implicitly typed `define` or `let` binding—leaving `:a` unresolved would make it impossible to choose a concrete runtime dictionary. Rather than rejecting every unannotated numeric literal as ambiguous, Coalton applies *type defaulting*, similar to [Haskell's type defaulting system](https://www.haskell.org/onlinereport/decls.html#sect4.3.4).

### How Defaulting Resolves Ambiguity

When type defaulting is invoked on a set of ambiguous predicates involving a numeric type variable, Coalton tries candidate types in order:

1. `Integer`
2. `F64`
3. `F32`

It selects the first candidate that satisfies all predicates on the ambiguous variable:

- For `(+ 1 2)`, the constraint is `Ring :a`. The first candidate, `Integer`, implements `Ring`, so `:a` defaults to `Integer` and the result is `3`.
- For `(/ 1 2)`, the constraint is `Reciprocable :a`. `Integer` does *not* implement `Reciprocable`, so Coalton moves to the next candidate, `F64`, which satisfies `Reciprocable` and yields `0.5d0`.

Note that functions with explicit polymorphic `declare` signatures do *not* default their declared type variables; defaulting only resolves otherwise-ambiguous variables in implicitly typed bindings and `(coalton ...)` entry points.

### Differences from Haskell 98

Coalton's defaulting is more permissive than Haskell 98 when an ambiguous numeric type variable appears inside a structured type or alongside non-standard classes, provided the ambiguous predicates do not reference any other unresolved type variables:

- **Structured types**: Haskell 98 considers a constraint like `Num (List :a)` ambiguous and refuses to default `:a`. Coalton still tests candidate types for `:a` (so if `Num (List Integer)` were a valid instance, `:a` would default to `Integer`).
- **Custom type classes**: Haskell 98 only defaults a variable if *all* of its constraints come from a fixed set of standard classes. In Coalton, if `:a` is constrained by `((Num :a) (CustomTypeClass :a))`, Coalton will still default `:a` to `Integer` (or `F64`, or `F32`) if `CustomTypeClass` has a matching instance for that candidate.

## Scoped Type Variables, Higher Kinds, and Result Polymorphism

In ordinary declarations, type variables such as `:a` and `:b` are implicitly quantified at the top of the signature:

```lisp
(coalton-toplevel
  (declare keep-first (:left * :right -> :left))
  (define (keep-first x _y)
    x))
```

When you want explicit control over quantifier order, lexical scoping of type variables inside a definition body, or kind annotations on type parameters, use [`forall`](/manual/operators/forall/).

### Explicit `forall` and Scoped Type Variables

An explicit `forall` lists its type variables before the qualified type:

```lisp
(coalton-toplevel
  (declare keep-first (forall (:left :right) :left * :right -> :left))
  (define (keep-first x _y)
    x))
```

The body of `forall` can be written either in the unparenthesized form above or as a single parenthesized type:

```lisp
(declare keep-first (forall (:left :right) (:left * :right -> :left)))
```

Unlike implicit quantification, an explicit `forall` brings its type variables into lexical scope inside the corresponding definition. Nested `declare`, `the`, and `lisp` annotations can refer to those variables directly:

```lisp
(coalton-toplevel
  (declare scoped-id (forall (:item) :item -> :item))
  (define (scoped-id x)
    (let ((declare keep-item (Void -> :item))
          (keep-item (fn ()
                       (the :item x))))
      (keep-item))))
```

This is especially useful when a helper function inside `let` needs an explicit type signature that mentions a type parameter from the enclosing function. If a local `declare` has its own `forall`, both the outer and inner binders are in scope (with inner binders shadowing outer ones of the same name):

```lisp
(coalton-toplevel
  (declare scoped-local (forall (:item) :item -> :item))
  (define (scoped-local x)
    (let ((declare keep-outer
                  (forall (:tmp) :item * :tmp -> :item))
          (keep-outer (fn (y _z)
                        (the :item y))))
      (keep-outer x Unit))))
```

Class method declarations in `define-class` can also use explicit `forall`. When they do, those binders are in scope inside the corresponding `define-instance` method bodies.

Similarly, an inline `lisp` form can return a value typed with a scoped type variable:

```lisp
(coalton-toplevel
  (declare identity-through-lisp (forall (:item) :item -> :item))
  (define (identity-through-lisp x)
    (lisp (-> :item) (x)
      x)))
```

Note that embedded `(coalton ...)` forms inside a raw Lisp body are a separate compilation context and do not inherit lexical type-variable bindings.

### Kind Annotations and Result Polymorphism (`Values`)

Coalton infers kinds for bare type variables automatically, including higher-kinded variables such as `:f` in `(:f :a)`. When you want to make a binder's kind explicit, write `(:var Kind)` inside `forall`:

- `(:item Type)` requires an ordinary single-value type.
- `(:f (Type -> Type))` requires a unary type constructor (such as `List` or `Optional`).

Coalton also supports *result polymorphism* using the special kind `Values`. While an ordinary `Type` variable always stands for a single runtime value, a `Values`-kinded variable stands for an entire sequence of function return values—including zero values (`Void`) or multiple values (`Integer * String`):

```lisp
(coalton-toplevel
  (declare call-results
    (forall ((:r Values)) (Void -> :r) -> :r))
  (define (call-results f)
    (f)))

(coalton (call-results (fn () (values 42 "answer"))))
;; Returns two values: 42 and "answer".
```

The same `call-results` function can forward zero, one, or multiple results. In written type annotations, an unannotated variable such as `:r` always has kind `Type` (or a higher constructor kind) and cannot stand for zero or multiple return values; you must annotate it with `Values`. Functions without explicit declarations can still infer result polymorphism automatically. See [`forall`](/manual/operators/forall/) for complete kind syntax and restrictions.

## Higher-Rank Polymorphism

In ordinary (rank-1) polymorphic functions, `forall` quantifiers live at the outermost level of a signature, which means the *caller* chooses the concrete types for all type variables at the call site. For example, in `((:a -> :a) * :a -> :a)`, the caller picks a single type `:a`, and the passed function only needs to work on that one type.

Sometimes, however, a function needs to receive a *polymorphic* function as an argument and instantiate it at multiple different types *inside* its own body. Coalton supports this—known as *higher-rank* or *rank-$N$ polymorphism*—by allowing explicit `(forall ...)` types inside function arguments, return types, `define-type` constructors, `define-struct` fields, and `define-type-alias` definitions.

### Rank-2 and Higher-Rank Function Arguments

Suppose we want a function `apply-both` that applies a polymorphic function `f` to two arguments of different types `:x` and `:y`. Writing `(forall (:a) (:a -> :a))` as the type of `f` requires `f` itself to be polymorphic:

```lisp
(coalton-toplevel
  ;; Rank-2: `f` must work for all types `:a`, so `apply-both` can
  ;; apply it to both `:x` and `:y`.
  (declare apply-both ((forall (:a) (:a -> :a)) * :x * :y -> Tuple :x :y))
  (define (apply-both f x y)
    (Tuple (f x) (f y)))

  ;; Rank-2 over type constructors: `nat` transforms any `(:f :a)` into `(:g :a)`.
  (declare map-pair
    (forall (:f :g)
      ((forall (:a) (:f :a -> :g :a))
       * (:f Integer)
       * (:f String)
       -> Tuple (:g Integer) (:g String))))
  (define (map-pair nat xs ys)
    (Tuple (nat xs) (nat ys))))
```

While the higher-rank function (`apply-both` or `map-pair`) must have an explicit type declaration, callers can pass either a named polymorphic function (such as `id` or `head`) or an unannotated inline `fn` lambda directly:

```lisp
(coalton (apply-both id 42 "hello"))
;; ==> #.(TUPLE 42 "hello")

(coalton (apply-both (fn (x) x) 42 "hello"))
;; ==> #.(TUPLE 42 "hello")

(coalton (map-pair head (the (List Integer) [1 2]) (the (List String) ["a" "b"])))
;; Returns (Tuple (Some 1) (Some "a"))
```

Nested `forall` types can also include type class constraints:

```lisp
(coalton-toplevel
  (declare apply-num-both ((forall (:a) (Num :a => (:a -> :a))) * Integer * F64 -> Tuple Integer F64))
  (define (apply-num-both f n x)
    (Tuple (f n) (f x))))

(coalton (apply-num-both (fn (v) (+ v 1)) 10 2.5d0))
;; ==> #.(TUPLE 11 3.5d0)
```

### Predicative Polymorphism and Nominal Wrappers

Coalton's higher-rank type system is **predicative**: ordinary type variables and type constructors (`List`, `Vector`, `Optional`, `Tuple`) can only be instantiated with *monotypes* (types that do not contain `forall`), never directly with `forall` types.

This keeps type inference predictable and has two main practical consequences:

1. **Higher-rank functions require an explicit declaration**: Unannotated function parameters are always inferred as monotypes. If a function uses a parameter polymorphically at multiple types, you must write a `declare` (or `the`) signature with a nested `forall`.
2. **Wrap `forall` types in a `define-type` or `define-struct` when storing them in containers**: A parameterized container like `List` cannot be applied directly to a `(forall ...)` type. Instead, wrap the polymorphic function in a nominal data type or struct field:

   ```lisp
   (coalton-toplevel
     (define-type PolyNumFn
       (PolyNumFn (forall (:a) (Num :a => (:a -> :a)))))

     (declare num-fns (List PolyNumFn))
     (define num-fns
       [(PolyNumFn (fn (x) (+ x 1)))
        (PolyNumFn (fn (x) (* x x)))]))
   ```

See [`forall`](/manual/operators/forall/#higher-rank-polymorphism) for complete rules on higher-rank types and predicativity.

## Polymorphism, Mutation, and the Value Restriction

In a pure language, any binding whose inferred type contains uninstantiated type variables can be generalized into a polymorphic value. In the presence of mutable state, unrestricted polymorphic bindings would be unsound: a single mutable container could be written at one type and read back at another.

Coalton prevents this by combining an ML-style syntactic *value restriction* with a *relaxed variance check*.

### Non-Expansive vs. Expansive Bindings

When generalizing an implicitly typed `define` or `let` binding, Coalton classifies the right-hand side:

- A **non-expansive** expression is syntactically value-like: variables, literals, `fn` abstractions, and data constructor applications over non-expansive arguments.
- An **expansive** expression is everything else. In particular, any ordinary function application is treated as expansive, even if the called function happens to be pure.
- Expansive bindings introduce **weak** type-variable candidates. Weak variables are generalized into polymorphic type parameters only when every occurrence in the inferred type is **covariant** and the variable does not appear in any retained type class constraints.

At top level, any unresolved weak type variable that cannot be generalized is rejected with a compile-time error.

For example, a non-expansive constructor expression generalizes freely:

```lisp
(coalton-toplevel
  (define wrapped-none (Some None)))
;; wrapped-none : Optional (Optional :a)
```

An expansive expression also generalizes when its undetermined type variable appears only in covariant (output-only) positions:

```lisp
(coalton-toplevel
  (define wrapped-id
    ((fn (x) x) None)))
;; wrapped-id : Optional :a
```

Even though `((fn (x) x) None)` is a function call (and therefore expansive), `:a` appears only inside `Optional :a`, which is covariant.

By contrast, mutable containers such as `Vector` or `Cell` are invariant in their element type. An expansive binding that allocates a mutable container without fixing its element type will not generalize:

```lisp
(coalton-toplevel
  (define wrapped-new
    (Some (coalton/vector:new))))
;; error: unresolved weak type variable
```

When you encounter this error, there are two standard ways to resolve it:

1. **Turn the binding into a function** (eta-expansion) so that each call allocates fresh mutable state.
2. **Add an explicit concrete type declaration** when you intend to create a single shared mutable value of a specific type:

```lisp
(coalton-toplevel
  (declare int-vec (Vector Integer))
  (define int-vec (coalton/vector:new)))
```

### Variance

Coalton computes the variance of each type parameter of a `define-type` or `define-struct` from its field positions:

- **Covariant**: the type parameter is only produced (for example, `Optional :a` and `List :a`).
- **Contravariant**: the type parameter is only consumed (for example, the input position `:a` in `:a -> :b`).
- **Invariant**: the type parameter is both consumed and produced, or appears inside another invariant type constructor.

Under the relaxed value restriction, weak type variables are generalized only when all of their occurrences are covariant. Any invariant or contravariant occurrence keeps the variable monomorphic.

### Opaque Native Types

For `define-type` definitions that wrap a host Common Lisp type using `(repr :native ...)` and have no Coalton constructors, Coalton cannot inspect the internal structure of the type. By design, all type parameters of an opaque native type are treated as **invariant**.

In the standard library, the following parametric native types are all invariant:

- `Vector :a`
- `Cell :a`
- `Queue :a`
- `Slice :a`
- `Hashtable :key :value`
- `LispArray :t`
- `FileStream :a`

Each of these types either supports in-place mutation directly or combines read and write capabilities.

For further background on the value restriction and variance:

- OCaml manual, "Polymorphism and its limitations".
- Jacques Garrigue, "Relaxing the Value Restriction".
- Andrew K. Wright, "Simple Imperative Polymorphism".

## Resumptions and Non-Unwinding Handlers

The [Coalton Language Guide](/manual/topics/language-guide/#error-handling-and-exceptions) covers ordinary exception handling with `define-exception`, `throw`, `catch`, and `protect`. Like exceptions in most languages, `catch` unwinds the call stack to the handler before running the recovery branch.

Inspired by Common Lisp's condition and restart system, Coalton also supports *resumptions*: a handler higher on the call stack can inspect a signaled exception *before* the stack unwinds and transfer control to a recovery point (`resumable`) closer to where the exception was raised.

The four forms involved are:

- [`define-resumption`](/manual/operators/define-resumption/): Defines a named resumption constructor and type.
- [`resumable`](/manual/operators/resumable/): Establishes one or more resumption branches around an expression.
- [`handle`](/manual/operators/handle/): Intercepts thrown exceptions *without* unwinding the stack, running its matching branch at the dynamic point of the `throw`.
- [`resume-to`](/manual/operators/resume-to/): Transfers control from inside a `handle` branch to an active `resumable` block matching the given resumption value.

### Example: Recovering Without Unwinding Outer Loops

Building on the `Egg` and `BadEgg` types from the Language Guide:

```lisp
(coalton-toplevel
  (define-type Egg
    ;;     cracked? cooked?
    (Goose Boolean Boolean)
    (Xenomorph))

  (define-exception BadEgg
    (UnCracked Egg)
    (DeadlyEgg Egg))

  (declare crack (Egg -> Egg))
  (define (crack egg)
    (match egg
      ((Goose _ cooked?)
       (Goose True cooked?))
      ((Xenomorph)
       (throw (DeadlyEgg egg)))))

  (define-resumption SkipEgg)
  (define-resumption (ServeRaw Egg)
    "Suggest the egg be served raw.")

  (declare cook (Egg -> Egg))
  (define (cook egg)
    (let ((badegg (UnCracked egg))) ; exceptions can be constructed outside throw
      (match egg
        ((Goose (True) _)  (Goose True True))
        ((Goose (False) _) (throw badegg))
        ((Xenomorph)       (throw (DeadlyEgg egg))))))

  ;; Return None if a SkipEgg resumption is received.
  (declare make-breakfast-with (Egg -> (Optional Egg)))
  (define (make-breakfast-with egg)
    (resumable (Some (cook (crack egg)))
      ((SkipEgg) None))))
```

Now we can write a higher-level function `make-breakfast-for` that cooks `n` eggs. Whenever a `DeadlyEgg` exception is thrown deep inside `crack` or `cook`, `handle` intercepts it without unwinding `make-breakfast-with` and invokes `(resume-to skip)`, causing `make-breakfast-with` to return `None`:

```lisp
(coalton-toplevel
  (declare make-breakfast-for (UFix -> (Vector Egg)))
  (define (make-breakfast-for n)
    (let ((eggs (vector:new))
          (skip SkipEgg))
      (for ((i 0 (1+ i)))
        :repeat n
        (let egg = (if (== 0 (mod i 5)) Xenomorph (Goose False False)))
        (do
         (cooked <- (handle (make-breakfast-with egg)
                      ((DeadlyEgg _) (resume-to skip))))
         (pure (vector:push! cooked eggs))))
      eggs)))
```

Every 5th egg is a `Xenomorph`, so making breakfast for 10 people produces 8 cooked eggs.

When `crack` throws `(DeadlyEgg egg)`, the active call stack is:

```
make-breakfast-for
      │
      └─ make-breakfast-with   (establishes `resumable` for `SkipEgg`)
                │
                └─ crack       (throws `DeadlyEgg`)
```

Because `make-breakfast-for` uses `handle` rather than `catch`, `make-breakfast-with` is still on the stack when the handler runs, so `(resume-to skip)` can jump to `make-breakfast-with`'s `((SkipEgg) None)` branch. Had `make-breakfast-for` used `catch`, the stack would already have unwound past `make-breakfast-with` before the handler ran.

### Caveats for Exceptions and Resumptions

Coalton's exception and resumption system is still evolving. Keep the following caveats in mind:

1. A wildcard `_` branch in `catch` or `handle` catches any Common Lisp `error` condition, but cannot bind it to a variable. To bind a Lisp condition, match `(the T var)` with a native exception type such as `exception:LispError` or a type declared with `(repr :native ...)`. Because Coalton does not have subtyping, a condition caught as `exception:ArithmeticError` must be narrowed with `exception:cast` before being used as a `DivisionByZero`.
2. `resumable` branches must match a concrete resumption constructor pattern; wildcard and type-assertion patterns are not supported there.
3. Although the `Exception` and `Resumption` classes classify throwable and resumable types, function signatures do not currently track which exceptions or resumptions a function may signal.

## Function Specialization

Type classes make it easy to write generic functions, but polymorphic calls normally pass type class method dictionaries at runtime. When a function is called with known concrete types on a performance-critical path, you can declare type-directed rewrite rules with [`specialize`](/manual/operators/specialize/) to replace generic calls with specialized implementations at compile time.

### Declaring Specializations

A `specialize` form names the generic function, the specialized replacement function, and the more specific unconstrained type at which the rewrite applies:

```lisp
(coalton-toplevel
  (declare inc (Num :a => :a -> :a))
  (define (inc x)
    (trace "standard call")
    (+ x 1))

  (declare inc-int (Integer -> Integer))
  (define (inc-int x)
    (trace "int specialized call")
    (+ x 1))

  (specialize inc inc-int (Integer -> Integer)))
```

When `inc` is called at a call site whose argument is statically known to be an `Integer`, the compiler rewrites the call to `(inc-int ...)` and drops the `Num` dictionary argument:

```
COALTON-USER> (coalton (inc 1.2))
standard call
2.2
COALTON-USER> (coalton (inc 1))
int specialized call
2
```

> [!IMPORTANT]
> The example above uses `trace` only to make the compile-time rewrite visible in the REPL. In real programs, a specialized function **must have the exact same observable behavior** as the generic function. Specialization is an opportunistic optimization and is not guaranteed to fire at every call site.

### When Specialization Does and Does Not Apply

Specialization only fires when the argument and return types at a call site are statically known to match the specialization signature. Consider a polymorphic wrapper that calls `inc`:

```lisp
(coalton-toplevel
  (declare inc2 (Num :a => :a -> :a))
  (define (inc2 x)
    (inc x)))
```

When `inc2` is compiled on its own, the type of `x` inside `inc2` is still the abstract type variable `:a`, so the inner call `(inc x)` is not specialized:

```
COALTON-USER> (coalton (inc2 1))
standard call
2
```

If you want inner calls inside a helper like `inc2` to specialize at concrete call sites, you can mark the helper with [`inline`](/manual/operators/inline/) or mark a concrete entry point with [`monomorphize`](/manual/operators/monomorphize/) so that concrete types are propagated into the body before specialization rules run. Also note that specialization is skipped when rewriting a call would discard constraints on a class that uses [`overlap`](/manual/operators/overlap/).

Active specializations can be inspected in the REPL with [`print-specializations`](/manual/operators/print-specializations/). For a broader guide to inlining, monomorphization, unboxing, and stack allocation, see [Performance and Optimization](/manual/topics/performance-and-optimization/).

