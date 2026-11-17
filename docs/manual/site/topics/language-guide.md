---
title: "Coalton language guide"
description: "A broad, practical tour of Coalton's core language and workflow."
hideMeta: true
weight: 20
---

Coalton is a statically typed language that is embedded in, and compiles to, Common Lisp.

This document is aimed toward people who are already familiar with functional programming languages. If you are setting up Coalton or creating your first project, start with [Introduction and Getting Started](/manual/topics/introduction/). If you are coming from Common Lisp, start with [Coalton for Common Lispers](/manual/topics/coalton-for-common-lispers/) and the [glossary](https://github.com/coalton-lang/coalton/blob/main/docs/glossary.md). When you've finished reading the language guide, check out [Advanced Coalton Programming](/manual/topics/advanced-coalton-programming/) for deeper coverage of the type system, custom abstractions, resumptions, and specialization.

{{<toc>}}

## Program structure and the REPL

If you are experimenting with Coalton in a Common Lisp REPL, switch to the `#:coalton-user` scratch package:

```lisp
(in-package #:coalton-user)
```

> [!NOTE]
> `#:coalton-user` is for interactive use only, just like `#:cl-user`. See [Packages and ASDF Systems](#packages-and-asdf-systems) for how to set up packages and `.ct` files in a project.

There are two primary entry points into Coalton from Lisp:

1. `coalton-toplevel` compiles top-level definitions, type declarations, data types, and type class instances:

   ```lisp
   (coalton-toplevel
     ;; <Coalton definition forms>
     )
   ```

   In SLIME/SLY, `C-c C-c` compiles an entire `coalton-toplevel` form at once, so it is common to group related definitions into separate `coalton-toplevel` blocks.

2. `coalton` evaluates one or more Coalton expressions (in an implicit `progn`) and returns the resulting value to Lisp:

   ```lisp
   (coalton (+ 1 2))
   ```

   You cannot define new top-level bindings inside a `coalton` form—only evaluate expressions.

> [!IMPORTANT]
> Coalton packages, including `#:coalton-user`, do *not* `:use` the `#:common-lisp` (`#:cl`) package, so Common Lisp symbols must be prefixed with `cl:` when referenced directly.

## Variables, functions, and static types

Variables and functions are defined with `define`:

```lisp
(coalton-toplevel
  ;; Variables are defined with the define keyword
  (define x 5)
  (define y 6)
  (define z (+ x y))
  (define p (Tuple 1.0 2.0))

  ;; Coalton supports integers, strings, booleans, and unit as primitive types
  (define name "Alyssa P. Hacker")
  (define hungry True)
  (define data Unit))
```

You can inspect or use top-level definitions from the REPL with `coalton`:

```lisp
(coalton (fst p))
```

> [!IMPORTANT]
> It may be tempting to omit `coalton` and evaluate `(fst p)` directly in the Lisp REPL, but you should always wrap Coalton expressions in `(coalton ...)`.

Unlike Common Lisp, Coalton is a Lisp-1: functions and variables share a single namespace, making higher-order functional programming natural:

```lisp
(coalton-toplevel
  ;; Functions are also defined with the define keyword
  (define (add2 x)
    (+ 2 x))

  ;; Functions exist in the same namespace as variables
  (define addTwo add2)

  (define x (addTwo 3))

  ;; Anonymous functions can be defined with fn
  (define z (map (fn (x) (+ 2 x)) [1 2 3 4]))

  ;; ƒ is short fn syntax in the Coalton readtable
  (define z-short (map ƒx.(+ 2 x) [1 2 3 4])))
```

### Type inference and declarations (`declare` and `forall`)

Coalton code is statically type-checked with full Hindley–Milner type inference, so explicit type signatures are optional:

```lisp
(coalton-toplevel
  (define (fun x)
    (map (fn (y) (+ 2 y)) (str:parse-int x))))
```

You can check the inferred type of any variable or expression with `type-of`:

```
COALTON-USER> (coalton (type-of fun))
STRING → OPTIONAL INTEGER
```

Even though types are inferred, adding explicit type declarations with `declare` is good practice for documenting intent and catching type errors early:

```lisp
(coalton-toplevel
  (declare add2 (Integer -> Integer))
  (define (add2 x)
    (+ 2 x))

  (declare three Integer)
  (define three (add2 1)))
```

Polymorphic type signatures use keyword symbols such as `:a` and `:b` for type variables. Type variables are implicitly universally quantified, or can be listed explicitly with `forall`:

```lisp
(coalton-toplevel
  (declare keep-first (:left * :right -> :left))
  (define (keep-first x _y)
    x)

  ;; Equivalent declaration with an explicit `forall`
  (declare keep-second (forall (:left :right) :left * :right -> :right))
  (define (keep-second _x y)
    y))
```

Writing an explicit `forall` also brings those type variable names into lexical scope inside the definition body so nested `declare`, `the`, and `lisp` forms can refer to them. See [Scoped Type Variables, Higher Kinds, and Result Polymorphism](/manual/topics/advanced-coalton-programming/#scoped-type-variables-higher-kinds-and-result-polymorphism) and [Higher-Rank Polymorphism](/manual/topics/advanced-coalton-programming/#higher-rank-polymorphism) in Advanced Coalton Programming for details.

### Inline type annotations (`the`)

Inline type annotations can be added anywhere in an expression using `the` to constrain a value's type or resolve ambiguities:

```lisp
(coalton-toplevel
  (define f (the U32 (+ 5 7))))
```

### Fixed-arity functions, nullary functions, and manual currying

Coalton functions have fixed input and output arities. A type like `Integer * String -> Boolean`
describes a function that takes two arguments and returns one value. A type like
`Integer -> String -> Boolean` describes a function that takes one argument and returns another
function.

Consider this function:

```lisp
(coalton-toplevel
  (define (fma a b c)
    (+ c (* a b))))
```

This function is called as you'd expect:

```lisp
(coalton-toplevel
  (define fma3 (fma 2 3 4))) ; evaluates to 10
```

Unlike some other ML-derived languages, Coalton does not "auto-curry" or "partially apply" functions. A function must take exactly the number of inputs it was defined to take. If you want currying or partial application, write it explicitly by returning a function using `fn`:

```lisp
(coalton-toplevel
  (define (make-fma a)
    (fn (b)
      (fn (c)
        (+ c (* a b)))))

  (define fma1 (make-fma 2))
  (define fma2 ((make-fma 2) 3))
  (define fma3 (((make-fma 2) 3) 4)))
```

Here is an example of using an explicitly constructed function to transform a sequence:

```lisp
(coalton-toplevel
  (define nums [2 3 4 5])

  (define add2
    (fn (x)
      (+ 2 x))))

(coalton
  (map add2 nums)) ;; #<SEQ [4 5 6 7]>
```

Coalton also has true nullary (zero-argument) functions. A type like `(Void -> Integer)` denotes a function that takes zero arguments and returns one `Integer`:

```lisp
(coalton-toplevel
  (declare five (Void -> Integer))
  (define five
    (fn () 5)))
```

`Unit` remains an ordinary type and value. A function of type `Unit -> Integer` still takes one argument, and must be called as `(f Unit)`.

Functions may also return multiple values directly using `values`. This is distinct from returning a `Tuple`, which is still an ordinary single value:

```lisp
(coalton-toplevel
  (declare sum-and-product (Integer * Integer -> Integer * Integer))
  (define (sum-and-product x y)
    (values (+ x y) (* x y))))

(coalton
  (progn
    (let (values s p) = (sum-and-product 3 4))
    (+ s p))) ;; 19
```

### Keyword arguments

Coalton also supports keyword arguments. Keyword arguments come after all positional arguments and are introduced with `&key`:

```lisp
(coalton-toplevel
  (define (render-line x &key (width 80) (pad "."))
    ...))
```

Every actual keyword parameter must have an initial value. The parameter name determines the
surface keyword, so `width` is called with `:width` and `pad` is called with `:pad`.

Calls supply positional arguments first, followed by keyword/value pairs:

```lisp
(coalton-toplevel
  (render-line 5)
  (render-line 5 :width 120)
  (render-line 5 :pad "-" :width 120))
```

Keyword arguments are optional. If a keyword is omitted, its initial value is used. Once a
keyword argument appears, no later positional arguments are allowed.

We recommend using the `Optional` type to denote arguments which do not require a value.
However, an explicit `None` initializer is still required.

Anonymous functions use the same syntax:

```lisp
(coalton-toplevel
  (define add-offset
    (fn (x &key (offset 1))
      (+ x offset))))
```

Types write the keyword tail between the positional inputs and `->`:

```lisp
(declare render-line (Integer &key (:width Integer) (:pad String) -> String))
(declare make-default (&key (:x Integer) -> Integer))
```

A bare `&key` with no following keyword parameters is allowed in both definitions and types, but
it is treated exactly the same as omitting `&key` entirely:

```lisp
(define (identity-with-empty-key x &key) x)
(declare identity-with-empty-key (Integer &key -> Integer))
```

Explicit declarations are exact about which keywords exist. Inferred higher-order function types
may internally remain open to additional keywords for compatibility, but there is no separate
surface syntax for those open rows.

### Pipelining syntax and function composition

There are convenient syntaxes for composing functions with the `pipe` and `nest` macros.

```lisp
(nest f g ... h x)

;; is equivalent to

(f (g (... (h x))))

;; is equivalent to

(pipe x h ... g f)
```

These are useful to make code less noisy.

> [!NOTE]
> Since these are macros (indicated by their variadic arguments), they *cannot* be used as higher-order functions. Consider either an explicit `fn` wrapper or the `compose` function if you're thinking in that direction.

### Ignoring function parameters

Consider the function:

```lisp
(coalton-toplevel
  (define (f x y)
    x))
```

Here, `y` is unused and will produce a warning:

```
COMMON-LISP:WARNING: warn: Unused variable
  --> <unknown>:3:15
   |
 3 |    (define (f x y)
   |                 ^ variable defined here
help: prefix the variable with '_' to declare it unused
 3 |   (define (f x _y)
   |                --
```

As suggested, one can replace `y` with `_y`, which tells the Coalton compiler that the parameter might be intentionally unused.

Variables prefixed with `_` like `_y` are still normal variables, and can be read. The following is valid Coalton:

```lisp
(define (f _x) _x)
```

One should treat underscore-prefixed variables as ignored whenever possible, and use a name not prefixed with `_` if it may be used. Reading from underscore-prefixed variables is permitted so that generated code (e.g., using macros or read-conditionals) may avoid unused variable warnings for variables which will be used in some compilation contexts but not others.

## Data types, structs, and pattern matching

### Algebraic data types (`define-type`)

Coalton supports parametric algebraic data types (ADTs) using `define-type`:

```lisp
(coalton-toplevel
  ;; A single-constructor product type
  (define-type Point3D (Point3D Integer Integer Integer))

  ;; A sum type (enumeration)
  (define-type Color
    Red
    Blue
    Green)

  ;; A parametric recursive data type
  ;; Type parameters are written as keyword symbols such as :a
  (define-type (Tree :a)
    (Branch (Tree :a) :a (Tree :a))
    (Leaf :a)))
```

Type definitions introduce value constructors with the same names as the variants:

```lisp
(coalton
  (Branch (Leaf Red)
          Green
          (Branch (Leaf Red) Green (Leaf Red))))
```

Two ubiquitous standard library sum types are `Optional :a` (with constructors `None` and `(Some x)`) and `Result :err :ok` (with constructors `(Err e)` and `(Ok x)`).

### Pattern matching (`match`)

`match` expressions inspect and deconstruct algebraic data types, literals, and nested structures:

```lisp
(coalton-toplevel
  (define-type Color
    Red
    Blue
    Green)

  ;; Zero-argument constructors in patterns must still be wrapped in parentheses
  (declare color-to-string (Color -> String))
  (define (color-to-string c)
    (match c
      ((Red) "Red")
      ((Blue) "Blue")
      ((Green) "Green")))

  ;; Pattern variables are not wrapped in parentheses
  (declare map-optional ((:a -> :b) * (Optional :a) -> (Optional :b)))
  (define (map-optional f x)
    (match x
      ((Some x_) (Some (f x_)))
      ((None) None)))

  ;; Patterns can be nested, and wildcard "_" patterns match anything
  (declare flatten-optional ((Optional (Optional :a)) -> (Optional :a)))
  (define (flatten-optional x)
    (match x
      ((Some (Some x_)) (Some x_))
      (_ None)))

  ;; Subpatterns can be bound to a name with (= name pattern)
  (declare dedup-head (Eq :a => List :a -> List :a))
  (define (dedup-head xs)
    "If the first and second member of list are equal, drop the first"
    (match xs
      ((Cons a (= tl1 (Cons b _)))
       (if (== a b) tl1 xs))
      (_ xs)))

  ;; Integer and String literals can also be matched
  (define (is-5-or-7 x)
    (match x
      (5 True)
      (7 True)
      (_ False)))

  (define (is-five-or-seven x)
    (match x
      ("five"  True)
      ("seven" True)
      (_       False))))
```

Functions and `fn` expressions can also pattern-match directly on their parameters, provided the parameter patterns are exhaustive (such as single-constructor types like `Tuple`):

```lisp
(coalton-toplevel
  (declare first (Tuple :a :b -> :a))
  (define (first (Tuple a _)) a)

  (declare second (Tuple :a :b -> :b))
  (define second (fn ((Tuple _ b)) b))

  ;; Pattern capture with `=` works in parameter patterns too
  (declare nest-right (Tuple :a :b -> (Tuple :a (Tuple :a :b))))
  (define (nest-right (= tpl (Tuple a _)))
    (Tuple a tpl)))
```

### Structs (`define-struct`)

When a data type has a single constructor with named fields, `define-struct` defines both the constructor and `.field` accessor functions:

```lisp
(coalton-toplevel
  (define-struct Point
    (x Integer)
    (y Integer)))
```

Structs are constructed just like single-constructor ADTs:

```lisp
(coalton (Point 1 2))
```

Each field `name` gets a first-class reader function `.name`:

```lisp
(coalton (.x (Point 1 2)))

(coalton (map .x [(Point 1 2) (Point 2 3)]))
```

Structs can also be parametric:

```lisp
(coalton-toplevel
  (define-struct (Pair :a)
    (first :a)
    (second :a)))
```

### Type aliases (`define-type-alias`)

Parametric type aliases give shorthand names to existing types created with primitive types, `define-type`, `define-struct`, or other aliases:

```lisp
(coalton-toplevel
  (define-type-alias Coordinate Integer)
  (define-type-alias (Pair :a) (Tuple :a :a))
  (define-type-alias Translation (Pair Coordinate -> Pair Coordinate))

  (declare shift-right Translation)
  (define (shift-right (Tuple x y))
    (Tuple (1+ x) y))

  (define shifted-coordinate (shift-right (Tuple 0 0)))

  ;; Type aliases can have multiple parameters
  (define-type-alias (MyTuple3 :a :b :c) (Tuple :a (Tuple :b :c)))

  ;; Type aliases can have higher-kinded parameters
  (define-type-alias (IntegerCollection :col) (:col Integer))

  ;; Type aliases can alias unapplied type constructors
  (define-type-alias MyCollection List))
```

Whenever a parametric type alias is used, all of its parameters must be supplied:

```lisp
(coalton-toplevel
  (define-type (T :a) (ConstrT (:a Integer)))

  (define-type-alias (MyCollection1 :a) (List :a))
  (define-type-alias MyCollection2 List)

  ;; Invalid: MyCollection1 has a parameter :a which is not applied
  ;; (define-type-alias A (T MyCollection1))

  ;; Valid: MyCollection2 takes no parameters of its own
  (define-type-alias A (T MyCollection2)))
```

Outside of a Coalton expression, `describe-type-of` prints the type of a symbol together with its aliases, and `describe-type-alias` prints a diagnostic representation of an alias together with the aliased type. For ordinary type display such as `(coalton (type-of expr))`, `set-type-printing-mode` switches between `:types` (default), `:aliases`, and `:types-and-aliases` (or configure `:type-printing-mode` via `:coalton-config` before compiling Coalton):

```lisp
COALTON-USER> (coalton-toplevel
                (define-type-alias A Integer)
                (define x (the A 5)))
; No value

COALTON-USER> (set-type-printing-mode :aliases)
:ALIASES

COALTON-USER> (coalton (type-of x))
A

COALTON-USER> (set-type-printing-mode :types-and-aliases)
:TYPES-AND-ALIASES

COALTON-USER> (coalton (type-of x))
[A := INTEGER]

COALTON-USER> (set-type-printing-mode :types)
:TYPES

COALTON-USER> shifted-coordinate ;; from the example above
#.(TUPLE 1 0)

COALTON-USER> (coalton (type-of shifted-coordinate))
TUPLE INTEGER INTEGER

COALTON-USER> (describe-type-of 'shifted-coordinate)
[PAIR [COORDINATE := INTEGER] := TUPLE [COORDINATE := INTEGER] [COORDINATE := INTEGER]]

COALTON-USER> (describe-type-alias 'Pair)
; prints a diagnostic representation containing [PAIR :A := TUPLE :A :A]
```

## Local bindings, sequencing, and conditionals

### Local variables: `let` and `let*`

The operators `let` and `let*` are expression forms
for local bindings. `let` binds its variables recursively and in parallel
(similar to Scheme's `letrec`), while `let*` binds them left-to-right so each
initializer can refer to the earlier bindings.

```lisp
(coalton
  (let ((x (+ y 1))
        (y 2))
    (+ x y)))
```

```lisp
(coalton
  (let* ((x 1)
         (y (+ x 1)))
    (+ x y)))
```

`let` and `let*` also accept local `declare` forms:

```lisp
(coalton-toplevel
  (define (f a b)
    (let ((declare g (Integer * Integer -> Integer))
          (g +))
      (g a b))))
```

`let` can be used to introduce local (possibly mutually recursive)
functions:

```lisp
(let ((odd (fn (x)
             (if (zero? x)
                 False
                 (even (1- x)))))
      (even (fn (x)
              (if (zero? x)
                  True
                  (odd (1- x))))))
  (odd 5))
```

> [!NOTE]
> Since `let` introduces recursive bindings, it is generally
> not allowed for non-function variables to be self-referential, as in
> `(let ((x (1+ x))) ...)`. Instead, use `let*`.

### `progn` and shorthand `let`

Coalton has a `coalton:progn` construct similar to Common Lisp's `cl:progn` for evaluating a sequence of expressions in order and returning the value(s) of the final expression. This is useful in positions that expect a single expression, such as the branches of an `if`:

```lisp
(coalton-toplevel
  (declare checked-sqrt (F64 -> F64))
  (define (checked-sqrt x)
    (if (< x 0.0d0)
        (progn
          (trace "negative input; clamping to 0")
          0.0d0)
        (sqrt x))))
```

Function bodies (`define` and `fn`), as well as `match`, `cond`, `when`, `unless`, and loop bodies, already establish an implicit `progn`. Inside `progn` and those implicit-`progn` bodies, Coalton supports flattened shorthand `let` syntax (outside those contexts, use the block `let` and `let*` forms described above):

```lisp
(coalton-toplevel
  (declare f (Integer * Integer -> String))
  (define (f x y)
    (let x_ = (into x))
    (let y_ = (into y))
    (<> x_ y_)))
```

Flattened `let` expressions also support destructuring patterns, such as tuples:

```lisp
(coalton-toplevel
  (declare sum-tuple (Tuple Integer Integer -> Integer))
  (define (sum-tuple t)
    (let (Tuple fst snd) = t)
    (+ fst snd)))
```

Flattened `let` bindings are sequential and non-recursive, and they do not generalize into polymorphic bindings (`let` polymorphism). Thus the following is invalid:

```lisp
(coalton
  (progn
    (let id = (fn (x) x))
    (id Unit)
    (id "hello")))
```

To bind a local polymorphic function, use a block `let` expression instead:

```lisp
(coalton
  (let ((id (fn (x) x)))
    (progn
      (id Unit)
      (id "hello"))))
```

The values of all but the last form of a `progn` block are discarded. Because discarding a `Result` could silently ignore an error, Coalton warns when a `Result` value is discarded in a non-final position. To ignore the result on purpose, bind it to `_`:

```lisp
(coalton-toplevel
  (declare check (Integer -> (Result String Unit)))
  (define (check x)
    (if (> x 0) (Ok Unit) (Err "not positive")))

  (define (f x)
    (check x)            ; warning: Discarded Result
    (let _ = (check x))  ; no warning
    x))
```

The same warning applies to the final form of a `when`, `unless`, or loop body, whose value is also discarded.

### Conditionals: `if`, `cond`, `and`, and `or`

The operator `coalton:if` branches on a `Boolean` condition:

```lisp
(coalton-toplevel
  (define (is-even x)
    (if (== 0 x)
        True
        (is-odd (- x 1))))

  (define (is-odd x)
    (if (== 0 x)
        False
        (is-even (- x 1)))))
```

Several conditional branches can be combined with `cond`:

```lisp
(coalton-toplevel
  (define (fizz-buzz n)
    (cond
      ((and (== 0 (mod n 5))
            (== 0 (mod n 3)))
       "Fizzbuzz")
      ((== 0 (mod n 3))
       "Fizz")
      ((== 0 (mod n 5))
       "Buzz")
      (True
       (into n)))))
```

The Boolean operators `and` and `or` are variadic macros that short-circuit and can take zero or more arguments. Their two-argument functional counterparts (which can be passed as higher-order functions) are `boolean-and` and `boolean-or`:

```lisp
(coalton
  (or (cheap 5) True (really-expensive (expt 2 1000000))))
```

Here, `really-expensive` is never called due to short-circuiting.

### `unless` and `when`

The `coalton` package also includes `unless` and `when` for one-armed conditionals that return `Unit`. They are primarily used for conditional side effects:

```lisp
(coalton-toplevel
  (define (f x)
    (when (== x 5)
      (error "I only want the number 5"))))
```

Both `unless` and `when` establish an implicit `progn` block:

```lisp
(coalton-toplevel
  (define (f b)
    (when b
      (let x = 5)
      (let y = 7)
      (traceObject "sum" (+ x y)))))
```

### Early returns: `return` and `need`

Functions can return early with `return`:

```lisp
(coalton-toplevel
  (define (fizz-buzz n)
    (when (== 0 (mod n 15))
      (return "fizzbuzz"))

    (when (== 0 (mod n 3))
      (return "fizz"))

    (when (== 0 (mod n 5))
      (return "buzz"))

    (into n)))
```

When working with `Optional` or `Result`, `need` unwraps a `Some` or `Ok` value and otherwise returns `None` or `Err` early from the enclosing function, much like Rust's `?` operator:

```lisp
(coalton-toplevel
  (declare add-parsed (String * String -> (Optional Integer)))
  (define (add-parsed a b)
    (let x = (need (str:parse-int a)))
    (let y = (need (str:parse-int b)))
    (Some (+ x y))))
```

## Collections and mutable state

### Collection and association builder syntax

When the Coalton reader is active, square brackets construct collections and key-value associations:

```lisp
(coalton-toplevel
  (define xs [1 2 3])

  (define empty-xs
    (the (List Integer) []))

  (define pairs
    ["x" => 1
     "y" => 2])

  (define empty-pairs
    (the (Seq (Tuple String Integer))
         [=>])))
```

- `[a b c]` is a collection builder, and `[]` is an empty collection builder.
- `[k => v ...]` is an association builder, and `[=>]` is an empty association builder.

Collection builders are homogeneous in their element type; association builders are homogeneous in both their key type and their value type. Mixing element types in a builder is a compile-time type error:

```
COALTON-USER> (coalton-toplevel
                (define wut [1.0d0 2.0d0 3.0]))
; error: Collection element type mismatch
;   --> repl:2:29
;    |
;  2 |                  (define wut [1.0d0 2.0d0 3.0]))
;    |                               ----- earlier element here
;    |                                           ^^^ collection elements must all have the same type; expected 'F64' but got 'F32'
;    [Condition of type COALTON-IMPL/TYPECHECKER/BASE:TC-ERROR]
```

When no concrete result type is constrained by the surrounding context, collection builders default to `Seq :a` and association builders default to `Seq (Tuple :key :value)`. Use `declare` or `the` whenever you want a specific collection type (such as `List`, `Vector`, or `HashMap`), or to disambiguate empty builders `[]` and `[=>]`:

```lisp
(coalton-toplevel
  (define xs
    (the (List Integer) [1 2 3]))

  (define ys
    (the (Vector Integer) [1 2 3])))
```

Square brackets also support finite collection and association comprehensions:

```lisp
(coalton-toplevel
  (define evens
    [x :for x :below 10 :when (even? x)])

  (define scaled
    [y
     :for x :below 5
     :with y = (* x 10)
     :when (even? x)])

  (define first-ten
    [x :for x :below 10])

  (define squares
    [x => (* x x) :for x :below 5]))
```

The supported comprehension clauses are:

- `:for <var> :below <ufix>`: Step `<var>` from `0` up to `<ufix> - 1`.
- `:for <var> :in <iter>`: Step `<var>` through an iterator.
- `:with <var> = <expr>`: Bind `<var>` lexically for the remaining clauses and body.
- `:when <expr>`: Filter elements using the `Boolean` guard `<expr>`.

> [!NOTE]
> Collection and association comprehensions must be finite; they eagerly build the target collection.

User-defined types can also opt into `[...]` and `[k => v]` syntax. See [Extending Builder Syntax to User-Defined Types](/manual/topics/advanced-coalton-programming/#extending-builder-syntax-to-user-defined-types) in Advanced Coalton Programming.

### Immutable sequences and maps

Coalton encourages writing efficient *immutable* code by offering persistent collection types in the standard library. The two most important general-purpose collections are `Seq` (from `coalton/seq`) and `HashMap` (from `coalton/hashmap`), which provide logarithmic-time lookup/update and efficient concatenation.

### Lists

Coalton uses Common Lisp lists under the hood. Lists can be constructed with `[...]` (when constrained to `List`) or directly with the constructors `Cons` and `Nil`:

```lisp
(coalton-toplevel
  (declare x (List Integer))
  (define x [1 2 3])

  (declare y (List String))
  (define y ["a" "b" "c"]))
```

Lists are deconstructed with `match` using `(Cons head tail)` and `(Nil)`:

```lisp
(coalton-toplevel
  (define (is-empty lst)
    (match lst
      ((Cons _ _) "is not empty")
      ((Nil) "is empty"))))
```

### Mutable cells and vectors

While Coalton defaults to immutable bindings, local imperative state is often convenient for accumulators and loops:

- `coalton/cell:Cell` (usually nicknamed `cell`) is a mutable single-value box created with `(cell:new init)`, read with `(cell:read c)`, and mutated with `(cell:write! c val)`, `(cell:increment! c)`, or `(cell:push! c item)`.
- `coalton/vector:Vector` (usually nicknamed `vector`) is a resizable mutable array created with `(vector:new)` or `(the (Vector :a) [...])`, mutated with `(vector:push! item v)` and `(vector:pop! v)`.

> [!TIP]
> In release mode, a `Cell` allocated with `cell:new` that does not escape its defining function is unboxed into a typed mutable lexical variable with zero heap allocation. See [Performance and Optimization](/manual/topics/performance-and-optimization/).

To keep the type system sound in the presence of mutation, Coalton applies a relaxed value restriction: a top-level `define` that allocates a mutable container (such as `Vector` or `Cell`) cannot remain polymorphic in its element type. Either make the definition a function so each call allocates fresh state, or declare a concrete type:

```lisp
(coalton-toplevel
  (declare int-vec (Vector Integer))
  (define int-vec (coalton/vector:new)))
```

See [Polymorphism, Mutation, and the Value Restriction](/manual/topics/advanced-coalton-programming/#polymorphism-mutation-and-the-value-restriction) in Advanced Coalton Programming for the full rules.

## Looping & iteration

Coalton offers three main ways to write explicit loops:

- Tail-recursion and `rec`,
- Built-in imperative `for` and `for*` loops, and
- Experimental Lisp-style iteration macros (e.g., `dotimes`).

### Tail-recursion and `rec`

Coalton eliminates self-tail calls, making tail recursion idiomatic:

```lisp
(coalton-toplevel
  (define (find-integer predicate limit)
    (cond
      ((negative? limit) False)
      ((predicate limit) True)
      (True              (find-integer predicate (1- limit))))))
```

This function can run for an arbitrarily large `limit` without growing the call stack:

```lisp
> (coalton (find-integer (fn (x) (== 65536 (* x x))) 1000000))
COMMON-LISP:T
```

For local accumulator loops, Coalton provides the built-in `rec` operator ("named-`let`"-style recursion):

```lisp
(rec <name> (<binding>*)
  <body>...)

;; <binding> := <declare-form>
;;            | (<symbol> <value>)
```

`<name>` is bound as a local tail-recursive function inside `<body>`, and the binding list supplies the arguments for its initial call. Local `declare` forms inside the binding list constrain the initial bindings (and thus the loop parameters). To constrain the result type of the whole `rec` expression, wrap it in `the`.

Recursive calls to `<name>` must be direct tail calls. If you need non-tail local recursion or want to pass the local function as a value, use `let` with `fn` instead. It is idiomatic in Coalton to choose `%` as `<name>` for small anonymous loops:

```lisp
(coalton-toplevel
  (define (rev l)
    (rec % ((result Nil) (remaining l))
      (match remaining
        ((Nil) result)
        ((Cons x xs) (% (Cons x result) xs))))))
```

Here is a Fibonacci function that is polymorphic in both its input and output numeric types:

```lisp
(coalton-toplevel
  (define (fib-polymorphic n)
    (rec % ((i n) (a 0) (b 1))
      (cond
        ((== 0 i) a)
        ((== 1 i) b)
        (True (% (- i 1) b (+ a b)))))))
```

Its inferred type is `∀ :A :B. (EQ :A) (RING :A) (RING :B) ⇒ (:A → :B)`, where `Ring` is the type class for numeric types that support `+`, `-`, `*`, and integer literals like `0` and `1` (see [Numbers](#numbers)):

```
> (coalton (the F32 (fib-polymorphic (the Integer 10))))
55.0
```

We can make a monomorphic variant for `Integer` by declaring the initial bindings and wrapping the `rec` in `the`:

```lisp
(define (fib-monomorphic n)
  (the Integer
    (rec %
         ((declare i Integer)
          (declare a Integer)
          (declare b Integer)
          (i n)
          (a 0)
          (b 1))
      (cond
        ((== 0 i) a)
        ((== 1 i) b)
        (True (% (- i 1) b (+ a b)))))))
```

Now it can be called without type annotations at the call site:

```
> (coalton (fib-monomorphic 10))
55
```

### Conventional looping: `for` and `for*`

The built-in imperative `for` form and its sequential variant `for*` have the
following shape:

```lisp
(for [label] (<binding-clause>*)
  [:returns expr]
  [(:while | :until | :repeat) expr]
  body...)

;; <binding-clause> := (declare var Type)
;;                   | (var init-expr step-expr)
```

The binding list is required, though it may be empty: `()`. Initializers are
evaluated once before the first iteration. In `for`, the initializer binding
group follows the same scoping rules as `let`, and step expressions are
evaluated in parallel using the pre-step bindings from that iteration. In
`for*`, initializers follow `let*`-style left-to-right scoping, and step
expressions are updated left-to-right so later steps can refer to
earlier updated variables (analogous to Common Lisp's `do*`). If no termination clause is present, the loop runs forever unless exited with `break` or `return`.

Each non-`declare` loop binding must include both an initializer and a step
expression. If a loop variable should stay constant across iterations, repeat it
as its own step, for example `(x 10 x)`.

You can iterate forever:

```lisp
(for ()
  (show "hi"))
```

You can also use `for` as a counted or state-threading loop:

```lisp
(coalton
 (for ((declare i UFix)
       (declare acc UFix)
       (i 0 (1+ i))
       (acc 0 (+ acc i)))
   :returns acc
   :repeat 10
   (when (even? i)
     (continue))
   (show i)))
```

When later loop bindings should build on earlier ones, use `for*`:

```lisp
(coalton
 (let ((xs (the (Vector Integer) [10 20 30])))
   (for* ((declare i UFix)
          (declare x Integer)
          (i 0 (1+ i))
          (x (vector:index-unsafe i xs) (vector:index-unsafe i xs)))
     :returns (Tuple i x)
     :repeat 2
     Unit)))
```

Here `x` tracks the current vector element while `i` tracks the index. Because
this is `for*`, the step for `x` sees the updated value of `i`.

If a `:returns` clause is present, that expression is evaluated once when the `for` exits
normally or via `break`. If `:returns` is absent, the `for` returns `Unit`.

You can use a `:while` clause when you only need a condition and a body:

```lisp
(coalton
 (let ((counter (cell:new 0))
       (limit 10))
   (for ()
     :while (< (cell:read counter) limit)
     (show "hi")
     (cell:increment! counter))))
```

You can also combine `for` with `match` and `break` when iteration depends on a
pattern match:

```lisp
(coalton
 (let ((xs (the (Vector Integer) [4 3 2 1])))
   (for ()
     (match (vector:pop! xs)
       ((Some x) (show x))
       ((None) (break))))))
```

#### `break` and `continue`

The `break` form immediately terminates the loop. The following prints `0`, `1`, and `2` and then exits:

```lisp
(coalton
 (let ((counter (cell:new 0)))
   (for ()
     (when (== 3 (cell:read counter))
       (break))
     (show (cell:read counter))
     (cell:increment! counter))))
```

The `continue` form skips the remainder of the current iteration's body and proceeds to the step phase and next iteration. The following prints `1`, `3`, and `5`:

```lisp
(coalton
 (let ((counter (cell:new 0)))
   (for ()
     (when (== 6 (cell:read counter))
       (break))
     (let n = (cell:read counter))
     (cell:increment! counter)
     (when (== 0 (mod n 2))
       (continue))
     (show n))))
```

Note that `continue` still evaluates the loop's step expressions before the next iteration, whereas `break` skips the step phase and exits immediately.

#### For labels

`for` and `for*` accept an optional keyword label immediately after the operator name, allowing `break` and `continue` to target an enclosing loop:

```lisp
(coalton
  (let ((acc (cell:new Nil)))
    (for :outer ((x 0 (+ x 10)))
      (for ((y 0 (1+ y)))
        :while (< y 10)
        (let total = (+ x y))
        (when (< 500 total)
          (break :outer))
        (when (== 0 (mod y 3))
          (continue))
        (cell:push! acc total)))
    (cell:read acc)))
```

### Experimental Lisp iteration macros

> [!WARNING]
> These constructs are experimental and may change.

The standard library package [`coalton/experimental/loops`](https://coalton-lang.github.io/reference/#coalton/experimental/loops-package) (nicknamed `loops` in `#:coalton-user`) contains Lisp-style iteration macros such as `dotimes` and `dolist`. They generally return `Unit` unless collecting or accumulating a value, and use `UFix` for counts and indices:

```lisp
(coalton-toplevel
  (define (print-pythagorean-triples limit)
    (loops:dotimes (c limit)
      (loops:dotimes (b c)
        (loops:dotimes (a b)
          (when (== (^ c 2) (+ (^ a 2) (^ b 2)))
            (traceObject "triple" (Tuple3 a b c))))))))

;; > (coalton (print-pythagorean-triples 20))
;; triple: #.(TUPLE3 3 4 5)
;; triple: #.(TUPLE3 6 8 10)
;; triple: #.(TUPLE3 5 12 13)
;; triple: #.(TUPLE3 9 12 15)
;; triple: #.(TUPLE3 8 15 17)
```

## Type classes

Type classes define interfaces of overloaded operations that types can implement.

All member methods declared by a class must be defined in each `define-instance`.

```lisp
(coalton-toplevel
  ;; Type classes are defined with define-class
  (define-class (Eq :a)
    (== (:a * :a -> Boolean)))

  (define-type Color
    Red
    Green
    Blue)

  ;; Type class instances are defined with define-instance
  (define-instance (Eq Color)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Red) (Red)) True)
        ((Tuple (Blue) (Blue)) True)
        ((Tuple (Green) (Green)) True)
        (_ False))))

  ;; Type declarations attach class constraints before `=>`
  (declare is-eql (Eq :a => (:a * :a -> String)))
  (define (is-eql a b)
    (if (== a b)
        "They are equal"
        "They are not equal"))

  ;; Multiple constraints must be wrapped in parentheses
  (declare double-is-eql ((Eq :a) (Eq :b) => (:a * :a * :b * :b -> String)))
  (define (double-is-eql a b c d)
    (if (and (== a b) (== c d))
        "Both pairs are equal"
        "The pairs are not both equal")))
```

For multi-parameter type classes with functional dependencies, overlapping instances (`overlap`), and method-scoped type variables, see [Advanced Coalton Programming](/manual/topics/advanced-coalton-programming/).

### Builtin type classes

The most common standard library type classes fall into three groups:

**Comparison, arithmetic, and combining values:**
* `Eq` (`==`) — types that can be compared for equality.
* `Ord` (`<=>`, `<`, `<=`, `>`, `>=`) — types with a total ordering.
* `Hash` (`hash`) — types that can be hashed (for example, as `HashMap` keys).
* `Num` (and its algebraic superclasses `Additive`, `AdditiveGroup`, `Multiplicative`, `Semiring`, and `Ring`: `+`, `-`, `neg`, `*`, `zero`, `one`, `from-int`) — numeric and algebraic types.
* `Semigroup` (`<>`) — types with an associative binary combining operation (such as string or list concatenation).
* `Monoid` (`mempty`) — semigroups with an identity element.
* `Default` (`default`) — types with a standard default value.

**Container and effect abstractions** (similar to their Haskell counterparts):
* `Functor` (`map`) — type constructors whose elements can be transformed with a function (called `fmap` in Haskell).
* `Applicative` (`pure`, `liftA2`) — functors that can wrap values and combine independent computations.
* `Monad` (`>>=`) — sequential computations where each step can depend on the previous result (use `pure` from `Applicative` rather than Haskell's `return`).
* `Alternative` (`alt`, `empty`) — choice and failure over type constructors (`alt` corresponds to Haskell's `<|>`).
* `Foldable` (`fold`, `foldr`) — containers that can be reduced to a summary value.
* `Traversable` (`traverse`, `sequence`) — containers that can be mapped over with an `Applicative` effect.

**Type conversions** (inspired by Rust):
* `Into` (`into`) — total conversions from one type to another.
* `TryInto` (`try-into`) — fallible conversions returning `Optional`.

### Automatically deriving type class instances (`derive`)

Instances of `Eq`, `Ord`, `Hash`, `Default`, and `Show` can be derived automatically on `define-type` and `define-struct` definitions using [`derive`](/manual/operators/derive/):

```lisp
(coalton-toplevel
  (derive Eq Hash)
  (define-struct Point
    (x UFix)
    (y UFix)))
```

Now `==` and `hash` work on `Point` values directly:

```lisp
(coalton
  ;; Test `Point` equality
  (== (Point 1 2) (Point 3 3))

  ;; Make a map using `Point` as a key
  (let ((map (the (coalton/hashmap:HashMap Point UFix)
                  coalton/hashmap:empty)))
    (coalton/hashmap:insert map (Point 0 0) 1)))
```

The generated instance follows the structural definition of the type (for example, `Eq` compares every field of matching constructors). When a type needs custom comparison or hashing semantics, write a manual `define-instance` instead. Writing custom derivers does not yet have a public API, though see [derivers.lisp](https://github.com/coalton-lang/coalton/blob/main/library/derivers.lisp) for how built-in derivers are implemented.

### Type casting, coercing, and conversion

Total conversions between types use the two-parameter `Into :from :to` type class and its method `into`. Because `into` takes only the source value, the target type `:to` is determined either by the surrounding context:

```lisp
(coalton-toplevel
  (define integer-part (substring (into 12.34d0) 0 2)))

;; ==> "12"
```

or by an explicit type annotation with `the`:

```lisp
(coalton-toplevel
  (define unique-letters
    (remove-duplicates (the (List Char) (into "mississippi")))))

;; ==> (#\m #\s #\p #\i)
```

Because `(the Target (into expr))` is so common, Coalton provides the `as` shorthand:

```lisp
(coalton-toplevel
  (define unique-letters
    (remove-duplicates (as (List Char) "mississippi"))))

;; ==> (#\m #\s #\p #\i)
```

`Into` is only for *total* conversions that always succeed. When a conversion can fail at runtime (such as parsing or narrowing a number), types implement `TryInto` and its method `try-into`, which returns `Optional` (`Some` on success and `None` on failure). You can also use [`try-as`](/manual/operators/try-as/) or [`unwrap-as`](/manual/operators/unwrap-as/) when specifying the target type explicitly.

The standard library provides the identity conversion `Into :a :a` alongside more specific generic conversions using [overlapping instances](/manual/topics/advanced-coalton-programming/#overlapping-instances).

### Do notation

The `do` macro provides a readable sequential syntax for chaining `Monad` operations (such as `Optional`, `Result`, `List`, or state/IO-like types) without deeply nested `>>=` callbacks:

```lisp
(coalton-toplevel
  (define (f ax bx)
    (do
      (a <- ax)
      (b <- bx)
      (let c = (+ a b))
      (pure c)))

  ;; For lists, `do` computes the Cartesian combinations:
  ;; [1+4, 1+5, 1+6, 2+4, 2+5, 2+6, 3+4, 3+5, 3+6]
  (declare xs (List Integer))
  (define xs (f [1 2 3] [4 5 6])))
```

Inside `do`, `(var <- expr)` binds the result of a monadic step via `>>=`, `(let var = expr)` binds a pure local value, and bare expressions are sequenced in order.

## Numbers

Coalton supports several numeric types. The most common are arbitrary-precision `Integer`, 32-bit floating-point `F32`, and 64-bit floating-point `F64`:

```lisp
(coalton-toplevel
  (define num-int 5)
  (define num-sf  5.0f0)
  (define num-df  5.0d0))
```

An unadorned floating-point literal such as `5.0` is read according to `cl:*read-default-float-format*` (which defaults to `cl:single-float`, i.e., `F32`).

Coalton also provides fixed-width signed integers (`I8`, `I16`, `I32`, `I64`, `IFix`), fixed-width unsigned integers (`U8`, `U16`, `U32`, `U64`, `UFix`), and exact rational numbers (`Fraction`, the ratio of two `Integer`s).

All numeric types implement the `Num` type class (and its algebraic superclasses `Additive`, `AdditiveGroup`, `Multiplicative`, `Semiring`, and `Ring`), which provides `+`, `-`, `neg`, `*`, `zero`, `one`, and `from-int`. Integer literals in source code are polymorphic: `5` is shorthand for `(from-int 5)` and can stand for any `Ring` or `Num` type. For a complete guide to numeric types, rounding, approximation, elementary functions, and algebraic structures, see [Numbers and Math](/manual/topics/numbers-and-math/).

### Division, in short

The `coalton-prelude` package exports `/` (from `Reciprocable`), `floor`, `ceiling`, and `round`, while `coalton/math` (nicknamed `math` in `#:coalton-user`) provides additional division helpers:

- The division operator `/` and its variants signal a run-time error if the divisor is zero. Use `math:safe/` if you prefer an `Optional` return type.
- If you have integers and want a double-float (`F64`), use `math:inexact/`. (For `F32`, convert with `into` or constrain `/` with `the F32`.)
- If you have integers and want an integer quotient, use `math:floor/`, `math:ceiling/`, or `math:round/`.
- If you want an exact rational (`Fraction`) from two integers, use `math:exact/`.
- If you are writing generic code over a single field-like type, use `/` (from `Reciprocable`).

### Details about division

Why isn't `/` part of `Num`? Division is separated from `Num` for two reasons:

1. Division by zero can fail at runtime.
2. Dividing two values of type `:s` does not necessarily produce a result of the same type `:s` (for example, dividing two `Integer`s).

Coalton separates same-type division from cross-type division:

- `/` is the method of `Reciprocable :a`, where both operands and the result share the same type `:a`:

  ```
  COALTON-USER> (describe-type-of '/)
  ∀ :A. RECIPROCABLE :A ⇒ :A * :A → :A
  ```

- `math:general/` is the method of `(math:Dividable :s :t)`, which divides two values of type `:s` to produce a result of type `:t`.

When you evaluate `(/ 1 2)` at the REPL with no surrounding type context, the expression requires `Reciprocable :a` (which implies `Ring :a`). Because `Integer` does not implement `Reciprocable`, Coalton's [instance defaulting](/manual/topics/advanced-coalton-programming/#instance-defaulting) resolves `:a` to `F64`:

```
COALTON-USER> (coalton (/ 1 2))
0.5d0
```

You can choose another `Reciprocable` type such as `F32` or `Fraction` with `the`:

```
COALTON-USER> (coalton (the F32 (/ 4 2)))
2.0
COALTON-USER> (coalton (the Fraction (/ 4 2)))
2
```

Requesting an `Integer` result from `/` is a compile-time error because `Integer` is not `Reciprocable`:

```
COALTON-USER> (coalton (the Integer (/ 4 2)))
; error: Unable to codegen
;   --> repl:1:22
;    |
;  1 |  (coalton (the Integer (/ 4 2)))
;    |                        ^^^^^^^ expression has ambiguous type Reciprocable Integer ⇒ Integer
;    |                        ^^^^^^^ Add a type assertion with THE to resolve ambiguity
;    [Condition of type COALTON-IMPL/TYPECHECKER/BASE:TC-ERROR]
```

Coalton keeps integer division rounding explicit. Built on top of `math:general/`, `coalton/math` provides:

- `math:safe/`: returns `None` on division by zero and `(Some result)` otherwise.
- `math:exact/`: divides two `Integer`s to produce an exact `Fraction`.
- `math:inexact/`: divides two `Integer`s to produce an `F64`.
- `math:floor/`, `math:ceiling/`, and `math:round/`: divide two `Integer`s and round to an `Integer`:

```
COALTON-USER> (coalton (math:floor/ 3 2))
1
COALTON-USER> (coalton (math:ceiling/ 3 2))
2
COALTON-USER> (coalton (math:round/ 3 2))
2
```

A `Fraction` can also be converted to any `math:Dividable Integer :t` type with `math:fromfrac` (which may lose precision for floating-point targets):

```
COALTON-USER> (coalton (the F64 (math:fromfrac 1/2)))
0.5d0
COALTON-USER> (coalton (the F32 (math:fromfrac 999/1000)))
0.999
```

## Dynamic variables

Coalton supports dynamically scoped variables, analogous to Common Lisp special variables:

- Dynamic variable names must begin and end with `*` and contain at least one non-`*` character (e.g., `*base*` or `*current-port*`). Ordinary lexical variables cannot use earmuff names.
- Dynamic variables are declared and defined at top level with the usual `declare` and `define` forms:

```lisp
(coalton-toplevel
  (declare *base* Integer)
  (define *base* 10)

  (define (base-value)
    *base*))
```

Referencing a dynamic variable uses its name directly:

```lisp
(coalton *base*) ; ==> 10
```

To dynamically rebind one or more dynamic variables for the duration of a body, use `dynamic-bind`:

```lisp
(coalton
  (dynamic-bind ((*base* 20))
    (base-value))) ; ==> 20
```

Inside `progn`, function bodies, and similar flattened-body contexts, `(let *var* = expr)` is shorthand for `dynamic-bind` over the remainder of the block:

```lisp
(coalton-toplevel
  (define (f)
    (let *base* = 20)
    (base-value))) ; ==> 20
```

`dynamic-bind` bindings are evaluated in parallel in the outer dynamic environment, so initializers see the previous dynamic values:

```lisp
(coalton-toplevel
  (declare *left* Integer)
  (define *left* 1)
  (declare *right* Integer)
  (define *right* 2)

  (define swapped
    (dynamic-bind ((*left* *right*)
                   (*right* *left*))
      (Tuple *left* *right*))))
```

Every dynamic rebinding must preserve the variable's declared type (and polymorphic rebindings remain subject to the value restriction). Because `dynamic-bind` restores the old dynamic bindings on exit, wrapping a recursive call in `dynamic-bind` prevents tail-call elimination.

## Error handling and exceptions

For expected recoverable failures, idiomatic Coalton code usually returns `Optional` or `Result` and propagates failures with [`need`](#early-returns-return-and-need) or [`do` notation](#do-notation).

Coalton also provides an exception and condition handling facility that interoperates with Common Lisp conditions:

- `define-exception`: Defines an exception type using the same constructor syntax as `define-type`.
- `throw`: Signals an exception.
- `catch`: Catches exceptions by pattern matching on exception constructors or types after unwinding the stack.
- `protect`: Runs cleanup forms whenever control leaves an expression (similar to `cl:unwind-protect`).

Coalton also supports non-unwinding handlers and restarts (`define-resumption`, `handle`, `resumable`, and `resume-to`); see [Resumptions and Non-Unwinding Handlers](/manual/topics/advanced-coalton-programming/#resumptions-and-non-unwinding-handlers) in Advanced Coalton Programming.

### Defining, throwing, and catching exceptions

To catch any exception—including arbitrary Common Lisp `error` conditions—use a wildcard `_` pattern in `catch`:

```lisp
(declare divide-by-random (Integer * Integer -> Integer))
(define (divide-by-random r m)
  "Divide `r` by a random integer between `0` and `m`.
   If the divisor is `0`, print a trace message and return `0`."
  (catch (lisp (-> Integer) (r m) (cl:/ r (cl:random m)))
    (_ (trace "An error was received")
       0)))
```

To define and match structured Coalton exceptions, use `define-exception`:

```lisp
(coalton-toplevel
  (define-type Egg
    ;;     cracked? cooked?
    (Goose Boolean Boolean)
    (Xenomorph))

  ;; Define an exception type BadEgg with two variants
  (define-exception BadEgg
    (UnCracked Egg)
    (DeadlyEgg Egg))

  ;; If we try to crack open a Xenomorph egg, throw a DeadlyEgg exception
  (declare crack (Egg -> Egg))
  (define (crack egg)
    (match egg
      ((Goose _ cooked?)
       (Goose True cooked?))
      ((Xenomorph)
       (throw (DeadlyEgg egg)))))

  ;; Catch BadEgg variants and convert them into a Result
  (declare crack-safely (Egg -> (Result BadEgg Egg)))
  (define (crack-safely egg)
    (catch (Ok (crack egg))
      ((DeadlyEgg _) (Err (DeadlyEgg egg)))
      ((UnCracked _) (Err (UnCracked egg))))))
```

A `catch` branch written `(the T var)` matches every exception of type `T` and binds the exception object itself to `var`:

```lisp
(declare crack-safely (Egg -> (Result BadEgg Egg)))
(define (crack-safely egg)
  (catch (Ok (crack egg))
    ((the BadEgg e) (Err e))))
```

A bound exception can be rethrown unchanged with `(throw e)`.

The standard library functions `coalton/result:try` and `coalton/result:ok-or-throw` convert between thrown exceptions and `Result` values:

```lisp
(declare crack-safely (Egg -> (Result BadEgg Egg)))
(define (crack-safely egg)
  (result:try (fn () (crack egg))))
```

### Panics

Functions such as `error`, `unwrap`, `expect`, `unreachable`, and `undefined`, as well as failed `assert`s, signal a `Panic`. A panic indicates a bug rather than an expected failure. Panics are exceptions like any other: a wildcard `_` branch catches them along with all other Lisp errors, and a `(the Panic p)` branch catches only panics. From Common Lisp, they are conditions of type `coalton/classes:panic`.

### Catching Lisp conditions

An existing Common Lisp condition type (which must be a subtype of `cl:serious-condition`) can be exposed as a Coalton exception type using `(repr :native ...)` and an empty `define-exception`:

```lisp
(coalton-toplevel
  (repr :native cl:division-by-zero)
  (define-exception DivisionByZero)

  (declare divide-or-zero (Integer * Integer -> Fraction))
  (define (divide-or-zero r m)
    (catch (lisp (-> Fraction) (r m) (cl:/ r m))
      ((the DivisionByZero _) 0))))
```

The `coalton/exception` package predefines exception types for standard Common Lisp conditions:

```lisp
(declare checked-divide (Integer * Integer -> (Result String Fraction)))
(define (checked-divide r m)
  (catch (Ok (lisp (-> Fraction) (r m) (cl:/ r m)))
    ((the exception:ArithmeticError e) (Err (exception:message e)))))
```

`exception:LispError` covers every Common Lisp `cl:error`. `exception:message` returns the condition's formatted message, and `exception:cast` narrows an exception to a more specific condition type when applicable (such as `ArithmeticError` to `DivisionByZero`). See [Lisp Interop](/manual/topics/lisp-interop/) for details.

### Cleaning up (`protect`)

`protect` runs its cleanup forms however control leaves the protected expression—whether normally, via `throw`, or via `return` or `break`:

```lisp
(define (process-file path)
  (let stream = (need (file:open path)))
  (protect (process stream)
    (let _ = (file:close stream))
    Unit))
```

## Packages and ASDF systems

Coalton uses ordinary Common Lisp packages and ASDF systems to organize libraries and applications. For a step-by-step walkthrough of creating a new project from scratch, see [Introduction and Getting Started](/manual/topics/introduction/).

### Packages

Unlike Common Lisp, which has a single large `"COMMON-LISP"` package, Coalton's standard library is organized into focused packages such as `#:coalton/string`, `#:coalton/vector`, and `#:coalton/math`. Refer to the [Coalton Reference](/reference/) for the complete list of standard library packages.

When defining a package for your own code, *do not `:use` `#:cl` or `#:common-lisp`*. Instead, `:use` `#:coalton` (core language forms) and `#:coalton-prelude` (common standard library exports), and import other packages with `:local-nicknames`:

```lisp
(defpackage #:my-package
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:str  #:coalton/string)
   (#:vec  #:coalton/vector)
   (#:math #:coalton/math)))

(in-package #:my-package)
```

Using `:local-nicknames` avoids symbol clashes: many Coalton packages intentionally export the same short function names (for example, both `str:length` and `vec:length`). We recommend never `:use`-ing any standard library packages other than `#:coalton` and `#:coalton-prelude`.

### `.ct` files and ASDF

Coalton code can be written in `.ct` files or in ordinary `.lisp` files:

- A `.ct` file is read using Coalton's readtable automatically (enabling accurate source locations for compiler diagnostics, `[...]` builder syntax, and `ƒ` shorthand).
- If you write Coalton in a `.lisp` file, enable the readtable explicitly after `in-package`:

  ```lisp
  (named-readtables:in-readtable coalton:coalton)
  ```

To use `.ct` files in an ASDF system, add `"coalton-asdf"` to `:defsystem-depends-on` and list components with `:ct-file`:

```lisp
(asdf:defsystem "my-project"
  :defsystem-depends-on ("coalton-asdf")
  :depends-on ("coalton")
  :components ((:file "lisp-functions")   ; .lisp file
               (:ct-file "core")))        ; .ct file
```

## Inline Lisp and inspecting the system

### Inline Lisp (`lisp`)

Coalton can embed raw Common Lisp expressions using `lisp`:

```lisp
(coalton-toplevel
  (declare random-int (Integer -> Integer))
  (define (random-int n)
    (lisp (-> Integer) (n)
      (cl:random n))))
```

The syntax is:

```lisp
(lisp (-> <output-type-spec>) (<coalton-variables>) <lisp-form>...)
```

`lisp` is an unsafe boundary: Coalton trusts the declared `<output-type-spec>` without type-checking the raw Common Lisp body.

`<output-type-spec>` may be `Void` (returning no values), a single type (consuming the primary Lisp value), or a `*`-separated list of types (returning Common Lisp multiple values directly):

```lisp
(coalton-toplevel
  (declare quot-rem (Integer * Integer -> Integer * Integer))
  (define (quot-rem x y)
    (lisp (-> Integer * Integer) (x y)
      (cl:truncate x y))))

(coalton
  (progn
    (let (values q r) = (quot-rem 17 5))
    (Tuple q r)))
```

For full details on data representation guarantees, calling conventions, and bridging Coalton and Common Lisp, see [Lisp Interop](/manual/topics/lisp-interop/).

### Inspecting the Coalton system

The `coalton` package provides several interactive inspection helpers. `type-of` (inside `coalton`) and `kind-of` (in Common Lisp) inspect types and kinds:

```
COALTON-USER> (coalton (type-of map))
∀ A B (F (Type → Type)). FUNCTOR F ⇒ (A → B) * F A → F B
COALTON-USER> (kind-of 'Result)
* → (* → *)
```

The following Common Lisp functions all take an optional package designator and print environment summaries:

* `print-type-db` — print every known type
* `print-value-db` — print the type of every top-level value
* `print-class-db` — print every type class and its methods
* `print-instance-db` — print the instances of every type class
* `print-specializations` — print active function specializations

See [Debugging Operators](/manual/topics/debugging/) for code-generation inspection tools such as `pprint-coalton-codegen`.

