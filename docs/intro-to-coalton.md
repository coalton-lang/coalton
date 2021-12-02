# Whirlwind Tour of Coalton

Coalton is a statically typed language that is embedded in and compiles to Common Lisp.

This document is aimed toward individuals with familiarity with strongly typed functional programming languages already.

## Program Structure

To start, we recommend changing your package to the `COALTON-USER` package like so:

```lisp
(in-package #:coalton-user)
```

This package does *not* `:use` the `COMMON-LISP` package, so you must prepend Common Lisp symbols with `cl:` if you need them.

The first primary entry points for Coalton code. Definitions and the like sit in a toplevel-form called `coalton-toplevel`.

```lisp
(coalton-toplevel
  ;; <Coalton definition forms>
  )
```

Currently, in SLIME/SLY, there's no way to `C-c-c` in any finer-grained way than a whole `coalton-toplevel` form.

The second primary entry point is calling Coalton from Lisp. In this case, one uses the `coalton` operator:

```lisp
;; Lisp code
;; ...
     (coalton #|coalton expression|#)
;; ...
```

Note that one _cannot_ make new definitions in a `coalton` form, only evaluate expressions.

Whereas `coalton-toplevel` expects one or more toplevel definitions or declarations, the `coalton` form takes a single expression, evaluates it relative to the current environment, and returns its (underlying) Lisp value. This can be useful for working with Coalton from a Lisp REPL.

## Variables and Functions

Variables and functions are defined with `define`. Here are some variable definitions.

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

One can get the first element of the tuple `p` defined above from the REPL using the `coalton` operator:
```lisp
(coalton (fst p))
```

**Note**: It may be tempting to elide the `coalton` form and simply evaluate `(fst p)` directly in a Lisp REPL, but such behavior should not be relied upon!

Functions are defined similarly to variables. Unlike Common Lisp, Coalton functions occupy the same namespace as variables. This makes high-order functional programming easier.

```lisp
(coalton-toplevel
  ;; Functions are also defined with the define keyword
  (define (add2 x)
    (+ 2 x))

  ;; Functions exist in the same namespace as variables
  (define addTwo add2)

  (define x (addTwo 3))

  ;; Anonymous functions can be defined with fn
  (define z (map (fn (x) (+ 2 x)) (make-list 1 2 3 4))))
```

*All* functions in Coalton take *exactly* one input, and produce *exactly* one output. Consider this function:

```lisp
(coalton-toplevel
  (define (fma a b c)
    (+ c (* a b))))
```

Truth be known, `fma` actually technically takes *one argument*: `a`. To a Common Lisper, this function is roughly equivalent to:

```lisp
(defun fma (a)
  (lambda (b)
    (lambda (c)
      (+ c (* a b)))))
```

However, Coalton hides this reality from you unless you need it:

```lisp
(coalton-toplevel
  (define fma1 (fma 2))      ; equiv: b -> (c -> (c + 2*b))
  (define fma2 (fma 2 3))    ; equiv: c -> (c + 6)
  (define fma3 (fma 2 3 4))) ; equiv: 10
```

We can see that we can call `fma` as if it's a three argument function, but that's merely convenient syntax. We can also call it with fewer arguments. Sometimes this property is called *curried functions*.

Coalton does work to optimize these functions to reduce as much closure allocation as possible. In fact, `(fma x y z)` will get compiled as a simple add and multiply, without any closure allocations.

Here is an example of using a curried function to transform a list.

```lisp
(coalton-toplevel
  ;; Lists can be created with the make-list macro
  (define nums (make-list 2 3 4 5)))

(coalton
  ;; Functions in coalton are curried
  (map (+ 2) nums)) ;; 4 5 6 7
```

There are convenient *syntaxes* for composing functions with the
`pipe` and `nest` macros:

```lisp
(nest f g ... h x)

;; is equivalent to

(f (g (... (h x))))

;; is equivalent to

(pipe x h ... g f)
```

These are useful to make code less noisy.

Note that since these are macros (indicated by their variadic arguments), they cannot be used as high-order functions. Consider either currying or the `compose` function if you're thinking in that direction.

## Data Types

Coalton allows the definition of parametric algebraic data types.

```lisp
(coalton-toplevel
  ;; New types are created with the DEFINE-TYPE operator
  (define-type Point3D (Point3D Integer Integer Integer))

  ;; Coalton supports sum types
  (define-type Color
    Red
    Blue
    Green)

  ;; Coalton supports generic type variables
  ;;
  ;; Type parameters are defined using keyword arguments
  (define-type (Tree :a)
    (Branch (Tree :a) :a (Tree :a))
    (Leaf :a)))
```

Type definitions introduce type constructors. For example, we may construct a somewhat festive tree as follows
```lisp
(coalton
  (Branch (Leaf Red)
          Green
          (Branch (Leaf Red) Green (Leaf Red))))
```

We'll see how to unpack these types using `match` later in this document.

## Numbers

Coalton supports a few numeric types. The main ones are `Integer`, `Single-Float`, and `Double-Float`.

```lisp
(coalton-toplevel
  (define num-int 5)
  (define num-sf  5.0f0)
  (define num-df  5.0d0))
```

One can leave off the suffix and just write `5.0`, which will be resolved depending on `cl:*read-default-float-format*`, which is typically `cl:single-float` (meaning unadorned floats will be single-precision).

Numbers implement the `Num` typeclass, which has methods `+`, `-`, `*`, and `fromInt`.


## Lists

Coalton has a list data type defined as

```lisp
(coalton-toplevel
  (define-type (List :a)
    (Cons :a (List :a))
    Nil))
```

Coalton lists are not Lisp lists.


Coalton also has a syntactic shorthand for constructing lists called `make-list`.

```lisp
(coalton-toplevel
  (define x (make-list 1 2 3))
  (define y (make-list "a" "b" "c")))
```


Lists must be homogeneous. This means the following produces a type error.

```
COALTON-USER> (coalton-toplevel
                (define wut (make-list 1 2 3.0)))

Failed to unify types SINGLE-FLOAT and INTEGER
in unification of types (INTEGER → (LIST SINGLE-FLOAT) → :A) and (:B → (LIST :B) → (LIST :B))
in definition of WUT
in COALTON-TOPLEVEL
   [Condition of type COALTON-IMPL/TYPECHECKER::COALTON-TYPE-ERROR-CONTEXT]
```

## Static Typing

Coalton code is statically typechecked and types are inferred.

```lisp
(coalton-toplevel
  (define (fun x)
    (map (+ 2) (parse-int x))))
```

The type of a variable or function can be checked with `coalton:type-of`.

```
COALTON-USER> (type-of 'fun)
(STRING -> (OPTIONAL INT)
```

Type annotations can always be added manually

```lisp
(coalton-toplevel
  (declare fun (String -> (Optional Integer)))
  (define (fun x)
    (map (+ 2) (parse-int x))))
```

## Match expressions

Match expressions can be used to pattern-match and deconstruct algebraic data types:

```lisp
(coalton-toplevel
  (define-type Color
    Red
    Blue
    Green)

  ;; Constructors must be wrapped in parenthesies
  (declare color-to-string (Color -> String))
  (define (color-to-string c)
    (match c
      ((Red) "Red")
      ((Blue) "Blue")
      ((Green) "Green")))

  ;; Variables are not wrapped in parenthesies
  (declare map-optional ((:a -> :b) -> (Optional :a) -> (Optional :b)))
  (define (map-optional f x)
    (match x
      ((Some x_) (Some (f x_)))
      ((None) None)))

  ;; Patterns can be nested. And wildcard "_" patterns are supported.
  (declare flatten-optional ((Optional (Optional :a)) -> (Optional :a)))
  (define (flatten-optional x)
    (match x
      ((Some (Some x_)) (Some x_))
      (_ None)))


  ;; Literal values can also be matched on
  (define (is-5-or-7 x)
    (match x
      (5 True)
      (7 True)
      (_ False))))
```

The operator `coalton-library:if` can be used as a shorthand when matching on Booleans:

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

Several `if` expressions can be combined with a `cond`:

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
      (True (show n)))))
```

The Boolean operators `and` and `or` (of `coalton-library`) are actually variadic macros that short-circuit. Their functional counterparts are `boolean-and` and `boolean-or`.

```lisp
(coalton
  (or (cheap 5) True (really-expensive (expt 2 1000000))))
```

In this case, `really-expensive` will never get called due to short-circuiting. Also note that both `and` and `or` can take any number of arguments, including zero.

Coalton also has `unless` and `when` (of `coalton-library`) which work similary to their definitions in Lisp. We recommend only using these operators for conditionalizing stateful operations.

```lisp
(coalton-toplevel
  (define (f x)
    (when (== x 5)
      (error "I only want the number 5"))))
```

## `COALTON-LIBRARY:PROGN`

Coalton has a `coalton-library:progn` construct similar to lisp.

```lisp
(coalton-toplevel
  (declare f (Integer -> Integer -> (Tuple Integer Integer)))
  (define (f x y)
    (progn
      (+ x y)
      (* x y)
      (Tuple x y))))
```

Coalton's `progn` can have flattened `let` syntax.

```lisp
(coalton-toplevel
 (declare f (Integer -> Integer -> String))
  (define (f x y)
    (progn
      (let x_ = (show x))
      (let y_ = (show y))
      (concat-string x_ y_))))
```

## Typeclasses

Coalton supports typeclasses.

Currently, *all* member functions must be defined for each typeclass instance.

```lisp
(coalton-toplevel
  ;; Type classes are defined with the define-class keyword
  (define-class (Eq :a)
    (== (:a -> :a -> Boolean))
    (/= (:a -> :a -> Boolean)))

  (define-type Color
    Red
    Green
    Blue)

  ;; Type class instance are defined with the define-instance keyword
  (define-instance (Eq Color)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Red) (Red)) True)
        ((Tuple (Blue) (Blue)) True)
        ((Tuple (Green) (Green)) True)
        (_ False)))
    (define (/= a b) (not (== a b))))


  ;; Type declerations can have constraints
  (declare is-eql (Eq :a => (:a -> :a -> String)))
  (define (is-eql a b)
    (if (== a b)
      "They are equal"
      "They are not equal"))

  ;; Multiple constraints must be wrapped in parenthesies
  (declare double-is-eql ((Eq :a) (Eq :b) => (:a -> :a -> :b -> :b -> String)))
  (define (double-is-eql a b c d)
    (if (and (== a b) (== c d))
      "Both pairs are equal"
      "The pairs are not both equal")))
```


## Builtin Type Classes

The following are the main type classes defined in the standard library.

* `Eq` - defined on types that are comparable
* `Ord` - defined on types that are orderable
* `Num` - defined on types that are numeric

* `Semigroup` - defined on types which support an associative binary operation
* `Monoid` - defined on types that are semigroups and have an identiy element


The following type classes are similar to the typeclasses with the same name in Haskell with a few differences.

* `Functor` - `fmap` is just `map` in Coalton
* `Applicative`
* `Monad` - monad does not have `return`, use `pure` from applicative instead
* `Alternative` - `<|>` is called `alt` in Coalton

These type classes are inspired by traits of the same name in Rust:

* `Into` - total conversions between one type and another
* `TryInto` - non total conversions between one type and another

## Do Notation

Coalton has a do-notation macro that works similary to do notation in Haskell.

```lisp
(coalton-toplevel
  (define (f ax bx)
    (do
      (a <- ax)
      (b <- bx)
      (let c = (+ a b))
      (pure c)))

    ;; [6+3, 5+3, 5+3, 6+2, 5+2, 4+2, 6+1, 5+1, 4+1]
   (define xs (f (make-list 1 2 3) (make-list 4 5 6))))
```

## Inline Type Annotations

Inline type annotations can be added to resolve ambiguities when using type classes.

```lisp
(coalton-toplevel
  (define f (the U32 (+ (fromInt 5) (fromInt 7)))))
```

## Shorthand Function Call Syntax

Coalton does not have nullary functions. However, a function with the type signature `Unit -> *` can be called in Coalton without explicitly passing `Unit`. For instance, the Coalton form `(make-vector)` is a shorthand for `(make-vector Unit)`.

## Inspecting the Coalton System

The `coalton` package defines several debugging functions.


Type-of and kind-of can be used to inspect the types and kinds of definitions.
```
COALTON-USER> (type-of 'map)
∀ :A :B :C. FUNCTOR :C ⇒ ((:A → :B) → (:C :A) → (:C :B))
COALTON-USER> (kind-of 'Result)
* -> (* -> *)
```

The following functions all take an optional package parameter 

* `print-type-db` - Print every known type
* `print-value-db` - Print the type of every toplevel value
* `print-class-db` - Print every class and their methods
* `print-instance-db` - Print the instances of every class
