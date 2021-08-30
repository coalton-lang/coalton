# Intro to Coalton

Coalton is a statically typed language that compiles to common lisp.

## Basics

```lisp
(coalton-toplevel
  ;; Variables are defined with the define keyword
  (define x 5)
  (define y 6)
  (define z (+ x y))

  ;; Coalton supports integers, strings, booleans, and unit as primitive types
  (define name "Alyssa P. Hacker")
  (define hungry True)
  (define data Unit))
```

## Functions

```lisp
(coalton-toplevel
  ;; Functions are also defined with the define keyword
  (define (add2 x)
    (+ 2 x))

  ;; Functions exist in the same namespace as variables
  (define addTwo add2)

  (define x (addTwo 3)))
```

```lisp
(coalton-toplevel
  ;; Lists can be created with the make-list macro
  (define nums (make-list 2 3 4 5))

  ;; Functions in coalton are curried
  (map (+ 2) nums)) ;; 4 5 6 7
```

## Data Types

```lisp
(coalton-toplevel
  ;; New types are created with the define-type keyword
  (define-type Point3D (Pont3D Int Int Int))

  ;; Coalton supports sum types
  (define-type Color
    Red
    Blue
    Green)

  ;; Coalton supports generic types
  ;; Type paramaters are defined using keyword arguments
  (define-type (Tree :a)
    (Branch (Tree :a) :a (Tree :a))
    (Leaf :a)))
```

## Lists

Coalton has a list defined as

```lisp
(coalton-toplevel
 (define-type (List :a)
   (Cons :a)
   Nil))
```

Coalton also has a shorthand for constructing lists called `make-list`.

```lisp
(coalton-toplevel
  (define x (make-list 1 2 3))
  (define y (make-list "a" "b" "c")))
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
  (declare fun (String -> (Optional Int)))
  (define (fun x)
    (map (+ 2) (parse-int x))))
```

## Match expressions

Match expressions can be used to pattern match types

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

  (define-type (Optional :a) (Some :a) None)

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

If can be used as a shorthand when matching on booleans

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

Several If expressions can be combined with a cond

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

### Progn

Coalton has a progn construct similar to lisp.

```lisp
(coalton-toplevel
  (declare f (Int -> Int -> (Tuple Int Int)))
  (define (f x y)
    (progn
      (+ x y)
      (* x y)
      (Tuple x y))))
```

Coalton's progn can have let forms.

```lisp
(coalton-toplevel
 (declare f (Int -> Int -> String))
  (define (f x y)
    (progn
      (let x_ = (show x))
      (let y_ = (show y))
      (concat-string x_ y_))))

## Typeclasses

Coalton supports typeclasses.

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


## Builtin Typeclasses

* `Eq` - defined on types that are comparable
* `Ord` - defined on types that are orderable
* `Num` - defined on types that are numeric

* `Semigroup` - defined on types which support an associative binary operation
* `Monoid` - defined on types that are semigroups and have an identiy element

* `Functor`  
* `Applicative`
* `Monad`
* `MonadFail`
* `Alternative`

## Do Notation

Coalton has a do notation macro that works similary to do notation in Haskell.

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
