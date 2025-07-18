# A guide to Documentation in Coalton

There are a number of ways to embed documentation in Coalton code, some with more utility than others. This document will explain each documentation feature.

## Documentation strings in primary Coalton forms

### `define`
Coalton's `define` form allows for docstrings to go after the variable or function, and before the actual definition.

```
(coalton-toplevel

  (define *pi* (the Double-Float math:pi)
    "This is a constant of `pi`, that will always be a `Double-Float`.")

  (define (get-pi)
    "This is a function that returns the `Double-Float` constant `*pi*`."
    *pi*))
```

### `define-type`

`define-type` allows for the same docstring style as `define`, with the addition of constructor docstrings:

```
(coalton-toplevel
  (define-type Pie
    "A pie is a baked dish which is usually made of a pastry dough casing
that contains a filling of various sweet or savoury ingredients."

    Empty-Shell    "An empty pie crust sans filling."
    (Fruit String) "A fruit pie with the type of fruit."
    (Meat String)  "A meat pie with the type of meat."))
```

### `define-type-alias`

`define-type-alias` allows for the same docstring style as `define`.

```lisp
(coalton-toplevel
  (define-type-alias Index Integer
    "This is a type alias for a discrete numeric type: INTEGER"))
```

### `define-class`

`define-class` allows for documentation both on the main form and in each method.

```
(coalton-toplevel
  (define-class (Bakeable :a)
    "A type class with methods for baking desserts"
    
    (temperature
     "What temperature should this be baked at?"
     (:a -> UFix))
     
    (bake-time
     "How long will it take to bake an item of type :a given its size?"
     (:a -> UFix -> UFix)))
     
  (define-instance (Bakeable Pie)
     "An instance of Bakeable for baking pies."
     (define (temperature p)
       "What temperature should a pie be baked at?"
       425)
     (define (bake-time p)
       "How long should it be baked for?"
       65)))
```

> [!TIP]
> Docstrings on instances are really only useful for local use. 
>
> They are not incorporated into documentation generation, but they can be good practice for readable code.

### `define-struct`

Coalton `Struct`s can feature docstrings on both the overall definition and for each individual field.

```
(coalton-toplevel
  (define-struct Dessert
    "A dessert item for human consumption."
    (Sweet?    "Is this item sweet?"                 Boolean)
    (Calories  "How many calories are in a serving?" UFix)
    (Allergens "What allergens might this contain?"  (List String))))
```

> [!TIP]
> Up until this point, every type of docstring featured will be automatically added to Lisp's [internal symbol documentation](#accessing-symbol-documentation). 
>
> Due to implementation particulars, struct field accessors do not pass any information to symbol documentation.
>
> They do, however, show up in Coalton's own documentation generation.

### `package`

Coalton's native `package` form also takes a documentation string after the package name:

```
(package sucre-bleu
  "A suite of tools for sweet chefs."
  (import coalton-prelude)
  (export *pi*
          get-pi
          Pie
          Bakeable
          temperature
          bake-time
          Dessert))
```

## Semicolon Comments

Finally, we have comments, which carry the least amount of information through the program.

Comments are useful for local information that doesn't need to be accessed publicly.

```
;;;; factorial.lisp

;;;
;;; The Factorial Function
;;;

(coalton-toplevel

  (define (factorial n)
    ;; this needs to be a cell because product needs to be mutable
    (let product = (cell:new 1))

    (for i in (iter:range-increasing 1 1 (1+ n)) ; this iterates from 1 to n
      (cell:update! (* i) product))              ; and updates the product accordingly

    (cell:read product)))
```

The Coalton development team follows standard lisp comment style conventions, as seen in the [hyperspec](https://www.lispworks.com/documentation/HyperSpec/Body/02_ddb.htm).

## Appendix

### Accessing Symbol Documentation

Assuming you are editing Coalton using Emacs with Slime, you can access documentation for a variety of defined symbols using `slime-documentation`, `slime-inspect-presentation-at-point`, or `slime-inspect`. Each of these have 

If you are not using slime (or emacs), you can use Lisp's native `(cl:describe '<symbol>)` to pull up documentation.

### Backticks

You may have noticed the backticks around certain symbols in these docstrings. 

Backticks help symbols format correctly in documentation generation.

