<img src="docs/assets/coalton-logo.png" style="zoom:50%;" />

Coalton is an efficient, statically typed functional programming language that supercharges Common Lisp.

```lisp
(in-package #:coalton-user)

(coalton-toplevel
  (define-type Symbol
    (Symbol String))

  (define (symbol-name sym)
    (match sym
      ((Symbol s) s)))

  (define-instance (Eq Symbol)
    (define (== a b)
      (== (symbol-name a) (symbol-name b)))
    (define (/= a b)
      (not (== a b))))
  
  (define-type Expr
    (EConst Int)
    (EVar   Symbol)
    (E+     Expr Expr)
    (E*     Expr Expr))

  (declare diff (Symbol -> Expr -> Expr))
  (define (diff x f)
    (match f
      ((EConst _)
       (EConst 0))
      ((EVar s)
       (if (== s x) (EConst 1) (EConst 0)))
      ((E+ a b)
       (E+ (diff x a) (diff x b)))
      ((E* a b)
       (E+ (E* (diff x a) b)
           (E* a          (diff x b)))))))
```

Unlike similar languages, which may force mutability or purity, Coalton embraces the spirit of Common Lisp and adapts to your programming style.

## What's Here?

This repository contains the source code to:

- the [type checker](src/typechecker/),
- the [compiler](src/codegen/), and
- the [standard library](src/library/).

It also contains a few example programs, such as:

- Some [simple pedagogical programs](examples/small-coalton-programs/),
- An [implementation](examples/thih/) of Jones's *Typing Haskell in Haskell*, and
- An [implementation](examples/quil-coalton/) of a simple Quil parser.

Lastly and importantly, we maintain a collection of documentation about Coalton in the [docs](docs/) directory.

## Getting Started

**Install**: clone this repository into a place your Lisp can see. (Coalton is **not yet** on Quicklisp!)

**Use**: Either run `(ql:quickload :coalton)`, or add `#:coalton` to your ASD's `:depends-on` list.

**Learn**: We recommend starting with the [*Intro to Coalton*](docs/intro-to-coalton.md), and then taking a look at some of the example programs in the [examples directory](examples/). 

## Contributing

We welcome contributions of all forms, especially as we stabilize toward a 1.0 release. We would be grateful to receive:

- bug reports (filed as issues)
- bug fixes and typo corrections (filed as pull requests)
- user experience troubles and reports.



