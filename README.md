<img src="docs/assets/coalton-logotype-gray.svg" style="zoom:50%;" />

[![Tests Status Badge](https://github.com/coalton-lang/coalton/actions/workflows/main.yml/badge.svg)](https://github.com/coalton-lang/coalton/actions/workflows/main.yml)

Coalton is an efficient, statically typed functional programming language that supercharges Common Lisp.

Coalton can be written in files:

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
    "A symbolic expression of basic arithmetic."
    (EConst Integer)
    (EVar   Symbol)
    (E+     Expr Expr)
    (E*     Expr Expr))

  (declare diff (Symbol -> Expr -> Expr))
  (define (diff x f)
    "Compute the derivative of F with respect to X."
    (match f
      ((EConst _)   ; c' = 0
       (EConst 0))
      ((EVar s)     ; x' = 1
       (if (== s x) (EConst 1) (EConst 0)))
      ((E+ a b)     ; (a+b)' = a' + b'
       (E+ (diff x a) (diff x b)))
      ((E* a b)     ; (ab)' = a'b + ab'
       (E+ (E* (diff x a) b)
           (E* a          (diff x b))))))

 (declare dt (Expr -> Expr))
 (define dt
   "The time derivative operator."
   (diff (Symbol "t"))))
```

And at the REPL:

```lisp
CL-USER> (in-package #:coalton-user)
COALTON-USER> (coalton (dt (E+ (EVar (Symbol "t"))
                               (EConst 1))))
#.(E+ #.(ECONST 1) #.(ECONST 0))
```

*Coalton has **not** reached "1.0" yet. This means that, from time to time, you may have a substandard user experience. While we try to be ANSI-conforming, Coalton may only work on SBCL 2.1.x.*

## What's Here?

This repository contains the source code to the [Coalton compiler](src/), and the [standard library](src/library/).

It also contains a few example programs, such as:

- Some [simple pedagogical programs](examples/small-coalton-programs/),
- A [JSON parser](examples/coalton-quil) piggybacking on a native Common Lisp library.
- An [implementation](examples/thih/) of Jones's *Typing Haskell in Haskell*, and
- An [implementation](examples/quil-coalton/) of a simple [Quil](https://en.wikipedia.org/wiki/Quil_(instruction_set_architecture)) parser.

Lastly and importantly, we maintain a collection of documentation about Coalton in the [docs](docs/) directory, including a [standard library reference guide](docs/reference.md).

## Getting Started

**Install**: Clone this repository into a place your Lisp can see. (Coalton is not yet on Quicklisp.)

**Use**: Either run `(ql:quickload :coalton)`, or add `#:coalton` to your ASD's `:depends-on` list.

**Test**: Run `(asdf:test-system :coalton)`.

**Learn**: We recommend starting with the [*Intro to Coalton*](docs/intro-to-coalton.md) document, and then taking a peek in the [examples directory](examples/). It may also be helpful to check out the [introductory blog post](https://coalton-lang.github.io/20211010-introducing-coalton/).

## Get Involved

Want to ask a question about Coalton, propose a feature, or share a cool program you wrote? Try posting in the [GitHub Discussions](https://github.com/coalton-lang/coalton/discussions) page!

We welcome contributions of all forms, especially as we stabilize toward a 1.0 release. We would be grateful to receive:

- bug reports (filed as issues),
- bug fixes and typo corrections (filed as pull requests),
- small [example programs](examples/small-coalton-programs/), and
- user experience troubles.

