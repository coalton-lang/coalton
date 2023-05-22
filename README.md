<p align="center">
  <img src="docs/assets/coalton-logotype-gray.svg" style="zoom:45%;" />
</p>

<p align="center" class="badges">
  <a href="https://github.com/coalton-lang/coalton/actions/workflows/main.yml">
    <img alt="Github Workflow Status" src="https://img.shields.io/github/actions/workflow/status/coalton-lang/coalton/main.yml?branch=main" />
  </a>
  <a href="https://discord.gg/cPb6Bc4xAH">
    <img alt="Discord" src="https://img.shields.io/discord/888196168067199046?logo=discord" />
  </a>
</p>

Coalton is an efficient, statically typed functional programming language that supercharges Common Lisp.

Coalton can be written in files:

```lisp
(in-package #:coalton-user)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;; Define Coalton `Symbol`s as Lisp `cl:keyword`s.
  (repr :native cl:keyword)
  (define-type Symbol)

  ;; Bind a Lisp function into Coalton.
  (declare sym (String -> Symbol))
  (define (sym s)
    "Create a new symbol named `s`."
    (lisp Symbol (s)
      (cl:intern s "KEYWORD")))

  ;; Define equality of `Symbol` types using CL's `eq`.
  (define-instance (Eq Symbol)
    (define (== a b)
      (lisp Boolean (a b)
        (cl:eq a b))))

  ;; Define a new parametric algebraic data type for simple
  ;; mathematical expressions.
  (define-type (Expr :t)
    "A symbolic expression of basic arithmetic."
    (EConst :t)
    (EVar   Symbol)
    (E+     (Expr :t) (Expr :t))
    (E*     (Expr :t) (Expr :t)))

  ;; The classic `diff` function, in Coalton.
  (declare diff (Num :t => Symbol -> Expr :t -> Expr :t))
  (define (diff x f)
    "Compute the derivative of `f` with respect to `x`."
    (match f
      ((EConst _)                       ; c' = 0
       (EConst 0))
      ((EVar s)                         ; x' = 1
       (if (== s x) (EConst 1) (EConst 0)))
      ((E+ a b)                         ; (a+b)' = a' + b'
       (E+ (diff x a) (diff x b)))
      ((E* a b)                         ; (ab)' = a'b + ab'
       (E+ (E* (diff x a) b)
           (E* a          (diff x b))))))

  ;; We can use `t` just fine since Coalton doesn't import `cl:t`.
  (define t (sym "t"))

  (declare dt (Num :t => Expr :t -> Expr :t))
  (define dt
    "The time derivative operator."
    (diff t)))
```

And at the REPL:

```lisp
CL-USER> (in-package #:coalton-user)
COALTON-USER> (coalton-toplevel
                (define (square x) (E* x x)))
; No value
COALTON-USER> (coalton (dt (E+ (square (EVar t)) (EConst 1))))
#.(E+ #.(E+ #.(E* #.(ECONST 1) #.(EVAR |t|))
            #.(E* #.(EVAR |t|) #.(ECONST 1)))
      #.(ECONST 0))
```

Type errors are discovered at compile-time, and errors are printed beautifully without sacrificing Common Lisp's interactive debugging facilities.

```lisp
COALTON-USER> (coalton (dt (E+ (EConst 1/2) (EConst 0.5))))
error: Type mismatch
  --> <unknown>:1:30
   |
 1 |  (coalton (dt (E+ (EConst 1/2) (EConst 0.5))))
   |                                ^^^^^^^^^^^^ Expected type '(EXPR FRACTION)' but got type '(EXPR DOUBLE-FLOAT)'
   [Condition of type COALTON-IMPL/TYPECHECKER/BASE:TC-ERROR]

Restarts:
 0: [RETRY] Retry REPL evaluation request.
 1: [*ABORT] Return to top level.
 2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {10013A8003}>)
```

*Coalton has **not** reached "1.0" yet. This means that, from time to time, you may have a substandard user experience. While we try to be ANSI-conforming, Coalton is currently only tested on SBCL 2.2.x, Allegro CL 10.1.* and Clozure CL 1.21.*.

## Getting Started

**Prepare**: Install [SBCL](http://www.sbcl.org/platform-table.html) (on macOS with Homebrew: `brew install sbcl`). Install Quicklisp by following instructions [here](https://www.quicklisp.org/beta/#installation). (The step command involving `gpg` is not needed.) After installing Quicklisp, you should have a `quicklisp` folder which will make installing Coalton easier.

**Install**: Clone this repository into a place your Lisp can see (e.g., `~/quicklisp/local-projects/`). Coalton is not yet on Quicklisp.

**Use**: Either run `(ql:quickload :coalton)`, or add `#:coalton` to your ASD's `:depends-on` list. Quicklisp will automatically download all of Coalton's dependencies.

**Test**: Compile the tests with `(ql:quickload :coalton/tests)`, then run the tests with `(asdf:test-system :coalton)`.

**Learn**: We recommend starting with the [*Intro to Coalton*](docs/intro-to-coalton.md) document, and then taking a peek in the [examples directory](examples/). It may also be helpful to check out the [introductory blog post](https://coalton-lang.github.io/20211010-introducing-coalton/).

## What's Here?

This repository contains the source code to the [Coalton compiler](src/), and the [standard library](library/).

It also contains a few example programs, such as:

- Some [simple pedagogical programs](examples/small-coalton-programs/),
- An [implementation](examples/thih/) of Jones's *Typing Haskell in Haskell*, and
- An [implementation](examples/quil-coalton/) of a simple [Quil](https://en.wikipedia.org/wiki/Quil_(instruction_set_architecture)) parser using parser combinators.

Lastly and importantly, we maintain a collection of documentation about Coalton in the [docs](docs/) directory, including a [standard library reference guide](https://coalton-lang.github.io/reference/).

## Get Involved

Want to ask a question about Coalton, propose a feature, or share a cool program you wrote? Try posting in the [GitHub Discussions](https://github.com/coalton-lang/coalton/discussions) page!

We welcome contributions of all forms, especially as we stabilize toward a 1.0 release. We would be grateful to receive:

- bug reports (filed as issues),
- bug fixes and typo corrections (filed as pull requests),
- small [example programs](examples/small-coalton-programs/), and
- user experience troubles.
