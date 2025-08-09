<p align="center">
  <a href="https://coalton-lang.github.io/">
    <img alt="Coalton" src="docs/assets/coalton-logotype-gray.svg" style="zoom:45%;" />
  </a>
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

Coalton integrates directly into Common Lisp:

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

It also works directly in the REPL:

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

```
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

Coalton is currently used in production to build defense and [quantum computing software](https://coalton-lang.github.io/20220906-quantum-compiler/).

## Getting Started

> [!WARNING]
> Coalton has **not** reached "1.0" yet. This means that, from time to time, you may have a substandard user experience. While we try to be ANSI-conforming, Coalton is currently only tested on recent versions of SBCL, Allegro CL, and Clozure CL.
>
> Coalton will **not** be in Quicklisp until it reaches its first stable version.

**Prepare**: Install [SBCL](http://www.sbcl.org/platform-table.html) (on macOS with Homebrew: `brew install sbcl`). Install Quicklisp by following instructions [here](https://www.quicklisp.org/beta/#installation). (The step command involving `gpg` is not needed.) After installing Quicklisp, you should have a `quicklisp` folder which will make installing Coalton easier.

**Install**: Clone this repository into a place your Lisp can see (e.g., `~/quicklisp/local-projects/`).

**Use**: Either run `(ql:quickload :coalton)`, or add `#:coalton` to your ASD's `:depends-on` list.

**Test**: Compile the tests with `(ql:quickload :coalton/tests)`, then run the tests with `(asdf:test-system :coalton)`. 

> [!NOTE] 
> Running the Coalton test suite on SBCL requires [GNU MPFR](https://www.mpfr.org/mpfr-current/mpfr.html#Installing-MPFR) in order to run `Big-Float` tests. If you would like to run tests without installing `gnu-mpfr`, you can use Coalton's portable `Big-Float` implementation by running `(pushnew :coalton-portable-bigfloat *features*)` before loading Coalton.

**Learn**: Start with [*Intro to Coalton*](docs/intro-to-coalton.md) and the [standard library reference](https://coalton-lang.github.io/reference/), and then take a peek at the [examples directory](examples/). It may also be helpful to check out the [introductory blog post](https://coalton-lang.github.io/20211010-introducing-coalton/).

## What's Here?

This repository contains the source code to the [Coalton compiler](src/), and the [standard library](library/).

It also contains a few example programs, such as:

- Some [simple pedagogical programs](examples/small-coalton-programs/),
- An [implementation](examples/thih/) of Jones's *Typing Haskell in Haskell*, and
- An [implementation](examples/quil-coalton/) of a simple [Quil](https://en.wikipedia.org/wiki/Quil_(instruction_set_architecture)) parser using parser combinators.

Lastly and importantly, we maintain a collection of documentation about Coalton in the [docs](docs/) directory.

## Get Involved

Want to ask a question about Coalton, propose a feature, or share a cool program you wrote? Try posting in the [GitHub Discussions](https://github.com/coalton-lang/coalton/discussions) page!

We welcome contributions of all forms, especially as we stabilize toward a 1.0 release. We would be grateful to receive:

- bug reports (filed as issues),
- bug fixes and typo corrections (filed as pull requests),
- small [example programs](examples/small-coalton-programs/), and
- user experience troubles.
