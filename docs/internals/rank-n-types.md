# Rank-$N$ polymorphism in Coalton

This document describes how arbitrary-rank (Rank-$N$) polymorphism is represented,
typechecked, and compiled in Coalton, as well as the design tradeoffs chosen
and lessons learned from GHC Haskell.

---

## 1. Overview and goals

Coalton's core type system is based on Hindley–Milner (Rank-1 prenex
polymorphism) extended with typeclasses, functional dependencies, multi-arity
functions, keyword arguments, and multi-value (`Values`) returns.

In a pure Rank-1 system, `forall` quantifiers can only live at the outermost
level of a top-level `ty-scheme`. That prevents expressing APIs where a callee
must instantiate a function argument at types chosen *internally* by the callee,
such as:

- Forward-mode automatic differentiation (instantiating the callback at `AD F32`):
  ```lisp
  (declare differentiate ((forall (:a) (Num :a => (:a -> :a))) -> (F32 -> F32)))
  (define (differentiate f)
    (fn (x)
      (coalton/xmath/dual:d-x f x)))
  ```
- Scoped resource/state tokens (`runST`-style phantom state):
  ```lisp
  (declare run-st ((forall (:s) (ST :s :a)) -> :a))
  ```
- Polymorphic record/ADT fields and natural transformations (`forall (:a) ((:f :a) -> (:g :a))`).

Unannotated type inference for Rank-3 and higher is undecidable (Wells, 1999),
and unannotated Rank-2 inference lacks principal types in the presence of
typeclasses. Coalton therefore follows the standard bidirectional approach
(*Practical Type Inference for Arbitrary-Rank Types*, Peyton Jones et al., 2007):
unannotated code infers Rank-1 types as before, while explicit `forall`
annotations enable arbitrary-rank types via top-down type checking.

---

## 2. Representation in the compiler

### 2.1 Parser (`src/parser/types.lisp`, `collect.lisp`, `renamer.lisp`)

`parser:qualified-ty` inherits from `parser:ty` (`(:include ty)`), allowing
`(forall (...) ...)` in any syntactic type position (`parse-type` and
`parse-type-list`), including function inputs/outputs, constructor/struct
fields, type aliases, and `the` annotations.

To avoid scoping ambiguity, nested qualified types (`Pred => Type`) must be
enclosed in an explicit `(forall (...) ...)`.

In `collect-type-variables`, a `parser:qualified-ty` subtracts its own
`explicit-variables` so nested `forall` binders are not mistaken for free type
variables of an enclosing declaration. Similarly, `rename-type-variables`
alpha-renames `explicit-variables` within the lexical scope of each nested
`qualified-ty`.

### 2.2 Typechecker (`src/typechecker/types.lisp`, `predicate.lisp`, `scheme.lisp`)

Rather than reusing `ty-scheme` (whose `tgen` indices are flat 0-based indices
that would collide if nested), nested polytypes are represented by a dedicated
`tc:forall-ty` structure (`(:include ty)`) in `src/typechecker/predicate.lisp`:

- `explicit-p`: boolean indicating explicit quantification.
- `binders`: list of `tc:tyvar`s bound by this `forall-ty`.
- `type`: a `tc:qualified-ty` holding the `predicates` and body `ty`.

To support checking against `forall-ty`, `tc:tyvar` carries a boolean slot
`rigid-p`:

- **Flexible unification variables (`:rigid-p nil`):** Created during ordinary
  type inference and when instantiating a polymorphic value at a use site.
- **Rigid skolem constants (`:rigid-p t`):** Created by `make-skolem` /
  `(instantiate-forall-ty ... :rigid-p t)` when *checking* an expression against
  an expected `forall-ty`.

Representing skolems as `tyvar`s with `:rigid-p t` means every existing pass in
the compiler (`tc-env-typevar-table`, pretty-printing, kind inference, variance
analysis, class entailment, `lisp-type`) works on skolems without duplication,
while `unify.lisp` and `scheme.lisp` enforce rigidity:

- `bind-variable` and `match` in `src/typechecker/unify.lisp` refuse to bind a
  rigid `tyvar` to anything other than itself (flexible variables may bind *to*
  a rigid `tyvar`, orienting the substitution `flex -> skolem`).
- `quantify` in `src/typechecker/scheme.lisp` never quantifies over rigid
  `tyvar`s.

---

## 3. Bidirectional checking and escape detection

### 3.1 Skolemization (`check-forall-expression-type` in `src/typechecker/define.lisp`)

`infer-expression-type (node expected-type subs env)` threads `expected-type`
top-down. Its `:around` method intercepts any expression whose substituted
`expected-type` is a `tc:forall-ty` (except transparent wrappers `node-body`,
`node-block`, and `node-the`, which forward `expected-type` to their inner
expressions) and invokes `check-forall-expression-type`:

1. **Watermark snapshot:** Records `watermark = *next-variable-id*` before
   skolemization.
2. **Skolemize:** Instantiates the `forall-ty` binders with fresh rigid skolems
   (`:rigid-p t`), yielding `given-preds` and `skolem-body-ty`, and extends
   `tc-env`'s lexical `typevar-table` with those skolems.
3. **Check body:** Recursively checks `node` against `skolem-body-ty`.
4. **Local fundep & accessor solving:** Solves functional dependencies (`solve-fundeps`
   and `fundep-entail`) and struct accessors using `given-preds`.
5. **Watermark skolem-escape check:** Verifies that no substitution in `subs`
   whose source variable ID is `< watermark` maps to a type mentioning any of
   the local `skolems`, and that no outer environment variable mentions any
   local `skolem`. Because `subs` is purely functional and passed speculatively
   into `handler-case` blocks across the typechecker, watermark ID checking
   avoids the backtracking bugs of mutable `tyvar` level fields.
6. **Local predicate entailment:** Discharges `wanted-preds` entailed by
   `given-preds` via `tc:entail`. Any remaining wanted predicate that still
   mentions a local `skolem` is reported immediately as a missing predicate error.
7. **Value restriction check:** If `given-preds` is empty and `node` is
   syntactically expansive (`(not (nonexpansive-expression-p node ...))`),
   verifies that every `skolem` occurs only covariantly in `skolem-body-ty`.
8. **Evidence wrapper:** Wraps the checked AST node with `given-preds` via
   `wrap-expression-with-given-predicates` and relaxes the local `skolems` back
   to fresh flexible variables for downstream codegen passes.

### 3.2 Two-pass application checking

In `parser:node-application`, arguments whose expected parameter type is *not* a
`forall-ty` are inferred in a first pass, and `forall-ty` arguments are checked
in a second pass. This ensures that outer type variables shared between ordinary
arguments and higher-rank arguments are determined before `check-forall-expression-type`
discharges predicates on the higher-rank arguments.

---

## 4. Dictionary passing and codegen

In Coalton, a constrained function `(Num :a => :a -> :a)` compiles to an
uncurried Common Lisp function taking its dictionary parameters followed by its
value parameters: `(lambda (dict x) ...)`.

When checking a higher-rank expression against `(forall (:a) (Num :a => (:a -> :a)))`:

- If the expression is already a lambda literal `(fn (x) (* x x))`,
  `wrap-expression-with-given-predicates` attaches `given-preds` directly to the
  `node-abstraction`, which `translate-expression` compiles to
  `(lambda (dict x) (* dict x x))`.
- If the expression is not a lambda literal (for example, a variable `square` or
  a call returning a function), `wrap-expression-with-given-predicates` wraps it
  in a dictionary-binding `node-abstraction`, and `translate-expression`
  eta-expands it across the visible positional and keyword parameters using
  `make-keyword-forwarders` so the resulting closure has the exact uncurried
  physical signature `(Dict * :a -> :a)` expected by call sites.

Post-translation passes (`typecheck-node`, `inliner`, `ast-substitutions`) use
`relax-codegen-type` to view `forall-ty` values as their physical
dictionary-passing function types.

### 4.1 Inlining, dictionary elimination, and scalar replacement

Two optimizer enhancements in `src/codegen/inliner.lisp` and
`src/codegen/lawnmower.lisp` ensure that `(inline)` higher-rank functions can
compile with zero overhead:

1. **Unwrapping hoisted dictionary variables (`extract-dict` in `inliner.lisp`):**
   When a higher-rank function like `differentiate` constructs a parameterized
   dictionary such as the `Num` instance for `AD F32`,
   `static-dict-lift` hoists that dictionary into a top-level `hoisted_N`
   binding. When `differentiate` is later inlined at a call site, `extract-dict`
   resolves `hoisted_N` via `tc:lookup-code` so `inline-method` and
   `inline-direct-method` can still inline the `Dual` methods and underlying
   `F32` arithmetic methods.
2. **Type-specializing beta-reduced lambdas (`specialize-inlined-abstraction` in `lawnmower.lisp`):**
   When a polymorphic `node-abstraction` argument is substituted into a call
   site and beta-reduced by the lawnmower, its parameter and body types are
   unified with the call-site operand types (`AD F32`, transparent over
   `Dual F32`), allowing downstream method inlining and scalar replacement
   to eliminate intermediate `Dual` allocations completely.

---

## 5. Comparison with GHC Haskell & mistakes avoided

### 5.1 Strictly predicative (avoiding `ImpredicativeTypes`)

GHC spent more than fifteen years attempting to support impredicative
instantiation (binding unification variables to `forall` types) through a
sequence of complex extensions (*Boxy Types* in 2006, *FPH* in 2008, a broken
`ImpredicativeTypes` for over a decade, and finally *QuickLook* in 2020).
Impredicativity makes unification order-dependent and fragile because a single
unification variable `?a` does not know whether to instantiate or preserve a
polytype.

Coalton avoids this entirely:
- `bind-variable` and `match` in `src/typechecker/unify.lisp` reject binding any
  `tyvar` to a `forall-ty`.
- `parse-type` and `check-no-impredicative-forall-types` reject `forall-ty`
  inside `tapp` (type constructor arguments), `result-ty` (multi-value packs),
  and `ty-predicate` arguments, including after `define-type-alias` expansion.
- Users who want a container of polymorphic functions wrap the `forall` in a
  nominal `define-type` or `define-struct`, making introduction and elimination
  explicit and zero-ambiguity.

### 5.2 Simplified subsumption (avoiding deep subsumption)

Prior to GHC 9.0, GHC implemented *deep subsumption*: the typechecker
contravariantly/covariantly compared function types and silently eta-expanded
functions to reorder quantifiers deep inside arrows (for example, coercing
`Int -> forall a. a -> a` into `forall a. Int -> a -> a` by synthesizing
`\x a -> f x a`).

In a strict language like Coalton—which also has multi-arity functions
(`A * B -> C` vs. `A -> B -> C`), optional keyword arguments, and multi-value
returns—silent deep eta-expansion is harmful:
1. It changes runtime evaluation order and side-effect timing when a function
   call is hidden inside a synthesized lambda.
2. It allocates wrapper closures implicitly and breaks multi-arity calling
   conventions.

GHC 9.0 eventually removed deep subsumption in favor of *Simplified Subsumption*
at the cost of breaking existing Haskell code. Coalton uses simplified
subsumption from the start: skolemization and instantiation happen only at the
top level of a checked expression, while nested `forall-ty`s inside compound
types must match invariant alpha-equivalence (`mgu-forall-types`).

### 5.3 Explicit quantification on nested contexts

Haskell permits signatures like `(Num a => a -> a) -> Int` without an inner
`forall`, which creates visual and syntactic ambiguity about whether `a` is
scoped to the inner argument or the outer declaration. Coalton requires an
explicit `forall` whenever `=>` appears in a nested type position:
`((forall (:a) (Num :a => (:a -> :a))) -> Integer)`.

### 5.4 Value restriction on unconstrained higher-rank expressions

Because Haskell is pure, any expression can be generalized to a `forall` type.
In strict ML-family languages with mutable references (`Cell`, `Vector`),
allowing an expansive allocation like `(cell:new (fn (x) x))` to be checked
against `(forall (:a) (Cell (:a -> :a)))` is unsound. Coalton enforces its
relaxed value restriction (`nonexpansive-expression-p` + `collect-tyvar-variances`)
inside `check-forall-expression-type`, ensuring Rank-$N$ polymorphism remains
sound in the presence of mutation.
