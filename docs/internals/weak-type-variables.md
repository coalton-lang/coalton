# Weak Type Variables in Coalton

Coalton implements variance inference and weak type variables, but it does not
allow unresolved weak variables to appear in inferred top-level types. Coalton
does it this way to give early, direct guidance at the definition site and to
reduce REPL order sensitivity. OCaml likely does it their way because exposing
weak variables improves interactivity and preserves more inferred results.
In this document, we describe Coalton's behavior and what we could do to
recover OCaml-style behavior if we choose to.

## Current Coalton Behavior

Coalton implements the value restriction plus relaxed value restriction in the
typechecker.

Main code paths:

- `src/typechecker/define.lisp`
  - `nonexpansive-expression-p`
  - `weak-binding-type-variables`
  - `blocked-weak-type-variables`
  - `error-non-generalizable-binding`
- `src/typechecker/variance.lisp`
  - variance lattice and type-variable variance collection
- `src/typechecker/environment.lisp`
  - constructor/type-entry variance metadata used by the variance pass

For an expansive binding, type variables from its inferred type are treated as
weak candidates. These variables can be solved by unification, but are only
generalized if they are provably covariant and do not escape through retained
predicates.

Coalton's non-expansive check is intentionally syntactic:

- variables/literals/lambdas are non-expansive
- constructor applications can be non-expansive (with restrictions)
- ordinary function applications are expansive

This is conservative and follows the standard value-restriction safety story.

Coalton currently rejects unresolved weak variables at top level. Instead of
printing a weak variable type, it reports a user-facing error via
`error-non-generalizable-binding`. In other words, Coalton infers
weak-variable candidates internally, but does not
surface OCaml-style toplevel weak types as inferred results.

## Implementing OCaml's Weak Type Variables

OCaml allows toplevel expressions to infer types with weak variables and prints
them (for example, names like `_weak1`).

Operationally:

- weak vars are monomorphic placeholders
- first constraining use can fix them
- later incompatible use produces a type error

So OCaml exposes weak variables in interactive inference results, while Coalton
currently reports a non-generalizable-binding error at the definition site.

To match OCaml behavior, we would keep the same value-restriction analysis but
change representation, instantiation, and printing policy.

### 1. Representation changes

Current `ty-scheme` (`src/typechecker/scheme.lisp`) only stores quantified
kinds plus type. It has no explicit weak-variable set.

Add explicit weak-variable metadata to schemes (or add a parallel "weak scheme"
representation) so unresolved weak variables are preserved when placing a
binding into the environment.

### 2. Generalization policy changes

In `src/typechecker/define.lisp`, stop eagerly failing on unresolved weak vars
for interactive/toplevel inference paths where OCaml-style behavior is desired.

Keep existing expansive/non-expansive and variance logic. The change is policy
for unresolved weak vars, not removal of value restriction.

### 3. Instantiation changes

Instantiation should:

- freshen quantified variables as today
- keep weak variables shared per binding (not freshly generalized each use)

This preserves the "first use can fix the weak variable" behavior.

### 4. Pretty-printing changes

Extend printing (`src/typechecker/types.lisp`, `src/typechecker/scheme.lisp`)
to render unresolved weak variables distinctly (OCaml-like names such as
`_weakN`), so users can see that the binding is not fully polymorphic.

### 5. Diagnostics and UX changes

When a weak variable is later fixed incompatibly, emit an error that explains:

- this came from a weak variable
- where it was first constrained (if available)
- likely fixes (eta-expand, explicit annotation, or split bindings)

### 6. Mode/compilation boundary policy

OCaml-like behavior is most useful in interactive settings. For compiled file
or system boundaries, we may still reject exported unresolved weak vars (or
require explicit annotations), depending on desired interface guarantees.
