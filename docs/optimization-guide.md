# Optimization Guide

Coalton compiles to Common Lisp, which in turn compiles to native machine code via the host CL implementation (e.g., SBCL). This gives Coalton access to mature optimizing compilers. This guide covers Coalton-specific optimization features that can help you write faster code.

## Inlining

Inlining replaces a function call with the function's body at the call site, eliminating call overhead and enabling further optimizations like constant folding.

### Declaring a Function Inline

Use the `(inline)` attribute before a definition to mark a function for inlining:

```lisp
(coalton-toplevel
  (inline)
  (declare add1 (Integer -> Integer))
  (define (add1 x)
    (+ x 1)))
```

When `add1` is called, the compiler will substitute the function body directly at the call site rather than emitting a function call.

### When to Inline

Inlining is most effective for:
- **Small functions** — a few operations, a simple pattern match, or a thin wrapper around another function
- **Hot inner loops** — functions called in tight loops where call overhead matters

Avoid inlining large functions, as this increases code size ("code bloat") and can hurt instruction cache performance.

### Heuristic Inlining

Even without an explicit `(inline)` attribute, Coalton's compiler applies heuristic inlining for sufficiently small functions. The default "gentle" heuristic inlines functions with at most 4 applications and 8 AST nodes. This happens automatically and does not require any annotation.

The heuristic can be configured at the CL level:
- `coalton-impl/codegen/inliner:*inliner-heuristic*` — the heuristic function (default: `'gentle-heuristic`)
- `coalton-impl/codegen/inliner:*inliner-max-depth*` — maximum inlining depth (default: 16)
- `coalton-impl/codegen/inliner:*inliner-max-unroll*` — maximum recursive unroll depth (default: 3)

## Monomorphization

Coalton functions that use type class constraints are compiled as functions that take *dictionary* arguments at runtime. For example:

```lisp
(declare double (Num :a => :a -> :a))
(define (double x) (+ x x))
```

At the CL level, `double` receives a dictionary argument for `Num` and dispatches `+` through it. This adds overhead compared to a function that directly uses fixnum or float addition.

**Monomorphization** eliminates this overhead by creating specialized copies of a function for the concrete types it is called with.

### Using `(monomorphize)`

Annotate a definition with the `(monomorphize)` attribute:

```lisp
(coalton-toplevel
  (monomorphize)
  (declare double (Num :a => :a -> :a))
  (define (double x) (+ x x)))
```

When the compiler encounters a call like `(double 5)` (where `5` is an `Integer`), it generates a specialized version `double<Integer>` that directly calls integer addition, with no dictionary passing.

### How It Works

The monomorphizer traverses the call graph starting from monomorphized entry points. At each call site, if the type class dictionaries are statically known (i.e., the concrete types are determined), it creates a specialized copy of the called function with those dictionaries inlined. This process is recursive — if the specialized function itself calls other generic functions with known dictionaries, those are specialized too.

### Trade-offs

- **Pro:** Eliminates dictionary-passing overhead, enables further inlining and optimization
- **Con:** Can increase code size if a function is called with many different type combinations
- **Con:** Increases compile time proportional to the number of specializations generated

Monomorphization is most valuable for performance-critical code where dictionary dispatch is a measurable bottleneck.

## Specialization

Specialization lets you manually provide an optimized implementation of a generic function for a specific type, without modifying the original function.

### Declaring a Specialization

```lisp
(coalton-toplevel
  ;; Generic function
  (declare sum-list (Num :a => List :a -> :a))
  (define (sum-list lst)
    (fold + 0 lst))

  ;; Optimized version for Integer
  (declare sum-list/integer (List Integer -> Integer))
  (define (sum-list/integer lst)
    (lisp Integer (lst)
      (cl:loop :for x :in lst :sum x)))

  ;; Tell the compiler to use sum-list/integer when sum-list is called on Integer lists
  (specialize sum-list sum-list/integer (List Integer -> Integer)))
```

When the compiler encounters `(sum-list my-integer-list)`, it will substitute the call with `(sum-list/integer my-integer-list)`, bypassing dictionary dispatch entirely.

### When to Use Specialization

- When you have a generic function that has a much faster implementation for a specific type
- When you want to use CL-native operations (via `lisp`) for specific types without sacrificing the generic API
- When monomorphization alone isn't sufficient because you need a fundamentally different algorithm for certain types

## Fixed-Width Numeric Types

The fixed-width types `IFix`, `UFix`, `I32`, `U32`, `I64`, `U64`, `F32`, and `F64` map directly to their CL counterparts. When their types are known at compile time (via type annotations or `:emit-type-annotations`), the host CL compiler can generate efficient machine instructions for arithmetic on these types.

## Compiler Configuration

Coalton provides several configuration options that affect optimization. These are set via the `:coalton-config` keyword's symbol-plist, typically in your Lisp init file (e.g., `.sbclrc`). See [Configuring Coalton](configuring-coalton.md) for the full reference.

Key optimization-related options:

- `:compiler-mode` — `"development"` (default) or `"release"`. Release mode assumes frozen data structures and enables additional optimizations.
- `:perform-specialization` — controls whether `specialize` directives are applied.
- `:perform-inlining` — controls heuristic inlining (explicit `(inline)` attributes are always respected).
- `:emit-type-annotations` — annotates generated CL code with type declarations, allowing the host compiler to generate more efficient code.

You can also set the compiler mode via the `COALTON_ENV` environment variable before loading Coalton:

```bash
COALTON_ENV=release sbcl --load my-project.asd
```

In release mode, generated code is compiled with high CL optimization settings. The host CL compiler (e.g., SBCL) then applies its own optimizations — unboxing, SIMD, etc. — to the generated code.
