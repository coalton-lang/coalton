# Configuring Coalton

Coalton allows a bit of configuration. **For ordinary development, it is not necessary!** There are a few ways Coalton can be configured, but in this document, we will limit our discussion to configuration through the `:coalton-config` keyword.

## Example Configuration Through `:coalton-config`

The `:coalton-config` keyword's symbol-plist is inspected just before the Coalton system is compiled. Therefore, the easiest is to add configure Coalton is to add configuration options to your Lisp system's init file (such as SBCL's `.sbclrc`). Here is an example configuration that one might use for day-to-day development:

```
(let ((config '((:compiler-mode              "development")
                (:print-unicode              t)
                (:perform-specialization     nil)
                (:perform-inlining           nil)
                (:emit-type-annotations      nil)
                (:print-types                t)
                (:print-rewrites             nil)
                (:auto-continue-redefinition t))))
  (setf (symbol-plist ':coalton-config) nil)
  (loop :for (key value) :in config
        :do (setf (get ':coalton-config key) value)))
```

This ensures Coalton is in development mode, turns off various optimizations, but enables printing of Coalton types when your code is being compiled. Here is another example configuration that one might use for working on high-performance code:

```
(let ((config '((:compiler-mode          "release")
                (:print-unicode          t)
                (:perform-specialization t)
                (:perform-inlining       t)
                (:emit-type-annotations  t)
                (:print-types            t)
                (:print-rewrites         t))))
  (setf (symbol-plist ':coalton-config) nil)
  (loop :for (key value) :in config
        :do (setf (get ':coalton-config key) value)))
```

This puts Coalton in release mode, tells Coalton to perform heuristic inlining and specialization, tells Coalton to type-annotate all generated Lisp code, and tells Coalton to print information about what rewriting it's doing. This code will be very inflexible, and may take longer to compile, but may help diagnose issues pertaining to optimization and high performance.

## Options for `:coalton-config`

### `:compiler-mode`

This tells the Coalton compiler in which mode it should compile all code.

Allowed options:
- `"development"`: Makes most things redefinable. Disables certain optimizations that impede interactive development.
- `"release"`: Assumes most data structures are frozen. Enables certain optimizations. Assumes program won't be interactively developed.

### `:print-unicode`

This controls whether types are printed with Unicode characters, e.g., `:a → :b` versus `:a -> :b`.

Allowed options:
- `t`: Print with Unicode.
- `nil`: Don't.

### `:perform-specialization`

This controls whether specialization is performed. Certain functions in Coalton may be specialized with the `specialize` directive, which allows replacing a function call with a more specialized one if the argument and return types match a pattern. This usually results in more optimized code, especially for numerical code. This configuration option should not affect the semantics of your program.

Allowed options:
- `t`: Allow Coalton to specialize function calls.
- `nil`: Don't.

### `:perform-inlining`

This controls whether *heuristic* inlining is performed. Heuristic inlining allows Coalton to decide what and when to inline. Coalton will respect `(inline)` directives regardless of this configuration option. This option may significantly increase compile time or code size.

Allowed options:
- `t`: Allow Coalton to perform heuristic inlining.
- `nil`: Don't.

### `:emit-type-annotations`

This controls whether the generated Lisp code should be annotated with Lisp types (e.g., `declare`). This allows the host Lisp compiler to generate more efficient code with better error checking. However, it can increase compile time.

Allowed options:
- `t`: Allow Coalton to emit type declarations.
- `nil`: Don't emit type declarations.

### `:print-types`

This controls whether function and value types are printed as they're compiled. For example, with this option enabled, compiling the following code:

```
(coalton-toplevel
  (define (square x)
    (* x x))

  (define (unit? x)
    (and (<= 0 x) (<= x 1))))
```

will produce the following compilation output (with Unicode printing on):

```
;; UNIT? :: ∀ A. (ORD A) (NUM A) ⇒ (A → BOOLEAN)
;; SQUARE :: ∀ A. NUM A ⇒ (A → A)
```

Allowed options:
- `t`: Print function and value types during compilation.
- `nil`: Don't.

### `:print-rewrites`

This controls whether information about how the compiler rewrites code should be printed. This is generally specialization and inlining information. This is noisy output and is used for performance debugging, typically. For example, this code:

```
(coalton-toplevel
  (inline)
  (define (double x)
    (* 2.0 x))

  (define (quadruple x)
    (double (double x))))
```

may display something like this in the compilation output:

```
;; Optimization run #1
;; Inlining method * -> COALTON-LIBRARY/MATH/NUM::|INSTANCE/CLS:NUM SINGLE-FLOAT-COALTON-LIBRARY/CLASSES:*|
;; Inlining COALTON-LIBRARY/MATH/NUM::|INSTANCE/CLS:NUM SINGLE-FLOAT-COALTON-LIBRARY/CLASSES:*|
;; Optimization run #2
;; Optimization run #1
;; Inlining DOUBLE
;; Inlining DOUBLE
;; Optimization run #2
;; Inlining method * -> COALTON-LIBRARY/MATH/NUM::|INSTANCE/CLS:NUM SINGLE-FLOAT-COALTON-LIBRARY/CLASSES:*|
;; Inlining method * -> COALTON-LIBRARY/MATH/NUM::|INSTANCE/CLS:NUM SINGLE-FLOAT-COALTON-LIBRARY/CLASSES:*|
;; Inlining COALTON-LIBRARY/MATH/NUM::|INSTANCE/CLS:NUM SINGLE-FLOAT-COALTON-LIBRARY/CLASSES:*|
;; Inlining COALTON-LIBRARY/MATH/NUM::|INSTANCE/CLS:NUM SINGLE-FLOAT-COALTON-LIBRARY/CLASSES:*|
;; Optimization run #3
;; DOUBLE :: (SINGLE-FLOAT → SINGLE-FLOAT)
;; QUADRUPLE :: (SINGLE-FLOAT → SINGLE-FLOAT)
```

The precise format of the output is not guaranteed.

Allowed options:
- `t`: Print rewriting information.
- `nil`: Don't.

### `:auto-continue-redefinition`

This controls whether to automatically continue with incompatible function redefinitions at the REPL instead of raising an error. This is useful for interactive development when you're experimenting with changing function signatures and don't want to be blocked by redefinition errors.

When an incompatible redefinition is detected (e.g., changing a function's type signature), Coalton will:
- With `nil` (default): Raise an error showing the detailed list of affected functions and provide restart options
- With `t`: Automatically continue with the redefinition and issue a concise warning showing only the count of affected functions

The warning message when auto-continuing shows only the count rather than listing all affected functions to keep the output concise, as the list could be quite long in a large codebase.

Example scenario where this is useful:
```lisp
;; Original definition
(coalton-toplevel
  (define (add x y)
    (+ x y)))

;; Later, changing the signature
(coalton-toplevel
  (define (add x y z)  ; Adding a third parameter
    (+ (+ x y) z)))
```

With `:auto-continue-redefinition` set to `t`, the second definition will succeed with a warning. With it set to `nil`, you'll get an error with restart options.

Allowed options:
- `t`: Automatically continue with incompatible redefinitions.
- `nil`: Raise an error and provide restart options (default).

## Further Reading

More details about configuration can be found in the Coalton source code for [settings](https://github.com/coalton-lang/coalton/blob/main/src/settings.lisp).
