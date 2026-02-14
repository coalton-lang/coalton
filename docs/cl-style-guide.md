# Common Lisp Style Guide for Coalton Compiler Development

This document describes the Common Lisp coding conventions used in the Coalton compiler (`src/`). It is intended for contributors working on the compiler internals, not for users writing Coalton code. For the Coalton standard library style guide, see [style-guide.md](./style-guide.md).

## Variable Binding

### Prefer `let*` over `let`

Use `let*` as the default binding form. Since most bindings depend on previous ones, `let*` avoids the confusion of parallel binding semantics. Use `let` only when parallel binding is specifically needed or when there's a single binding.

```lisp
;; Preferred
(let* ((x (compute-something))
       (y (transform x))
       (z (finalize y)))
  (use z))

;; Acceptable for single bindings
(let ((x 5))
  (use x))
```

### Initially-nil variables

Enclose initially-nil variables in explicit parentheses with `nil` rather than using the bare-symbol shorthand, for clarity:

```lisp
;; Preferred
(let* ((result nil)
       (count 0))
  ...)

;; Avoid
(let* (result
       (count 0))
  ...)
```

### Predeclared local variables

Predeclared local variables (bound at the top of a function and set later) are acceptable when the control flow makes it awkward to bind them in-line. However, prefer functional style (binding in `let*`) when possible.

## Control Flow

### Prefer `when`/`unless` for one-armed conditionals

Use `when` and `unless` for side-effecting single-branch conditionals. Use `if` only when both branches produce a value.

```lisp
;; Good — side effect, no else branch
(when (some-condition)
  (do-something))

;; Good — value-producing, both branches needed
(if (some-condition) value-a value-b)
```

### Use `cond` for multi-way branching

```lisp
(cond
  ((null list)
   (handle-empty))
  ((= (length list) 1)
   (handle-singleton list))
  (t
   (handle-general list)))
```

### Early return with `return-from`

Functions that validate preconditions often return early. This is idiomatic:

```lisp
(defun apply-optimization (node env)
  (unless (optimization-applicable-p node)
    (return-from apply-optimization node))
  ;; ... main logic
  )
```

## Expression-Oriented Style

Prefer expression-oriented code where the result flows through binding forms rather than accumulating via mutation:

```lisp
;; Preferred — expression-oriented
(let* ((processed (process input))
       (validated (validate processed))
       (result (transform validated)))
  result)

;; Avoid when possible — procedural accumulation
(let ((result nil))
  (setf result (process input))
  (setf result (validate result))
  (setf result (transform result))
  result)
```

Mutation via `setf`, `push`, and `incf` is acceptable in loops and when building up collections.

## Naming Conventions

### Functions

- Use `kebab-case` for all function and variable names
- Predicates end in `-p`: `node-variable-p`, `coalton-release-p`
- Constructors use `make-`: `make-node-variable`, `make-ast-substitution`
- Accessors use `struct-name-field-name`: `node-type`, `node-match-branches`

### Special variables

- Global special variables use earmuffs: `*inliner-max-depth*`, `*coalton-optimize-library*`
- Constants use `+`: `+macro-expansion-max+`

### Packages

- Implementation packages use the `coalton-impl/` prefix: `coalton-impl/parser`, `coalton-impl/typechecker`
- Use local nicknames rather than long package prefixes: `(#:tc #:coalton-impl/typechecker)`

## Comments

Follow standard Lisp comment conventions:

- `;;;;` — File-level headers and section separators
- `;;;` — Section descriptions within a file
- `;;` — Inline comments explaining the following code block
- `;` — End-of-line annotations on the same line as code

```lisp
;;;; src/codegen/optimizer.lisp
;;;;
;;;; The optimization pipeline for compiled Coalton code.

;;; Constant Folding

;; We only fold pure operations
(defun foldable-p (node)
  (pure-operation-p node))  ; check purity before folding
```

## Exports and Package Organization

The export list in `defpackage` should annotate each export with its kind:

```lisp
(:export
 #:node-variable                ; STRUCT
 #:make-node-variable           ; CONSTRUCTOR
 #:node-variable-name           ; ACCESSOR
 #:parse-expression             ; FUNCTION
 #:*debug-mode*                 ; VARIABLE
 #:+max-depth+                  ; CONSTANT
 )
```

## Type Declarations

Use `declare` and `declaim` for function signatures to help the CL compiler generate efficient code and to serve as documentation:

```lisp
(defun process-node (node env)
  (declare (type node node)
           (type environment env)
           (values node &optional))
  ...)
```

## Error Handling

- Use `coalton-bug` (from `coalton-impl/util`) for internal compiler errors that indicate a bug
- Use the condition system (`parse-error`, `tc-error`) for user-facing errors
- Include enough context in error messages to diagnose the issue

```lisp
;; Internal bug — should never happen
(util:coalton-bug "Expected function ~A to have at least ~A args" name (length preds))

;; User-facing parse error
(parse-error "Malformed expression"
             (note source form "unexpected dotted list"))
```
