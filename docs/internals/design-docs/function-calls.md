# Function application in Coalton

Coalton source functions are fixed-arity. A type such as `A * B -> C` denotes
one function that takes two arguments. A type such as `A -> B -> C` denotes a
function that takes one argument and returns another function; this is how
user-written currying is represented.

Keyword arguments are still fixed-arity at runtime. A visible type such as
`A &key (:x B) (:y C) -> D` lowers to one Common Lisp function with one required
positional argument and a `cl:&key` tail for the Coalton keywords.

Immediate calls stay direct. If the callee is constrained, the compiler resolves
the needed dictionaries and prepends them at the same call site. It does not
eta-expand immediate calls into forwarding lambdas just to supply hidden
arguments.

At runtime, first-class Coalton functions are ordinary Common Lisp function
objects. When a constrained function becomes a first-class value, the compiler
captures the resolved hidden dictionaries in a closure whose Common Lisp lambda
list matches the remaining visible positional and keyword interface, so Common
Lisp performs the runtime arity and keyword checks.

For keyword-bearing calls, the compiler evaluates the visible arguments once and
emits an ordinary Common Lisp call with keyword arguments. The visible Coalton
keyword syntax therefore maps directly onto the generated `cl:&key` call shape.

The generalized application path is still useful for explicit higher-order
values and Common Lisp interop. In those cases, `call-coalton-function` calls
raw Lisp function objects directly and forwards Lisp keyword arguments
unchanged. Compiler-generated indirect calls use `funcall` or `apply` on raw
function objects directly.

## Calling conventions and multi-entrypoint functions

Every top-level Coalton function `f` is compiled into a coordinated bundle of
one or more entrypoints recorded on its `function-env-entry`:

1. **`:lisp` (Canonical Boxed Entrypoint)**:
   - Bound to `(fdefinition 'f)` and the global lexical value `f`.
   - Uses standard Common Lisp boxed arguments and return values, including any
     `cl:&key` parameters.
   - Used for higher-order values (`#'f`), `call-coalton-function`, keyword
     calls, Common Lisp / FFI interop, and REPL inspection.
   - When `f` has a specialized primary worker entrypoint, the `:lisp`
     entrypoint is emitted as a lightweight trampoline that unpacks composite
     inputs, delegates to the primary worker, and packs the return value.

2. **`:unpacked` (Flattened Composite Worker Entrypoint, `f%UNPACKED`)**:
   - Flattens eligible single-constructor product arguments (`define-struct`
     and single-constructor `define-type` ADTs such as `Tuple`) into individual
     positional parameters when the parameter is purely destructured in the
     worker body.
   - Returns eligible single-constructor product results as multiple Common
     Lisp return values (`cl:values`), and returns eligible concrete sum
     results (such as `Optional` and `Result`) as `(cl:values tag payload_1 ...)`
     (`Boolean` tag for 2-variant sums, `UFix` tag for 3–8 variant sums).

3. **`:unboxed` (SBCL Register-Specialized Worker Entrypoint, `f%UNBOXED`)**:
   - Extends the flattened `:unpacked` layout with SBCL's internal register
     calling convention (`sb-int:specialized-xep` /
     `sb-impl::%defun-specialized-xep`).
   - Passes and returns machine scalars (`U64`, `I64`, `UFix`/`IFix`, `F32`,
     `F64`, `(Complex F32)`, `(Complex F64)`, and `sb-sys:system-area-pointer`)
     directly in general-purpose and XMM registers across non-inlined,
     recursive, and cross-file calls without heap boxing.

### Call-site selection and pack/unpack cancellation

After `direct-application` identifies saturated direct calls (`node-direct-application`),
`rewrite-callsite-entrypoints` rewrites each eligible direct call to target the
callee's highest-priority worker entrypoint (`:unboxed` > `:unpacked` > `:lisp`),
inserting AST-level input unpacking and output packing around the worker call.
Subsequent `lawnmow` and `scalar-replace` passes cancel back-to-back pack/unpack
boundaries across chained calls (`(f (g x))`), `match` scrutinees, and tail
returns.

### Redefinition semantics

- **Same-Signature Redefinition**: Redefining `f` with the same ABI layout
  updates `f`, `f%UNPACKED`, and `f%UNBOXED` in place so existing callers see
  the updated definition immediately.
- **Type-Changing Redefinition**: If a redefinition removes or changes a
  previously emitted worker entrypoint, `emit-stale-entrypoint-stubs` emits a
  forwarding stub on the retired worker symbol that packs its old ABI arguments,
  calls the canonical `f`, and unpacks the result to the old return layout.

