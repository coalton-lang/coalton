# Function Application in Coalton

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

At runtime, first-class Coalton functions are wrapped in `function-entry`
structs. A `function-entry` stores:

- the visible positional arity
- the underlying Lisp function
- any hidden dictionary arguments already bound onto the value

Keyword arguments do not contribute to the stored arity.

For keyword-bearing calls, the compiler evaluates the visible arguments once and
emits an ordinary Common Lisp call with keyword arguments. The visible Coalton
keyword syntax therefore maps directly onto the generated `cl:&key` call shape.

The generalized application path is still useful for explicit higher-order
values and Common Lisp interop. In those cases, `call-coalton-function` checks
the exact visible positional arity of a `function-entry`, prepends any bound
hidden arguments, and forwards Lisp keyword arguments unchanged. When the call
site is known to have no keywords and the callee is already a `function-entry`,
the compiler may emit `exact-call` instead.
