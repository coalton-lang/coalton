(cl:in-package #:coalton-impl/early-library-defs)

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

;; define early classes first, in their own block; `define-type' is not available until after the early
;; classes have been defined.
(coalton-toplevel
  ;; Addressable is an early class; compile its `define-class' here to get the defstruct for its instances and whatnot.
  (define-class (Addressable :obj)
    "Types for which object identity is meaningful.

`eq?' should correspond exactly to the Common Lisp function `eq', testing object identity (aka pointer
equality).

The compiler will auto-generate instances of `Addressable' for types which specify `repr :enum' or `repr
:lisp'.

Types with `repr :native' may manually implement `Addressable', but programmers are encouraged to check the
Common Lisp Hyperspec (at http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm) to determine what
guarantees, if any, are imposed on the behavior of `eq'. Types represented by `cl:character' or
`cl:number' (or sub- or supertypes thereof) should not implement `Addressable', as those objects may be
implicitly copied.

No other types may implement `Addressable'. Defining an `Addressable' instance manually for a type which does
not specify `repr :native' will error. If you need an `Addressable' instance for a non-`repr :native' type,
specify `repr :lisp'.

`Addressable' and `eq?' are exported from the package `coalton-library/addressable', and should be used via
that package."
    (eq? (:obj -> :obj -> Boolean)))

  ;; Num (and therefore Eq) is sort of an early class; the compiler doesn't define any instances, but it does
  ;; insert calls to `fromInt'.
  (define-class (Eq :a)
    "Types which have equality defined."
    (== (:a -> :a -> Boolean)))

  (define-class ((Eq :a) => (Num :a))
    "Types which have numeric operations defined."
    (+ (:a -> :a -> :a))
    (- (:a -> :a -> :a))
    (* (:a -> :a -> :a))
    (fromInt (Integer -> :a))))
