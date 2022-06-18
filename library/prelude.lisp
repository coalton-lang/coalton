;;;; prelude.lisp
;;;;
;;;; Collections of packages

(uiop:define-package #:coalton-library/math
  (:use-reexport
   #:coalton-library/math/arith
   #:coalton-library/math/integral
   #:coalton-library/math/real
   #:coalton-library/math/complex
   #:coalton-library/math/elementary))

(uiop:define-package #:coalton-prelude
  (:use-reexport
   #:coalton-library/classes
   #:coalton-library/builtin
   #:coalton-library/functions)

  (:import-from
   #:coalton-library/math/arith
   #:Reciprocable #:/
   #:Fraction
   #:negate
   #:abs
   #:1+
   #:1-)
  (:export
   #:Reciprocable #:/
   #:Fraction
   #:negate
   #:abs
   #:1+
   #:1-)

  (:import-from
   #:coalton-library/math/real
   #:floor
   #:ceiling
   #:round
   #:Real
   #:Rational)
  (:export
   #:floor
   #:ceiling
   #:round
   #:Real
   #:Rational)

  (:import-from
   #:coalton-library/math/complex
   #:Complex
   #:real-part
   #:imag-part)
  (:export
   #:Complex
   #:real-part
   #:imag-part)

  (:import-from
   #:coalton-library/math/integral
   #:Integral
   #:^
   #:^^
   #:mod
   #:even?
   #:odd?)
  (:export
   #:Integral
   #:^
   #:^^
   #:mod
   #:even?
   #:odd?)

  (:import-from
   #:coalton-library/string
   #:substring)
  (:export
   #:substring)

  (:import-from
   #:coalton-library/tuple
   #:fst
   #:snd
   #:Tuple3
   #:Tuple4
   #:Tuple5)
  (:export
   #:fst
   #:snd
   #:Tuple3
   #:Tuple4
   #:Tuple5)

  (:import-from
   #:coalton-library/optional
   #:fromSome
   #:isSome
   #:isNone)
  (:export
   #:fromSome
   #:isSome
   #:isNone)

  (:import-from
   #:coalton-library/list
   #:head
   #:tail
   #:singleton
   #:repeat
   #:reverse
   #:drop
   #:take
   #:find
   #:fold
   #:foldr
   #:filter
   #:length
   #:range
   #:append
   #:concat
   #:concatMap
   #:remove-duplicates
   #:zip
   #:zipWith
   #:sum
   #:product
   #:all
   #:any
   #:split)
  (:export
   #:head
   #:tail
   #:singleton
   #:repeat
   #:reverse
   #:drop
   #:take
   #:find
   #:fold
   #:foldr
   #:filter
   #:length
   #:range
   #:append
   #:concat
   #:concatMap
   #:remove-duplicates
   #:zip
   #:zipWith
   #:sum
   #:product
   #:all
   #:any
   #:split)

  (:import-from
   #:coalton-library/cell
   #:Cell)
  (:export
   #:Cell)

  (:import-from
   #:coalton-library/vector
   #:Vector)
  (:export
   #:Vector)

  (:import-from
   #:coalton-library/hashtable
   #:Hashtable)
  (:export
   #:Hashtable)

  (:import-from
   #:coalton-library/iterator
   #:Iterator)
  (:export
   #:Iterator)

  (:import-from
   #:coalton-library/system
   #:time)
  (:export
   #:time))

(cl:defpackage #:coalton-user
  (:import-from
   #:common-lisp
   #:describe
   #:disassemble)
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:bits #:coalton-library/bits)
   (#:math #:coalton-library/math)
   (#:char #:coalton-library/char)
   (#:string #:coalton-library/string)
   (#:tuple #:coalton-library/tuple)
   (#:optional #:coalton-library/optional)
   (#:list #:coalton-library/list)
   (#:result #:coalton-library/result)
   (#:cell #:coalton-library/cell)
   (#:vector #:coalton-library/vector)
   (#:slice #:coalton-library/slice)
   (#:hashtable #:coalton-library/hashtable)
   (#:st #:coalton-library/monad/state)
   (#:iter #:coalton-library/iterator)
   (#:sys #:coalton-library/system)))

(cl:in-package #:coalton-prelude)

#+sb-package-locks
(sb-ext:lock-package "COALTON-PRELUDE")

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH")
