;;;; prelude.lisp
;;;;
;;;; Collections of packages

(uiop:define-package #:coalton-library/math
  (:use-reexport
   #:coalton-library/math/arith
   #:coalton-library/math/num
   #:coalton-library/math/bounded
   #:coalton-library/math/conversions
   #:coalton-library/math/fraction
   #:coalton-library/math/integral
   #:coalton-library/math/real
   #:coalton-library/math/complex
   #:coalton-library/math/elementary
   #:coalton-library/math/dual))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH")

(uiop:define-package #:coalton-prelude
  (:use-reexport
   #:coalton-library/classes
   #:coalton-library/builtin
   #:coalton-library/functions)

  (:import-from
   #:coalton-library/hash
   #:hash)
  (:export
   #:hash)

  (:import-from
   #:coalton-library/math/arith
   #:Reciprocable #:/
   #:Fraction
   #:reciprocal
   #:negate
   #:abs
   #:1+
   #:1-)
  (:export
   #:Reciprocable #:/
   #:reciprocal
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
   #:coalton-library/math/elementary
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan
   #:log
   #:ln
   #:pow
   #:exp
   #:sqrt
   #:nth-root)
  (:export
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan
   #:log
   #:ln
   #:pow
   #:exp
   #:sqrt
   #:nth-root)

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
   #:from-some
   #:some?
   #:none?)
  (:export
   #:from-some
   #:some?
   #:none?)

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

#+sb-package-locks
(sb-ext:lock-package "COALTON-PRELUDE")

(defpackage #:coalton-user
  (:import-from
   #:common-lisp
   #:describe
   #:disassemble)
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:types #:coalton-library/types)
   (#:hash #:coalton-library/hash)
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

