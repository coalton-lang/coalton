;;;; prelude.lisp
;;;;
;;;; Collections of packages

(uiop:define-package #:coalton-prelude
  (:use-reexport
   #:coalton/classes
   #:coalton/builtin
   #:coalton/functions)

  (:import-from
   #:coalton/math/arith
   #:Reciprocable #:/
   #:Fraction
   #:reciprocal
   #:negate
   #:abs
   #:1+
   #:1-
   #:positive?
   #:negative?
   #:zero?
   #:nonzero?)
  (:export
   #:Reciprocable #:/
   #:reciprocal
   #:Fraction
   #:negate
   #:abs
   #:1+
   #:1-
   #:positive?
   #:negative?
   #:zero?
   #:nonzero?)

  (:import-from
   #:coalton/math/real
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
   #:coalton/math/elementary
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
   #:coalton/math/complex
   #:Complex
   #:real-part
   #:imag-part)
  (:export
   #:Complex
   #:real-part
   #:imag-part)

  (:import-from
   #:coalton/math/integral
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
   #:coalton/string
   #:substring
   #:split)
  (:export
   #:substring
   #:split)

  (:import-from
   #:coalton/tuple
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
   #:coalton/optional
   #:from-some
   #:some?
   #:none?)
  (:export
   #:from-some
   #:some?
   #:none?)

  (:import-from
   #:coalton/list
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
   #:split-at)
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
   #:split-at)

  (:import-from
   #:coalton/cell
   #:Cell)
  (:export
   #:Cell)

  (:import-from
   #:coalton/vector
   #:Vector)
  (:export
   #:Vector)

  (:import-from
   #:coalton/hashtable
   #:Hashtable)
  (:export
   #:Hashtable)

  (:import-from
   #:coalton/iterator
   #:Iterator)
  (:export
   #:Iterator)

  (:import-from
   #:coalton/system
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
   (#:types #:coalton/types)
   (#:hash #:coalton/hash)
   (#:bits #:coalton/bits)
   (#:math #:coalton/math)
   (#:char #:coalton/char)
   (#:string #:coalton/string)
   (#:tuple #:coalton/tuple)
   (#:optional #:coalton/optional)
   (#:list #:coalton/list)
   (#:result #:coalton/result)
   (#:cell #:coalton/cell)
   (#:vector #:coalton/vector)
   (#:slice #:coalton/slice)
   (#:hashtable #:coalton/hashtable)
   (#:id #:coalton/monad/identity)
   (#:st #:coalton/monad/state)
   (#:env #:coalton/monad/environment)
   (#:resultt #:coalton/monad/resultt)
   (#:optionalt #:coalton/monad/optionalt)
   (#:iter #:coalton/iterator)
   (#:sys #:coalton/system)
   (#:file #:coalton/file)
   (#:experimental #:coalton/experimental)
   (#:loops #:coalton/experimental/loops)))
