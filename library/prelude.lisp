;;;; prelude.lisp
;;;;
;;;; Collections of packages

(uiop:define-package #:coalton-prelude
  (:use-reexport
   #:coalton-library/classes
   #:coalton-library/builtin
   #:coalton-library/functions)

  (:import-from
   #:coalton-library/math/arith
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
   #:time)

  (:import-from
   #:coalton-library/show
   #:standard-output
   #:error-output
   #:with-show-stream
   #:Show
   #:show*
   #:show-to
   #:show-to-string
   #:newline
   #:Reveal
   #:Expose)
  (:export
   #:standard-output
   #:error-output
   #:with-show-stream
   #:Show                               ; class and function
   #:show*
   #:show-to
   #:show-to-string
   #:newline
   #:Reveal
   #:Expose))

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
   (#:id #:coalton-library/monad/identity)
   (#:st #:coalton-library/monad/state)
   (#:env #:coalton-library/monad/environment)
   (#:resultt #:coalton-library/monad/resultt)
   (#:optionalt #:coalton-library/monad/optionalt)
   (#:iter #:coalton-library/iterator)
   (#:sys #:coalton-library/system)
   (#:file #:coalton-library/file)
   (#:experimental #:coalton-library/experimental)
   (#:loops #:coalton-library/experimental/loops)
   (#:show #:coalton-library/show)))
