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
   #:coalton-library/collections
   #:Collection
   #:new-collection
   #:new-repeat
   #:new-from
   #:new-convert
   #:filter
   #:remove-duplicates
   #:empty?
   #:length
   #:contains-elt?
   #:contains-where?
   #:count-where
   #:add
   #:remove-elt

   #:ImmutableCollection

   #:MutableCollection
   #:copy
   #:filter!
   #:add!

   #:LinearCollection
   #:head
   #:head#
   #:last
   #:last#
   #:tail
   #:take
   #:drop
   #:index-elt
   #:index-elt#
   #:index-where
   #:index-where#
   #:find-where
   #:indices-elt
   #:indices-where
   #:subseq
   #:split-at
   #:split-elt
   #:split-where
   #:reverse
   #:sort
   #:sort-with
   #:zip
   #:zip-with
   #:push
   #:push-end
   #:insert-at
   #:remove-at
   #:remove-at#
   #:set-at

   #:ImmutableLinearCollection

   #:MutableLinearCollection
   #:reverse!
   #:sort!
   #:sort-with!
   #:push!
   #:push-end!
   #:pop!
   #:pop!#
   #:pop-end!
   #:pop-end!#
   #:insert-at!
   #:remove-at!
   #:remove-at!#
   #:set-at!)

  (:export
   #:Collection
   #:new-collection
   #:new-repeat
   #:new-from
   #:new-convert
   #:filter
   #:remove-duplicates
   #:empty?
   #:length
   #:contains-elt?
   #:contains-where?
   #:count-where
   #:add
   #:remove-elt

   #:ImmutableCollection

   #:MutableCollection
   #:copy
   #:filter!
   #:add!
   
   #:LinearCollection
   #:head
   #:head#
   #:last
   #:last#
   #:tail
   #:take
   #:drop
   #:index-elt
   #:index-elt#
   #:index-where
   #:index-where#
   #:find-where
   #:subseq
   #:split-at
   #:split-elt
   #:split-where
   #:reverse
   #:sort
   #:sort-with
   #:zip
   #:zip-with
   #:push
   #:push-end
   #:insert-at
   #:remove-at
   #:remove-at#
   #:set-at

   #:ImmutableLinearCollection
    
   #:MutableLinearCollection
   #:reverse!
   #:sort!
   #:sort-with!
   #:push!
   #:push-end!
   #:pop!
   #:pop!#
   #:pop-end!
   #:pop-end!#
   #:insert-at!
   #:remove-at!
   #:remove-at!#
   #:set-at!)

  (:import-from
   #:coalton-library/cell
   #:Cell)
  (:export
   #:Cell)

  (:import-from
   #:coalton-library/collections/immutable/list
   #:range
   #:all
   #:append
   #:singleton)
  (:export
   #:range
   #:all
   #:append
   #:singleton)

  (:import-from
   #:coalton-library/collections/mutable/vector
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
   (#:list #:coalton-library/collections/immutable/list)
   (#:result #:coalton-library/result)
   (#:cell #:coalton-library/cell)
   (#:vector #:coalton-library/collections/mutable/vector)
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
   (#:loops #:coalton-library/experimental/loops)))
