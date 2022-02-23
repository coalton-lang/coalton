(uiop:define-package #:coalton-prelude
  (:use-reexport
   #:coalton-library/classes
   #:coalton-library/builtin
   #:coalton-library/functions)

  (:import-from
   #:coalton-library/arith
   #:Dividable #:/
   #:Fraction
   #:negate
   #:abs
   #:expt
   #:mod
   #:even?
   #:odd?
   #:floor
   #:ceiling
   #:round
   #:1+
   #:1-)
  (:export
   #:Dividable #:/
   #:Fraction
   #:negate
   #:abs
   #:expt
   #:mod
   #:even?
   #:odd?
   #:floor
   #:ceiling
   #:round
   #:1+
   #:1-)

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
   #:Tupel4
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
   #:Iterator))

(cl:defpackage #:coalton-user
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:bits #:coalton-library/bits)
   (#:arith #:coalton-library/arith)
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
   (#:iter #:coalton-library/iterator)))

(cl:in-package #:coalton-prelude)

#+sb-package-locks
(sb-ext:lock-package "COALTON-PRELUDE")
