(coalton-library/utils:defstdlib-package #:coalton-library/collections/classes
  (:use
   #:coalton
   #:coalton-library/classes)
  (:local-nicknames
   (#:l #:coalton-library/collections/immutable/list)
   (#:types #:coalton-library/types))
  (:export
   #:Collection
   #:new-collection
   #:new-repeat
   #:new-from
   #:flatten
   #:filter
   #:remove-duplicates
   #:empty?
   #:length
   #:contains-elt?
   #:contains-where?
   #:count-where
   #:add

   #:ImmutableCollection

   #:MutableCollection
   #:copy
   #:add!
   
   #:LinearCollection
   
   #:MutableLinearCollection
   #:reverse!))

(in-package #:coalton-library/collections/classes)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;; TODO: Figure out what syntax for this:
  ;; (define-class ((Monoid :m) (Monad :m) => (Collection :m :a))
  (define-class (Collection :m)
    "Types that contain a collection of elements of another type.

Does not have an ordering of elements.

Could be mutable or immutable. All methods are allowed to modify the
underlying collection. If you need immutablility as part of the contract,
use one of the Immutable collection typeclasses."
    ;; Create new collections
    (new-collection
     "Create a new, empty collection."
     (Unit -> :m :a))
    (new-repeat
     "Create a new collection, attempting to add `elt` `n` times."
     (UFix -> :a -> :m :a))
    (new-from
     "Create a new collection by appling a function over the range [0, n)."
     (UFix -> (UFix -> :a) -> :m :a))
    ;; Manipulate at the collection level
    (flatten
     "Flatten a collection of collections into a collection of their elements."
     (:m (:m :a) -> :m :a))
    (filter
     "Create a new collection with the elements satisfying the predicate."
     ((:a -> Boolean) -> :m :a -> :m :a))
    (remove-duplicates
     "Create a new collection with all distinct elements."
     (Eq :a => :m :a -> :m :a))
    ;; Query the collection
    (empty?
     "Check if the collection contains no elements."
     (:m :a -> Boolean))
    (length
     "The number of elements in the collection"
     (:m :a -> UFix))
    (contains-elt?
     "Check if the collection contains an element."
     (Eq :a => :a -> :m :a -> Boolean))
    (contains-where?
     "Check if the collection contanis an element satisfying the predicate."
     ((:a -> Boolean) -> :m :a -> Boolean))
    (count-where
     "The number of elements satisfying the predicate."
     ((:a -> Boolean) -> :m :a -> UFix))
    ;; Manipulate at the element level
    (add
     "Add an element to the collection. For linear collections, should add to
the front or back, depending on which is natural for the underlying data structure."
     (:a -> :m :a -> :m :a)))

  (define-class (Collection :m => ImmutableCollection :m)
    "An immutable collection.")

  (define-class (Collection :m => MutableCollection :m)
    "A mutable collection."
    (copy
     "Create a shallow copy of the collection."
     (:m :a -> :m :a))
    (add!
     "Add an element to the collection in place. See `add`."
     (:a -> :m :a -> :m :a))))

;; TODO: Make it so that these must all be the proper KeyedCollection types as well
(coalton-toplevel
  (define-class (Collection :m => LinearCollection :m))

  (define-class (LinearCollection :m => MutableLinearCollection :m)
    (reverse!
     "Reverse the collection in place. The contract of `reverse` is that the
value of the sequence and the return value will be the same."
     (:m :a -> :m :a))))

;; TODO: Because `List` is a predefined type, we can't define this
;; in the new collections/immutable/list.lisp file. Define it here
;; for now, and when we remove the deprecated versions move this
;; into the list package.
(coalton-toplevel
  (define-instance (Collection List)
    (define (new-collection)
      Nil)
    (define (new-repeat n elt)
      (l:repeat n elt))
    (define (new-from n f)
      (map f (l:range 0 (- n 1))))
    (define (flatten lst)
      (>>= lst (fn (x) x)))
    (define filter l:filter)
    (define remove-duplicates l:remove-duplicates)
    (define empty? l:empty?)
    (define length l:length)
    (define contains-elt? l:contains-elt?)
    (define contains-where? l:contains-where?)
    (define count-where l:countBy)
    (define add l:Cons))

  (define-instance (ImmutableCollection List)))

;; #+sb-package-locks
;; (sb-ext:lock-package "COALTON-LIBRARY/COLLECTIONS/CLASSES")
