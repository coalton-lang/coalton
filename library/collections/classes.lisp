(coalton-library/utils:defstdlib-package #:coalton-library/collections/classes
  (:use
   #:coalton
   #:coalton-library/functions
   #:coalton-library/classes)
  (:local-nicknames
   (#:itr #:coalton-library/iterator)
   (#:types #:coalton-library/types))
  (:export
   #:Collection
   #:NestedCollection
   #:new-collection
   #:new-repeat
   #:new-from
   #:new-convert
   #:filter
   #:remove-duplicates
   #:remove-elt
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
   #:insert-at!))

(in-package #:coalton-library/collections/classes)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define-class (Collection :m :a (:m -> :a))
    "Types that contain a collection of elements of another type.

Does not have an ordering of elements.

Could be mutable or immutable. Methods are not allowed to modify the
underlying collection. If you need mutability as part of the contract,
probably to prevent defensive copying, use one of the Mutable
collection typeclasses."
    ;; Create new collections
    (new-collection
     "Create a new, empty collection."
     (Unit -> :m))
    (new-repeat
     "Create a new collection, attempting to add `elt` `n` times."
     (UFix -> :a -> :m))
    (new-from
     "Create a new collection by appling a function over the range [0, n)."
     (UFix -> (UFix -> :a) -> :m))
    (new-convert
     "Convert a collection of another type. If converting a LinearCollection to a LinearCollection, should preserve order."
     ((Collection :f :a) (itr:IntoIterator :f :a) => :f -> :m))
    ;; Manipulate at the collection level
    (filter
     "Create a new collection with the elements satisfying the predicate."
     ((:a -> Boolean) -> :m -> :m))
    (remove-duplicates
     "Create a new collection with all distinct elements."
     (Eq :a => :m -> :m))
    (remove-elt
     "Remove all occurrences of `elt` from the collection."
     (Eq :a => :a -> :m -> :m))
    ;; Query the collection
    (empty?
     "Check if the collection contains no elements."
     (:m -> Boolean))
    (length
     "The number of elements in the collection"
     (:m -> UFix))
    (contains-elt?
     "Check if the collection contains an element."
     (Eq :a => :a -> :m -> Boolean))
    (contains-where?
     "Check if the collection contains an element satisfying the predicate."
     ((:a -> Boolean) -> :m -> Boolean))
    (count-where
     "The number of elements satisfying the predicate."
     ((:a -> Boolean) -> :m -> UFix))
    ;; Manipulate at the element level
    (add
     "Add an element to the collection. For linear collections, should add to
the front or back, depending on which is natural for the underlying data structure."
     (:a -> :m -> :m)))

  (define-class (Collection :m :a => ImmutableCollection :m :a)
    "An immutable collection.")

  (define-class (Collection :m :a => MutableCollection :m :a (:m -> :a))
    "A mutable collection."
    (copy
     "Create a shallow copy of the collection."
     (:m -> :m))
    (add!
     "Add an element to the collection in place. See `add`."
     (:a -> :m -> :m))))

;; TODO: Will tackle KeyedCollection in another PR.
;; (coalton-toplevel
;;   (define-class (KeyedCollection :m :k :v (:m -> :k :v)))
;;   )

(coalton-toplevel
  ;; NOTE: In all cases, LinearCollection methods should return collections that don't
  ;; share mutable state with the original.
  (define-class (LinearCollection :m)
    ;; Get elements from the collection
    (head
     "Return the first element of the collection."
     (:m :a -> Optional :a))
    (head#
     "Return the first element of the collection, erroring if it does not exist."
     (:m :a -> :a))
    (last
     "Return the last element of the collection."
     (:m :a -> Optional :a))
    (last#
     "Return the last element of the collection, erroring if it does not exist."
     (:m :a -> :a))
    (tail
     "Return all except the first element of the collection."
     (:m :a -> :m :a))
    (take
     "Return the first `n` elements of the collection."
     (UFix -> :m :a -> :m :a))
    (drop
     "Return all except the first `n` elements of the collection."
     (UFix -> :m :a -> :m :a))
    ;; Query the collection
    (index-elt
     "Return the index of the first occurence of `elt`, if it can be found."
     (Eq :a => :a -> :m :a -> Optional UFix))
    (index-elt#
     "Return the index of the first occurence of `elt`, erroring if it cannot be found."
     (Eq :a => :a -> :m :a -> UFix))
    (index-where
     "Return the index of the first element matching a predicate function."
     ((:a -> Boolean) -> :m :a -> Optional UFIx))
    (index-where#
     "Return the index of the first element matching a predicate function, erroring if none can be found."
     ((:a -> Boolean) -> :m :a -> UFix))
    (find-where
     "Return the first element matching a predicate function."
     ((:a -> Boolean) -> :m :a -> Optional :a))
    (indices-elt
     "Return the indices of every occurence of `elt` in the collection."
     (Eq :a => :a -> :m :a -> List UFix))
    (indices-where
     "Return the indices of every element satisfying `pred` in the collection."
     ((:a -> Boolean) -> :m :a -> List UFix))
    ;; Retrieve subsets of the collection.
    (subseq
     "Extract the collection from `start` (inclusive) to `end` (exclusive)."
     (UFix -> UFix -> :m :a -> :m :a))
    (split-at
     "Split into two collections at `i`. The first collection ends at `i`-1, the second collection begins at `i`+1."
     (UFix -> :m :a -> Tuple (:m :a) (:m :a)))
    (split-elt
     "Split into collections on the each occurrence of `elt`. `elt` will be excluded from the resulting collections."
     (Eq :a => :a -> :m :a -> List (:m :a)))
    (split-where
     "Split into collections on each element satisfying `pred`. Elements satisfying `pred` will be excluded from the resulting collections."
     ((:a -> Boolean) -> :m :a -> List (:m :a)))
    ;; Manipulate at the collection level
    (reverse
     "Return the collection with elements reversed."
     (:m :a -> :m :a))
    (sort
     "Return a sorted collection of orderable elements."
     (Ord :a => :m :a -> :m :a))
    (sort-with
     "Return the sorted collection under the given ordering."
     ((:a -> :a -> Ord) -> :m :a -> :m :a))
    (zip
     "Return a collection of this collection's elements combined with elements from an iterable object."
     (itr:IntoIterator :n :b => :m :a -> :n -> :m (Tuple :a :b)))
    (zip-with
     "Return a collection of this collection's elements and an iterable object's elements applied to `f`."
     (itr:IntoIterator :n :b => (:a -> :b -> :c) -> :m :a -> :n -> :m :c))
    ;; Manipulate at the element level
    (push
     "Return the collection with an element added to the front."
     (:a -> :m :a -> :m :a))
    (push-end
     "Return the collection with an element added to the end."
     (:a -> :m :a -> :m :a))
    (insert-at
     "Return the collection with an element inserted at an index."
     (UFix -> :a -> :m :a -> :m :a)))

  (define-class (LinearCollection :m => ImmutableLinearCollection :m))

  (define-class (LinearCollection :m => MutableLinearCollection :m)
    (reverse!
     "Reverse the collection in place. The collection is returned for convenience."
     (:m :a -> :m :a))
    (sort!
     "Sort a collection of orderable elements in place. The collection is returned for convenience."
     (Ord :a => :m :a -> :m :a))
    (sort-with!
     "Sort the collection in place under the given ordering. The collection is returned for convenience."
     ((:a -> :a -> Ord) -> :m :a -> :m :a))
    (push!
     "Add an element to the front of the collection. The collection is returned for convenience."
     (:a -> :m :a -> :m :a))
    (push-end!
     "Add an element to the end of the collection. The collection is returned for convenience."
     (:a -> :m :a -> :m :a))
    (pop!
     "Remove the first element of the collection and return it, if any."
     (:m :a -> Optional :a))
    (pop!#
     "Remove the first element of the collection and return it, erroring if none is found."
     (:m :a -> :a))
    (pop-end!
     "Remove the last element of the collection and return it, if any."
     (:m :a -> Optional :a))
    (pop-end!#
     "Remove the last element of the collection and return it, erroring if none is found."
     (:m :a -> :a))
    (insert-at!
     "Insert an item at the given index of the collection, erroring if out of bounds. The collection is returned for convenience."
     (UFix -> :a -> :m :a -> :m :a))))

;; #+sb-package-locks
;; (sb-ext:lock-package "COALTON-LIBRARY/COLLECTIONS/CLASSES")
