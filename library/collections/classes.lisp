(coalton-library/utils:defstdlib-package #:coalton-library/collections/classes
  (:use
   #:coalton
   #:coalton-library/functions
   #:coalton-library/classes)
  (:local-nicknames
   (#:opt #:coalton-library/optional)
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
   #:size
   #:contains-elt?
   #:contains-where?
   #:count-where
   #:add

   #:ImmutableCollection

   #:MutableCollection
   #:copy
   #:filter!
   #:remove-duplicates!
   #:remove-elt!
   #:add!
   
   #:LinearCollection
   #:head
   #:head#
   #:last
   #:last#
   #:tail
   #:at
   #:at#
   #:take
   #:drop
   #:length
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

   #:zip
   #:zip-with

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
   #:set-at!))

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
    ;; Query the collection
    (empty?
     "Check if the collection contains no elements."
     (:m -> Boolean))
    (size
     "The number of elements in the collection"
     (:m -> UFix))
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
    (filter!
     "Remove elements from the collection that do not satisfy the predicate. Returns the collection for convenience."
     ((:a -> Boolean) -> :m -> :m))
    (remove-duplicates!
     "Remove duplicate elements from the collection. Returns the collection for convenience."
     (Eq :a => :m -> :m))
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
  (define-class (Collection :m :a => LinearCollection :m :a (:m -> :a))
    ;; Get elements from the collection
    (head#
     "Return the first element of the collection, erroring if it does not exist."
     (:m -> :a))
    (last#
     "Return the last element of the collection, erroring if it does not exist."
     (:m -> :a))
    (tail
     "Return all except the first element of the collection."
     (:m -> :m))
    (at
     "Return the element at the given index of the collection."
     (UFix -> :m -> Optional :a))
    (take
     "Return the first `n` elements of the collection."
     (UFix -> :m -> :m))
    (drop
     "Return all except the first `n` elements of the collection."
     (UFix -> :m -> :m))
    ;; Query the collection
    (index-where
     "Return the index of the first element matching a predicate function."
     ((:a -> Boolean) -> :m -> Optional UFix))
    (find-where
     "Return the first element matching a predicate function."
     ((:a -> Boolean) -> :m -> Optional :a))
    (indices-elt
     "Return the indices of every occurence of `elt` in the collection."
     (Eq :a => :a -> :m -> List UFix))
    (indices-where
     "Return the indices of every element satisfying `pred` in the collection."
     ((:a -> Boolean) -> :m -> List UFix))
    ;; Retrieve subsets of the collection.
    (subseq
     "Extract the collection from `start` (inclusive) to `end` (exclusive)."
     (UFix -> UFix -> :m -> :m))
    (split-at
     "Split into two collections at `i`. The first collection ends at `i`-1, the second collection begins at `i`+1."
     (UFix -> :m -> Tuple :m :m))
    (split-elt
     "Split into collections on the each occurrence of `elt`. `elt` will be excluded from the resulting collections."
     (Eq :a => :a -> :m -> List :m))
    (split-where
     "Split into collections on each element satisfying `pred`. Elements satisfying `pred` will be excluded from the resulting collections."
     ((:a -> Boolean) -> :m -> List :m))
    ;; Manipulate at the collection level
    (reverse
     "Return the collection with elements reversed."
     (:m -> :m))
    (sort
     "Return a sorted collection of orderable elements."
     (Ord :a => :m -> :m))
    (sort-with
     "Return the sorted collection under the given ordering."
     ((:a -> :a -> Ord) -> :m -> :m))
    ;; Manipulate at the element level
    (push
     "Return the collection with an element added to the front."
     (:a -> :m -> :m))
    (push-end
     "Return the collection with an element added to the end."
     (:a -> :m -> :m))
    (insert-at
     "Return the collection with an element inserted at an index, erroring if out of bounds."
     (UFix -> :a -> :m -> :m))
    (remove-at
     "Return the collection with the element at an index removed and the element."
     (UFix -> :m -> Optional (Tuple :a :m)))
    (set-at
     "Return the collection with the element set at at an index, erroring if out of bounds."
     (UFix -> :a -> :m -> :m)))

  (define-class (LinearCollection :m :a => ImmutableLinearCollection :m :a))

  (define-class (LinearCollection :m :a => MutableLinearCollection :m :a)
    (reverse!
     "Reverse the collection in place. The collection is returned for convenience."
     (:m -> :m))
    (sort!
     "Sort a collection of orderable elements in place. The collection is returned for convenience."
     (Ord :a => :m -> :m))
    (sort-with!
     "Sort the collection in place under the given ordering. The collection is returned for convenience."
     ((:a -> :a -> Ord) -> :m -> :m))
    (push!
     "Add an element to the front of the collection. The collection is returned for convenience."
     (:a -> :m -> :m))
    (push-end!
     "Add an element to the end of the collection. The collection is returned for convenience."
     (:a -> :m -> :m))
    (pop!#
     "Remove the first element of the collection and return it, erroring if none is found."
     (:m -> :a))
    (pop-end!#
     "Remove the last element of the collection and return it, erroring if none is found."
     (:m -> :a))
    (insert-at!
     "Insert an item at the given index of the collection, erroring if out of bounds. The collection is returned for convenience."
     (UFix -> :a -> :m -> :m))
    (remove-at!
     "Remove an item at the given index of the collection and return it."
     (UFix -> :m -> Optional :a))
    (set-at!
     "Set the item at the given index of the collection, erroring if out of bounds. The collection is returned for convenience."
     (UFix -> :a -> :m -> :m))))

(coalton-toplevel
  (declare contains-elt? ((Collection :m :a) (Eq :a) => :a -> :m -> Boolean))
  (define (contains-elt? elt coll)
     "Check if the collection contains an element."
    (inline (contains-where? (== elt) coll)))

  (declare remove-elt ((Collection :m :a) (Eq :a) => :a -> :m -> :m))
  (define (remove-elt elt coll)
    "Remove all occurrences of `elt` from the collection."
    (filter (/= elt) coll))

  (declare remove-elt! ((MutableCollection :m :a) (Eq :a) => :a -> :m -> :m))
  (define (remove-elt! elt coll)
    "Remove all occurrences of `elt` from the collection. The collection is returned for convenience."
    (filter! (/= elt) coll))

  (declare head (LinearCollection :m :a => :m -> Optional :a))
  (define (head coll)
    "Return the first element of the collection."
    (if (empty? coll)
        None
        (Some (inline (head# coll)))))

  (declare last (LinearCollection :m :a => :m -> Optional :a))
  (define (last coll)
    "Return the last element of the collection."
    (if (empty? coll)
        None
        (Some (inline (last# coll)))))

  (declare at# (LinearCollection :m :a => UFix -> :m -> :a))
  (define (at# i coll)
    "Return the element at the given index of the collection, erroring if it does not exist.."
    (opt:from-some "Index out of bounds." (inline (at i coll))))

  (declare index-elt ((LinearCollection :m :a) (Eq :a) => :a -> :m -> Optional UFix))
  (define (index-elt elt coll)
     "Return the index of the first occurence of `elt`, if it can be found."
    (inline (index-where (== elt) coll)))

  (declare index-elt# ((LinearCollection :m :a) (Eq :a) => :a -> :m -> UFix))
  (define (index-elt# elt coll)
     "Return the index of the first occurence of `elt`, erroring if it cannot be found."
    (opt:from-some "Cannot find element in collection." (inline (index-elt elt coll))))

  (declare index-where# (LinearCollection :m :a => (:a -> Boolean) -> :m -> UFix))
  (define (index-where# f coll)
     "Return the index of the first element matching a predicate function, erroring if none can be found."
    (opt:from-some "Cannot find matching element in collection." (inline (index-where f coll))))

  (declare length (LinearCollection :m :a => :m -> UFix))
  (define length size)

  ;; (declare zip ((itr:IntoIterator :m :a) (itr:IntoIterator :n :b) ; (LinearCollection :c (Tuple :a :b))
  ;;               (itr:FromIterator :c (Tuple :a :b))
  ;;               => :m -> :n -> :c))
  ;; (declare zip
  ;;          ((ITR:FROMITERATOR :E (TUPLE :A :B)) (ITR:INTOITERATOR :C :A) (ITR:INTOITERATOR :D :B) => :C -> :D -> :E))
  ;; See: https://github.com/coalton-lang/coalton/issues/1643
  (define (zip as bs)
    "Return a collection of two iterable object's items zipped together."
    (itr:collect! (itr:zip! (itr:into-iter as) (itr:into-iter bs))))

  (declare zip-with ((itr:IntoIterator :m :a) (itr:IntoIterator :n :b) (LinearCollection :t :c)
                     (itr:FromIterator :t :c) => (:a -> :b -> :c) -> :m -> :n -> :t))
  (define (zip-with f a b)
    "Return a collection of two iterable objects' elements applied to `f`."
    (itr:collect! (itr:zip-with! f (itr:into-iter a) (itr:into-iter b))))

  (declare remove-at# (LinearCollection :m :a => UFix -> :m -> Tuple :a :m))
  (define (remove-at# i coll)
    "Return the collection with the element at an index removed and the element."
    (opt:from-some "Collection index out of bounds." (inline (remove-at i coll))))

  (declare pop! (MutableLinearCollection :m :a => :m -> Optional :a))
  (define (pop! coll)
    "Remove the first element of the collection and return it, if any."
    (if (empty? coll)
        None
        (Some (inline (pop!# coll)))))

  (declare pop-end! (MutableLinearCollection :m :a => :m -> Optional :a))
  (define (pop-end! coll)
    "Remove the last element of the collection and return it, if any."
    (if (empty? coll)
        None
        (Some (inline (pop-end!# coll)))))

  (declare remove-at!# (MutableLinearCollection :m :a => UFix -> :m -> :a))
  (define (remove-at!# i coll)
    "Remove an item at the given index of the collection and return it."
    (opt:from-some "Collection index out of bounds." (inline (remove-at! i coll))))
  )

;; #+sb-package-locks
;; (sb-ext:lock-package "COALTON-LIBRARY/COLLECTIONS/CLASSES")
