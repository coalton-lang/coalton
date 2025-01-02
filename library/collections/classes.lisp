(coalton-library/utils:defstdlib-package #:coalton-library/collections/classes
  (:use
   #:coalton
   #:coalton-library/functions
   #:coalton-library/classes)
  (:local-nicknames
   (#:itr #:coalton-library/iterator)
   (#:l #:coalton-library/collections/immutable/list)
   (#:types #:coalton-library/types)
   (#:o #:coalton-library/optional))
  (:export
   #:Collection
   #:EqCollection
   #:NestedCollection
   #:new-collection
   #:new-repeat
   #:new-from
   #:new-convert
   #:flatten
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
   #:add!
   
   #:LinearCollection
   #:head
   #:head#
   #:last
   #:last#
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

;;; NOTE:
;;; We have to define `Collection` with functional dependencies
;;; to allow `KeyedCollection`s to be collections of tuples.
;;;
;;; Because Coalton doesn't support typeclass functions that provide
;;; extra constraints on the typeclass's type parameters, we need
;;; to create separate typeclasses (`EqCollection`, etc) for each
;;; constraint we need to add. In other words, we'd like to do
;;; this but we can't:
;;;
;;; (define-class (Collection :m :a (:m -> :a))
;;;   (new-collection
;;;     (Unit -> :m))
;;;   ...
;;;   (contains-elt?
;;;     (Eq :a => :a -> :m -> Boolean)))
;;;
;;; Finally, for the moment we're not able to actually constrain
;;; the type parameters in EqCollection, etc. See:
;;; https://github.com/coalton-lang/coalton/issues/1340
;;;
;;; When that is fixed, we should go back and cover them in the
;;; appropriate constraints.

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
    ;; Query the collection
    (empty?
     "Check if the collection contains no elements."
     (:m -> Boolean))
    (length
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

  ;; Instances should wrap each method in Eq :a
  (define-class (Collection :m :a => EqCollection :m :a (:m -> :a))
    (remove-duplicates
     "Create a new collection with all distinct elements."
     (:m -> :m))
    (contains-elt?
     "Check if the collection contains an element."
     (:a -> :m -> Boolean))
    (remove-elt
     "Remove all occurrences of `elt` from the collection."
     (:a -> :m -> :m)))

  (define-class ((Collection :n :m)
                 (Collection :m :a) =>
                 NestedCollection :n :m :a (:n :m -> :a))
    (flatten
     "Flatten a collection of collections into a collection of their elements."
     (:n -> :m)))

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

;; TODO: Make it so that these must all be the proper KeyedCollection types as well
(coalton-toplevel
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
    ;; Retrieve subsets of the collection.
    ;; NOTE: In all cases, these should return collections that don't share mutable state with the original.
    (subseq
     "Extract the collection from `start` (inclusive) to `end` (exclusive)."
     (UFix -> UFix -> :m :a -> :m :a))
    (split-at
     "Split into two collections at `i`. The second collection begins with element at index `i`."
     (UFix -> :m :a -> Tuple (:m :a) (:m :a)))
    (split-elt
     "Split into two collections at the first occurrence of `elt`. The second collection begins with `elt`.
The second collection is empty if `elt` cannot be found."
     (Eq :a => :a -> :m :a -> Tuple (:m :a) (:m :a)))
    (split-where
     "Split into two collections at the first element that satisfies `pred`. The second collection begins
with that element. The second collection is empty if no element satisfied `pred`."
     ((:a -> Boolean) -> :m :a -> Tuple (:m :a) (:m :a)))
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

;; TODO: Because `List` is a predefined type, we can't define this
;; in the new collections/immutable/list.lisp file. Define it here
;; for now, and when we remove the deprecated versions move this
;; into the list package.
(coalton-toplevel
  (define-instance (Collection (List :a) :a)
    (define (new-collection)
      Nil)
    (define (new-repeat n elt)
      (l:repeat n elt))
    (define (new-from n f)
      (if (== 0 n)
        Nil
        (map f (l:range 0 (- n 1)))))
    (define (new-convert coll)
      (itr:collect! (itr:into-iter coll)))
    ;; (define (flatten lst)
    ;;   (fold l:append (new-collection) lst))
    (define filter l:filter)
    (define empty? l:empty?)
    (define length l:length)
    (define contains-where? l:contains-where?)
    (define count-where l:countBy)
    (define add Cons))

  (define-instance (Eq :a => EqCollection (List :a) :a)
    (define remove-duplicates l:remove-duplicates)
    (define contains-elt? l:contains-elt?)
    (define (remove-elt elt lst)
      (l:filter (/= elt) lst)))

  (define-instance (NestedCollection (List (List :a)) (List :a) :a)
    (define (flatten lst)
      (fold l:append (new-collection) lst)))

  (define-instance (ImmutableCollection (List :a) :a)))

(coalton-toplevel
  (define-instance (LinearCollection List)
    (define head l:head)
    (define (head# lst)
      (o:from-some "Attempted to retrieve head of empty list." (l:head lst)))
    (define last l:last)
    (define (last# lst)
      (o:from-some "Attempted to retrieve last element of empty list." (l:last lst)))
    (define index-elt l:elemIndex)
    (define (index-elt# elt lst)
      (o:from-some "Cannot find element in list." (l:elemIndex elt lst)))
    (define index-where l:findIndex)
    (define (index-where# pred lst)
      (o:from-some "Cannot find matching element in list." (l:findIndex pred lst)))
    (define find-where l:find)
    (define subseq l:subseq-list)
    (define (split-at i lst)
      (Tuple (l:take i lst) (l:drop i lst)))
    (define (split-elt elt lst)
      (match (l:elemIndex elt lst)
        ((None) (Tuple lst Nil))
        ((Some i) (split-at i lst))))
    (define (split-where pred lst)
      (match (l:findIndex pred lst)
        ((None) (Tuple lst Nil))
        ((Some i) (split-at i lst))))
    (define reverse l:reverse)
    (define sort l:sort)
    (define sort-with l:sortBy)
    (define zip l:zip-itr)
    (define zip-with l:zip-with-itr)
    (define push Cons)
    (define push-end l:push-end)
    (define insert-at l:insert-at)))

;; #+sb-package-locks
;; (sb-ext:lock-package "COALTON-LIBRARY/COLLECTIONS/CLASSES")
