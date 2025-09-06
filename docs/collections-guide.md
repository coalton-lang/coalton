# Coalton Collections Library

## Functions Refrence

All of these functions are in the `coalton-prelude` package, and don't need to be imported.

#### Collection

| Function Name       | Type Signature                                                   | Docstring                                                                                                                                                                                                                               |
|---------------------|-----------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `new-collection`    | Unit -> :m                                                      | Create a new, empty collection.                                                                                                                                                                                                         |
| `new-repeat`        | UFix -> :a -> :m                                                | Create a new collection, attempting to add `elt` `n` times.                                                                                                                                                                            |
| `new-from`          | UFix -> (UFix -> :a) -> :m                                      | Create a new collection by appling a function over the range [0, n).                                                                                                                                                                    |
| `new-convert`       | (Collection :f :a) (itr:IntoIterator :f :a) => :f -> :m         | Convert a collection of another type. If converting a LinearCollection to a LinearCollection, should preserve order.                                                                                                                    |
| `filter`            | (:a -> Boolean) -> :m -> :m                                     | Create a new collection with the elements satisfying the predicate.                                                                                                                                                                     |
| `remove-duplicates` | :m -> :m                      | Create a new collection with all distinct elements.             |
| `remove-elt`        | :a -> :m -> :m                | Remove all occurrences of `elt` from the collection.            |
| `empty?`            | :m -> Boolean                                                   | Check if the collection contains no elements.                                                                                                                                                                                           |
| `length`            | :m -> UFix                                                      | The number of elements in the collection.                                                                                                                                                                                               |
| `contains-where?`   | (:a -> Boolean) -> :m -> Boolean                                | Check if the collection contains an element satisfying the predicate.                                                                                                                                                                   |
| `contains-elt?`     | :a -> :m -> Boolean           | Check if the collection contains an element.                    |
| `count-where`       | (:a -> Boolean) -> :m -> UFix                                   | The number of elements satisfying the predicate.                                                                                                                                                                                        |
| `add`               | :a -> :m -> :m                                                  | Add an element to the collection. For linear collections, should add to the front or back, depending on which is natural for the underlying data structure.                                                                              |

#### MutableCollection

| Function Name | Type Signature  | Docstring                                                       |
|--------------|------------------|-----------------------------------------------------------------|
| `copy`       | :m -> :m        | Create a shallow copy of the collection.                        |
| `add!`       | :a -> :m -> :m  | Add an element to the collection in place. See `add`.           |

#### LinearCollection

| Function Name      | Type Signature                                         | Docstring                                                                                                                                               |
|--------------------|--------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------|
| `head`            | :m :a -> Optional :a                                   | Return the first element of the collection.                                                                                                             |
| `head#`           | :m :a -> :a                                           | Return the first element of the collection, erroring if it does not exist.                                                                              |
| `last`            | :m :a -> Optional :a                                   | Return the last element of the collection.                                                                                                              |
| `last#`           | :m :a -> :a                                           | Return the last element of the collection, erroring if it does not exist.                                                                               |
| `tail`            | :m :a -> :m :a                                        | Return all except the first element of the collection.                                                                                                  |
| `take`            | UFix -> :m :a -> :m :a                                 | Return the first `n` elements of the collection.                                                                                                        |
| `drop`            | UFix -> :m :a -> :m :a                                 | Return all except the first `n` elements of the collection.                                                                                             |
| `index-elt`       | Eq :a => :a -> :m :a -> Optional UFix                  | Return the index of the first occurrence of `elt`, if it can be found.                                                                                  |
| `index-elt#`      | Eq :a => :a -> :m :a -> UFix                           | Return the index of the first occurrence of `elt`, erroring if it cannot be found.                                                                      |
| `index-where`     | (:a -> Boolean) -> :m :a -> Optional UFix             | Return the index of the first element matching a predicate function.                                                                                    |
| `index-where#`    | (:a -> Boolean) -> :m :a -> UFix                      | Return the index of the first element matching a predicate function, erroring if none can be found.                                                                 |
| `find-where`      | (:a -> Boolean) -> :m :a -> Optional :a               | Return the first element matching a predicate function.                                                                                                 |
| `subseq`          | UFix -> UFix -> :m :a -> :m :a                         | Extract the collection from `start` (inclusive) to `end` (exclusive).                                                                                    |
| `split-at`        | UFix -> :m :a -> Tuple (:m :a) (:m :a)                | Split into two collections at `i`. The second collection begins with element at index `i`.                                                              |
| `split-elt`       | Eq :a => :a -> :m :a -> Tuple (:m :a) (:m :a)         | Split into two collections at the first occurrence of `elt`. The second collection begins with `elt`. The second collection is empty if `elt` is not found. |
| `split-where`     | (:a -> Boolean) -> :m :a -> Tuple (:m :a) (:m :a)     | Split into two collections at the first element that satisfies `pred`. The second collection begins with that element. Empty if no element satisfied `pred`.   |
| `reverse`         | :m :a -> :m :a                                        | Return the collection with elements reversed.                                                                                                            |
| `sort`            | Ord :a => :m :a -> :m :a                              | Return a sorted collection of orderable elements.                                                                                                       |
| `sort-with`       | (:a -> :a -> Ord) -> :m :a -> :m :a                   | Return the sorted collection under the given ordering.                                                                                                  |
| `zip`             | itr:IntoIterator :n :b => :m :a -> :n -> :m (Tuple :a :b) | Return a collection of this collection's elements combined with elements from an iterable object.                                                           |
| `zip-with`        | itr:IntoIterator :n :b => (:a -> :b -> :c) -> :m :a -> :n -> :m :c | Return a collection of this collection's elements and an iterable object's elements applied to `f`.                                              |
| `push`            | :a -> :m :a -> :m :a                                  | Return the collection with an element added to the front.                                                                                                |
| `push-end`        | :a -> :m :a -> :m :a                                  | Return the collection with an element added to the end.                                                                                                  |
| `insert-at`       | UFix -> :a -> :m :a -> :m :a                           | Return the collection with an element inserted at an index.                                                                                             |

#### MutableLinearCollection

| Function Name  | Type Signature                                         | Docstring                                                                                                                                                   |
|----------------|--------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `reverse!`     | :m :a -> :m :a                                        | Reverse the collection in place. The collection is returned for convenience.                                                                                |
| `sort!`        | Ord :a => :m :a -> :m :a                              | Sort a collection of orderable elements in place. The collection is returned for convenience.                                                               |
| `sort-with!`   | (:a -> :a -> Ord) -> :m :a -> :m :a                   | Sort the collection in place under the given ordering. The collection is returned for convenience.                                                          |
| `push!`        | :a -> :m :a -> :m :a                                  | Add an element to the front of the collection. The collection is returned for convenience.                                                                  |
| `push-end!`    | :a -> :m :a -> :m :a                                  | Add an element to the end of the collection. The collection is returned for convenience.                                                                    |
| `pop!`         | :m :a -> Optional :a                                   | Remove the first element of the collection and return it, if any.                                                                                           |
| `pop!#`        | :m :a -> :a                                           | Remove the first element of the collection and return it, erroring if none is found.                                                                        |
| `pop-end!`     | :m :a -> Optional :a                                   | Remove the last element of the collection and return it, if any.                                                                                            |
| `pop-end!#`    | :m :a -> :a                                           | Remove the last element of the collection and return it, erroring if none is found.                                                                         |
| `insert-at!`   | UFix -> :a -> :m :a -> :m :a                           | Insert an item at the given index of the collection, erroring if out of bounds. The collection is returned for convenience.                                 |

## How to Write Generic Functions

The `Collection` and `MutableCollection` typeclass use a special kind of generics that require setting up your generic functions a little differently. These example functions can take any kind of `Collection` or `MutableCollection`. Notice that they have to use `Collection :m UFix => :m -> ...` instead of the more typical `Collection :m => :m UFix -> ...` syntax.

```lisp
(in-package :cl-user)
(defpackage :collections-demo
  (:use
    #:coalton
    #:coalton-prelude)
  (:local-nicknames
   (#:vector #:coalton-library/collections/mutable/vector)
   (#:cel #:coalton-library/cell)
   (#:iter #:coalton-library/iterator)))
(in-package :collections-demo)

(coalton-toplevel
  (declare add-zeros (Collection :m UFix => :m -> UFix -> :m))
  (define (add-zeros coll n-zeros)
    "Add N-ZEROS number of 0s to COLL."
    (let result = (cel:new coll))
    (for _ in (iter:range-increasing 1 0 n-zeros)
      (cel:write! result (add 0 (cel:read result))))
    (cel:read result))

  (declare add-zeros! (MutableCollection :m UFix => :m -> UFix -> :m))
  (define (add-zeros! mut-coll n-zeros)
    "Add N-ZEROS number of 0s to MUT-COLL."
    (for _ in (iter:range-increasing 1 0 n-zeros)
      (add! 0 mut-coll))
    mut-coll))

(coalton
 (traceobject "List" (add-zeros (make-list 1 2 3) 3))
 (traceobject "Vector" (add-zeros (vector:make 1 2 3) 3))
 (traceobject "Vector!" (add-zeros! (vector:make 1 2 3) 3)))
```

The `LinearCollection` and `MutableLinearCollection` typeclasses, however, use the traditional generic syntax.

```lisp
(coalton-toplevel
  (declare pad-zeros (LinearCollection :m => :m UFix -> UFix -> :m UFix))
  (define (pad-zeros coll n-zeros)
    "Pad the start and end of COLL with N-ZEROS number of 0s."
    (let result = (cel:new coll))
    (for _ in (iter:range-increasing 1 0 n-zeros)
      (cel:write! result (push 0 (cel:read result)))
      (cel:write! result (push-end 0 (cel:read result))))
    (cel:read result))

  (declare pad-zeros! (MutableLinearCollection :m => :m UFix -> Ufix -> :m UFix))
  (define (pad-zeros! mut-coll n-zeros)
    "Pad the stand and end of MUT-COLL with N-ZEROS number of 0s."
    (for _ in (iter:range-increasing 1 0 n-zeros)
      (push! 0 mut-coll)
      (push-end! 0 mut-coll))
    mut-coll))

(coalton
 (traceobject "List" (pad-zeros (make-list 1 2 3) 3))
 (traceobject "Vector" (pad-zeros (vector:make 1 2 3) 3))
 (traceobject "Vector!" (pad-zeros! (vector:make 1 2 3) 3)))
```
