# Coalton Collections Library

## Functions Refrence

All of these functions are in the `coalton-prelude` package, and don't need to be imported.

#### Reading the Type Signatures

In the type signatures, `:a` is the element type of the collection and `:m` is the type of the collection itself.

For example, the signature for `contains-elt?` is `:a -> :m -> Boolean`. If you call `(contains-elt? (make-list 1 2 3))`, then `:m` is `List` and `:a` is `Integer`.

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
| `size`            | :m -> UFix                                                      | The number of elements in the collection.                                                                                                                                                                                               |
| `contains-where?`   | (:a -> Boolean) -> :m -> Boolean                                | Check if the collection contains an element satisfying the predicate.                                                                                                                                                                   |
| `contains-elt?`     | Eq :a => :a -> :m -> Boolean           | Check if the collection contains an element.                    |
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
| `head`            | :m -> Optional :a                                   | Return the first element of the collection.                                                                                                             |
| `head#`           | :m -> :a                                           | Return the first element of the collection, erroring if it does not exist.                                                                              |
| `last`            | :m -> Optional :a                                   | Return the last element of the collection.                                                                                                              |
| `last#`           | :m -> :a                                           | Return the last element of the collection, erroring if it does not exist.                                                                               |
| `tail`            | :m -> :m                                        | Return all except the first element of the collection.                                                                                                  |
| `take`            | UFix -> :m -> :m                                 | Return the first `n` elements of the collection.                                                                                                        |
| `drop`            | UFix -> :m -> :m                                 | Return all except the first `n` elements of the collection.                                                                                             |
| `length`            | :m -> UFix                                                      | The number of elements in the linear collection. (Alias for `size`)                                                                                                                                                                                               |
| `index-elt`       | Eq :a => :a -> :m -> Optional UFix                  | Return the index of the first occurrence of `elt`, if it can be found.                                                                                  |
| `index-elt#`      | Eq :a => :a -> :m -> UFix                           | Return the index of the first occurrence of `elt`, erroring if it cannot be found.                                                                      |
| `index-where`     | (:a -> Boolean) -> :m -> Optional UFix             | Return the index of the first element matching a predicate function.                                                                                    |
| `index-where#`    | (:a -> Boolean) -> :m -> UFix                      | Return the index of the first element matching a predicate function, erroring if none can be found.                                                                 |
| `find-where`      | (:a -> Boolean) -> :m -> Optional :a               | Return the first element matching a predicate function.                                                                                                 |
| `subseq`          | UFix -> UFix -> :m -> :m                         | Extract the collection from `start` (inclusive) to `end` (exclusive).                                                                                    |
| `split-at`        | UFix -> :m :a -> Tuple :m :m                | Split into two collections at `i`. The second collection begins with element at index `i`.                                                              |
| `split-elt`       | Eq :a => :a -> :m :a -> Tuple :m :m)         | Split into two collections at the first occurrence of `elt`. The second collection begins with `elt`. The second collection is empty if `elt` is not found. |
| `split-where`     | (:a -> Boolean) -> :m :a -> Tuple :m :m     | Split into two collections at the first element that satisfies `pred`. The second collection begins with that element. Empty if no element satisfied `pred`.   |
| `reverse`         | :m -> :m                                        | Return the collection with elements reversed.                                                                                                            |
| `sort`            | Ord :a => :m -> :m                              | Return a sorted collection of orderable elements.                                                                                                       |
| `sort-with`       | (:a -> :a -> Ord) -> :m -> :m                   | Return the sorted collection under the given ordering.                                                                                                  |
| `push`            | :a -> :m -> :m                                  | Return the collection with an element added to the front.                                                                                                |
| `push-end`        | :a -> :m -> :m                                  | Return the collection with an element added to the end.                                                                                                  |
| `insert-at`       | UFix -> :a -> :m -> :m                           | Return the collection with an element inserted at an index, erroring if out of bounds.                                                                                             |
| `set-at`       | UFix -> :a -> :m -> :m                           | Return the collection with the element set at at an index, erroring if out of bounds.  |

#### MutableLinearCollection

| Function Name  | Type Signature                                         | Docstring                                                                                                                                                   |
|----------------|--------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `reverse!`     | :m -> :m                                        | Reverse the collection in place. The collection is returned for convenience.                                                                                |
| `sort!`        | Ord :a => :m -> :m                              | Sort a collection of orderable elements in place. The collection is returned for convenience.                                                               |
| `sort-with!`   | (:a -> :a -> Ord) -> :m -> :m                   | Sort the collection in place under the given ordering. The collection is returned for convenience.                                                          |
| `push!`        | :a -> :m -> :m                                  | Add an element to the front of the collection. The collection is returned for convenience.                                                                  |
| `push-end!`    | :a -> :m -> :m                                  | Add an element to the end of the collection. The collection is returned for convenience.                                                                    |
| `pop!`         | :m -> Optional :a                                   | Remove the first element of the collection and return it, if any.                                                                                           |
| `pop!#`        | :m -> :a                                           | Remove the first element of the collection and return it, erroring if none is found.                                                                        |
| `pop-end!`     | :m -> Optional :a                                   | Remove the last element of the collection and return it, if any.                                                                                            |
| `pop-end!#`    | :m -> :a                                           | Remove the last element of the collection and return it, erroring if none is found.                                                                         |
| `insert-at!`   | UFix -> :a -> :m -> :m                           | Insert an item at the given index of the collection, erroring if out of bounds. The collection is returned for convenience.                                 |
| `set-at!`   | UFix -> :a -> :m -> :m                           | Set the item at the given index of the collection, erroring if out of bounds. The collection is returned for convenience.                                 |

## How to Write Generic Functions

The collection typeclasses use a special kind of generics that require setting up your generic functions a little differently. These example functions can take any kind of `Collection` or `MutableCollection`. Notice that they have to use `Collection :m UFix => :m -> ...` instead of the more typical `Collection :m => :m UFix -> ...` syntax.

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
