# Collections Guide

Coalton's standard library provides several collection types, each with different trade-offs for mutability, persistence, and performance. This guide helps you choose the right collection for your use case.

## Overview

| Collection | Mutable? | Ordered? | Keyed? | Package |
|------------|----------|----------|--------|---------|
| `List` | No | Yes | By position | `coalton/list` |
| `Vector` | Yes | Yes | By index | `coalton/vector` |
| `Seq` | No | Yes | By index | `coalton/seq` |
| `Queue` | Yes | Yes (FIFO) | By position | `coalton/queue` |
| `Hashtable` | Yes | No | By key | `coalton/hashtable` |
| `HashMap` | No | No | By key | `coalton/hashmap` |
| `Cell` | Yes | N/A | N/A | `coalton/cell` |
| `Slice` | No | Yes | By index | `coalton/slice` |

## List

A singly-linked persistent list. This is the most fundamental collection in Coalton and implements `Functor`, `Applicative`, `Monad`, `Foldable`, `Traversable`, `Semigroup`, `Monoid`, and `Eq`.

**Key characteristics:**
- Immutable — all operations return new lists
- O(1) prepend (`Cons`), O(n) append, O(n) access by index
- Pattern-matchable with `Cons` and `Nil` constructors
- Every Coalton `List` is also a valid Common Lisp list

```lisp
(coalton-toplevel
  (define my-list (make-list 1 2 3 4 5))

  (define first-elem (list:head my-list))      ;; => (Some 1)
  (define rest-elems (list:tail my-list))       ;; => (Some (2 3 4 5))
  (define reversed (list:reverse my-list))      ;; => (5 4 3 2 1)
  (define filtered (list:filter even? my-list)) ;; => (2 4)
  (define mapped (map (+ 10) my-list)))         ;; => (11 12 13 14 15)
```

**When to use:** Default choice for sequential data that is consumed by pattern matching, mapping, or folding. Ideal when you build data by prepending elements.

## Vector

A mutable, growable array backed by a Common Lisp adjustable vector with a fill pointer.

**Key characteristics:**
- Mutable — operations like `push!`, `pop!`, `set!` modify in place
- O(1) amortized `push!` and `pop!` at the end, O(1) random access by index
- O(n) for insertion/removal at arbitrary positions

```lisp
(coalton-toplevel
  (define (build-squares n)
    (let ((v (vec:new)))
      (for i in (iter:up-to n)
        (vec:push! (* i i) v))
      v)))
```

**When to use:** When you need mutable, indexed storage. Good for building collections incrementally with `push!`, for algorithms that need random access, and for interop with Common Lisp code expecting arrays.

## Seq

A persistent (immutable) sequence based on Relaxed Radix Balanced Trees (RRB-Trees).

**Key characteristics:**
- Immutable — `push` and `put` return new sequences
- O(log n) random access, update, and append
- O(log n) `push` (append to end) and `pop` (remove from end)
- Efficient concatenation with `conc`

```lisp
(coalton-toplevel
  (define my-seq (seq:make 1 2 3 4 5))

  (define with-six (seq:push 6 my-seq))   ;; => (Seq 1 2 3 4 5 6)
  (define third (seq:get 2 my-seq))        ;; => (Some 3)
  (define updated (seq:put 0 99 my-seq)))  ;; => (Seq 99 2 3 4 5)
```

**When to use:** When you need a persistent (immutable) sequence with efficient random access and concatenation. Preferred over `List` when you need indexed access or append-heavy workloads without mutation.

## Queue

A mutable FIFO (first-in, first-out) queue.

**Key characteristics:**
- Mutable — `push!` and `pop!` modify in place
- O(1) `push!` (enqueue at back) and O(1) `pop!` (dequeue from front)
- O(1) `peek` at the front element

```lisp
(coalton-toplevel
  (define (process-work-items items)
    (let ((q (queue:new)))
      (for item in items
        (queue:push! item q))
      ;; Process items in FIFO order
      (while-let (Some item) = (queue:pop! q)
        (process item)))))
```

**When to use:** When you need FIFO ordering — task queues, breadth-first search, buffering.

## Hashtable

A mutable hash table mapping keys to values. Keys must implement the `Hash` type class.

**Key characteristics:**
- Mutable — `set!` and `remove!` modify in place
- O(1) average-case lookup, insertion, and deletion
- Keys must implement `Hash` (and `Eq`)
- Backed by Common Lisp's hash table implementation

```lisp
(coalton-toplevel
  (define (word-frequencies words)
    (let ((ht (hashtable:new)))
      (for word in words
        (match (hashtable:get ht word)
          ((Some count) (hashtable:set! ht word (+ count 1)))
          ((None) (hashtable:set! ht word 1))))
      ht)))
```

**When to use:** When you need fast mutable key-value storage. The go-to choice for caches, frequency counters, and lookup tables that are built once and queried many times.

## HashMap

A persistent (immutable) hash map based on Hash Array Mapped Tries (HAMT).

**Key characteristics:**
- Immutable — `insert`, `remove`, and `update` return new maps
- O(log32 n) ≈ O(1) lookup, insertion, and deletion
- Keys must implement `Hash` (and `Eq`)
- Supports set operations: `union`, `intersection`, `difference`, `xor`

```lisp
(coalton-toplevel
  (define empty-map (hashmap:empty))

  (define my-map
    (pipe empty-map
          (hashmap:insert "alice" 42)
          (hashmap:insert "bob" 37)))

  (define alice-age (hashmap:lookup my-map "alice"))) ;; => (Some 42)
```

**When to use:** When you need an immutable key-value map. Preferred over `Hashtable` when you need persistence (keeping old versions), structural sharing, or thread safety without locks.

## Cell

A mutable single-value container (a mutable reference).

**Key characteristics:**
- Mutable — `write!` and `update!` modify the contained value
- O(1) read and write
- Used for local mutable state within otherwise-pure code

```lisp
(coalton-toplevel
  (define (sum-list lst)
    (let ((acc (cell:new 0)))
      (for x in lst
        (cell:update! (+ x) acc))
      (cell:read acc))))
```

**When to use:** When you need a mutable variable in a local scope — loop accumulators, counters, caches. `Cell` is Coalton's equivalent of a mutable reference.

## Slice

An immutable view into a contiguous subsequence of a `Vector`.

**Key characteristics:**
- Immutable view — does not copy data
- O(1) creation from a vector, O(1) indexed access within the slice

**When to use:** When you need to work with a subrange of a vector without copying.

## Choosing the Right Collection

**Do you need mutation?**
- Yes → `Vector` (indexed), `Hashtable` (keyed), `Queue` (FIFO), `Cell` (single value)
- No → `List` (sequential), `Seq` (indexed), `HashMap` (keyed)

**Do you need key-value lookup?**
- Mutable → `Hashtable`
- Immutable → `HashMap`

**Do you need indexed random access?**
- Mutable → `Vector`
- Immutable → `Seq`

**Do you need to pattern match on the structure?**
- Use `List` — it's the only collection with pattern-matchable constructors (`Cons`, `Nil`)

**Do you need FIFO ordering?**
- Use `Queue`

**Is performance critical?**
- For tight loops with indexed access → `Vector`
- For key lookups → `Hashtable` (mutable) or `HashMap` (immutable)
- For prepend-heavy workloads → `List`
- For append and concatenation → `Seq`

## Iterators and Collections

All collections can be converted to iterators via the `IntoIterator` type class. This means you can use `for` loops with any collection:

```lisp
(for x in my-list   (process x))
(for x in my-vector (process x))
(for x in my-seq    (process x))
```

To build a collection from an iterator, use `collect!` with a type annotation:

```lisp
(the (List Integer)   (iter:collect! my-iterator))
(the (Vector Integer) (iter:collect! my-iterator))
```

See the [Iterator Protocol](./iterator-protocol.md) document for details on working with iterators.
