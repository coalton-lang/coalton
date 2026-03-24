# Iterator Protocol

Coalton's iterators provide a lazy, forward-only traversal of sequences of values. They are the primary abstraction for looping and data transformation in Coalton.

## The Iterator Type

An `Iterator :elt` is defined as a function that, when called, returns either `(Some value)` for the next element or `None` when the sequence is exhausted:

```lisp
;; The Iterator type (simplified)
(define-type (Iterator :elt)
  (%Iterator (Void -> (Optional :elt)) (Optional UFix)))
```

The second field is an optional size hint that some operations use for pre-allocation.

## Creating Iterators

### From Collections

Any type that implements `IntoIterator` can be converted into an iterator with `into-iter`:

```lisp
(coalton-toplevel
  (define list-iter (iter:into-iter (make-list 1 2 3)))
  (define vec-iter  (iter:into-iter (vec:make 10 20 30)))
  (define str-iter  (iter:into-iter "hello")))
```

### Range Iterators

```lisp
(coalton-toplevel
  ;; 0, 1, 2, ..., 9
  (define ten (iter:up-to 10))

  ;; 0, 1, 2, ..., 10 (inclusive)
  (define eleven (iter:up-through 10))

  ;; Custom step: 0, 2, 4, 6, 8
  (define evens (iter:range-increasing 2 0 10))

  ;; Counting down: 10, 8, 6, 4, 2
  (define countdown (iter:range-decreasing 2 10 0)))
```

### Other Constructors

```lisp
(coalton-toplevel
  ;; Repeat a single value forever
  (define ones (iter:repeat 1))

  ;; Repeat a value a fixed number of times
  (define five-zeros (iter:repeat-for 0 5))

  ;; Yield a single element
  (define just-one (iter:once 42))

  ;; Empty iterator
  (define nothing iter:empty)

  ;; Count forever: 0, 1, 2, 3, ...
  (define naturals (iter:count-forever))

  ;; Build from a function
  (define custom (iter:new (fn () (Some 1)))))
```

## Consuming Iterators

Iterators are consumed by calling `next!` repeatedly. Most users will not call `next!` directly, but instead use higher-level consumers.

**Important:** Consuming operations are destructive — they advance the iterator and the consumed elements are gone. An iterator can only be traversed once.

### Collecting into a Container

```lisp
;; Collect into a list
(the (List Integer) (iter:collect! (iter:up-to 5)))
;; => (0 1 2 3 4)

;; Collect into a vector
(the (Vector Integer) (iter:collect! (iter:up-to 5)))
```

### Folding

```lisp
;; Sum via fold
(iter:fold! + 0 (iter:up-to 10))  ;; => 45

;; Specialized sum
(iter:sum! (iter:up-to 10))  ;; => 45
```

### Iterating for Side Effects

```lisp
(iter:for-each! show
                (iter:up-to 5))
```

### Searching

```lisp
;; Find the first even number
(iter:find! even? (iter:into-iter (make-list 1 3 4 5)))
;; => (Some 4)

;; Find the index of an element
(iter:index-of! (== 3) (iter:into-iter (make-list 1 2 3 4)))
;; => (Some 2)
```

### Aggregation

```lisp
(iter:count!  (iter:up-to 10))               ;; => 10
(iter:every!  positive? (iter:up-to 10))      ;; => False (0 is not positive)
(iter:any!    even? (iter:up-to 10))          ;; => True
(iter:max!    (iter:into-iter (make-list 3 1 4 1 5)))  ;; => (Some 5)
(iter:min!    (iter:into-iter (make-list 3 1 4 1 5)))  ;; => (Some 1)
```

## Transforming Iterators

Transformations create new iterators that lazily apply operations as elements are consumed. They do not process any elements until something downstream consumes the result.

### Mapping and Filtering

```lisp
;; Map a function over elements
(iter:collect! (map (* 2) (iter:up-to 5)))
;; => (0 2 4 6 8)

;; Filter elements
(iter:collect! (iter:filter! even? (iter:up-to 10)))
;; => (0 2 4 6 8)

;; Combined map and filter
(iter:collect! (iter:filter-map! (fn (x) (if (even? x) (Some (* x x)) None))
                                 (iter:up-to 10)))
;; => (0 4 16 36 64)
```

### Taking and Chaining

```lisp
;; Take the first n elements
(iter:collect! (iter:take! 3 (iter:count-forever)))
;; => (0 1 2)

;; Chain two iterators
(iter:collect!
  (iter:chain! (iter:up-to 3) (iter:up-to 3)))
;; => (0 1 2 0 1 2)
```

### Zipping

```lisp
;; Zip two iterators into pairs
(iter:collect!
  (iter:zip! (iter:up-to 3)
             (iter:into-iter (make-list "a" "b" "c"))))
;; => ((Tuple 0 "a") (Tuple 1 "b") (Tuple 2 "c"))

;; Zip with a custom combiner
(iter:collect!
  (iter:zip-with! + (iter:up-to 3) (iter:up-to 3)))
;; => (0 2 4)

;; Enumerate: pair each element with its index
(iter:collect! (iter:enumerate! (iter:into-iter "abc")))
;; => ((Tuple 0 #\a) (Tuple 1 #\b) (Tuple 2 #\c))
```

### Flattening

```lisp
;; Flatten nested iterators
(iter:collect!
  (iter:flatten! (map (fn (x) (iter:up-to x))
                      (iter:into-iter (make-list 1 2 3)))))
;; => (0 0 1 0 1 2)

;; Flat-map (map then flatten)
(iter:collect!
  (iter:flat-map! (fn (x) (iter:repeat-for 2 x))
                  (iter:into-iter (make-list 1 2 3))))
;; => (1 1 2 2 3 3)
```

## Protocol Guarantees and Caveats

### Once-only Traversal

An iterator can only be traversed once. After `next!` returns `None`, subsequent calls to `next!` should also return `None`, but this behavior is not strictly enforced by all iterator constructors.

### Behavior After Exhaustion

The behavior of calling `next!` on an exhausted iterator (one that has already returned `None`) is currently unspecified. Most standard library iterators will continue to return `None`, but consumers should not rely on this. Best practice: stop consuming after the first `None`.

### Thread Safety

Iterators are not thread-safe. Calling `next!` from multiple threads concurrently on the same iterator without external synchronization is undefined behavior.

### Draining

Functions like `collect!`, `fold!`, and `for-each!` fully drain an iterator. Functions like `find!` and `any!` may stop early (short-circuit) and leave the iterator partially consumed.

### Laziness

Transformations like `map`, `filter!`, and `take!` are lazy — they do not process elements until the resulting iterator is consumed. This means you can build chains of transformations without allocating intermediate collections:

```lisp
;; No intermediate lists are created here
(iter:sum!
  (map (* 2)
    (iter:filter! odd?
      (iter:up-to 1000))))
```

## Common Patterns

### Pipeline Style

```lisp
(coalton-toplevel
  (define (sum-of-even-squares n)
    (pipe (iter:up-to n)
          (iter:filter! even?)
          (map (fn (x) (* x x)))
          iter:sum!)))
```

### Building Collections from Iterators

Use the `FromIterator` type class via `collect!`:

```lisp
(coalton-toplevel
  ;; Type annotation determines the output collection type
  (declare evens-list (List Integer))
  (define evens-list
    (iter:collect! (iter:filter! even? (iter:up-to 20)))))
```

### Mutable Accumulation

```lisp
(coalton-toplevel
  (define (group-by-parity n)
    (let ((evens (vec:new))
          (odds  (vec:new))
          (iter  (iter:up-to n)))
      (for ()
        (let next = (iter:next! iter))
        (when (== next None)
          (break))
        (match next
          ((Some i)
           (if (even? i)
               (vec:push! i evens)
               (vec:push! i odds)))
          ((None) Unit)))
      (Tuple evens odds))))
```
