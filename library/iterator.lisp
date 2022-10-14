(coalton-library/utils:defstdlib-package #:coalton-library/iterator
  (:shadow #:empty)
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/hash
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/optional
   #:coalton-library/tuple
   #:coalton-library/char)
  (:local-nicknames
   (#:types #:coalton-library/types)
   (#:list #:coalton-library/list)
   (#:string #:coalton-library/string)
   (#:cell #:coalton-library/cell)
   (#:vector #:coalton-library/vector)
   (#:hashtable #:coalton-library/hashtable))
  (:import-from
   #:coalton-library/vector
   #:Vector)
  (:import-from
   #:coalton-library/hashtable
   #:Hashtable)
  (:export
   #:Iterator
   #:new
   #:next!
   #:fold!
   #:empty
   #:last!
   #:IntoIterator
   #:into-iter
   #:list-iter
   #:vector-iter
   #:string-chars
   #:recursive-iter
   #:range-increasing
   #:up-to
   #:up-through
   #:range-decreasing
   #:down-from
   #:count-forever
   #:repeat-forever
   #:repeat-item
   #:char-range
   #:interleave!
   #:zip!
   #:zipWith!
   #:enumerate!
   #:filter!
   #:unwrapped!
   #:take!
   #:flatten!
   #:chain!
   #:concat!
   #:remove-duplicates!
   #:pair-with!
   #:sum!
   #:and!
   #:or!
   #:count!
   #:for-each!
   #:find!
   #:index-of!
   #:optimize!
   #:max!
   #:min!
   #:every!
   #:any!
   #:elementwise-match!
   #:elementwise==!
   #:elementwise-hash!
   #:FromIterator
   #:collect!
   #:collect-list!
   #:collect-vector-size-hint!
   #:collect-vector!
   #:collect-hashtable!))

(in-package #:coalton-library/iterator)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;; fundamental operators
(coalton-toplevel
  (repr :transparent)
  (define-type (Iterator :elt)
    "A forward-moving pointer into an ordered sequence of :ELTs"
    (%Iterator (Unit -> (Optional :elt))))

  (declare new ((Unit -> Optional :elt) -> Iterator :elt))
  (define new
    "Create a new iterator from a function that yields elements."
    %Iterator)

  (declare next! (Iterator :elt -> Optional :elt))
  (define (next! iter)
    "Advance ITER, returning its next yielded value, or `None` if the iterator is exhausted.
Behavior is undefined if two threads concurrently call `next!` on the same iterator without a lock. Note that
most of the operators defined on iterators call `next!` internally, or create new iterators which will call
`next!` on their inputs."
    (match iter
      ((%Iterator func) (func Unit))))

  (declare fold! ((:state -> :elt -> :state) -> :state -> Iterator :elt -> :state))
  (define (fold! func init iter)
    "Tail recursive in-order fold. Common Lisp calls this operation `reduce`.

If ITER is empty, returns INIT. Otherwise, calls (FUNC STATE ITEM) for each ITEM of ITER to produce a new
STATE, using INIT as the first STATE."
    (match (next! iter)
      ((Some item) (fold! func
                          (func init item)
                          iter))
      ((None) init)))

;;; instances
  ;; should iterator implement applicative or monad? the only law-abiding applicative instances are
  ;; pathological, so i doubt it.
  (define-instance (Functor Iterator)
    (define (map func iter)
      (%Iterator (fn () (map func (next! iter))))))

;;; constructors
  (declare empty (Iterator :any))
  (define empty
    "Yields nothing; stops immediately"
    (%Iterator (fn () None)))

  (define-class (IntoIterator :container :elt (:container -> :elt))
    "Containers which can be converted into iterators.

`into-iter' must not mutate its argument, only produce a \"view\" into it."
    (into-iter (:container -> Iterator :elt)))

  (declare list-iter ((List :elt) -> (Iterator :elt)))
  (define (list-iter lst)
    "Yield successive elements of LST.
Behavior is undefined if the iterator is advanced after a destructive modification of LST."
    (let ((remaining (cell:new lst)))
      (%Iterator (fn () (cell:pop! remaining)))))

  (define-instance (IntoIterator (List :elt) :elt)
    (define into-iter list-iter))

  (define-instance (IntoIterator (Iterator :elt) :elt)
    (define into-iter id))

  (define-instance (IntoIterator (Optional :elt) :elt)
    (define (into-iter opt)
      "Yield the single value of this OPT, or nothing."
      (match opt
        ((Some a) (let ((cell (cell:new True)))
                    (%Iterator (fn () (if (cell:read cell)
                                          (progn (cell:write! cell False)
                                                 (Some a))
                                          None)))))
        ((None) (%Iterator (fn () None))))))

  (define-instance (IntoIterator (Result :a :elt) :elt)
    (define (into-iter result)
      "Yield the single value of this RESULT, or nothing."
      (match result
        ((Ok a) (let ((cell (cell:new True)))
                  (%Iterator (fn () (if (cell:read cell)
                                        (progn (cell:write! cell False)
                                               (Some a))
                                        None)))))
        ((Err _) (%Iterator (fn () None))))))

  (declare vector-iter (Vector :elt -> Iterator :elt))
  (define (vector-iter vec)
    "Yield successive elements of VEC.
Behavior is undefined if the iterator is advanced after a destructive modification of VEC."
    (map ((flip vector:index-unsafe) vec)
         (up-to (vector:length vec))))

  (define-instance (IntoIterator (Vector :elt) :elt)
    (define into-iter vector-iter))

  (declare string-chars (String -> Iterator Char))
  (define (string-chars str)
    "Yield successive `Char`s from STR.
Behavior is undefined if the iterator is advanced after a destructive modification of STR."
    (map (string:ref-unchecked str)
         (up-to (string:length str))))

  (define-instance (IntoIterator String Char)
    (define into-iter string-chars))

  (declare recursive-iter ((:elt -> :elt) -> (:elt -> Boolean) -> :elt -> Iterator :elt))
  (define (recursive-iter succ done? start)
    "An iterator which yields first START, then (SUCC START), then (SUCC (SUCC START)), and so on, stopping as soon as such a value is `done?`.

Beware off-by-one errors: the first value which is `done?` is not yielded. If `(done?  start)' is true, the
iterator is empty."
    (let ((next (cell:new start)))
      (%Iterator
       (fn ()
         (let ((this (cell:read next)))
           (if (done? this)
               None
               (Some (cell:update-swap! succ next))))))))

  (declare range-increasing ((Num :num) (Ord :num) =>
                             :num ->
                             :num ->
                             :num ->
                             Iterator :num))
  (define (range-increasing step start end)
    "An iterator which begins at START and yields successive elements spaced by STEP, stopping before END."
    (assert (>= end start)
        "END ~a should be greater than or equal to START ~a in RANGE-INCREASING"
        end start)
    (assert (> step 0)
        "STEP ~a should be positive and non-zero in RANGE-INCREASING"
        step)
    (recursive-iter (+ step) (<= end) start))

  (declare up-to ((Num :num) (Ord :num) => :num -> Iterator :num))
  (define (up-to limit)
    "An iterator which begins at zero and counts up to, but not including, LIMIT."
    (range-increasing 1 0 limit))

  (declare up-through ((Num :num) (Ord :num) => :num -> Iterator :num))
  (define (up-through limit)
    "An iterator which begins at zero and counts up through and including LIMIT."
    (up-to (+ 1 limit)))

  (declare range-decreasing ((Num :num) (Ord :num) =>
                             :num ->
                             :num ->
                             :num ->
                             (Iterator :num)))
  (define (range-decreasing step start end)
    "A range which begins below START and counts down through and including END by STEP.

Equivalent to reversing `range-increasing`"
    (assert (<= end start)
        "END ~a should be less than or equal to START ~a in RANGE-INCREASING"
        end start)
    (assert (> step 0)
        "STEP ~a should be positive and non-zero in RANGE-INCREASING"
        step)
    ;; FIXME: avoid underflow in the DONE? test
    (recursive-iter ((flip -) step)
                    (fn (n) (>= end (+ n step))) ; like (>= (- end step)), but without potential underflow
                    (- start step)               ; begin after START
                    ))

  (declare down-from ((Num :num) (Ord :num) => :num -> Iterator :num))
  (define (down-from limit)
    "An iterator which begins below the provided limit and counts down through and including zero."
    (range-decreasing 1 limit 0))

  (declare count-forever ((Num :num) (Ord :num) => Unit -> Iterator :num))
  (define (count-forever _)
    "An infinite iterator which starts at 0 and counts upwards by 1."
    (recursive-iter (+ 1)
                    (const False)
                    0))

  (declare repeat-forever (:item -> Iterator :item))
  (define (repeat-forever item)
    "Yield ITEM over and over, infinitely."
    (%Iterator
     (fn ()
       (Some item))))

  (declare repeat-item (:item -> UFix -> Iterator :item))
  (define (repeat-item item count)
    "Yield ITEM COUNT times, then stop."
    (take! count (repeat-forever item)))

  (declare char-range (Char -> Char -> Iterator Char))
  (define (char-range start end)
    "An inclusive range of characters from START to END by cl:char-code."
    (map (compose unwrap code-char)
         (range-increasing 1
                           (char-code start)
                           (+ 1 (char-code end)))))

;;; combinators
  ;; these are named with a !, even though they're lazy and therefore do not actually mutate anything on their
  ;; own, for two reasons:
  ;; 1. it prevents name conflicts with list operators
  ;; 2. to emphasize the flow of mutable data
  (declare interleave! (Iterator :a -> Iterator :a -> Iterator :a))
  (define (interleave! left right)
    "Return an interator of interleaved elements from LEFT and RIGHT which terminates as soon as both LEFT and RIGHT do.

If one iterator terminates before the other, elements from the longer iterator will be yielded without
interleaving. (interleave empty ITER) is equivalent to (id ITER)."
    (let flag = (cell:new False))
    (%Iterator
     (fn ()
       (cell:update! not flag)
       (if (cell:read flag)
           (match (next! left)
             ((Some val) (Some val))
             ((None) (next! right)))
           (match (next! right)
             ((Some val) (Some val))
             ((None) (next! left)))))))

  (declare zipWith! ((:left -> :right -> :out) -> Iterator :left -> Iterator :right -> Iterator :out))
  (define (zipWith! f left right)
    "Return an iterator of elements from LEFT and RIGHT which terminates as soon as either LEFT or RIGHT does."
    (%Iterator
     (fn ()
       (match (Tuple (next! left) (next! right))
         ((Tuple (Some l) (Some r)) (Some (f l r)))
         (_ None)))))

  (declare zip! (Iterator :left -> Iterator :right -> Iterator (Tuple :left :right)))
  (define zip!
    "Return an iterator of tuples contining elements from two iterators."
    (zipWith! Tuple))

  (declare enumerate! (Iterator :elt -> Iterator (Tuple UFix :elt)))
  (define (enumerate! iter)
    "Pair successive zero-based incides with elements from ITER"
    (zip! (count-forever) iter))

  (declare filter! ((:elt -> Boolean) -> Iterator :elt -> Iterator :elt))
  (define (filter! keep? iter)
    "Return an iterator over the elements from ITER for which KEEP? returns true."
    (let ((filter-iter (fn (u)
                         (match (next! iter)
                           ((None) None)
                           ((Some candidate) (if (keep? candidate)
                                                 (Some candidate)
                                                 (filter-iter u)))))))
      (%Iterator filter-iter)))

  (declare unwrapped! ((Unwrappable :wrapper) => Iterator (:wrapper :elt) -> Iterator :elt))
  (define (unwrapped! iter)
    (let ((next (fn ()
                  (match (next! iter)
                    ((None) None)
                    ((Some container)
                     (unwrap-or-else Some
                                     next
                                     container))))))
      (new next)))

  (declare take! (UFix -> Iterator :elt -> Iterator :elt))
  (define (take! count iter)
    "An `Iterator` which yields at most COUNT elements from ITER."
    (map fst
         (zip! iter
               (up-to count))))

  ;; this appears to be a synonym for `concat!'. i would like to remove it - gefjon
  (declare chain! (Iterator :elt -> Iterator :elt -> Iterator :elt))
  (define (chain! iter1 iter2)
    (%iterator
     (fn ()
       (match (next! iter1)
         ((None)    (next! iter2))
         ((Some el) (Some el))))))

  (declare flatten! (Iterator (Iterator :elt) -> Iterator :elt))
  (define (flatten! iters)
    "Yield all the elements from each of the ITERS in order."
    (match (next! iters)
      ((None) empty)
      ((Some first)
       (let ((current (cell:new first))
             (flatten-iter-inner
               (fn ()
                 (match (next! (cell:read current))
                   ((Some elt) (Some elt))
                   ((None) (match (next! iters)
                             ((Some next-iter) (progn (cell:write! current next-iter)
                                                      (flatten-iter-inner)))
                             ((None) None)))))))
         (%Iterator flatten-iter-inner)))))

  (declare concat! (Iterator :elt -> Iterator :elt -> Iterator :elt))
  (define (concat! first second)
    "Yield all the elements of FIRST followed by all the elements from SECOND."
    (flatten! (list-iter (make-list first second))))

  (declare remove-duplicates! (Hash :elt => Iterator :elt -> Iterator :elt))
  (define (remove-duplicates! iter)
    "Yield unique elements from ITER in order of first appearance."
    (let ((already-seen (hashtable:new))
          (unique? (fn (elt)
                     (match (hashtable:get already-seen elt)
                       ((Some (Unit)) False)
                       ((None) (progn (hashtable:set! already-seen elt Unit)
                                      True))))))
      (filter! unique? iter)))

  (declare pair-with! ((:key -> :value) -> Iterator :key -> Iterator (Tuple :key :value)))
  (define (pair-with! func keys)
    "Returns an iterator over tuples whose FSTs are elements from KEYS, and whose SNDs are the results of applying FUNC to those KEYS."
    (map (fn (key) (Tuple key (func key)))
         keys))

;;; consumers
  (declare last! ((Iterator :a) -> (Optional :a)))
  (define (last! iter)
    "Yields the last element of ITER, completely consuming it."
    (fold! (fn (s e) (Some e)) None iter))

  (declare and! (Iterator Boolean -> Boolean))
  (define (and! iter)
    "Returns True if all iterator elements are True. May not consume the entire iterator. Returns True on an empty iterator."
    (let ((inner
            (fn (state)
              (when (not state)
                (return False))

              (match (next! iter)
                ((Some x) (inner (and state x)))
                ((None) True)))))

      (inner True)))

  (declare or! (Iterator Boolean -> Boolean))
  (define (or! iter)
    "Returns True if any iterator elements are True. May not consume the entire iterator. Returns False on an empty iterator."
    (let ((inner
            (fn (state)
              (when state
                (return True))

              (match (next! iter)
                ((Some x) (inner (or state x)))
                ((None) False)))))
      (inner False)))

  (declare sum! (Num :num => Iterator :num -> :num))
  (define (sum! iter)
    "Add together all the elements of ITER."
    (fold! + 0 iter))

  (declare count! (Iterator :elt -> UFix))
  (define (count! iter)
    "Return the number of elements in ITER.
This operation could be called `length!`, but `count!` emphasizes the fact that it consumes ITER, and
afterwards, ITER will be exhausted."
    (sum! (map (const 1) iter)))

  (declare for-each! ((:elt -> :any) -> Iterator :elt -> Unit))
  (define (for-each! thunk iter)
    "Call THUNK on each element of ITER in order for side effects.
Discard values returned by THUNK."
    (fold! (fn (u elt) (thunk elt) u)
           Unit
           iter))

  (declare find! ((:elt -> Boolean) -> Iterator :elt -> Optional :elt))
  (define (find! this? iter)
    "Return the first element of ITER for which THIS? returns `True`, or `None` if no element matches."
    (match (next! iter)
      ((Some elt) (if (this? elt)
                      (Some elt)
                      (find! this? iter)))
      ((None) None)))

  (declare index-of! ((:elt -> Boolean) -> Iterator :elt -> Optional UFix))
  (define (index-of! this? iter)
    "Return the zero-based index of the first element of ITER for which THIS? is `True`, or `None` if no element matches."
    (map fst
         (find! (compose this? snd)
                (enumerate! iter))))

  (declare optimize! ((:elt -> :elt -> Boolean) -> Iterator :elt -> Optional :elt))
  (define (optimize! better? iter)
    "For an order BETTER? which returns `True` if its first argument is better than its second argument, return the best element of ITER.

Return `None` if ITER is empty."
    (match (next! iter)
      ((None) None)
      ((Some first)
       (Some
        (fold! (fn (best new)
                 (if (better? new best) new best))
               first
               iter)))))

  (declare max! (Ord :num => Iterator :num -> Optional :num))
  (define (max! iter)
    "Return the most-positive element of ITER, or `None` if ITER is empty."
    (optimize! > iter))

  (declare min! (Ord :num => Iterator :num -> Optional :num))
  (define (min! iter)
    "Return the most-negative element of ITER, or `None` if ITER is empty."
    (optimize! < iter))

  (declare every! ((:elt -> Boolean) -> Iterator :elt -> Boolean))
  (define (every! good? iter)
    "Return `True` if every element of ITER is GOOD?, or `False` as soon as any element is not GOOD?.

Returns `True` if ITER is empty."
    (match (next! iter)
      ((None) True)
      ((Some item) (and (good? item) (every! good? iter)))))

  (declare any! ((:elt -> Boolean) -> Iterator :elt -> Boolean))
  (define (any! good? iter)
    "Return `True` as soon as any element of ITER is GOOD?, or `False` if none of them are.

Returns `False` if ITER is empty."
    (some? (find! good? iter)))

  (declare elementwise-match! ((:elt -> :elt -> Boolean) -> (Iterator :elt) -> (Iterator :elt) -> Boolean))
  (define (elementwise-match! same? left right)
    "Are LEFT and RIGHT elementwise-identical under SAME?

True if, for every pair of elements (A B) in (LEFT RIGHT), (same? A B) is True, and LEFT and RIGHT have the
same length."
    (match (Tuple (next! left) (next! right))
      ((Tuple (None) (None)) True)
      ((Tuple (Some l) (Some r)) (and (same? l r)
                                      (elementwise-match! same? left right)))
      (_ False)))

  (declare elementwise==! ((Eq :elt) => ((Iterator :elt) -> (Iterator :elt) -> Boolean)))
  (define elementwise==!
    "Is every element of the first iterator `==' to the corresponding element of the second?

True if two iterators have the same length, and for every N, the Nth element of the first iterator is `==' to
the Nth element of the second iterator."
    (elementwise-match! ==))

  (declare elementwise-hash! ((Hash :elt) => ((Iterator :elt) -> Hash)))
  (define (elementwise-hash! iter)
    "Hash an iterator by combining the hashes of all its elements.

The empty iterator will hash as 0."
    (match (next! iter)
      ((Some first) (fold! (fn (current new)
                             (combine-hashes current (hash new)))
                           (hash first)
                           iter))
      ((None) mempty)))

;;; collecting
  (define-class (FromIterator :container :elt (:container -> :elt))
    (collect! (Iterator :elt -> :container)))
  
  (declare collect-list! (Iterator :elt -> List :elt))
  (define (collect-list! iter)
    "Construct a `List` containing all the elements from ITER in order."
    (list:reverse (fold! (flip Cons) Nil iter)))

  (define-instance (FromIterator (List :elt) :elt)
    (define collect! collect-list!))

  (declare collect-vector-size-hint! (types:RuntimeRepr :elt => UFix -> Iterator :elt -> Vector :elt))
  (define (collect-vector-size-hint! size iter)
    "Construct a `Vector` with initial allocation for SIZE elements, and fill it with all the elements from ITER in order.

The vector will be resized if ITER contains more than SIZE elements."
    (let v = (vector:with-capacity size))
    (for-each! ((flip vector:push!) v) iter)
    v)

  (declare collect-vector! (types:RuntimeRepr :elt => Iterator :elt -> Vector :elt))
  (define (collect-vector! iter)
    "Construct a `Vector` containing all the elements from ITER in order."
    (collect-vector-size-hint! 0 iter))

  (define-instance (types:RuntimeRepr :elt => FromIterator (Vector :elt) :elt)
    (define collect! collect-vector!))

  (declare collect-hashtable-size-hint!
           ((Hash :key) =>
            UFix ->
            (Iterator (Tuple :key :value)) ->
            (HashTable :key :value)))
  (define (collect-hashtable-size-hint! size iter)
    "Construct a `HashTable` with initial allocation for SIZE key/value pairs, and fill it with all the key/value pairs from ITER.

If a key appears in ITER multiple times, the resulting table will contain its last corresponding value.

The table will be resized if ITER contains more than SIZE unique keys."
    (let ht = (hashtable:with-capacity (into size)))
    (for-each! (uncurry (hashtable:set! ht))
               iter)
    ht)

  (declare collect-hashtable!
           (Hash :key => Iterator (Tuple :key :value) -> HashTable :key :value))
  (define (collect-hashtable! iter)
    "Construct a `HashTable` containing all the key/value pairs from ITER.

If a key appears in ITER multiple times, the resulting table will contain its last corresponding value."
    (collect-hashtable-size-hint! coalton-library/hashtable::default-hash-table-capacity iter))

  (define-instance (Hash :key => FromIterator (hashtable:HashTable :key :value) (Tuple :key :value))
    (define collect! collect-hashtable!)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/ITERATOR")
