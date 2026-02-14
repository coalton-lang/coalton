(coalton/utils:defstdlib-package #:coalton/iterator
  (:shadow #:empty)
  (:use
   #:coalton
   #:coalton/classes
   #:coalton/hash
   #:coalton/builtin
   #:coalton/functions)
  (:local-nicknames
   (#:types #:coalton/types)
   (#:cell #:coalton/cell))
  (:export
   #:Iterator
   #:new
   #:with-size
   #:size-hint
   #:next!
   #:fold!
   #:empty
   #:last!
   #:IntoIterator
   #:into-iter
   #:recursive-iter
   #:range-increasing
   #:up-to
   #:up-through
   #:range-decreasing
   #:down-from
   #:count-forever
   #:repeat
   #:repeat-for
   #:once
   #:char-range
   #:interleave!
   #:zip!
   #:zip-with!
   #:enumerate!
   #:filter!
   #:filter-map!
   #:map-while!
   #:unwrapped!
   #:take!
   #:flatten!
   #:flat-map!
   #:mconcat!
   #:mconcatmap!
   #:chain!
   #:remove-duplicates! ; defined in library/hashtable.lisp
   #:pair-with!
   #:sum!
   #:and!
   #:or!
   #:count!
   #:for-each!
   #:find!
   #:find-map!
   #:index-of!
   #:optimize!
   #:max!
   #:min!
   #:optimize-by!
   #:maximize-by!
   #:minimize-by!
   #:every!
   #:any!
   #:elementwise-match!
   #:elementwise==!
   #:elementwise-hash!
   #:FromIterator
   #:collect!))

(in-package #:coalton/iterator)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  ;;
  ;; Iterators
  ;;

  (define-type (Iterator :elt)
    "A forward-moving pointer into an ordered sequence of :ELTs"
    (%Iterator (Unit -> (Optional :elt)) (Optional UFix)))

  (declare new ((Unit -> Optional :elt) -> Iterator :elt))
  (define (new f)
    "Create a new iterator from a function that yields elements."
    (%Iterator f None))

  (declare with-size ((Unit -> Optional :elt) -> UFix -> Iterator :elt))
  (define (with-size f size)
    (%Iterator f (Some size)))

  (declare size-hint (Iterator :elt -> Optional UFix))
  (define (size-hint iter)
    (let (%Iterator _f size) = iter)
    size)

  (declare next! (Iterator :elt -> Optional :elt))
  (define (next! iter)
    "Advance ITER, returning its next yielded value, or `None` if the iterator is exhausted.
Behavior is undefined if two threads concurrently call `next!` on the same iterator without a lock. Note that
most of the operators defined on iterators call `next!` internally, or create new iterators which will call
`next!` on their inputs."
    (match iter
      ((%Iterator func _) (func Unit))))

  (declare fold! ((:state -> :elt -> :state) -> :state -> Iterator :elt -> :state))
  (define (fold! func init iter)
    "Tail recursive in-order fold. Common Lisp calls this operation `reduce`.

If ITER is empty, returns INIT. Otherwise, calls (FUNC STATE ITEM) for each ITEM of ITER to produce a new
STATE, using INIT as the first STATE."
    (match (next! iter)
      ((Some item)
       (fold! func
              (func init item)
              iter))
      ((None) init)))

  (define-instance (Functor Iterator)
    (define (map func iter)
      (%Iterator (fn () (match (next! iter)
                          ((Some x) (Some (func x)))
                          ((None) None)))
                 (size-hint iter))))

  ;;
  ;; Constructors
  ;;

  (declare empty (Iterator :any))
  (define empty
    "Yields nothing; stops immediately"
    (%Iterator (fn () None) (Some 0)))

  (define-class (IntoIterator :container :elt (:container -> :elt))
    "Containers which can be converted into iterators.

`INTO-ITER` must not mutate its argument, only produce a \"view\" into it."
    (into-iter (:container -> Iterator :elt)))

  (define-instance (IntoIterator (Iterator :elt) :elt)
    (define into-iter id))

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
               (Some (cell:update-swap! succ next)))))
       None)))

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


  ;; All the haranguing below is so that we don't overflow a bounded
  ;; type in a range-decreasing call. We generally assume a lower
  ;; bound (e.g., 0 for unsigned types) is more common than an upper
  ;; bound.
  (repr :enum)
  (define-type RangeStatus
    RangeContinue
    RangeLast
    RangeDone)

  (declare range-decreasing ((Num :num) (Ord :num) =>
                             :num ->
                             :num ->
                             :num ->
                             (Iterator :num)))
  (define (range-decreasing step start end)
    "A range which begins below START and counts down through and including END by STEP.

Equivalent to reversing `range-increasing`"
    (assert (<= end start)
        "END ~a should be less than or equal to START ~a in RANGE-DECREASING"
        end start)
    (assert (> step 0)
        "STEP ~a should be positive and non-zero in RANGE-DECREASING"
        step)
    (let ((end+step (+ end step)))
      (if (< start end+step)
          empty
          (let ((start-step (- start step))
                (next (cell:new start-step))
                (status (cell:new (if (< start-step end+step)
                                      RangeLast
                                      RangeContinue))))
            (%Iterator
             (fn ()
               (match (cell:read status)
                 ((RangeDone)
                  None)
                 ((RangeLast)
                  (cell:write! status RangeDone)
                  (Some (cell:read next)))
                 ((RangeContinue)
                  (let ((this (cell:read next))
                        (next-next (- this step)))
                    (cell:write! status (if (< next-next end+step)
                                            RangeLast
                                            RangeContinue))
                    (cell:write! next next-next)
                    (Some this)))))
             None)))))

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

  (declare repeat (:item -> Iterator :item))
  (define (repeat item)
    "Yield ITEM over and over, infinitely."
    (%Iterator
     (fn ()
       (Some item))
     None))

  (declare repeat-for (:item -> UFix -> Iterator :item))
  (define (repeat-for item count)
    "Yield ITEM COUNT times, then stop."
    (take! count (repeat item)))

  (declare once (:item -> Iterator :item))
  (define (once item)
    "Yield `item` once."
    (let ((unread? (cell:new True)))
      (%Iterator
       (fn ()
         (if (cell:read unread?)
             (progn (cell:write! unread? False)
                    (Some item))
             None))
       (Some 1))))

  (declare cycle (List :item -> Iterator :item))
  (define (cycle list)
    "Cycles through the elements of LIST indefinitely."
    (let null? = (fn (list)
                   (match list
                     ((Cons _ _) False)
                     ((Nil) True))))
    (when (null? list)
      (return empty))

    (let state = (cell:new list))
    (new (fn ()
           (when (null? (cell:read state))
             (cell:write! state list)
             Unit)

           (cell:pop! state))))

  ;;
  ;; combinators
  ;; these are named with a !, even though they're lazy and therefore do not actually mutate anything on their
  ;; own, for two reasons:
  ;; 1. it prevents name conflicts with list operators
  ;; 2. to emphasize the flow of mutable data
  ;;

  (declare interleave! (Iterator :a -> Iterator :a -> Iterator :a))
  (define (interleave! left right)
    "Return an iterator of interleaved elements from LEFT and RIGHT which terminates as soon as both LEFT and RIGHT do.

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
             ((None) (next! left)))))

     (match (Tuple (size-hint left) (size-hint right))
       ((Tuple (Some x) (Some y)) (Some (+ x y)))
       (_ None))))

  (declare zip-with! ((:left -> :right -> :out) -> Iterator :left -> Iterator :right -> Iterator :out))
  (define (zip-with! f left right)
    "Return an iterator of elements from LEFT and RIGHT which terminates as soon as either LEFT or RIGHT does."
    (%Iterator
     (fn ()
       (match (Tuple (next! left) (next! right))
         ((Tuple (Some l) (Some r)) (Some (f l r)))
         (_ None)))

     (match (Tuple (size-hint left) (size-hint right))
       ((Tuple (Some x) (Some y))
        (Some (max x y)))
       (_ None))))

  (declare zip! (Iterator :left -> Iterator :right -> Iterator (Tuple :left :right)))
  (define zip!
    "Return an iterator of tuples contining elements from two iterators."
    (zip-with! Tuple))

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
      (%Iterator filter-iter (size-hint iter))))

  (declare filter-map! ((:a -> Optional :b) -> Iterator :a -> Iterator :b))
  (define (filter-map! f iter)
    "Map an iterator, retaining only the elements where F returns SOME."
    (let ((fun (fn ()
                 (match (next! iter)
                   ((Some x)
                    (let x = (f x))
                    (match x
                      ((Some _) x)
                      ((None) (fun))))
                   ((None)
                    None)))))
      (%Iterator
       fun
       (size-hint iter))))

  (declare map-while! ((:a -> Optional :b) -> Iterator :a -> Iterator :b))
  (define (map-while! f iter)
    "Map an iterator, stopping early if F returns NONE."
    (let done = (cell:new False))
    (%Iterator
     (fn ()
       (when (cell:read done)
         (return None))

       (match (next! iter)
         ((Some x)
          (let x = (f x))
          (match x
            ((Some _)
             x)
            ((None)
             (cell:write! done True)
             None)))
         ((None)
          (cell:write! done True)
          None)))
     (size-hint iter)))

  (declare unwrapped! (Unwrappable :wrapper => Iterator (:wrapper :elt) -> Iterator :elt))
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
    (map (fn ((Tuple x _)) x)
         (zip! iter
               (up-to count))))

  (declare chain! (Iterator :elt -> Iterator :elt -> Iterator :elt))
  (define (chain! iter1 iter2)
    "Yield all the elements of ITER1 followed by all the elements from ITER2."
    (%iterator
     (fn ()
       (match (next! iter1)
         ((None)    (next! iter2))
         ((Some el) (Some el))))

     (match (Tuple (size-hint iter1) (size-hint iter2))
       ((Tuple (Some x) (Some y))
        (Some (+ x y)))
       (_ None))))

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
         (%Iterator flatten-iter-inner None)))))

  (declare flat-map! ((:a -> (Iterator :b)) -> Iterator :a -> Iterator :b))
  (define (flat-map! func iter)
    "Flatten! wrapped around map."
    (flatten! (map func iter)))

  (declare mconcat! ((Monoid :a) => (Iterator :a) -> :a))
  (define (mconcat! iter)
    "Fold an iterator of monoids into a single element."
    (fold! <> mempty iter))

  (declare mconcatmap! ((Monoid :a) => (:b -> :a) -> (Iterator :b) -> :a))
  (define (mconcatmap! func iter)
    "Map an iterator to an iterator of monoids, and then fold that iterator into a single element."
    (fold! <> mempty (map func iter)))

  (declare pair-with! ((:key -> :value) -> Iterator :key -> Iterator (Tuple :key :value)))
  (define (pair-with! func keys)
    "Returns an iterator over tuples whose FSTs are elements from KEYS, and whose SNDs are the results of applying FUNC to those KEYS."
    (map (fn (key) (Tuple key (func key)))
         keys))

  ;;
  ;; Consuming
  ;;

  (declare last! (Iterator :a -> Optional :a))
  (define (last! iter)
    "Yields the last element of ITER, completely consuming it."
    (fold! (fn (_ e) (Some e)) None iter))

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

  (declare for-each! ((:elt -> Unit) -> Iterator :elt -> Unit))
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

  (declare find-map! ((:a -> Optional :b) -> Iterator :a -> Optional :b))
  (define (find-map! f)
    "Return the first element of (map F ITER) for which F returns `Some`."
    (compose next! (filter-map! f)))

  (declare index-of! ((:elt -> Boolean) -> Iterator :elt -> Optional UFix))
  (define (index-of! this? iter)
    "Return the zero-based index of the first element of ITER for which THIS? is `True`, or `None` if no element matches."
    (match (find! (compose this? (fn ((Tuple _ y)) y))
                  (enumerate! iter))
      ((Some (Tuple x _))
       (Some x))
      (_ None)))

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

  (declare optimize-by! ((:b -> :b -> Boolean) ->
                         (:a -> :b) ->
                         Iterator :a ->
                         Optional :a))
  (define (optimize-by! better? f iter)
    "For an order BETTER? which returns `True` if its first argument is better than its second argument, return the element of ITER where (F ELT) is the best.

Return `None` if ITER is empty."
    (match (optimize! (fn ((Tuple _ a) (Tuple _ b)) (better? a b))
                      (pair-with! f iter))
      ((Some (Tuple result _)) (Some result))
      ((None) None)))

  (declare maximize-by! (Ord :a => (:elt -> :a) -> Iterator :elt -> Optional :elt))
  (define (maximize-by! f iter)
    "For a function F, which maps the iterator, return the element of ITER where (F ELT) is the most-positive.

Return `None' if ITER is empty."
    (optimize-by! > f iter))

  (declare minimize-by! (Ord :a => (:elt -> :a) -> Iterator :elt -> Optional :elt))
  (define (minimize-by! f iter)
    "For a function F, which maps the iterator, return the element of ITER where (F ELT) is the most-negative.

Return `None' if ITER is empty."
    (optimize-by! < f iter))

  (declare every! ((:elt -> Boolean) -> Iterator :elt -> Boolean))
  (define (every! good? iter)
    "Return `True` if every element of ITER is GOOD?, or `False` as soon as any element is not GOOD?.

Returns `True` if ITER is empty."
    (match (next! iter)
      ((None) True)
      ((Some item) (if (good? item) (every! good? iter) False))))

  (declare any! ((:elt -> Boolean) -> Iterator :elt -> Boolean))
  (define (any! good? iter)
    "Return `True` as soon as any element of ITER is GOOD?, or `False` if none of them are.

Returns `False` if ITER is empty."
    (match (find! good? iter)
      ((Some _) True)
      ((None) False)))

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

  ;;
  ;; Collecting
  ;;

  (define-class (FromIterator :container :elt (:container -> :elt))
    (collect! (Iterator :elt -> :container))))

#+sb-package-locks
(sb-ext:lock-package "COALTON/ITERATOR")
