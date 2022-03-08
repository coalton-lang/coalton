(coalton-library/utils:defstdlib-package #:coalton-library/iterator
  (:shadow #:empty)
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/optional
   #:coalton-library/tuple
   #:coalton-library/char)
  (:local-nicknames
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
   #:zip!
   #:enumerate!
   #:filter!
   #:take!
   #:flatten!
   #:concat!
   #:remove-duplicates!
   #:pair-with!
   #:sum!
   #:count!
   #:for-each!
   #:find!
   #:index-of!
   #:optimize!
   #:max!
   #:min!
   #:every!
   #:any!
   #:collect-list!
   #:collect-vector-size-hint!
   #:collect-vector!
   #:collect-hashtable!))

(cl:in-package #:coalton-library/iterator)

;;; fundamental operators
(coalton-toplevel
  (repr :transparent)
  (define-type (Iterator :elt)
    "A forward-moving pointer into an ordered sequence of :ELTs"
    (%Iterator (Unit -> (Optional :elt))))

  (declare new ((Unit -> (Optional :elt)) -> (Iterator :elt)))
  (define new %Iterator)

  (declare next! ((Iterator :elt) -> (Optional :elt)))
  (define (next! iter)
    "Advance ITER, returning its next yielded value, or `None` if the iterator is exhausted.
Behavior is undefined if two threads concurrently call `next!` on the same iterator without a lock. Note that
most of the operators defined on iterators call `next!` internally, or create new iterators which will call
`next!` on their inputs."
    (match iter
      ((%Iterator func) (func Unit))))

  (declare fold! ((:state -> :elt -> :state) -> :state -> (Iterator :elt) -> :state))
  (define (fold! func init iter)
    "Tail recursive in-order fold. Common Lisp calls this operation `reduce`.

If ITER is empty, returns INIT. Otherwise, calls (FUNC STATE ITEM) for each ITEM of ITER to produce a new
STATE, using INIT as the first STATE."
    (match (next! iter)
      ((Some item) (fold! func
                          (func init item)
                          iter))
      ((None) init))))

;;; instances
;; should iterator implement applicative or monad? the only law-abiding applicative instances are
;; pathological, so i doubt it.
(coalton-toplevel
  (define-instance (Functor Iterator)
    (define (map func iter)
      (%Iterator (fn (_) (map func (next! iter)))))))

;;; constructors
;; once coalton gets functional dependencies, associated types or type families, much of this will be
;; abstracted into a class `(IntoIterator :collection :item)', with instances like `(IntoIterator (List :elt)
;; :elt)' and `(IntoIterator String Char)'. It's currently not possible for Coalton to do useful type
;; inference on these classes, so we're stuck with monomorphic constructors like `list-iter`.
(coalton-toplevel
  (declare empty (Iterator :any))
  (define empty
    "Yields nothing; stops immediately"
    (%Iterator (fn (_) None)))
  
  (declare list-iter ((List :elt) -> (Iterator :elt)))
  (define (list-iter lst)
    "Yield successive elements of LST.
Behavior is undefined if the iterator is advanced after a destructive modification of LST."
    (let ((remaining (cell:new lst)))
      (%Iterator (fn (_) (cell:pop! remaining)))))

  (declare vector-iter ((Vector :elt) -> (Iterator :elt)))
  (define (vector-iter vec)
    "Yield successive elements of VEC.
Behavior is undefined if the iterator is advanced after a destructive modification of VEC."
    (map ((flip vector:index-unsafe) vec)
         (up-to (fromInt (vector:length vec)))))

  (declare string-chars (String -> (Iterator Char)))
  (define (string-chars str)
    "Yield successive `Char`s from STR.
Behavior is undefined if the iterator is advanced after a destructive modification of STR."
    (map (string:ref-unchecked str)
         (up-to (fromInt (string:length str)))))

  (declare recursive-iter ((:elt -> :elt) -> (:elt -> Boolean) -> :elt -> (Iterator :elt)))
  (define (recursive-iter succ done? start)
    "An iterator which yields first START, then (SUCC START), then (SUCC (SUCC START)), and so on, stopping as soon as such a value is `done?`.

Beware off-by-one errors: the first value which is `done?` is not yielded. If `(done?  start)' is true, the
iterator is empty."
    (let ((next (cell:new start)))
      (%Iterator
       (fn (_)
         (let ((this (cell:read next)))
           (if (done? this)
               None
               (Some (cell:update-swap! succ next))))))))

  (declare range-increasing ((Num :num) (Ord :num) => (:num -> :num -> :num -> (Iterator :num))))
  (define (range-increasing step start end)
    "An iterator which begins at START and yields successive elements spaced by STEP, stopping before END."
    (progn 
      (assert (>= end start)
          "END ~a should be greater than or equal to START ~a in RANGE-INCREASING"
          end start)
      (assert (> step 0)
          "STEP ~a should be positive and non-zero in RANGE-INCREASING"
          step)
      (recursive-iter (+ step) (<= end) start)))

  (declare up-to ((Num :num) (Ord :num) => :num -> (Iterator :num)))
  (define (up-to limit)
    "An iterator which begins at zero and counts up to, but not including, LIMIT."
    (range-increasing 1 0 limit))

  (declare up-through ((Num :num) (Ord :num) => :num -> (Iterator :num)))
  (define (up-through limit)
    "An iterator which begins at zero and counts up through and including LIMIT."
    (up-to (+ 1 limit)))

  (declare range-decreasing ((Num :num) (Ord :num) => (:num -> :num -> :num -> (Iterator :num))))
  (define (range-decreasing step start end)
    "A range which begins below START and counts down through and including END by STEP.

Equivalent to reversing `range-increasing`"
    (progn 
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
                      )))

  (declare down-from ((Num :num) (Ord :num) => :num -> (Iterator :num)))
  (define (down-from limit)
    "An iterator which begins below the provided limit and counts down through and including zero."
    (range-decreasing 1 limit 0))

  (declare count-forever ((Num :num) (Ord :num) => Unit -> (Iterator :num)))
  (define (count-forever _)
    "An infinite iterator which starts at 0 and counts upwards by 1."
    (recursive-iter (+ 1)
                    (const False)
                    0))

  (declare repeat-forever (:item -> (Iterator :item)))
  (define (repeat-forever item)
    "Yield ITEM over and over, infinitely."
    (%Iterator
      (fn (_)
        (Some item))))

  (declare repeat-item (:item -> UFix -> (Iterator :item)))
  (define (repeat-item item count)
    "Yield ITEM COUNT times, then stop."
    (take! count (repeat-forever item)))

  (declare char-range (Char -> Char -> (Iterator Char)))
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
  (declare zip! ((Iterator :left) -> (Iterator :right) -> (Iterator (Tuple :left :right))))
  (define (zip! left right)
    "Return an iterator of tuples of elements from LEFT and RIGHT which terminates as soon as either LEFT or RIGHT does.

Often useful combined with `uncurry` to allow mapping a multi-argument function across multiple iterators."
    (%Iterator
      (fn (_)
        (match (Tuple (next! left) (next! right))
          ((Tuple (Some l) (Some r)) (Some (Tuple l r)))
          (_ None)))))

  (declare enumerate! ((Iterator :elt) -> (Iterator (Tuple UFix :elt))))
  (define (enumerate! iter)
    "Pair successive zero-based incides with elements from ITER"
    (zip! (count-forever) iter))

  (declare filter! ((:elt -> Boolean) -> (Iterator :elt) -> (Iterator :elt)))
  (define (filter! keep? iter)
    "Return an iterator over the elements from ITER for which KEEP? returns true."
    (let ((filter-iter (fn (u)
                         (match (next! iter)
                           ((None) None)
                           ((Some candidate) (if (keep? candidate)
                                                 (Some candidate)
                                                 (filter-iter u)))))))
      (%Iterator filter-iter)))

  (declare take! (UFix -> (Iterator :elt) -> (Iterator :elt)))
  (define (take! count iter)
    "An `Iterator` which yields at most COUNT elements from ITER."
    (map fst
         (zip! iter
               (up-to count))))

  (declare flatten! ((Iterator (Iterator :elt)) -> (Iterator :elt)))
  (define (flatten! iters)
    "Yield all the elements from each of the ITERS in order."
    (match (next! iters)
      ((None) empty)
      ((Some first)
       (let ((current (cell:new first))
             (flatten-iter-inner
               (fn (_)
                 (match (next! (cell:read current))
                   ((Some elt) (Some elt))
                   ((None) (match (next! iters)
                             ((Some next-iter) (progn (cell:write! current next-iter)
                                                      (flatten-iter-inner)))
                             ((None) None)))))))
         (%Iterator flatten-iter-inner)))))

  (declare concat! ((Iterator :elt) -> (Iterator :elt) -> (Iterator :elt)))
  (define (concat! first second)
    "Yield all the elements of FIRST followed by all the elements from SECOND."
    (flatten! (list-iter (make-list first second))))

  (declare remove-duplicates! ((Hash :elt) => (Iterator :elt) -> (Iterator :elt)))
  (define (remove-duplicates! iter)
    "Yield unique elements from ITER in order of first appearance."
    (let ((already-seen (hashtable:new))
          (unique? (fn (elt)
                     (match (hashtable:get already-seen elt)
                       ((Some (Unit)) False)
                       ((None) (progn (hashtable:set! already-seen elt Unit)
                                      True))))))
      (filter! unique? iter)))

  (declare pair-with! ((:key -> :value) -> (Iterator :key) -> (Iterator (Tuple :key :value))))
  (define (pair-with! func keys)
    "Returns an iterator over tuples whose FSTs are elements from KEYS, and whose SNDs are the results of applying FUNC to those KEYS."
    (map (fn (key) (Tuple key (func key)))
         keys))

;;; consumers
  (declare sum! ((Num :num) => (Iterator :num) -> :num))
  (define (sum! iter)
    "Add together all the elements of ITER."
    (fold! + 0 iter))

  (declare count! ((Iterator :elt) -> Integer))
  (define (count! iter)
    "Return the number of elements in ITER.
This operation could be called `length!`, but `count!` emphasizes the fact that it consumes ITER, and
afterwards, ITER will be exhausted."
    (sum! (map (const 1) iter)))

  (declare for-each! ((:elt -> :any) -> (Iterator :elt) -> Unit))
  (define (for-each! thunk iter)
    "Call THUNK on each element of ITER in order for side effects.
Discard values returned by THUNK."
    (fold! (fn (u elt) (progn (thunk elt) u))
           Unit
           iter))

  (declare find! ((:elt -> Boolean) -> (Iterator :elt) -> (Optional :elt)))
  (define (find! this? iter)
    "Return the first element of ITER for which THIS? returns `True`, or `None` if no element matches."
    (match (next! iter)
      ((Some elt) (if (this? elt)
                      (Some elt)
                      (find! this? iter)))
      ((None) None)))

  (declare index-of! ((:elt -> Boolean) -> (Iterator :elt) -> (Optional UFix)))
  (define (index-of! this? iter)
    "Return the zero-based index of the first element of ITER for which THIS? is `True`, or `None` if no element matches."
    (map fst
         (find! (compose this? snd)
                (enumerate! iter))))

  (declare optimize! ((:elt -> :elt -> Boolean) -> (Iterator :elt) -> (Optional :elt)))
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

  (declare max! ((Ord :num) => (Iterator :num) -> (Optional :num)))
  (define (max! iter)
    "Return the most-positive element of ITER, or `None` if ITER is empty."
    (optimize! > iter))

  (declare min! ((Ord :num) => (Iterator :num) -> (Optional :num)))
  (define (min! iter)
    "Return the most-negative element of ITER, or `None` if ITER is empty."
    (optimize! < iter))

  (declare every! ((:elt -> Boolean) -> (Iterator :elt) -> Boolean))
  (define (every! good? iter)
    "Return `True` if every element of ITER is GOOD?, or `False` as soon as any element is not GOOD?.

Returns `True` if ITER is empty."
    (match (next! iter)
      ((None) True)
      ((Some item) (and (good? item) (every! good? iter)))))

  (declare any! ((:elt -> Boolean) -> (Iterator :elt) -> Boolean))
  (define (any! good? iter)
    "Return `True` as soon as any element of ITER is GOOD?, or `False` if none of them are.

Returns `False` if ITER is empty."
    (some? (find! good? iter)))

;;; collecting
  ;; as with `IntoIterator`, these will one day be abstracted into a class `(FromIterator :collection :item)'.
  (declare collect-list! ((Iterator :elt) -> (List :elt)))
  (define (collect-list! iter)
    "Construct a `List` containing all the elements from ITER in order."
    (list:reverse (fold! (flip Cons) Nil iter)))

  (declare collect-vector-size-hint! (Integer -> (Iterator :elt) -> (Vector :elt)))
  (define (collect-vector-size-hint! size iter)
    "Construct a `Vector` with initial allocation for SIZE elements, and fill it with all the elements from ITER in order.

The vector will be resized if ITER contains more than SIZE elements."
    (progn 
      (let v = (vector:with-capacity size))
      (for-each! ((flip vector:push!) v) iter)
      v))

  (declare collect-vector! ((Iterator :elt) -> (Vector :elt)))
  (define (collect-vector! iter)
    "Construct a `Vector` containing all the elements from ITER in order."
    (collect-vector-size-hint! 0 iter))

  (declare collect-hashtable-size-hint!
           ((Hash :key) =>
            UFix ->
            (Iterator (Tuple :key :value)) ->
            (HashTable :key :value)))
  (define (collect-hashtable-size-hint! size iter)
    "Construct a `HashTable` with initial allocation for SIZE key/value pairs, and fill it with all the key/value pairs from ITER.

If a key appears in ITER multiple times, the resulting table will contain its last corresponding value.

The table will be resized if ITER contains more than SIZE unique keys."
    (let ((ht (hashtable:with-capacity (into size))))
      (progn
        (for-each! (uncurry (hashtable:set! ht))
                   iter)
        ht)))

  (declare collect-hashtable!
           ((Hash :key) => (Iterator (Tuple :key :value)) -> (HashTable :key :value)))
  (define (collect-hashtable! iter)
    "Construct a `HashTable` containing all the key/value pairs from ITER.

If a key appears in ITER multiple times, the resulting table will contain its last corresponding value."
    (collect-hashtable-size-hint! coalton-library/hashtable::default-hash-table-capacity iter)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/ITERATOR")
