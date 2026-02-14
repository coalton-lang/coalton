(coalton/utils::defstdlib-package #:coalton/list
  (:use
   #:coalton
   #:coalton/builtin
   #:coalton/classes
   #:coalton/hash
   #:coalton/functions
   #:coalton/tuple
   #:coalton/optional)
  (:local-nicknames
   (#:cell #:coalton/cell)
   (#:iter #:coalton/iterator)
   (#:math #:coalton/math))
  (:export
   #:head
   #:tail
   #:car
   #:cdr
   #:last
   #:init
   #:null?
   #:cons?
   #:singleton
   #:singleton?
   #:repeat
   #:reverse
   #:drop
   #:take
   #:find
   #:filter
   #:length
   #:index
   #:nth
   #:nth-cdr
   #:elemIndex
   #:findIndex
   #:range
   #:append
   #:concat
   #:concatMap
   #:member
   #:union
   #:intersection
   #:lookup
   #:remove-duplicates
   #:remove-if
   #:remove
   #:difference
   #:zipWith
   #:zipWith3
   #:zipWith4
   #:zipWith5
   #:zip
   #:countBy
   #:insert
   #:insertBy
   #:sort
   #:sortBy
   #:intersperse
   #:intercalate
   #:insertions
   #:transpose
   #:partition
   #:equivalence-classes
   #:equivalence-classes-by
   #:optimumBy
   #:maximum
   #:minimum
   #:sum
   #:product
   #:all
   #:any
   #:split-at
   #:split-around
   #:perms
   #:combs
   #:combsOf
   #:make))

(in-package #:coalton/list)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  ;;
  ;; Cons Lists
  ;;

  ;; List is an early type

  (declare head (List :a -> Optional :a))
  (define (head l)
    "Returns the first element of a list."
    (match l
      ((Cons x _) (Some x))
      ((Nil) None)))

  (declare tail (List :a -> Optional (List :a)))
  (define (tail l)
    "Returns every element except the first in a list."
    (match l
      ((Cons _ xs) (Some xs))
      ((Nil) None)))

  (declare car (List :a -> :a))
  (define (car x)
    "Return the traditional car of a list. This function is partial."
    (match x
      ((Cons x _) x)
      ((Nil) (error "there is no first element"))))

  (declare cdr (List :a -> List :a))
  (define (cdr xs)
    "Return the traditional cdr of a list."
    (match xs
      ((Cons _ xs) xs)
      ((Nil) Nil)))

  (declare last (List :a -> Optional :a))
  (define (last l)
    "Returns the last element of a list."
    (match l
      ((Cons x (Nil)) (Some x))
      ((Cons _ xs) (last xs))
      ((Nil) None)))

  (declare init (List :a -> List :a))
  (define (init l)
    "Returns every element except the last in a list."
    (lisp (List :a) (l)
      (cl:butlast l)))

  (declare null? (List :a -> Boolean))
  (define (null? xs)
    "Returns TRUE if XS is an empty list."
    (match xs
      ((Nil) True)
      (_ False)))

  (declare cons? (List :a -> Boolean))
  (define (cons? xs)
    "Returns TRUE if XS is a non-empty list."
    (match xs
      ((Nil) False)
      (_ True)))

  (declare singleton (:a -> List :a))
  (define (singleton x)
    "Returns a list containing one element."
    (Cons x Nil))

  (declare singleton? (List :a -> Boolean))
  (define (singleton? xs)
    "Is `xs` a list containing exactly one element?"
    (match xs
      ((Cons _ (Nil)) True)
      (_ False)))

  (declare repeat (UFix -> :a -> List :a))
  (define (repeat n x)
    "Returns a list with the same value repeated multiple times."
    (rec f ((k n)
            (acc Nil))
      (if (== k 0)
          acc
          (f (- k 1) (Cons x acc)))))

  (define (%reverse as bs)
    (match as
      ((Nil) bs)
      ((Cons a as) (%reverse as (Cons a bs)))))

  (declare reverse (List :a -> List :a))
  (define (reverse xs)
    "Returns a new list containing the same elements in reverse order."
    ;; like (fold (flip Cons) Nil xs)
    (%reverse xs Nil))

  ;; This is only for internal usage
  (declare %reverse! (List :a -> List :a))
  (define (%reverse! xs)
    "A mutating reverse operation. After (%reverse! LST), LST must not be referenced; the original list, and all its sublists, should be treated as consumed by this operation. Callers should use the return value and only the return value."
    (lisp (List :a) (xs)
      (cl:nreverse xs)))

  (declare drop (UFix -> List :a -> List :a))
  (define (drop n xs)
    "Returns a list with the first N elements removed."
    (if (== n 0)
        xs
        (match xs
          ((Cons _ xs)
           (drop (- n 1) xs))
          ((Nil) Nil))))

  (declare take (UFix -> List :a -> List :a))
  (define (take n xs)
    "Returns the first N elements of a list."
    (%reverse!
     (rec f ((n n)
             (in xs)
             (out Nil))
       (if (== n 0)
           out
           (match in
             ((Cons x xs) (f (- n 1) xs (Cons x out)))
             ((Nil) out))))))

  (declare split-at (UFix -> List :a -> (Tuple (List :a) (List :a))))
  (define (split-at n xs)
    "Splits a list into a Tuple of the first N elements and all remaining elements. The return value is equivalent to `(Tuple (take n xs) (drop n xs)`."
    (rec % ((n n)
            (tail xs)
            (acc-head Nil))
      (if (== n 0)
          (Tuple (%reverse! acc-head) tail)
          (match tail
            ((Cons x xs) (% (- n 1) xs (Cons x acc-head)))
            ((Nil) (Tuple (%reverse! acc-head) Nil))))))

  (declare split-around (UFix -> List :a -> (Tuple3 (List :a) (Optional :a) (List :a))))
  (define (split-around n xs)
    "Splits a list around N into a Tuple of the first N elements, the element at index N, and a tail of all remaining elements. N must be a valid index."
    (match (split-at n xs)
      ((Tuple head (Cons x xs)) (Tuple3 head (Some x) xs))
      ((Tuple head (Nil)) (Tuple3 head None Nil))))

  (declare find ((:a -> Boolean) -> List :a -> Optional :a))
  (define (find f xs)
    "Returns the first element in a list matching the predicate function F."
    (fold (fn (a b)
            (match a
              ((Some _) a)
              (_
               (if (f b) (Some b) None))))
          None xs))

  (declare filter ((:a -> Boolean) -> List :a -> List :a))
  (define (filter f xs)
    "Returns a new list containing every element of XS that matches the predicate function F in the same order."
    (let ((fun (fn (xs ys)
                 (match xs
                   ((Nil) (%reverse! ys))
                   ((Cons x xs)
                    (if (f x)
                        (fun xs (Cons x ys))
                        (fun xs ys)))))))
      (fun xs Nil)))

  (declare length (List :a -> UFix))
  (define (length l)
    "Returns the length of a list."
    (fold (fn (a _)
            (+ 1 a))
          0
          l))

  (declare index (UFix -> List :a -> Optional :a))
  (define (index i xs)
    "Returns the Ith element of a list."
    (match xs
      ((Nil) None)
      ((Cons x xs)
       (if (== 0 i)
           (Some x)
           (index (- i 1) xs)))))

  (declare nth (UFix -> List :t -> :t))
  (define (nth n l)
    "Like INDEX, but errors if the index is not found."
    (from-some "There is no NTH" (index n l)))

  (declare nth-cdr (Ufix -> List :a -> List :a))
  (define (nth-cdr n l)
    "Returns the nth-cdr of a list."
    (cond ((null? l)
	   Nil)
	  ((math:zero? n)
	   l)
	  (True
	   (nth-cdr (math:1- n) (cdr l)))))
  
  (declare elemIndex (Eq :a => :a -> List :a -> Optional UFix))
  (define (elemIndex x xs)
    (findIndex (== x) xs))

  (declare findIndex ((:a -> Boolean) -> List :a -> Optional UFix))
  (define (findIndex f xs)
    "Returns the index of the first element matching the predicate function F."
    (let ((find (fn (xs n)
                  (match xs
                    ((Nil) None)
                    ((Cons x xs)
                     (if (f x)
                         (Some n)
                         (find xs (+ n 1))))))))
      (find xs 0)))

  (declare range ((Num :int) (Ord :int) => :int -> :int -> List :int))
  (define (range start end)
    "Returns a list containing the numbers from START to END inclusive, counting by 1.

    COALTON-USER> (coalton (range 1 5))
    (1 2 3 4 5)

    COALTON-USER> (coalton (range 5 2))
    (5 4 3 2)
"
    (let ((inner (fn (x end a)
                   (if (> x end)
                       a
                       (inner (+ 1 x) end (Cons x a))))))
      (if (<= start end)
          (%reverse! (inner start end Nil))
          (inner end start Nil))))

  (define (append-rev list result)
    (match list
      ((Nil) result)
      ((Cons x xs) (append-rev xs (Cons x result)))))

  (declare append (List :a -> List :a -> List :a))
  (define (append xs ys)
    "Appends two lists together and returns a new list."
    (%reverse! (append-rev ys (append-rev xs Nil))))

  (specialize mconcat concat (List (List :a) -> List :a))
  (declare concat (List (List :a) -> List :a))
  (define (concat xs)
    "Appends a list of lists together into a single new list."
    (concatMap (fn (x) x) xs))

  (specialize mconcatMap concatMap ((:a -> (List :b)) -> List :a -> List :b))
  (declare concatMap ((:a -> (List :b)) -> List :a -> List :b))
  (define (concatMap f xs)
    "Apply F to each element in XS and concatenate the results."
    (%reverse! (fold (fn (a b) (append-rev (f b) a)) Nil xs)))

  (declare member (Eq :a => (:a -> (List :a) -> Boolean)))
  (define (member e xs)
    "Returns true if any element of XS is equal to E."
    (match xs
      ((Cons x xs)
       (if (== x e)
           True
           (member e xs)))
      ((Nil) False)))

  (declare union (Eq :a => ((List :a) -> (List :a) -> (List :a))))
  (define (union xs ys)
    "Returns a new list with the elements from both XS and YS and without duplicates."
    (%reverse!
     (remove-duplicates-rev
      ys
      (rec f ((xs xs)
              (acc Nil))
        (match xs
          ((Nil) acc)
          ((Cons x xs)
           (if (or (member x ys)
                   (member x acc))
               (f xs acc)
               (f xs (Cons x acc)))))))))

  (declare intersection (Eq :a => ((List :a) -> (List :a) -> (List :a))))
  (define (intersection xs ys)
    "Returns elements which occur in both lists. Does not return duplicates and does not guarantee order."
    (let ((inner (fn (xs ys acc)
                   (match xs
                     ((Cons x xs)
                      (if (member x ys)
                          (inner xs ys (Cons x acc))
                          (inner xs ys acc)))
                     ((Nil) acc)))))
      (%reverse! (inner (remove-duplicates-rev xs Nil) (remove-duplicates-rev ys Nil) Nil))))

  (declare lookup (Eq :a => (:a -> (List (Tuple :a :b)) -> (Optional :b))))
  (define (lookup e xs)
    "Returns the value of the first (key, value) tuple in XS where the key matches E."
    (match xs
      ((Cons x xs)
       (match x
         ((Tuple k v)
          (if (== e k)
              (Some v)
              (lookup e xs)))))
      ((Nil) None)))

  (declare remove-duplicates-rev (Eq :a => ((List :a) -> (List :a) -> (List :a))))
  (define (remove-duplicates-rev xs acc)
    (match xs
      ((Nil) acc)
      ((Cons x xs)
       (if (member x xs)
           (remove-duplicates-rev xs acc)
           (remove-duplicates-rev xs (Cons x acc))))))

  (declare remove-duplicates (Eq :a => ((List :a) -> (List :a))))
  (define (remove-duplicates xs)
    "Returns a new list without duplicate elements."
    (%reverse! (remove-duplicates-rev xs Nil)))

  (declare remove-rev-if ((:a -> Boolean) -> (List :a) -> (List :a) -> (List :a)))
  (define (remove-rev-if pred ys acc)
    (match ys
      ((Nil) acc)
      ((Cons y ys)
       (if (pred y)
           (append-rev ys acc)
           (remove-rev-if pred ys (Cons y acc))))))

  (declare remove-if ((:a -> Boolean) -> (List :a) -> (List :a)))
  (define (remove-if pred xs)
    "Return a new list with the first element for which PRED is `True` is removed."
    (%reverse! (remove-rev-if pred xs Nil)))

  (declare remove (Eq :a => (:a -> (List :a) -> (List :a))))
  (define (remove x ys)
    "Return a new list with the first element equal to `x` removed."
    (remove-if (== x) ys))

  (declare difference (Eq :a => ((List :a) -> (List :a) -> (List :a))))
  (define (difference xs ys)
    "Returns a new list with the first occurrence of each element in `ys` removed from `xs`."
    (fold (fn (a b) (remove b a)) xs ys))

  (declare zipWith ((:a -> :b -> :c) -> (List :a) -> (List :b) -> (List :c)))
  (define (zipWith f xs ys)
    "Builds a new list by calling `f` with elements of `xs` and `ys`."
    (%reverse!
     (rec g ((xs xs)
             (ys ys)
             (acc nil))
       (match (Tuple xs ys)
         ((Tuple (Cons x xs) (Cons y ys))
          (g xs ys (Cons (f x y) acc)))
         (_ acc)))))

  (declare zipWith3 ((:a -> :b -> :c -> :d) -> (List :a) -> (List :b) -> (List :c) -> (List :d)))
  (define (zipWith3 f xs ys zs)
    "Build a new list by calling F with elements of XS, YS and ZS"
    (%reverse!
     (rec g ((xs xs)
             (ys ys)
             (zs zs)
             (acc nil))
       (match (Tuple3 xs ys zs)
         ((Tuple3 (Cons x xs) (Cons y ys) (Cons z zs))
          (g xs ys zs (Cons (f x y z) acc)))
         (_ acc)))))

  (declare zipWith4 ((:a -> :b -> :c -> :d -> :e) -> (List :a) -> (List :b) -> (List :c) -> (List :d) -> (List :e)))
  (define (zipWith4 f as bs cs ds)
    "Build a new list by calling F with elements of AS, BS, CS and DS"
    (%reverse!
     (rec g ((as as)
             (bs bs)
             (cs cs)
             (ds ds)
             (acc nil))
       (match (Tuple4 as bs cs ds)
         ((Tuple4 (Cons a as) (Cons b bs) (Cons c cs) (Cons d ds))
          (g as bs cs ds (Cons (f a b c d) acc)))
         (_ acc)))))

  (declare zipWith5 ((:a -> :b -> :c -> :d -> :e -> :f) -> (List :a) -> (List :b) -> (List :c) -> (List :d) -> (List :e) -> (List :f)))
  (define (zipWith5 f as bs cs ds es)
    "Build a new list by calling F with elements of AS, BS, CS, DS and ES"
    (%reverse!
     (rec g ((as as)
             (bs bs)
             (cs cs)
             (ds ds)
             (es es)
             (acc nil))
       (match (Tuple5 as bs cs ds es)
         ((Tuple5 (Cons a as) (Cons b bs) (Cons c cs) (Cons d ds) (Cons e es))
          (g as bs cs ds es (Cons (f a b c d e) acc)))
         (_ acc)))))

  (declare zip ((List :a) -> (List :b) -> (List (Tuple :a :b))))
  (define (zip xs ys)
    "Builds a list of tuples with the elements of XS and YS."
    (zipWith Tuple xs ys))

  (declare countBy ((:a -> Boolean) -> (List :a) -> UFix))
  (define (countBy f things)
    "Count the number of items in THINGS that satisfy the predicate F."
    (fold (fn (sum x)
            (if (f x)
                (+ 1 sum)
                sum))
          0
          things))

  (declare insert (Ord :a => (:a -> (List :a) -> (List :a))))
  (define (insert e ls)
    "Inserts an element into a list at the first place it is less than or equal to the next element."
    (insertBy <=> e ls))

  (declare insertBy ((:a -> :a -> Ord) -> :a -> (List :a) -> (List :a)))
  (define (insertBy cmp x ys)
    "Generic version of insert"
    (%reverse!
     (rec f ((ys ys)
             (acc Nil))
       (match ys
         ((Nil) (Cons x acc))
         ((Cons y yss)
          (match (cmp x y)
            ((GT) (f yss (Cons y acc)))
            (_    (append-rev ys (Cons x acc)))))))))

  (declare sort (Ord :a => ((List :a) -> (List :a))))
  (define (sort xs)
    "Sort `xs`."
    (sortBy <=> xs))

  (declare sortBy ((:a -> :a -> Ord) -> (List :a) -> (List :a)))
  (define (sortBy cmp xs)
    "Sort `xs` by a custom comparison function `cmp`."
    (lisp (List :a) (cmp xs)
      (cl:sort (cl:copy-list xs)
               (cl:lambda (a b)
                 (cl:eq 'coalton/classes::ord/lt (call-coalton-function cmp a b))))))

  (declare intersperse (:a -> (List :a) -> (List :a)))
  (define (intersperse e xs)
    "Returns a new list by inserting `e` between every element of `xs`."
    (match xs
      ((Nil)       Nil)
      ((Cons x xs) (Cons x (concatMap (fn (y) (make-list e y)) xs)))))

  (declare intercalate ((List :a) -> (List (List :a)) -> (List :a)))
  (define (intercalate xs xss)
    "Intersperse `xs` into `xss` and then concatenate the result."
    (concat (intersperse xs xss)))

  (declare insertions (:a -> List :a -> (List (List :a))))
  (define (insertions a l)
    "Produce a list of copies of `l`, each with A inserted at a possible position.

    (insertions 0 (make-list 1 2))
    ;; => ((0 1 2) (1 0 2) (1 2 0))
"
    (match l
      ((Nil)       (make-list (make-list a)))
      ((Cons x ls) (Cons (Cons a l)
                         (map (Cons x) (insertions a ls))))))

  (declare transpose ((List (List :a)) -> (List (List :a))))
  (define (transpose xs)
    "Transposes a matrix represented by a list of lists."
    (match xs
      ((Nil) Nil)
      ((Cons (Nil) xss) (transpose xss))
      ((Cons (Cons x xs) xss)
       (Cons (Cons x (map
                      (fn (ys)
                        (match ys
                          ((Cons h _) h)
                          ((Nil) (error "Invalid shape"))))
                      xss))

             (transpose (Cons xs (map
                                  (fn (ys)
                                    (match ys
                                      ((Cons _ t) t)
                                      ((Nil) (error "Invalid shape"))))
                                  xss)))))))

  (declare partition ((:a -> Boolean) -> (List :a) -> (Tuple (List :a) (List :a))))
  (define (partition f xs)
    "Splits a list into two new lists. The first list contains elements matching predicate F."
    (let ((inner (fn (xs as bs)
                   (match xs
                     ((Nil) (Tuple as bs))
                     ((Cons x xs)
                      (if (f x)
                          (inner xs (Cons x as) bs)
                          (inner xs as (Cons x bs))))))))
      (inner xs Nil Nil)))

  (declare equivalence-classes-by ((:a -> :a -> Boolean) -> (List :a) -> (List (List :a))))
  (define (equivalence-classes-by f l)
    "Break a list into a list of equivalence classes according to an equivalence relation."
    (rec g ((remaining l)
            (partitions Nil))
      (match remaining
        ((Nil) partitions)
        ((Cons x rst)
         (match (partition (f x) rst)
           ((Tuple yes no)
            (g no (Cons (Cons x yes) partitions))))))))

  (declare equivalence-classes (Eq :a => ((List :a) -> (List (List :a)))))
  (define equivalence-classes (equivalence-classes-by ==))

  (declare optimumBy ((:a -> :a -> Boolean)
                      -> (List :a)
                      -> (Optional :a)))
  (define (optimumBy f xs)
    "Returns an optimum according to a total order."
    (match xs
      ((Nil) None)
      ((Cons x xs)
       (Some
        (fold (fn (opt x)
                (if (f x opt)
                    x
                    opt))
              x xs)))))

  (declare maximum (Ord :a => ((List :a) -> (Optional :a))))
  (define (maximum l)
    "Returns a greatest element of a list, or `None`."
    (optimumBy > l))

  (declare minimum (Ord :a => ((List :a) -> (Optional :a))))
  (define (minimum l)
    "Returns a least element of a list, or `None`."
    (optimumBy < l))

  (declare sum (Num :a => ((List :a) -> :a)))
  (define (sum xs)
    "Returns the sum of `xs`."
    (fold + (fromInt 0) xs))

  (declare product (Num :a => ((List :a) -> :a)))
  (define (product xs)
    "Returns the product of `xs`."
    (fold * (fromInt 1) xs))

  (declare all ((:a -> Boolean) -> (List :a) -> Boolean))
  (define (all f? xs)
    "Returns `True` if every element in `xs` matches `f?`."
    (match xs
      ((Cons x xs)
       (if (f? x)
           (all f? xs)
           False))
      ((Nil) True)))

  (declare any ((:a -> Boolean) -> (List :a) -> Boolean))
  (define (any f? l)
    "Returns `True` if at least one element in `xs` matches `f?`."
    (match l
      ((Cons x xs)
       (if (f? x)
           True
           (any f? xs)))
      ((Nil) False)))

  (declare perms (List :a -> (List (List :a))))
  (define (perms l)
    "Produce all permutations of the list L."
    (foldr (compose concatMap insertions) (make-list Nil) l))

  (declare combs (List :a -> (List (List :a))))
  (define (combs l)
    "Compute a list of all combinations of elements of `l`. This function sometimes goes by the name \"power set\" or \"subsets\".

The ordering of elements of `l` is preserved in the ordering of elements in each list produced by this function."
    (match l
      ((Nil)
       (make-list Nil))
      ((Cons x xs)
       (concatMap (fn (y) (make-list y (Cons x y))) (combs xs)))))

  (declare combsOf (UFix -> List :a -> (List (List :a))))
  (define (combsOf n l)
    "Produce a list of size-N subsets of `l`.

The ordering of elements of `l` is preserved in the ordering of elements in each list produced by `(combsOf n l)`.

This function is equivalent to all size-`n` elements of `(combs l)`."

    (cond ((== 0 n) (make-list Nil))
          ((== 1 n) (map singleton l))
          (True (match l
                  ((Nil) Nil)
                  ((Cons x xs) (append
                                (map (Cons x) (combsOf (- n 1) xs)) ; combs with X
                                (combsOf n xs))))))) ; and without x

  ;;
  ;; Instances
  ;;

  (define-instance (Eq :a => Eq (List :a))
    (define (== a b)
      (match a
        ((Cons x xs)
         (match b
           ((Cons y ys)
            (and (== x y)
                 (== xs ys)))
           (_ False)))
        ((Nil)
         (match b
           ((Nil) True)
           (_ False))))))

  ;; <=> on lists uses lexicographic order, like strings.
  ;; Nil is the smallest list, and is LT any non-nil list.
  ;; Two Conses with non-EQ cars are ordered based on the ordering of the cars, so (0 ...) is less than (1
  ;; ...) no matter the ...s.
  ;; Two Conses with EQ cars recurse into the tails for their ordering, so (0 0 ...) is less than (0 1 ...) no
  ;; matter the ...s.
  (define-instance (Ord :elt => Ord (List :elt))
    (define (<=> left right)
      (match (Tuple left right)
        ((Tuple (Nil) (Nil)) Eq)
        ((Tuple (Nil) _) LT)
        ((Tuple _ (Nil)) GT)
        ((Tuple (Cons left-head left-tail) (Cons right-head right-tail))
         (if (== left-head right-head)
             (<=> left-tail right-tail)
             (<=> left-head right-head))))))

  (define-instance (Hash :a => Hash (List :a))
    (define (hash lst)
      (fold (fn (so-far elt)
              (combine-hashes so-far (hash elt)))
            mempty
            lst)))

  (define-instance (Semigroup (List :a))
    (define (<> a b) (append a b)))

  (define-instance (Monoid (List :a))
    (define mempty Nil))

  (define-instance (Functor List)
    (define (map f l)
      (%reverse! (fold (fn (a x) (Cons (f x) a)) Nil l))))

  (define-instance (Applicative List)
    (define (pure x) (Cons x Nil))
    (define (liftA2 f as bs)
      (concatMap (fn (a)
                   (map (f a) bs))
                 as)))

  (define-instance (Alternative List)
    (define (alt a b)
      (append a b))
    (define empty Nil))

  (define-instance (Monad List)
    (define (>>= m f)
      (concatMap f m)))

  (define-instance (Foldable List)
    (define (fold f y xs)
      (match xs
        ((Cons x xs) (fold f (f y x) xs))
        ((Nil) y)))

    (define (foldr f y xs)
      (match xs
        ((Cons x xs) (f x (foldr f y xs)))
        ((Nil) y))))

  (define-instance (Traversable List)
    (define (traverse f xs)
      (match xs
        ((Cons x xs) (liftA2 Cons (f x) (traverse f xs)))
        ((Nil) (pure Nil)))))

  (define-instance (iter:IntoIterator (List :elt) :elt)
    (define (iter:into-iter list)
      (let remaining = (cell:new list))
      (iter:with-size
          (fn ()
            (cell:pop! remaining))
        (length list))))

  (define-instance (iter:FromIterator (List :elt) :elt)
    (define (iter:collect! iter)
      ;; Dropping into lisp is necessary because building a list from
      ;; front to back requires mutability.
      (lisp (List :elt) (iter)
        (cl:loop
           :with top := cl:nil
           :with current := cl:nil
           :for res := (iter:next! iter)
           :while (some? res)
           :do (cl:if current
                      (cl:progn
                        (cl:setf (cl:cdr current) (cl:cons (from-some "" res) cl:nil))
                        (cl:setf current (cl:cdr current)))
                      (cl:progn
                        (cl:setf top (cl:cons (from-some "" res) cl:nil))
                        (cl:setf current top)))
           :finally (cl:return top)))))

  (define-instance (Into (Optional :a) (List :a))
    (define (into opt)
      (match opt
        ((None) Nil)
        ((Some a) (Cons a Nil)))))

  (define-instance (Default (List :a))
    (define (default) Nil)))

(defmacro make (cl:&rest elements)
  "Make a homogeneous list of `elements`. Synonym for `coalton:make-list`."
  `(coalton:make-list ,@elements))

#+sb-package-locks
(sb-ext:lock-package "COALTON/LIST")
