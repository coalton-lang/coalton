(coalton-library/utils::defstdlib-package #:coalton-library/collections/immutable/list
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/hash
   #:coalton-library/functions
   #:coalton-library/tuple
   #:coalton-library/optional)
  (:local-nicknames
   (#:cln #:coalton-library/collections/classes)
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator)
   (#:o #:coalton-library/optional)
   (#:math #:coalton-library/math))
  (:export
   #:car
   #:cdr
   #:init
   #:null?
   #:singleton
   #:singleton?
   #:index
   #:nth
   #:nth-cdr
   #:range
   #:append
   #:concat
   #:concatMap
   #:union
   #:intersection
   #:lookup
   #:remove-if
   #:remove
   #:difference
   #:zipWith3
   #:zipWith4
   #:zipWith5
   #:insert
   #:insertBy
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
   #:split
   #:perms
   #:combs
   #:combsOf))

(in-package #:coalton-library/collections/immutable/list)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  
  ;;
  ;; Cons Lists
  ;;

  ;; List is an early type
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                       ;;;
  ;;; Datatype Definition & Internal Helper Functions                       ;;;
  ;;;                                                                       ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; This is only for internal usage
  (define (%reverse as bs)
    (match as
      ((Nil) bs)
      ((Cons a as) (%reverse as (Cons a bs)))))

  ;; This is only for internal usage
  (declare %reverse! (List :a -> List :a))
  (define (%reverse! xs)
    "A mutating reverse operation. After (%reverse! LST), LST must not be referenced; the original list, and all its sublists, should be treated as consumed by this operation. Callers should use the return value and only the return value."
    (lisp (List :a) (xs)
      (cl:nreverse xs)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                       ;;;
  ;;; Collections API Methods (Shouldn't be exported)                       ;;;
  ;;;                                                                       ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;
  ;;; Collection List
  ;;;

  (declare repeat (UFix -> :a -> List :a))
  (define (repeat n x)
    "Returns a list with the same value repeated multiple times."
    (let ((rec
            (fn (k acc)
              (if (== k 0)
                  acc
                  (rec (- k 1) (Cons x acc))))))
      (rec n Nil)))

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

  (declare remove-duplicates (Eq :a => ((List :a) -> (List :a))))
  (define (remove-duplicates xs)
    "Returns a new list without duplicate elements."
    (%reverse! (remove-duplicates-rev xs Nil)))

  (declare empty? (List :a -> Boolean))
  (define (empty? l)
    (match l
      ((Nil) True)
      (_     False)))

  (declare length (List :a -> UFix))
  (define (length l)
    "Returns the length of a list."
    (fold (fn (a _)
            (+ 1 a))
          0
          l))

  (declare member (Eq :a => (:a -> (List :a) -> Boolean)))
  (define (member e xs)
    "Returns true if any element of XS is equal to E."
    (match xs
      ((Cons x xs)
       (if (== x e)
           True
           (member e xs)))
      ((Nil) False)))

  (declare contains-where? ((:a -> Boolean) -> List :a -> Boolean))
  (define (contains-where? f l)
    (match l
      ((Nil) False)
      ((Cons x xs)
       (if (f x)
           True
           (contains-where? f xs)))))

  (declare countBy ((:a -> Boolean) -> (List :a) -> UFix))
  (define (countBy f things)
    "Count the number of items in THINGS that satisfy the predicate F."
    (fold (fn (sum x)
            (if (f x)
                (+ 1 sum)
                sum))
          0
          things))
  
  ;;;
  ;;; LinearCollection List
  ;;;

  (declare head (List :a -> Optional :a))
  (define (head l)
    "Returns the first element of a list."
    (match l
      ((Cons x _) (Some x))
      ((Nil) None)))
  
  (declare last (List :a -> Optional :a))
  (define (last l)
    "Returns the last element of a list."
    (match l
      ((Cons x (Nil)) (Some x))
      ((Cons _ xs) (last xs))
      ((Nil) None)))
  
  (declare tail (List :a -> List :a))
  (define (tail l)
    "Returns every element except the first in a list."
    (match l
      ((Cons _ xs) xs)
      ((Nil) Nil)))

  (declare take (UFix -> List :a -> List :a))
  (define (take n xs)
    "Returns the first N elements of a list."
    (let ((rec
            (fn (n in out)
              (if (== n 0)
                  out
                  (match in
                    ((Cons x xs) (rec (- n 1) xs (Cons x out)))
                    ((Nil) out))))))
      (%reverse! (rec n xs Nil))))

  (declare drop (UFix -> List :a -> List :a))
  (define (drop n xs)
    "Returns a list with the first N elements removed."
    (if (== n 0)
        xs
        (match xs
          ((Cons _ xs)
           (drop (- n 1) xs))
          ((Nil) Nil))))
  
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

  (declare find ((:a -> Boolean) -> List :a -> Optional :a))
  (define (find f xs)
    "Returns the first element in a list matching the predicate function F."
    (fold (fn (a b)
            (match a
              ((Some _) a)
              (_
               (if (f b) (Some b) None))))
          None xs))
 
  (declare indices-where ((:a -> Boolean) -> List :a -> List UFix))
  (define (indices-where f lst)
    (let ((recur (fn (rem n indices)
                   (match rem
                     ((Nil) indices)
                     ((Cons x xs)
                      (if (f x)
                        (recur xs (+ 1 n) (Cons n indices))
                        (recur xs (+ 1 n) indices)))))))
      (%reverse! (recur lst 0 Nil))))
  
  (declare subseq (UFix -> UFix -> List :a -> List :a))
  (define (subseq start end lst)
    (if (> start end)
      Nil
      (take (- end start) (drop start lst))))
  
  (declare split-where ((:a -> Boolean) -> List :a -> List (List :a)))
  (define (split-where pred lst)
    (let ((recur (fn (rem cur-split splits)
                  ;  (traceobject "Rem" rem)
                  ;  (traceobject "Current Split" cur-split)
                  ;  (traceobject "Splits" splits)
                  ;  (trace "----")
                   (match rem
                     ((Nil)
                      (%reverse! (Cons (%reverse! cur-split)
                                             splits)))
                     ((Cons x xs)
                      (if (pred x)
                        (recur xs
                               Nil
                               (Cons (%reverse! cur-split) splits))
                        (recur xs
                               (Cons x cur-split)
                               splits)))))))
      (recur lst Nil Nil)))

  (declare reverse (List :a -> List :a))
  (define (reverse xs)
    "Returns a new list containing the same elements in reverse order."
    ;; like (fold (flip Cons) Nil xs)
    (%reverse xs Nil))

  (declare sort (Ord :a => ((List :a) -> (List :a))))
  (define (sort xs)
    "Performs a sort of XS."
    (sortBy <=> xs))

  (declare sortBy ((:a -> :a -> Ord) -> (List :a) -> (List :a)))
  (define (sortBy cmp xs)
    "Generic version of sort"
    (lisp (List :a) (cmp xs)
      (cl:sort (cl:copy-list xs)
               (cl:lambda (a b)
                 (cl:eq 'coalton-library/classes::ord/lt (call-coalton-function cmp a b))))))

  (declare zip-itr (iter:IntoIterator :m :b => List :a -> :m -> List (Tuple :a :b)))
  (define (zip-itr lst col)
    (zip-with-itr Tuple lst col))

  (declare zip-with-itr (iter:IntoIterator :m :b => (:a -> :b -> :c) -> List :a -> :m -> List :c))
  (define (zip-with-itr f lst col)
    (let ((it (iter:into-iter col))
          (rec
           (fn (rem acc)
             (match (Tuple rem (iter:next! it))
               ((Tuple (Cons a xs) (Some b))
                (rec xs (Cons (f a b) acc)))
               (_ acc)))))
      (%reverse! (rec lst Nil))))             

  ;; TODO: Make this more effecient. Walk the original front to back in Common Lisp,
  ;; mutating the CDR of new Cons cells as we go until we get to the end, then mutate
  ;; a new Cons cell with the new element at the end.
  (declare push-end (:a -> List :a -> List :a))
  (define (push-end elt lst)
    "Add an element to the end of the list."
    (reverse (Cons elt (reverse lst))))
  
  ;; TODO: Make this more effecient. Walk the original front to back in Common Lisp,
  ;; mutating the CDR of new Cons cells as we go until we get to `i`, then mutate
  ;; a new Cons cell with the new element at the end.
  (declare insert-at (UFix -> :a -> List :a -> List :a))
  (define (insert-at i elt lst)
    (append (take i lst) (Cons elt (drop i lst))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                       ;;;
  ;;; Extra Public Functions                                                ;;;
  ;;;                                                                       ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (declare car (List :a -> :a))
  (define (car x)
    "Return the traditional car of a list. This function is partial"
    (match x
      ((Cons x _) x)
      ((Nil) (error "there is no first element"))))

  (declare cdr (List :a -> List :a))
  (define (cdr xs)
    "Return the traditional cdr of a list."
    (match xs
      ((Cons _ xs) xs)
      ((Nil) Nil)))

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

  (declare concat (List (List :a) -> List :a))
  (define (concat xs)
    "Appends a list of lists together into a single new list."
    (concatMap (fn (x) x) xs))

  (declare concatMap ((:a -> (List :b)) -> List :a -> List :b))
  (define (concatMap f xs)
    "Apply F to each element in XS and concatenate the results."
    (%reverse! (fold (fn (a b) (append-rev (f b) a)) Nil xs)))

  (declare union (Eq :a => ((List :a) -> (List :a) -> (List :a))))
  (define (union xs ys)
    "Returns a new list with the elements from both XS and YS and without duplicates."
    (let ((rec
            (fn (xs acc)
              (match xs
                ((Nil) acc)
                ((Cons x xs)
                 (if (or (member x ys)
                         (member x ys))
                     (rec xs acc)
                     (rec xs (Cons x acc))))))))
      (%reverse! (remove-duplicates-rev ys (rec xs Nil)))))

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
    "Returns a new list with the first occurence of each element in `ys` removed from `xs`."
    (fold (fn (a b) (remove b a)) xs ys))

  (declare zipWith3 ((:a -> :b -> :c -> :d) -> (List :a) -> (List :b) -> (List :c) -> (List :d)))
  (define (zipWith3 f xs ys zs)
    "Build a new list by calling F with elements of XS, YS and ZS"
    (let ((rec
            (fn (xs ys zs acc)
              (match (Tuple3 xs ys zs)
                ((Tuple3 (Cons x xs) (Cons y ys) (Cons z zs))
                 (rec xs ys zs (Cons (f x y z) acc)))
                (_ acc)))))
      (%reverse! (rec xs ys zs nil))))

  (declare zipWith4 ((:a -> :b -> :c -> :d -> :e) -> (List :a) -> (List :b) -> (List :c) -> (List :d) -> (List :e)))
  (define (zipWith4 f as bs cs ds)
    "Build a new list by calling F with elements of AS, BS, CS and DS"
    (let ((rec
            (fn (as bs cs ds acc)
              (match (Tuple4 as bs cs ds)
                ((Tuple4 (Cons a as) (Cons b bs) (Cons c cs) (Cons d ds))
                 (rec as bs cs ds (Cons (f a b c d) acc)))
                (_ acc)))))
      (%reverse! (rec as bs cs ds nil))))

  (declare zipWith5 ((:a -> :b -> :c -> :d -> :e -> :f) -> (List :a) -> (List :b) -> (List :c) -> (List :d) -> (List :e) -> (List :f)))
  (define (zipWith5 f as bs cs ds es)
    "Build a new list by calling F with elements of AS, BS, CS, DS and ES"
    (let ((rec
            (fn (as bs cs ds es acc)
              (match (Tuple5 as bs cs ds es)
                ((Tuple5 (Cons a as) (Cons b bs) (Cons c cs) (Cons d ds) (Cons e es))
                 (rec as bs cs ds es (Cons (f a b c d e) acc)))
                (_ acc)))))
      (%reverse! (rec as bs cs ds es nil))))

  (declare insert (Ord :a => (:a -> (List :a) -> (List :a))))
  (define (insert e ls)
    "Inserts an element into a list at the first place it is less than or equal to the next element."
    (insertBy <=> e ls))

  (declare insertBy ((:a -> :a -> Ord) -> :a -> (List :a) -> (List :a)))
  (define (insertBy cmp x ys)
    "Generic version of insert"
    (let ((rec
            (fn (ys acc)
              (match ys
                ((Nil) (Cons x acc))
                ((Cons y yss)
                 (match (cmp x y)
                   ((GT) (rec yss (Cons y acc)))
                   (_    (append-rev ys (Cons x acc)))))))))
      (%reverse! (rec ys Nil))))

  (declare intersperse (:a -> (List :a) -> (List :a)))
  (define (intersperse e xs)
    "Returns a new list where every other element is E."
    (match xs
      ((Nil)       Nil)
      ((Cons x xs) (Cons x (concatMap (fn (y) (make-list e y)) xs)))))

  (declare intercalate ((List :a) -> (List (List :a)) -> (List :a)))
  (define (intercalate xs xss)
    "Intersperses XS into XSS and then concatenates the result."
    (concat (intersperse xs xss)))

  (declare insertions (:a -> List :a -> (List (List :a))))
  (define (insertions a l)
    "Produce a list of copies of L, each with A inserted at a possible position.

    (insertions 0 (make-list 1 2))
    => ((0 1 2) (1 0 2) (1 2 0))
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
    (let ((rec (fn (remaining partitions)
                 (match remaining
                   ((Nil) partitions)
                   ((Cons x rst)
                    (match (partition (f x) rst)
                      ((Tuple yes no)
                       (rec no (Cons (Cons x yes) partitions)))))))))
      (rec l Nil)))

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
    "Returns a greatest element of a list, or None."
    (optimumBy > l))

  (declare minimum (Ord :a => ((List :a) -> (Optional :a))))
  (define (minimum l)
    "Returns a least element of a list, or None."
    (optimumBy < l))

  (declare sum (Num :a => ((List :a) -> :a)))
  (define (sum xs)
    "Returns the sum of XS"
    (fold + (fromInt 0) xs))

  (declare product (Num :a => ((List :a) -> :a)))
  (define (product xs)
    "Returns the product of XS"
    (fold * (fromInt 1) xs))

  (declare all ((:a -> Boolean) -> (List :a) -> Boolean))
  (define (all f xs)
    "Returns TRUE if every element in XS matches F."
    (match xs
      ((Cons x xs)
       (if (f x)
           (all f xs)
           False))
      ((Nil) True)))

  (declare any ((:a -> Boolean) -> (List :a) -> Boolean))
  (define (any f l)
    "Returns TRUE if at least one element in XS matches F."
    (match l
      ((Cons x xs)
       (if (f x)
           True
           (any f xs)))
      ((Nil) False)))

  (declare split (Char -> String -> (List String)))
  (define (split c str)
    (lisp (List String) (c str)
      (cl:let ((split-chars (cl:list c)))
        (cl:declare (cl:dynamic-extent split-chars))
        (uiop:split-string str :separator split-chars))))

  (declare perms (List :a -> (List (List :a))))
  (define (perms l)
    "Produce all permutations of the list L."
    (foldr (compose concatMap insertions) (make-list Nil) l))

  (declare combs (List :a -> (List (List :a))))
  (define (combs l)
    "Compute a list of all combinations of elements of L. This function is sometimes goes by the name \"power set\" or \"subsets\".

The ordering of elements of L is preserved in the ordering of elements in each list produced by `(COMBS L)`."
    (match l
      ((Nil)
       (make-list Nil))
      ((Cons x xs)
       (concatMap (fn (y) (make-list y (Cons x y))) (combs xs)))))

  (declare combsOf (UFix -> List :a -> (List (List :a))))
  (define (combsOf n l)
    "Produce a list of size-N subsets of L.

The ordering of elements of L is preserved in the ordering of elements in each list produced by `(COMBSOF N L)`.

This function is equivalent to all size-N elements of `(COMBS L)`."

    (cond ((== 0 n) (make-list Nil))
          ((== 1 n) (map singleton l))
          (True (match l
                  ((Nil) Nil)
                  ((Cons x xs) (append
                                (map (Cons x) (combsOf (- n 1) xs)) ; combs with X
                                (combsOf n xs))))))) ; and without x
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                       ;;;
  ;;; Instances                                                             ;;;
  ;;;                                                                       ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (define (default) Nil))
  
  (define-instance (cln:Collection (List :a) :a)
    (define (cln:new-collection)
      Nil)
    (define (cln:new-repeat n elt)
      (repeat n elt))
    (define (cln:new-from n f)
      (if (== 0 n)
        Nil
        (map f (range 0 (- n 1)))))
    (define (cln:new-convert coll)
      (iter:collect! (iter:into-iter coll)))
    ;; (define (flatten lst)
    ;;   (fold append (new-collection) lst))
    (define cln:filter filter)
    (define cln:empty? empty?)
    (define cln:length length)
    (define cln:contains-where? contains-where?)
    (define cln:count-where countBy)
    (define cln:add Cons))

  (define-instance (Eq :a => cln:EqCollection (List :a) :a)
    (define cln:remove-duplicates remove-duplicates)
    (define cln:contains-elt? member)
    (define (cln:remove-elt elt lst)
      (filter (/= elt) lst)))

  (define-instance (cln:NestedCollection (List (List :a)) (List :a) :a)
    (define (cln:flatten lst)
      (fold append (cln:new-collection) lst)))

  (define-instance (cln:ImmutableCollection (List :a) :a))

  (define-instance (cln:LinearCollection List)
    (define cln:head head)
    (define (cln:head# lst)
      (o:from-some "Attempted to retrieve head of empty list." (head lst)))
    (define cln:last last)
    (define (cln:last# lst)
      (o:from-some "Attempted to retrieve last element of empty list." (last lst)))
    (define cln:tail tail)
    (define cln:take take)
    (define cln:drop drop)
    (define cln:index-elt elemIndex)
    (define (cln:index-elt# elt lst)
      (o:from-some "Cannot find element in list." (elemIndex elt lst)))
    (define cln:index-where findIndex)
    (define (cln:index-where# pred lst)
      (o:from-some "Cannot find matching element in list." (findIndex pred lst)))
    (define cln:find-where find)
    (define (cln:indices-elt elt lst)
      (indices-where (== elt) lst))
    (define cln:indices-where indices-where)
    (define cln:subseq subseq)
    (define (cln:split-at i lst)
      (Tuple (take i lst) (drop (+ 1 i) lst)))
    (define (cln:split-elt elt lst)
      (split-where (== elt) lst))
    (define cln:split-where split-where)
    (define cln:reverse reverse)
    (define cln:sort sort)
    (define cln:sort-with sortBy)
    (define cln:zip zip-itr)
    (define cln:zip-with zip-with-itr)
    (define cln:push Cons)
    (define cln:push-end push-end)
    (define cln:insert-at insert-at)))

;; #+sb-package-locks
;; (sb-ext:lock-package "COALTON-LIBRARY/COLLECTIONS/IMMUTABLE/LIST")
