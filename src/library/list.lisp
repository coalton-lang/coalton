(in-package #:coalton-library)

(coalton-toplevel

  ;;
  ;; Cons Lists
  ;;

  ;; List is an early type

  (declare head ((List :a) -> (Optional :a)))
  (define (head l)
    "Returns the first element of a list."
    (match l
      ((Cons x _) (Some x))
      ((Nil) None)))

  (declare tail ((List :a) -> (Optional (List :a))))
  (define (tail l)
    "Returns every element except the first in a list."
    (match l
      ((Cons _ xs) (Some xs))
      ((Nil) None)))

  (define (car x)
    "Return the traditional car of a list XS. (Error when there is no car.)"
    (match x
      ((Cons x _) x)
      ((Nil) (error "there is no first element"))))

  (declare cdr ((List :a) -> (List :a)))
  (define (cdr xs)
    "Return the traditional cdr of a list XS."
    (match xs
      ((Cons _ xs) xs)
      ((Nil) Nil)))

  (declare last ((List :a) -> (Optional :a)))
  (define (last l)
    "Returns the last element of a list."
    (match l
      ((Cons x (Nil)) (Some x))
      ((Cons _ xs) (last xs))
      ((Nil) None)))

  (declare init ((List :a) -> (List :a)))
  (define (init l)
    "Returns every element except the last in a list."
    (lisp (List :a) (l)
      (cl:butlast l)))

  (declare null ((List :a) -> Boolean))
  (define (null xs)
    "Returns TRUE if XS is an empty list."
    (match xs
      ((Nil) True)
      (_ False)))

  (declare singleton (:a -> (List :a)))
  (define (singleton x)
    "Returns a single element list containg only X."
    (Cons x Nil))

  (declare repeat (Integer -> :a -> (List :a)))
  (define (repeat n x)
    "Returns a list with X repeated N times."
    (if (== 0 n)
        Nil
        (Cons x (repeat (- n 1) x))))

  (declare reverse ((List :a) -> (List :a)))
  (define (reverse xs)
    "Returns a new list containing the same elements in reverse order."
    (let ((inner (fn (as bs)
                   (match as
                     ((Nil) bs)
                     ((Cons a as) (inner as (Cons a bs)))))))
      (inner xs Nil)))

  (declare drop (Integer -> (List :a) -> (List :a)))
  (define (drop n xs)
    "Returns a list with the first N elements of XS removed"
    (if (== n 0)
        xs
        (match xs
          ((Cons _ xs)
           (drop (- n 1) xs))
          ((Nil) Nil))))

  (declare take (Integer -> (List :a) -> (List :a)))
  (define (take n xs)
    "Returns the first N elements of XS"
    (if (== n 0)
        Nil
        (match xs
          ((Cons x xs)
           (Cons x (take (- n 1) xs)))
          ((Nil) Nil))))

  (declare find ((:a -> Boolean) -> (List :a) -> (Optional :a)))
  (define (find f xs)
    "Returns the first element in a list matching the predicate function F."
    (fold (fn (a b)
            (match b
              ((Some _) b)
              (_
               (if (f a) (Some a) None))))
          None xs))

  (declare fold ((:a -> :b -> :b) -> :b -> (List :a) -> :b))
  (define (fold f y xs)
    "Tail recursive left fold on lists."
    (match xs
      ((Cons x xs) (fold f (f x y) xs))
      ((Nil) y)))

  (declare foldr ((:a -> :b -> :b) -> :b -> (List :a) -> :b))
  (define (foldr f y xs)
    "Right fold on lists. Is not tail recursive."
    (match xs
      ((Cons x xs) (f x (foldr f y xs)))
      ((Nil) y)))

  (declare filter ((:a -> Boolean) -> (List :a) -> (List :a)))
  (define (filter f xs)
    "Returns a new list containing every element of XS that matches the predicate function F in the same order."
    (let ((fun (fn (xs ys)
                 (match xs
                   ((Nil)
                    (reverse ys))
                   ((Cons x xs)
                    (if (f x)
                        (fun xs (Cons x ys))
                        (fun xs ys)))))))
      (fun xs Nil)))

  (declare length ((List :a) -> Integer))
  (define (length l)
    "Returns the length of a list."
    (fold (fn (a b)
            (+ 1 b))
          0
          l))

  (declare index ((List :a) -> Integer -> (Optional :a)))
  (define (index xs i)
    "Returns the Ith element of XS."
    (match xs
      ((Nil)
       None)
      ((Cons x xs)
       (if (== 0 i)
           (Some x)
           (index xs (- i 1))))))

  (declare nth (Integer -> (List :t) -> :t))
  (define (nth n l)
    "Like INDEX, but errors if the index is not found."
    (fromSome "There is no NTH" (index l n)))

  (declare elemIndex (Eq :a => (:a -> (List :a) -> (Optional Integer))))
  (define (elemIndex x xs)
    (findIndex (== x) xs))

  (declare findIndex ((:a -> Boolean) -> (List :a) -> (Optional Integer)))
  (define (findIndex f xs)
    (let ((find (fn (xs n)
                  (match xs
                    ((Nil)
                     None)
                    ((Cons x xs)
                     (if (f x)
                         (Some n)
                         (find xs (+ n 1))))))))
      (find xs 0)))

  (declare range (Integer -> Integer -> (List Integer)))
  (define (range start end)
    "Returns a list containing the numbers from START to END inclusive."
    (let ((inner (fn (x)
                   (if (> x end)
                       Nil
                       (Cons x (inner (+ 1 x)))))))
      (if (<= start end)
          (inner start)
          (reverse (range end start)))))

  (declare append ((List :a) -> (List :a) -> (List :a)))
  (define (append xs ys)
    "Appends two lists together and returns a new list."
    (match xs
      ((Nil) ys)
      ((Cons x xs) (Cons x (append xs ys)))))

  (declare concat ((List (List :a)) -> (List :a)))
  (define (concat xs)
    "Appends a list of lists together into a single new list."
    (concatMap (fn (x) x) xs))

  (declare concatMap ((:a -> (List :b)) -> (List :a) -> (List :b)))
  (define (concatMap f xs)
    "Apply F to each element in XS and concatenate the results."
    (fold (fn (a b) (append b (f a))) Nil xs))

  (declare member (Eq :a => (:a -> (List :a) -> Boolean)))
  (define (member e xs)
    "Returns true if any element of XS is equal to E."
    (match xs
      ((Cons x xs)
       (if (== x e)
           True
           (member e xs)))
      ((Nil)
       False)))

  (declare union (Eq :a => ((List :a) -> (List :a) -> (List :a))))
  (define (union xs ys)
    "Returns a new list with the elements from both XS and YS and without duplicates."
    (match xs
      ((Cons x xs)
       (if (or (member x ys)
               (member x xs))
           (union xs ys)
           (Cons x (union xs ys))))
      ((Nil) (remove-duplicates ys))))

  (declare intersection (Eq :a => ((List :a) -> (List :a) -> (List :a))))
  (define (intersection xs ys)
    "Returns elements which occur in both lists. Does not return duplicates and does not guarantee order."
    (let ((inner (fn (xs ys)
                   (match xs
                     ((Cons x xs)
                      (if (member x ys)
                          (Cons x (intersection xs ys))
                          (intersection xs ys)))
                     ((Nil) Nil)))))
      (inner (remove-duplicates xs) (remove-duplicates ys))))

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

  (declare remove-duplicates (Eq :a => ((List :a) -> (List :a))))
  (define (remove-duplicates xs)
    "Returns a new list without duplicate elements."
    (match xs
      ((Cons x xs)
       (if (member x xs)
           (remove-duplicates xs)
           (Cons x (remove-duplicates xs))))
      ((Nil) Nil)))

  (declare delete (Eq :a => (:a -> (List :a) -> (List :a))))
  (define (delete x ys)
    "Return a new list with the first element equal to X removed."
    (match ys
      ((Nil)
       Nil)
      ((Cons y ys)
       (if (== x y)
           ys
           (Cons y (delete x ys))))))

  (declare list-difference (Eq :a => ((List :a) -> (List :a) -> (List :a))))
  (define (list-difference xs ys)
    "Returns a new list with the first occurence of each element in YS deleted from XS."
    (fold delete xs ys))

  (declare zipWith ((:a -> :b -> :c) -> (List :a) -> (List :b) -> (List :c)))
  (define (zipWith f xs ys)
    "Builds a new list by calling F with elements of XS and YS."
    (match (Tuple xs ys)
      ((Tuple (Cons x xs)
              (Cons y ys))
       (Cons (f x y) (zipWith f xs ys)))
      (_ Nil)))

  (declare zipWith3 ((:a -> :b -> :c -> :d) -> (List :a) -> (List :b) -> (List :c) -> (List :d)))
  (define (zipWith3 f xs ys zs)
    "Build a new list by calling F with elements of XS, YS and ZS"
    (match (Tuple3 xs ys zs)
      ((Tuple3 (Cons x xs)
               (Cons y ys)
               (Cons z zs))
       (Cons (f x y z) (zipWith3 f xs ys zs)))
      (_ Nil)))

  (declare zipWith4 ((:a -> :b -> :c -> :d -> :e) -> (List :a) -> (List :b) -> (List :c) -> (List :d) -> (List :e)))
  (define (zipWith4 f as bs cs ds)
    "Build a new list by calling F with elements of AS, BS, CS and DS"
    (match (Tuple4 as bs cs ds)
      ((Tuple4 (Cons a as)
               (Cons b bs)
               (Cons c cs)
               (Cons d ds))
       (Cons (f a b c d) (zipWith4 f as bs cs ds)))
      (_ Nil)))

  (declare zipWith5 ((:a -> :b -> :c -> :d -> :e -> :f) -> (List :a) -> (List :b) -> (List :c) -> (List :d) -> (List :e) -> (List :f)))
  (define (zipWith5 f as bs cs ds es)
    "Build a new list by calling F with elements of AS, BS, CS, DS and ES"
    (match (Tuple5 as bs cs ds es)
      ((Tuple5 (Cons a as)
               (Cons b bs)
               (Cons c cs)
               (Cons d ds)
               (Cons e es))
       (Cons (f a b c d e) (zipWith5 f as bs cs ds es)))
      (_ Nil)))

  (declare zip ((List :a) -> (List :b) -> (List (Tuple :a :b))))
  (define (zip xs ys)
    "Builds a list of tuples with the elements of XS and YS."
    (zipWith Tuple xs ys))

  (declare countBy ((:a -> Boolean) -> (List :a) -> Integer))
  (define (countBy f things)
    "Count the number of items in THINGS that satisfy the predicate F."
    (fold (fn (x sum)
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
    (match ys
      ((Nil)
       (make-list x))
      ((Cons y ys_)
       (match (cmp x y)
         ((GT)
          (Cons y (insertBy cmp x ys_)))
         (_
          (Cons x ys))))))

  (declare sort (Ord :a => ((List :a) -> (List :a))))
  (define (sort xs)
    "Performs a stable sort of XS."
    (sortBy <=> xs))

  ;; NOTE: This is a very inefficient implementation and can be
  ;;       replaced with a faster stable sorting algorithm.
  (declare sortBy ((:a -> :a -> Ord) -> (List :a) -> (List :a)))
  (define (sortBy cmp xs)
    "Generic version of sort"
    (foldr (insertBy cmp) Nil xs))

  (declare intersperse (:a -> (List :a) -> (List :a)))
  (define (intersperse e xs)
    "Returns a new list where every other element is E."
    (match xs
      ((Cons x xs) (Cons x (Cons e (intersperse e xs))))
      ((Nil) Nil)))

  (declare intercalate ((List :a) -> (List (List :a)) -> (List :a)))
  (define (intercalate xs xss)
    "Intersperses XS into XSS and then concatenates the result."
    (concat (intersperse xs xss)))

  (declare transpose ((List (List :a)) -> (List (List :a))))
  (define (transpose xs)
    "Transposes a matrix represented by a list of lists."
    (match xs
      ((Nil)
       Nil)
      ((Cons (Nil) xss)
       (transpose xss))
      ((Cons (Cons x xs) xss)
       (Cons (Cons x (map
                      (fn (ys)
                        (match ys
                          ((Cons h _) h)))
                      xss))
             (transpose (Cons xs (map
                                  (fn (ys)
                                    (match ys
                                      ((Cons _ t) t)))
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
  (define maximum
    "Returns a greatest element of a list, or None."
    (optimumBy >))

  (declare minimum (Ord :a => ((List :a) -> (Optional :a))))
  (define minimum
    "Returns a least element of a list, or None."
    (optimumBy <))

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

  ;;
  ;; List instances
  ;;

  (define-instance (Eq :a => (Eq (List :a)))
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

  (define-instance (Semigroup (List :a))
    (define (<> a b) (append a b)))

  (define-instance (Monoid (List :a))
    (define mempty Nil))

  (define-instance (Functor List)
    (define (map f l)
      (match l
        ((Cons x xs) (Cons (f x) (map f xs)))
        ((Nil) Nil))))

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
      (concatMap f m))
    (define (>> a b)
      (>>= a (fn (_) b)))))
