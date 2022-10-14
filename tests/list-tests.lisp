(cl:in-package #:coalton-native-tests)

(coalton-toplevel
  (declare x (List Integer))
  (define x (make-list 1 2 3))

  (declare n (List Integer))
  (define n Nil)

  (define (set== a b)
    (and (list:null? (list:difference a b))
         (list:null? (list:difference b a)))))

(define-test test-basics ()
  (is (== (list:head x) (Some 1)))
  (is (== (list:head n) None))

  (is (== (list:tail x) (Some (make-list 2 3))))
  (is (== (list:tail n) None))

  (is (== (list:car x) 1))
  ;(is (== (car (make-list)) None))

  (is (== (list:cdr x) (make-list 2 3)))
  (is (== (list:cdr n) Nil)))

(define-test test-selectors ()
  (is (== (list:last x) (Some 3)))
  (is (== (list:last n) None))

  (is (== (list:init x) (make-list 1 2)))
  (is (== (list:init n) n))

  (is (list:null? Nil))
  (is (not (list:null? x))))

(define-test test-constructors ()
  (is (== (list:singleton 3) (make-list 3)))
  (is (== (list:repeat 3 0) (make-list 0 0 0)))
  (is (== (list:reverse x) (make-list 3 2 1))))

(define-test test-droptake ()
  (is (== (list:drop 2 x) (make-list 3)))
  (is (== (list:take 2 x) (make-list 1 2))))

(define-test test-search ()
  (is (== (list:find even? x)  (Some 2)))
  (is (== (list:find (< 10) x) None))

  (is (== (list:filter odd? x) (make-list 1 3)))

  (is (== (list:index 0 x) (Some 1)))
  (is (== (list:index 3 x) None))

  (is (== (list:nth 0 x) 1))

  (is (== (list:elemIndex 2 x) 1))

  (is (== (list:findIndex even? x) 1))

  (is (== (list:length x) 3)))

(define-test test-operators ()
  (is (== (list:range 1 3) x))

  (is (== (list:append x x) (make-list 1 2 3 1 2 3)))

  (is (== (list:concat (make-list x x x)) (make-list 1 2 3 1 2 3 1 2 3)))

  (is (== (list:concatMap list:cdr (make-list x x x)) (make-list 2 3 2 3 2 3)))

  (is (list:member 2 x)))

(define-test test-set-basics ()

  (is (== (list:union (list:append x x) x) x))
  (is (== (list:union Nil (list:append (Cons 4 x) x)) (Cons 4 x)))

  (is (set== (list:intersection x x) x))
  (is (set== (list:intersection (list:append x (make-list 0 9 8 7)) (Cons 4 x)) x)))

(define-test test-lookup ()
  (is (== (list:lookup "two"
                       (make-list
                        (Tuple "one"   1)
                        (Tuple "two"   2)
                        (Tuple "three" 3)))
          (Some 2)))

  (is (== (list:lookup "four"
                       (make-list
                        (Tuple "one"   1)
                        (Tuple "two"   2)
                        (Tuple "three" 3)))
          None)))

(define-test test-removal ()
  (is (== (list:remove-duplicates (make-list 1 3 2 2 3)) x))

  (is (== (list:delete 2 x) (make-list 1 3)))
  (is (== (list:delete 4 x) x))

  (is (== (list:difference x x) Nil))
  (is (== (list:difference x Nil) x)))

(define-test test-zips ()  
  (is (== (list:zipWith + x x) (make-list 2 4 6)))

  (is (== (list:zipWith3 Tuple3 x x x)
          (make-list (Tuple3 1 1 1) (Tuple3 2 2 2) (Tuple3 3 3 3))))

  (is (== (list:zipWith4 Tuple4 x x x x)
          (make-list (Tuple4 1 1 1 1) (Tuple4 2 2 2 2) (Tuple4 3 3 3 3))))

  (is (== (list:zipWith5 Tuple5 x x x x x)
          (make-list (Tuple5 1 1 1 1 1) (Tuple5 2 2 2 2 2) (Tuple5 3 3 3 3 3))))

  (is (== (list:zip x x) (make-list (Tuple 1 1) (Tuple 2 2) (Tuple 3 3)))))

(define-test test-sorting ()  
  (is (== (list:countBy even? x) 1))

  (is (== (list:insert 2 (make-list 1 3)) x))

  (is (== (list:insertBy (fn (_a _b) LT) 2 (make-list 1 3)) (make-list 2 1 3)))

  (is (== (list:sort (list:append x x)) (make-list 1 1 2 2 3 3)))

  (is (== (list:sortBy (fn (a b) (<=> (negate a) (negate b))) x)
          (make-list 3 2 1))))

(define-test test-array-functions ()

  (is (== (list:intersperse 0 x) (make-list 1 0 2 0 3)))

  (is (== (list:intercalate (make-list 0 5) (map list:singleton x))
          (make-list 1 0 5 2 0 5 3)))

  (is (== (list:insertions 0 x)
          (make-list
           (make-list 0 1 2 3)
           (make-list 1 0 2 3)
           (make-list 1 2 0 3)
           (make-list 1 2 3 0))))

  (is (==
       (list:transpose (make-list
                        (make-list 0 1 2 3)
                        (make-list 4 5 6 7)
                        (make-list 9 0 1 2)
                        (make-list 3 4 5 6)))
       (make-list
        (make-list 0 4 9 3)
        (make-list 1 5 0 4)
        (make-list 2 6 1 5)
        (make-list 3 7 2 6))))

  (match (list:partition even? x)
    ((Tuple x y)
     (is (== x (make-list 2)))
     (is (set== y (make-list 1 3)))))

  (is (set==
       (list:equivalence-classes-by
        (fn (x y)
          (== (math:mod x 2) (math:mod y 2)))
        x)
       (make-list
        (make-list 1 3)
        (make-list 2))))

  (is (set==
       (list:equivalence-classes (list:append x x))
       (make-list
        (make-list 1 1)
        (make-list 2 2)
        (make-list 3 3)))))

(define-test test-reductions ()

  (let by-len =
    (the ((List Integer) -> (List Integer) -> Boolean)
         (fn (x y) (> (list:length x) (list:length y)))))

  (is (== (list:optimumBy by-len Nil) None))
  (is (== (list:optimumBy by-len (make-list x (make-list 1 3) n)) (Some x)))

  (is (== (list:maximum x) (Some 3)))
  (is (== (list:maximum n) None))

  (is (== (list:minimum x) (Some 1)))
  (is (== (list:minimum n) None))

  (is (== (list:sum x) 6))
  (is (== (list:sum Nil) 0))

  (is (== (list:product x) 6))
  (is (== (list:product Nil) 1))

  (is (== (list:all even? x) False))
  (is (== (list:all (> 10) x) True))

  (is (== (list:any even? x) True))
  (is (== (list:any (< 10) x) False))

  Unit)

(define-test test-combinatorics ()
  (is (== (list:split #\, "one,two,three") (make-list "one" "two" "three")))
  (is (== (list:split #\, "one,,three") (make-list "one" "" "three")))

  (is (set== (list:perms x)
             (make-list
              (make-list 1 2 3)
              (make-list 1 3 2)
              (make-list 2 1 3)
              (make-list 2 3 1)
              (make-list 3 1 2)
              (make-list 3 2 1))))

  (is (set== (list:combs x)
             (make-list
              (make-list 1 2 3)
              (make-list 1 2)
              (make-list 1 3)
              (make-list 2 3)
              (make-list 1)
              (make-list 2)
              (make-list 3)
              (make-list))))

  (is (set== (list:combsOf 2 x)
             (make-list
              (make-list 1 2)
              (make-list 1 3)
              (make-list 2 3)))))

(define-test test-instances ()
  (is (== (map (+ 1) x) (make-list 2 3 4)))
  (is (== (map (+ 1) x) (make-list 2 3 4)))

  (is (== x (<> x mempty)))
  (is (== x (<> mempty x)))

  (is (== x (alt x empty)))
  (is (== (make-list 1 2 3 1 2 3) (alt x x)))

  ;; Folds are tested in class-tests

  ;; From the Data.Traversable docs
  (is (== (traverse
           (fn (x)
             (if (odd? x)
                 (Some x)
                 None))
           x)
          None))

  (is (== (traverse
           (fn (x)
             (if (> 10 x)
                 (Some x)
                 None))
           x)
          (Some x))))

(define-test list-lexographic-order ()
  (let is-lt = (fn (smaller larger)
                 (is (== LT (<=> smaller larger)))
                 (is (== GT (<=> larger smaller)))))
  (let is-eq = (fn (a b)
                 (is (== EQ (<=> a b)))
                 (is (== a b))))
  (is-eq Nil Nil)
  (is-lt Nil (make-list 0))
  (is-eq (make-list 0) (make-list 0))
  (is-lt (make-list 0) (make-list 1))
  (is-lt (make-list 0 9) (make-list 1))
  (is-lt (make-list 0 0) (make-list 0 1))
  (is-lt (make-list 0 0 9) (make-list 0 1 0)))
