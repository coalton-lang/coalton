(cl:in-package #:coalton-native-tests)

;; invariant checking
(coalton-toplevel
  (define-type (InvariantError :elt)
    (RedWithRedLeftChild (red-black/tree:Tree :elt))
    (RedWithRedRightChild (red-black/tree:Tree :elt))
    (DifferentCountToBlack UFix (red-black/tree:Tree :elt)
                           UFix (red-black/tree:Tree :elt))
    (BothChildrenErrors (InvariantError :elt) (InvariantError :elt))
    (IllegalColor (red-black/tree:Tree :elt)))

  (declare count-blacks-to-leaf ((red-black/tree:Tree :elt) -> (Result (InvariantError :elt) UFix)))
  (define (count-blacks-to-leaf tre)
    (match tre
      ((red-black/tree:Empty) (Ok 0))
      ((red-black/tree::Branch (red-black/tree::Red)
                               (red-black/tree::Branch (red-black/tree::Red) _ _ _)
                               _
                               _)
       (Err (RedWithRedLeftChild tre)))
      ((red-black/tree::Branch (red-black/tree::Red)
                               _
                               _
                               (red-black/tree::Branch (red-black/tree::Red) _ _ _))
       (Err (RedWithRedRightChild tre)))
      ((red-black/tree::Branch c left _ right)
       (match (Tuple (count-blacks-to-leaf left)
                     (count-blacks-to-leaf right))
         ((Tuple (Err left-err) (Err right-err))
          (Err (BothChildrenErrors left-err right-err)))
         ((Tuple (Err left-err) _)
          (Err left-err))
         ((Tuple _ (Err right-err))
          (Err right-err))
         ((Tuple (Ok left-ct) (Ok right-ct))
          (if (== left-ct right-ct)
              (match c
                ((red-black/tree::Black) (Ok (+ left-ct 1)))
                ((red-black/tree::Red) (Ok left-ct))
                (_ (Err (IllegalColor tre))))
              (Err (DifferentCountToBlack left-ct left
                                          right-ct right))))))
      (_ (Err (IllegalColor tre))))))

(coalton-toplevel
  (declare random-below! (UFix -> UFix))
  (define (random-below! limit)
    (lisp UFix (limit)
      (cl:random limit)))

  (declare random! (Unit -> UFix))
  (define (random! _)
    (random-below! (lisp UFix () cl:most-positive-fixnum)))

  (declare random-iter! (UFix -> (iter:Iterator UFix)))
  (define (random-iter! length)
    (map (fn (_) (random!))
         (iter:up-to length))))

(define-test tree-from-iter-equiv-to-manual-construction ()
  (let manual = (red-black/tree:insert-or-replace
                 (red-black/tree:insert-or-replace
                  (red-black/tree:insert-or-replace red-black/tree:Empty
                                                    5)
                  11)
                 2))
  (let iterated = (red-black/tree:collect! (iter:into-iter (the (List Integer)
                                                                (make-list 5 11 2)))))
  (is (== manual iterated))
  (is (== (hash manual) (hash iterated))))

(define-test tree-always-increasing-order ()
  (let increasing? = (fn (lst)
                       (== (list:sort lst) lst)))
  (let random-tree! = (fn (size)
                        (the (red-black/tree:Tree Ufix)
                             (iter:collect! (random-iter! size)))))
  (let increasing-list = (fn (tree)
                           (iter:collect! (red-black/tree:increasing-order tree))))
  (let decreasing-list = (fn (tree)
                           (iter:collect! (red-black/tree:decreasing-order tree))))
  (let tree-good? = (fn (tree)
                      (let increasing = (increasing-list tree))
                      (let decreasing = (decreasing-list tree))
                      (is (increasing? increasing))
                      (is (== (reverse decreasing) increasing))))
  (iter:for-each!
   (fn (length)
     (pipe length
           random-tree!
           tree-good?))
   (iter:up-to 128)))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro is-ok (check cl:&optional (message (cl:format cl:nil "~A returned Err" check)))
    `(is (result:ok? ,check)
         ,message)))

(define-test insertion-upholds-invariants ()
  (let insert-and-check-invariants =
    (fn (tre new-elt)
      (let new-tre = (red-black/tree:insert-or-replace tre new-elt))
      (is-ok (count-blacks-to-leaf new-tre))
      new-tre))
  (let collect-tree-checking-invariants =
    (fn (iter)
      (iter:fold! insert-and-check-invariants red-black/tree:Empty iter)))
  (let up-to-1024 =
    (collect-tree-checking-invariants (iter:up-to 1024)))
  (let down-from-1024 =
    (collect-tree-checking-invariants (iter:down-from 1024)))
  (is (== up-to-1024 down-from-1024))
  (is (== (hash up-to-1024) (hash down-from-1024)))
  (let range-1024 = (the (List Integer)
                         (iter:collect! (iter:up-to 1024))))
  (let range-shuffled = (lisp (List Integer) (range-1024)
                          (alexandria:shuffle range-1024)))
  (let shuffled =
    (collect-tree-checking-invariants (iter:into-iter range-shuffled)))
  (is (== up-to-1024 shuffled))
  (is (== (hash up-to-1024) (hash shuffled))))

(coalton-toplevel
  (declare tree-4 (red-black/tree:Tree Integer))
  (define tree-4 (red-black/tree:collect! (iter:up-to (the Integer 4))))

  (declare tree-1024 (red-black/tree:Tree Integer))
  (define tree-1024 (red-black/tree:collect! (iter:up-to (the Integer 1024))))

  (declare remove-and-check-invariants ((red-black/tree:Tree Integer) -> Integer -> (red-black/tree:Tree Integer)))
  (define (remove-and-check-invariants tre elt-to-remove)
    (match (red-black/tree:remove tre elt-to-remove)
      ((None) (error "Tried to remove non-present element in `remove-and-check-invariants'"))
      ((Some new-tre)
       (is-ok (count-blacks-to-leaf new-tre))
       new-tre)))

  (declare destroy-tree-checking-invariants ((red-black/tree:Tree Integer) -> (iter:Iterator Integer) -> Unit))
  (define (destroy-tree-checking-invariants start iter)
    (let should-be-empty = (iter:fold! remove-and-check-invariants start iter))
    (matches (red-black/tree:Empty) should-be-empty "Non-empty tree after removing all elements")))

(define-test removal-upholds-invariants-small-upward ()
  (destroy-tree-checking-invariants tree-4 (iter:up-to 4)))

(define-test removal-upholds-invariants-large-upward ()
  (destroy-tree-checking-invariants tree-1024 (iter:up-to 1024)))

(define-test removal-upholds-invariants-small-downward ()
  (destroy-tree-checking-invariants tree-4 (iter:down-from 4)))

(define-test removal-upholds-invariants-large-downward ()
  (destroy-tree-checking-invariants tree-1024 (iter:down-from 1024)))

(define-test removal-upholds-invariants-shuffled ()
  (let range-1024 = (the (List Integer)
                         (iter:collect-list! (iter:up-to 1024))))
  (let range-shuffled = (lisp (List Integer) (range-1024)
                          (alexandria:shuffle range-1024)))
  (destroy-tree-checking-invariants tree-1024 (iter:into-iter range-shuffled)))

(define-test detect-bad-tree ()
  (let red-with-red-child = (red-black/tree::Branch red-black/tree::Red
                                                    (red-black/tree::Branch red-black/tree::Red red-black/tree:Empty 0 red-black/tree:Empty)
                                                    1
                                                    red-black/tree:Empty))
  (matches (Err (RedWithRedLeftChild _)) (count-blacks-to-leaf red-with-red-child))

  (let unbalanced = (red-black/tree::Branch red-black/tree::Black
                                            (red-black/tree::Branch red-black/tree::Black red-black/tree:Empty 0 red-black/tree:Empty)
                                            1
                                            red-black/tree:Empty))
  (matches (Err (DifferentCountToBlack _ _ _ _)) (count-blacks-to-leaf unbalanced)))

(define-test map-from-iter-equiv-to-manual-construction ()
  (let manual = (the (red-black/map:Map Integer String)
                     (red-black/map:insert-or-replace
                      (red-black/map:insert-or-replace
                       (red-black/map:insert-or-replace red-black/map:empty 0 "zero")
                       11 "eleven")
                      5 "five")))
  (let iterated = (iter:collect! (iter:into-iter (the (List (Tuple Integer String))
                                                      (make-list (Tuple 0 "zero")
                                                                 (Tuple 11 "eleven")
                                                                 (Tuple 5 "five"))))))
  (is (== manual iterated))
  (is (== (hash manual) (hash iterated))))

(define-test map-non-equal ()
  (let map-012 = (the (red-black/map:Map Integer String)
                      (iter:collect! (iter:into-iter (make-list (Tuple 0 "zero")
                                                                (Tuple 1 "one")
                                                                (Tuple 2 "two"))))))
  (let map-01 = (iter:collect! (iter:into-iter (make-list (Tuple 0 "zero")
                                                          (Tuple 1 "one")))))
  (let map-wrong-names = (iter:collect! (iter:into-iter (make-list (Tuple 0 "one")
                                                                   (Tuple 1 "zero")))))

  (is (/= map-012 map-01))
  (is (/= (hash map-012) (hash map-01)))
  (is (/= map-01 map-wrong-names))
  (is (/= (hash map-01) (hash map-wrong-names)))
  (is (/= map-012 map-wrong-names))
  (is (/= (hash map-012) (hash map-wrong-names))))
