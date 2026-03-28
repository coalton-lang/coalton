(in-package #:coalton-native-tests)

(define-test ordtree-basic-test ()
  "OrdTree basic operations"
  (let a = (the (ordtree:OrdTree Integer) ordtree:empty))

  (is (ordtree:empty? a))
  (is (== (ordtree:lookup a 1) None))

  (let b = (ordtree:insert a 1))
  (is (== (ordtree:lookup b 1) (Some 1)))

  ;; This data sequence is long enough to cover the branches in
  ;; rebalancing routines.
  (let data = (map (fn (k) (bits:and #xffffffff (* k 2654435761)))
                   (range 0 4096)))

  (let c = (fold ordtree:insert a data))
  (is (some? (fold (fn (m k) (>>= (ordtree:lookup c k) Some))
                   (Some 1) data)))
  (is (ordtree::consistent? c))

  (let d = (fold (fn (m k)
                   (match (ordtree:lookup m k)
                     ((None) (lisp (-> :a) (k)
                               (cl:error "Key ~a dissapeared unexpectedly" k)))
                     ((Some _) Unit))
                   (let ((m1 (ordtree:remove m k)))
                     (match (ordtree:lookup m1 k)
                       ((None) Unit)
                       ((Some _) (lisp (-> :a) (k)
                                   (cl:error "Key ~a failed to be removed" k))))
                     (if (ordtree::consistent? m1)
                         Unit
                         (lisp (-> :a) (k)
                           (cl:error "Key ~a removal caused inconsistency" k)))
                     m1))
                 c data))
  (is (ordtree:empty? d))

  (let (values bb prev) = (ordtree:update a 1 (fn (z) (values (Some 1) z))))
  (is (== (ordtree:lookup bb 1) (Some 1)))
  (is (== prev None))
  (let (values bb2 prev2) = (ordtree:update bb 1 (fn (z) (values None z))))
  (is (ordtree:empty? bb2))
  (is (== prev2 (Some 1)))

  (let cc = (fold (fn (m k)
                     (let (values mm _) =
                       (ordtree:update m k (fn (_) (values (Some k) False))))
                    mm)
                  a data))
  (is (some? (fold (fn (m k) (>>= (ordtree:lookup cc k) Some))
                   (Some 1) data)))
  (is (ordtree::consistent? cc))

  (let dd = (fold (fn (m k)
                    (let (values m1 _) = (ordtree:update m k (fn (z) (values None z))))
                    (is (ordtree::consistent? m1))
                    m1)
                  cc data))
  (is (ordtree::empty? dd))
  )

(coalton-toplevel
  (define-type OrdTreeTestEntry
    (OrdTreeTestEntry Integer String))
  (define (ordtree-test-entry-key (OrdTreeTestEntry k _)) k)
  (define-instance (Eq OrdTreeTestEntry)
    (define (== a b)
      (== (ordtree-test-entry-key a) (ordtree-test-entry-key b))))
  (define-instance (Ord OrdTreeTestEntry)
    (define (<=> a b)
      (<=> (ordtree-test-entry-key a) (ordtree-test-entry-key b))))
  )

(define-test ordtree-update-test ()
  "OrdTree update operations with more involved update function"
  (let a = (the (ordtree:OrdTree OrdTreeTestEntry)
                (fold ordtree:insert ordtree:empty
                      (make-list (OrdTreeTestEntry 1 "one")
                                 (OrdTreeTestEntry 2 "two")
                                 (OrdTreeTestEntry 3 "three")
                                 (OrdTreeTestEntry 4 "four")))))

  ;; lookup - hit
  (let (values b v) =
    (ordtree:update a (OrdTreeTestEntry 1 "")
                    (fn (e)
                      (match e
                        ((None) (values None "nope"))
                        ((Some (OrdTreeTestEntry _ v)) (values e v))))))
  (is (== v "one"))
  (is (== (ordtree:lookup b (OrdTreeTestEntry 1 ""))
          (Some (OrdTreeTestEntry 1 "one"))))

  ;; lookup - miss
  (let (values _ v) =
    (ordtree:update a (OrdTreeTestEntry 0 "")
                    (fn (e)
                      (match e
                        ((None) (values None "nope"))
                        ((Some (OrdTreeTestEntry _ v)) (values e v))))))
  (is (== v "nope"))

  ;; delete - hit
  (let (values b v) =
    (ordtree:update a (OrdTreeTestEntry 2 "")
                    (fn (e)
                      (match e
                        ((None) (values None "nope"))
                        ((Some (OrdTreeTestEntry _ v)) (values None v))))))
  (is (== v "two"))
  (is (== (ordtree:lookup b (OrdTreeTestEntry 2 "")) None))

  ;; delete - miss
  (let (values _ v) =
    (ordtree:update a (OrdTreeTestEntry 0 "")
                    (fn (e)
                      (match e
                        ((None) (values None "nope"))
                        ((Some _) (values None "huh?"))))))
  (is (== v "nope"))

  ;; insert
  (let (values b v) =
    (ordtree:update a (OrdTreeTestEntry 0 "")
                    (fn (e)
                      (match e
                        ((None)  (values (Some (OrdTreeTestEntry 0 "zero"))
                                         "yeah"))
                        ((Some _) (values None "huh?"))))))
  (is (== v "yeah"))
  (is (== (ordtree:lookup b (OrdTreeTestEntry 0 ""))
          (Some (OrdTreeTestEntry 0 "zero"))))

  ;; replace
  (let (values b v) =
    (ordtree:update a (OrdTreeTestEntry 3 "")
                    (fn (e)
                      (match e
                        ((None)  (values None "huh?"))
                        ((Some (OrdTreeTestEntry _ v))
                         (values (Some (OrdTreeTestEntry 3 "san")) v))))))
  (is (== v "three"))
  (is (== (ordtree:lookup b (OrdTreeTestEntry 3 ""))
          (Some (OrdTreeTestEntry 3 "san"))))
  )

(coalton-toplevel
  (define (list->ordtree lis)
    (the (ordtree:OrdTree :a) (iter:collect! (iter:into-iter lis))))
  (define (ordtree->list tre)
    (iter:collect! (ordtree:increasing-order tre))))

(define-test ordtree-set-test ()
  (let a = (the (ordtree:OrdTree Integer)
                (list->ordtree (make-list 1 4 5 9 10 14))))
  (let b = (the (ordtree:OrdTree Integer)
                (list->ordtree (make-list 2 3 5 7 9 13))))

  (is (== (ordtree->list (ordtree:union a b))
          (make-list 1 2 3 4 5 7 9 10 13 14)))
  (is (== (ordtree->list (ordtree:intersection a b))
          (make-list 5 9)))
  (is (== (ordtree->list (ordtree:difference a b))
          (make-list 1 4 10 14)))
  (is (== (ordtree->list (ordtree:difference b a))
          (make-list 2 3 7 13)))
  (is (== (ordtree->list (ordtree:xor b a))
          (make-list 1 2 3 4 7 10 13 14)))
  )

(define-test ordtree-instance-test ()
  (let a = (the (ordtree:OrdTree Integer) (list->ordtree (range 0 10))))

  (is (== (foldr Cons Nil a)
          (range 0 10)))
  (is (== (reverse (fold (flip Cons) Nil a))
          (range 0 10)))

  (is (== (the (ordtree:OrdTree Integer) (list->ordtree (reverse (range 0 10))))
          a))

  (is (== (ordtree->list
           (ordtree:transform-elements (fn (x) (* x 2))
                                       (list->ordtree (range 0 10))))
          (make-list 0 2 4 6 8 10 12 14 16 18 20)))
  )

(define-test ordtree-neighbors-test ()
  (is (== (ordtree:max-element (list->ordtree (make-list 1 2 3 4 5 6 7 8 9)))
          (Some 9)))
  (is (== (ordtree:min-element (list->ordtree (make-list 1 2 3 4 5 6 7 8 9)))
          (Some 1)))
  (is (== (ordtree:max-element (list->ordtree (make-list 1)))
          (Some 1)))
  (is (== (ordtree:min-element (list->ordtree (make-list 1)))
          (Some 1)))
  (is (== (ordtree:max-element (the (ordtree:OrdTree Integer) ordtree:empty))
          None))
  (is (== (ordtree:min-element (the (ordtree:OrdTree Integer) ordtree:empty))
          None))

  (let ((t (list->ordtree (the (List Integer) (make-list 1 3 5 7 9)))))

    (is (== (ordtree:lookup-neighbors t 0)
            (Tuple3 None None (Some 1))))
    (is (== (ordtree:lookup-neighbors t 1)
            (Tuple3 None (Some 1) (Some 3))))
    (is (== (ordtree:lookup-neighbors t 2)
            (Tuple3 (Some 1) None (Some 3))))
    (is (== (ordtree:lookup-neighbors t 3)
            (Tuple3 (Some 1) (Some 3) (Some 5))))
    (is (== (ordtree:lookup-neighbors t 4)
            (Tuple3 (Some 3) None (Some 5))))
    (is (== (ordtree:lookup-neighbors t 5)
            (Tuple3 (Some 3) (Some 5) (Some 7))))
    (is (== (ordtree:lookup-neighbors t 6)
            (Tuple3 (Some 5) None (Some 7))))
    (is (== (ordtree:lookup-neighbors t 7)
            (Tuple3 (Some 5) (Some 7) (Some 9))))
    (is (== (ordtree:lookup-neighbors t 8)
            (Tuple3 (Some 7) None (Some 9))))
    (is (== (ordtree:lookup-neighbors t 9)
            (Tuple3 (Some 7) (Some 9) None)))
    (is (== (ordtree:lookup-neighbors t 10)
            (Tuple3 (Some 9) None None))))

  ;; edge cases
  (is (== (ordtree:lookup-neighbors ordtree:Empty 1)
          (Tuple3 None None None)))
  (is (== (ordtree:lookup-neighbors (list->ordtree (make-list 1)) 1)
          (Tuple3 None (Some 1) None)))
  )
