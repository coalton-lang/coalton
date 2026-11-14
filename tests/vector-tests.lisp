(cl:in-package #:coalton-native-tests)

(coalton-toplevel
  ;; An element type whose EQ dictionary records every comparison, so the
  ;; tests below can observe that EQ short-circuits.
  (define-struct Eq-Probe
    (value Integer))

  (define eq-probe-comparisons (cell:new (the Integer 0)))

  (define-instance (Eq Eq-Probe)
    (define (== a b)
      (progn
        (cell:increment! eq-probe-comparisons)
        (match (Tuple a b)
          ((Tuple (Eq-Probe x) (Eq-Probe y)) (== x y)))))))

(define-test test-vector-constructor-equivalencies ()
  (let vec = (vector:with-capacity 10))
  (iter:for-each! (fn (x)
                    (vector:push! x vec)
                    (values))
                  (iter:up-to 10))
  (is (== (vector:make 0 1 2 3 4 5 6 7 8 9)
          vec))
  (is (== (iter:collect! (iter:up-to 10))
          vec)))

(define-test test-vector-initial-element ()
  (is (== (vector:make "x" "x" "x") (vector:with-initial-element 3 "x")))
  ;; The one-pass constructor must preserve the invariants of the old
  ;; WITH-CAPACITY + EXTEND! implementation: exact capacity, a fill pointer,
  ;; and the ability to grow in place.
  (let v = (vector:with-initial-element 4 "x"))
  (is (== 4 (vector:length v)))
  (is (== 4 (vector:capacity v)))
  (is (== (Some "x") (vector:head v)))
  (vector:push! "y" v)
  (is (== (vector:make "x" "x" "x" "x" "y") v))
  (is (vector:empty? (vector:with-initial-element 0 "x"))))

(define-test test-vector-append-empty ()
  (let empty = (the (Vector Integer) (vector:new)))
  (let v = (vector:make 1 2 3))
  (is (== (vector:make 1 2 3) (vector:append empty v)))
  (is (== (vector:make 1 2 3) (vector:append v empty)))
  (is (vector:empty? (vector:append empty empty)))
  ;; APPEND must allocate one fresh, growable vector rather than a view of
  ;; either input.
  (let out = (vector:append v v))
  (is (== (vector:make 1 2 3 1 2 3) out))
  (is (== 6 (vector:capacity out)))
  (vector:push! 4 out)
  (is (== (vector:make 1 2 3) v))
  (is (== (vector:make 1 2 3 1 2 3 4) out)))

(define-test test-vector-extend-preserves-spare-capacity ()
  ;; Regression: EXTEND! used to resize to LENGTH + (SIZE - REMAINING),
  ;; shrinking a vector that already had spare capacity and forcing every
  ;; following PUSH! to reallocate.
  (let v = (the (Vector Integer) (vector:with-capacity 1000)))
  (vector:push! 0 v)
  (is (== 1000 (vector:capacity v)))
  (vector:extend! v (iter:take! 1000 (iter:count-forever)))
  (is (== 1001 (vector:length v)))
  (is (== 1001 (vector:capacity v)))
  (is (== (Some 0) (vector:head v)))
  (is (== (Some 999) (vector:last v))))

(define-test test-vector-extend-uses-size-hint ()
  ;; EXTEND! should size its single resize from the iterator's hint even
  ;; when that iterator yields no elements at all.
  (let v = (the (Vector Integer) (vector:with-capacity 10)))
  (vector:push! 0 v)
  (vector:extend! v (iter:with-size (fn () (the (Optional Integer) None)) 100))
  (is (== 1 (vector:length v)))
  (is (== 101 (vector:capacity v))))

(define-test test-vector-eq-basic ()
  (is (== (vector:make 1 2 3) (vector:make 1 2 3)))
  (is (/= (vector:make 1 2 3) (vector:make 1 2 4)))
  (is (/= (vector:make 1 2) (vector:make 1 2 3)))
  (is (== (the (Vector Integer) (vector:new))
          (the (Vector Integer) (vector:new))))
  ;; Nested vectors exercise dispatch through the element EQ dictionary.
  (is (== (vector:make (vector:make 1 2) (vector:make 3))
          (vector:make (vector:make 1 2) (vector:make 3))))
  (is (/= (vector:make (vector:make 1 2) (vector:make 3))
          (vector:make (vector:make 1 2) (vector:make 4)))))

(define-test test-vector-eq-short-circuits ()
  (let a = (vector:make (Eq-Probe 1) (Eq-Probe 2) (Eq-Probe 3)))
  (cell:write! eq-probe-comparisons 0)
  (is (/= a (vector:make (Eq-Probe 9) (Eq-Probe 2) (Eq-Probe 3))))
  ;; EQ must stop at the first unequal pair rather than walking all three.
  (is (== 1 (cell:read eq-probe-comparisons)))
  (cell:write! eq-probe-comparisons 0)
  (is (== a (vector:make (Eq-Probe 1) (Eq-Probe 2) (Eq-Probe 3))))
  (is (== 3 (cell:read eq-probe-comparisons)))
  (cell:write! eq-probe-comparisons 0)
  ;; Differing lengths are rejected before any element is compared.
  (is (/= a (vector:make (Eq-Probe 1))))
  (is (== 0 (cell:read eq-probe-comparisons))))

(define-test test-vector-basic-operations ()
  (let vec0 = (vector:new))
  (let vec1 = (vector:make 10 11 12 13 14))
  (iter:for-each! (fn (x)
                    (vector:push! x vec0)
                    (values))
                  (iter:up-to 5))
  (is (== (vector:make 0 1 2 3 4 10 11 12 13 14)
          (vector:append vec0 vec1)))
  (is (== (vector:make 4 3 2 1 0)
          (vector:reverse vec0)))
  (is (== (vector:make 0 1 2 3 4)
          vec0))                        ;ensure vec0 is intact
  (is (== (vector:make 4 3 2 1 0)
          (vector:reverse! vec0)))
  (vector:clear! vec0)
  (is (== (vector:new) vec0))
  (is (== True (vector:empty? vec0)))
  (is (== (Some 2) (vector:find-elem 12 vec1)))
  (is (== None (vector:find-elem 0 vec1)))
  (is (== None (vector:head vec0)))
  (is (== (Some 10) (vector:head vec1)))
  (is (== None (vector:index 1 vec0)))
  (is (== (Some 11) (vector:index 1 vec1)))
  (is (== None (vector:last vec0)))
  (is (== (Some 14) (vector:last vec1)))
  (is (== (Some 14) (vector:pop! vec1)))
  (is (== (Some 13) (vector:last vec1)))
  (vector:push! 9 vec1)
  (is (== (Some 9) (vector:last vec1)))
  (let vec2 = (fold vector:append
                    (vector:singleton 99)
                    (make-list vec1 (vector:make 2 7))))
  (is (== (vector:make 99 10 11 12 13 9 2 7) vec2))
  (vector:sort! vec2)
  (is (== (vector:make 2 7 9 10 11 12 13 99) vec2))
  (vector:sort-by! > vec2)
  (is (== (vector:make 99 13 12 11 10 9 7 2) vec2))
  (vector:set! 3 88 vec2)
  (is (== (vector:make 99 13 12 88 10 9 7 2) vec2))
  (vector:set-capacity! 5 vec2)
  (is (== (vector:make 99 13 12 88 10) vec2))
  (is (== (Some 12) (vector:swap-remove! 2 vec2)))
  (is (== (vector:make 99 13 10 88) vec2))
  (is (== None (vector:swap-remove! 10 vec2)))
  (is (== (vector:make 99 13 10 88) vec2))
  (vector:extend! vec2 (iter:into-iter (make-list 5 6 7 8)))
  (is (== (vector:make 99 13 10 88 5 6 7 8) vec2))

  (let vec3 = (vector:singleton "a"))
  (is (== (Some "a") (vector:swap-remove! 0 vec3)))
  (is (== True (vector:empty? vec3)))
  (is (== None (vector:swap-remove! 0 vec3)))

  (is (== False (vector:singleton? vec2)))
  (is (== False (vector:singleton? (vector:new))))
  (is (== True (vector:singleton? (vector:make 1)))))

(define-test test-vector-resect ()
  (let v = (vector:make 0 1 2 3 4 5 6))
  (vector:resect! v 2 4)
  (is (== v (vector:make 0 1 4 5 6)))

  (let v = (vector:make 0 1 2 3 4 5 6))
  (vector:resect! v 0 400)
  (is (== v (vector:make))))

(define-test test-vector-subseq ()
  (let v = (vector:make 0 1 2 3 4 5 6))
  (is (== (vector:make 2 3) (vector:subseq v 2 4)))
  (is (== v (vector:subseq v 0 1000)))
  (is (== (vector:make) (vector:subseq v 10 12)))
  ;; The result is an ordinary vector, which can grow.
  (let s = (vector:subseq v 2 4))
  (vector:push! 9 s)
  (is (== (vector:make 2 3 9) s))
  (let c = (vector:copy (vector:subseq v 5 7)))
  (vector:push! 7 c)
  (is (== (vector:make 5 6 7) c)))

(define-test test-vector-resect-beyond-length ()
  (let v = (vector:make 0 1 2))
  (vector:resect! v 10 12)
  (is (== (vector:make 0 1 2) v))
  (vector:resect! v 1 10)
  (is (== (vector:make 0) v)))

(define-test test-vector-show ()
  (is (== "#<Vector [1 2 3]>"
          (show-as-string (vector:make 1 2 3)))))
