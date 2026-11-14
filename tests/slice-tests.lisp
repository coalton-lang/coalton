(in-package #:coalton-native-tests)

(coalton-toplevel
  ;; A second probe type, local to this file, so slice EQ short-circuiting
  ;; can be observed without depending on load order.
  (define-struct Slice-Eq-Probe
    (value Integer))

  (define slice-eq-probe-comparisons (cell:new (the Integer 0)))

  (define-instance (Eq Slice-Eq-Probe)
    (define (== a b)
      (progn
        (cell:increment! slice-eq-probe-comparisons)
        (match (Tuple a b)
          ((Tuple (Slice-Eq-Probe x) (Slice-Eq-Probe y)) (== x y)))))))

(define-test test-slice-basic ()
  (let ((v (the
            (Vector Integer)
            (into
             (the
              (List Integer)
              (make-list 1 2 3 4 5 6 7 8)))))

        (s (slice:new 0 3 v)))

    ;; Length should be correct
    (is (== 3 (slice:length s)))

    ;; Indexing should work
    (is (== 1 (slice:index-unsafe 0 s)))
    (is (== 2 (slice:index-unsafe 1 s)))
    (is (== 3 (slice:index-unsafe 2 s)))

    ;; Writes to the backing array should be visible in the slice
    (vector:set! 0 25 v)
    (is (== 25 (slice:index-unsafe 0 s)))

    ;; Writes to the slice should be visible in the backing array
    (slice:set! 0 26 s)
    (is (== 26 (vector:index-unsafe 0 v)))))

(define-test test-slice-offset ()
  (let ((v (the
            (Vector Integer)
            (into
             (the
              (List Integer)
              (make-list 1 2 3 4 5 6 7 8)))))

        (s (slice:new 2 3 v)))

    (is (== 3 (slice:index-unsafe 0 s)))
    (is (== 4 (slice:index-unsafe 1 s)))
    (is (== 5 (slice:index-unsafe 2 s)))))

(define-test test-slice-sliding-iteration ()
  (let ((v (the
            (Vector Integer)
            (into
             (the
              (List Integer)
              (make-list 1 2 3 4 5 6 7 8)))))

        (out (the (Vector (List Integer))
                  (vector:new))))

    (iter:for-each!
     (fn (s)
       (vector:push!
        (make-list
         (slice:index-unsafe 0 s)
         (slice:index-unsafe 1 s)
         (slice:index-unsafe 2 s))
        out)
       (values))
     (slice:iter-sliding 3 v))
    
    (is (== (into out)
            (make-list
             (make-list 1 2 3)
             (make-list 2 3 4)
             (make-list 3 4 5)
             (make-list 4 5 6)
             (make-list 5 6 7)
             (make-list 6 7 8))))))

(define-test test-slice-chunked-iteration ()
  (let ((v (the
            (Vector Integer)
            (into
             (the
              (List Integer)
              (make-list 1 2 3 4 5 6 7 8))))))

    (let out = (the (Vector (List Integer)) (vector:new)))

    (iter:for-each!
     (fn (s)
       (vector:push!
        (iter:collect! (iter:into-iter s))
        out)
       (values))
     (slice:iter-chunked 3 v))
     
    (is (== (into out)
            (make-list
             (make-list 1 2 3)
             (make-list 4 5 6)
             (make-list 7 8))))))

(define-test test-slice-chunked-exact-iteration ()
  (let ((v (the
            (Vector Integer)
            (into
             (the
              (List Integer)
              (make-list 1 2 3 4 5 6 7 8)))))

        (out (the (Vector (List Integer))
                  (vector:new))))

    (iter:for-each!
     (fn (s)
       (vector:push!
        (make-list
         (slice:index-unsafe 0 s)
         (slice:index-unsafe 1 s)
         (slice:index-unsafe 2 s))
        out)
       (values))
     (slice:iter-chunked-exact 3 v))
     
    (is (== (into out)
            (make-list
             (make-list 1 2 3)
             (make-list 4 5 6))))))

(define-test test-slice-eq ()
  (let ((v (the (Vector Integer)
                (into (the (List Integer) (make-list 1 2 3 4 5 6)))))
        (w (the (Vector Integer)
                (into (the (List Integer) (make-list 9 1 2 3))))))
    (let ((a (slice:new 0 3 v))
          (b (slice:new 0 3 v))
          (c (slice:new 3 3 v))
          (d (slice:new 0 2 v))
          (e (slice:new 1 3 w)))
      ;; Equality follows the displaced window, not the backing array.
      (is (== a b))
      (is (== a e))
      (is (/= a c))
      (is (/= a d))
      ;; EQ reads the live backing array rather than a copy.
      (vector:set! 1 99 w)
      (is (/= a e))
      (is (== 3 (slice:length a))))))

(define-test test-slice-eq-short-circuits ()
  (let v = (the (Vector Slice-Eq-Probe)
                (into (the (List Slice-Eq-Probe)
                           (make-list (Slice-Eq-Probe 1)
                                      (Slice-Eq-Probe 2)
                                      (Slice-Eq-Probe 3))))))
    (let a = (slice:new 0 3 v))
    ;; A length mismatch is rejected without comparing any element.
    (cell:write! slice-eq-probe-comparisons 0)
    (is (/= a (slice:new 0 2 v)))
    (is (== 0 (cell:read slice-eq-probe-comparisons)))
    ;; A mismatch at the first element stops the walk.
    (vector:set! 0 (Slice-Eq-Probe 9) v)
    (cell:write! slice-eq-probe-comparisons 0)
    (let other = (the (Vector Slice-Eq-Probe)
                      (into (the (List Slice-Eq-Probe)
                                 (make-list (Slice-Eq-Probe 1)
                                            (Slice-Eq-Probe 2)
                                            (Slice-Eq-Probe 3))))))
      (is (/= a (slice:new 0 3 other)))
      (is (== 1 (cell:read slice-eq-probe-comparisons))))

(define-test test-slice-collect-with-oversized-hint ()
  ;; A size hint is only an upper bound: TAKE! 3 over UP-TO 1 reports 3 but
  ;; yields one element. COLLECT! preallocates from that hint, so the backing
  ;; vector has spare capacity. The resulting Slice must expose only the
  ;; elements actually collected.
  (let s = (the (slice:Slice Integer)
                (iter:collect! (iter:take! 3 (iter:up-to 1)))))
    (is (== 1 (slice:length s)))
    (is (== (Some 0) (slice:index 0 s)))
    (is (== None (slice:index 1 s)))
    (is (== (make-list 0)
            (the (List Integer) (iter:collect! (iter:into-iter s)))))
    ;; An exact hint must still produce the full length.
    (let t = (the (slice:Slice Integer)
                  (iter:collect! (iter:take! 3 (iter:up-to 3)))))
      (is (== 3 (slice:length t)))
      (is (== (make-list 0 1 2)
              (the (List Integer) (iter:collect! (iter:into-iter t)))))
    ;; An empty iterator with an oversized hint yields an empty Slice.
    (let e = (the (slice:Slice Integer)
                  (iter:collect! (iter:take! 0 (iter:up-to 1)))))
      (is (== 0 (slice:length e)))
      (is (== None (slice:index 0 e))))
