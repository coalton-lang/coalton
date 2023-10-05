(in-package #:coalton-native-tests)

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
       Unit)
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
       Unit)
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
       Unit)
     (slice:iter-chunked-exact 3 v))
     
    (is (== (into out)
            (make-list
             (make-list 1 2 3)
             (make-list 4 5 6))))))
