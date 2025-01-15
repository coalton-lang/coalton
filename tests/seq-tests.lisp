(in-package #:coalton-native-tests)

(define-test seq-push-and-pop ()
  (let ((seq (the (seq:Seq String) (seq:make "a" "b" "c"))))
    (is (== (Some "a") (seq:get seq 0)))
    (is (== (Some "b") (seq:get seq 1)))
    (is (== (Some "c") (seq:get seq 2)))
    (is (coalton-library/optional:none? (seq:get seq 3)))
    (match (seq:pop seq)
      ((Some (Tuple x seq2))
       (is (== x "c"))
       (is (== (Some "b") (seq:get seq2 1)))
       Unit)
      (_
       (unreachable)))
    (is (seq:empty?
         (pipe seq
               seq:pop
               defaulting-unwrap
               snd

               seq:pop
               defaulting-unwrap
               snd

               seq:pop
               defaulting-unwrap
               snd)))))

(coalton-toplevel
  (declare legible-seq (UFix -> seq:Seq String))
  (define (legible-seq n)
    (iter:collect!
     (map fmt:eng
          (iter:up-to n)))))

(define-test seq-push-and-pop-implementation ()
  (let seq = (legible-seq 33))
  (let popped = (match (seq:pop seq) ((Some (Tuple _ popped)) popped) (_  (unreachable))))
  ;; ensure that tree restructuring works
  (is (== (seq::height seq) (+ 1 (seq::height popped))))

  (let seq2 = (seq:push popped "thirty-two"))
  ;; just warming up
  (is (== seq seq2))
  ;; now test that popped and popped2 are identical - i.e. memory is shared between them
  (let popped2 = (match (seq:pop seq2) ((Some (Tuple _ popped)) popped) (_  (unreachable))))
  (is (lisp Boolean (popped popped2) (cl:eq popped popped2))))


(define-test seq-concat ()
  (let ((seq
          (legible-seq 1000))
        (seqseq
          (seq:conc seq seq)))
    
    (is (== 2000 (seq:size seqseq)))
    (is (== 1000 (seq:size (seq:conc (seq:new) seq))))
    (is (== 1000 (seq:size (seq:conc seq (seq:new)))))
    (is (== 2001 (seq:size (seq:conc (seq:push seq "negative one")
                                       seq))))
    (is (== (Some "zero")
            (seq:get seqseq 1000)))
    (is (== (Some "one hundred twenty-seven")
            (seq:get seqseq 1127)))))

(define-test seq-get-and-put ()
  (let ((seq
          (iter:collect!
           (iter:up-to 30000)))
        (seq2
          (defaulting-unwrap
           (seq:put seq 11234 0))))
    (is (== (Some 0) (seq:get seq2 11234)))))


(coalton-toplevel 
  (define (branching-valid? seq)
    "Returns T if the branching invariants are respected.  Namely, that
every non-leaf node in the tree other than nodes on the right-most
edge all have between MIN-BRANCHING and MAX-BRANCHING subnodes."
    (let ((satisfied?
            (fn (len)
              (and (<= seq::min-branching len)
                   (<= len seq::max-branching))))
          (valid?
            (fn (right-most-edge? node)
              (match node
                ((seq::LeafArray leaves)
                 (or right-most-edge? (satisfied? (vector:length leaves))))

                ((seq::RelaxedNode _ _ _ subs)
                 (and (or right-most-edge? (satisfied? (vector:length subs)))
                      (iter:every!
                       (valid? False)
                       (map (flip vector:index-unsafe subs)
                            (iter:range-increasing 1 0 (- (vector:length subs) 1))))
                      (valid? True (vector:last-unsafe subs))))))))
      (valid? True seq))))

(define-test seq-branch-invariants ()
  "Test that branch invariants hold after mangling up some sequences."
  (let ((seq
          (legible-seq 2000))
        (pop-n (fn (n s)
                 (if (== 0 n) (pure s)
                     (do
                      ((Tuple _ s2) <- (seq:pop s))
                      (pop-n (- n 1) s)))))
        (seq2
          (defaulting-unwrap (pop-n 600 seq)))
        (seq3
          (seq:conc seq2 seq2))
        (seq4
          (seq:conc (seq:push seq2 "pushed") seq)))

    (is (branching-valid? seq))
    (is (branching-valid? seq2))
    (is (branching-valid? seq3))
    (is (branching-valid? seq4))))

(define-test seq-eq ()
  (let ((seq1
          (legible-seq 1000))
        (seq2
          (legible-seq 1000))
        (seq3
          (legible-seq 10))
        (empty
          (the (seq:Seq String) (seq:new))))
    (is (== empty (seq:new)))
    (is (== seq1 seq1))
    (is (== seq1 seq2))
    (is (not (== seq1 seq3)))))

(define-test seq-make ()
  (let short-seq = (the (seq:Seq Integer)
                        (seq:make 1 2 3)))
  (let longer-seq = (the (seq:Seq Integer)
                         (seq:make 1 (+ 1 1) 3 4 5 6 7 8 9 10 11 12 13
                                   14 15 16 17 18 19 20 21 22 23 24 25
                                   26 27 28 29 30 31 32 33 34 35 36 37
                                   38 39 40 41 42 43 44 45 46 47 48 49
                                   50 51 52 53 54 55 56 57 58 59 60 61
                                   62 62 64 65 66 67 (+ 57 10))))
  (is (== 3 (seq:size short-seq)))
  (is (== 68 (seq:size longer-seq))))
