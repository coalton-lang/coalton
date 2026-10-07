(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(define-test seq-fold-order ()
  (is (== (fold + 7 (the (seq:Seq Integer) (seq:new))) 7))
  (is (== (foldr + 7 (the (seq:Seq Integer) (seq:new))) 7))
  ;; Cross leaf and multi-level branch boundaries, including concatenated trees.
  (iter:for-each!
    (fn (count)
      (let xs = (iter:collect! (iter:up-to (the Integer count))))
      (let left = (the (seq:Seq Integer) (into (the (List Integer) xs))))
      (let joined = (seq:conc left (seq:make count (+ count 1))))
      (let expected = (<> xs (make-list count (+ count 1))))
      (is (== (fold (fn (acc x) (Cons x acc)) Nil joined) (list:reverse expected)))
      (is (== (foldr Cons Nil joined) expected)))
    (iter:into-iter (make-list 1 32 33 1100))))

(define-test library-conversions ()
  (is (== (the Integer (into (the Integer 42))) 42))
  (is (== (the UFix (into (the UFix 42))) 42))
  (is (== (the String (into "hello")) "hello"))
  (let path = (the file:Pathname (into "conversion-test")))
  (is (== (the file:Pathname (into path)) path))
  (is (== (coalton/tuple:swap (the (Tuple Integer Integer) (Tuple 1 2))) (Tuple 2 1)))
  (is (== (the (Tuple Integer Integer) (into (the (Tuple Integer Integer) (Tuple 1 2))))
          (Tuple 1 2)))
  (is (== (the (seq:Seq Integer) (into (the (List Integer) (make-list 1 2)))) (seq:make 1 2)))
  (is (== (the (seq:Seq Integer) (into (the (vector:Vector Integer) (vector:make 1 2)))) (seq:make 1 2)))
  (is (== (the (seq:Seq Integer) (into (Some (the Integer 1)))) (seq:make 1)))
  (is (seq:empty? (the (seq:Seq Integer) (into (the (Optional Integer) None)))))
  (let z = (the (math:Complex creal:CReal) (into (math:Complex (the Integer 1) 2))))
  (is (== (math:real-part z) 1))
  (is (== (math:imag-part z) 2)))

(define-test explicit-cell-contents-conversion ()
  (let c = (cell:new (the Integer 42)))
  (let text = (the String (into (cell:read c))))
  (cell:write! c 7)
  (is (== text "42"))
  (is (== (the String (into (cell:read c))) "7"))
  (is (== (cell:read (cell:new "hello")) "hello")))

(coalton-toplevel
  (define-type (ConversionFoldable :a)
    (ConversionFoldable :a :a))

  (define-instance (Foldable ConversionFoldable)
    (define (fold f init (ConversionFoldable x y))
      (f (f init x) y))
    (define (foldr f init (ConversionFoldable x y))
      (f x (f y init))))

  (define-type ConversionScalar
    (ConversionScalar Integer))

  (define-instance (Eq ConversionScalar)
    (define (== (ConversionScalar x) (ConversionScalar y)) (== x y)))

  (define-instance (Num ConversionScalar)
    (define (+ (ConversionScalar x) (ConversionScalar y)) (ConversionScalar (+ x y)))
    (define (- (ConversionScalar x) (ConversionScalar y)) (ConversionScalar (- x y)))
    (define (* (ConversionScalar x) (ConversionScalar y)) (ConversionScalar (* x y)))
    (define fromInt ConversionScalar))

  (define-instance (math:ComplexComponent ConversionScalar)
    (define (math:complex re im) (coalton/math/complex::%Complex re im))
    (define (math:real-part z)
      (match z ((coalton/math/complex::%Complex re _) re)))
    (define (math:imag-part z)
      (match z ((coalton/math/complex::%Complex _ im) im))))

  (define-instance (Into ConversionScalar creal:CReal)
    (define (into (ConversionScalar x)) (into x)))

  (define-instance (Into ConversionScalar String)
    (define (into (ConversionScalar x)) (into x)))

  (declare conversion-same (Into :a :a => :a -> :a))
  (define (conversion-same x) (into x))

  (declare conversion-same-via-iso (Iso :a :a => :a -> :a))
  (define (conversion-same-via-iso x) (into x))

  (declare conversion-to-seq (Into (:f :a) (seq:Seq :a) => :f :a -> seq:Seq :a))
  (define (conversion-to-seq xs) (into xs)))

(define-test generic-library-conversions ()
  ;; User-defined Foldable and scalar instances automatically participate in
  ;; the library's generic conversions, without container-specific instances.
  (is (== (the (seq:Seq Integer) (into (ConversionFoldable 3 7))) (seq:make 3 7)))
  (is (== (conversion-to-seq (ConversionFoldable 3 7)) (seq:make 3 7)))
  (is (== (conversion-same (ConversionScalar 3)) (ConversionScalar 3)))
  (is (== (conversion-same-via-iso (ConversionScalar 7)) (ConversionScalar 7)))
  (is (== (the String (into (ConversionScalar 3))) "3"))
  (let z = (the (math:Complex creal:CReal)
               (into (math:complex (ConversionScalar 3) (ConversionScalar 7)))))
  (is (== (math:real-part z) 3))
  (is (== (math:imag-part z) 7)))

(define-test library-conversion-intersections ()
  ;; Identity conversions must not rebuild containers or complex numbers.
  (let xs = (seq:make (ConversionScalar 3) (ConversionScalar 7)))
  (let ys = (conversion-same xs))
  (is (lisp (-> Boolean) (xs ys) (cl:eq xs ys)))
  (let z = (the (math:Complex creal:CReal) (math:Complex 3 7)))
  (let w = (conversion-same-via-iso z))
  (is (lisp (-> Boolean) (z w) (cl:eq z w)))
  (let c = (cell:new (ConversionScalar 3)))
  (let d = (conversion-same c))
  (is (lisp (-> Boolean) (c d) (cl:eq c d)))
  (is (== (cell:read c) (ConversionScalar 3))))

(define-test seq-push-and-pop ()
  (let ((seq (the (seq:Seq String) (seq:make "a" "b" "c"))))
    (is (== (Some "a") (seq:get seq 0)))
    (is (== (Some "b") (seq:get seq 1)))
    (is (== (Some "c") (seq:get seq 2)))
    (is (coalton/optional:none? (seq:get seq 3)))
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
     (map (fn (i) (lisp (-> String) (i) (cl:format cl:nil "~r" i)))
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
  (is (lisp (-> Boolean) (popped popped2) (cl:eq popped popped2))))


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
            (seq:get seqseq 1127))))

  ;; Regression: this used to fail near 32^2 boundary while rebalancing.
  (let ((seq
          (legible-seq 1025))
        (seqseq
          (seq:conc (legible-seq 1025) (legible-seq 1025))))
    (is (== 2050 (seq:size seqseq)))
    (is (== (Some "zero")
            (seq:get seqseq 1025)))
    (is (== (Some "one thousand twenty-four")
            (seq:get seqseq 2049)))
    (is (== (Some "one thousand twenty-four")
            (seq:get seq 1024)))))

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
                       (fn (subnode) (valid? False subnode))
                       (map (fn (i)
                              (vector:index-unsafe i subs))
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

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro make-large-seq (n)
    `(seq:make ,@(cl:loop :for x :from 0 :below n :collect x))))

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

  (let longest-seq = (make-large-seq 2048))

  (is (== 3 (seq:size short-seq)))
  (is (== 68 (seq:size longer-seq)))
  (is (== 2048 (seq:size longest-seq)))
  (is (== (Some 1346) (seq:get longest-seq 1346))))

(define-test seq-make-into ()
  (let my-seq = (seq:make "Hello, world!" (into 3)))
  (is (== (Some "Hello, world!") (seq:get my-seq 0)))
  (is (== (Some "3") (seq:get my-seq 1))))

(define-test seq-show ()
  (is (== "#<Seq [1 2 3]>"
          (show-as-string (the (seq:Seq Integer)
                               (seq:make 1 2 3))))))

;;; Folding over ranges, and mapping in chunks

(coalton-toplevel
  (declare seq-test-of-size (UFix -> seq:Seq Integer))
  (define (seq-test-of-size n)
    (into (the (List Integer) (iter:collect! (iter:up-to (the Integer (into n)))))))

  (declare seq-test-shapes (Void -> List (seq:Seq Integer)))
  (define (seq-test-shapes)
    "Seqs of various sizes, including concatenations and seqs shortened by
POP, whose trees are irregular."
    (let ((popped (rec % ((s (seq-test-of-size 2000)) (k (the UFix 40)))
                    (if (== k 0)
                        s
                        (match (seq:pop s)
                          ((Some (Tuple _ rest)) (% rest (- k 1)))
                          ((None) s))))))
      (<> (map seq-test-of-size (make-list 0 1 31 32 33 1023 1024 1025 33000))
          (make-list
           (fold seq:conc (seq:new) (map seq-test-of-size (make-list 1100 47 30000 5 2048)))
           (fold seq:conc (seq:new) (map seq-test-of-size (range 1 60)))
           popped))))

  (declare seq-test-backwards-by-7 (UFix * UFix * (UFix * UFix -> Void) -> Void))
  (define (seq-test-backwards-by-7 start end chunk)
    "Call CHUNK on chunks of at most 7 indices covering START to END, from
the last chunk to the first."
    (rec % ((hi end))
      (when (> hi start)
        (let ((lo (if (> (- hi start) 7) (- hi 7) start)))
          (chunk lo hi)
          (% lo))))))

(define-test seq-fold-range ()
  (iter:for-each!
   (fn (s)
     (let n = (seq:size s))
     (let elements = (the (List Integer) (into s)))
     (iter:for-each!
      (fn ((Tuple start end))
        (let stop = (min end n))
        (let expected = (if (< start stop)
                            (list:take (- stop start) (list:drop start elements))
                            Nil))
        (is (== expected
                (list:reverse (seq:fold-range (fn (acc x) (Cons x acc)) Nil s start end)))))
      (iter:into-iter
       (make-list (Tuple 0 n) (Tuple 0 0) (Tuple n n) (Tuple 1 n) (Tuple 0 (+ n 5))
                  (Tuple (+ n 1) (+ n 9)) (Tuple 31 33) (Tuple 32 64) (Tuple 1000 1100)
                  (Tuple (math:div n 3) (+ 1 (math:div (* 2 n) 3)))))))
   (iter:into-iter (seq-test-shapes))))

(define-test seq-map-with ()
  (iter:for-each!
   (fn (s)
     (let n = (seq:size s))
     (let expected = (map (fn (x) (* 2 x)) s))
     (iter:for-each!
      (fn (for-chunks)
        (let mapped = (seq:map-with for-chunks (fn (x) (* 2 x)) s))
        (is (== expected mapped))
        (is (== n (seq:size mapped)))
        ;; The result supports the operations of any Seq.
        (when (> n 0)
          (is (== (seq:get s (- n 1)) (map (fn (x) (math:div x 2)) (seq:get mapped (- n 1))))))
        (let pushed = (seq:push mapped 7))
        (is (== (+ n 1) (seq:size pushed)))
        (is (== (Some 7) (seq:get pushed n)))
        (is (== (<> (the (List Integer) (into expected)) (make-list 7)) (into pushed))))
      (iter:into-iter
       (make-list (fn (start end chunk) (chunk start end))
                  seq-test-backwards-by-7)))
     ;; F is only called within the calls on chunks.
     (let inside = (cell:new False))
     (let strays = (cell:new (the UFix 0)))
     (let mapped = (seq:map-with (fn (start end chunk)
                                   (cell:write! inside True)
                                   (chunk start end)
                                   (cell:write! inside False)
                                   (values))
                                 (fn (x)
                                   (unless (cell:read inside)
                                     (cell:increment! strays)
                                     (values))
                                   x)
                                 s))
     (is (== s mapped))
     (is (== 0 (cell:read strays))))
   (iter:into-iter (seq-test-shapes))))

;;; Invariants of the tree, operations that used to break them, and
;;; randomized operations checked against a list

(coalton-toplevel
  (declare seq-test-violations (seq:Seq :a * Boolean -> List String))
  (define (seq-test-violations s root?)
    "The invariants of the tree of S that it violates. ROOT? tells whether S
is the root of its tree."
    (let ((problem (fn (bad? message) (if bad? (make-list message) Nil))))
      (match s
        ((coalton/seq::LeafArray v)
         (<> (problem (> (vector:length v) 32) "leaf with more than 32 elements")
             (problem (and (not root?) (vector:empty? v)) "empty leaf below the root")))
        ((coalton/seq::RelaxedNode h fss cst subs)
         (let ((cumulative (vector:new)))
           (fold (fn (total sub)
                   (let ((next (+ total (seq:size sub))))
                     (vector:push! next cumulative)
                     next))
                 0
                 subs)
           (<> (fold <> Nil
                     (make-list
                      (problem (< h 2) "node of height below 2")
                      (problem (/= fss (math:^ 32 (- h 1))) "wrong full subtree size")
                      (problem (or (vector:empty? subs) (> (vector:length subs) 32))
                               "node with no subtrees or more than 32")
                      (problem (/= cumulative cst) "wrong cumulative size table")
                      (problem (iter:any! (fn (sub) (/= (coalton/seq::height sub) (- h 1)))
                                          (iter:into-iter subs))
                               "subtree of the wrong height")
                      (problem (iter:any! (fn (sub) (or (seq:empty? sub) (> (seq:size sub) fss)))
                                          (iter:into-iter subs))
                               "subtree empty or larger than the full subtree size")))
               (fold (fn (acc sub) (<> acc (seq-test-violations sub False))) Nil subs)))))))

  (declare seq-test-valid? (seq:Seq :a -> Boolean))
  (define (seq-test-valid? s)
    (list:null? (seq-test-violations s True))))

(define-test seq-conc-keeps-invariants ()
  ;; Each of these concatenations used to mix subtrees of different
  ;; heights, or to signal that unreachable code was reached.
  (iter:for-each!
   (fn (sizes)
     (let pieces = (map seq-test-of-size sizes))
     (let joined = (fold seq:conc (seq:new) pieces))
     (is (seq-test-valid? joined))
     (is (== (fold (fn (acc piece) (<> acc (the (List Integer) (into piece)))) Nil pieces)
             (into joined))))
   (iter:into-iter
    (make-list (make-list 1 3000 1) (make-list 1100 47 3000 5) (make-list 5 3000 31 1)
               (make-list 1 1024 1) (make-list 33000 1 1) (make-list 40 40 40 1100 40)
               (make-list 3000 3000 3000) (make-list 1 33000)))))

(define-test seq-pop-keeps-elements-and-invariants ()
  ;; A concatenated tree of 1025 elements: popping used to keep only the
  ;; first subtree of its root, which need not be full.
  (let s = (seq:conc (seq-test-of-size 600) (seq-test-of-size 425)))
  (match (seq:pop s)
    ((Some (Tuple x rest))
     (is (== 424 x))
     (is (seq-test-valid? rest))
     (is (== (list:take 1024 (the (List Integer) (into s))) (into rest))))
    ((None) (is False)))
  ;; Popping the only element of the second leaf of a subtree of 33
  ;; elements used to replace that subtree with its first leaf.
  (let u = (seq-test-of-size 1057))
  (match (seq:pop u)
    ((Some (Tuple x rest))
     (is (== 1056 x))
     (is (seq-test-valid? rest))
     (is (seq-test-valid? (seq:conc rest (seq-test-of-size 100)))))
    ((None) (is False)))
  ;; Popping everything.
  (let emptied = (rec % ((s (seq-test-of-size 2100)))
                   (match (seq:pop s)
                     ((Some (Tuple _ rest)) (is (seq-test-valid? rest)) (% rest))
                     ((None) s))))
  (is (seq:empty? emptied))
  ;; The popped element is no longer referred to by the new Seq, which
  ;; used to keep it in its leaf, beyond the end of the elements.
  (let strings = (fold seq:push (the (seq:Seq String) (seq:new)) (make-list "a" "b" "c" "d")))
  (match (seq:pop strings)
    ((Some (Tuple popped rest))
     (match rest
       ((coalton/seq::LeafArray v)
        (is (lisp (-> Boolean) (v popped)
              (cl:let ((end (cl:fill-pointer v)))
                (cl:setf (cl:fill-pointer v) (cl:array-total-size v))
                (cl:prog1 (cl:notany (cl:lambda (x) (cl:eq x popped)) (cl:subseq v end))
                  (cl:setf (cl:fill-pointer v) end))))))
       (_ (is False))))
    ((None) (is False))))

(define-test seq-construction ()
  ;; MAKE with a multiple of 32 elements used to add an empty leaf.
  (let s32 = (seq:make 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
                       16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))
  (is (seq-test-valid? s32))
  (is (== (seq-test-of-size 32) s32))
  (is (seq:empty? (the (seq:Seq Integer) (seq:make))))
  (iter:for-each!
   (fn (n)
     (let elements = (the (List Integer) (iter:collect! (iter:up-to (the Integer (into n))))))
     (let collected = (the (seq:Seq Integer) (iter:collect! (iter:into-iter elements))))
     (is (seq-test-valid? collected))
     (is (== elements (into collected)))
     (let converted = (the (seq:Seq Integer) (into elements)))
     (is (seq-test-valid? converted))
     (is (== collected converted))
     (let from-vector = (the (seq:Seq Integer) (into (the (vector:Vector Integer) (into elements)))))
     (is (== collected from-vector))
     (is (== collected (the (seq:Seq Integer) [x :for x :in elements])))
     (is (== (Some (seq:size collected)) (iter:size-hint (iter:into-iter collected))))
     (is (== elements (iter:collect! (iter:into-iter collected)))))
   (iter:into-iter (make-list 0 1 31 32 33 1024 1025 33000)))
  (is (== (seq:make 1 2 3) (the (seq:Seq Integer) [1 2 3])))
  (is (== (seq:make (Tuple 1 "one") (Tuple 2 "two"))
          (the (seq:Seq (Tuple Integer String)) [1 => "one" 2 => "two"])))
  (is (== (seq:make 0 1 4) [(* x x) :for x :below 3])))

(define-test seq-equality-and-printing ()
  (let a = (seq-test-of-size 5000))
  ;; The same elements, in a tree built differently.
  (let b = (fold seq:push (seq:new) (the (List Integer) (into a))))
  (is (== a b))
  (is (== (seq:conc a b) (seq:conc b a)))
  (is (/= a (seq:push a 0)))
  (is (/= a (unwrap (seq:put a 4999 -1))))
  (is (/= a (unwrap (seq:put a 0 -1))))
  (is (/= a (seq-test-of-size 4999)))
  (is (== "#<SEQ [1 2 3]>" (lisp (-> String) () (cl:prin1-to-string (coalton (seq:make 1 2 3))))))
  (is (== "#<SEQ []>" (lisp (-> String) () (cl:prin1-to-string (coalton (the (seq:Seq Integer) (seq:new)))))))
  (is (== "#<SEQ [0 1 2 ...]>"
          (lisp (-> String) (a)
            (cl:let ((cl:*print-length* 3)) (cl:prin1-to-string a)))))
  (is (== "#<SEQ [1 2 3]>"
          (lisp (-> String) ()
            (cl:let ((cl:*print-length* cl:most-positive-fixnum))
              (cl:prin1-to-string (coalton (seq:make 1 2 3))))))))

(coalton-toplevel
  (declare seq-test-random (cell:Cell UFix * UFix -> UFix))
  (define (seq-test-random state n)
    "A pseudo-random integer below N, which must be positive."
    (let ((x (mod (+ (* 1103515245 (cell:read state)) 12345) 2147483648)))
      (cell:write! state x)
      (mod (math:div x 65536) n)))

  (declare seq-test-random-seq (cell:Cell UFix * UFix * UFix
                                -> Tuple (seq:Seq Integer) (List Integer)))
  (define (seq-test-random-seq state n depth)
    "A Seq of N elements, built in a random way, and the list of its elements."
    (let ((base (the Integer (into (* 100000 (seq-test-random state 1000)))))
          (elements (map (fn (i) (+ base (into i))) (the (List UFix) (iter:collect! (iter:up-to n))))))
      (match (if (>= depth 3) (seq-test-random state 2) (seq-test-random state 5))
        (0 (Tuple (into elements) elements))
        (1 (Tuple (fold seq:push (seq:new) elements) elements))
        (2 (let ((cut (seq-test-random state (+ n 1))))
             (match (Tuple (seq-test-random-seq state cut (+ depth 1))
                           (seq-test-random-seq state (- n cut) (+ depth 1)))
               ((Tuple (Tuple a ea) (Tuple b eb))
                (Tuple (seq:conc a b) (<> ea eb))))))
        (3 (let ((extra (seq-test-random state 40)))
             (match (seq-test-random-seq state (+ n extra) (+ depth 1))
               ((Tuple s e)
                (Tuple (rec % ((s s) (k extra))
                         (if (== k 0)
                             s
                             (match (seq:pop s)
                               ((Some (Tuple _ rest)) (% rest (- k 1)))
                               ((None) s))))
                       (list:take n e))))))
        (_ (let ((cut (seq-test-random state (+ n 1))))
             (match (seq-test-random-seq state cut (+ depth 1))
               ((Tuple s e)
                (let ((rest (list:drop cut elements)))
                  (Tuple (fold seq:push s rest) (<> e rest))))))))))

  (declare seq-test-check (String * seq:Seq Integer * List Integer -> Unit))
  (define (seq-test-check operation s elements)
    (let ((problems (seq-test-violations s True)))
      (unless (list:null? problems)
        (error (fold <> (<> operation ": ") (list:intersperse ", " problems)))))
    (unless (== elements (into s))
      (error (<> operation ": wrong elements")))
    Unit)

  (declare seq-test-random-operations (UFix * UFix -> Unit))
  (define (seq-test-random-operations seed steps)
    "Apply STEPS random operations to a Seq, checking its invariants and
elements after each one."
    (let ((state (cell:new seed))
          (random-size (fn ()
                         (match (seq-test-random state 6)
                           (0 (seq-test-random state 40))
                           (1 (+ 1000 (seq-test-random state 100)))
                           (2 (+ 1000 (seq-test-random state 4000)))
                           (3 (* 32 (seq-test-random state 40)))
                           (_ (seq-test-random state 300))))))
      (rec % ((s (the (seq:Seq Integer) (seq:new))) (elements Nil) (k 0))
        (when (< k steps)
          (let ((n (seq:size s)))
            (match (seq-test-random state 6)
              (0 (let ((x (the Integer (into (seq-test-random state 1000)))))
                   (let ((s2 (seq:push s x)) (e2 (<> elements (make-list x))))
                     (seq-test-check "push" s2 e2)
                     (% s2 e2 (+ k 1)))))
              (1 (match (seq:pop s)
                   ((None) (% s elements (+ k 1)))
                   ((Some (Tuple x s2))
                    (unless (== (Some x) (list:last elements)) (error "pop: wrong element"))
                    (let ((e2 (list:take (- n 1) elements)))
                      (seq-test-check "pop" s2 e2)
                      (% s2 e2 (+ k 1))))))
              (2 (if (== n 0)
                     (% s elements (+ k 1))
                     (let ((i (seq-test-random state n)))
                       (let ((s2 (unwrap (seq:put s i -1)))
                             (e2 (<> (list:take i elements) (Cons -1 (list:drop (+ i 1) elements)))))
                         (seq-test-check "put" s2 e2)
                         (% s2 e2 (+ k 1))))))
              (3 (match (seq-test-random-seq state (random-size) 0)
                   ((Tuple u eu)
                    (let ((s2 (seq:conc s u)) (e2 (<> elements eu)))
                      (seq-test-check "conc on the right" s2 e2)
                      (% s2 e2 (+ k 1))))))
              (4 (match (seq-test-random-seq state (random-size) 0)
                   ((Tuple u eu)
                    (let ((s2 (seq:conc u s)) (e2 (<> eu elements)))
                      (seq-test-check "conc on the left" s2 e2)
                      (% s2 e2 (+ k 1))))))
              (_ (match (seq-test-random-seq state (random-size) 0)
                   ((Tuple u eu)
                    (seq-test-check "construction" u eu)
                    (% u eu (+ k 1)))))))))
      Unit)))

(define-test seq-random-operations ()
  (iter:for-each!
   (fn (seed)
     (is (== None
             (catch (progn (seq-test-random-operations seed 150) None)
               ((the Panic p) (Some (exception:message p)))))))
   (iter:into-iter (make-list 1 7919 15838 23757 31676 39595 47514 55433))))
