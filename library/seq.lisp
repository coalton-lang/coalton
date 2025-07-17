(coalton-library/utils:defstdlib-package #:coalton-library/seq
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/classes
   #:coalton-library/experimental/loops)
  (:local-nicknames
   (#:types #:coalton-library/types)
   (#:math #:coalton-library/math)
   (#:optional #:coalton-library/optional)
   (#:cell #:coalton-library/cell)
   (#:vector #:coalton-library/vector)
   (#:iter #:coalton-library/iterator))
  (:export
   #:Seq
   #:new
   #:singleton
   #:push
   #:pop
   #:size
   #:get
   #:put
   #:empty?
   #:conc
   #:make))

(in-package #:coalton-library/seq)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;;;
;;; This module implements a persistent sequence data type based on
;;; Relaxed Radix Balanced Trees, introduced by Phillip Bagwell
;;; See:
;;; https://www.semanticscholar.org/paper/RRB-Trees%3A-Efficient-Immutable-Vectors-Bagwell-Rompf/30c8c562f6421ab6b00d0b7faebd897c407de69c
;;;

;;; Private utility macro for convenient matching of a single case of a ADT
(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro match1 (pattern form cl:&body body)
    `(match ,form
       (,pattern ,@body)
       (_ (unreachable)))))

(coalton-toplevel
  ;;
  ;; Seq
  ;;

  (declare max-branching UFix)
  (define max-branching 32)

  (declare min-branching UFix)
  (define min-branching 24)

  (define-type (Seq :a)
    (RelaxedNode
     UFix                               ; height
     UFix                               ; cached full subtree size
     (vector:Vector UFix)               ; cumulative size table
     (vector:Vector (Seq :a)))         ; subtrees
    (LeafArray (vector:Vector :a)))

  (declare new (types:RuntimeRepr :a => Unit -> Seq :a))
  (define (new)
    "Create a new empty `Seq`."
    (LeafArray (vector:new)))

  ;; Can be written as (make x), but for consistency with vec:singleton
  ;; (And we can't use make before defining it)
  (declare singleton (types:RuntimeRepr :a => :a -> Seq :a))
  (define (singleton x)
    "Create a seq with a signle element."
    (LeafArray (vector:singleton x)))

  (declare size (Seq :a -> UFix))
  (define (size seq)
    "Return the number of elements in the `seq`."
    (match seq
      ((RelaxedNode _ _ cst _) (vector:last-unsafe cst))
      ((LeafArray leaves) (vector:length leaves))))

  (define (empty? seq)
    (== 0 (size seq)))

  (declare get (Seq :a -> UFix -> Optional :a))
  (define (get seq idx)
    "Get the member of `seq` at index `idx`, or `None` if `idx` is larger
than `(size seq)`"
    (match seq
      ((LeafArray leaves)
       (vector:index idx leaves))

      ((RelaxedNode _ fss cst sts)
       (do
        (let guess = (math:div idx fss))
        ((Tuple subtree-idx offset) <- (cst-search guess cst idx))
         (subtree <- (vector:index subtree-idx sts))
         (get subtree (- idx offset))))))

  (declare put (Seq :a -> Ufix -> :a -> Optional (Seq :a)))
  (define (put seq idx a)
    "If `idx` is less than `(size seq)`, Return a new `seq` whose `idx` position
contains `a`."
    (match seq
      ((LeafArray leaves)
       (if (<= (vector:length leaves) idx)
           None
           (let ((newleaves (vector:copy leaves)))
             (vector:set! idx a newleaves)
             (Some (LeafArray newleaves)))))

      ((RelaxedNode h fss cst sts)
       (do
        (let guess = (math:div idx fss))
        ((Tuple subtree-idx offset) <- (cst-search guess cst idx))
         (subtree <- (vector:index subtree-idx sts))
         (new-subtree <- (put subtree (- idx offset) a))
         (let ((newsts (vector:copy sts)))
           (vector:set! subtree-idx new-subtree newsts)
           (pure (RelaxedNode h fss cst newsts)))))))

  (define (push seq a)
    "Push `a` onto the end of `seq`, returning a new `Seq` instance."
    (let (Tuple node in-place?) =  (%push seq a))
    (if in-place? node
        (let ((h (+ 1 (height seq)))
              (s (size seq)))
          (RelaxedNode h (perfect-subtree-size-at-height h)
                       (vector:make s (+ 1 s))
                       (vector:make seq node)))))

  (define (pop seq)
    "If `seq` is empty, return `None`. Otherwise, the last member of `seq` and
a new `Seq` instance."
    (match seq
      ((LeafArray leaves)
       (do
        (let newleaves = (vector:copy leaves))
        (leaf <- (vector:pop! newleaves))
         (pure (Tuple leaf (LeafArray newleaves)))))

      ((RelaxedNode h fss cst sts)
       (do
        ((Tuple leaf newsub) <- (pop (vector:last-unsafe sts)))
        (let newsts = (vector:copy sts))
        (let newcst = (vector:copy cst))
        (let last-idx = (- (vector:length cst) 1))
        (let seq-size = (size seq))
         (pure
          (cond
            ;; this was the only thing left in seq
            ((== 1 seq-size)
             (Tuple leaf newsub))     ; newsub is empty

            ;; the seq was exactly one larger than the subtree size
            ;; for the current height, this means we can reduce the tree height
            ((== (+ 1 fss) seq-size)
             (Tuple leaf (vector:index-unsafe 0 sts)))

            ;; it wasn't, but newsub is empty
            ((== 0 (size newsub))
             (vector:pop! newcst)
             (vector:pop! newsts)
             (Tuple leaf (RelaxedNode h fss newcst newsts)))

            (True
             (vector:set! last-idx (- (vector:last-unsafe newcst) 1) newcst)
             (vector:set! last-idx newsub newsts)
             (Tuple leaf (RelaxedNode h fss newcst newsts)))))))))

    (define (conc left right)
    "Concatenate two `Seq`s"
    (cond
      ((empty? left) right)
      ((empty? right) left)
      (True
       (match (Tuple left right)
         ((Tuple (LeafArray _) (LeafArray _))
          (rebalance-branches (vector:make left right)))

         ((Tuple (LeafArray _) (RelaxedNode _ht _fss _cst subts))
          (match (conc left (vector:head-unsafe subts))
            ((LeafArray leaves)
             (rebalance-branches (replace-first subts (LeafArray leaves))))

            ((RelaxedNode _nht _nfss _ncst nsubts)
             (rebalance-branches (vector:append nsubts (butfirst subts))))))

         ((Tuple (RelaxedNode _ht _fss _cst subts)  (LeafArray _))
          (match (conc (vector:last-unsafe subts) right)
            ((LeafArray leaves)
             (rebalance-branches (replace-last subts (LeafArray leaves))))

            ((RelaxedNode _nht _nfss _ncst nsubts)
             (rebalance-branches (vector:append (butlast subts) nsubts)))))

         ((Tuple (RelaxedNode lht _lfss _lcst lsubts) (RelaxedNode rht _rfss _rcst rsubts))
          (cond ((< lht rht)
                 (match1 (RelaxedNode _nht _nfss _ncst nsubts)
                     (conc left (vector:head-unsafe rsubts))
                   (rebalance-branches
                    (vector:append nsubts (butfirst rsubts)))))
                ((> lht rht)
                 (match1 (RelaxedNode _nht _nfss _ncst nsubts)
                     (conc (vector:last-unsafe lsubts) right)
                   (rebalance-branches
                    (vector:append (butlast lsubts) nsubts))))
                (True
                 (match1 (RelaxedNode _nht _nfss _ncst nsubts)
                     (conc (vector:last-unsafe lsubts) (vector:head-unsafe rsubts))
                   (rebalance-branches
                    (fold <> (butlast lsubts) (make-list nsubts (butfirst rsubts))))))))))))

  ;;
  ;; Instances
  ;;

  (define-instance (types:RuntimeRepr :a => Semigroup (Seq :a))
    (define <> conc))

  (define-instance (types:RuntimeRepr :a => Monoid (Seq :a))
    (define mempty (new)))

  (define-instance (iter:IntoIterator (Seq :a) :a)
    (define (iter:into-iter seq)
      (match seq
        ((LeafArray v) (iter:into-iter v))
        ((RelaxedNode _ _ _ subs)
         (fold iter:chain! iter:empty (map iter:into-iter subs))))))

  (define-instance (Functor Seq)
    (define (map f seq)
      (match seq
        ((LeafArray v) (LeafArray (map f v)))
        ((RelaxedNode ht fss cst subs)
         (RelaxedNode ht fss cst (map (map f) subs))))))

  (define-instance (types:RuntimeRepr :a => iter:FromIterator (Seq :a) :a)
    (define iter:collect!
      (iter:fold! push (new))))

  (define-instance (types:RuntimeRepr :a => Default (Seq :a))
    (define default new))

  (define-instance ((Foldable :f) (types:RuntimeRepr :a) => Into (:f :a) (Seq :a))
    (define (into fld)
      (fold push (new) fld)))

  (define-instance (types:RuntimeRepr :t => Unfoldable Seq :t)
    (define (unfold f seed)
      (rec next ((s (new))
                 (seed seed))
        (match (f seed)
          ((None) s)
          ((Some (Tuple seed x)) (next (conc (singleton x) s) seed)))))
    (define (unfoldr f seed)
      (rec next ((s (new))
                 (seed seed))
        (match (f seed)
          ((None) s)
          ((Some (Tuple x seed)) (next (push s x) seed))))))

  (define-instance (types:RuntimeRepr :t => Tabulatable Seq :t)
    ;; Could be more efficient by taking advantage of internal structure of seq
    (define (tabulate f len)
      (rec next ((s (new))
                 (i 0))
        (if (== i len)
            s
            (next (push s (f i)) (+ 1 i))))))

  (define-instance (Eq :a => Eq (Seq :a))
    (define (== a b)
      ;; because they're immutable, if they happen to be identical then == is true
      (or (lisp Boolean (a b) (cl:eq a b))
          ;; otherwise, recurse
          (and (== (size a) (size b))
               (iter:every! (fn ((Tuple x y)) (== x y))
                            (iter:zip! (iter:into-iter a)
                                       (iter:into-iter b)))))))

  (define-instance (Into (Seq :a) (List :a))
    (define (into seq)
      (iter:collect! (iter:into-iter seq))))

  (define-instance (Into (Seq :a) (vector:Vector :a))
    (define (into seq)
      (iter:collect! (iter:into-iter seq))))

  ;;
  ;; Helpers
  ;;

  (define (height seq)
    (match seq
      ;;relaxed nodes should have a minimum height of 2
      ((RelaxedNode h _ _ _) h)
      ((LeafArray _) 1)))

  ;; helper. Calculate the capacity of a subtree of node
  (define (perfect-subtree-size-at-height h)
    (math:^ max-branching (- h 1)))

  ;; helper. Return the index into a Relaxed Node's subtree array, as
  ;; well as the offset to subtract from the search index.
  (define (cst-search guess cst idx)
    (let ((search-forward
            (fn (gs last-cumulative)
              (do (cumulative <- (vector:index gs cst))
                  (if (< idx cumulative)
                      (pure (Tuple gs last-cumulative))
                      (search-forward (+ 1 gs) cumulative))))))
      (>>= (alt (if (math:zero? guess)  ; avoid UFix underflow
                    None
                    (vector:index (- guess 1) cst)) ; Note, 0 < guess <= 31
                (pure 0))
           (search-forward guess))))


  (define (%push seq a)
    (match seq
      ((LeafArray v)
       (if (< (vector:length v) max-branching)
           (let ((newv (vector:copy v)))
             (vector:push! a newv)
             (Tuple (LeafArray newv) True))
           (Tuple (LeafArray (vector:make a)) False)))

      ((RelaxedNode h fss cst sts)
       (let (Tuple new-node in-place?) = (%push (vector:last-unsafe sts) a))
       (cond
         ;; modified "in place": i.e. height not adjusted on recursive step
         (in-place?
          (let ((newsts
                  (vector:copy sts))
                (newcst
                  (vector:copy cst))
                (last-idx
                  (- (vector:length sts) 1)))
            (vector:set! last-idx (+ 1 (vector:last-unsafe newcst)) newcst)
            (vector:set! last-idx new-node newsts)
            (Tuple (RelaxedNode h fss newcst newsts) True)))

         ;; wasn't in place, but there's room here
         ((< (vector:length cst) max-branching)
          (let ((newsts
                  (vector:copy sts))
                (newcst
                  (vector:copy cst)))
            (vector:push! (+ 1 (size seq)) newcst)
            (vector:push! new-node newsts)
            (Tuple (RelaxedNode h fss newcst newsts) True)))

         ;; not in place, and no room here.
         (True
          (let ((newh (+ 1 (height new-node))))
            (Tuple
             (RelaxedNode newh
                          (perfect-subtree-size-at-height newh)
                          (vector:make 1)
                          (vector:make new-node))
             False)))))))

  (declare group-vectors (types:runtimerepr :a => UFix -> vector:Vector :a -> vector:Vector (vector:Vector :a)))
  (define (group-vectors len vec)
    "Return a vector of vectors of elements from `vec`, each of length `len`
ecxept for the last one which has a nonzero length less than or equal
to `len`."
    (let ((result
            (vector:new))
          (acc
            (vector:new)))
      (iter:for-each!
       (fn (elem)
         (vector:push! elem acc)
         (when (== len (vector:length acc))
           (vector:push! (vector:copy acc) result)
           (vector:clear! acc)))
       (iter:into-iter vec))
      ;; push the last if non-empty
      (unless (== 0 (vector:length acc))
        (vector:push! acc result)
        Unit)
      result))

  (declare build-cumulative-size-table (vector:Vector (Seq :a) -> vector:Vector UFix))
  (define (build-cumulative-size-table subtrees)
    (let ((cumulative (cell:new 0)))
      (iter:collect!
       (map (fn (sub)
              (let ((ret (+ (size sub) (cell:read cumulative))))
                (cell:swap! cumulative ret)
                ret))
            (iter:into-iter subtrees)))))

  (declare shift-n-onto! (vector:Vector :a -> vector:Vector :a -> UFix -> Unit))
  (define (shift-n-onto! target source n0)
    "Shifts the first `n` members of `source` onto the end of `target`, and
shifts the each member of `target` down by `n` positions.  Mutates both
`target` and `source`. If `n` is greater than or equal to the length of the
`source`, then the entire `source` is so shifted."
    (let ((source-len
            (vector:length source))
          (n
            (min n0 source-len)))
      (iter:for-each!
       (fn (i)
         (vector:push! (vector:index-unsafe i source) target)
         (iter:for-each!
          (fn (j)
            (do
             (x <- (vector:index (+ j n) source))
             (pure (vector:set! j x source)))
            Unit)
          (iter:range-increasing n i source-len))
         Unit)
       (iter:up-to n))
      (iter:for-each! (fn (_i) (vector:pop! source) Unit) (iter:up-to n))))

  (define (replace-first v a)
    (let ((cv (vector:copy v)))
      (vector:set! 0 a cv)
      cv))

  (define (replace-last v a)
    (let ((cv  (vector:copy v)))
      (vector:set! (- (vector:length v) 1) a cv)
      cv))

  (declare butfirst (vector:Vector :a -> vector:Vector :a))
  (define (butfirst v)
    (iter:collect!
     (map (flip vector:index-unsafe v)
          (iter:range-increasing 1 1 (vector:length v)))))

  (define (butlast v)
    (let ((cv (vector:copy v)))
      (vector:pop-unsafe! cv)
      cv))

  (define (branch-count seq)
    (match seq
      ((LeafArray v) (vector:length v))
      ((RelaxedNode _ _ v _) (vector:length v))))

  (define (%shift-n-branches-onto seq1 seq2 n)
    "Moves `n` subbranches from the front of `seq2` to the back of
`seq1`. Leaves both in a potentially dirty state: the cumulative size
table of relaxed nodes may be incaccurate."
    (match (Tuple seq1 seq2)
      ((Tuple (LeafArray vec1) (LeafArray vec2))
       (shift-n-onto! vec1 vec2 n))
      ((Tuple (RelaxedNode _ _ _ vec1) (RelaxedNode _ _ _ vec2))
       (shift-n-onto! vec1 vec2 n))
      (_ (unreachable))))

  (define (rebuild-size-table seq)
    (match seq
      ((LeafArray _) seq)
      ((RelaxedNode ht fss _ subs)
       (RelaxedNode ht fss (build-cumulative-size-table subs) subs))))

  (define (make-relaxed-node ht branches)
    (RelaxedNode ht (perfect-subtree-size-at-height ht) (build-cumulative-size-table branches) branches))

  (declare make-node-from-branches (types:runtimerepr :a => vector:Vector (Seq :a) -> Seq :a))
  (define (make-node-from-branches branches)
    "Makes a `Seq` tall enough to contain all the branches. Branches are
all assumed to have the same height."
    (let branches-length = (vector:length branches))
    (cond ((== 0 branches-length)
           (new))
          ((== 1 branches-length)
           (vector:pop-unsafe! branches))
          (True
           (let ht = (height (vector:head-unsafe branches)))
           (if (<= (vector:length branches) max-branching)
               (make-relaxed-node (+ ht 1) branches)
               (let ((groups
                       (group-vectors max-branching branches))
                     (taller-branches
                       (map (make-relaxed-node (+ ht 1)) groups)))
                 (make-node-from-branches taller-branches))))))

  (define (copy seq)
    "A shallow copy of `seq`"
    (match seq
      ((LeafArray vec)
       (LeafArray (vector:copy vec)))
      ((RelaxedNode ht fss cst seq)
       (RelaxedNode ht fss (vector:copy cst) (vector:copy seq)))))

  (declare rebalance-branches (types:runtimerepr :a => vector:Vector (Seq :a) -> Seq :a))
  (define (rebalance-branches branches)
    "Ensures each member of `branches` has between `min-branching` and
`max-branching` subnodes of their own. Returns a new `Seq` whose branches
are the rebalanced `branches`.

This function assumes that each member of `branches` has the same
height. It also assumes that each member of `branches` is itself already
balanced.

It attempts to rebalance with a minimum of array copying."
    (let ((stop
            (- (vector:length branches) 1))
          (cached-branch
            (cell:new None))
          (branch-rebalancer
            (compose
             rebuild-size-table
             (fn (i)
               (let branch =
                 (match (cell:read cached-branch)
                   ((None) (vector:index-unsafe i branches))
                   ((Some cached) cached)))
               (let subbranch-count = (branch-count branch))
               (cond
                 ((or (== i stop) (<= min-branching subbranch-count))
                  (cell:swap! cached-branch None)
                  branch)
                 (True
                  (let this-branch =
                    (if (optional:some? (cell:read cached-branch))
                        branch          ; already a copy
                        (copy branch)))
                  ;; need to mutate the next branch so we copy it
                  (let next-branch =
                    (copy (vector:index-unsafe (+ i 1) branches)))
                  ;; Do the subbranch shifting. NOTE: the choice of
                  ;; branch capacity comes with trade-offs: using
                  ;; MAX-BRANCHING tends to keep trees short over
                  ;; time, improving lookup speed. Using MIN-BRANCHING
                  ;; tends to minimize the number of array copies we
                  ;; will make over time. I have chosen MIN-BRANCHING.
                  (%shift-n-branches-onto
                   this-branch next-branch (- min-branching subbranch-count))
                  ;; cache the branch for the next round
                  (cell:swap! cached-branch (Some next-branch))
                  this-branch))))))

      ;;make a node from rebalanced branches
      (make-node-from-branches
       (iter:collect!
        (iter:filter! (compose (< 0) size)
                      (map branch-rebalancer
                           (iter:up-through stop))))))))

(cl:defmacro make (cl:&rest elems)
  "Create a new `Seq` containing `elems`."
  (cl:let* ((l
              (cl:length elems))
            (leaf-arrays
              (cl:loop :for i :from 0 :to l :by 32
                       :for i+32 := (cl:+ i 32)
                       :when (cl:< i+32 l)
                         :collect (cl:subseq elems i i+32)
                       :else
                         :collect (cl:subseq elems i)))
            (la-count
              (cl:length leaf-arrays)))
    (cl:cond
      ((cl:zerop la-count)
       '(new))
      ((cl:= 1 la-count)
       `(LeafArray (vector:make ,@(cl:car leaf-arrays))))
      ((cl:<= la-count 32)
       `(rebuild-size-table
         (RelaxedNode 2 32 (vector:new) (vector:make ,@(cl:loop :for a :in leaf-arrays
                                                                :collect `(LeafArray (vector:make ,@a)))))))
      (cl:t
       (cl:reduce (cl:lambda (acc la) `(conc ,acc (LeafArray (vector:make ,@la))))
                  (cl:rest leaf-arrays)
                  :initial-value `(LeafArray (vector:make ,@(cl:first leaf-arrays))))))))

;; This method implementation uses :around because sum types implement
;; cl:print-object for each representation, to avoid a brittle design
;; by overwriting them manually, the :around method short-circuits them.
(cl:defmethod cl:print-object :around ((self seq) stream)
  (cl:print-unreadable-object (self stream :type cl:nil)
    (cl:format stream "SEQ~{ ~A~}"
               (coalton ((the ((Seq :a) -> (List :a)) (fn (seq) (into seq)))
                         (lisp (Seq :a) () self)))))
  self)

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/SEQ")
