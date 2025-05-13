(coalton-library/utils:defstdlib-package #:coalton-library/hamt
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/math/arith
   #:coalton-library/hash
   #:coalton-library/optional
   #:coalton-library/experimental/loops)
  (:local-nicknames
   (#:arr  #:coalton-library/lisparray)
   (#:bits #:coalton-library/bits)
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:math #:coalton-library/math)
   )
  (:shadow #:count)
  (:export
   #:Hamt
   #:new
   #:empty?
   #:count
   #:get
   #:insert
   #:remove))

(in-package #:coalton-library/hamt)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  (define-type (HamtEntry :key :value)
    (HamtEntry :key :value))

  (define-type (HamtNode :key :value)
    "Node of Hamt can be a Leaf (single entry), Bud (two entries),
Chain (mutiple entries), or Tree.  Chain is only used at the root
of empty hamt, or nodes at the maximum depth."
    (Leaf (HamtEntry :key :value))
    (Bud (HamtEntry :key :value) (HamtEntry :key :value))
    (Chain (List (HamtEntry :key :value)))
    (Tree U32 (arr:LispArray (HamtNode :key :value))))

  (define-struct (Hamt :key :value)
    "Hash array mapped trie"
    (root (HamtNode :key :value)))

  )

;; Mask, index, and position
;;
;;  HAMT's intermediate node (Tree) consists of a bitmask and a packed
;;  LispArray.
;;
;;  Bitmask shows exsting entry; if N-th bit is on, the node has #N child.
;;  Suppose a node has #1, #2, #4, #7 children.  Bitmask is 10010110,
;;  and the array has 4 entries, each of which for the child node.
;;
;;  The 'position', or 'pos', is the actual index of the array of #index
;;  child.  In the above example, the index and position has the following
;;  relations.
;;
;;     index    pos
;;   +-------+------+
;;       0       -
;;       1       0
;;       2       1
;;       3       -
;;       4       2
;;       5       -
;;       6       -
;;       7       3
;;
;;  index->pos convers index to pos.  tree-has-entry? checks if the given
;;  index has a child.
;;
;;  We use 32bit mask so the maximum number of children per node is 32.

;; Ideas to be explored:
;;  - Linear updater.  Especially useful when building from the iterator,
;;    for it will save tons of copying.

(coalton-toplevel

  ;; Instances

  (define-instance (iter:IntoIterator (Hamt :k :v) (Tuple :k :v))
    (define (iter:into-iter ht)
      (iter:new (->generator ht))))

  (define-instance ((Eq :k) (Eq :v) => Eq (Hamt :k :v))
    (define (== a b)
      (iter:elementwise==! (iter:into-iter a) (iter:into-iter b))))

  (define-instance ((Hash :k) (Hash :v) => Hash (Hamt :k :v))
    (define (hash ht)
      (iter:elementwise-hash! (iter:into-iter ht))))

  (define-instance ((Hash :k) => iter:FromIterator (Hamt :k :v) (Tuple :k :v))
    (define iter:collect! collect!))

  ;; Internal parameters

  (define trie-bits 5)
  (define hashword-bits 64)

  (declare max-arity UFix)
  (define max-arity (bits:shift trie-bits 1))

  (declare trie-bit-mask UFix)
  (define trie-bit-mask (- max-arity 1))

  (define max-depth (math:quot hashword-bits trie-bits))

  ;; Utilities

  (define (entry-key entry)
    (match entry
      ((HamtEntry key _) key)))

  (define (entry-value entry)
    (match entry
      ((HamtEntry _ value) value)))

  (declare tree-has-entry? (U32 -> UFix -> Boolean))
  (define (tree-has-entry? mask index)
    (lisp Boolean (mask index)
      (cl:logtest mask (cl:ash 1 (cl:the (cl:unsigned-byte 64) index)))))

  (declare index->pos (U32 -> UFix -> UFix))
  (define (index->pos mask index)
    (lisp UFix (mask index)
      (cl:logcount (cl:logand mask (cl:1- (cl:ash 1 index))))))

  (declare mask-set (U32 -> UFix -> U32))
  (define (mask-set mask index)
    (lisp U32 (mask index)
      (cl:the cl:fixnum (cl:logior mask
                                   (cl:the cl:fixnum (cl:ash 1 index))))))

  (declare mask-clear (U32 -> UFix -> U32))
  (define (mask-clear mask index)
    (lisp U32 (mask index)
      (cl:logand mask (cl:lognot (cl:ash 1 index)))))

  ;; Single index to mask.
  (declare index->mask (UFix -> U32))
  (define (index->mask index)
    (lisp U32 (index) (cl:ash 1 index)))

  (declare hbits (Hash :k => :k -> UFix))
  (define (hbits key)
    "Compute 'hash bits'.  It's easier for us to treat it as UFix than Hash."
    (let ((hv (hash key)))
      (lisp UFix (hv) hv)))

  (declare trie-index (UFix -> Integer -> UFix))
  (define (trie-index hbits depth)
    "Extract index of trie at depth from hash bits"
    (lisp UFix (hbits depth trie-bit-mask trie-bits)
      (cl:logand trie-bit-mask
                 (cl:ash hbits (cl:- (cl:* trie-bits depth))))))

  (declare replace (Eq :k => :k -> :v -> List (HamtEntry :k :v)
                       -> List (HamtEntry :k :v)))
  (define (replace key val entries)
    (match entries
      ((Nil) (Cons (HamtEntry key val) Nil))
      ((Cons e es)
       (match e
         ((HamtEntry k1 _)
          (if (== key k1)
              (Cons (HamtEntry key val) es)
              (let ((es2 (replace key val es)))
                (if (unchanged? es es2)
                    es
                    (Cons e es2)))))))))

  (declare tree-insert (U32 -> (arr:LispArray (HamtNode :k :v)) -> UFix -> (HamtNode :k :v)
                            -> (HamtNode :k :v)))
  (define (tree-insert mask array index elt)
    "Returns a new tree node out of MASK and ARRAY, plus ELT
being inserted at INDEX. If the array already has the
entry with INDEX, the entry is replaced."
    (let size = (arr:length array))
    (let newsize = (if (tree-has-entry? mask index)
                       size
                       (+ size 1)))
    (let newarray = (arr:make-uninitialized newsize))
    (let newmask = (mask-set mask index))
    (dotimes (i max-arity)
      (when (tree-has-entry? newmask i)
        (let ((oldpos (index->pos mask i))
              (newpos (index->pos newmask i)))
          (if (== i index)
              (arr:set! newarray newpos elt)
              (arr:set! newarray newpos (arr:aref array oldpos))))))
    (Tree newmask newarray))

  (declare tree-delete (U32 -> (arr:LispArray (HamtNode :k :v)) -> UFix
                            -> Optional (HamtNode :k :v)))
  (define (tree-delete mask array index)
    "Removes the entry of INDEX-th entry from the array.  The caller must
ensure the entry exists.  Returns an updated node."
    (let len = (arr:length array))
    (let newtree =
      (fn ()
        (let ((newmask (mask-clear mask index))
              (newarray (arr:make-uninitialized (1- len))))
          (dotimes (i max-arity)
            (when (tree-has-entry? newmask i)
              (let ((oldpos (index->pos mask i))
                    (newpos (index->pos newmask i)))
                (arr:set! newarray newpos (arr:aref array oldpos)))))
          (Tree newmask newarray))))
    (cond
      ((== len 1) None)                 ; this is the last entry
      ((== len 2)
       ;; If we get 2-entry array and the remaining branch is Leaf,
       ;; we can eliminate the Tree node.
       ;; In this case, pos is 0 or 1, so remaining node pos is (- 1 pos)
       (let ((pos (index->pos mask index))
             (remaining (arr:aref array (- 1 pos))))
         (match remaining
           ((Leaf entry) (Some (Leaf entry))) ;drop tree
           (_ (Some (newtree))))))
      (True (Some (newtree)))))

  (define (new-tree-1 depth entry)
    "Create a new tree with ENTRY as a sole branch."
    (let ((ind1 (trie-index (hbits (entry-key entry)) depth)))
      (Tree (index->mask ind1) (arr:make 1 (Leaf entry)))))

  (define (new-tree-2 depth entry1 entry2)
    "Create a new tree with ENTRY as a sole branch."
    (let ((ind1 (trie-index (hbits (entry-key entry1)) depth))
          (ind2 (trie-index (hbits (entry-key entry2)) depth)))
      (if (== ind1 ind2)
          (Tree (index->mask ind1) (arr:make 1 (Bud entry1 entry2)))
          (let ((arr (arr:make 2 (Chain Nil)))
                (mask (bits:or (index->mask ind1) (index->mask ind2))))
            (cond ((< ind1 ind2)
                   (arr:set! arr 0 (Leaf entry1))
                   (arr:set! arr 1 (Leaf entry2)))
                  (True
                   (arr:set! arr 0 (Leaf entry2))
                   (arr:set! arr 1 (Leaf entry1))))
            (Tree mask arr)))))

  ;; API
  (declare new (Unit -> Hamt :k :v))
  (define (new)
    "Returns an empty Hamt"
    (Hamt (Chain Nil)))

  ;; API
  (declare empty? (Hamt :k :v -> Boolean))
  (define (empty? ht)
    "Returns True if a hamt HT is empty, False if not."
    (match (.root ht)
      ((Chain (Nil)) True)
      (_ False)))

  ;; API
  (declare count (Hamt :k :v -> Integer))
  (define (count ht)
    "Returns the number of entries in HT."
    (into
     (rec walk ((node (.root ht)))
       (match node
         ((Leaf _) 1)
         ((Bud _ _) 2)
         ((Chain lis) (list:length lis))
         ((Tree _ array)
          (fold (fn (sum elt) (+ sum (walk elt))) 0 array))))))

  ;; API
  (declare get (Hash :k => Hamt :k :v -> :k -> Optional :v))
  (define (get ht key)
    "Returns a value associated with KEY in the hamt HT."
    (let hb = (hbits key))
    (rec search ((depth 0)
                 (node (.root ht)))
      (match node
        ((Leaf (HamtEntry k v))
         (if (== key k)
             (Some v)
             None))
        ((Bud (HamtEntry k1 v1) (HamtEntry k2 v2))
         (cond ((== key k1) (Some v1))
               ((== key k2) (Some v2))
               (True None)))
        ((Chain entries)
         (match (iter:find! (fn (e) (== key (entry-key e)))
                            (iter:into-iter entries))
           ((None) None)
           ((Some e) (Some (entry-value e)))))
        ((Tree mask arr)
         (let ind = (trie-index hb depth))
         (if (tree-has-entry? mask ind)
             (search (+ depth 1) (arr:aref arr (index->pos mask ind)))
             None)))))

  ;; API
  (declare insert (Hash :k => Hamt :k :v -> :k -> :v
                        -> Hamt :k :v))
  (define (insert ht key val)
    "Returns a hamt that has a new entry of (KEY, VAL) added to HT.  If HT
containes an entry with KEY, the new hamt replaces it for the new entry."
    (let ((hb (hbits key))
          (walk (fn (depth node)
                  (match node
                    ((Chain (Nil))  ; only happens on previously empty hamt
                     (Leaf (HamtEntry key val)))
                    ((Chain entries); only happens on depth == max-depth
                     (Chain (replace key val entries)))
                    ((Leaf entry)
                     (cond ((== key (entry-key entry))
                            (Leaf (HamtEntry key val))) ;replace
                           ((== depth max-depth)
                            (Chain (make-list (HamtEntry key val) entry)))
                           (True
                            (Bud entry (HamtEntry key val)))))
                    ((Bud entry1 entry2)
                     (cond ((== key (entry-key entry1))
                            (Bud (HamtEntry key val) entry2))
                           ((== key (entry-key entry2))
                            (Bud (HamtEntry key val) entry1))
                           (True
                            (walk depth (new-tree-2 depth entry1 entry2)))))
                    ((Tree mask arr)
                     (let ind = (trie-index hb depth))
                     (let newelt =
                       (if (tree-has-entry? mask ind)
                           (walk (+ depth 1)
                                 (arr:aref arr (index->pos mask ind)))
                           (Leaf (HamtEntry key val))))
                     (tree-insert mask arr ind newelt)))))
          )
      (Hamt (walk 0 (.root ht)))))

  (declare unchanged? (:a -> :a -> Boolean))
  (define (unchanged? a b)
    (lisp Boolean (a b) (cl:eq a b)))

  ;; We avoid using list:remove-if, for we need to keep identity of input
  ;; when lis is unmodified.  It is also assumed that KEY is unique in XS.
  (define (remove-keyed-entry xs key)
    (match xs
      ((Nil) Nil)
      ((Cons x xs) (if (== (entry-key x) key)
                       xs
                       (remove-keyed-entry xs key)))))


  ;; API
  (declare remove (Hash :k => Hamt :k :v -> :k
                        -> Hamt :k :v))
  (define (remove ht key)
    "Returns a hamt that is identical to HT except the entry with KEY is
removed.  If HT does not contain an entry with KEY, HT is returned as is."
    (let ((hb (hbits key))
          (walk (fn (depth node)
                  (match node
                    ((Chain (Nil)) (Some node))
                    ((Chain entries)
                     (let ((es (remove-keyed-entry entries key)))
                       (if (unchanged? es entries)
                           (Some node)
                           (Some (Chain es)))))
                    ((Leaf (HamtEntry k _))
                     (if (== key k)
                         None
                         (Some node)))
                    ((Bud entry1 entry2)
                     (cond ((== key (entry-key entry1)) (Some (Leaf entry2)))
                           ((== key (entry-key entry2)) (Some (Leaf entry2)))
                           (True None)))
                    ((Tree mask arr)
                     (let ind = (trie-index hb depth))
                     (if (tree-has-entry? mask ind)
                         (let ((sub (arr:aref arr (index->pos mask ind))))
                           (match (walk (+ depth 1) sub)
                             ((None) (tree-delete mask arr ind))
                             ((Some newsub)
                              (if (unchanged? sub newsub)
                                  (Some node)
                                  (Some (tree-insert mask arr ind newsub))))))
                         (Some node)))))))
      (if (empty? ht)
          ht
          (match (walk 0 (.root ht))
            ((None) (new))
            ((Some newroot)
             (if (unchanged? (.root ht) newroot)
                 ht
                 (Hamt newroot)))))))

  ;; Iterator
  (declare ->generator (Hamt :k :v -> (Unit -> Optional (Tuple :k :v))))
  (define (->generator ht)
    (let current = (cell:new (.root ht)))
    (let current-ind = (cell:new (the UFix 0)))
    (let path = (cell:new (the (List (Tuple UFix (HamtNode :k :v))) Nil)))
    (let next!? = (fn ()
                    (match (cell:pop! path)
                      ((None) False)
                      ((Some (Tuple ind node))
                       (cell:write! current node)
                       (cell:write! current-ind ind)
                       True))))
    (let gen =
      (fn ()
        (rec %loop ()
          (match (cell:read current)
            ((Chain (Nil))
             (if (next!?) (%loop) None))
            ((Chain (Cons (HamtEntry k v) es))
             (cell:write! current (Chain es))
             (Some (Tuple k v)))
            ((Leaf (HamtEntry k v))
             (cell:write! current (Chain Nil))
             (Some (Tuple k v)))
            ((Bud (HamtEntry k v) entry2)
             (cell:write! current (Leaf entry2))
             (Some (Tuple k v)))
            ((Tree _ array)
             (if (== (cell:read current-ind) (arr:length array))
                 (if (next!?) (%loop) None)
                 (let ((i (cell:read current-ind)))
                   (cell:write! current-ind 0)
                   (cell:push! path (Tuple (1+ i) (cell:read current)))
                   (cell:write! current (arr:aref array i))
                   (%loop))))))))
    gen)

  (declare collect! ((Hash :k) => (iter:Iterator (Tuple :k :v) -> Hamt :k :v)))
  (define (collect! iter)
    (iter:fold! (fn (ht (Tuple k v))
                  (insert ht k v))
                (new) iter))

  ;; Debug tools
  (declare dump (Hamt :k :v -> Unit))
  (define (dump ht)
    "For debugging"
    (rec dump-node ((node (.root ht))
                    (indent 0))
      (match node
        ((Leaf entry)
         (lisp Unit (entry indent)
           (cl:progn
             (cl:format cl:t "~vALeaf: ~W~%"
                        indent #\space entry)
             Unit)))
        ((Bud entry1 entry2)
         (lisp Unit (entry1 entry2 indent)
           (cl:progn
             (cl:format cl:t "~vABud: ~W~%"
                        indent #\space (cl:list entry1 entry2))
             Unit)))
        ((Chain entries)
         (lisp Unit (entries indent)
           (cl:progn
             (cl:format cl:t "~vAChain: ~W~%"
                        indent #\space entries)
             Unit)))
        ((Tree mask arr)
         (lisp Unit (mask indent arr)
           (cl:progn
             (cl:format cl:t "~vATree[~D]: ~32,'0B~%"
                        indent #\space (arr:length arr) mask)
             Unit))
         (dotimes (i max-arity)
           (when (tree-has-entry? mask i)
             (let ((pos (index->pos mask i)))
               (lisp Unit (i indent)
                 (cl:progn
                   (cl:format cl:t "~vA~2d:~%" indent #\space i)
                   Unit))
               (dump-node (arr:aref arr pos) (+ indent 2)))))))))
  )
