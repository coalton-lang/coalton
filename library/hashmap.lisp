(coalton-library/utils:defstdlib-package #:coalton-library/hashmap
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
  (:shadow #:count #:empty)
  (:export
   #:Hashmap
   #:empty
   #:empty?
   #:count
   #:get
   #:insert
   #:remove
   #:keys
   #:values))

(in-package #:coalton-library/hashmap)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  (define-type (HmEntry :key :value)
    (HmEntry :key :value))

  (define-type (HmNode :key :value)
    "Node of Hashmap can be a Leaf (single entry), Bud (two entries),
Chain (mutiple entries), or Tree.  Chain is only used at the root
of empty hashmap, or nodes at the maximum depth."
    (Leaf (HmEntry :key :value))
    (Bud (HmEntry :key :value) (HmEntry :key :value))
    (Chain (List (HmEntry :key :value)))
    (Tree U32 (arr:LispArray (HmNode :key :value))))

  ;; We don't use a struct so that the accessor to the internal data
  ;; element (in this case, ROOT) doesn't appear exported in the docs.
  ;;
  ;; We also make this type (repr :lisp) so that it's an opaque type
  ;; which may be depended upon by other non-Coalton code.
  (repr :lisp)
  (define-type (HashMap :key :value)
    "Immutable map (also known as a dictionary or dict) using hashes. Implemented as a hash array mapped trie data structure."
    (HashMap (HmNode :key :value)))

  (inline)
  (define (root (HashMap node))
    node))

;; Mask, index, and position
;;
;;  HashMap's intermediate node (Tree) consists of a bitmask and a packed
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
;;     index   mask     pos
;;   +-------+-------+-------+
;;       0       0       -
;;       1       1       0
;;       2       1       1
;;       3       0       -
;;       4       1       2
;;       5       0       -
;;       6       0       -
;;       7       1       3
;;
;;  index->pos converts index to pos.  tree-has-entry? checks if the given
;;  index has a child.
;;
;;  We use 32bit mask so the maximum number of children per node is 32.

;; Leaf, Bud, Tree
;;
;;  If a tree's position has only one child, it is Leaf.  Another entry
;;  comes to the same position, we turn it to Bud.  More than two entry
;;  come to the same position, we create a Tree.  We use Bud since
;;  creating and searching a Bud is slightly faster than a full Tree.
;;
;;  If we use up all bits of the hash value, we chain the entries of the
;;  same hash value into Chain and we do linear search.  It only happens
;;  when the tree reaches max-depth.

;; Ideas to be explored:
;;  - Linear updater.  Especially useful when building from the iterator,
;;    for it will save tons of copying.

(coalton-toplevel

  ;; Instances

  (define-instance (iter:IntoIterator (HashMap :k :v) (Tuple :k :v))
    (define (iter:into-iter hm)
      (iter:new (->generator hm Tuple))))

  (define-instance ((Eq :k) (Eq :v) => Eq (HashMap :k :v))
    (define (== a b)
      (iter:elementwise==! (iter:into-iter a) (iter:into-iter b))))

  (define-instance ((Hash :k) (Hash :v) => Hash (HashMap :k :v))
    (define (hash hm)
      (iter:elementwise-hash! (iter:into-iter hm))))

  (define-instance ((Hash :k) => iter:FromIterator (HashMap :k :v) (Tuple :k :v))
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

  (define (entry-key (HmEntry key _)) key)
  (define (entry-value (HmEntry _ value)) value)

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

  (declare replace (Eq :k => :k -> :v -> List (HmEntry :k :v)
                       -> List (HmEntry :k :v)))
  (define (replace key val entries)
    (match entries
      ((Nil) (Cons (HmEntry key val) Nil))
      ((Cons e es)
       (match e
         ((HmEntry k1 _)
          (if (== key k1)
              (Cons (HmEntry key val) es)
              (let ((es2 (replace key val es)))
                (if (unchanged? es es2)
                    es
                    (Cons e es2)))))))))

  (declare tree-insert (U32 -> (arr:LispArray (HmNode :k :v)) -> UFix -> (HmNode :k :v)
                            -> (HmNode :k :v)))
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

  (declare tree-delete (U32 -> (arr:LispArray (HmNode :k :v)) -> UFix
                            -> Optional (HmNode :k :v)))
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
    "Create a new tree with ENTRY as a sole branch.  The caller will add a
new entry to the returned node."
    (let ((ind1 (trie-index (hbits (entry-key entry)) depth)))
      (Tree (index->mask ind1) (arr:make 1 (Leaf entry)))))

  (define (new-tree-2 depth entry1 entry2)
    "Create a new tree with ENTRY1 and ENTRY2 as branches.  The caller will add
a new entry."
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
  (declare empty (HashMap :k :v))
  (define empty
    "An empty HashMap"
    (HashMap (Chain Nil)))

  ;; API
  (declare empty? (HashMap :k :v -> Boolean))
  (define (empty? hm)
    "Returns True if a hashmap HM is empty, False if not."
    (match (root hm)
      ((Chain (Nil)) True)
      (_ False)))

  ;; API
  ;; TODO: We can cache the # of entries in Tree node to avoid scanning
  ;; the entire tree every time.
  (declare count (HashMap :k :v -> Integer))
  (define (count hm)
    "Returns the number of entries in HM."
    (into
     (rec walk ((node (root hm)))
       (match node
         ((Leaf _) 1)
         ((Bud _ _) 2)
         ((Chain lis) (list:length lis))
         ((Tree _ array)
          (fold (fn (sum elt) (+ sum (walk elt))) 0 array))))))

  ;; API
  (declare get (Hash :k => HashMap :k :v -> :k -> Optional :v))
  (define (get hm key)
    "Returns a value associated with KEY in the hashmap HM."
    (let hb = (hbits key))
    (rec search ((depth 0)
                 (node (root hm)))
      (match node
        ((Leaf (HmEntry k v))
         (if (== key k)
             (Some v)
             None))
        ((Bud (HmEntry k1 v1) (HmEntry k2 v2))
         (cond ((== key k1) (Some v1))
               ((== key k2) (Some v2))
               (True None)))
        ((Chain entries)
         (map entry-value (iter:find! (fn (e) (== key (entry-key e)))
                                      (iter:into-iter entries))))
        ((Tree mask arr)
         (let ind = (trie-index hb depth))
         (if (tree-has-entry? mask ind)
             (search (+ depth 1) (arr:aref arr (index->pos mask ind)))
             None)))))

  ;; API
  (declare insert (Hash :k => HashMap :k :v -> :k -> :v
                        -> HashMap :k :v))
  (define (insert hm key val)
    "Returns a hashmap that has a new entry of (KEY, VAL) added to HM.  If HM
containes an entry with KEY, the new hashmap replaces it for the new entry."
    (let ((hb (hbits key))
          (walk (fn (depth node)
                  (assert (<= depth max-depth))
                  (match node
                    ((Chain (Nil))  ; only happens on previously empty hashmap
                     (Leaf (HmEntry key val)))
                    ((Chain entries); only happens on depth == max-depth
                     (Chain (replace key val entries)))
                    ((Leaf entry)
                     (cond ((== key (entry-key entry))
                            (Leaf (HmEntry key val))) ;replace
                           ((== depth max-depth)
                            (Chain (make-list (HmEntry key val) entry)))
                           (True
                            (Bud entry (HmEntry key val)))))
                    ((Bud entry1 entry2)
                     (cond ((== key (entry-key entry1))
                            (Bud (HmEntry key val) entry2))
                           ((== key (entry-key entry2))
                            (Bud (HmEntry key val) entry1))
                           ((== depth max-depth)
                            (Chain (make-list (HmEntry key val)
                                              entry1 entry2)))
                           (True
                            ;; delegate to the Tree branch
                            (walk depth
                                  (new-tree-2 depth entry1 entry2)))))
                    ((Tree mask arr)
                     (let ind = (trie-index hb depth))
                     (let newelt =
                       (if (tree-has-entry? mask ind)
                           (walk (+ depth 1)
                                 (arr:aref arr (index->pos mask ind)))
                           (Leaf (HmEntry key val))))
                     (tree-insert mask arr ind newelt)))))
          )
      (HashMap (walk 0 (root hm)))))

  (declare unchanged? (:a -> :a -> Boolean))
  (define (unchanged? a b)
    (lisp Boolean (a b) (cl:eq a b)))

  ;; We avoid using list:remove-if, for we need to keep identity of input
  ;; when list is unmodified.  It also assumes that KEY is unique in XS.
  (define (remove-keyed-entry xs key)
    (match xs
      ((Nil) Nil)
      ((Cons x xs) (if (== (entry-key x) key)
                       xs
                       (remove-keyed-entry xs key)))))


  ;; API
  (declare remove (Hash :k => HashMap :k :v -> :k
                        -> HashMap :k :v))
  (define (remove hm key)
    "Returns a hashmap that is identical to HM except the entry with KEY is
removed.  If HM does not contain an entry with KEY, HM is returned as is."
    (let ((hb (hbits key))
          (walk (fn (depth node)
                  (match node
                    ((Chain (Nil)) (Some node))
                    ((Chain entries)
                     (let ((es (remove-keyed-entry entries key)))
                       (if (unchanged? es entries)
                           (Some node)
                           (match es
                             ((Nil) None)
                             ((Cons _ _)  (Some (Chain es)))))))
                    ((Leaf (HmEntry k _))
                     (if (== key k)
                         None
                         (Some node)))
                    ((Bud entry1 entry2)
                     (cond ((== key (entry-key entry1)) (Some (Leaf entry2)))
                           ((== key (entry-key entry2)) (Some (Leaf entry1)))
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
      (if (empty? hm)
          hm
          (match (walk 0 (root hm))
            ((None) empty)
            ((Some newroot)
             (if (unchanged? (root hm) newroot)
                 hm
                 (HashMap newroot)))))))

  ;; Iterator
  (declare ->generator (HashMap :k :v -> (:k -> :v -> :a)
                                -> (Unit -> Optional :a)))
  (define (->generator hm f)
    (let current = (cell:new (root hm)))
    (let current-ind = (cell:new (the UFix 0)))
    (let path = (cell:new (the (List (Tuple UFix (HmNode :k :v))) Nil)))
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
            ((Chain (Cons (HmEntry k v) es))
             (cell:write! current (Chain es))
             (Some (f k v)))
            ((Leaf (HmEntry k v))
             (cell:write! current (Chain Nil))
             (Some (f k v)))
            ((Bud (HmEntry k v) entry2)
             (cell:write! current (Leaf entry2))
             (Some (f k v)))
            ((Tree _ array)
             (if (== (cell:read current-ind) (arr:length array))
                 (if (next!?) (%loop) None)
                 (let ((i (cell:read current-ind)))
                   (cell:write! current-ind 0)
                   (cell:push! path (Tuple (1+ i) (cell:read current)))
                   (cell:write! current (arr:aref array i))
                   (%loop))))))))
    gen)

  (declare collect! ((Hash :k) => (iter:Iterator (Tuple :k :v) -> HashMap :k :v)))
  (define (collect! iter)
    (iter:fold! (fn (hm (Tuple k v))
                  (insert hm k v))
                empty iter))

  ;; API
  (declare keys (Hash :k => HashMap :k :v -> (iter:Iterator :k)))
  (define (keys hm)
    "Returns an interator to iterate over all the keys in a hashmap hm."
    (iter:new (->generator hm (fn (k _) k))))

  ;; API
  (declare values (Hash :k => HashMap :k :v -> (iter:Iterator :v)))
  (define (values hm)
    "Returns an interator to iterate over all the values in a hashmap hm."
    (iter:new (->generator hm (fn (_ v) v))))

  ;; Debug tools
  (declare dump (HashMap :k :v -> Unit))
  (define (dump hm)
    "For debugging"
    (rec dump-node ((node (root hm))
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
