(coalton/utils:defstdlib-package #:coalton/hashmap
  (:use
   #:coalton
   #:coalton/builtin
   #:coalton/classes
   #:coalton/math/arith
   #:coalton/tuple
   #:coalton/hash
   #:coalton/optional
   #:coalton/experimental/loops)
  (:local-nicknames
   (#:arr  #:coalton/lisparray)
   (#:bits #:coalton/bits)
   (#:cell #:coalton/cell)
   (#:iter #:coalton/iterator)
   (#:list #:coalton/list)
   (#:math #:coalton/math)
   (#:util #:coalton-impl/runtime)
   )
  (:shadow #:count #:empty #:xor)
  (:export
   #:HashMap
   #:empty
   #:empty?
   #:count
   #:lookup
   #:insert
   #:adjoin
   #:replace
   #:remove
   #:update
   #:modify-get
   #:modify
   #:keys
   #:values
   #:entries
   #:union
   #:intersection
   #:difference
   #:xor
   #:show))

(in-package #:coalton/hashmap)

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
  ;; element (in this case, ROOT) won't appear exported in the docs.
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

  ;; Internal parameters

  (define trie-bits 5)
  (define hashword-bits 64)

  (declare max-arity UFix)
  (define max-arity (bits:shift trie-bits 1))

  (declare trie-bit-mask UFix)
  (define trie-bit-mask (- max-arity 1))

  (declare max-depth UFix)
  (define max-depth (unwrap-as UFix (math:quot hashword-bits trie-bits)))

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

  (declare trie-index (UFix -> UFix -> UFix))
  (define (trie-index hbits depth)
    "Extract index of trie at depth from hash bits"
    (lisp UFix (hbits depth trie-bit-mask trie-bits)
      (cl:logand trie-bit-mask
                 (cl:ash hbits (cl:- (cl:* trie-bits depth))))))

  ;; Performance hack.  Check if two objs are eq in CL sense.
  ;; Used to avoid unnecessary consing.
  (declare unchanged? (:a -> :a -> Boolean))
  (define (unchanged? a b)
    (lisp Boolean (a b) (cl:eq a b)))

  (declare chain-entries (HmNode :k :v -> List (HmEntry :k :v)))
  (define (chain-entries node)
    (match node
      ((Chain entries) entries)
      (_ (lisp :a (node)
           (util:coalton-bug "Chain expected, but got ~s" node)))))

  (declare chain-replace (Eq :k => HmNode :k :v -> :k -> :v -> Boolean
                             -> HmNode :k :v))
  (define (chain-replace chn key val insert?)
    (let ((recur (fn (entries)
                   (match entries
                     ((Nil) (if insert?
                                (Cons (HmEntry key val) Nil)
                                Nil))
                     ((Cons e es)
                      (if (== (entry-key e) key)
                          (Cons (HmEntry key val) es)
                          (let ((es2 (recur es)))
                            (if (unchanged? es es2)
                                es
                                (Cons e es2)))))))))
      (Chain (recur (chain-entries chn)))))

  ;; We avoid using list:remove-if, for we need to keep identity of input
  ;; when list is unmodified.  It also assumes that KEY is unique in XS.
  (declare chain-remove (Eq :k => HmNode :k :v  -> :k -> HmNode :k :v))
  (define (chain-remove chn key)
    (let entries = (chain-entries chn))
    (let ((traverse (fn (entries)
                     (match entries
                       ((Nil) Nil)
                       ((Cons e es)
                        (if (== (entry-key e) key)
                            es
                            (let ((es2 (traverse es)))
                              (if (unchanged? es es2)
                                  es
                                  (Cons e es2))))))))
          (es (traverse entries)))
      (if (unchanged? es entries)
          chn
          (Chain es))))

  (declare chain-update (Eq :k => HmNode :k :v -> :k
                            -> (Optional :v -> (Tuple (Optional :v) :a))
                            -> (Tuple (HmNode :k :v) :a)))
  (define (chain-update chn key f)
    (let ((recur (fn (entries)
                   (match entries
                     ((Nil)
                      (match (f None)
                        ((Tuple (None) aux)
                         (Tuple Nil aux))
                        ((Tuple (Some val) aux) ; insert
                         (Tuple (Cons (HmEntry key val) Nil) aux))))
                     ((Cons e es)
                      (if (== (entry-key e) key)
                          (match (f (Some (entry-value e)))
                            ((Tuple (None) aux) ; remove
                             (Tuple es aux))
                            ((Tuple (Some newval) aux) ; replace
                             (Tuple (if (unchanged? (entry-value e) newval)
                                        entries
                                        (Cons (HmEntry key newval) es))
                                    aux)))
                          (match (recur es)
                            ((Tuple es2 aux)
                             (Tuple (if (unchanged? es es2)
                                        es
                                        (Cons e es2))
                                    aux)))))))))
      (let (Tuple entries aux) = (recur (chain-entries chn)))
      (Tuple (Chain entries) aux)))

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
                            -> HmNode :k :v))
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
      ((== len 1) (Chain Nil))          ; this is the last entry
      ((== len 2)
       ;; If we get 2-entry array and the remaining branch is Leaf,
       ;; we can eliminate the Tree node.
       ;; In this case, pos is 0 or 1, so remaining node pos is (- 1 pos)
       (let ((pos (index->pos mask index))
             (remaining (arr:aref array (- 1 pos))))
         (match remaining
           ((Leaf entry) (Leaf entry))  ;drop tree
           (_ (newtree)))))
      (True (newtree))))

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
  (declare lookup (Hash :k => HashMap :k :v -> :k -> Optional :v))
  (define (lookup hm key)
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

  (repr :enum)
  (define-type InsertionMode
    InsertOp
    AdjoinOp
    ReplaceOp)

  (declare %insertion (Hash :k => InsertionMode -> UFix -> UFix
                            -> HmNode :k :v -> :k -> :v
                            -> HmNode :k :v))
  (define (%insertion mode depth hb node key val)
    "Internal common routine to handle insert, adjoin and replace.
`hb` is (hbits key)."
    (assert (<= depth max-depth))
    (match node
      ((Chain (Nil))  ; only happens on previously empty hashmap
       (match mode
         ((ReplaceOp) node) ; replace does nothing here
         (_ (Leaf (HmEntry key val)))))
      ((Chain _); only happens on depth == max-depth
       (match mode
         ((ReplaceOp) (chain-replace node key val False))
         (_           (chain-replace node key val True))))
      ((Leaf entry)
       (cond ((== key (entry-key entry))
              (match mode
                ((AdjoinOp) node) ; adjoin leaves existing entry
                (_ (Leaf (HmEntry key val))))) ;replace
             ((== depth max-depth)
              (match mode
                ((ReplaceOp) node)
                (_ (Chain (make-list (HmEntry key val) entry)))))
             (True
              (match mode
                ((ReplaceOp) node)
                (_ (Bud entry (HmEntry key val)))))))
      ((Bud entry1 entry2)
       (cond ((== key (entry-key entry1))
              (match mode
                ((AdjoinOp) node)
                (_ (Bud (HmEntry key val) entry2))))
             ((== key (entry-key entry2))
              (match mode
                ((AdjoinOp) node)
                (_ (Bud (HmEntry key val) entry1))))
             ((== depth max-depth)
              (match mode
                ((ReplaceOp) node)
                (_ (Chain (make-list (HmEntry key val)
                                     entry1 entry2)))))
             (True
              ;; delegate to the Tree branch
              (match mode
                ((ReplaceOp) node)
                (_ (%insertion mode depth hb
                               (new-tree-2 depth entry1 entry2)
                               key val))))))
      ((Tree mask arr)
       (let ind = (trie-index hb depth))
       (if (tree-has-entry? mask ind)
           (let ((newelt (%insertion mode (1+ depth) hb
                                     (arr:aref arr (index->pos mask ind))
                                     key val)))
             (tree-insert mask arr ind newelt))
           (match mode
             ((ReplaceOp) node) ; noop
             (_ (let ((newelt (Leaf (HmEntry key val))))
                  (tree-insert mask arr ind newelt))))))))

  (declare %removal (Hash :k => UFix -> UFix -> HmNode :k :v -> :k
                          -> HmNode :k :v))
  (define (%removal depth hb node key)
    (match node
      ((Chain _) (chain-remove node key))
      ((Leaf (HmEntry k _))
       (if (== key k)
           (Chain Nil)
           node))
      ((Bud entry1 entry2)
       (cond ((== key (entry-key entry1)) (Leaf entry2))
             ((== key (entry-key entry2)) (Leaf entry1))
             (True node)))
      ((Tree mask arr)
       (let ind = (trie-index hb depth))
       (if (tree-has-entry? mask ind)
           (let ((sub (arr:aref arr (index->pos mask ind))))
             (match (%removal (+ depth 1) hb sub key)
               ((Chain (Nil)) (tree-delete mask arr ind))
               (newsub
                (if (unchanged? sub newsub)
                    node
                    (tree-insert mask arr ind newsub)))))
           node))))

  ;; API
  (declare insert (Hash :k => HashMap :k :v -> :k -> :v
                        -> HashMap :k :v))
  (define (insert hm key val)
    "Returns a hashmap that has a new entry of (KEY, VAL) added to HM.  If HM
contains an entry with KEY, the new hashmap replaces it for the new entry."
    (HashMap (%insertion InsertOp 0 (hbits key) (root hm) key val)))

  ;; API
  (declare adjoin (Hash :k => HashMap :k :v -> :k -> :v
                        -> HashMap :k :v))
  (define (adjoin hm key val)
    "Returns a hashmap that has a new entry of (`key`, `val`) added to `hm`.
If `hm` alreay contains an entry with `key`, however, `hm` is returned as is."
    (HashMap (%insertion AdjoinOp 0 (hbits key) (root hm) key val)))

  ;; API
  (declare replace (Hash :k => HashMap :k :v -> :k -> :v
                         -> HashMap :k :v))
  (define (replace hm key val)
    "Returns a hashmap where the value associated with `key` is replaced
with `val`.  If `hm` does not contain an entry with `key`, `hm` is
 returned as is."
    (HashMap (%insertion ReplaceOp 0 (hbits key) (root hm) key val)))

  ;; API
  (declare remove (Hash :k => HashMap :k :v -> :k
                        -> HashMap :k :v))
  (define (remove hm key)
    "Returns a hashmap that is identical to HM except the entry with KEY is
removed.  If HM does not contain an entry with KEY, HM is returned as is."
    (if (empty? hm)
        hm
        (match (%removal 0 (hbits key) (root hm) key)
          ((Chain (Nil)) empty)
          (newroot
           (if (unchanged? (root hm) newroot)
               hm
               (HashMap newroot))))))

  ;; API
  ;;  Generic updater
  (declare update (Hash :k => HashMap :k :v -> :k
                        -> (Optional :v -> (Tuple (Optional :v) :a))
                        -> Tuple (HashMap :k :v) :a))
  (define (update hm key f)
    "Generic update/filter function. Takes a KEY and a F. F is passed
NONE if KEY is not found, (Some KEY) if it is found. F returns a tuple,
(Optional :v, :a). If the first term is NONE, then the KEY entry is cleared
from the hashmap. If it is (SOME v), then the KEY entry is updated to V.
The second term, :a, of the tuple is returned from `update` along with the
modified `HashMap`."
    (let ((hb (hbits key))
          ;; walk may return (Chain Nil) to indicate the sole node is
          ;; deleted.
          ;;(declare walk (Hash :k => UFix -> HmNode :k :v
          ;;                    -> Tuple (HmNode :k :v) :a))
          (walk
            (fn (depth node)
              (assert (<= depth max-depth))
              (match node
                ((Chain (Nil))  ; only happens on previously empty hashmap
                 (match (f None)
                   ((Tuple (None) aux)
                    (Tuple node aux))
                   ((Tuple (Some newval) aux)
                    (Tuple (Leaf (HmEntry key newval)) aux))))
                ((Chain _); only happens on depth == max-depth
                 (assert (== depth max-depth))
                 (match (f None)
                   ((Tuple (None) aux)
                    (Tuple (chain-remove node key) aux))
                   ((Tuple (Some newval) aux)
                    (Tuple (chain-replace node key newval True) aux))))
                ((Leaf entry)
                 (if (== key (entry-key entry))
                     (match (f (Some (entry-value entry)))
                       ((Tuple (None) aux)        ; delete
                        (Tuple (Chain nil) aux))
                       ((Tuple (Some newval) aux) ; replace
                        (Tuple (Leaf (HmEntry key newval)) aux)))
                     (match (f None)
                       ((Tuple (None) aux)        ; no-op
                        (Tuple node aux))
                       ((Tuple (Some newval) aux) ; insert
                        (Tuple
                         (%insertion InsertOp depth hb node key newval)
                         aux)))))
                ((Bud entry1 entry2)
                 (let ((newbud (fn (hit-entry miss-entry)
                                 (match (f (Some (entry-value hit-entry)))
                                   ((Tuple (None) aux) ; delete
                                    (Tuple (Leaf miss-entry) aux))
                                   ((Tuple (Some newval) aux) ;replace
                                    (Tuple (Bud (HmEntry key newval) miss-entry)
                                           aux))))))
                   (cond ((== key (entry-key entry1))
                          (newbud entry1 entry2))
                         ((== key (entry-key entry2))
                          (newbud entry2 entry1))
                         (True
                          (match (f None)
                            ((Tuple (None) aux) ; no-op
                             (Tuple node aux))
                            ((Tuple (Some newval) aux) ; insert
                             (Tuple
                              (%insertion InsertOp depth hb node key newval)
                              aux)))))))
                ((Tree mask arr)
                 (let ind = (trie-index hb (as UFix depth)))
                 (if (tree-has-entry? mask ind)
                     (match (walk (as UFix (1+ depth))
                                  (arr:aref arr (index->pos mask ind)))
                       ((Tuple (Chain (Nil)) aux) ; branch is deleted
                        (Tuple (tree-delete mask arr ind) aux))
                       ((Tuple rnode aux) ; branch is updated
                        (Tuple (tree-insert mask arr ind rnode) aux)))
                     (match (f None)
                       ((Tuple (None) aux) ; no-op
                        (Tuple node aux))
                       ((Tuple (Some newval) aux) ; insert
                        (Tuple (tree-insert mask arr ind
                                            (Leaf (HmEntry key newval)))
                               aux))))))))
          )
      (let (Tuple newnode aux) = (walk (the UFix 0) (root hm)))
      (Tuple (HashMap newnode) aux)))

  (inline)
  (declare modify-get (Hash :k => HashMap :k :v -> :k -> (:v -> :v) -> Tuple (HashMap :k :v) (Optional :v)))
  (define (modify-get hm key f)
    "Modify the value at KEY with F. Returns the modified `HashMap` and the
new value, if the key was found."
    (update hm key (fn (key?)
                     (match key?
                       ((None)
                        (Tuple None None))
                       ((Some v)
                        (let ((result (f v)))
                          (Tuple (Some result) (Some result))))))))

  (inline)
  (declare modify (Hash :k => HashMap :k :v -> :k -> (:v -> :v) -> HashMap :k :v))
  (define (modify hm key f)
    "Modify the value at KEY with F. Returns the modified `HashMap`."
    (fst (modify-get hm key f)))

  ;; Auxiliary functions for functor
  (declare %fmap-entry ((:v -> :w) ->  HmEntry :k :v ->  HmEntry :k :w))
  (define (%fmap-entry f (HmEntry k v))
    (HmEntry k (f v)))

  (declare %map ((:v -> :w) -> HmNode :k :v -> HmNode :k :w))
  (define (%map f node)
    (match node
      ((Chain es) (Chain (map (%fmap-entry f) es)))
      ((Leaf entry) (Leaf (%fmap-entry f entry)))
      ((Bud entry1 entry2) (Bud (%fmap-entry f entry1)
                                (%fmap-entry f entry2)))
      ((Tree mask arr)
       (let newarr = (arr:make-uninitialized (arr:length arr)))
       (dotimes (i (arr:length arr))
         (arr:set! newarr i (%map f (arr:aref arr i))))
       (Tree mask newarr))))

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
    "Returns an iterator over all the keys in a hashmap hm."
    (iter:new (->generator hm (fn (k _) k))))

  ;; API
  (declare values (Hash :k => HashMap :k :v -> (iter:Iterator :v)))
  (define (values hm)
    "Returns an iterator over all the values in a hashmap hm."
    (iter:new (->generator hm (fn (_ v) v))))

  ;; API
  (declare entries (Hash :k => HashMap :k :v -> (iter:Iterator (Tuple :k :v))))
  (define (entries hm)
    "Returns an iterator over all entries in hashmap hm."
    (iter:new (->generator hm Tuple)))
  )

;;
;; Instances
;;

(coalton-toplevel
  (define-instance (iter:IntoIterator (HashMap :k :v) (Tuple :k :v))
    (define (iter:into-iter hm)
      (iter:new (->generator hm Tuple))))

  (define-instance ((Eq :k) (Eq :v) (Hash :k) => Eq (HashMap :k :v))
    (define (== a b)
      (and (== (count a) (count b))
           (iter:every! (fn ((Tuple k v))
                          (== (lookup b k) (Some v)))
                        (entries a)))))

  (define-instance ((Hash :k) (Hash :v) => Hash (HashMap :k :v))
    (define (hash hm)
      (iter:elementwise-hash! (iter:into-iter hm))))

  (define-instance (Hash :k => iter:FromIterator (HashMap :k :v) (Tuple :k :v))
    (define iter:collect! collect!))

  (define-instance (Functor (HashMap :key))
    (define (map func mp)
      (HashMap (%map func (root mp)))))
  )

;;
;; Set operations
;;

(coalton-toplevel

  (declare union (Hash :k => HashMap :k :v -> HashMap :k :v -> HashMap :k :v))
  (define (union a b)
    "Construct a HashMap containing all the mappings from A and B.

If A and B contain mappings X -> A' and X -> B', the former mapping is kept.

The operation is associative, but not commutative."
    (iter:fold! (fn (m (Tuple k v)) (adjoin m k v)) a (iter:into-iter b)))

  (declare intersection (Hash :k => HashMap :k :v -> HashMap :k :v -> HashMap :k :v))
  (define (intersection a b)
    "Construct a HashMap containing all the mappings whose key is in both A and B.

The entries from A remains in the result."
    ;; TODO: This can be more efficient by traversing both trees in parallel,
    ;; as keys are both ordered with their hash values.
    (iter:fold! (fn (m (Tuple k v))
                  (match (lookup b k)
                    ((None) m)
                    ((Some _) (insert m k v))))
                Empty (iter:into-iter a)))

  (declare difference (Hash :k => HashMap :k :v -> HashMap :k :v -> HashMap :k :v))
  (define (difference a b)
    "Returns a HashMap that contains mappings in `a` but not in `b`."
    (iter:fold! (fn (m (Tuple k _v)) (remove m k)) a (iter:into-iter b)))

  (declare xor (Hash :k => HashMap :k :v -> HashMap :k :v -> HashMap :k :v))
  (define (xor a b)
    "Returns a HashMap that contains mappings either in `a` or in `b`,
but not in both."
    (iter:fold! (fn (m (Tuple k v))
                  (fst (update m k
                               (fn (e)
                                 (match e
                                   ((None) (Tuple (Some v) Unit))
                                   ((Some _) (Tuple None Unit)))))))
                Empty (iter:chain! (iter:into-iter a)
                                   (iter:into-iter b))))

  (define-instance (Hash :k => Semigroup (HashMap :k :v))
    (define <> union))

  (define-instance (Hash :k => Monoid (HashMap :k :v))
    (define mempty Empty))

  (declare show ((Hash :k) (Into :k String) (Into :v String) => HashMap :k :v -> String))
  (define (show hm)
    "Return a human-readable representation of HM."
    (let the-entries = (entries hm))
    (match (iter:next! the-entries)
      ((None) "()")
      ((Some (Tuple k1 v1))
       (<> "("
           (<>
            (iter:fold!
             (fn (accum (Tuple k v))
                 (<> accum (<> ", " (<> (into k) (<> " -> " (into v))))))
             (<> (into k1) (<> " -> " (into v1)))
             the-entries)
            ")")))))
  )

;;
;; Debug tool
;;

(coalton-toplevel
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
