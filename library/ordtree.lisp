(coalton-library/utils:defstdlib-package :coalton-library/ordtree
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/hash
   #:coalton-library/tuple
   #:coalton-library/functions
   #:coalton-library/math)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:cell #:coalton-library/cell)
   (#:util #:coalton-impl/util))
  (:shadow #:empty)
  (:export
   #:OrdTree #:empty
   #:empty?
   #:lookup
   #:insert
   #:adjoin
   #:replace
   #:remove
   #:update
   #:max-element
   #:min-element
   #:lookup-neighbors
   #:transform-elements
   #:increasing-order
   #:decreasing-order
   #:union
   #:intersection
   #:difference
   #:xor))

(in-package :coalton-library/ordtree)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;; Based on Ralf Hinze, Purely Functional 1-2 Brother Trees
;; https://www.researchgate.net/publication/220676591_Purely_Functional_1-2_Brother_Trees

(coalton-toplevel

  (define-type (OrdTree :elt)
    "A 1-2 brother tree, sorted by `<=>` and unique by `==`."

    Empty
    "exported; an empty tree."

    (N1 (OrdTree :elt))
    "unexported; unary node"

    (N2 (OrdTree :elt) :elt (OrdTree :elt))
    "unexported; binary node"

    (N3 (OrdTree :elt) :elt (OrdTree :elt) :elt (OrdTree :elt))
    "unexported; ternary node - only appear intermediately during balancing"

    (L2 :elt)
    "unexported; leaf node - only appear intermediately during balancing"
    )

  (declare empty? (OrdTree :elt -> Boolean))
  (define (empty? t)
    (match t
      ((Empty) True)
      (_ False)))

  ;;; aux
  (declare stray-node (Unit -> :a))
  (define (stray-node)
    (lisp :a ()
      (util:coalton-bug "Encountered ephemeral node during traversal")))

  (declare consistent? (OrdTree :elt -> Boolean))
  (define (consistent? t)
    "Check invariance condition of the tree `t`.  If the condition is broken,
an error is thrown."
    (let ((dep (fn (t)
                 "Returns the depth of nullary tree, while checking other conditions."
                 (match t
                   ((Empty) 0)
                   ((N1 (N1 _)) (error "Unary node has unary child"))
                   ((N1 t) (1+ (dep t)))
                   ((N2 l _ r) (let ((dl (dep l))
                                     (dr (dep r)))
                                 (if (== dl dr)
                                     (1+ dl)
                                     (error "Depth inbalance"))))
                   (_ (error "Ephemeral node"))))))
      (dep t)
      True))

  ;;; searching

  ;; API
  (declare lookup (Ord :elt => OrdTree :elt -> :elt -> Optional :elt))
  (inline)
  (define (lookup haystack needle)
    "If HAYSTACK contains an element `==` to NEEDLE, return it."
    (let ((lup (fn (t)
                 (match t
                   ((Empty) None)
                   ((N1 t) (lup t))
                   ((N2 left elt right)
                    (match (<=> needle elt)
                      ((LT) (lup left))
                      ((EQ) (Some elt))
                      ((GT) (lup right))))
                   (_ (stray-node))))))
      (lup haystack)))

  ;; Smart constructors; eliminating intermediate node and balancing the
  ;; subtree.  See the paper for the details.
  (declare make-root (OrdTree :elt -> OrdTree :elt))
  (inline)
  (define (make-root t)
    (match t
      ((L2 a) (N2 Empty a Empty))
      ((N3 t1 a1 t2 a2 t3) (N2 (N2 t1 a1 t2) a2 (N1 t3)))
      ((N1 t) t)
      (_ t)))

  (declare make-n1 (OrdTree :elt -> OrdTree :elt))
  (inline)
  (define (make-n1 t)
    (match t
      ((L2 a) (N2 Empty a Empty))
      ((N3 t1 a1 t2 a2 t3) (N2 (N2 t1 a1 t2) a2 (N1 t3)))
      (_ (N1 t))))

  (declare make-n2i (OrdTree :elt -> :elt -> OrdTree :elt -> OrdTree :elt))
  (inline)
  (define (make-n2i tl a tr)
    "Smart N2 constructor for insertion/replace only"
    (match tl
      ((L2 a1) (N3 Empty a1 Empty a tr))
      ((N3 t1 a1 t2 a2 t3)
       (match tr
         ((N1 t4) (N2 (N2 t1 a1 t2) a2 (N2 t3 a t4)))
         (_       (N3 (N2 t1 a1 t2) a2 (N1 t3) a tr))))
      ((N1 t1)
       (match tr
         ((N3 t2 a2 t3 a3 t4) (N2 (N2 t1 a t2) a2 (N2 t3 a3 t4)))
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         ((N1 t2)             (N1 (N2 t1 a t2)))
         (_                   (N2 tl a tr))))
      ((N2 _ _ _)
       (match tr
         ((N3 t2 a2 t3 a3 t4) (N3 tl a (N1 t2) a2 (N2 t3 a3 t4)))
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         (_                   (N2 tl a tr))))
      (_
       (match tr
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         (_                   (N2 tl a tr))))))

  (declare make-n2 (OrdTree :elt -> :elt -> OrdTree :elt -> OrdTree :elt))
  (inline)
  (define (make-n2 tl a tr)
    "Generic N2 constructor.  This handles both deletion and insertion,
so that it can be directly used for update procedure."
    (match tl
      ((L2 a1) (N3 Empty a1 Empty a tr))
      ((N3 t1 a1 t2 a2 t3)
       (match tr
         ((N1 t4) (N2 (N2 t1 a1 t2) a2 (N2 t3 a t4)))
         (_       (N3 (N2 t1 a1 t2) a2 (N1 t3) a tr))))
      ((N1 (N1 t1))
       (match tr
         ((N2 (N1 t2) a2 (= t3 (N2 _ _ _))) (N1 (N2 (N2 t1 a t2) a2 t3)))
         ((N2 (N2 t2 a2 t3) a3 (N1 t4))     (N1 (N2 (N2 t1 a t2) a2
                                                    (N2 t3 a3 t4))))
         ((N2 (= t2 (N2 _ _ _)) a2 (= t3 (N2 _ _ _)))
                                            (N2 (N2 (N1 t1) a t2) a2
                                                (N1 t3)))
         ((N1 t2)                           (N1 (N2 (N1 t1) a t2)))
         (_                                 (N2 tl a tr))))
      ((N1 t1)
       (match tr
         ((N3 t2 a2 t3 a3 t4) (N2 (N2 t1 a t2) a2 (N2 t3 a3 t4)))
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         ((N1 t2)             (N1 (N2 t1 a t2)))
         (_                   (N2 tl a tr))))
      ((N2 (N1 t1) a1 (N2 t2 a2 t3))
       (match tr
         ((N1 (N1 t4))        (N1 (N2 (N2 t1 a1 t2) a2
                                      (N2 t3 a t4))))
         ((N3 t2 a2 t3 a3 t4) (N3 tl a (N1 t2) a2 (N2 t3 a3 t4)))
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         (_                   (N2 tl a tr))))
      ((N2 (= t1 (N2 _ _ _)) a1 (N1 t2))
       (match tr
         ((N1 (N1 t3))        (N1 (N2 t1 a1 (N2 t2 a t3))))
         ((N3 t2 a2 t3 a3 t4) (N3 tl a (N1 t2) a2 (N2 t3 a3 t4)))
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         (_                   (N2 tl a tr))))
      ((N2 (= t1 (N2 _ _ _)) a1 (= t2 (N2 _ _ _)))
       (match tr
         ((N1 (= t3 (N1 _)))  (N2 (N1 t1) a1 (N2 t2 a t3)))
         ((N3 t2 a2 t3 a3 t4) (N3 tl a (N1 t2) a2 (N2 t3 a3 t4)))
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         (_                   (N2 tl a tr))))
      ((N2 _ _ _)
       (match tr
         ((N3 t2 a2 t3 a3 t4) (N3 tl a (N1 t2) a2 (N2 t3 a3 t4)))
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         (_                   (N2 tl a tr))))
      (_
       (match tr
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         (_                   (N2 tl a tr))))))

  ;; API
  (declare insert (Ord :elt => OrdTree :elt -> :elt -> OrdTree :elt))
  (define (insert t a)
    "Returns an ordtree that has an new entry `a` added to `t`.  If `t` already
has an entry which is `==` to `a`,  The new ordtree has `a` in place of the
existing entry."
    (let ((ins (fn (n)
                 (match n
                   ((Empty)    (L2 a))
                   ((N1 t1)    (make-n1 (ins t1)))
                   ((N2 l b r)
                    (match (<=> a b)
                      ((LT)    (make-n2i (ins l) b r))
                      ((EQ)    (N2 l a r))
                      ((GT)    (make-n2i l b (ins r)))))
                   (_ (stray-node))))))
      (make-root (ins t))))

  ;; API
  (declare adjoin (Ord :elt => OrdTree :elt -> :elt -> OrdTree :elt))
  (define (adjoin t a)
    "Returns an ordtree that has a new entry `a`.  If `t` already has an entry
which is `==` to `a`, however, the original `t` is returned as is."
    (let ((rep (fn (n)
                 (match n
                   ((Empty)    (L2 a))
                   ((N1 t1)    (make-n1 (rep t1)))
                   ((N2 l b r)
                    (match (<=> a b)
                      ((LT)    (make-n2i (rep l) b r))
                      ((EQ)    n)
                      ((GT)    (make-n2i l b (rep r)))))
                   (_ (stray-node))))))
      (make-root (rep t))))

  ;; API
  (declare replace (Ord :elt => OrdTree :elt -> :elt -> OrdTree :elt))
  (define (replace t a)
    "Returns an ordtree that has an entry `a` only if `t` already has an
entry which is `==` to `a`.  The original entry is replaced with the given
`a`.  If `t` doesn't have an entry `==` to `a`, `t` is returned as is."
    (let ((rep (fn (n)
                 (match n
                   ((Empty)    Empty)
                   ((N1 t1)    (make-n1 (rep t1)))
                   ((N2 l b r)
                    (match (<=> a b)
                      ((LT)    (make-n2i (rep l) b r))
                      ((EQ)    (N2 l a r))
                      ((GT)    (make-n2i l b (rep r)))))
                   (_ (stray-node))))))
      (make-root (rep t))))

  ;; API
  (declare remove (Ord :elt => OrdTree :elt -> :elt -> OrdTree :elt))
  (define (remove t a)
    "Returns an ordtree that is the same as `t` except that the entry
which is `==` to `a` is removed.  If `t` does not have such an entry,
`t` is returned as is."
    (let ((del (fn (n)
                 (match n
                   ((Empty) Empty)
                   ((N1 t)  (N1 (del t)))
                   ((N2 l b r)
                    (match (<=> a b)
                      ((LT) (make-n2 (del l) b r))
                      ((EQ) (match (split-min r)
                              ((None)               (N1 l))
                              ((Some (Tuple a1 r1)) (make-n2 l a1 r1))))
                      ((GT) (make-n2 l b (del r)))))
                   (_ (stray-node))))))
      (make-root (del t))))

  (define (split-min n)
    "Helper for removal"
    (match n
      ((Empty)       None)
      ((N1 t)        (match (split-min t)
                       ((None)              None)
                       ((Some (Tuple a t1)) (Some (Tuple a (N1 t1))))))
      ((N2 t1 a1 t2) (match (split-min t1)
                       ((None)               (Some (Tuple a1 (N1 t2))))
                       ((Some (Tuple a t11)) (Some (Tuple a
                                                          (make-n2 t11 a1 t2))))))
      (_ (stray-node))))

  ;; API
  (declare update (Ord :elt => OrdTree :elt -> :elt
                       -> (Optional :elt -> (Tuple (Optional :elt) :a))
                       -> (Tuple (OrdTree :elt) :a)))
  (define (update t a f)
    "Generic update.  Look for the element `a` in `t`.  If there's an entry,
call `f` with the existing entry wrapped with Some.  If there isn't an entry,
call `f` with None.  `f` must return a tuple of possible replacement entry,
and an auxiliary result.

If the entry doesn't exist in `t` and `f` returns `(Some elt)`, `elt` is
inserted.  If the entry exists in `t` and `f` returns None, the element
is removed.  If the entry exists in `t` and `f` returns `(Some elt)`, `elt`
replaces the original entry.

It is important that if `f` returns `(Some elt)`, `elt` must be still greater
than the 'previous' element and less than the 'next' element in the tree,
otherwise the returned tree would be inconsistent.
If you use an ordtree to keep a set of keys, you don't really need to alter
the existing entry.  It is useful if you define your own element type that
carries extra info, though; see OrdMap implementation."
    (let ((unchanged? (fn (a b)
                        (lisp Boolean (a b)
                          (cl:eq a b))))
          (walk (fn (n)
                  (match n
                    ((Empty)
                     (match (f None)
                       ((Tuple (None) aux) (Tuple n aux))
                       ((Tuple (Some x) aux)
                        (Tuple (L2 x) aux)))) ; insert
                    ((N1 t1)
                     (let (Tuple t2 aux) = (walk t1))
                     (if (unchanged? t1 t2)
                         (Tuple n aux)
                         (Tuple (make-n1 t2) aux)))
                    ((N2 l b r)
                     (match (<=> a b)
                       ((LT) (let (Tuple ll aux) = (walk l))
                             (if (unchanged? l ll)
                                 (Tuple n aux)
                                 (Tuple (make-n2 ll b r) aux)))
                       ((EQ) (match (f (Some b))
                               ((Tuple (None) aux) ; delete
                                (match (split-min r)
                                  ((None) (Tuple (N1 l) aux))
                                  ((Some (Tuple a1 r1))
                                   (Tuple (make-n2 l a1 r1) aux))))
                               ((Tuple (Some b2) aux) ;replace
                                ;; NB: `f` must guarantee that the new element
                                ;; still falls between the previous and next
                                ;; element in the tree.
                                ;; TODO: Should we check the conditon of
                                ;; returned element of `f`?  It will be
                                ;; expensive.  One idea is to check it in
                                ;; development mode, but not in release mode.
                                (if (unchanged? b b2)
                                    (Tuple n aux)
                                    (Tuple (N2 l b2 r) aux)))))
                       ((GT) (let (Tuple rr aux) = (walk r))
                             (if (unchanged? r rr)
                                 (Tuple n aux)
                                 (Tuple (make-n2 l b rr) aux)))))
                    (_ (stray-node))))))
      (let (Tuple t2 aux) = (walk t))
      (Tuple (make-root t2) aux)))

  (declare transform-elements ((:a -> :b) -> OrdTree :a -> OrdTree :b))
  (define (transform-elements f tre)
    "Returns a tree whose element consists of the result of `f` applied to
the original element, and isomorphic to the original tree.

It is important that transforming keys with `f` does not change the order
of the element.  If `f` violates the condition, the resulting tree isn't
guaranteed to be consistent.

We do not name this `map` because of this restriction."
    (match tre
      ((Empty) Empty)
      ((N1 t) (transform-elements f t))
      ((N2 l e r) (N2 (transform-elements f l) (f e) (transform-elements f r)))
      (_ (stray-node))))

  ;; Note: We repurpose L2 node to keep element in the stack
  (declare increasing-order (OrdTree :elt -> iter:Iterator :elt))
  (define (increasing-order tre)
    "Returns an iterator that traverses elements in `tre` in increasing order.
This is same as (iter:into-iter tre)."
    (let ((stack (cell:new (Cons tre Nil)))
          (next! (fn ()
                   (match (cell:pop! stack)
                     ((None) None)
                     ((Some (Empty)) (next!))
                     ((Some (N1 t)) (cell:push! stack t) (next!))
                     ((Some (N2 l e r))
                      (cell:push! stack r)
                      (cell:push! stack (L2 e))
                      (cell:push! stack l)
                      (next!))
                     ((Some (L2 e)) (Some e))
                     (_ (stray-node))))))
      (iter:new next!)))

  (declare decreasing-order (OrdTree :elt -> iter:Iterator :elt))
  (define (decreasing-order tre)
    "Returns an iterator that traverses elements in `tre` in decreasing order."
    (let ((stack (cell:new (Cons tre Nil)))
          (next! (fn ()
                   (match (cell:pop! stack)
                     ((None) None)
                     ((Some (Empty)) (next!))
                     ((Some (N1 t)) (cell:push! stack t) (next!))
                     ((Some (N2 l e r))
                      (cell:push! stack l)
                      (cell:push! stack (L2 e))
                      (cell:push! stack r)
                      (next!))
                     ((Some (L2 e)) (Some e))
                     (_ (stray-node))))))
      (iter:new next!)))
  )

;;
;; Neighborhood
;;

(coalton-toplevel
  ;; API
  (declare max-element (Ord :elt => OrdTree :elt -> Optional :elt))
  (define (max-element tre)
    "Returns the maximum element in the tree, or None if the tree is empty."
    (match tre
      ((Empty) None)
      ((N1 t) (max-element t))
      ((N2 _ elt r) (match (max-element r)
                      ((None) (Some elt))
                      (e e)))
      (_ (stray-node))))

  ;; API
  (declare min-element (Ord :elt => OrdTree :elt -> Optional :elt))
  (define (min-element tre)
    "Returns the minimum element in the tree, or None if the tree is empty."
    (match tre
      ((Empty) None)
      ((N1 t) (max-element t))
      ((N2 l elt _) (match (min-element l)
                      ((None) (Some elt))
                      (e e)))
      (_ (stray-node))))

  ;; API
  (declare lookup-neighbors (Ord :elt => OrdTree :elt -> :elt
                                 -> (Tuple3 (Optional :elt)
                                            (Optional :elt)
                                            (Optional :elt))))
  (define (lookup-neighbors haystack needle)
    "Returns elements LO, ON, and HI, such that LO is the closest
element that is strictly less than `needle`, ON is the element
that is `==` to `needle`, and HI is the closest element that is
strictly greater than `needle`.  Any of these values can be None
if there's no such element."
    (rec lup ((tree haystack)
              (lo None)
              (hi None))
      (match tree
        ((Empty) (Tuple3 lo None hi))
        ((N1 t) (lup t lo hi))
        ((N2 left elt right)
         (match (<=> needle elt)
           ((LT) (lup left lo (Some elt)))
           ((EQ) (let ((lo1 (match (max-element left)
                              ((None) lo)
                              (e e)))
                       (hi1 (match (min-element right)
                              ((None) hi)
                              (e e))))
                   (Tuple3 lo1 (Some elt) hi1)))
           ((GT) (lup right (Some elt) hi))))
        (_ (stray-node)))))
  )

;;
;; Set operations
;;

(coalton-toplevel
  (declare union (Ord :elt => OrdTree :elt -> OrdTree :elt -> OrdTree :elt))
  (define (union a b)
    "Returns an OrdTree that contains all the elements from `a` and `b`.
If both OrdTrees has the same (`==`) element, the one from `a` is taken."
    (iter:fold! adjoin a (increasing-order b)))

  (declare intersection (Ord :elt => OrdTree :elt -> OrdTree :elt -> OrdTree :elt))
  (define (intersection a b)
    "Returns an OrdTree that contains elements that appear in both `a` and `b`.
The resulting elements are from `a`."
    ;; TODO: This can be more efficient by traversing both trees in the
    ;; same order and selecting common elements.
    (iter:fold! (fn (m k)
                  (match (lookup b k)
                    ((None) m)
                    ((Some _) (insert m k))))
                Empty (increasing-order a)))

  (declare difference (Ord :elt => OrdTree :elt -> OrdTree :elt -> OrdTree :elt))
  (define (difference a b)
    "Returns an OrdTree that contains elements in `a` but not in `b`."
    (iter:fold! remove a (increasing-order b)))

  (declare xor (Ord :elt => OrdTree :elt -> OrdTree :elt -> OrdTree :elt))
  (define (xor a b)
    "Rdturns an OrdTree that contains elements either in `a` or in `b`,
but not in both."
    (iter:fold! (fn (m k)
                  (fst (update m k
                               (fn (e)
                                 (match e
                                   ((None) (Tuple (Some k) Unit))
                                   ((Some _) (Tuple None Unit)))))))
                Empty (iter:chain! (increasing-order a)
                                   (increasing-order b))))
  )

;;
;; Instances
;;

(coalton-toplevel

  (define-instance (iter:IntoIterator (OrdTree :elt) :elt)
    (define iter:into-iter increasing-order))

  (define-instance (Ord :elt => iter:FromIterator (OrdTree :elt) :elt)
    (define (iter:collect! iter)
      (iter:fold! insert empty iter)))

  (define-instance (Eq :elt => Eq (OrdTree :elt))
    (define (== ta tb)
      (iter:elementwise==! (iter:into-iter ta) (iter:into-iter tb))))

  (define-instance (Hash :elt => Hash (OrdTree :elt))
    (define (hash t)
      (iter:elementwise-hash! (iter:into-iter t))))

  (define-instance (Foldable OrdTree)
    (define (fold f seed t)
      (iter:fold! f seed (increasing-order t)))
    (define (foldr f seed t)
      (iter:fold! (flip f) seed (decreasing-order t))))
  )
