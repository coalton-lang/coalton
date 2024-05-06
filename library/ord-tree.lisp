(coalton-library/utils:defstdlib-package :coalton-library/ord-tree
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/hash
   #:coalton-library/tuple
   #:coalton-library/functions)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:cell #:coalton-library/cell))
  (:shadow #:empty)
  (:export
   #:Tree #:Empty
   #:lookup
   #:insert
   #:replace
   #:replace-or-insert
   #:insert-or-replace
   #:remove
   #:increasing-order
   #:decreasing-order
   #:collect!
   #:merge
   #:make))

(in-package :coalton-library/ord-tree)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;; adapted from https://matt.might.net/articles/red-black-delete/

(coalton-toplevel
  ;; the red-black tree invariants:
  ;; - the left child of a node is less than the node, and the right child is greater
  ;; - a red node has no red children
  ;; - a leaf is black
  ;; - every path from the root to a leaf passes through the same number of black nodes

  ;; unexported; a marker held by trees to enable self-balancing.
  (repr :enum)
  (define-type Color
    ;; has no red children
    Red

    ;; may have either red or black children
    Black

    ;; intermediate states during deletion; will never exist outside of a `remove' operation
    DoubleBlack
    NegativeBlack)

  (define-instance (Eq Color)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Red) (Red)) True)
        ((Tuple (Black) (Black)) True)
        ((Tuple (DoubleBlack) (DoubleBlack)) True)
        ((Tuple (NegativeBlack) (NegativeBlack)) True)
        (_ False))))

  (declare color-plus-black (Color -> Color))
  (define (color-plus-black c)
    (match c
      ((DoubleBlack) (error "cannot add black to double-black"))
      ((Black) DoubleBlack)
      ((Red) Black)
      ((NegativeBlack) Red)))

  (declare color-minus-black (Color -> Color))
  (define (color-minus-black c)
    (match c
      ((DoubleBlack) Black)
      ((Black) Red)
      ((Red) NegativeBlack)
      ((NegativeBlack) (error "cannot subtract black from negative-black"))))

  (define-type (Tree :elt)
    "A red-black balanced binary tree, sorted by `<=>` and unique by `==`."
    ;; exported; an empty tree.
    ;; considered black for the purpose of the invariants.
    Empty

    ;; unexported; a tree with at least one element, and possibly children.
    ;; (Branch clr less elt right)
    ;; every element of LESS is less than ELT, and every element of RIGHT is greater than ELT.
    (Branch Color (Tree :elt) :elt (Tree :elt))

    ;; unexported: a double-black leaf node. intermediate stage during deletion; will never exist outside of a
    ;; `remove' operation.
    DoubleBlackEmpty
    )

  ;;; color operations

  (declare tree-plus-black ((Tree :elt) -> (Tree :elt)))
  (define (tree-plus-black tre)
    (match tre
      ((Empty) DoubleBlackEmpty)
      ((Branch c left pivot right) (Branch (color-plus-black c) left pivot right))
      ((DoubleBlackEmpty) (error "cannot add black to double-black empty tree"))))

  (declare tree-minus-black ((Tree :elt) -> (Tree :elt)))
  (define (tree-minus-black tre)
    (match tre
      ((Empty) (error "cannot subtract black from empty tree"))
      ((Branch c left pivot right) (Branch (color-minus-black c) left pivot right))
      ((DoubleBlackEmpty) Empty)))

  (declare as-black ((Tree :elt) -> (Tree :elt)))
  (define (as-black tre)
    (match tre
      ((Empty) Empty)
      ((DoubleBlackEmpty) Empty)
      ((Branch _ left elt right) (Branch Black left elt right))))

  (declare tree-double-black? ((Tree :elt) -> Boolean))
  (define (tree-double-black? tre)
    (match tre
      ((Branch clr _ _ _) (== clr DoubleBlack))
      ((Empty) False)
      ((DoubleBlackEmpty) True)))

  ;;; searching trees

  (declare lookup ((Ord :elt) => ((Tree :elt) -> :elt -> (Optional :elt))))
  (define (lookup haystack needle)
    "If HAYSTACK contains an element `==` to NEEDLE, return it."
    (match haystack
      ((Empty) None)
      ((Branch _ left elt right)
       (match (<=> needle elt)
         ((LT) (lookup left needle))
         ((EQ) (Some elt))
         ((GT) (lookup right needle))))
      ((DoubleBlackEmpty) (error "Found double-black node outside of removal process"))))

  ;;; inserting into and replacing elements of trees

  (declare balance (Color -> (Tree :elt) -> :elt -> (Tree :elt) -> (Tree :elt)))
  (define (balance clr left elt right)
    (match (Branch clr left elt right)
      ;; cases for insertion violations
      ((Branch (Black)
               (Branch (Red)
                       (Branch (Red) a x b)
                       y
                       c)
               z
               d)
       (Branch Red
               (Branch Black a x b)
               y
               (Branch Black c z d)))
      ((Branch (Black)
               (Branch (Red)
                       a
                       x
                       (Branch (Red) b y c))
               z
               d)
       (Branch Red
               (Branch Black a x b)
               y
               (Branch Black c z d)))
      ((Branch (Black)
               a
               x
               (Branch (Red)
                       (Branch (Red)
                               b
                               y
                               c)
                       z
                       d))
       (Branch Red
               (Branch Black a x b)
               y
               (Branch Black c z d)))
      ((Branch (Black)
               a
               x
               (Branch (Red)
                       b
                       y
                       (Branch (Red)
                               c
                               z
                               d)))
       (Branch Red
               (Branch Black a x b)
               y
               (Branch Black c z d)))

      ;; duplicates of above cases with black swapped for double-black
      ((Branch (DoubleBlack)
               (Branch (Red)
                       (Branch (Red) a x b)
                       y
                       c)
               z
               d)
       (Branch Black
               (Branch Black a x b)
               y
               (Branch Black c z d)))
      ((Branch (DoubleBlack)
               (Branch (Red)
                       a
                       x
                       (Branch (Red) b y c))
               z
               d)
       (Branch Black
               (Branch Black a x b)
               y
               (Branch Black c z d)))
      ((Branch (DoubleBlack)
               a
               x
               (Branch (Red)
                       (Branch (Red)
                               b
                               y
                               c)
                       z
                       d))
       (Branch Black
               (Branch Black a x b)
               y
               (Branch Black c z d)))
      ((Branch (DoubleBlack)
               a
               x
               (Branch (Red)
                       b
                       y
                       (Branch (Red)
                               c
                               z
                               d)))
       (Branch Black
               (Branch Black a x b)
               y
               (Branch Black c z d)))

      ;; cases for removal violations with double-blacks and/or negative-blacks
      ((Branch (DoubleBlack)
               a
               x
               (Branch (NegativeBlack)
                       (Branch (Black) b y c)
                       z
                       (Branch (Black) d-left d-value d-right)))
       (Branch Black
               (Branch Black a x b)
               y
               (balance Black c z (Branch Red d-left d-value d-right))))

      ((Branch (DoubleBlack)
               (Branch (NegativeBlack)
                       (Branch (Black) a-left a-value a-right)
                       x
                       (Branch (Black) b y c))
               z
               d)
       (Branch Black
               (balance Black
                        (Branch Red a-left a-value a-right)
                        x
                        b)
               y
               (Branch Black c z d)))

      (tre tre)))

  (declare insert ((Ord :elt) => ((Tree :elt) -> :elt -> (Optional (Tree :elt)))))
  (define (insert tre elt)
    "Construct a new Tree like TRE but containing ELT. If TRE already had an element `==` to ELT, return None."
    (let ((ins (fn (subtree)
                 (match subtree
                   ((Empty) (Some (Branch Red Empty elt Empty)))
                   ((Branch clr left pivot right)
                    (match (<=> elt pivot)
                      ((LT) (map (fn (new-left) (balance clr new-left pivot right))
                                 (ins left)))
                      ((EQ) None)
                      ((GT) (map (fn (new-right) (balance clr left pivot new-right))
                                 (ins right)))))
                   ((DoubleBlackEmpty) (error "Found double-black node outside of removal process"))))))
      (ins tre)))

  (declare replace ((Ord :elt) => ((Tree :elt) -> :elt -> (Optional (Tuple (Tree :elt) :elt)))))
  (define (replace tre elt)
    "Construct a new Tree like TRE but with ELT replacing an old element `==` to ELT.

Return the new tree and the removed element.

If TRE did not have an element `==' to ELT, return None."
    (let ((ins (fn (subtree)
                 (match subtree
                   ((Empty) None)
                   ((Branch clr left pivot right)
                    (match (<=> elt pivot)
                      ((LT) (map (uncurry (fn (new-left removed-elt) (Tuple (balance clr new-left pivot right) removed-elt)))
                                 (ins left)))
                      ((EQ) (Some (Tuple (Branch clr left elt right)
                                         pivot)))
                      ((GT) (map (uncurry (fn (new-right removed-elt) (Tuple (balance clr left pivot new-right) removed-elt)))
                                 (ins right)))))
                   ((DoubleBlackEmpty) (error "Found double-black node outside of removal process"))))))
      (ins tre)))

  (declare replace-or-insert ((Ord :elt) => ((Tree :elt) -> :elt -> (Tuple (Tree :elt) (Optional :elt)))))
  (define (replace-or-insert tre elt)
    "Construct a new Tree like TRE but containing ELT.

If TRE already had an element `==` to ELT, remove it, replace it with ELT, and return the removed value
alongside the new tree."
    (let ((ins (fn (subtree)
                 (match subtree
                   ((Empty) (Tuple (Branch Red Empty elt Empty)
                                   None))
                   ((Branch clr left pivot right)
                    (match (<=> elt pivot)
                      ((LT)
                       (let (Tuple new-left removed-elt) = (ins left))
                       (Tuple (balance clr new-left pivot right)
                              removed-elt))
                      ((EQ) (Tuple (Branch clr left elt right)
                                   (Some pivot)))
                      ((GT)
                       (let (Tuple new-right removed-elt) = (ins right))
                       (Tuple (balance clr left pivot new-right)
                              removed-elt))))
                   ((DoubleBlackEmpty) (error "Found double-black node outside of removal process"))))))
      (let (Tuple new-tree removed-elt) = (ins tre))
      (Tuple (as-black new-tree) removed-elt)))

  (declare insert-or-replace ((Ord :elt) => ((Tree :elt) -> :elt -> (Tree :elt))))
  (define (insert-or-replace tre elt)
    "Construct a new Tree like TRE but containing ELT.

If TRE already had an element `==` to ELT, remove it, replace it with ELT, and discard the removed value.

Like `replace-or-insert`, but prioritizing insertion as a use case."
    (fst (replace-or-insert tre elt)))

  ;;; removing from trees

  ;; matt might calls this operation `sorted-map-delete'
  (declare remove ((Ord :elt) => ((Tree :elt) -> :elt -> (Optional (Tree :elt)))))
  (define (remove tre elt)
    "Construct a new Tree like TRE but without an element `==' to ELT. Return None if TRE does not contain an element `==` to ELT."
    (map as-black
         (remove-without-as-black tre elt)))

  ;; matt might calls this operation `del'
  (declare remove-without-as-black ((Ord :elt) => ((Tree :elt) -> :elt -> (Optional (Tree :elt)))))
  (define (remove-without-as-black tre elt)
    (match tre
      ((Empty) None)
      ((Branch clr left pivot right)
       (match (<=> elt pivot)
         ((LT)
          (map (fn (new-left) (bubble-double-black clr new-left pivot right))
               (remove-without-as-black left elt)))
         ((EQ) (Some (remove-leaving-double-black tre)))
         ((GT)
          (map (fn (new-right) (bubble-double-black clr left pivot new-right))
               (remove-without-as-black right elt)))))
      ((DoubleBlackEmpty) (error "Encountered double-black node early in `remove` while searching for the node to remove."))))

  ;; matt might calls this operation `remove'
  (declare remove-leaving-double-black (((Tree :elt) -> (Tree :elt))))
  (define (remove-leaving-double-black tre)
    "Remove the pivot of TRE from TRE, fusing its left and right children to form a new tree.

The result tree may be in an intermediate state with a double-black node."
    (match tre
      ;; nodes with no subtrees
      ((Branch (Red) (Empty) _ (Empty)) Empty)
      ((Branch (Black) (Empty) _ (Empty)) DoubleBlackEmpty)

      ;; nodes with one subtree
      ((Branch (Black)
               (Empty)
               _
               (Branch child-clr child-left child-value child-right))
       (assert (== child-clr Red)
           "Black node with black leaf and black non-empty child violates red-black constraints.")
       (Branch Black child-left child-value child-right))
      ((Branch (Black)
               (Branch child-clr child-left child-value child-right)
               _
               (Empty))
       (assert (== child-clr Red)
           "Black node with black leaf and black non-empty child violates red-black constraints.")
       (Branch Black child-left child-value child-right))
      ;; all other one-child cases should be impossible because they violate red-black constraints

      ;; nodes with two subtrees
      ((Branch clr left _ right)
       (let (Tuple new-left new-pivot) = (remove-max left))
       (bubble-double-black clr new-left new-pivot right))

      ((DoubleBlackEmpty) (error "Encountered double-black node early in `remove` while searching for the node to remove."))
      ((Empty) (error "Attempt to `remove-leaving-double-black` on an empty tree."))))

  ;; matt might calls this operation `bubble'
  (declare bubble-double-black ((Color -> (Tree :elt) -> :elt -> (Tree :elt) -> (Tree :elt))))
  (define (bubble-double-black clr left pivot right)
    (if (or (tree-double-black? left) (tree-double-black? right))
        (balance (color-plus-black clr) (tree-minus-black left) pivot (tree-minus-black right))
        (Branch clr left pivot right)))

  ;; matt might has this in two operations, `remove-max' and `sorted-map-max'
  (declare remove-max ((Tree :elt) -> (Tuple (Tree :elt) :elt)))
  (define (remove-max tre)
    (match tre
      ((Branch _ _ pivot (Empty)) (Tuple (remove-leaving-double-black tre) pivot))
      ((Branch clr left pivot right)
       (let (Tuple new-right removed-max) = (remove-max right))
       (Tuple (bubble-double-black clr left pivot new-right)
              removed-max))
      ((DoubleBlackEmpty) (error "Encountered double-black node early in `remove` while searching for the node to remove."))
      ((Empty) (error "Attempt to `remove-max` on an empty tree."))))

  ;;; iterating through trees

  (define-type (IteratorStackNode :elt)
    (Yield :elt)
    (Expand (Tree :elt)))

  (declare tree-iterator (((cell:Cell (List (IteratorStackNode :elt))) -> (Tree :elt) -> Unit)
                          -> (Tree :elt)
                          -> (iter:Iterator :elt)))
  (define (tree-iterator add-to-stack tre)
    (let ((stack (cell:new (make-list (Expand tre))))
          (next!
            (fn ()
              (match (cell:pop! stack)
                ((None) None)
                ((Some (Yield elt)) (Some elt))
                ((Some (Expand node)) (add-to-stack stack node) (next!))))))
      (iter:new next!)))

  (declare stack-for-increasing-order-traversal ((cell:Cell (List (IteratorStackNode :elt))) -> (Tree :elt) -> Unit))
  (define (stack-for-increasing-order-traversal stack node)
    (match node
      ((Empty) Unit)
      ((Branch _ less elt more)
       (cell:push! stack (Expand more))
       (cell:push! stack (Yield elt))
       (cell:push! stack (Expand less))
       Unit)
      ((DoubleBlackEmpty) (error "Found double-black node outside of removal process"))))

  (declare increasing-order ((Tree :elt) -> (iter:Iterator :elt)))
  (define increasing-order
    "Iterate the elements of a tree, starting with the least by `<=>' and ending with the greatest."
    (tree-iterator stack-for-increasing-order-traversal))

  (declare stack-for-decreasing-order-traversal ((cell:Cell (List (IteratorStackNode :elt))) -> (Tree :elt) -> Unit))
  (define (stack-for-decreasing-order-traversal stack node)
    (match node
      ((Empty) Unit)
      ((Branch _ less elt more)
       (cell:push! stack (Expand less))
       (cell:push! stack (Yield elt))
       (cell:push! stack (Expand more))
       Unit)
      ((DoubleBlackEmpty) (error "Found double-black node outside of removal process"))))

  (declare decreasing-order ((Tree :elt) -> (iter:Iterator :elt)))
  (define decreasing-order
    "Iterate the elements of a tree, starting with the greatest by `<=>' and ending with the least."
    (tree-iterator stack-for-decreasing-order-traversal))

  (define-instance (iter:IntoIterator (Tree :elt) :elt)
    (define iter:into-iter increasing-order))

  (define-instance ((Eq :elt) => Eq (Tree :elt))
    (define (== left right)
      (iter:elementwise==! (increasing-order left) (increasing-order right))))

  (define-instance ((Hash :elt) => Hash (Tree :elt))
    (define (hash tre)
      (iter:elementwise-hash! (increasing-order tre))))

  (declare collect! ((Ord :elt) => (iter:Iterator :elt) -> (Tree :elt)))
  (define (collect! iter)
    "Construct a Tree containing all the elements of ITER.

If ITER contains duplicates, later elements will overwrite earlier elements."
    (iter:fold! insert-or-replace Empty iter))

  (define-instance (Ord :elt => iter:FromIterator (Tree :elt) :elt)
    (define iter:collect! collect!))

  (declare merge (Ord :elt => Tree :elt -> Tree :elt -> Tree :elt))
  (define (merge a b)
    "Construct a Tree containing all the elements of both A and B.

If A and B contain elements A' and B' respectively where (== A' B'), the result will contain either A' or
B'. Which one is chosen for the result is undefined."
    (iter:fold! insert-or-replace
                a
                (increasing-order b)))

  (define-instance (Ord :elt => Semigroup (Tree :elt))
    (define <> merge))

  (define-instance (Ord :elt => Monoid (Tree :elt))
    (define mempty Empty))

  (define-instance (Foldable Tree)
    (define (fold func init tre)
      (iter:fold! func init (increasing-order tre)))
    (define (foldr func init tre)
      (iter:fold! (flip func) init (decreasing-order tre))))

  ;; We possibly should have an instance (Traversable Tree). Open questions:
  ;; - Is it necessary (or desirable) that `traverse' walk the tree in the same order as `fold',
  ;;   i.e. left-to-right? My gut says yes.
  ;; - Is it possible to write a better instance than one which converts the tree to an iterator using
  ;;   `increasing-order', then traverses it via `liftA2' of `insert-or-replace'? My gut says no, given the
  ;;   previous point.
  )

(cl:defmacro make (cl:&rest elements)
  "Construct a tree containing the ELEMENTS.

e.g. (tree:make 5 6 1 8 9) => tree containing 1, 5, 6, 8, 9."
  `(collect! (iter:into-iter (make-list ,@elements))))
