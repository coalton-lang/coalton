;;;; avl-tree.lisp — Persistent (purely functional) AVL tree.
;;;;
;;;; Used as the backing store for Coalton's immutable-map and
;;;; immutable-listmap.  Replaces FSet's wb-map.
;;;;
;;;; Invariants at every node:
;;;;   1. BST: all keys in left < key < all keys in right
;;;;   2. AVL: |height(left) - height(right)| <= 1
;;;;   3. Size: count = 1 + count(left) + count(right)
;;;;
;;;; Keys may be symbols or fixnums.  Symbols are ordered first by
;;;; package name, then by symbol name; uninterned symbols (nil package)
;;;; sort before all interned ones.  Fixnums sort before all symbols.

(defpackage #:coalton-impl/algorithm/avl-tree
  (:use #:cl)
  (:export
   #:avl-tree                           ; TYPE
   #:avl-node                           ; STRUCT
   #:avl-node-p                         ; PREDICATE
   #:empty-avl-tree                     ; FUNCTION

   #:avl-lookup                         ; FUNCTION
   #:avl-set                            ; FUNCTION
   #:avl-remove                         ; FUNCTION
   #:avl-count                          ; FUNCTION
   #:avl-foreach                        ; FUNCTION
   #:avl-keys                           ; FUNCTION
   #:avl-values                         ; FUNCTION
   #:avl-to-alist                       ; FUNCTION
   #:avl-image                          ; FUNCTION
   #:avl-difference                     ; FUNCTION
   #:avl-from-alist                     ; FUNCTION

   #:do-avl                             ; MACRO

   #:make-avl-map                       ; MACRO
   ))

(in-package #:coalton-impl/algorithm/avl-tree)


;;;; Key ordering

(defun symbol< (a b)
  "Total order on symbols by package name then symbol name."
  (if (eq a b)
      nil
      (let ((pa (symbol-package a))
            (pb (symbol-package b)))
        (cond ((and (null pa) (null pb))
               (string< (symbol-name a) (symbol-name b)))
              ((null pa) t)
              ((null pb) nil)
              (t (let ((pna (package-name pa))
                       (pnb (package-name pb)))
                   (cond ((string< pna pnb) t)
                         ((string= pna pnb)
                          (string< (symbol-name a) (symbol-name b)))
                         (t nil))))))))

(defun type-rank (x)
  "Integer ranking for the type of X, used to order values of different types."
  (etypecase x
    (fixnum  0)
    (cons    1)
    (symbol  2)
    (string  3)))

(defun key< (a b)
  "Total order on map keys.
Supported types: fixnum, cons, symbol, string.  Values of different
types are ordered by type rank.  Within a type: fixnums by <, symbols
by package then name, conses lexicographically, strings by string<."
  (let ((ra (type-rank a))
        (rb (type-rank b)))
    (cond ((/= ra rb) (< ra rb))
          ((typep a 'fixnum) (< a b))
          ((typep a 'symbol) (symbol< a b))
          ((typep a 'cons)
           (or (key< (car a) (car b))
               (and (not (key< (car b) (car a)))
                    (key< (cdr a) (cdr b)))))
          ((typep a 'string) (string< a b))
          (t nil))))


;;;; Empty tree sentinel and type

(deftype avl-tree ()
  "An AVL tree: either an avl-node or the empty tree sentinel."
  '(or avl-node (member empty-avl-tree)))

(declaim (inline empty-avl-tree))
(defun empty-avl-tree ()
  "Return the empty AVL tree."
  'empty-avl-tree)


;;;; Node structure

(defstruct (avl-node
            (:constructor %make-node (key value left right height count))
            (:copier nil))
  "A node in a persistent AVL tree."
  (key    nil                            :read-only t)
  (value  nil                            :read-only t)
  (left   'empty-avl-tree :type avl-tree :read-only t)
  (right  'empty-avl-tree :type avl-tree :read-only t)
  (height 1                :type fixnum  :read-only t)
  (count  1                :type fixnum  :read-only t))

#+sbcl (declaim (sb-ext:freeze-type avl-node))

(defmethod make-load-form ((self avl-node) &optional env)
  (make-load-form-saving-slots self :environment env))


;;;; Height and count accessors for nullable nodes

(declaim (inline node-height node-count))

(defun node-height (node)
  (if (avl-node-p node) (avl-node-height node) 0))

(defun node-count (node)
  (if (avl-node-p node) (avl-node-count node) 0))


;;;; Node constructor (computes height and count)

(declaim (inline make-node))
(defun make-node (key value left right)
  (%make-node key value left right
              (1+ (max (node-height left) (node-height right)))
              (+ 1 (node-count left) (node-count right))))


;;;; Rotations

(defun rotate-right (node)
  "Single right rotation.  Assumes (avl-node-left node) is non-nil."
  (let ((l (avl-node-left node)))
    (make-node (avl-node-key l) (avl-node-value l)
               (avl-node-left l)
               (make-node (avl-node-key node) (avl-node-value node)
                          (avl-node-right l)
                          (avl-node-right node)))))

(defun rotate-left (node)
  "Single left rotation.  Assumes (avl-node-right node) is non-nil."
  (let ((r (avl-node-right node)))
    (make-node (avl-node-key r) (avl-node-value r)
               (make-node (avl-node-key node) (avl-node-value node)
                          (avl-node-left node)
                          (avl-node-left r))
               (avl-node-right r))))


;;;; Rebalancing

(defun balance (key value left right)
  "Construct a node and rebalance if needed.
Assumes LEFT and RIGHT individually satisfy AVL, and their heights
differ by at most 2 (the situation after a single insert or remove)."
  (let ((hl (node-height left))
        (hr (node-height right)))
    (cond
      ;; Left-heavy
      ((> hl (+ hr 1))
       (if (>= (node-height (avl-node-left left))
               (node-height (avl-node-right left)))
           ;; Left-left: single right rotation
           (rotate-right (make-node key value left right))
           ;; Left-right: double rotation
           (rotate-right (make-node key value
                                    (rotate-left left)
                                    right))))
      ;; Right-heavy
      ((> hr (+ hl 1))
       (if (>= (node-height (avl-node-right right))
               (node-height (avl-node-left right)))
           ;; Right-right: single left rotation
           (rotate-left (make-node key value left right))
           ;; Right-left: double rotation
           (rotate-left (make-node key value
                                   left
                                   (rotate-right right)))))
      ;; Balanced
      (t (make-node key value left right)))))


;;;; Lookup

(defun avl-lookup (tree key)
  "Look up KEY in TREE.  Returns (values value found-p)."
  (declare (type avl-tree tree))
  (if (not (avl-node-p tree))
      (values nil nil)
      (cond ((key< key (avl-node-key tree))
             (avl-lookup (avl-node-left tree) key))
            ((key< (avl-node-key tree) key)
             (avl-lookup (avl-node-right tree) key))
            (t
             (values (avl-node-value tree) t)))))


;;;; Insert / update

(defun avl-set (tree key value)
  "Return a new tree like TREE but with KEY mapped to VALUE.
If KEY is already present, its value is replaced."
  (declare (type avl-tree tree))
  (if (not (avl-node-p tree))
      (make-node key value (empty-avl-tree) (empty-avl-tree))
      (cond ((key< key (avl-node-key tree))
             (balance (avl-node-key tree) (avl-node-value tree)
                      (avl-set (avl-node-left tree) key value)
                      (avl-node-right tree)))
            ((key< (avl-node-key tree) key)
             (balance (avl-node-key tree) (avl-node-value tree)
                      (avl-node-left tree)
                      (avl-set (avl-node-right tree) key value)))
            (t
             ;; Key matches — replace value (no structural change).
             (if (eq value (avl-node-value tree))
                 tree
                 (make-node key value
                            (avl-node-left tree)
                            (avl-node-right tree)))))))


;;;; Remove

(defun node-min-key (tree)
  "Return the minimum key in a non-empty tree."
  (if (avl-node-p (avl-node-left tree))
      (node-min-key (avl-node-left tree))
      (avl-node-key tree)))

(defun node-min-value (tree)
  "Return the value associated with the minimum key."
  (if (avl-node-p (avl-node-left tree))
      (node-min-value (avl-node-left tree))
      (avl-node-value tree)))

(defun remove-min (tree)
  "Remove the minimum element.  Assumes TREE is a non-empty avl-node."
  (if (avl-node-p (avl-node-left tree))
      (balance (avl-node-key tree) (avl-node-value tree)
               (remove-min (avl-node-left tree))
               (avl-node-right tree))
      (avl-node-right tree)))

(defun avl-remove (tree key)
  "Return a new tree like TREE but without KEY."
  (declare (type avl-tree tree))
  (if (not (avl-node-p tree))
      (empty-avl-tree)
      (cond ((key< key (avl-node-key tree))
             (balance (avl-node-key tree) (avl-node-value tree)
                      (avl-remove (avl-node-left tree) key)
                      (avl-node-right tree)))
            ((key< (avl-node-key tree) key)
             (balance (avl-node-key tree) (avl-node-value tree)
                      (avl-node-left tree)
                      (avl-remove (avl-node-right tree) key)))
            (t
             ;; Found: merge left and right subtrees.
             (let ((left (avl-node-left tree))
                   (right (avl-node-right tree)))
               (cond ((not (avl-node-p left))  right)
                     ((not (avl-node-p right)) left)
                     (t (balance (node-min-key right)
                                 (node-min-value right)
                                 left
                                 (remove-min right)))))))))


;;;; Count

(declaim (inline avl-count))
(defun avl-count (tree)
  "Return the number of entries in TREE."
  (declare (type avl-tree tree))
  (node-count tree))


;;;; Iteration

(defun avl-foreach (fn tree)
  "Call (funcall FN key value) for each entry in TREE, in ascending key order."
  (declare (type function fn)
           (type avl-tree tree))
  (when (avl-node-p tree)
    (avl-foreach fn (avl-node-left tree))
    (funcall fn (avl-node-key tree) (avl-node-value tree))
    (avl-foreach fn (avl-node-right tree)))
  (values))

(defmacro do-avl ((key value tree &optional result) &body body)
  "Iterate over TREE in ascending key order, binding KEY and VALUE.
Supports RETURN-FROM in the body."
  (let ((walk (gensym "WALK"))
        (node (gensym "NODE")))
    `(block nil
       (labels ((,walk (,node)
                  (when (avl-node-p ,node)
                    (,walk (avl-node-left ,node))
                    (let ((,key (avl-node-key ,node))
                          (,value (avl-node-value ,node)))
                      ,@body)
                    (,walk (avl-node-right ,node)))))
         (,walk ,tree)
         ,result))))


;;;; Collecting

(defun avl-keys (tree)
  "Return a list of all keys in TREE in ascending order."
  (declare (type avl-tree tree))
  (let ((acc nil))
    (avl-foreach (lambda (k v) (declare (ignore v)) (push k acc)) tree)
    (nreverse acc)))

(defun avl-values (tree)
  "Return a list of all values in TREE in ascending key order."
  (declare (type avl-tree tree))
  (let ((acc nil))
    (avl-foreach (lambda (k v) (declare (ignore k)) (push v acc)) tree)
    (nreverse acc)))

(defun avl-to-alist (tree)
  "Return an alist of (KEY . VALUE) pairs in ascending key order."
  (declare (type avl-tree tree))
  (let ((acc nil))
    (avl-foreach (lambda (k v) (push (cons k v) acc)) tree)
    (nreverse acc)))


;;;; Image (map over values)

(defun avl-image (fn tree)
  "Return a new tree with each value replaced by (funcall FN value).
Preserves tree structure (keys and balance unchanged)."
  (declare (type function fn)
           (type avl-tree tree))
  (if (not (avl-node-p tree))
      (empty-avl-tree)
      (%make-node (avl-node-key tree)
                  (funcall fn (avl-node-value tree))
                  (avl-image fn (avl-node-left tree))
                  (avl-image fn (avl-node-right tree))
                  (avl-node-height tree)
                  (avl-node-count tree))))


;;;; Difference

(defun avl-difference (tree1 tree2)
  "Return a tree containing entries from TREE1 whose keys are absent in TREE2."
  (declare (type avl-tree tree1 tree2))
  (if (or (not (avl-node-p tree1)) (not (avl-node-p tree2)))
      tree1
      (let ((result (empty-avl-tree)))
        (avl-foreach (lambda (k v)
                       (multiple-value-bind (v2 found) (avl-lookup tree2 k)
                         (declare (ignore v2))
                         (unless found
                           (setf result (avl-set result k v)))))
                     tree1)
        result)))


;;;; Construction from alist

(defun avl-from-alist (alist)
  "Build a tree from an alist of (KEY . VALUE) pairs."
  (let ((tree (empty-avl-tree)))
    (dolist (pair alist tree)
      (setf tree (avl-set tree (car pair) (cdr pair))))))


;;;; Convenience macro for literal maps

(defmacro make-avl-map (&rest pairs)
  "Construct an AVL tree from literal (key-expr value-expr) pairs.
Usage: (make-avl-map ('foo val1) ('bar val2))"
  (let ((tree (gensym "TREE")))
    `(let ((,tree (empty-avl-tree)))
       ,@(loop :for (k v) :in pairs
               :collect `(setf ,tree (avl-set ,tree ,k ,v)))
       ,tree)))
