;;;; avl-tree-tests.lisp — Comprehensive tests for the persistent AVL tree.

(in-package #:coalton-tests)

;;;; Helpers

(defun avl-empty ()
  "Shorthand for the empty AVL tree in tests."
  (coalton-impl/algorithm/avl-tree:empty-avl-tree))

(defun avl-empty-p (tree)
  "Return T if TREE is the empty AVL tree."
  (not (coalton-impl/algorithm/avl-tree:avl-node-p tree)))

(defun avl-height (tree)
  "Return the height of TREE (0 for empty)."
  (if (coalton-impl/algorithm/avl-tree:avl-node-p tree)
      (coalton-impl/algorithm/avl-tree::avl-node-height tree)
      0))

(defun avl-balanced-p (tree)
  "Return T if TREE satisfies the AVL balance invariant at every node."
  (or (avl-empty-p tree)
      (let* ((l (coalton-impl/algorithm/avl-tree::avl-node-left tree))
             (r (coalton-impl/algorithm/avl-tree::avl-node-right tree))
             (bf (- (avl-height l) (avl-height r))))
        (and (<= -1 bf 1)
             (avl-balanced-p l)
             (avl-balanced-p r)))))

(defun avl-bst-p (tree &optional lo hi)
  "Return T if TREE satisfies the BST ordering invariant.
LO and HI are exclusive bounds (nil means unbounded)."
  (or (avl-empty-p tree)
      (let ((k (coalton-impl/algorithm/avl-tree::avl-node-key tree)))
        (and (or (null lo) (coalton-impl/algorithm/avl-tree::key< lo k))
             (or (null hi) (coalton-impl/algorithm/avl-tree::key< k hi))
             (avl-bst-p (coalton-impl/algorithm/avl-tree::avl-node-left tree) lo k)
             (avl-bst-p (coalton-impl/algorithm/avl-tree::avl-node-right tree) k hi)))))

(defun avl-count-correct-p (tree)
  "Return T if every node's count field equals 1 + count(left) + count(right)."
  (or (avl-empty-p tree)
      (let ((l (coalton-impl/algorithm/avl-tree::avl-node-left tree))
            (r (coalton-impl/algorithm/avl-tree::avl-node-right tree)))
        (and (= (coalton-impl/algorithm/avl-tree::avl-node-count tree)
                (+ 1
                   (coalton-impl/algorithm/avl-tree::node-count l)
                   (coalton-impl/algorithm/avl-tree::node-count r)))
             (avl-count-correct-p l)
             (avl-count-correct-p r)))))

(defun avl-height-correct-p (tree)
  "Return T if every node's height field equals 1 + max(height(left), height(right))."
  (or (avl-empty-p tree)
      (let ((l (coalton-impl/algorithm/avl-tree::avl-node-left tree))
            (r (coalton-impl/algorithm/avl-tree::avl-node-right tree)))
        (and (= (coalton-impl/algorithm/avl-tree::avl-node-height tree)
                (1+ (max (avl-height l) (avl-height r))))
             (avl-height-correct-p l)
             (avl-height-correct-p r)))))

(defun check-invariants (tree)
  "Assert all AVL tree invariants hold."
  (is (avl-balanced-p tree))
  (is (avl-bst-p tree))
  (is (avl-count-correct-p tree))
  (is (avl-height-correct-p tree)))

(defun build-tree (pairs)
  "Build an AVL tree from a list of (key . value) pairs."
  (let ((tree (avl-empty)))
    (dolist (p pairs tree)
      (setf tree (coalton-impl/algorithm/avl-tree:avl-set tree (car p) (cdr p))))))


;;;; Tests

(deftest test-avl-empty ()
  ;; Empty tree
  (let ((empty (avl-empty)))
    (is (avl-empty-p empty))
    (is (= 0 (coalton-impl/algorithm/avl-tree:avl-count empty)))
    (multiple-value-bind (val found)
        (coalton-impl/algorithm/avl-tree:avl-lookup empty 'x)
      (is (null val))
      (is (null found)))
    (is (null (coalton-impl/algorithm/avl-tree:avl-keys empty)))
    (is (null (coalton-impl/algorithm/avl-tree:avl-values empty)))
    (is (null (coalton-impl/algorithm/avl-tree:avl-to-alist empty)))
    ;; Remove from empty returns empty
    (is (avl-empty-p (coalton-impl/algorithm/avl-tree:avl-remove empty 'x)))))


(deftest test-avl-single-insert ()
  (let ((tree (coalton-impl/algorithm/avl-tree:avl-set (avl-empty) 'a 1)))
    (check-invariants tree)
    (is (= 1 (coalton-impl/algorithm/avl-tree:avl-count tree)))
    (multiple-value-bind (val found)
        (coalton-impl/algorithm/avl-tree:avl-lookup tree 'a)
      (is (= 1 val))
      (is found))
    ;; Key not present
    (multiple-value-bind (val found)
        (coalton-impl/algorithm/avl-tree:avl-lookup tree 'b)
      (is (null val))
      (is (null found)))))


(deftest test-avl-insert-multiple ()
  ;; Insert several keys and verify all are found
  (let* ((keys '(d b f a c e g))
         (tree (avl-empty)))
    (loop :for k :in keys
          :for i :from 1
          :do (setf tree (coalton-impl/algorithm/avl-tree:avl-set tree k i)))
    (check-invariants tree)
    (is (= 7 (coalton-impl/algorithm/avl-tree:avl-count tree)))
    (loop :for k :in keys
          :for i :from 1
          :do (multiple-value-bind (val found)
                  (coalton-impl/algorithm/avl-tree:avl-lookup tree k)
                (is found)
                (is (= i val))))))


(deftest test-avl-replace-value ()
  ;; Inserting the same key replaces the value
  (let* ((tree (coalton-impl/algorithm/avl-tree:avl-set (avl-empty) 'a 1))
         (tree2 (coalton-impl/algorithm/avl-tree:avl-set tree 'a 2)))
    (check-invariants tree2)
    (is (= 1 (coalton-impl/algorithm/avl-tree:avl-count tree2)))
    (multiple-value-bind (val found)
        (coalton-impl/algorithm/avl-tree:avl-lookup tree2 'a)
      (is (= 2 val))
      (is found))
    ;; Original tree is unchanged (persistence)
    (multiple-value-bind (val found)
        (coalton-impl/algorithm/avl-tree:avl-lookup tree 'a)
      (is (= 1 val))
      (is found))))


(deftest test-avl-eq-value-no-copy ()
  ;; When replacing with EQ-identical value, returns same tree node
  (let* ((val (list 1 2 3))
         (tree (coalton-impl/algorithm/avl-tree:avl-set (avl-empty) 'a val))
         (tree2 (coalton-impl/algorithm/avl-tree:avl-set tree 'a val)))
    (is (eq tree tree2))))


(deftest test-avl-remove-leaf ()
  (let* ((tree (build-tree '((a . 1) (b . 2) (c . 3))))
         (tree2 (coalton-impl/algorithm/avl-tree:avl-remove tree 'a)))
    (check-invariants tree2)
    (is (= 2 (coalton-impl/algorithm/avl-tree:avl-count tree2)))
    (multiple-value-bind (val found)
        (coalton-impl/algorithm/avl-tree:avl-lookup tree2 'a)
      (declare (ignore val))
      (is (null found)))
    ;; Other keys still present
    (is (nth-value 1 (coalton-impl/algorithm/avl-tree:avl-lookup tree2 'b)))
    (is (nth-value 1 (coalton-impl/algorithm/avl-tree:avl-lookup tree2 'c)))
    ;; Original unchanged
    (is (= 3 (coalton-impl/algorithm/avl-tree:avl-count tree)))))


(deftest test-avl-remove-root ()
  (let* ((tree (build-tree '((a . 1) (b . 2) (c . 3))))
         (tree2 (coalton-impl/algorithm/avl-tree:avl-remove tree 'b)))
    (check-invariants tree2)
    (is (= 2 (coalton-impl/algorithm/avl-tree:avl-count tree2)))
    (is (null (nth-value 1 (coalton-impl/algorithm/avl-tree:avl-lookup tree2 'b))))
    (is (nth-value 1 (coalton-impl/algorithm/avl-tree:avl-lookup tree2 'a)))
    (is (nth-value 1 (coalton-impl/algorithm/avl-tree:avl-lookup tree2 'c)))))


(deftest test-avl-remove-absent ()
  ;; Removing a nonexistent key returns unchanged tree
  (let* ((tree (build-tree '((a . 1) (b . 2))))
         (tree2 (coalton-impl/algorithm/avl-tree:avl-remove tree 'z)))
    (check-invariants tree2)
    (is (= 2 (coalton-impl/algorithm/avl-tree:avl-count tree2)))))


(deftest test-avl-remove-all ()
  ;; Insert and remove every key, tree should be empty
  (let ((keys '(d b f a c e g))
        (tree (avl-empty)))
    (loop :for k :in keys
          :for i :from 1
          :do (setf tree (coalton-impl/algorithm/avl-tree:avl-set tree k i)))
    (dolist (k keys)
      (setf tree (coalton-impl/algorithm/avl-tree:avl-remove tree k))
      (check-invariants tree))
    (is (avl-empty-p tree))))


(deftest test-avl-ascending-insert ()
  ;; Worst case for naive BST — ascending insertion.
  ;; AVL must remain balanced.
  (let ((tree (avl-empty)))
    (loop :for i :from 0 :below 100
          :do (setf tree (coalton-impl/algorithm/avl-tree:avl-set tree i i)))
    (check-invariants tree)
    (is (= 100 (coalton-impl/algorithm/avl-tree:avl-count tree)))
    ;; Height should be O(log n) — for 100 nodes, at most 9
    (is (<= (avl-height tree) 9))
    ;; Verify all values
    (loop :for i :from 0 :below 100
          :do (is (= i (nth-value 0 (coalton-impl/algorithm/avl-tree:avl-lookup tree i)))))))


(deftest test-avl-descending-insert ()
  ;; Other worst case — descending insertion
  (let ((tree (avl-empty)))
    (loop :for i :from 99 :downto 0
          :do (setf tree (coalton-impl/algorithm/avl-tree:avl-set tree i i)))
    (check-invariants tree)
    (is (= 100 (coalton-impl/algorithm/avl-tree:avl-count tree)))
    (is (<= (avl-height tree) 9))))


(deftest test-avl-keys-sorted ()
  ;; Keys should come out in ascending order regardless of insertion order
  (let* ((tree (build-tree '((c . 3) (a . 1) (e . 5) (b . 2) (d . 4))))
         (keys (coalton-impl/algorithm/avl-tree:avl-keys tree)))
    (is (equal '(a b c d e) keys))))


(deftest test-avl-values-in-key-order ()
  (let* ((tree (build-tree '((c . 3) (a . 1) (e . 5) (b . 2) (d . 4))))
         (vals (coalton-impl/algorithm/avl-tree:avl-values tree)))
    (is (equal '(1 2 3 4 5) vals))))


(deftest test-avl-to-alist ()
  (let* ((tree (build-tree '((c . 3) (a . 1) (b . 2))))
         (alist (coalton-impl/algorithm/avl-tree:avl-to-alist tree)))
    (is (equal '((a . 1) (b . 2) (c . 3)) alist))))


(deftest test-avl-from-alist ()
  (let* ((alist '((c . 3) (a . 1) (b . 2)))
         (tree (coalton-impl/algorithm/avl-tree:avl-from-alist alist)))
    (check-invariants tree)
    (is (= 3 (coalton-impl/algorithm/avl-tree:avl-count tree)))
    (is (equal '((a . 1) (b . 2) (c . 3))
               (coalton-impl/algorithm/avl-tree:avl-to-alist tree)))))


(deftest test-avl-from-alist-duplicate-keys ()
  ;; Later pairs overwrite earlier ones
  (let* ((tree (coalton-impl/algorithm/avl-tree:avl-from-alist
                '((a . 1) (b . 2) (a . 99)))))
    (check-invariants tree)
    (is (= 2 (coalton-impl/algorithm/avl-tree:avl-count tree)))
    (is (= 99 (nth-value 0 (coalton-impl/algorithm/avl-tree:avl-lookup tree 'a))))))


(deftest test-avl-foreach ()
  (let* ((tree (build-tree '((c . 3) (a . 1) (b . 2))))
         (acc nil))
    (coalton-impl/algorithm/avl-tree:avl-foreach
     (lambda (k v) (push (cons k v) acc))
     tree)
    ;; foreach visits in ascending order, so reversed acc matches
    (is (equal '((a . 1) (b . 2) (c . 3)) (nreverse acc)))))


(deftest test-avl-do-avl ()
  ;; do-avl macro iterates in order and supports early return
  (let* ((tree (build-tree '((a . 1) (b . 2) (c . 3) (d . 4))))
         (acc nil))
    (coalton-impl/algorithm/avl-tree:do-avl (k v tree)
      (declare (ignore v))
      (push k acc))
    (is (equal '(d c b a) acc)))
  ;; Test result form
  (let ((tree (build-tree '((a . 1) (b . 2)))))
    (is (= 42
           (coalton-impl/algorithm/avl-tree:do-avl (k v tree 42)
             (declare (ignore k v))))))
  ;; Test early return
  (let ((tree (build-tree '((a . 1) (b . 2) (c . 3)))))
    (is (= 2
           (coalton-impl/algorithm/avl-tree:do-avl (k v tree)
             (declare (ignore k))
             (when (= v 2) (return v)))))))


(deftest test-avl-image ()
  ;; Transform all values
  (let* ((tree (build-tree '((a . 1) (b . 2) (c . 3))))
         (tree2 (coalton-impl/algorithm/avl-tree:avl-image #'1+ tree)))
    (check-invariants tree2)
    (is (equal '((a . 2) (b . 3) (c . 4))
               (coalton-impl/algorithm/avl-tree:avl-to-alist tree2)))
    ;; Original unchanged
    (is (equal '((a . 1) (b . 2) (c . 3))
               (coalton-impl/algorithm/avl-tree:avl-to-alist tree)))))


(deftest test-avl-image-empty ()
  (let ((result (coalton-impl/algorithm/avl-tree:avl-image #'1+ (avl-empty))))
    (is (avl-empty-p result))))


(deftest test-avl-difference ()
  (let* ((t1 (build-tree '((a . 1) (b . 2) (c . 3) (d . 4))))
         (t2 (build-tree '((b . 99) (d . 99))))
         (diff (coalton-impl/algorithm/avl-tree:avl-difference t1 t2)))
    (check-invariants diff)
    (is (equal '((a . 1) (c . 3))
               (coalton-impl/algorithm/avl-tree:avl-to-alist diff)))))


(deftest test-avl-difference-disjoint ()
  ;; No overlap: difference = tree1
  (let* ((t1 (build-tree '((a . 1) (b . 2))))
         (t2 (build-tree '((c . 3) (d . 4))))
         (diff (coalton-impl/algorithm/avl-tree:avl-difference t1 t2)))
    (check-invariants diff)
    (is (equal '((a . 1) (b . 2))
               (coalton-impl/algorithm/avl-tree:avl-to-alist diff)))))


(deftest test-avl-difference-empty ()
  (let ((tree (build-tree '((a . 1))))
        (empty (avl-empty)))
    ;; diff(tree, empty) = tree
    (is (eq tree (coalton-impl/algorithm/avl-tree:avl-difference tree empty)))
    ;; diff(empty, tree) = empty
    (is (avl-empty-p (coalton-impl/algorithm/avl-tree:avl-difference empty tree)))))


(deftest test-avl-make-avl-map ()
  (let ((tree (coalton-impl/algorithm/avl-tree:make-avl-map ('a 1) ('b 2) ('c 3))))
    (check-invariants tree)
    (is (= 3 (coalton-impl/algorithm/avl-tree:avl-count tree)))
    (is (= 1 (nth-value 0 (coalton-impl/algorithm/avl-tree:avl-lookup tree 'a))))
    (is (= 2 (nth-value 0 (coalton-impl/algorithm/avl-tree:avl-lookup tree 'b))))
    (is (= 3 (nth-value 0 (coalton-impl/algorithm/avl-tree:avl-lookup tree 'c))))))


;;; Key ordering tests

(deftest test-avl-fixnum-keys ()
  ;; Fixnum keys sort numerically
  (let ((tree (build-tree '((3 . c) (1 . a) (2 . b)))))
    (check-invariants tree)
    (is (equal '(1 2 3) (coalton-impl/algorithm/avl-tree:avl-keys tree)))))


(deftest test-avl-string-keys ()
  (let ((tree (build-tree '(("cherry" . 3) ("apple" . 1) ("banana" . 2)))))
    (check-invariants tree)
    (is (equal '("apple" "banana" "cherry")
               (coalton-impl/algorithm/avl-tree:avl-keys tree)))))


(deftest test-avl-cons-keys ()
  ;; Cons keys sort lexicographically
  (let ((tree (build-tree '(((b . 2) . :b2) ((a . 2) . :a2) ((a . 1) . :a1)))))
    (check-invariants tree)
    (is (equal '((a . 1) (a . 2) (b . 2))
               (coalton-impl/algorithm/avl-tree:avl-keys tree)))))


(deftest test-avl-mixed-type-keys ()
  ;; Keys of different types: fixnum < cons < symbol < string
  (let ((tree (build-tree '((foo . :sym) (42 . :num) ("bar" . :str) ((a . b) . :cons)))))
    (check-invariants tree)
    (is (equal '(42 (a . b) foo "bar")
               (coalton-impl/algorithm/avl-tree:avl-keys tree)))))


(deftest test-avl-symbol-ordering ()
  ;; Symbols ordered by package then name
  (let ((tree (avl-empty)))
    ;; Use keywords and CL symbols to test cross-package ordering
    (setf tree (coalton-impl/algorithm/avl-tree:avl-set tree :z 1))
    (setf tree (coalton-impl/algorithm/avl-tree:avl-set tree :a 2))
    (setf tree (coalton-impl/algorithm/avl-tree:avl-set tree 'cl:list 3))
    (setf tree (coalton-impl/algorithm/avl-tree:avl-set tree 'cl:car 4))
    (check-invariants tree)
    (let ((keys (coalton-impl/algorithm/avl-tree:avl-keys tree)))
      ;; All CL symbols before KEYWORD symbols (alphabetically by package name)
      (is (eq 'cl:car (first keys)))
      (is (eq 'cl:list (second keys)))
      (is (eq :a (third keys)))
      (is (eq :z (fourth keys))))))


(deftest test-avl-uninterned-symbol-keys ()
  ;; Uninterned symbols (nil package) sort before interned ones
  (let* ((u1 (make-symbol "ZZZ"))
         (u2 (make-symbol "AAA"))
         (tree (build-tree (list (cons u1 1) (cons 'cl:car 2) (cons u2 3)))))
    (check-invariants tree)
    (let ((keys (coalton-impl/algorithm/avl-tree:avl-keys tree)))
      ;; Uninterned first (sorted by name), then interned
      (is (eq u2 (first keys)))
      (is (eq u1 (second keys)))
      (is (eq 'cl:car (third keys))))))


;;; Persistence tests

(deftest test-avl-persistence ()
  ;; Modifications produce new trees; originals are unchanged
  (let* ((t0 (avl-empty))
         (t1 (coalton-impl/algorithm/avl-tree:avl-set t0 'a 1))
         (t2 (coalton-impl/algorithm/avl-tree:avl-set t1 'b 2))
         (t3 (coalton-impl/algorithm/avl-tree:avl-remove t2 'a)))
    (is (= 0 (coalton-impl/algorithm/avl-tree:avl-count t0)))
    (is (= 1 (coalton-impl/algorithm/avl-tree:avl-count t1)))
    (is (= 2 (coalton-impl/algorithm/avl-tree:avl-count t2)))
    (is (= 1 (coalton-impl/algorithm/avl-tree:avl-count t3)))
    ;; t1 still has 'a
    (is (nth-value 1 (coalton-impl/algorithm/avl-tree:avl-lookup t1 'a)))
    ;; t3 does not
    (is (null (nth-value 1 (coalton-impl/algorithm/avl-tree:avl-lookup t3 'a))))
    ;; t3 still has 'b
    (is (nth-value 1 (coalton-impl/algorithm/avl-tree:avl-lookup t3 'b)))))


;;; Stress test

(deftest test-avl-stress-insert-remove ()
  ;; Insert 1000 keys, verify all present, remove half, verify invariants
  (let ((tree (avl-empty))
        (n 1000))
    ;; Insert 0..999
    (loop :for i :from 0 :below n
          :do (setf tree (coalton-impl/algorithm/avl-tree:avl-set tree i (* i i))))
    (check-invariants tree)
    (is (= n (coalton-impl/algorithm/avl-tree:avl-count tree)))
    ;; Verify all
    (loop :for i :from 0 :below n
          :do (multiple-value-bind (val found)
                  (coalton-impl/algorithm/avl-tree:avl-lookup tree i)
                (is found)
                (is (= (* i i) val))))
    ;; Remove even keys
    (loop :for i :from 0 :below n :by 2
          :do (setf tree (coalton-impl/algorithm/avl-tree:avl-remove tree i)))
    (check-invariants tree)
    (is (= (/ n 2) (coalton-impl/algorithm/avl-tree:avl-count tree)))
    ;; Even keys gone, odd keys remain
    (loop :for i :from 0 :below n
          :do (multiple-value-bind (val found)
                  (coalton-impl/algorithm/avl-tree:avl-lookup tree i)
                (if (evenp i)
                    (is (null found))
                    (progn
                      (is found)
                      (is (= (* i i) val))))))))


(deftest test-avl-stress-overwrite ()
  ;; Repeated overwrites of same keys — tests that replacement is stable
  (let ((tree (avl-empty))
        (n 200))
    (dotimes (round 5)
      (loop :for i :from 0 :below n
            :do (setf tree (coalton-impl/algorithm/avl-tree:avl-set tree i (+ i round)))))
    (check-invariants tree)
    (is (= n (coalton-impl/algorithm/avl-tree:avl-count tree)))
    ;; Final values should be from round 4
    (loop :for i :from 0 :below n
          :do (is (= (+ i 4)
                     (nth-value 0 (coalton-impl/algorithm/avl-tree:avl-lookup tree i)))))))


;;; make-load-form test (FASL serialization)

(deftest test-avl-make-load-form ()
  ;; avl-node supports make-load-form (required for FASL)
  (let ((tree (build-tree '((a . 1) (b . 2)))))
    (multiple-value-bind (creation init)
        (make-load-form tree)
      (declare (ignore init))
      (is (not (null creation))))))
