(cl:in-package #:coalton-native-tests)

;;; NOTE:
;;; The suites in this file represent the full contract of the collections typeclasses,
;;; specifying and verifying behavior beyond what is in the docstring of the function
;;; definitions. The tests should be comprehensive enough to verify the correctness
;;; of any implementations, but must be careful to not over-specify the behavior.
;;;
;;; Valid collections could exhibit behavior that does not align with the standard
;;; List/Vector paradigm that most are used to. For example, in the test for `add`
;;; we verify that adding an element to an empty collection makes it length 1, making
;;; that behavior part of the contract for all collections:
;;;
;;; (let ((c (,@the-type (cln:add 99 (cln:new-collection)))))
;;;   (is (== (cln:length c) 1))
;;;   (is (cln:contains-elt? 99 c)))
;;;
;;; However, we are careful to not assume that adding increases length by 1 *in general*
;;; because this would not be true of Sets:
;;;
;;; (let ((c (,@the-type (cln:add 99 (cln:add 99 (cln:new-collection))))))
;;;   (is (cln:contains-elt? 99 c)))
;;;
;;; Therefore, whenever a new collection is added to the library that violates a previous
;;; assumption, the test suite should be weakened to allow for the new collection. Also,
;;; it might be reasonable for individual collections to provide additional tests on
;;; the standard functions to verify the guarantees they make above the generic contract.

(cl:defun test-name (type-symbol test-name)
  "Intern and return a symbol of the form 'TEST-<`TYPE-SYMBOL`>-<`TEST-NAME`>

Example:
  (test-name 'List 'append') => 'TEST-LIST-APPEND"
  (cl:intern (cl:string-upcase
               (cl:concatenate
                'cl:string
                "TEST-"
                (cl:symbol-name type-symbol)
                "-"
                test-name))))

(cl:defmacro collection-tests (type-symbol)
  "Run a standard test suite to verify correct behavior for a Collection typeclass instance.

Example:
  (collection-tests List)"
  (cl:let ((the-type `(the (,type-symbol :a)))
           (the-ufix `(the (,type-symbol UFix))))
    (cl:labels ((make-the-cln (cl:&rest args)
                  `(,@the-type (cln:new-convert (make-list ,@args))))
                (make-ufix-cln (cl:&rest args)
                  `(,@the-ufix (cln:new-convert (make-list ,@args)))))
    `(cl:progn
      (define-test ,(test-name type-symbol "new-collection") ()
        ;; Length should be 0
        (is (== (cln:length (,@the-type (cln:new-collection)))
                0)))
      (define-test ,(test-name type-symbol "new-repeat") ()
        ;; Length should be n
        (is (== (cln:length (,@the-type (cln:new-repeat 4 100)))
                4))
        ;; Every element should be the specified element
        (is (== (cln:length
                  (cln:filter (== 100)
                              (,@the-type (cln:new-repeat 4 100))))
                4)))
      (define-test ,(test-name type-symbol "new-from") ()
        ;; Creates a collection of length n
        (is (== (cln:length (,@the-ufix (cln:new-from 5 id)))
                5))
        ;; Contains values produced by applying the function [0..n)
        (let ((c (,@the-ufix (cln:new-from 4 (fn (x) (* x 2))))))
          (is (== (cln:length c) 4))
          (is (cln:contains-elt? 0 c))
          (is (cln:contains-elt? 2 c))
          (is (cln:contains-elt? 6 c))
          ;; Should not contain a value outside the generated set
          (is (not (cln:contains-elt? 5 c))))
        ;; n = 0 => empty collection
        (is (cln:empty? (,@the-ufix (cln:new-from 0 id)))))
      (define-test ,(test-name type-symbol "new-convert") ()
        ;; Converting empty => empty
        (let ((source (make-list)))
          (is (cln:empty? (,@the-ufix (cln:new-convert source)))))
        ;; The converted collection has the same elements
        (let ((source (the (List UFix) (cln:new-repeat 3 99))))
          (is (== (cln:length (,@the-ufix (cln:new-convert source))) 3))
          (is (cln:empty?
                (cln:filter (/= 99) (,@the-ufix (cln:new-convert source)))))))
        ;; TODO: Fix this test case
      ;; (define-test ,(test-name type-symbol "flatten") ()
      ;;   ;; Note: Can't assume the length of flattened collections - e.g. Set
      ;;   ;; Flatten empty
      ;;   ;; (let ((empty-collections
      ;;   ;;         (the (,type-symbol (,type-symbol UFix))
      ;;   ;;             (cln:new-collection)))
      ;;   ;;       (flattened (,@the-ufix (cln:flatten empty-collections))))
      ;;   ;;   (is (cln:empty? flattened)))
      ;;   ;; Flatten single sub-collection
      ;;   (let ((single
      ;;           (the (,type-symbol (,type-symbol UFix))
      ;;                (cln:new-convert (make-list (cln:new-convert (make-list 10)))))))
      ;;     (is (cln:contains-elt? 10 (cln:flatten single))))
      ;;   ;; Flatten multiple sub-collections
      ;;   (let ((nested
      ;;           (the (,type-symbol (,type-symbol UFix))
      ;;               (cln:new-convert (make-list (cln:new-convert (make-list 10 10))
      ;;                                           (cln:new-convert (make-list 20))))))
      ;;         (flattened (cln:flatten nested)))
      ;;     (is (cln:contains-elt? 10 flattened))
      ;;     (is (cln:contains-elt? 20 flattened)))
      ;;   ; ;; Ensure immutability
      ;;   (let ((nested
      ;;           (the (,type-symbol (,type-symbol UFix))
      ;;               (cln:new-convert (make-list (cln:new-convert (make-list 10 20)))))))
      ;;     (cln:flatten nested)
      ;;     (is (== (cln:length nested) 1))))
      (define-test ,(test-name type-symbol "filter") ()
        ;; Filter empty => empty
        (let ((empty (,@the-type (cln:new-collection))))
          (is (cln:empty? (cln:filter (== 1) empty))))
        ;; Filter no matches
        (let ((no-matches ,(make-the-cln 2 4 6)))
          (is (cln:empty? (cln:filter (== 1) no-matches))))
        ;; Filter partial matches
        (let ((part ,(make-the-cln 1 2 3 4 5))
              (f (cln:filter (> 3) part)))
          (is (== (cln:length f) 2))
          (is (cln:contains-elt? 1 f))
          (is (cln:contains-elt? 2 f))
          (is (not (cln:contains-elt? 3 f))))
        ;; Filter all matches
        (let ((all (,@the-type (cln:new-repeat 3 10)))
              (f (cln:filter (== 10) all)))
          (is (== (cln:length f) 3))
          (is (cln:contains-elt? 10 f)))
        ;; Ensure immutability
        (let ((original ,(make-the-cln 1 2 3 4 5)))
          (cln:filter (== 1) original)
          (is (== (cln:length original) 5))))
      (define-test ,(test-name type-symbol "remove-duplicates") ()
        ;; Empty
        (let ((empty-c (,@the-ufix (cln:new-collection))))
          (is (cln:empty? (cln:remove-duplicates empty-c))))
        ;; Single element
        (let ((single ,(make-ufix-cln 10))
              (rd (cln:remove-duplicates single)))
          (is (== (cln:length rd) 1))
          (is (cln:contains-elt? 10 rd)))
        ;; Multiple duplicates
        (let ((dupes ,(make-ufix-cln 10 10 10 20 20))
              (rd (cln:remove-duplicates dupes)))
          (is (== (cln:length rd) 2))
          (is (cln:contains-elt? 10 rd))
          (is (cln:contains-elt? 20 rd)))
        ;; Some duplicates and some non-duplicates
        (let ((mixed ,(make-ufix-cln 1 2 1 3 2 4))
              (rd (,@the-ufix (cln:remove-duplicates mixed))))
          (is (== (cln:length rd) 4))
          (is (cln:contains-elt? 1 rd))
          (is (cln:contains-elt? 2 rd))
          (is (cln:contains-elt? 3 rd))
          (is (cln:contains-elt? 4 rd)))
        ;; Ensure immutability
        (let ((orig ,(make-ufix-cln 1 2 3 2 3)))
          (cln:remove-duplicates orig)
          (is (== (cln:length orig) 5))))
      (define-test ,(test-name type-symbol "empty?") ()
        ;; Empty => true
        (let ((c (,@the-type (cln:new-collection))))
          (is (cln:empty? c)))
        ;; Non-empty => false
        (let ((c ,(make-the-cln 10)))
          (is (not (cln:empty? c)))))
      (define-test ,(test-name type-symbol "length") ()
        ;; Empty => 0
        (let ((c (,@the-type (cln:new-collection))))
          (is (== (cln:length c) 0)))
        ;; Single => 1
        (let ((c ,(make-the-cln 10)))
          (is (== (cln:length c) 1))))
      (define-test ,(test-name type-symbol "contains-elt?") ()
        ;; Empty => false
        (let ((c (,@the-type (cln:new-collection))))
          (is (not (cln:contains-elt? 10 c))))
        ;; Element present => true
        (let ((c ,(make-the-cln 0 10 20)))
          (is (cln:contains-elt? 10 c)))
        ;; Element absent => false
        (let ((c ,(make-the-cln 0 10 20)))
          (is (not (cln:contains-elt? 30 c)))))
      (define-test ,(test-name type-symbol "contains-where?") ()
        ;; Empty => false
        (let ((empty (,@the-type (cln:new-collection))))
          (is (not (cln:contains-where? (== 1) empty))))
        ;; No matches => false
        (let ((no-matches ,(make-the-cln 2 4 6)))
          (is (not (cln:contains-where? (== 1) no-matches))))
        ;; Some matches => true
        (let ((part ,(make-the-cln 1 2 3 4 5)))
          (is (cln:contains-where? (> 3) part)))
        ;; Multiple matches => true
        (let ((mult ,(make-the-cln 10 20 30)))
          (is (cln:contains-where? (>= 10) mult))))
      (define-test ,(test-name type-symbol "count-where") ()
        ;; Empty => 0
        (let ((empty (,@the-type (cln:new-collection))))
          (is (== (cln:count-where (== 1) empty) 0)))
        ;; No matches => 0
        (let ((no-matches ,(make-the-cln 2 4 6)))
          (is (== (cln:count-where (== 1) no-matches) 0)))
        ;; Some matches
        (let ((part ,(make-the-cln 1 2 3 4 5)))
          (is (== (cln:count-where (> 3) part) 2)))
        ;; All matches
        (let ((all (,@the-type (cln:new-repeat 3 10))))
          (is (== (cln:count-where (== 10) all) 3))))
      (define-test ,(test-name type-symbol "add") ()
        ;; Add to Empty Collection
        (let ((c (,@the-type (cln:add 99 (cln:new-collection)))))
          (is (== (cln:length c) 1))
          (is (cln:contains-elt? 99 c)))
        ;; Add duplicate element (can't make any guarantees about length beyond the first element)
        (let ((c (,@the-type (cln:add 99 (cln:add 99 (cln:new-collection))))))
          (is (cln:contains-elt? 99 c)))
        ;; Ensure immutability
        (let ((c (,@the-type (cln:new-collection))))
          (cln:add 99 c)
          (is (cln:empty? c))))
      (define-test ,(test-name type-symbol "remove-elt") ()
        ;; Remove from Empty Collection
        (let ((c (,@the-type (cln:remove-elt 1 (cln:new-collection)))))
          (is (cln:empty? c)))
        ;; Remove Single Occurrence
        (let ((c ,(make-the-cln 10)))
          (is (cln:empty? (cln:remove-elt 10 c))))
        ;; Remove Multiple Occurrences
        (let ((c ,(make-the-cln 20 20 20)))
          (is (cln:empty? (cln:remove-elt 20 c))))
        ;; Remove From Heterogeous Collection
        (let ((c ,(make-the-cln 10 20))
              (removed (cln:remove-elt 10 c)))
          (is (cln:contains-elt? 20 removed))
          (is (not (cln:contains-elt? 10 removed))))
        ;; Remove Missing Element
        (let ((c ,(make-the-cln 99))
              (removed (cln:remove-elt 10 c)))
          (is (== (cln:length removed) 1)))
        ;; Ensure immutability
        (let ((c ,(make-the-cln 10 20)))
          (cln:remove-elt 10 c)
          (is (cln:contains-elt? 10 c))))))))

(cl:defmacro mutable-collection-tests (type-symbol)
  "Run a standard test suite to verify correct behavior for a MutableCollection typeclass instance.

Example:
  (mutable-collection-tests Vector)"
  (cl:let ((the-type `(the (,type-symbol :a))))
    (cl:labels ((make-the-cln (cl:&rest args)
                  `(,@the-type (cln:new-convert (make-list ,@args)))))
    `(cl:progn
      (define-test ,(test-name type-symbol "copy") ()
        ;; Contains the same elements
        (let ((orig ,(make-the-cln 10))
              (the-copy (cln:copy orig)))
          (is (== (cln:length the-copy) 1))
          (is (cln:contains-elt? 10 the-copy)))
        ;; ;; Mutates separately
        (let ((orig ,(make-the-cln 10))
              (the-copy (cln:copy orig)))
          (add! 20 the-copy)
          (is (not (cln:contains-elt? 20 orig)))))
      (define-test ,(test-name type-symbol "add!") ()
        ;; Add to empty collection
        (let ((c ,(make-the-cln)))
          (add! 10 c)
          (is (== 1 (cln:length c)))
          (is (cln:contains-elt? 10 c)))
        ;; Add to a full collection
        (let ((c ,(make-the-cln 0)))
          (add! 10 c)
          (is (cln:contains-elt? 0 c))
          (is (cln:contains-elt? 10 c)))
        ;; Add duplicate element retains
        (let ((c ,(make-the-cln 0 10)))
          (add! 10 c)
          (is (cln:contains-elt? 10 c))))))))

(cl:defmacro linear-collection-tests (type-symbol)
  "Run a standard test suite to verify correct behavior for a LinearCollection typeclass instance.
The LinearCollection must have an Eq instance over the test type such that (== (col1 :type) (col2 :type))
if & only if col1 and col2 are the same length and have (== col1_i col2_i) for all 0 <= i < (length col1).

Example:
  (linear-collection-tests List)"
  (cl:let ((the-ufix `(the (,type-symbol UFix))))
    (cl:labels ((make-ufix-cln (cl:&rest args)
                  `(,@the-ufix (cln:new-convert (make-list ,@args)))))
      `(cl:progn
        (define-test ,(test-name type-symbol "head") ()
          ;; head of empty => NONE
          (let ((empty (,@the-ufix (cln:new-collection))))
            (is (== None (cln:head empty))))
          ;; head of single => SOME(elt)
          (let ((one ,(make-ufix-cln 10)))
            (is (== (Some 10) (cln:head one))))
          ;; head of multi => SOME(first-element)
          (let ((many ,(make-ufix-cln 1 2 3 4)))
            (is (== (Some 1) (cln:head many)))))
        (define-test ,(test-name type-symbol "head#") ()
          ;; Single => returns element
          (let ((one ,(make-ufix-cln 10)))
            (is (== 10 (cln:head# one))))
          ;; Multiple => returns first element
          (let ((many ,(make-ufix-cln 1 2 3 4)))
            (is (== 1 (cln:head# many)))))
        (define-test ,(test-name type-symbol "last") ()
          ;; last of empty => NONE
          (let ((empty (,@the-ufix (cln:new-collection))))
            (is (== None (cln:last empty))))
          ;; last of single => SOME(elt)
          (let ((one ,(make-ufix-cln 10)))
            (is (== (Some 10) (cln:last one))))
          ;; last of multi => SOME(last-element)
          (let ((many ,(make-ufix-cln 1 2 3 4)))
            (is (== (Some 4) (cln:last many)))))
        (define-test ,(test-name type-symbol "last#") ()
          ;; Single => returns element
          (let ((one ,(make-ufix-cln 10)))
            (is (== 10 (cln:last# one))))
          ;; Multiple => returns last element
          (let ((many ,(make-ufix-cln 1 2 3 4)))
            (is (== 4 (cln:last# many)))))
        (define-test ,(test-name type-symbol "tail") ()
          ;; tail of empty => empty
          (let ((empty (,@the-ufix (cln:new-collection))))
            (is (cln:empty? (cln:tail empty))))
          ;; tail of single => empty
          (let ((one ,(make-ufix-cln 10)))
            (is (cln:empty? (cln:tail one))))
          ;; tail of multi => all except first
          (let ((many ,(make-ufix-cln 1 2 3 4))
                (t3 (cln:tail many)))
            (is (== ,(make-ufix-cln 2 3 4) t3))))
        (define-test ,(test-name type-symbol "take") ()
          ;; take 0 => empty
          (let ((c ,(make-ufix-cln 1 2 3 4 5)))
            (is (cln:empty? (cln:take 0 c))))
          ;; take < length
          (let ((c ,(make-ufix-cln 1 2 3 4 5))
                (t2 (cln:take 2 c)))
            (is (== ,(make-ufix-cln 1 2) t2)))
          ;; take == length => full collection
          (let ((c ,(make-ufix-cln 1 2 3 4 5))
                (all (cln:take 5 c)))
            (is (== c all)))
          ;; take > length => full collection
          (let ((c ,(make-ufix-cln 1 2 3 4 5))
                (all (cln:take 10 c)))
            (is (== c all))))
        (define-test ,(test-name type-symbol "index-elt") ()
          ;; empty => NONE
          (let ((empty (,@the-ufix (cln:new-collection))))
            (is (== None (cln:index-elt 10 empty))))
          ;; single => SOME(0) if it matches
          (let ((one ,(make-ufix-cln 10)))
            (is (== (Some 0) (cln:index-elt 10 one)))
            (is (== None (cln:index-elt 99 one))))
          ;; multiple => SOME(index) of first occurrence
          (let ((many ,(make-ufix-cln 1 2 3 2 4)))
            (is (== (Some 1) (cln:index-elt 2 many)))
            (is (== None (cln:index-elt 99 many)))))
        (define-test ,(test-name type-symbol "index-elt#") ()
          ;; single => returns 0
          (let ((one ,(make-ufix-cln 10)))
            (is (== 0 (cln:index-elt# 10 one))))
          ;; multiple => index of first occurrence
          (let ((many ,(make-ufix-cln 1 2 3 2 4)))
            (is (== 1 (cln:index-elt# 2 many)))))
        (define-test ,(test-name type-symbol "index-where") ()
          ;; empty => NONE
          (let ((empty (,@the-ufix (cln:new-collection))))
            (is (== None (cln:index-where (== 10) empty))))
          ;; single => SOME(0) if predicate matches
          (let ((one ,(make-ufix-cln 10)))
            (is (== (Some 0) (cln:index-where (== 10) one)))
            (is (== None (cln:index-where (== 99) one))))
          ;; multiple => SOME(index) of first match
          (let ((many ,(make-ufix-cln 1 2 3 4 5)))
            (is (== (Some 1) (cln:index-where (< 1) many)))
            (is (== None (cln:index-where (== 99) many)))))
        (define-test ,(test-name type-symbol "index-where#") ()
          ;; single => returns 0 if match
          (let ((one ,(make-ufix-cln 10)))
            (is (== 0 (cln:index-where# (== 10) one))))
          ;; multiple => index of first match
          (let ((many ,(make-ufix-cln 1 2 3 4 5)))
            (is (== 1 (cln:index-where# (< 1) many)))))
        (define-test ,(test-name type-symbol "find-where") ()
          ;; empty => NONE
          (let ((empty (,@the-ufix (cln:new-collection))))
            (is (== None (cln:find-where (== 10) empty))))
          ;; single => SOME(elt) if match
          (let ((one ,(make-ufix-cln 10)))
            (is (== (Some 10) (cln:find-where (== 10) one))))
          ;; multiple => SOME(first-match)
          (let ((many ,(make-ufix-cln 1 2 3 4 5)))
            (is (== (Some 2) (cln:find-where (< 1) many)))
            (is (== None (cln:find-where (== 99) many)))))
        (define-test ,(test-name type-symbol "indices-elt") ()
          ;; empty => no indices
          (let ((empty (,@the-ufix (cln:new-collection))))
            (is (cln:empty? (cln:indices-elt 10 empty))))
          ;; single => returns (0) if match, else ()
          (let ((one ,(make-ufix-cln 10)))
            (is (== (make-list 0) (cln:indices-elt 10 one)))
            (is (cln:empty? (cln:indices-elt 99 one))))
          ;; multiple => returns indices in ascending order
          (let ((many ,(make-ufix-cln 1 2 3 2 4 2)))
            (is (== (make-list 1 3 5) (cln:indices-elt 2 many)))))
        (define-test ,(test-name type-symbol "indices-where") ()
          ;; empty => no indices
          (let ((empty (,@the-ufix (cln:new-collection))))
            (is (cln:empty? (cln:indices-where (== 10) empty))))
          ;; single => returns (0) if predicate matches, else ()
          (let ((one ,(make-ufix-cln 10)))
            (is (== (make-list 0) (cln:indices-where (== 10) one)))
            (is (cln:empty? (cln:indices-where (== 99) one))))
          ;; multiple => returns indices of all matches in ascending order
          (let ((many ,(make-ufix-cln 1 2 3 4 5 2 10 2)))
            (is (== (make-list 1 5 7) (cln:indices-where (== 2) many)))
            (is (cln:empty? (cln:indices-where (< 99) many)))))
        (define-test ,(test-name type-symbol "subseq") ()
          ;; 0 0 => empty
          (let ((c ,(make-ufix-cln 1 2 3 4 5))
                (s (cln:subseq 0 0 c)))
            (is (cln:empty? s)))
          ;; 0 1 => first element
          (let ((c ,(make-ufix-cln 1 2 3 4 5))
                (s (cln:subseq 0 1 c)))
            (is (== ,(make-ufix-cln 1) s)))
          ;; 1 3 => subset of the collection
          (let ((c ,(make-ufix-cln 1 2 3 4 5))
                (s (cln:subseq 1 3 c)))
            (is (== ,(make-ufix-cln 2 3) s)))
          ;; 0 length => full collection
          (let ((c ,(make-ufix-cln 1 2 3 4 5))
                (s (cln:subseq 0 5 c)))
            (is (== c s)))
          ;; 0 > length => full collection
          (let ((c ,(make-ufix-cln 1 2 3 4 5))
                (s (cln:subseq 0 10 c)))
            (is (== (cln:length s) 5)))
          ;; start beyond length => empty
          (let ((c ,(make-ufix-cln 1 2 3 4 5))
                (s (cln:subseq 10 20 c)))
            (is (cln:empty? s)))
          ;; start = end => empty
          (let ((c ,(make-ufix-cln 1 2 3 4 5))
                (s (cln:subseq 3 3 c)))
            (is (cln:empty? s)))
          ;; start > end => empty
          (let ((c ,(make-ufix-cln 1 2 3 4 5))
                (s (cln:subseq 3 1 c)))
            (is (cln:empty? s)))
          ;; Ensure immutability
          (let ((original ,(make-ufix-cln 1 2 3 4 5)))
            (cln:subseq 1 2 original)
            (is (== (cln:length original) 5))))
        (define-test ,(test-name type-symbol "split-at") ()
          ;; split-at on empty => (empty, empty) regardless of index
          (let ((empty (,@the-ufix (cln:new-collection))))
            (match (cln:split-at 0 empty)
              ((Tuple c1 c2)
               (is (cln:empty? c1))
               (is (cln:empty? c2)))))
          ; split-at 0 => remove 0th element => (empty, rest)
          (let ((c ,(make-ufix-cln 10 20 30)))
            (match (cln:split-at 0 c)
              ((Tuple c1 c2)
               (is (cln:empty? c1))
               (is (== ,(make-ufix-cln 20 30) c2)))))
          ;; split in the middle => col1 is [0..i-1], skip i, col2 is [i+1..end]
          (let ((c ,(make-ufix-cln 1 2 3 4 5)))
            (match (cln:split-at 2 c)
              ((Tuple c1 c2)
               (is (== ,(make-ufix-cln 1 2) c1))
               (is (== ,(make-ufix-cln 4 5) c2)))))
          ;; split-at index >= length => entire collection in first, empty in second
          (let ((c ,(make-ufix-cln 1 2 3)))
            (match (cln:split-at 10 c)
              ((Tuple c1 c2)
               (is (== c c1))
               (is (cln:empty? c2)))))
          ; ;; Ensure immutability
          (let ((orig ,(make-ufix-cln 1 2 3 4 5)))
            (cln:split-at 2 orig)
            (is (== ,(make-ufix-cln 1 2 3 4 5) orig))))
        (define-test ,(test-name type-symbol "split-elt") ()
          ;; empty => (list of a single empty collection)
          (let ((empty (,@the-ufix (cln:new-collection)))
                (res (cln:split-elt 99 empty)))
            (is (== (make-list (,@the-ufix (cln:new-collection))) res)))
          ;; no occurrences => single element list containing original
          (let ((c ,(make-ufix-cln 1 2 3))
                (res (cln:split-elt 99 c)))
            (is (== (make-list c) res)))
          ;; single occurrence
          (let ((c ,(make-ufix-cln 1 2 3))
                (res (cln:split-elt 2 c)))
            (is (== (make-list ,(make-ufix-cln 1)
                               ,(make-ufix-cln 3))
                     res)))
          ;; multiple occurrences
          (let ((c ,(make-ufix-cln 2 1 2 3 2 4))
                (res (cln:split-elt 2 ,(make-ufix-cln 2 1 2 3 2 4 5 2))))
            (is (== (make-list ,(make-ufix-cln)
                               ,(make-ufix-cln 1)
                               ,(make-ufix-cln 3)
                               ,(make-ufix-cln 4 5)
                               ,(make-ufix-cln))
                     res)))
          ;; ensure immutability
          (let ((orig ,(make-ufix-cln 1 2 3 4)))
            (cln:split-elt 2 orig)
            (is (== ,(make-ufix-cln 1 2 3 4) orig))))
        (define-test ,(test-name type-symbol "reverse") ()
          ;; Reverse of empty => empty
          (let ((empty (,@the-ufix (cln:new-collection))))
            (is (cln:empty? (cln:reverse empty))))
          ;; Reverse of single => same collection
          (let ((one ,(make-ufix-cln 10)))
            (is (== one (cln:reverse one))))
          ;; Reverse of multiple => elements reversed
          (let ((many ,(make-ufix-cln 1 2 3 4 5)))
            (is (== ,(make-ufix-cln 5 4 3 2 1) (cln:reverse many))))
          ;; Ensure immutability
          (let ((orig ,(make-ufix-cln 1 2 3 4 5)))
            (cln:reverse orig)
            (is (== ,(make-ufix-cln 1 2 3 4 5) orig))))
        (define-test ,(test-name type-symbol "sort") ()
          ;; Sort of empty => empty
          (let ((empty (,@the-ufix (cln:new-collection))))
            (is (cln:empty? (cln:sort empty))))
          ;; Sort of single => same collection
          (let ((one ,(make-ufix-cln 10)))
            (is (== one (cln:sort one))))
          ;; Sort of multiple unsorted => ascending order
          (let ((many ,(make-ufix-cln 3 1 2 5 4)))
            (is (== ,(make-ufix-cln 1 2 3 4 5) (cln:sort many))))
          ;; Sort of already sorted => stays the same
          (let ((sorted ,(make-ufix-cln 1 2 3 4 5)))
            (is (== sorted (cln:sort sorted))))
          ;; Ensure immutability
          (let ((orig ,(make-ufix-cln 4 3 2)))
            (cln:sort orig)
            (is (== ,(make-ufix-cln 4 3 2) orig))))
        (define-test ,(test-name type-symbol "sort-with") ()
          ;; Sort-with on empty => empty
          (let ((empty (,@the-ufix (cln:new-collection))))
            (is (cln:empty? (cln:sort-with <=> empty))))
          ;; Sort-with on single => same collection
          (let ((one ,(make-ufix-cln 10)))
            (is (== one (cln:sort-with <=> one))))
          ;; Sort-with a custom comparator => sorted accordingly
          (let ((many ,(make-ufix-cln 3 1 2 5 4))
                (descending (cln:sort-with (fn (a b) (<=> b a)) many)))
            (is (== ,(make-ufix-cln 5 4 3 2 1) descending)))
          ;; Ensure immutability
          (let ((orig ,(make-ufix-cln 4 3 2)))
            (cln:sort-with <=> orig)
            (is (== ,(make-ufix-cln 4 3 2) orig))))
        (define-test ,(test-name type-symbol "push") ()
          ;; push onto empty => single element
          (let ((empty (,@the-ufix (cln:new-collection)))
                (res (cln:push 99 empty)))
            (is (== ,(make-ufix-cln 99) res)))
          ;; push onto non-empty => new element is the first
          (let ((orig ,(make-ufix-cln 1 2 3))
                (res (cln:push 99 orig)))
            (is (== ,(make-ufix-cln 99 1 2 3) res)))
          ;; Ensure immutability
          (let ((orig ,(make-ufix-cln 1 2 3)))
            (cln:push 99 orig)
            (is (== ,(make-ufix-cln 1 2 3) orig))))
        (define-test ,(test-name type-symbol "push-end") ()
          ;; push-end onto empty => single element
          (let ((empty (,@the-ufix (cln:new-collection)))
                (res (cln:push-end 99 empty)))
            (is (== ,(make-ufix-cln 99) res)))
          ;; push-end onto non-empty => new element is the last
          (let ((orig ,(make-ufix-cln 1 2 3))
                (res (cln:push-end 99 orig)))
            (is (== ,(make-ufix-cln 1 2 3 99) res)))
          ;; Ensure immutability
          (let ((orig ,(make-ufix-cln 1 2 3)))
            (cln:push-end 99 orig)
            (is (== ,(make-ufix-cln 1 2 3) orig))))
        (define-test ,(test-name type-symbol "insert-at") ()
          ;; Insert at 0 on an empty collection => becomes single-element
          (let ((empty (,@the-ufix (cln:new-collection))))
            (is (== ,(make-ufix-cln 99) (cln:insert-at 0 99 empty))))
          ;; Insert at 0 on multi-element collection => element becomes first
          (let ((c ,(make-ufix-cln 1 2 3 4)))
            (is (== ,(make-ufix-cln 99 1 2 3 4) (cln:insert-at 0 99 c))))
          ;; Insert in the middle
          (let ((c ,(make-ufix-cln 1 2 3 4 5)))
            (is (== ,(make-ufix-cln 1 2 99 3 4 5) (cln:insert-at 2 99 c))))
          ;; Insert at index = length => appended at the end
          (let ((c ,(make-ufix-cln 10 20 30)))
            (is (== ,(make-ufix-cln 10 20 30 99) (cln:insert-at 3 99 c))))
          ;; Insert at index > length => appended at the end
          (let ((c ,(make-ufix-cln 1 2 3)))
            (is (== ,(make-ufix-cln 1 2 3 99) (cln:insert-at 99 99 c))))
          ;; Ensure immutability
          (let ((orig ,(make-ufix-cln 1 2 3)))
            (cln:insert-at 1 99 orig)
            (is (== ,(make-ufix-cln 1 2 3) orig))))
        ))))