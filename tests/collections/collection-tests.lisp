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
                  `(,@the-type (cln:new-convert (make-list ,@args)))))
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
      (define-test ,(test-name type-symbol "flatten") ()
        ;; Note: Can't assume the length of flattened collections - e.g. Set
        ;; Flatten empty
        (let ((empty-collections
                (the (,type-symbol (,type-symbol UFix))
                    (cln:new-collection)))
              (flattened (cln:flatten empty-collections)))
          (is (cln:empty? flattened)))
        ;; Flatten single sub-collection
        (let ((single
                (the (,type-symbol (,type-symbol UFix))
                     (cln:new-convert (make-list (cln:new-convert (make-list 10)))))))
          (is (cln:contains-elt? 10 (cln:flatten single))))
        ;; Flatten multiple sub-collections
        (let ((nested
                (the (,type-symbol (,type-symbol UFix))
                    (cln:new-convert (make-list (cln:new-convert (make-list 10 10))
                                                (cln:new-convert (make-list 20))))))
              (flattened (cln:flatten nested)))
          (is (cln:contains-elt? 10 flattened))
          (is (cln:contains-elt? 20 flattened)))
        ; ;; Ensure immutability
        (let ((nested
                (the (,type-symbol (,type-symbol UFix))
                    (cln:new-convert (make-list (cln:new-convert (make-list 10 20)))))))
          (cln:flatten nested)
          (is (== (cln:length nested) 1))))
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
        (let ((single ,(make-the-cln 10))
              (rd (cln:remove-duplicates single)))
          (is (== (cln:length rd) 1))
          (is (cln:contains-elt? 10 rd)))
        ;; Multiple duplicates
        (let ((dupes ,(make-the-cln 10 10 10 20 20))
              (rd (cln:remove-duplicates dupes)))
          (is (== (cln:length rd) 2))
          (is (cln:contains-elt? 10 rd))
          (is (cln:contains-elt? 20 rd)))
        ;; Some duplicates and some non-duplicates
        (let ((mixed ,(make-the-cln 1 2 1 3 2 4))
              (rd (,@the-type (cln:remove-duplicates mixed))))
          (is (== (cln:length rd) 4))
          (is (cln:contains-elt? 1 rd))
          (is (cln:contains-elt? 2 rd))
          (is (cln:contains-elt? 3 rd))
          (is (cln:contains-elt? 4 rd)))
        ;; Ensure immutability
        (let ((orig ,(make-the-cln 1 2 3 2 3)))
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
