(cl:in-package #:coalton-native-tests)

(define-test iter-list-iter-collect-list-id ()
  (is (== (the (List Integer) (make-list 0 1 2 3))
          (iter:collect! (iter:into-iter (make-list 0 1 2 3))))))

(define-test iter-empty-iter-none ()
  (is (== (the (Optional UFix) None)
          (iter:next! iter:empty)))
  (is (== (the (Optional Char) None)
          (iter:next! iter:empty)))
  (is (== (the (Optional String) None)
          (iter:next! iter:empty))))

(define-test iter-string-chars ()
  (let ((iter (iter:into-iter "abcdef")))
    (progn
      (is (== (Some #\a)
              (iter:next! iter)))
      (is (== (Some #\b)
              (iter:next! iter)))
      (is (== (Some #\c)
              (iter:next! iter)))
      (is (== (Some #\d)
              (iter:next! iter)))
      (is (== (Some #\e)
              (iter:next! iter)))
      (is (== (Some #\f)
              (iter:next! iter)))
      (is (== None
              (iter:next! iter))))))

(define-test iter-char-range-string-chars ()
  (let ((same? (fn (expected-str range-start range-end)
                 (iter:and!
                  (iter:zip-with!
                   ==
                   (iter:into-iter expected-str)
                   (char:range range-start range-end))))))
    (progn (is (same? "abcdef" #\a #\f))
           (is (same? "0123456789" #\0 #\9))
           (is (same? "ABCDEF" #\A #\F)))))

(define-test iter-take-10-filter-even ()
  (is (== (the (List Integer) (make-list 0 2 4 6 8))
          (iter:collect! (iter:filter! even?
                                       (iter:take! 10
                                                   (iter:count-forever))))))
  (is (== (the (List Integer) (make-list 0 2 4 6 8 10 12 14 16 18))
          (iter:collect! (iter:take! 10
                                     (iter:filter! even?
                                                   (iter:count-forever)))))))

(define-test iter-concat-collect-list ()
  (is (== (the (List Integer) (make-list 0 1 2 3 4 5 6 7 8 9))
          (iter:collect! (iter:chain! (iter:up-to 5)
                                      (iter:range-increasing 1 5 10)))))
  (is (== (the (List Integer) (make-list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
          (iter:collect! (iter:flatten! (iter:into-iter (make-list (iter:up-to 5)
                                                                   (iter:range-increasing 1 5 10)
                                                                   (iter:into-iter (make-list 10 11 12 13 14)))))))))

(define-test iter-remove-duplicates-collect-list ()
  (is (== (the (List Integer) (make-list 0 1 2 3 4))
          (iter:collect! (iter:remove-duplicates! (iter:into-iter (make-list 0
                                                                             1 1
                                                                             2 2 2
                                                                             3 3 3 3
                                                                             4 4 4 4 4))))))
  (is (== (make-list #\a #\b #\c #\d #\e)
          (iter:collect! (iter:remove-duplicates! (iter:into-iter "abbcccddddeeeee"))))))

(define-test iter-index-of ()
  (is (== (Some 0)
          (iter:index-of! (== #\a) (iter:into-iter "abcde"))))
  (is (== (Some 4)
          (iter:index-of! (== #\e) (iter:into-iter "abcde"))))
  (is (== None
          (iter:index-of! (== #\f) (iter:into-iter "abcde"))))
  (is (== (Some 0)
          (iter:index-of! (== 0) (iter:count-forever))))
  (is (== (Some 5)
          (iter:index-of! (== 5) (iter:count-forever)))))

(define-test iter-sum ()
  (is (== 0 (iter:sum! iter:empty)))
  (is (== (the UFix 0)
          (iter:sum! iter:empty)))
  (is (== 45 (iter:sum! (iter:up-to 10))))
  (is (== (the UFix 45)
          (iter:sum! (iter:up-to 10))))
  (is (== 55 (iter:sum! (iter:up-through 10))))
  (is (== (the UFix 55)
          (iter:sum! (iter:up-through 10)))))

(define-test iter-repeat-item-every ()
  (is (iter:every! (== "foo")
                   (iter:repeat-for "foo" 10)))
  (is (not (iter:any! (/= "foo")
                      (iter:repeat-for "foo" 10)))))

(define-test iter-downfrom ()
  (is (== (the (List Integer) (make-list 9 8 7 6 5 4 3 2 1 0))
          (iter:collect! (iter:down-from 10))))
  (is (== (the (List Integer) (make-list 9 8 7 6 5 4 3 2 1 0))
          (iter:collect! (iter:range-decreasing 1 10 0)))))

(define-test iter-min-max ()
  (is (== (Some (the Integer 10))
          (iter:max! (iter:chain! (iter:up-through 10)
                                  (iter:down-from 10)))))
  (is (== (Some (the UFix 10))
          (iter:max! (iter:chain! (iter:up-through 10)
                                  (iter:down-from 10)))))
  (is (== (Some (the Integer 0))
          (iter:min! (iter:chain! (iter:down-from 10)
                                  (iter:up-to 10)))))
  (is (== (Some (the UFix 0))
          (iter:min! (iter:chain! (iter:down-from 10)
                                  (iter:up-to 10)))))
  (is (== (Some (the Integer 0))
          (iter:maximize-by! negate (iter:chain! (iter:up-to 10)
                                                (iter:down-from 10)))))
  (is (== (Some (the Integer 10))
          (iter:minimize-by! negate (iter:chain! (iter:down-from 10)
                                                (iter:up-through 10))))))

(define-test iter-optimize-string-length ()
  (let ((longer? (fn (long short)
                   (> (string:length long)
                      (string:length short))))
        (strings (make-list "a" "aa" "aaa" "aa" "a")))
    (progn
      (is (== (Some "aaa")
              (iter:optimize! longer? (iter:into-iter strings))))
      (is (== (Some "a")
              (iter:optimize! (fn (short long) (not (longer? short long)))
                              (iter:into-iter strings)))))))

(define-test iter-enumerate ()
  (let ((iter (iter:enumerate! (iter:into-iter "abcde")))
        (expect (fn (idx c)
                  (is (== (Some (Tuple idx c))
                          (iter:next! iter))))))
    (progn
      (expect 0 #\a)
      (expect 1 #\b)
      (expect 2 #\c)
      (expect 3 #\d)
      (expect 4 #\e)
      (is (== None
              (iter:next! iter))))))

(define-test iter-interleave ()
  (is (== (iter:collect!
           (iter:interleave! (iter:into-iter (make-list 1 2 3 4 5 6))
                             iter:empty))
          (the (List Integer) (make-list 1 2 3 4 5 6))))
  (is (== (iter:collect!
           (iter:interleave! iter:empty
                             (iter:into-iter (make-list 1 2 3 4 5 6))))
          (the (List Integer) (make-list 1 2 3 4 5 6))))
  (is (== (iter:collect!
           (iter:interleave! (iter:into-iter (make-list 1 3 5))
                             (iter:into-iter (make-list 2 4 6))))
          (the (List Integer) (make-list 1 2 3 4 5 6))))
  (is (== (iter:collect!
           (iter:interleave! (iter:into-iter (make-list 1 3 5))
                             (iter:into-iter (make-list 2 4 6 7 8 9 10))))
          (the (List Integer) (make-list 1 2 3 4 5 6 7 8 9 10))))
  (is (== (iter:collect!
           (iter:interleave! (iter:into-iter (make-list 1 3 5 7 8 9 10))
                             (iter:into-iter (make-list 2 4 6))))
          (the (List Integer) (make-list 1 2 3 4 5 6 7 8 9 10)))))

(define-test elementwise-match-/= ()
  (is (not (iter:elementwise==! (iter:into-iter (make-list 0 1 2))
                                (iter:into-iter (make-list 0 1)))))
  (is (not (iter:elementwise-match! <
                                    (iter:into-iter (make-list 0 1 2))
                                    (iter:into-iter (make-list 0 1))))))

(define-test filter-map ()
  (is (== (iter:collect! (iter:filter-map! string:parse-int (iter:into-iter (make-list "1" "2" "three" "" "5"))))
          (make-list 1 2 5))))

(define-test iter-for-each ()
  (let ((populate-vector (fn ()
                           (let v = (vector:with-capacity 6))
                           (iter:for-each! (fn (x)
                                             (vector:push! x v)
                                             Unit)
                                           (iter:up-to 5))
                           v)))
    (is (== (iter:collect! (iter:into-iter (populate-vector)))
            (make-list 0 1 2 3 4)))))

(define-test iter-count ()
  (is (== (iter:count! (iter:into-iter (make-list 1 2 3 4 5)))
          5)))

(define-test iter-find ()
  (let ((even-in-iter (fn (iter)
                        (iter:find! (fn (x) (math:zero? (mod x 2)))
                                    iter))))
    (is (== (even-in-iter (iter:into-iter (make-list 1 3 5 6)))
            (Some 6)))
    (is (== (even-in-iter (iter:into-iter (make-list 1 3 5 7)))
            None))))

(define-test iter-flat-map ()
  (is (== (the (List Integer) (make-list 0 2 1 3 2 4 3 5))
	  (iter:collect!
	   (iter:flat-map! (fn (x) (iter:into-iter (make-list x (+ x 2))))
			   (iter:up-to 4))))))

;;; FIXME: define more tests
;; - vector-iter
;; - recursive-iter
;; - repeat-forever
;; - pair-with!
;; - collect-vector!
;; - collect-vector-size-hint!
;; - collect-hashtable!
;; - collect-hashtable-size-hint!
