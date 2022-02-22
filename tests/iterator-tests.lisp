(cl:in-package #:coalton-native-tests)

(define-test iter-list-iter-collect-list-id ()
  (is (== (make-list 0 1 2 3)
          (iter:collect-list! (iter:list-iter (make-list 0 1 2 3))))))

(define-test iter-empty-iter-none ()
  (is (== (the (Optional UFix) None)
          (iter:next! iter:empty)))
  (is (== (the (Optional Char) None)
          (iter:next! iter:empty)))
  (is (== (the (Optional String) None)
          (iter:next! iter:empty))))

(define-test iter-string-chars ()
  (let ((iter (iter:string-chars "abcdef")))
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
                 (iter:every! (uncurry ==)
                         (iter:zip! (iter:string-chars expected-str)
                               (iter:char-range range-start range-end))))))
    (progn (is (same? "abcdef" #\a #\f))
           (is (same? "0123456789" #\0 #\9))
           (is (same? "ABCDEF" #\A #\F)))))

(define-test iter-take-10-filter-even ()
  (is (== (the (List Integer) (make-list 0 2 4 6 8))
          (iter:collect-list! (iter:filter! even?
                                  (iter:take! 10
                                         (iter:count-forever))))))
  (is (== (the (List Integer) (make-list 0 2 4 6 8 10 12 14 16 18))
          (iter:collect-list! (iter:take! 10
                                (iter:filter! even?
                                         (iter:count-forever)))))))

(define-test iter-concat-collect-list ()
  (is (== (make-list 0 1 2 3 4 5 6 7 8 9)
          (iter:collect-list! (iter:concat! (iter:up-to 5)
                                  (iter:range-increasing 1 5 10)))))
  (is (== (make-list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
          (iter:collect-list! (iter:flatten! (iter:list-iter (make-list (iter:up-to 5)
                                                         (iter:range-increasing 1 5 10)
                                                         (iter:list-iter (make-list 10 11 12 13 14)))))))))

(define-test iter-remove-duplicates-collect-list ()
  (is (== (make-list 0 1 2 3 4)
          (iter:collect-list! (iter:remove-duplicates! (iter:list-iter (make-list 0
                                                                   1 1
                                                                   2 2 2
                                                                   3 3 3 3
                                                                   4 4 4 4 4))))))
  (is (== (make-list #\a #\b #\c #\d #\e)
          (iter:collect-list! (iter:remove-duplicates! (iter:string-chars "abbcccddddeeeee"))))))

(define-test iter-index-of ()
  (is (== (Some 0)
          (iter:index-of! (== #\a) (iter:string-chars "abcde"))))
  (is (== (Some 4)
          (iter:index-of! (== #\e) (iter:string-chars "abcde"))))
  (is (== None
          (iter:index-of! (== #\f) (iter:string-chars "abcde"))))
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
              (iter:repeat-item "foo" 10)))
  (is (not (iter:any! (/= "foo")
                 (iter:repeat-item "foo" 10)))))

(define-test iter-downfrom ()
  (is (== (make-list 9 8 7 6 5 4 3 2 1 0)
          (iter:collect-list! (iter:down-from 10))))
  (is (== (make-list 9 8 7 6 5 4 3 2 1 0)
          (iter:collect-list! (iter:range-decreasing 1 10 0)))))

(define-test iter-min-max ()
  (is (== (Some 10)
          (iter:max! (iter:concat! (iter:up-through 10)
                                   (iter:down-from 10)))))
  (is (== (Some (the UFix 10))
          (iter:max! (iter:concat! (iter:up-through 10)
                                   (iter:down-from 10)))))
  (is (== (Some 0)
          (iter:min! (iter:concat! (iter:down-from 10)
                                   (iter:up-to 10)))))
  (is (== (Some (the UFix 0))
          (iter:min! (iter:concat! (iter:down-from 10)
                                   (iter:up-to 10))))))

(define-test iter-optimize-string-length ()
  (let ((longer? (fn (long short)
                   (> (string:length long)
                      (string:length short))))
        (strings (make-list "a" "aa" "aaa" "aa" "a")))
    (progn
      (is (== (Some "aaa")
              (iter:optimize! longer? (iter:list-iter strings))))
      (is (== (Some "a")
              (iter:optimize! (fn (short long) (not (longer? short long)))
                         (iter:list-iter strings)))))))

(define-test iter-enumerate ()
  (let ((iter (iter:enumerate! (iter:string-chars "abcde")))
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

;;; FIXME: define more tests
;; - vector-iter
;; - recursive-iter
;; - repeat-forever
;; - pair-with!
;; - count!
;; - for-each!
;; - find!
;; - collect-vector!
;; - collect-vector-size-hint!
;; - collect-hashtable!
;; - collect-hashtable-size-hint!
