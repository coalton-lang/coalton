(cl:in-package #:coalton-native-tests)

(define-test simple-hashtable ()
  (let ((ht (the (Hashtable String Integer) (hashtable:new)))
        (insert! (hashtable:set! ht))
        (get (hashtable:get ht)))
    (progn
      (insert! "zero" 0)
      (insert! "one" 1)
      (insert! "two" 2)
      (insert! "three" 3)
      (is (== (Some 0) (get "zero")))
      (is (== (Some 1) (get "one")))
      (is (== (Some 2) (get "two")))
      (is (== (Some 3) (get "three")))
      (is (none? (get "four")))
      (is (== 4 (hashtable:count ht)))
      (hashtable:remove! ht "zero")
      (is (none? (get "zero")))
      (is (== 3 (hashtable:count ht))))))

(define-test hashtable-constructor-equivalencies ()
  (let ht-eq? = (fn (ht-a ht-b)
                  (== (list:sort (hashtable:entries ht-a))
                      (list:sort (hashtable:entries ht-b)))))
  (let ht = (hashtable:new))
  (hashtable:set! ht "zero" 0)
  (hashtable:set! ht "one" 1)
  (hashtable:set! ht "two" 2)
  (hashtable:set! ht "three" 3)
  (is (ht-eq? ht
              (hashtable:make ("zero" 0)
                              ("one" 1)
                              ("two" 2)
                              ("three" 3))))
  (is (ht-eq? ht
              (let ((zero "zero")
                    (one "one")
                    (two "two")
                    (three "three"))
                (hashtable:make (zero 0)
                                (one 1)
                                (two 2)
                                (three 3))))))

(cl:in-package #:coalton-tests)

(deftest hashtable-static-duplicate-keys ()
  (signals coalton-library/hashtable::make-hash-table-static-duplicate-keys
    (check-coalton-types
     "(define my-ht
        (coalton-library/hashtable:make (\"zero\" 0)
                                         (\"one\" 1)
                                         (\"two\" 2)
                                         (\"zero\" 3)))"))

  (signals coalton-library/hashtable::make-hash-table-static-duplicate-keys
    (check-coalton-types
     "(define my-ht
        (let ((zero \"zero\")
                       (one \"one\")
                       (two \"two\"))
           (coalton-library/hashtable:make (zero 0)
                                           (one 1)
                                           (two 2)
                                           (zero 3))))")))
