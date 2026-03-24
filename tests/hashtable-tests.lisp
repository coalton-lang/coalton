(in-package #:coalton-native-tests)

(define-test simple-hashtable ()
  (let ((ht (the (Hashtable String Integer) (hashtable:new)))
        (insert! (fn (key value)
                   (hashtable:set! ht key value)))
        (get (fn (key)
               (hashtable:get ht key))))
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
                  (== (list:sort (iter:collect! (hashtable:entries ht-a)))
                      (list:sort (iter:collect! (hashtable:entries ht-b))))))
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

(define-test hashtable-hashing ()
  (is (== (hash (the (Hashtable String Integer) (hashtable:new)))
          (hash (the (Hashtable String Integer) (hashtable:new)))))

  (is (== (hash (hashtable:make (0 "zero") (1 "one")))
          (hash (hashtable:make (0 "zero") (1 "one")))))

  (is (== (hash (hashtable:make (1 "one") (0 "zero")))
          (hash (hashtable:make (0 "zero") (1 "one")))))

  (let ht1 = (hashtable:new))
  (for ((declare x UFix)
        (x 0 (1+ x)))
    :repeat 10000
    (hashtable:set! ht1 x x))

  (let ht2 = (hashtable:new))
  (for ((declare x UFix)
        (x 10000 (1- x)))
    :repeat 10000
    (let y = (1- x))
    (hashtable:set! ht2 y y))

  (is (== (hash ht1) (hash ht2)))

  (is (/= (hash (hashtable:make (0 "zero")))
          (hash (hashtable:make (0 "two")))))

  (is (/= (hash (the (Hashtable String String) (hashtable:make)))
          (hash (hashtable:make ("a" "b"))))))

(define-test hashtable-show ()
  (let ht = (hashtable:new))
  (is (== "#<HashTable [=>]>"
          (show-as-string (the (Hashtable String Integer) (hashtable:new)))))
  (hashtable:set! ht "alpha" 1)
  (is (== "#<HashTable [\"alpha\" => 1]>"
          (show-as-string ht))))
