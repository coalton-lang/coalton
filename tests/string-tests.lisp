(cl:in-package #:coalton-native-tests)

(define-test string-into-list-string-string ()
  (is (== "foobar"
          (the String (into (make-list "foo" "bar"))))))
