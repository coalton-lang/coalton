(cl:in-package #:coalton-native-tests)

(define-test string-substring-finders ()
  (let find-foo = (string:substring-index "foo"))
  (is (== (Some 0) (find-foo "foo")))
  (is (== (Some 0) (find-foo "foo bar")))
  (is (== (Some 4) (find-foo "bar foo")))
  (is (== (Some 4) (find-foo "bar foo baz")))
  (is (== None (find-foo "")))
  (is (== None (find-foo "bar")))
  (is (== None (find-foo "bar baz quux")))

  (let find-empty-string = (string:substring-index ""))
  (is (== (Some 0) (find-empty-string "")))
  (is (== (Some 0) (find-empty-string "foo")))

  (let has-foo? = (string:substring? "foo"))
  (is (has-foo? "foo"))
  (is (has-foo? "foo bar"))
  (is (has-foo? "bar foo"))
  (is (has-foo? "bar foo baz"))
  (is (not (has-foo? "")))
  (is (not (has-foo? "bar")))
  (is (not (has-foo? "bar baz quux")))

  (let has-empty? = (string:substring? ""))
  (is (has-empty? ""))
  (is (has-empty? "foo")))
