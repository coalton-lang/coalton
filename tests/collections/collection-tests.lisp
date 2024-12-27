(cl:in-package #:coalton-native-tests)

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
        (is (== (cln:length (,@the-ufix (cln:new-from 5 (fn (x) x))))
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
        (is (cln:empty? (,@the-ufix (cln:new-from 0 (fn (x) x))))))
      (define-test ,(test-name type-symbol "new-convert") ()
        ;; Converting empty => empty
        (let ((source (make-list)))
          (is (cln:empty? (,@the-ufix (cln:new-convert source)))))
        ;; The converted collection has the same elements
        (let ((source (the (List UFix) (cln:new-repeat 3 99))))
          (is (== (cln:length (,@the-ufix (cln:new-convert source))) 3))
          (is (cln:empty?
                (cln:filter (/= 99) (,@the-ufix (cln:new-convert source)))))))        
    )))