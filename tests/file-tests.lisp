(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare res-succeeds ((Result :a :b) -> Boolean))
  (define (res-succeeds res)
    (match res
      ((Ok _)
       True)
      ((Err _)
       False)))
  
  (declare res-fails ((Result :a :b) -> Boolean))
  (define (res-fails res)
    (match res
      ((Ok _)
       False)
      ((Err _)
       True))))

(define-test file-test ()
  (let syspath = (unwrap (file:system-relative-pathname "coalton" "tests/")))
  (is (file:directory-pathname? syspath))
  
  (let testpath = (unwrap (file:merge syspath "file-tests/")))
  (is (not (unwrap (file:directory-exists? testpath))))
  (is (res-succeeds (file:create-directory testpath)))
  (is (unwrap (file:directory-exists? testpath)))

  (let filepath = (unwrap (file:merge testpath "test-file.txt")))
  (is (not (unwrap (file:file-exists? filepath))))
  (is (res-succeeds
       (file:with-open-file (file:Output filepath file:EError file:Create)
         (fn (stream)
           (file:write-string stream "Hello World!")))))
  (is (unwrap (file:file-exists? filepath)))
  (is (== (unwrap (file:read-file-to-string filepath))
          "Hello World!"))

  (let filepath2 = (unwrap (file:merge testpath "test-file2.txt")))
  (is (res-succeeds (file:copy filepath filepath2)))
  (is (unwrap (file:file-exists? filepath2)))
  (is (== (unwrap (file:read-file-to-string filepath2))
          "Hello World!"))

  ;; this handles order ambiguity between lisp implementations
  #+allegro
  (is (== (unwrap (file:directory-files testpath))
            (make-list filepath2 filepath)))
  #-allegro
  (is (== (unwrap (file:directory-files testpath))
            (make-list filepath filepath2)))
  
  ;; clearing the file-test directory
  (is (res-fails (file:remove-directory testpath)))
  (is (res-fails (file:remove-directory filepath)))
  (is (res-fails (file:remove-directory-recursive filepath)))

  (is (res-succeeds (file:delete-file filepath)))
  (is (res-succeeds (file:delete-file filepath2)))
  (is (unwrap (file:empty? testpath)))

  (is (res-succeeds (file:remove-directory testpath)))
  ;; clearing the file-test directory for real this time
  (is (not (unwrap (file:directory-exists? testpath)))))
