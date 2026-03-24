(in-package #:coalton-tests)

(defun parse-doc-test-scheme (string)
  (let ((*package* (make-package "COALTON-DOC-TEST-PACKAGE"
                                 :use '("COALTON" "COALTON-PRELUDE"))))
    (unwind-protect
         (let ((source (source:make-source-string string)))
           (with-open-stream (stream (source:source-stream source))
             (tc:parse-ty-scheme
              (parser:parse-qualified-type
               (parser:with-reader-context stream
                 (eclector.concrete-syntax-tree:read stream))
               source)
              entry:*global-environment*)))
      (delete-package *package*))))

(deftest test-doc-keyword-type-rendering ()
  (let* ((keyword-type
           (tc:qualified-ty-type
            (tc:ty-scheme-type
             (parse-doc-test-scheme
              "(Integer &key (:timeout Integer) (:extra Integer) -> Integer)"))))
         (keyword-only-type
           (tc:qualified-ty-type
            (tc:ty-scheme-type
             (parse-doc-test-scheme
              "(&key (:x Integer) -> Integer)"))))
         (empty-keyword-type
           (tc:qualified-ty-type
            (tc:ty-scheme-type
             (parse-doc-test-scheme
              "(&key -> Integer)"))))
         (nested-application-type
           (tc:qualified-ty-type
            (tc:ty-scheme-type
             (parse-doc-test-scheme
              "(List (coalton/seq:Seq Integer) -> Tuple Boolean String)"))))
         (string-render
           (coalton/doc/model:object-name keyword-type))
         (keyword-only-render
           (coalton/doc/model:object-name keyword-only-type))
         (empty-render
           (coalton/doc/model:object-name empty-keyword-type))
         (nested-application-render
           (coalton/doc/model:object-name nested-application-type))
         (markdown-render
           (coalton/doc/markdown::to-markdown keyword-type))
         (nested-markdown-render
           (coalton/doc/markdown::to-markdown nested-application-type)))
    (is (string= "Integer &key (:extra Integer) (:timeout Integer) -> Integer"
                 string-render))
    (is (string= "&key (:x Integer) -> Integer"
                 keyword-only-render))
    (is (string= "Void -> Integer"
                 empty-render))
    (is (string= "List (Seq Integer) -> Tuple Boolean String"
                 nested-application-render))
    (is (not (search "&key ->" empty-render)))
    (is (search "&key" markdown-render))
    (is (search "(:extra" markdown-render))
    (is (search "(:timeout" markdown-render))
    (is (string=
         "<a href=\"#coalton-list-type\">List</a> (<a href=\"#coalton-seq-seq-type\">Seq</a> <a href=\"#coalton-integer-type\">Integer</a>) &rarr; <a href=\"#coalton-classes-tuple-type\">Tuple</a> <a href=\"#coalton-boolean-type\">Boolean</a> <a href=\"#coalton-string-type\">String</a>"
         nested-markdown-render))))
