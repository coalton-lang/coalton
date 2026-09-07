(in-package #:coalton-tests)

(deftest test-doc-instance-overlap-badges ()
  (let* ((class (coalton/doc/model::make-coalton-class
                 (tc:lookup-class entry:*global-environment* 'coalton/classes:Into)))
         (instances (coalton/doc/model:object-instances class))
         (marked (find-if #'tc:ty-class-instance-overlap-p instances))
         (unmarked (find-if-not #'tc:ty-class-instance-overlap-p instances)))
    (is (not (null marked)))
    (is (not (null unmarked)))
    ;; Types and structs use the same instance renderer. A marked singleton
    ;; must still advertise overlap even without any competing instance.
    (dolist (object (list class
                         (make-instance 'coalton/doc/model:coalton-type
                                        :instances (list marked))
                         (make-instance 'coalton/doc/model:coalton-struct
                                        :instances (list unmarked))))
      (dolist (output (list (coalton/doc/html::instances-html object)
                           (with-output-to-string (stream)
                             ;; Hugo delegates its instance lists to Markdown.
                             (coalton/doc/markdown::write-instances
                              (coalton/doc/base:make-backend :markdown stream)
                              object))))
        (is (= (count-if #'tc:ty-class-instance-overlap-p
                         (coalton/doc/model:object-instances object))
               (length (cl-ppcre:all-matches-as-strings
                        "class=.?instance-overlap-badge" output))))
        (dolist (instance (coalton/doc/model:object-instances object))
          (let* ((signature (coalton/doc/markdown::to-markdown instance))
                 (signature-end (search (concatenate 'string signature "</code>") output)))
            (is (not (null signature-end)))
            (when (tc:ty-class-instance-overlap-p instance)
              (let ((suffix (subseq output (+ signature-end (length signature) 7))))
                (is (search "instance-overlap-badge" suffix))
                (is (search "/manual/operators/overlap/" suffix))
                (is (search "Declared with (overlap)" suffix))
                ;; The badge follows the closing code tag, not the type text.
                (is (= 0 (search "<a " suffix)))))))))))

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

(deftest test-doc-kind-binder-rendering ()
  (let ((coalton-impl/settings:*coalton-print-unicode* nil))
    (dolist (example
              '(("(forall ((:r Values)) ((Void -> :r) -> :r))"
                 . "forall (:R Values). (Void -> :R) -> :R")
                ("(forall ((:a Type)) ((Void -> :a) -> :a))"
                 . "forall :A. (Void -> :A) -> :A")
                ("(forall ((:f (Type -> Type)) :a) ((:f :a) -> (:f :a)))"
                 . "forall (:F (Type -> Type)) :A. :F :A -> :F :A")
                ("(forall ((:h ((Type -> Type) -> Type)))
                    ((:h List) -> (:h List)))"
                 . "forall (:H ((Type -> Type) -> Type)). :H List -> :H List")))
      (is (string= (cdr example)
                   (coalton-impl/typechecker/type-string:type-to-string
                    (parse-doc-test-scheme (car example))
                    entry:*global-environment*))))))

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
