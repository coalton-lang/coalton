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

(deftest test-doc-private-parser-instances ()
  (let* ((class (tc:lookup-class entry:*global-environment* 'coalton/classes:Alternative))
         (instances (coalton/doc/environment:class-instances class))
         (coalton/doc/base:*local* (namestring (asdf:system-source-directory "coalton"))))
    ;; The compiler must retain the internal instances used by FORMAT.
    (is (some (lambda (instance)
                (member 'coalton/format::Parser
                        (tc:type-constructors (tc:ty-predicate-types
                                               (tc:ty-class-instance-predicate instance)))))
              (tc:lookup-class-instances entry:*global-environment* 'coalton/classes:Alternative)))
    (is (not (some (lambda (instance)
                     (member 'coalton/format::Parser
                             (tc:type-constructors (tc:ty-predicate-types
                                                    (tc:ty-class-instance-predicate instance)))))
                   instances)))
    (is (some (lambda (instance)
                (member 'coalton:Optional
                        (tc:type-constructors (tc:ty-predicate-types
                                               (tc:ty-class-instance-predicate instance)))))
              instances))
    (dolist (backend '(:markdown :html :hugo))
      (let ((output
              (with-output-to-string (stream)
                (coalton/doc/base:write-packages
                 (coalton/doc/base:make-backend backend stream)
                 (list (coalton/doc/model::make-coalton-package
                        (find-package "COALTON/CLASSES")))))))
        (is (not (search "Parser" output)))
        (is (search "Optional" output))))))

(deftest test-doc-instance-type-visibility ()
  (let ((*package* (make-package "COALTON-DOC-INSTANCE-TEST-PACKAGE"
                                :use '("COALTON" "COALTON-PRELUDE"))))
    (unwind-protect
         (let ((source
                 (source:make-source-string
                  "(define-type Public Public)
                   (define-type Private Private)
                   (define-class (DocClass :a))
                   (define-class (DocMultiClass :a :b))
                   (define-instance (DocClass Public))
                   (define-instance (DocClass Private))
                   (define-instance (DocClass (List Public)))
                   (define-instance (DocClass (List Private)))
                   (define-instance (DocClass (Optional :a)))
                   (define-instance (DocMultiClass Public Private))
                   (define-instance (DocMultiClass Private Public))
                   (define-instance (DocMultiClass Public Public))")))
           (with-open-stream (stream (source:source-stream source))
             (let ((program (parser:with-reader-context stream
                              (parser:read-program stream source))))
               (multiple-value-bind (program env) (entry:entry-point program)
                 (declare (ignore program))
                 (export (mapcar (lambda (name) (find-symbol name *package*))
                                 '("PUBLIC" "DOCCLASS" "DOCMULTICLASS")))
                 (let* ((entry:*global-environment* env)
                        (objects (coalton/doc/model:find-objects :package *package*))
                        (class (find "DocClass" objects :test #'string=
                                                       :key #'coalton/doc/model:object-name))
                        (multi-class (find "DocMultiClass" objects :test #'string=
                                                                  :key #'coalton/doc/model:object-name))
                        (public-type (find "Public" objects :test #'string=
                                                           :key #'coalton/doc/model:object-name))
                        (private (find-symbol "PRIVATE" *package*)))
                   (flet ((instance-names (object)
                            (mapcar #'coalton/doc/model:object-name
                                    (coalton/doc/model:sort-objects
                                     (coalton/doc/model:object-instances object)))))
                     (is (equal '("DocClass (List Public)"
                                  "DocClass (Optional :A)"
                                  "DocClass Public")
                                (instance-names class)))
                     (is (equal '("DocMultiClass Public Public")
                                (instance-names multi-class)))
                     ;; Type documentation must use the same visibility rules.
                     (is (member "DocClass Public" (instance-names public-type) :test #'string=))
                     (is (member "DocMultiClass Public Public" (instance-names public-type) :test #'string=)))
                   (is (not (some (lambda (instance)
                                    (member private
                                            (tc:type-constructors
                                             (tc:ty-predicate-types
                                              (tc:ty-class-instance-predicate instance)))))
                                  (coalton/doc/model:object-instances public-type)))))))))
      (delete-package *package*))))
