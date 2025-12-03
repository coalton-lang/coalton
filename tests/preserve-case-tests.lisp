(in-package #:coalton-tests)

(deftest test-source-names-are-preserved ()
  (let* ((program "
(package coalton-case-tests/preserve)

(define-type (CamelType :a)
  LowerCaseCtor
  (WrappedValue :a))

(define-type-alias (CamelAlias :a) (CamelType :a))

(define-struct (MixedStruct :a)
  (FieldOne :a))

(define-class (FunkyClass :f))
")
         (source (source:make-source-string program :name "case-preserve"))
         (pkg-name "COALTON-CASE-TESTS/PRESERVE"))
    (unwind-protect
         (with-open-stream (stream (source:source-stream source))
           (parser:with-reader-context stream
             (multiple-value-bind (_ env)
                 (entry:entry-point (parser:read-program stream source ':file))
               (declare (ignore _))
               (let* ((pkg (find-package pkg-name))
                      (type-sym (intern "CAMELTYPE" pkg))
                      (ctor-a (intern "LOWERCASECTOR" pkg))
                      (ctor-b (intern "WRAPPEDVALUE" pkg))
                      (alias-sym (intern "CAMELALIAS" pkg))
                      (struct-sym (intern "MIXEDSTRUCT" pkg))
                      (class-sym (intern "FUNKYCLASS" pkg)))
                 (is (equal "CamelType"
                            (tc:type-entry-source-name (tc:lookup-type env type-sym))))
                 (is (equal "LowerCaseCtor"
                            (tc:constructor-entry-source-name (tc:lookup-constructor env ctor-a))))
                 (is (equal "WrappedValue"
                            (tc:constructor-entry-source-name (tc:lookup-constructor env ctor-b))))
                 (is (equal "CamelAlias"
                            (tc:type-alias-entry-source-name (tc:lookup-type-alias env alias-sym))))
                 (is (equal "MixedStruct"
                            (tc:struct-entry-source-name (tc:lookup-struct env struct-sym))))
                 (is (equal "FunkyClass"
                            (tc:ty-class-source-name (tc:lookup-class env class-sym))))
                 ;; Built-in types have hardcoded source-names
                 (is (equal "Boolean"
                            (tc:type-entry-source-name (tc:lookup-type env 'coalton:Boolean))))))))
      (alexandria:when-let ((pkg (find-package pkg-name)))
        (delete-package pkg)))))
