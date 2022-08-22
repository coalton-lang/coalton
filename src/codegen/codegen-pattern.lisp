(defpackage #:coalton-impl/codegen/codegen-pattern
  (:use
   #:cl
   #:coalton-impl/util)
  (:local-nicknames
   (#:ast #:coalton-impl/ast)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:codegen-pattern))

(in-package #:coalton-impl/codegen/codegen-pattern)

(defgeneric codegen-pattern (pattern env)
  (:method ((pattern ast:pattern-var) env)
    (declare (type tc:environment env)
             (ignore env))
    (ast:pattern-var-id pattern))
  (:method ((pattern ast:pattern-wildcard) env)
    (declare (type coalton-impl/typechecker::environment env))
    '_)
  (:method ((pattern ast:pattern-literal) env)
    (declare (type coalton-impl/typechecker::environment env))
    (ast:pattern-literal-value pattern))

  (:method ((pattern ast:pattern-constructor) env)
    (declare (type coalton-impl/typechecker::environment env))
    (let* ((name (ast:pattern-constructor-name pattern))
           (entry (tc:lookup-constructor env name))
           (type (tc:lookup-type env (tc:constructor-entry-constructs entry)))
           (class-name (tc:constructor-entry-classname entry))
           (package (symbol-package name)))

      (cond
        ((tc:type-entry-enum-repr type)
         `',(tc:constructor-entry-compressed-repr entry))

        ((tc:type-entry-newtype type)
         (progn
           (unless (= 1 (length (ast:pattern-constructor-patterns pattern)))
             (coalton-impl::coalton-bug "Unexpected number of fields in newtype pattern.~%    Expected: 1~%    Received: ~A~%"
                                        (length (ast:pattern-constructor-patterns pattern))))
           (codegen-pattern (first (ast:pattern-constructor-patterns pattern)) env)))

        ((eql (ast:pattern-constructor-name pattern) 'coalton:Cons)
         `(cl:cons
           ,(codegen-pattern (first (ast:pattern-constructor-patterns pattern)) env)
           ,(codegen-pattern (second (ast:pattern-constructor-patterns pattern)) env)))

        ((eql (ast:pattern-constructor-name pattern) 'coalton:Nil)
         'cl:nil)


        (t
         (let ((fields (loop :for field :in (ast:pattern-constructor-patterns pattern)
                             :for i :from 0
                             :for subpattern := (codegen-pattern field env)
                             :for accessor := (alexandria:format-symbol package "_~D" i)
                             :collect `(,accessor ,subpattern))))
           `(cl:structure ,class-name ,@fields)))))))
