(in-package #:coalton-impl/codegen)

(defgeneric compile-pattern (pattern env)
  (:method ((pattern pattern-var) env)
    (declare (type coalton-impl/typechecker::environment env)
             (ignore env))
    (pattern-var-id pattern))
  (:method ((pattern pattern-wildcard) env)
    (declare (type coalton-impl/typechecker::environment env))
    '_)
  (:method ((pattern pattern-literal) env)
    (declare (type coalton-impl/typechecker::environment env))
    (pattern-literal-value pattern))

  (:method ((pattern pattern-constructor) env)
    (declare (type coalton-impl/typechecker::environment env))
    (let* ((name (pattern-constructor-name pattern))
           (entry (lookup-constructor env name))
           (type (lookup-type env (constructor-entry-constructs entry)))
           (class-name (constructor-entry-classname entry))
           (package (symbol-package name)))

      (cond
        ((type-entry-enum-repr type)
         `',(constructor-entry-compressed-repr entry))

        ((type-entry-newtype type)
         (progn
           (unless (= 1 (length (pattern-constructor-patterns pattern)))
             (coalton-impl::coalton-bug "Unexpected number of fields in newtype pattern.~%    Expected: 1~%    Received: ~A~%"
                                        (length (pattern-constructor-patterns pattern))))
           (compile-pattern (first (pattern-constructor-patterns pattern)) env)))

        ((eql (pattern-constructor-name pattern) 'coalton:Cons)
         `(cl:cons
           ,(compile-pattern (first (pattern-constructor-patterns pattern)) env)
           ,(compile-pattern (second (pattern-constructor-patterns pattern)) env)))

        ((eql (pattern-constructor-name pattern) 'coalton:Nil)
         'cl:nil)


        (t
         (let ((fields (loop :for field :in (pattern-constructor-patterns pattern)
                             :for i :from 0
                             :for subpattern := (compile-pattern field env)
                             :for accessor := (alexandria:format-symbol package "_~D" i)
                             :collect `(,accessor ,subpattern))))
           `(cl:structure ,class-name ,@fields)))))))
