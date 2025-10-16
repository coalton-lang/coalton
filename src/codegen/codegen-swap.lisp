(defpackage #:coalton-impl/codegen/codegen-swap
  (:use
   #:cl)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:settings #:coalton-impl/settings)
   (#:ast #:coalton-impl/codegen/ast)
   (#:pattern #:coalton-impl/codegen/pattern)
   (#:codegen-pattern #:coalton-impl/codegen/codegen-pattern))
  (:export
   #:normalize-pattern))

(in-package #:coalton-impl/codegen/codegen-swap)

(defgeneric normalize-pattern (pattern)
  (:method ((pattern pattern:pattern-literal))
    (declare (values pattern:pattern t &optional))
    (values
     (pattern:make-pattern-wildcard :type tc:*unit-type*)
     (pattern:pattern-literal-value pattern))
    )

  (:method ((pattern pattern:pattern-wildcard))
    (declare (values pattern:pattern t &optional))
    (let ((name (gensym "VAR"))) 
      (values 
       (pattern:make-pattern-var :type tc:*unit-type* :name name)
       name)))

  (:method ((pattern pattern:pattern-constructor))
    (declare (values pattern:pattern t &optional))

    (let ((fields '())
          (subpatterns '()))
 
      (loop :for pattern :in (reverse (pattern:pattern-constructor-patterns pattern))
            :do (multiple-value-bind (pattern field)
                    (normalize-pattern pattern)
                  (push field fields)
                  (push pattern subpatterns)))

      (values 
       (pattern:make-pattern-constructor :type tc:*unit-type*
                                         :name (pattern:pattern-constructor-name pattern)
                                         :patterns subpatterns)
       `(,(pattern:pattern-constructor-name pattern) ,@fields)))))
