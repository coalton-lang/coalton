(in-package #:coalton-tests)

(deftest test-patterns-exhaustive-tyvar-head-does-not-crash ()
  "Constructor-only pattern exhaustiveness checks should return NIL when the
type head is still a variable, rather than signaling a type error."
  (let ((ty (tc:make-variable))
        (pattern
          (coalton-impl/codegen/pattern:make-pattern-constructor
           :type (tc:make-variable)
           :name 'coalton:True
           :patterns nil)))
    (is (null (coalton-impl/codegen/pattern:patterns-exhaustive-p
               (list pattern)
               ty
               entry:*global-environment*)))))

(deftest test-patterns-exhaustive-tyvar-head-boolean-signature ()
  "Constructor signatures should still be recognized when the scrutinee type
head is a type variable."
  (let ((ty (tc:make-variable))
        (true-pattern
          (coalton-impl/codegen/pattern:make-pattern-constructor
           :type tc:*boolean-type*
           :name 'coalton:True
           :patterns nil))
        (false-pattern
          (coalton-impl/codegen/pattern:make-pattern-constructor
           :type tc:*boolean-type*
           :name 'coalton:False
           :patterns nil)))
    (is (coalton-impl/codegen/pattern:patterns-exhaustive-p
         (list true-pattern false-pattern)
         ty
         entry:*global-environment*))))

(deftest test-patterns-exhaustive-collapse-binding-patterns ()
  "Codegen exhaustiveness should match analysis behavior for binding patterns."
  (let ((bound-true-pattern
          (coalton-impl/codegen/pattern:make-pattern-binding
           :type tc:*boolean-type*
           :var (coalton-impl/codegen/pattern:make-pattern-var
                 :type tc:*boolean-type*
                 :name 'x)
           :pattern (coalton-impl/codegen/pattern:make-pattern-constructor
                     :type tc:*boolean-type*
                     :name 'coalton:True
                     :patterns nil)))
        (false-pattern
          (coalton-impl/codegen/pattern:make-pattern-constructor
           :type tc:*boolean-type*
           :name 'coalton:False
           :patterns nil)))
    (is (coalton-impl/codegen/pattern:patterns-exhaustive-p
         (list bound-true-pattern false-pattern)
         tc:*boolean-type*
         entry:*global-environment*))))
