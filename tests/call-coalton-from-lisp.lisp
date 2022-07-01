(in-package #:coalton-tests)

(deftest call-coalton-from-lisp ()
  (let ((coalton+ (coalton:coalton (coalton:fn (a b)
                                      ;; force this function to be of non-parametric type, so it doesn't get
                                      ;; extra instance args injected
                                      (coalton-prelude:+ (coalton:the coalton:UFix a)
                                                         (coalton:the coalton:UFix b))))))
    (is (typep coalton+ 'coalton-impl/codegen/function-entry::function-entry))
    (is (= (coalton-impl/codegen/function-entry::function-entry-arity coalton+)
           2))
    (is (= 3 (coalton:call-coalton-function coalton+ 1 2)))
    (is (= 3 (apply #'coalton:call-coalton-function coalton+ '(1 2))))
    (signals coalton-impl/codegen/function-entry:too-many-arguments-to-coalton-function
      (apply #'coalton:call-coalton-function coalton+ (loop :for i :below 100 :collect i)))))
