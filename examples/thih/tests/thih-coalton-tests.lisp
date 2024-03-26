(in-package #:thih-coalton/tests)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define *initial-env* (from-some "Failed to init classenv"
                                   (th:compose
                                    th:addPreludeClasses
                                    th:exampleInsts
                                    th:initialEnv))))

(define-test test-thih-type-inference ()
  (is (th::assumption-list-equal (th:tiProgram
                                  *initial-env*
                                  Nil
                                  (th:Program
                                   (make-list
                                    (th:BindGroup
                                     Nil
                                     (make-list
                                      (make-list
                                       (th:Impl
                                        (th:Id "f")
                                        (make-list
                                         (th:Alt
                                          (make-list
                                           (th:PVar (th:Id "x")))
                                          (th:Var (th:Id "x")))))))))))
              
                                 (make-list
                                  (th:Assump
                                   (th:Id "f")
                                   (th:Forall
                                    (make-list th:Star)
                                    (th:Qual Nil
                                             (th:mkFn (th:TGen 0)
                                                      (th:TGen 0)))))))))
