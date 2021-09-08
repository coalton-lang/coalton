(cl:in-package #:thih-coalton-tests)

(cl:defparameter *initial-env* (coalton-library:fromSome "Failed to init classenv"
                                                         (compose
                                                          addPreludeClasses
                                                          exampleInsts
                                                          initialEnv)))

(deftest test-type-inference ()
  (is (cl:equalp (tiProgram
                  *initial-env*
                  Nil
                  (Program
                   (make-list
                    (BindGroup
                     Nil
                     (make-list
                      (make-list
                       (Impl
                        (Id "f")
                        (make-list
                         (Alt
                          (make-list
                           (PVar (Id "x")))
                          (Var (Id "x")))))))))))
              
                 (make-list
                  (Assump
                   (Id "f")
                   (Forall
                    (make-list Star)
                    (Qual Nil
                          (mkFn (TGen 0)
                                (TGen 0)))))))))
