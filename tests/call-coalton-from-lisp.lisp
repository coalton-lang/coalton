(in-package #:coalton-tests)

(deftest call-coalton-from-lisp ()
  (let ((coalton+ (coalton:coalton (coalton:fn (a b)
                                      ;; force this function to be of non-parametric type, so it doesn't get
                                      ;; extra instance args injected
                                      (coalton-prelude:+ (coalton:the coalton:UFix a)
                                                         (coalton:the coalton:UFix b))))))
    (is (typep coalton+ 'coalton-impl/runtime:function-entry))
    (is (= (coalton-impl/runtime:function-entry-arity coalton+)
           2))
    (is (= 3 (coalton:call-coalton-function coalton+ 1 2)))
    (is (= 3 (apply #'coalton:call-coalton-function coalton+ '(1 2))))
    (signals coalton-impl/runtime:coalton-function-arity-mismatch
      (coalton:call-coalton-function coalton+ 1))
    (signals coalton-impl/runtime:coalton-function-arity-mismatch
      (apply #'coalton:call-coalton-function coalton+ (loop :for i :below 100 :collect i)))))

(deftest call-coalton-sign-ufix-from-lisp ()
  (let ((sign-ufix (coalton:coalton
                     (coalton:fn (x)
                       (coalton/math:sign (coalton:the coalton:UFix x))))))
    (is (= 0 (coalton:call-coalton-function sign-ufix 0)))
    (is (= 1 (coalton:call-coalton-function sign-ufix 2)))))

(deftest call-keyword-coalton-from-lisp ()
  (let ((coalton+kw
          (coalton:coalton
           (coalton:fn (a coalton:&key (offset (coalton:the coalton:UFix 1)))
             (coalton-prelude:+ (coalton:the coalton:UFix a)
                                (coalton:the coalton:UFix offset))))))
    (is (typep coalton+kw 'coalton-impl/runtime:function-entry))
    (is (= (coalton-impl/runtime:function-entry-arity coalton+kw)
           1))
    (is (= 3 (coalton:call-coalton-function coalton+kw 2)))
    (is (= 7 (coalton:call-coalton-function coalton+kw 2 :offset 5)))
    (is (= 7 (apply #'coalton:call-coalton-function coalton+kw '(2 :offset 5))))
    (is (= 3 (funcall (coalton-impl/runtime:function-entry-function coalton+kw) 2)))
    (is (= 7 (funcall (coalton-impl/runtime:function-entry-function coalton+kw) 2 :offset 5)))
    (signals coalton-impl/runtime:coalton-function-arity-mismatch
      (coalton:call-coalton-function coalton+kw))
    (signals coalton-impl/runtime:coalton-function-arity-mismatch
      (coalton:call-coalton-function coalton+kw 2 3))))

(deftest call-overloaded-coalton-from-lisp ()
  (let ((coalton+
          (coalton:coalton
           (coalton:the (coalton:UFix * coalton:UFix -> coalton:UFix)
                        coalton-prelude:+))))
    (is (typep coalton+ 'coalton-impl/runtime:function-entry))
    (is (= (coalton-impl/runtime:function-entry-arity coalton+)
           2))
    (is (= (length (coalton-impl/runtime:function-entry-bound-arguments coalton+))
           1))
    (is (= 3 (coalton:call-coalton-function coalton+ 1 2)))
    (is (= 3 (coalton-impl/runtime:exact-call coalton+ 1 2)))
    (signals coalton-impl/runtime:coalton-function-arity-mismatch
      (coalton:call-coalton-function coalton+ 1))))
