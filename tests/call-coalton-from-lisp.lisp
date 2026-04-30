(in-package #:coalton-tests)

(coalton:coalton-toplevel
  (coalton:declare constrained-keyword-from-lisp-target
    (coalton/classes:Num :a coalton:=> coalton:&key (:offset :a) coalton:-> :a))
  (coalton:define (constrained-keyword-from-lisp-target coalton:&key (offset 10))
    offset))

(deftest call-coalton-from-lisp ()
  (let ((coalton+ (coalton:coalton (coalton:fn (a b)
                                      ;; force this function to be of non-parametric type, so it doesn't get
                                      ;; extra instance args injected
                                      (coalton-prelude:+ (coalton:the coalton:UFix a)
                                                         (coalton:the coalton:UFix b))))))
    (is (functionp coalton+))
    (is (= 3 (coalton:call-coalton-function coalton+ 1 2)))
    (is (= 3 (apply #'coalton:call-coalton-function coalton+ '(1 2))))))

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
    (is (functionp coalton+kw))
    (is (= 3 (coalton:call-coalton-function coalton+kw 2)))
    (is (= 7 (coalton:call-coalton-function coalton+kw 2 :offset 5)))
    (is (= 7 (apply #'coalton:call-coalton-function coalton+kw '(2 :offset 5))))
    (is (= 3 (funcall coalton+kw 2)))
    (is (= 7 (funcall coalton+kw 2 :offset 5)))))

(deftest call-constrained-keyword-coalton-from-lisp ()
  (let ((constrained-kw
          (coalton:coalton
           (coalton:the (coalton:&key (:offset coalton:Integer) coalton:-> coalton:Integer)
                        constrained-keyword-from-lisp-target))))
    (is (functionp constrained-kw))
    (is (= 10 (coalton:call-coalton-function constrained-kw)))
    (is (= 6 (coalton:call-coalton-function constrained-kw :offset 6)))
    (is (= 6 (funcall constrained-kw :offset 6)))))

(deftest call-overloaded-coalton-from-lisp ()
  (let ((coalton+
          (coalton:coalton
           (coalton:the (coalton:UFix * coalton:UFix -> coalton:UFix)
                        coalton-prelude:+))))
    (is (functionp coalton+))
    (is (= 3 (coalton:call-coalton-function coalton+ 1 2)))
    (is (= 3 (funcall coalton+ 1 2)))))
