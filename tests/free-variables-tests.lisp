;;;; free-variables-tests.lisp

(in-package #:coalton-tests)

(deftest test-free-variables ()
  (let ((m (coalton-impl/ast::make-immutable-map))
        (package (symbol-package '@@STUB@@)))
    (is (equalp (coalton-impl/ast::free-variables (coalton-impl::parse-form 5 m package))
                nil))

    (is (equalp (coalton-impl/ast::free-variables (coalton-impl::parse-form "hi" m package))
                nil))

    (is (equalp (coalton-impl/ast::free-variables (coalton-impl::parse-form 'x m package))
                '(x)))

    ;; Variables captured in a let are not free
    (is (equalp (coalton-impl/ast::free-variables (coalton-impl::parse-form '(coalton:let ((x 5)) x) m package))
                nil))

    (is (equalp (coalton-impl/ast::free-variables (coalton-impl::parse-form '(coalton:let ((x 5)) (f x)) m package))
                '(f)))
    ;; Function paramaters are not free
    (is (equalp (coalton-impl/ast::free-variables (coalton-impl::parse-form '(coalton:let ((f (coalton:fn (x) x))) (f 5)) m package))
                nil))

    ;; Free variable with the same name as a function paramater is free
    (is (equalp (coalton-impl/ast::free-variables (coalton-impl::parse-form '(coalton:let ((f (coalton:fn (x) x))) (f x)) m package))
                '(x)))))
