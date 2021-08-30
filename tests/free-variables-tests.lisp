;;;; free-variables-tests.lisp

(in-package #:coalton-tests)

(deftest test-free-variables ()
  (let ((sr (coalton-impl/ast::make-shadow-realm))
	(package (symbol-package '@@STUB@@)))
    (is (equalp (coalton-impl/ast::free-variables (coalton-impl::parse-form 5 sr package))
		nil))
    
    (is (equalp (coalton-impl/ast::free-variables (coalton-impl::parse-form "hi" sr package))
		nil))
    
    (is (equalp (coalton-impl/ast::free-variables (coalton-impl::parse-form 'x sr package))
		'(x)))

    ;; Variables captured in a let are not free
    (is (equalp (coalton-impl/ast::free-variables (coalton-impl::parse-form '(coalton:let ((x 5)) x) sr package))
		nil))

    (is (equalp (coalton-impl/ast::free-variables (coalton-impl::parse-form '(coalton:let ((x 5)) (f x)) sr package))
		'(f)))
    ;; Function paramaters are not free
    (is (equalp (coalton-impl/ast::free-variables (coalton-impl::parse-form '(coalton:let ((f (coalton:fn (x) x))) (f 5)) sr package))
		nil))

    ;; Free variable with the same name as a function paramater is free
    (is (equalp (coalton-impl/ast::free-variables (coalton-impl::parse-form '(coalton:let ((f (coalton:fn (x) x))) (f x)) sr package))
		'(x)))))
