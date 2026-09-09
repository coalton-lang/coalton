(in-package :fiasco)

;;; Redefine define-test-package, so that it doesn't call make-package.
;;; Code copied from https://github.com/appleby/fiasco/tree/fix-define-test-package-for-abcl
(defmacro define-test-package (name-or-name-with-args &body package-options)
  "Defines a new package and binds to it a new test suite.

The binding between package and suite means that tests defined while
inside this package are automatically added to the associated
suite. Inside the new package, the function RUN-PACKAGE-TESTS is the
preferred way to execute the suite. To run the tests from outside, use
RUN-TESTS.

NAME-OR-NAME-WITH-ARGS names the package and suite to create. It is
either a single symbol NAME, or a list (NAME :IN PARENT-SUITE) where
PARENT-SUITE designated the Fiasco suite previously created with
DEFSUITE that should parent the newly created suite.

Package NAME is defined via normal `defpackage', and in addition to
processing PACKAGE-OPTIONS, automatically USES the :FIASCO and :CL
packages."
  (destructuring-bind (name &key (in 'fiasco-suites::all-tests))
      (alexandria:ensure-list name-or-name-with-args)
    #-abcl
    (unless (find-package name)
      (make-package name :use nil))
    (let ((suite-sym (intern (string name) :fiasco-suites)))
      `(progn
	 (defpackage ,name
	   ,@(append `((:use :fiasco :cl))
		     package-options))
	 (defsuite (,suite-sym :bind-to-package ,name
			       :in ,in))))))
