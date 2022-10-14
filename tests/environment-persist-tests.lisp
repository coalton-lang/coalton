(in-package #:coalton-tests)

(deftest test-persist-value ()
  "Test that the value environment persists across loads and fasl loads"
  (let ((test-expr
          '((in-package #:coalton-native-tests)
            (coalton-toplevel
             (define (test-id a) a)))))

    ;; Write out our test program to a temporary file
    (uiop:with-temporary-file (:stream f
                               :pathname input-file
                               :suffix "lisp"
                               :direction :output)

      (dolist (expr test-expr)
        (write-string (princ-to-string expr) f))

      ;; Close the file so we can start testing
      :close-stream

      ;; Create a temp output fasl file
      (uiop:with-temporary-file (:pathname output-file
                                 :type #+ccl (pathname-type ccl:*.fasl-pathname*)
                                       #+(not ccl) "fasl")
        (handler-bind
            ;; Tell the compiler to be quiet
            ((style-warning (lambda (c)
                              (declare (ignore c))
                              (invoke-restart 'muffle-warning))))
          (let ((entry:*global-environment* (tc:make-default-environment)))
            ;; Be sure that the environment is clean
            (is (null (tc:lookup-value-type entry:*global-environment* 'coalton-native-tests::test-id :no-error t)))

            ;; Load our coalton code and check for function availability
            (load input-file :verbose nil)

            ;; Be sure that we have the correct function type
            (is (tc:lookup-value-type entry:*global-environment* 'coalton-native-tests::test-id :no-error t))

            ;; Ok, now clean the environment
            (setf entry:*global-environment* (tc:make-default-environment))
            (is (null (tc:lookup-value-type entry:*global-environment* 'coalton-native-tests::test-id :no-error t)))

            ;; And compile the file
            (compile-file input-file :output-file output-file :verbose nil :print nil)

            ;; After compiling we should have the function available
            (is (not (null (tc:lookup-value-type entry:*global-environment* 'coalton-native-tests::test-id :no-error t))))
            ;; Clean
            (setf entry:*global-environment* (tc:make-default-environment))
            (is (null (tc:lookup-value-type entry:*global-environment* 'coalton-native-tests::test-id :no-error t)))

            ;; Now load the fasl
            (load output-file :verbose nil)

            ;; Now we should have our function
            (is (not (null (tc:lookup-value-type entry:*global-environment* 'coalton-native-tests::test-id :no-error t))))

            t))))))
