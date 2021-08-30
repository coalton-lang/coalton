(in-package #:coalton-tests)

(deftest test-persist-value ()
  "Test that the value environment persists across loads and fasl loads"
  (let ((test-expr
          '((in-package #:coalton-user)
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
                                 :suffix "fasl")
        (handler-bind
            ;; Tell the compiler to be quiet
            ((style-warning (lambda (c)
                              (declare (ignore c))
                              (invoke-restart 'muffle-warning))))
          (let ((coalton-impl::*global-environment* (make-default-environment)))
            ;; Be sure that the environment is clean
            (is (null (coalton-impl::lookup-value-type coalton-impl::*global-environment* 'coalton-user::test-id :no-error t)))

            ;; Load our coalton code and check for function availability
            (load input-file :verbose nil)

            ;; Be sure that we have the correct function type
            (is (coalton-impl::lookup-value-type coalton-impl::*global-environment* 'coalton-user::test-id :no-error t))

            ;; Ok, now clean the environment
            (setf coalton-impl::*global-environment* (make-default-environment))
            (is (null (coalton-impl::lookup-value-type coalton-impl::*global-environment* 'coalton-user::test-id :no-error t)))

            ;; And compile the file
            (compile-file input-file :output-file output-file :verbose nil :print nil)

            ;; After compiling we should have the function available
            (is (not (null (coalton-impl::lookup-value-type coalton-impl::*global-environment* 'coalton-user::test-id :no-error t))))
            ;; Clean
            (setf coalton-impl::*global-environment* (make-default-environment))
            (is (null (coalton-impl::lookup-value-type coalton-impl::*global-environment* 'coalton-user::test-id :no-error t)))

            ;; Now load the fasl
            (load output-file :verbose nil)

            ;; Now we should have our function
            (is (not (null (coalton-impl::lookup-value-type coalton-impl::*global-environment* 'coalton-user::test-id :no-error t))))

            t))))))
