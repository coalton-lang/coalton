(cl:in-package #:coalton-native-tests)

(cl:defmacro define-test (name args cl:&body body)
  `(fiasco:deftest (,name :in fiasco-suites::coalton-tests)
       ,args
     (coalton
      (progn ,@body))))

(cl:define-condition coalton-is-assertion (fiasco::test-assertion)
  ((form :initarg :form
         :accessor coalton-is-assertion-form))
  (:documentation "Signaled before evaluating an IS assertion"))

(cl:define-condition coalton-failed-assertion (fiasco::failure)
  ((form :initarg :form
         :accessor coalton-is-assertion-form))
  (:documentation "Signaled when an IS assertion fails")
  (:report (cl:lambda (failure stream)
             (cl:format stream "IS assertion ~a failed" (coalton-is-assertion-form failure)))))

(coalton-toplevel
  (declare %register-assertion (Lisp-Object -> Unit))
  (define (%register-assertion form)
    (progn 
      (lisp :any (form)
        (cl:warn 'coalton-is-assertion
                 :form form))
      Unit))

  (declare %register-success (Unit -> Unit))
  (define (%register-success _)
    (progn 
      (lisp :any ()
        (fiasco::register-assertion-was-successful))
      Unit))

  (declare %register-failure (Lisp-Object -> String -> Unit))
  (define (%register-failure form message)
    (progn
      (lisp :any (form message)
        (fiasco::record-failure 'coalton-failed-assertion
                                :form form
                                :format-control "~a"
                                :format-arguments message))
      Unit))

  (declare %is (Lisp-Object -> (Unit -> Boolean) -> String -> Unit))
  (define (%is form ok? message)
    (progn
      (%register-assertion form)
      (if (ok?)
          (%register-success)
          (%register-failure form message))
      Unit)))

(cl:defmacro is (check cl:&optional (message (cl:format cl:nil "assertion failed: ~a" check)))
  `(%is (lisp Lisp-Object () ',check)
        (fn (_) ,check)
        ,message))
