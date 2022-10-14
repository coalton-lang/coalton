(cl:in-package #:coalton-testing)

(cl:defmacro coalton-fiasco-init (test-system)
  "Allows for the use of `define-test' to define a fiasco test within a given
test-package `test-system'."
  `(cl:defmacro ,(cl:intern "DEFINE-TEST") (name args cl:&body body)
     "Defines a fiasco test NAME with parameters ARGS and evaluates the
BODY within a `coalton' expression."
     (cl:let ((test-system
                ',(cl:find-symbol
                   (cl:package-name test-system)
                   :fiasco-suites)))
       `(fiasco:deftest (,name :in ,test-system)
            ,args
          (coalton
           (progn ,@body))))))

(cl:define-condition coalton-test-assertion (fiasco::test-assertion)
  ()
  (:documentation "Signaled when an assertion such as IS is encountered from coalton"))

(cl:define-condition coalton-failure (fiasco::failure)
  ()
  (:documentation "Signaled when an assertion such as IS fails from coalton"))

(coalton-toplevel
  (declare %register-assertion (Unit -> Unit))
  (define (%register-assertion _)
    (progn 
      (lisp :any ()
        (cl:warn 'coalton-test-assertion))
      Unit))

  (declare %register-success (Unit -> Unit))
  (define (%register-success _)
    (progn 
      (lisp :any ()
        (fiasco::register-assertion-was-successful))
      Unit)))

(cl:defmacro is (check cl:&optional (message "")) 
  ;; What I'm doing here is a simplified version of what fiasco does,
  ;; which is check if try to expand one layer of function application
  (cl:check-type message cl:string)
  (cl:cond
    ((cl:and (alexandria:proper-list-p check)
             (cl:or (cl:not (cl:symbolp (cl:first check)))
                    (cl:and (cl:not (cl:macro-function (cl:first check)))
                            (cl:not (cl:eql 'coalton:if (cl:first check))))))
     (cl:let* ((rator (cl:first check))
               (rands (cl:rest check))
               (name-els (cl:loop :for rand :in (cl:cons rator rands) :collect (cl:list (cl:gensym) rand)))
               (names (cl:mapcar #'cl:first name-els)))
       `(progn
          (%register-assertion) 
          ,@(cl:loop :for (name value) :in name-els
               :collect `(let ,name = ,value))
          (if ,names
              (%register-success)
              (lisp :a ,names
                (fiasco::record-failure
                 'coalton-failure
                 :format-control "IS assertion ~A~%Evaluates to application ~A~%Evaluates to False~%~A"
                 :format-arguments (cl:list ',check ,(cons 'cl:list names) ,message)))))))
    (cl:t
     `(progn
        (%register-assertion)
        (if ,check
            (%register-success)
            (lisp :a ()
              (fiasco::record-failure
               'coalton-failure
               :format-control "IS assertion ~A~%Evaluates to False~%~A"
               :format-arguments (cl:list ',check ,message))))))))

(cl:defmacro matches (pattern expr cl:&optional (message ""))
  (cl:check-type message cl:string)
  (cl:let ((result (cl:gensym)))
    `(progn
       (%register-assertion)
       (let ,result = ,expr)
       (match ,result
         (,pattern (%register-success))
         (_ (lisp Unit (,result)
              (fiasco::record-failure
               'coalton-failure
               :format-control "~A => ~A does not match pattern ~A~%~A"
               :format-arguments (cl:list ',expr ,result ',pattern ,message))))))))
