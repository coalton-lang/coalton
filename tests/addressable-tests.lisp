(in-package #:coalton-tests)

#+broken
(eval-when (:compile-toplevel :load-toplevel)
  (defun addressable-define-same? (type-name)
    `((coalton:declare same? (,type-name coalton:-> ,type-name coalton:-> coalton:Boolean))
      (coalton:define (same? a b) (coalton-library/addressable:eq? a b))))
  (defun addressable-define-instance (type-name)
    `(coalton:define-instance (coalton-library/addressable:Addressable ,type-name)
       (coalton:define (coalton-library/addressable:eq? a b)
         (coalton-library/builtin:error "doesn't matter")))))

#+broken
(defun addressable-define-type-and-same? (type-name &rest type-definition)
  `(,@type-definition
    ,@(addressable-define-same? type-name)))

#+broken
(defun addressable-define-type-and-instance (type-name &rest type-definition)
  `(,@type-definition
    ,@(addressable-define-instance type-name)))

#+broken
(deftest addressable-naughty-auto-instances ()
  (macrolet ((errors (&body (body))
               `(signals coalton-impl/typechecker::coalton-type-error
                  (run-coalton-typechecker
                   ,body))))
    ;; unspecified repr doesn't get an automatic instance
    (errors
      (addressable-define-type-and-same? 'Foo
        '(coalton:define-type Foo Foo)))
    ;; transparent repr doesn't get an automatic instance
    (errors
      (addressable-define-type-and-same? 'Foo
        '(coalton:repr :transparent)
        '(coalton:define-type Foo (Foo coalton:UFix))))
    ;; native repr doesn't get an automatic instance
    (errors
      (addressable-define-type-and-same? 'Foo
        '(coalton:repr :native t)
        '(coalton:define-type Foo)))))

#+broken
(deftest addressable-naughty-explicit-instances ()
  (macrolet ((errors (&body (forms))
               `(signals coalton-impl/ast::coalton-parse-error
                  (run-coalton-typechecker ,forms))))
    ;; unspecified repr cannot define instances
    (errors
     (addressable-define-type-and-instance 'Foo
       '(coalton:define-type Foo Foo)))
    ;; transparent repr cannot define instances
    (errors
     (addressable-define-type-and-instance 'Foo
       '(coalton:repr :transparent)
       '(coalton:define-type Foo (Foo coalton:UFix))))
    ;; enum repr cannot define instances
    (errors
     (addressable-define-type-and-instance 'Foo
       '(coalton:repr :enum)
       '(coalton:define-type Foo Foo)))
    ;; lisp repr cannot define instances
    (errors
     (addressable-define-type-and-instance 'Foo
       '(coalton:repr :lisp)
       '(coalton:define-type Foo Foo)))))

#+broken
(deftest addressable-automatic-instances ()
  (macrolet ((same-good-type (type-name &body (forms))
               `(check-coalton-types
                 ,forms
                 '((same? . (,type-name coalton:-> ,type-name coalton:-> coalton:Boolean))))))
    ;; explicit repr lisp gets addressable automatically
    (same-good-type Foo
                    (addressable-define-type-and-same? 'Foo
                      '(coalton:repr :lisp)
                      '(coalton:define-type Foo Foo)))
    ;; so does explicit repr enum
    (same-good-type Foo
                    (addressable-define-type-and-same? 'Foo
                      '(coalton:repr :enum)
                      '(coalton:define-type Foo Foo)))))

#+broken
(deftest addressable-explicit-instance ()
  (check-coalton-types
   `((coalton:repr :native t)
     (coalton:define-type Foo)
     ,(addressable-define-instance 'Foo)
     ,@(addressable-define-same? 'Foo))
   '((same? . (Foo coalton:-> Foo coalton:-> coalton:Boolean)))))
