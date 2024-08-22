(in-package #:coalton-native-tests)

(coalton-toplevel
  (repr :transparent)
  (define-type (TransparentTypeTest :t)
    (TransparentTypeTest (coalton-library/lisparray:LispArray :t)))

  (declare complex-type-fn ((Complex :t) -> (Complex :t)))
  (define (complex-type-fn x) x)

  (declare complex-type-fn-1 ((Complex Double-Float) -> (Complex Double-Float)))
  (define (complex-type-fn-1 x) x)

  (declare lisp-array-fn ((coalton-library/lisparray:LispArray :t) -> :t))
  (define (lisp-array-fn x)
    (coalton-library/lisparray:aref x 0))

  (declare lisp-array-fn-1 ((coalton-library/lisparray:LispArray IFix) -> IFix))
  (define (lisp-array-fn-1 x)
    (coalton-library/lisparray:aref x 0))

  (declare transparent-type-fn ((TransparentTypeTest :t) -> :t))
  (define (transparent-type-fn (TransparentTypeTest x))
    (coalton-library/lisparray:aref x 0))

  (declare transparent-type-fn-1 ((TransparentTypeTest IFix) -> IFix))
  (define (transparent-type-fn-1 (TransparentTypeTest x))
    (coalton-library/lisparray:aref x 0))
  )

(in-package #:coalton-tests)

(deftest test-lisp-types ()
  (let ((env coalton-impl/entry:*global-environment*))
    (labels ((coalton-type (name)
               (coalton-impl/typechecker/environment:type-entry-type
                (coalton-impl/typechecker/environment:lookup-type env name)))
             (coalton-type-of (value-name)
               (coalton-impl/typechecker/environment:lookup-value-type env value-name))
             (coalton-type-of-arg1 (value-name)
               (car (coalton-impl/typechecker/types:function-type-arguments
                     (coalton-type-of value-name))))
             (lisp-type (coalton-type)
               (coalton-impl/typechecker/lisp-type:lisp-type coalton-type env)))

      ;; Concrete type mappings
      (is (equal 'single-float (lisp-type (coalton-type 'coalton:single-float))))
      (is (equal 'double-float (lisp-type (coalton-type 'coalton:double-float))))

      ;; A few special cases
      (is (equal '(or number coalton-library/math/complex:complex)
                 (lisp-type (coalton-type-of-arg1 'coalton-native-tests::complex-type-fn))))
      (is (equal '(complex double-float)
                 (lisp-type (coalton-type-of-arg1 'coalton-native-tests::complex-type-fn-1))))
      (is (equal '(simple-array * (*))
                 (lisp-type (coalton-type-of-arg1 'coalton-native-tests::lisp-array-fn))))
      (is (equal '(simple-array fixnum (*))
                 (lisp-type (coalton-type-of-arg1 'coalton-native-tests::lisp-array-fn-1))))

      ;; Transparent type
      (is (equal 'coalton-impl/runtime/function-entry:function-entry
                 (lisp-type (coalton-type-of 'coalton-native-tests::transparent-type-fn))))
      (is (equal '(simple-array * (*))
                 (lisp-type (coalton-type-of-arg1 'coalton-native-tests::transparent-type-fn))))
      (is (equal '(simple-array fixnum (*))
                 (lisp-type (coalton-type-of-arg1 'coalton-native-tests::transparent-type-fn-1))))
      )))
