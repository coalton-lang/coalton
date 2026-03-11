(in-package #:coalton-native-tests)

(coalton-toplevel
  (repr :transparent)
  (define-type (TransparentTypeTest :t)
    (TransparentTypeTest (coalton/lisparray:LispArray :t)))

  (declare complex-type-fn ((Complex :t) -> (Complex :t)))
  (define (complex-type-fn x) x)

  (declare complex-type-fn-1 ((Complex Double-Float) -> (Complex Double-Float)))
  (define (complex-type-fn-1 x) x)

  (declare lisp-array-fn ((coalton/lisparray:LispArray :t) -> :t))
  (define (lisp-array-fn x)
    (coalton/lisparray:aref x 0))

  (declare lisp-array-fn-1 ((coalton/lisparray:LispArray IFix) -> IFix))
  (define (lisp-array-fn-1 x)
    (coalton/lisparray:aref x 0))

  (declare transparent-type-fn ((TransparentTypeTest :t) -> :t))
  (define (transparent-type-fn (TransparentTypeTest x))
    (coalton/lisparray:aref x 0))

  (declare transparent-type-fn-1 ((TransparentTypeTest IFix) -> IFix))
  (define (transparent-type-fn-1 (TransparentTypeTest x))
    (coalton/lisparray:aref x 0))

  (define-type (LispArrayMultiTyvar :a :b))
  (declare lisp-array-multi-tyvar-fn ((coalton/lisparray:LispArray (LispArrayMultiTyvar :a :b)) -> (LispArrayMultiTyvar :a :b)))
  (define (lisp-array-multi-tyvar-fn x)
    (coalton/lisparray:aref x 0))

  (declare named-vars-fn ((Tuple :left :right) -> (Tuple :left :right)))
  (define (named-vars-fn pair)
    pair)

  (define-class (NameTrackedClass :monad :state)
    (name-tracked-get (:monad :state)))
  )

(in-package #:coalton-tests)

(deftest test-lisp-types ()
  (let ((env coalton-impl/entry:*global-environment*))
    (labels ((coalton-type (name)
               (coalton-impl/typechecker/environment:type-entry-type
                (coalton-impl/typechecker/environment:lookup-type env name)))
             (coalton-type-of (value-name)
               (coalton-impl/typechecker/environment:lookup-value-type env value-name))
             (render-type (coalton-type)
               (let ((coalton-impl/settings:*coalton-print-unicode* nil))
                 (with-output-to-string (stream)
                   (write coalton-type :stream stream))))
             (coalton-type-of-arg1 (value-name)
               (car (coalton-impl/typechecker/types:function-type-arguments
                     (coalton-type-of value-name))))
             (lisp-type (coalton-type)
               (coalton-impl/typechecker/lisp-type:lisp-type coalton-type env)))

      ;; Concrete type mappings
      (is (equal 'single-float (lisp-type (coalton-type 'coalton:F32))))
      (is (equal 'double-float (lisp-type (coalton-type 'coalton:F64))))

      ;; A few special cases
      (is (equal '(or number coalton/math/complex:complex)
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

      ;; LispArray with multi-tyvar element type
      (is (equal '(simple-array coalton-native-tests::LispArrayMultiTyvar (*))
                 (lisp-type (coalton-type-of-arg1 'coalton-native-tests::lisp-array-multi-tyvar-fn))))

      ;; Preserved type-variable names in printed schemes
      (check-string= "named function type variables"
                     "FORALL :LEFT :RIGHT. ((COALTON/CLASSES:TUPLE :LEFT :RIGHT) -> (COALTON/CLASSES:TUPLE :LEFT :RIGHT))"
                     (render-type (coalton-type-of 'coalton-native-tests::named-vars-fn)))
      (check-string= "class method type variables"
                     "FORALL :MONAD :STATE. COALTON-NATIVE-TESTS::NAMETRACKEDCLASS :MONAD :STATE => (:MONAD :STATE)"
                     (render-type (coalton-type-of 'coalton-native-tests::name-tracked-get)))
      )))

(deftest test-tyvar-source-names-do-not-affect-quantification ()
  (let* ((left (coalton-impl/typechecker:make-tyvar
                :id 0
                :kind coalton-impl/typechecker:+kstar+
                :source-name :left))
         (right (coalton-impl/typechecker:make-tyvar
                 :id 0
                 :kind coalton-impl/typechecker:+kstar+
                 :source-name :right))
         (scheme (coalton-impl/typechecker:quantify
                  (list left)
                  (coalton-impl/typechecker:qualify nil right))))
    (is (coalton-impl/typechecker:ty= left right))
    (is (= 1 (length (coalton-impl/typechecker:ty-scheme-kinds scheme))))))
