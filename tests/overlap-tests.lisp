(in-package #:coalton-tests)

(defmacro with-overlap-environment (&body body)
  `(let ((*package* (make-package (gensym "OVERLAP-") :use '("COALTON" "COALTON-PRELUDE")))
         (entry:*global-environment* entry:*global-environment*))
     (unwind-protect (handler-bind ((style-warning #'muffle-warning)) ,@body)
       (delete-package *package*))))

(defun overlap-compile (text)
  (let ((source (source:make-source-string text)))
    (with-open-stream (stream (source:source-stream source))
      (eval (entry:compile-coalton-toplevel
             (parser:with-reader-context stream (parser:read-program stream source)))))))

(defun overlap-eval (text)
  (eval (list 'coalton:coalton (read-from-string text))))

(deftest overlap-manual-examples ()
  (with-overlap-environment
    (overlap-compile
     "(define-class (Label :a) (label (:a -> String)))
      (overlap) (define-instance (Label :a) (define (label _) \"value\"))
      (overlap) (define-instance (Label Integer) (define (label _) \"integer\"))
      (declare describe (Label :a => :a -> String))
      (define (describe x) (label x))
      (define-class (PairLabel :a :b) (pair-label (:a * :b -> String)))
      (overlap) (define-instance (PairLabel :a Integer)
                  (define (pair-label _ _) \"integer on the right\"))
      (overlap) (define-instance (PairLabel Integer :b)
                  (define (pair-label _ _) \"integer on the left\"))
      (overlap) (define-instance (PairLabel Integer Integer)
                  (define (pair-label _ _) \"two integers\"))
      (declare to-seq
        (Into (:f :a) (coalton/seq:Seq :a) => :f :a -> coalton/seq:Seq :a))
      (define (to-seq xs) (into xs))")
    (is (equal "value" (overlap-eval "(label True)")))
    (is (equal "integer" (overlap-eval "(describe (the Integer 42))")))
    (is (equal "integer on the right" (overlap-eval "(pair-label True (the Integer 1))")))
    (is (equal "integer on the left" (overlap-eval "(pair-label (the Integer 1) True)")))
    (is (equal "two integers" (overlap-eval "(pair-label (the Integer 1) (the Integer 2))")))
    (is (= 1 (overlap-eval "(coalton/seq:size (to-seq (Some (the Integer 42))))")))
    (is (= 2 (overlap-eval "(coalton/seq:size (to-seq (the (List Integer) (make-list 1 2))))")))))

(deftest overlap-library-wrapper-accepts-later-specialization ()
  (with-overlap-environment
    (overlap-compile
     "(define-type (Bag :a) (Bag :a))
      (define-instance (Foldable Bag)
        (define (fold f init (Bag x)) (f init x))
        (define (foldr f init (Bag x)) (f x init)))
      (declare to-seq
        (Into (:f :a) (coalton/seq:Seq :a) => :f :a -> coalton/seq:Seq :a))
      (define (to-seq xs) (into xs))")
    (overlap-compile
     "(overlap)
      (define-instance (Into (Bag Integer) (coalton/seq:Seq Integer))
        (define (into (Bag x)) (coalton/seq:make x x)))")
    (is (= 2 (overlap-eval "(coalton/seq:size (to-seq (Bag (the Integer 42))))")))
    (is (= 1 (overlap-eval "(coalton/seq:size (to-seq (Bag True)))")))))

(deftest overlap-attributes ()
  (dolist (text '("(overlap 1) (define-class (C :a))"
                  "(overlap) (overlap) (define-instance (Eq Integer))"
                  "(overlap) (define x 1)"
                  "(overlap) (declare x Integer)"
                  "(overlap) (define-type X X)"
                  "(overlap) (define-exception X X)"
                  "(overlap) (define-resumption X Unit)"
                  "(overlap) (define-class (C :a))"
                  "(overlap)"))
    (signals parser:parse-error (check-coalton-types text)))
  (dolist (instances '("(define-instance (C :a)) (overlap) (define-instance (C Integer))"
                       "(overlap) (define-instance (C :a)) (define-instance (C Integer))"
                       "(define-instance (C Integer)) (overlap) (define-instance (C :a))"))
    (signals tc:tc-error
      (check-coalton-types (concatenate 'string "(define-class (C :a))" instances)))))

(deftest overlap-specificity-and-deferred-evidence ()
  (dolist (instances
            '("(overlap) (define-instance (Pick :a) (define (pick _) 1))
               (overlap) (define-instance (Pick Integer) (define (pick _) 2))"
              "(overlap) (define-instance (Pick Integer) (define (pick _) 2))
               (overlap) (define-instance (Pick :a) (define (pick _) 1))"))
    (with-overlap-environment
      (overlap-compile (concatenate 'string
                                   "(define-class (Pick :a) (pick (:a -> Integer)))" instances
                                   "(define (forward x) (pick x))"))
      (is (= 2 (overlap-eval "(forward (the Integer 42))")))
      (is (= 1 (overlap-eval "(forward True)")))
      (is (= 1 (length (tc:qualified-ty-predicates
                        (tc:fresh-inst (tc:lookup-value-type entry:*global-environment*
                                                             (intern "FORWARD"))))))))))

(deftest overlap-intersections ()
  (with-overlap-environment
    (overlap-compile
     "(define-class (Choose :a :b) (choose (:a * :b -> Integer)))
      (overlap) (define-instance (Choose :a Integer) (define (choose _ _) 1))
      (overlap) (define-instance (Choose Integer :b) (define (choose _ _) 2))")
    (signals tc:ambiguous-instance-error (overlap-eval "(choose (the Integer 1) (the Integer 2))"))
    (overlap-compile
     "(overlap) (define-instance (Choose Integer Integer) (define (choose _ _) 3))")
    (is (= 3 (overlap-eval "(choose (the Integer 1) (the Integer 2))")))
    (is (= 1 (overlap-eval "(choose True (the Integer 2))")))
    (is (= 2 (overlap-eval "(choose (the Integer 1) True)")))))

(deftest overlap-does-not-backtrack-through-contexts ()
  (with-overlap-environment
    (overlap-compile
     "(define-class (Missing :a))
      (define-class (Pick :a) (pick (:a -> Integer)))
      (overlap) (define-instance (Pick :a) (define (pick _) 1))
      (overlap) (define-instance (Missing Integer => Pick Integer) (define (pick _) 2))")
    (signals tc:tc-error (overlap-eval "(pick (the Integer 42))"))))

(deftest overlap-keeps-functional-dependencies-consistent ()
  (signals tc:tc-error
    (check-coalton-types
     "(define-class (C :a :b (:a -> :b)))
      (overlap) (define-instance (C :a :a))
      (overlap) (define-instance (C Integer String))")))

(deftest overlap-redefinition-rechecks-markers ()
  (with-overlap-environment
    (overlap-compile
     "(define-class (C :a))
      (overlap) (define-instance (C :a))
      (overlap) (define-instance (C Integer))")
    (signals tc:tc-error (overlap-compile "(define-instance (C :a))"))))

(deftest overlap-late-specialization-keeps-generic-clients ()
  (with-overlap-environment
    (overlap-compile
     "(define-class (Pick :a) (pick (:a -> Integer)))
      (overlap) (define-instance (Pick :a) (define (pick _) 1))
      (define (forward x) (pick x))")
    (overlap-compile "(overlap) (define-instance (Pick Integer) (define (pick _) 2))")
    (is (= 2 (overlap-eval "(forward (the Integer 42))")))))

(deftest overlap-rejects-stale-compiled-choices ()
  (with-overlap-environment
    (overlap-compile
     "(define-class (Pick :a) (pick (:a -> Integer)))
      (overlap) (define-instance (Pick :a) (define (pick _) 1))
      (define (old-client) (pick (the Integer 42)))")
    (let ((before entry:*global-environment*))
      (signals tc:stale-instance-selection-error
        (overlap-compile "(overlap) (define-instance (Pick Integer) (define (pick _) 2))"))
      (is (eq before entry:*global-environment*)))
    (is (= 1 (overlap-eval "(old-client)")))))

(deftest overlap-fasl-validates-both-load-orders ()
  (with-overlap-environment
    (overlap-compile
     "(define-class (Pick :a) (pick (:a -> Integer)))
      (overlap) (define-instance (Pick :a) (define (pick _) 1))")
    (let ((base entry:*global-environment*))
      (uiop:with-temporary-file (:stream out :pathname source-file :type "lisp")
        (format out "(in-package ~S)~%(coalton-toplevel (define (saved-client) (pick (the Integer 42))))~%"
                (package-name *package*))
        :close-stream
        (uiop:with-temporary-file (:pathname fasl-file
                                  :type (pathname-type (compile-file-pathname source-file)))
          (multiple-value-bind (file warnings failure)
              (compile-file source-file :output-file fasl-file :verbose nil :print nil)
            (declare (ignore file warnings))
            (is (not failure)))
          ;; Specialization first: reject the stale FASL before installing its code.
          (setf entry:*global-environment* base)
          (overlap-compile "(overlap) (define-instance (Pick Integer) (define (pick _) 2))")
          (let ((before entry:*global-environment*))
            (signals tc:stale-instance-selection-error (load fasl-file :verbose nil))
            (is (eq before entry:*global-environment*)))
          ;; Client first: reject a later specialization, preserving the old world.
          (setf entry:*global-environment* base)
          (load fasl-file :verbose nil)
          (signals tc:stale-instance-selection-error
            (overlap-compile "(overlap) (define-instance (Pick Integer) (define (pick _) 2))"))
          (is (= 1 (overlap-eval "(saved-client)"))))))))

(deftest overlap-instance-context-abi-is-validated ()
  (with-overlap-environment
    (overlap-compile
     "(define-class (Extra :a))
      (define-instance (Extra Integer))
      (define-class (Pick :a) (pick (:a -> Integer)))
      (overlap) (define-instance (Pick :a) (define (pick _) 1))
      (define (client) (pick (the Integer 42)))")
    (signals tc:stale-instance-selection-error
      (overlap-compile "(overlap) (define-instance (Extra :a => Pick :a) (define (pick _) 1))"))
    ;; Equivalent definitions do not invalidate dictionary choices.
    (overlap-compile "(overlap) (define-instance (Pick :b) (define (pick _) 1))")
    (is (= 1 (overlap-eval "(client)")))))

(deftest overlap-specialization-preserves-dictionary-arguments ()
  (with-overlap-environment
    (overlap-compile
     "(define-class (Pick :a) (pick (:a -> Integer)))
      (overlap) (define-instance (Pick :a) (define (pick _) 1))
      (define (forward x) (pick x))
      (declare fast (Integer -> Integer)) (define (fast _) 1)
      (specialize forward fast (Integer -> Integer))")
    (let* ((env entry:*global-environment*)
           (dict-ty (coalton-impl/codegen/resolve-instance:pred-type
                     (tc:make-ty-predicate :class (intern "PICK") :types (list tc:*integer-type*)) env))
           (dict (ast:make-node-variable :type dict-ty :value (gensym "PASSED-DICT")))
           (call (ast:make-node-application
                  :type tc:*integer-type* :properties nil
                  :rator (ast:make-node-variable
                          :value (intern "FORWARD")
                          :type (tc:make-function-type* (list dict-ty tc:*integer-type*) tc:*integer-type*))
                  :rands (list dict (ast:make-node-literal :type tc:*integer-type* :value 42))))
           (result (coalton-impl/codegen/specializer:apply-specializations call env)))
      (is (eq (intern "FORWARD") (ast:node-variable-value (ast:node-application-rator result))))
      (is (= 2 (length (ast:node-application-rands result)))))))

(deftest overlap-validates-optimizer-negative-assumptions ()
  (with-overlap-environment
    ;; There need not be any selected instance: specialization can discard an
    ;; explicitly supplied dictionary before the class has its first instance.
    (overlap-compile
     "(define-class (Pick :a) (pick (:a -> Integer)))
      (define (forward x) (pick x))
      (declare fast (Integer -> Integer)) (define (fast _) 1)
      (specialize forward fast (Integer -> Integer))
      (declare caller (Pick Integer => Integer -> Integer))
      (define (caller x) (forward x))")
    (if coalton-impl/settings:*coalton-disable-specialization*
        (progn
          ;; Without specialization, CALLER still takes a dictionary and makes
          ;; no assumption that would prevent adding this instance.
          (overlap-compile "(overlap) (define-instance (Pick Integer) (define (pick _) 2))")
          (is (= 2 (overlap-eval "(caller (the Integer 42))"))))
        (signals tc:stale-instance-selection-error
          (overlap-compile "(overlap) (define-instance (Pick Integer) (define (pick _) 2))")))))

(deftest overlap-superclass-projection-preserves-evidence ()
  (with-overlap-environment
    (overlap-compile
     "(define-class (Pick :a) (pick (:a -> Integer)))
      (overlap) (define-instance (Pick :a) (define (pick _) 1))
      (overlap) (define-instance (Pick Integer) (define (pick _) 2))
      (define-class (Pick :a => Sub :a))
      (define-instance (Pick :a => Sub :a))")
    (let* ((env entry:*global-environment*)
           (pick-pred (tc:make-ty-predicate :class (intern "PICK") :types (list tc:*integer-type*)))
           (sub-pred (tc:make-ty-predicate :class (intern "SUB") :types (list tc:*integer-type*)))
           (pick-ty (coalton-impl/codegen/resolve-instance:pred-type pick-pred env))
           (sub-ty (coalton-impl/codegen/resolve-instance:pred-type sub-pred env))
           (general (find-if (lambda (instance) (tc:type-variables (tc:ty-class-instance-predicate instance)))
                             (tc:lookup-class-instances env (intern "PICK"))))
           (sub (first (tc:lookup-class-instances env (intern "SUB"))))
           (dict (ast:make-node-application
                  :type sub-ty :properties nil
                  :rator (ast:make-node-variable
                          :type (tc:make-function-type pick-ty sub-ty)
                          :value (tc:ty-class-instance-codegen-sym sub))
                  :rands (list (ast:make-node-variable :type pick-ty
                                                      :value (tc:ty-class-instance-codegen-sym general)))))
           (projection (ast:make-node-field
                        :type pick-ty :dict dict
                        :name (caar (tc:ty-class-superclass-map (tc:lookup-class env (intern "SUB"))))))
           (result (coalton-impl/codegen/optimizer::resolve-static-superclass projection env)))
      (is (ast:node-field-p result))
      (is (eq (tc:ty-class-instance-codegen-sym general)
              (ast:node-variable-value (first (ast:node-application-rands (ast:node-field-dict result)))))))))

(deftest overlap-fasl-generic-client-accepts-later-specialization ()
  (with-overlap-environment
    (overlap-compile
     "(define-class (Pick :a) (pick (:a -> Integer)))
      (overlap) (define-instance (Pick :a) (define (pick _) 1))")
    (let ((base entry:*global-environment*))
      (uiop:with-temporary-file (:stream out :pathname source-file :type "lisp")
        (format out "(in-package ~S)~%(coalton-toplevel (define (saved-forward x) (pick x)))~%"
                (package-name *package*))
        :close-stream
        (uiop:with-temporary-file (:pathname fasl-file
                                  :type (pathname-type (compile-file-pathname source-file)))
          (multiple-value-bind (file warnings failure)
              (compile-file source-file :output-file fasl-file :verbose nil :print nil)
            (declare (ignore file warnings))
            (is (not failure)))
          (setf entry:*global-environment* base)
          (overlap-compile "(overlap) (define-instance (Pick Integer) (define (pick _) 2))")
          (load fasl-file :verbose nil)
          (is (= 2 (overlap-eval "(saved-forward (the Integer 42))"))))))))

(deftest overlap-lisp-expression-fasl-records-choices ()
  (with-overlap-environment
    (overlap-compile
     "(define-class (Pick :a) (pick (:a -> Integer)))
      (overlap) (define-instance (Pick :a) (define (pick _) 1))")
    (let ((base entry:*global-environment*))
      (uiop:with-temporary-file (:stream out :pathname source-file :type "lisp")
        (format out "(in-package ~S)~%(cl:defun saved-lisp-client () (coalton (pick (the Integer 42))))~%"
                (package-name *package*))
        :close-stream
        (uiop:with-temporary-file (:pathname fasl-file
                                  :type (pathname-type (compile-file-pathname source-file)))
          (multiple-value-bind (file warnings failure)
              (compile-file source-file :output-file fasl-file :verbose nil :print nil)
            (declare (ignore file warnings))
            (is (not failure)))
          (setf entry:*global-environment* base)
          (load fasl-file :verbose nil)
          (is (= 1 (funcall (intern "SAVED-LISP-CLIENT"))))
          (signals tc:stale-instance-selection-error
            (overlap-compile "(overlap) (define-instance (Pick Integer) (define (pick _) 2))"))
          (setf entry:*global-environment* base)
          (overlap-compile "(overlap) (define-instance (Pick Integer) (define (pick _) 2))")
          (signals tc:stale-instance-selection-error (load fasl-file :verbose nil)))))))

(deftest overlap-structured-constraints-remain-open ()
  (check-coalton-types
   "(define-class (C :a) (m (:a -> Integer)))
    (overlap) (define-instance (C (List :a)) (define (m _) 1))
    (define (f xs) (m (Cons True xs)))"
   '("f" . "(List Boolean -> Integer)"))
  (check-coalton-types
   "(define-class (C :a) (m (:a -> Integer)))
    (overlap) (define-instance (C (List :a)) (define (m _) 1))
    (define (f x xs) (m (Cons x xs)))"
   '("f" . "(C (List :a) => :a * List :a -> Integer)")))
