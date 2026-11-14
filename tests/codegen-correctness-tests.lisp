(in-package #:coalton-tests)

(defvar *codegen-events* nil)

(defmacro with-codegen-test-environment (&body body)
  `(let ((*package* (make-package (gensym "CODEGEN-")
                                  :use '("COALTON" "COALTON-PRELUDE")))
         (entry:*global-environment* entry:*global-environment*)
         (*codegen-events* nil))
     (unwind-protect
          (handler-bind ((style-warning #'muffle-warning)) ,@body)
       (delete-package *package*))))

(defun codegen-test-compile (text)
  (let ((source (source:make-source-string text)))
    (with-open-stream (stream (source:source-stream source))
      (eval (entry:compile-coalton-toplevel
             (parser:with-reader-context stream (parser:read-program stream source)))))))

(defun codegen-test-eval (text)
  (eval (list 'coalton:coalton (read-from-string text))))

(defun codegen-test-event-recorder ()
  (codegen-test-compile
   "(declare observe (Integer -> Integer))
    (define (observe x)
      (lisp (-> Integer) (x) (cl:push x coalton-tests::*codegen-events*) x))"))

(deftest codegen-struct-accessors ()
  (with-codegen-test-environment
    (codegen-test-event-recorder)
    (codegen-test-compile
     "(define-struct Point (x F64) (y F64))
      (define-struct (Boxed :a) (item :a))
      (define-struct Callbacks
        (unary (Integer -> Integer))
        (nullary (Void -> Integer))
        (keyed (Integer &key (:offset Integer) -> Integer)))
      (declare get-x (Point -> F64))
      (define get-x .x)
      (declare unbox (Boxed :a -> :a))
      (define (unbox box) (.item box))
      (declare get-keyed (Callbacks -> (Integer &key (:offset Integer) -> Integer)))
      (define (get-keyed callbacks) (.keyed callbacks))
      (declare add-offset (Integer &key (:offset Integer) -> Integer))
      (define (add-offset n &key (offset 1)) (+ n offset))
      (define saved-callbacks
        (Callbacks (fn (n) (+ n 1)) (fn () 42) add-offset))")
    (is (= 42.0d0 (codegen-test-eval "(get-x (Point 42.0d0 7.0d0))")))
    (is (equal '(1.0d0 3.0d0)
               (codegen-test-eval "(map .x (make-list (Point 1.0d0 2.0d0)
                                                      (Point 3.0d0 4.0d0)))")))
    (is (= 42 (codegen-test-eval "(unbox (Boxed (the Integer 42)))")))
    (is (equal "answer" (codegen-test-eval "(unbox (Boxed \"answer\"))")))
    (is (= 42.0d0 (codegen-test-eval "(.item (.item (Boxed (Boxed 42.0d0))))")))
    (is (= 42 (codegen-test-eval "((.unary saved-callbacks) 41)")))
    (is (= 42 (codegen-test-eval "((.nullary saved-callbacks))")))
    (is (= 42 (codegen-test-eval "((get-keyed saved-callbacks) 40 :offset 2)")))
    (is (= 42 (codegen-test-eval "((get-keyed saved-callbacks) 41)")))
    ;; A reader must evaluate its receiver once, before using the field.
    (is (= 42 (codegen-test-eval "(.item (Boxed (observe 42)))")))
    (is (equal '(42) *codegen-events*))
    (setf *codegen-events* nil)
    (is (= 3 (codegen-test-eval
              "((.unary (progn (observe 1) saved-callbacks)) (observe 2))")))
    (is (equal '(1 2) (reverse *codegen-events*)))))

(deftest codegen-struct-readers-are-direct-across-compilation-units ()
  (with-codegen-test-environment
    (let ((initial-env entry:*global-environment*))
      (multiple-value-bind (source-file fasl-file)
          (compile-and-load-forms
           (list `(in-package ,(package-name *package*))
                 (read-from-string
                  "(coalton-toplevel (define-struct Point (x F64) (y F64)))")))
        (unwind-protect
             (progn
               ;; Recover the reader metadata from the FASL, not the environment
               ;; that was modified when compiling the struct definition.
               (setf entry:*global-environment* initial-env)
               (load fasl-file)
               (let ((point (intern "POINT")))
                 (with-codegen-test-environment
                   (import point)
                   (codegen-test-compile
                    "(declare coordinate-sum (Point -> F64))
                     (define (coordinate-sum point) (+ (.x point) (.y point)))")
                   (let ((readers nil))
                     (traverse:traverse
                      (tc:lookup-code entry:*global-environment* (intern "COORDINATE-SUM"))
                      (list
                       (traverse:action (:after ast:node-direct-application app)
                         (push (ast:node-direct-application-rator app) readers)
                         (values))))
                     ;; FUNCALL through the reader's function-valued global hides
                     ;; its inline definition and field type from the Lisp compiler.
                     (dotimes (index 2)
                       (is (member (tc:struct-field-accessor-name point index) readers))))
                   (is (= 42.0d0 (codegen-test-eval
                                  "(coordinate-sum (Point 20.0d0 22.0d0))"))))))
          (delete-file source-file)
          (delete-file fasl-file))))))

(deftest codegen-struct-reader-metadata-redefinition ()
  (with-codegen-test-environment
    ;; Exercise compiler environment updates without redefining a live Lisp
    ;; struct, which need not support incompatible layout changes.
    (labels ((define-type (text)
               (let ((source (source:make-source-string text)))
                 (with-open-stream (stream (source:source-stream source))
                   (multiple-value-bind (form env)
                       (entry:entry-point
                        (parser:with-reader-context stream (parser:read-program stream source)))
                     (declare (ignore form))
                     (setf entry:*global-environment* env)))))
             (reader (index)
               (tc:lookup-function entry:*global-environment*
                                   (tc:struct-field-accessor-name (intern "POINT") index)
                                   :no-error t)))
      (define-type "(define-struct Point (x F64) (y F64))")
      (is (reader 0))
      (is (reader 1))
      (define-type "(define-struct Point (x F64))")
      (is (reader 0))
      (is (null (reader 1)))
      (define-type "(repr :transparent) (define-struct Point (x F64))")
      (is (null (reader 0)))
      (define-type "(define-struct Point (x F64))")
      (is (reader 0))
      (define-type "(define-type Point (Point F64))")
      (is (null (reader 0))))))

#+sbcl
(deftest codegen-block-compilation-preserves-result-arities ()
  (dolist (mode '(nil :specified t))
    (dolist (case
              '(("(declare results (Void -> Void))
                  (define (results) (values))"
                 "(results)" nil)
                ("(declare results (Void -> Integer * String))
                  (define (results) (values 42 \"answer\"))"
                 "(results)" (42 "answer"))
                ("(declare forward-results
                    (forall ((:r Values)) (Void -> :r) -> :r))
                  (define (forward-results f) (f))"
                 "(forward-results (fn () (values (the Integer 42) \"answer\")))"
                 (42 "answer"))
                ("(define-class (Sink :a) (sink (:a -> Void)))
                  (define-instance (Sink Integer) (define (sink _) (values)))"
                 "(sink (the Integer 42))" nil)))
      (with-codegen-test-environment
        (let* ((sb-ext:*block-compile-default* mode)
               (source
                 (source:make-source-string
                  (concatenate
                   'string
                   "(declare increment (F64 -> F64))
                    (define (increment x) (+ x 1.0d0))
                    (declare twice (F64 -> F64))
                    (define (twice x) (increment (increment x)))
                    "
                   (first case)))))
          (with-open-stream (stream (source:source-stream source))
            (multiple-value-bind (form env)
                (entry:entry-point
                 (parser:with-reader-context stream (parser:read-program stream source)))
              (let* ((declarations (loop :for subform :in (rest form)
                                         :when (and (consp subform) (eq (first subform) 'declaim))
                                           :append (rest subform)))
                     (expected-count (if (eq mode :specified) 1 0)))
                (is (= expected-count (count 'sb-ext:start-block declarations :key #'first)))
                (is (= expected-count (count 'sb-ext:end-block declarations :key #'first)))
                (when (eq mode :specified)
                  (is (member (intern "TWICE")
                              (rest (assoc 'sb-ext:start-block declarations))))))
              ;; EVAL alone does not exercise SBCL's file block compiler.
              (let ((*print-circle* t))
                (multiple-value-bind (source-file fasl-file)
                    (compile-and-load-forms
                     (list `(in-package ,(package-name *package*)) form))
                  (delete-file source-file)
                  (delete-file fasl-file)))
              (setf entry:*global-environment* env)
              (is (= 42.0d0 (funcall (intern "TWICE") 40.0d0))))
            (is (equal (third case)
                       (multiple-value-list (codegen-test-eval (second case)))))))))))

(deftest codegen-match-preserves-evaluation ()
  (with-codegen-test-environment
    (codegen-test-event-recorder)
    (is (= 1 (codegen-test-eval "(match (observe 1) (x (observe 2) x))")))
    (is (equal '(1 2) (reverse *codegen-events*)))
    (setf *codegen-events* nil)
    (codegen-test-compile
     "(declare delayed (Void -> (Void -> Integer)))
      (define (delayed) (match (observe 1) (x (fn () x))))")
    (let ((function (codegen-test-eval "(delayed)")))
      (is (equal '(1) *codegen-events*))
      (is (= 1 (funcall function)))
      (is (= 1 (funcall function)))
      (is (equal '(1) *codegen-events*)))
    (setf *codegen-events* nil)
    (is (= 0 (codegen-test-eval "(match (observe 3) (x (if False x 0)))")))
    (is (equal '(3) *codegen-events*))
    (setf *codegen-events* nil)
    (is (= 3 (codegen-test-eval
              "(match (Tuple (observe 1) (observe 2))
                 ((Tuple x y) (observe 3) (+ x y)))")))
    (is (equal '(1 2 3) (reverse *codegen-events*)))))

(deftest codegen-inline-preserves-argument-order ()
  (with-codegen-test-environment
    (codegen-test-event-recorder)
    (codegen-test-compile
     "(inline)
      (declare first-arg (Integer * Integer * Integer -> Integer))
      (define (first-arg x _y _z) x)
      (declare call-first (Void -> Integer))
      (define (call-first) (first-arg (observe 1) (observe 2) (observe 3)))")
    (is (= 1 (codegen-test-eval "(call-first)")))
    (is (equal '(1 2 3) (reverse *codegen-events*)))
    (setf *codegen-events* nil)
    (is (= 1 (codegen-test-eval
              "(first-arg (first-arg (observe 1) (observe 2) (observe 3))
                          (observe 4) (observe 5))")))
    (is (equal '(1 2 3 4 5) (reverse *codegen-events*)))
    (setf *codegen-events* nil)
    (codegen-test-compile
     "(declare fail-argument (Void -> Integer))
      (define (fail-argument) (lisp (-> Integer) () (cl:error \"argument failure\")))")
    (signals simple-error
      (codegen-test-eval "(first-arg (fail-argument) (observe 2) (observe 3))"))
    (is (null *codegen-events*))))

(deftest codegen-loop-snapshots-survive-updates ()
  (with-codegen-test-environment
    (dolist (loop-name '("for" "for*"))
      (dolist (binding '("(let ((snapshot i)) BODY)"
                        "(progn (let snapshot = i) BODY)"
                        "(match i (snapshot BODY))"))
        (let* ((save "(coalton/cell:write! saved
                       (Cons (fn () snapshot) (coalton/cell:read saved)))")
               (body (concatenate 'string (subseq binding 0 (search "BODY" binding))
                                  save (subseq binding (+ 4 (search "BODY" binding)))))
               (functions (codegen-test-eval
                           (format nil
                            "(let ((saved (coalton/cell:new Nil)))
                               (~A ((i (the Integer 0) (+ i 1))) :repeat 3 ~A)
                               (coalton/cell:read saved))" loop-name body))))
          (is (equal '(2 1 0) (mapcar #'funcall functions))))))))

(deftest codegen-retains-functions-imported-by-lisp ()
  (with-codegen-test-environment
    (is (= 4 (codegen-test-eval
              "(let ((f (fn (x) (+ x (the Integer 1)))))
                 (f 2)
                 (lisp (-> Integer) (f) (cl:funcall f 3)))")))
    (let ((function (codegen-test-eval
                     "(let ((f (fn (x) (+ x (the Integer 1)))))
                        (f 2)
                        (fn () (lisp (-> Integer) (f) (cl:funcall f 4))))")))
      (is (= 5 (funcall function)))
      (is (= 5 (funcall function))))))

(deftest codegen-transparent-nested-patterns ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(repr :transparent)
      (define-type IntList (IntList (List Integer)))
      (declare head-or-zero (IntList -> Integer))
      (define (head-or-zero xs)
        (match xs ((IntList (Cons x _)) x) ((IntList (Nil)) 0)))
      (repr :transparent)
      (define-type (Wrapped :a) (Wrapped (Optional (Tuple :a :a))))
      (declare first-or-zero (Wrapped Integer -> Integer))
      (define (first-or-zero x)
        (match x ((Wrapped (Some (Tuple a _))) a) ((Wrapped (None)) 0)))")
    (is (= 42 (codegen-test-eval "(head-or-zero (IntList (make-list 42 43)))")))
    (is (= 0 (codegen-test-eval "(head-or-zero (IntList Nil))")))
    (is (= 42 (codegen-test-eval "(first-or-zero (Wrapped (Some (Tuple 42 43))))")))
    (is (= 0 (codegen-test-eval "(first-or-zero (Wrapped None))")))))

(deftest codegen-inline-loop-initializer-scope ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(inline)
      (declare sequential-init (Integer -> Integer))
      (define (sequential-init n)
        (for* ((i n (+ i 1)) (j i (+ j 1))) :returns j :repeat 1 (values)))
      (inline)
      (declare recursive-init (Integer -> Integer))
      (define (recursive-init n)
        (for ((j i (+ j 1)) (i n (+ i 1))) :returns j :repeat 1 (values)))
      (inline)
      (declare shadow-init (Integer -> Integer))
      (define (shadow-init i)
        (for* ((i (+ i 1) (+ i 1)) (j i (+ j i))) :returns j :repeat 1 (values)))")
    (is (= 11 (codegen-test-eval "(sequential-init 10)")))
    (is (= 11 (codegen-test-eval "(recursive-init 10)")))
    (is (= 23 (codegen-test-eval "(shadow-init 10)")))
    (is (= 32 (codegen-test-eval "(+ (sequential-init 10) (sequential-init 20))")))))

(deftest codegen-nullary-resumption-evaluates-operand ()
  (with-codegen-test-environment
    (codegen-test-event-recorder)
    (codegen-test-compile "(define-resumption Stop)
                           (define-resumption (StopWith Integer))")
    (is (= 42 (codegen-test-eval
               "(resumable (resume-to (match (observe 7) (_ Stop)))
                  ((Stop) 42))")))
    (is (equal '(7) *codegen-events*))
    (setf *codegen-events* nil)
    (is (= 8 (codegen-test-eval
              "(resumable (resume-to (StopWith (observe 8))) ((StopWith x) x))")))
    (is (equal '(8) *codegen-events*))
    (setf *codegen-events* nil)
    (signals simple-error
      (codegen-test-eval
       "(resumable (resume-to (progn (lisp (-> Void) () (cl:error \"operand failed\")) Stop))
          ((Stop) (observe 9)))"))
    (is (null *codegen-events*))))

(deftest codegen-class-constants-initialize-once ()
  (with-codegen-test-environment
    (codegen-test-event-recorder)
    (codegen-test-compile
     "(define-class (Stamp :a) (stamp :a))
      (define-instance (Stamp Integer) (define stamp (observe 9)))
      (define-class (Counter :a) (counter (coalton/cell:Cell :a)))
      (define-instance (Counter Integer) (define counter (coalton/cell:new 0)))")
    (is (equal '(9) *codegen-events*))
    (setf *codegen-events* nil)
    (codegen-test-compile
     "(declare get-stamp (Void -> Integer))
      (define (get-stamp) stamp)
      (declare get-counter (Void -> coalton/cell:Cell Integer))
      (define (get-counter) counter)")
    (is (= 9 (codegen-test-eval "(get-stamp)")))
    (is (= 9 (codegen-test-eval "(get-stamp)")))
    (is (null *codegen-events*))
    (codegen-test-compile
     "(define-class (Callback :a) (callback (Void -> :a)))
      (define-instance (Callback Integer)
        (define callback (match (observe 7) (_ (fn () 42)))))")
    (is (equal '(7) *codegen-events*))
    (setf *codegen-events* nil)
    (is (= 42 (codegen-test-eval "(the Integer (callback))")))
    (is (= 42 (codegen-test-eval "(the Integer (callback))")))
    (is (null *codegen-events*))
    (codegen-test-eval "(coalton/cell:write! (get-counter) 42)")
    (is (= 42 (codegen-test-eval "(coalton/cell:read (get-counter))")))))

(deftest codegen-dictionary-hoisting-preserves-keywords ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(declare consume-eq (Eq :a => :a * :a -> Boolean))
      (define (consume-eq x y) (== x y))
      (declare compare-key (Eq :a => :a &key (:other :a) -> Boolean))
      (define (compare-key x &key (other x))
        (consume-eq (Cons x Nil) (Cons other Nil)))")
    (is (eq coalton:True (codegen-test-eval "(compare-key (the Integer 1))")))
    (is (eq coalton:True (codegen-test-eval "(compare-key (the Integer 1) :other 1)")))
    (is (eq coalton:False (codegen-test-eval "(compare-key (the Integer 1) :other 2)")))
    (is (eq coalton:False (codegen-test-eval "(compare-key \"one\" :other \"two\")")))))

(deftest codegen-exception-constructors-as-values ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(define-exception Problem (EmptyProblem) (NumberProblem Integer))
      (declare raise-problem (Problem -> Integer))
      (define (raise-problem problem) (throw problem))
      (declare raise-using ((Integer -> Problem) -> Integer))
      (define (raise-using constructor) (throw (constructor 42)))")
    (is (= 1 (codegen-test-eval "(catch (throw EmptyProblem) ((EmptyProblem) 1) (_ 0))")))
    (is (= 2 (codegen-test-eval "(catch (raise-problem EmptyProblem) ((EmptyProblem) 2) (_ 0))")))
    (is (= 42 (codegen-test-eval "(catch (raise-using NumberProblem) ((NumberProblem x) x) (_ 0))")))))

(deftest exception-redefinition-keeps-exception-status ()
  ;; Redefining a type does not remove its instances, so an exception type
  ;; cannot become an ordinary type while keeping its Exception instance.
  (with-codegen-test-environment
    (codegen-test-compile "(define-exception Problem (Problem Integer))")
    (codegen-test-compile "(define-exception Problem (Problem Integer) (OtherProblem String))")
    (signals tc:tc-error
      (codegen-test-compile "(define-type Problem (Problem Integer))"))))

(deftest resumption-redefinition-keeps-resumption-status ()
  ;; Likewise for resumption types and their Resumption instances.
  (with-codegen-test-environment
    (codegen-test-compile "(define-resumption (Retry Integer))")
    (codegen-test-compile "(define-resumption (Retry Integer) \"Retry the operation.\")")
    (signals tc:tc-error
      (codegen-test-compile "(define-type Retry (Retry Integer))"))))

(deftest codegen-handler-payload-patterns ()
  (with-codegen-test-environment
    (codegen-test-event-recorder)
    (codegen-test-compile
     "(define-exception Problem (NumberProblem Integer) (ListProblem (List Integer)))
      (define-resumption (NumberResume Integer))
      (define-resumption (ListResume (List Integer)))
      (define *state* (the Integer 0))")
    (is (= 20 (codegen-test-eval
               "(catch (throw (NumberProblem 2))
                  ((NumberProblem 1) 10) ((NumberProblem _) 20))")))
    (is (= 30 (codegen-test-eval
               "(catch (catch (throw (NumberProblem 2)) ((NumberProblem 1) 10))
                  ((NumberProblem 2) 30))")))
    (is (= 2 (codegen-test-eval
              "(catch (throw (ListProblem (make-list 1 2)))
                 ((ListProblem (Nil)) 0) ((ListProblem (Cons 1 (Cons x _))) x) (_ 3))")))
    (is (= 20 (codegen-test-eval
               "(resumable (resume-to (NumberResume 2))
                  ((NumberResume 1) 10) ((NumberResume _) 20))")))
    (is (= 30 (codegen-test-eval
               "(resumable (resumable (resume-to (NumberResume 2)) ((NumberResume 1) 10))
                  ((NumberResume 2) 30))")))
    (is (= 2 (codegen-test-eval
              "(resumable (resume-to (ListResume (make-list 1 2)))
                 ((ListResume (Nil)) 0) ((ListResume (Cons 1 (Cons x _))) x))")))
    ;; Resumption bodies run outside the dynamic extent being resumed from.
    (is (= 0 (codegen-test-eval
              "(resumable (dynamic-bind ((*state* 9)) (resume-to (NumberResume 2)))
                 ((NumberResume 2) *state*))")))
    (signals simple-error
      (codegen-test-eval "(resumable (resume-to (NumberResume 2)) ((NumberResume 1) 10))"))
    (is (= 3 (codegen-test-eval "(resumable (observe 3) ((NumberResume _) (observe 1)))")))
    (is (equal '(3) *codegen-events*))))

(deftest codegen-loop-function-bindings-are-assignable ()
  (with-codegen-test-environment
    (dolist (loop-name '("for" "for*"))
      (is (= 2 (codegen-test-eval
                (format nil "(~A ((f (fn () (the Integer 1)) (fn () 2)))
                               :returns (f) :repeat 1 (values))" loop-name)))))
    (is (= 11 (codegen-test-eval
               "(for ((f (fn () i) f) (i (the Integer 10) (+ i 1)))
                  :returns (f) :repeat 1 (values))")))
    (is (= 7 (codegen-test-eval
              "(for ((f (fn (x) (if (== x (the Integer 0)) 1 (f (- x 1))))
                        (fn (_) 7)))
                 :returns (f 3) :repeat 1 (values))")))))

(deftest codegen-calling-convention-attributes-and-entrypoints ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(define-struct Pair64 (left U64) (right U64))

      (abi :lisp)
      (declare boxed-pair-sum (Pair64 -> U64))
      (define (boxed-pair-sum p)
        (+ (.left p) (.right p)))

      (abi :unpacked)
      (declare unpacked-pair-swap (Tuple Integer Integer -> Tuple Integer Integer))
      (define (unpacked-pair-swap (Tuple a b))
        (Tuple b a))

      (abi :unboxed)
      (declare unboxed-pair-sum (Pair64 -> U64))
      (define (unboxed-pair-sum p)
        (+ (.left p) (.right p)))

      (abi :auto)
      (declare auto-pair-sum (Pair64 -> U64))
      (define (auto-pair-sum p)
        (+ (.left p) (.right p)))

      (define-class (PairOps :a)
        (pair-combine (:a * :a -> Tuple :a :a)))

      (define-instance (PairOps U64)
        (abi :unboxed)
        (define (pair-combine x y)
          (Tuple (+ x y) (* x y))))")
    (let* ((env entry:*global-environment*)
           (boxed-entry (tc:lookup-function env (intern "BOXED-PAIR-SUM")))
           (unpacked-entry (tc:lookup-function env (intern "UNPACKED-PAIR-SWAP")))
           (unboxed-entry (tc:lookup-function env (intern "UNBOXED-PAIR-SUM"))))
      (is (eq ':lisp (tc:function-env-entry-primary-convention boxed-entry)))
      (is (null (tc:function-env-entry-entrypoints boxed-entry)))

      (is (eq ':unpacked (tc:function-env-entry-primary-convention unpacked-entry)))
      (is (= 1 (length (tc:function-env-entry-entrypoints unpacked-entry))))
      (is (fboundp (intern "UNPACKED-PAIR-SWAP")))
      (is (fboundp (intern "UNPACKED-PAIR-SWAP%UNPACKED")))

      #+sbcl
      (when (and coalton-impl/settings:*emit-type-annotations*
                 (coalton-impl/codegen/calling-convention::sbcl-specialized-xep-available-p))
        (is (eq ':unboxed (tc:function-env-entry-primary-convention unboxed-entry)))
        (is (= 2 (length (tc:function-env-entry-entrypoints unboxed-entry))))
        (is (fboundp (intern "UNBOXED-PAIR-SUM")))
        (is (fboundp (intern "UNBOXED-PAIR-SUM%UNPACKED")))
        (is (fboundp (intern "UNBOXED-PAIR-SUM%UNBOXED")))))

    (is (= 30 (codegen-test-eval "(boxed-pair-sum (Pair64 10 20))")))
    (is (eq coalton:True
            (codegen-test-eval "(== (unpacked-pair-swap (Tuple 10 20)) (Tuple 20 10))")))
    (is (= 30 (codegen-test-eval "(unboxed-pair-sum (Pair64 10 20))")))
    (is (= 30 (codegen-test-eval "(auto-pair-sum (Pair64 10 20))")))
    (is (eq coalton:True
            (codegen-test-eval "(== (pair-combine (the U64 5) (the U64 10)) (Tuple 15 50))")))

    ;; Verify portable fallback when :unboxed is disabled (simulating non-SBCL host)
    (let ((coalton-impl/codegen/calling-convention:*enable-unboxed-abi* nil))
      (codegen-test-compile
       "(abi :unboxed)
        (declare portable-pair-sum (Pair64 -> U64))
        (define (portable-pair-sum p)
          (+ (.left p) (.right p)))

        (abi :unboxed)
        (declare portable-scalar-inc (U64 -> U64))
        (define (portable-scalar-inc x)
          (+ x 1))")
      (let* ((env entry:*global-environment*)
             (pair-entry (tc:lookup-function env (intern "PORTABLE-PAIR-SUM")))
             (scalar-entry (tc:lookup-function env (intern "PORTABLE-SCALAR-INC"))))
        (is (eq ':unpacked (tc:function-env-entry-primary-convention pair-entry)))
        (is (= 1 (length (tc:function-env-entry-entrypoints pair-entry))))
        (is (eq ':lisp (tc:function-env-entry-primary-convention scalar-entry)))
        (is (null (tc:function-env-entry-entrypoints scalar-entry))))
      (is (= 30 (codegen-test-eval "(portable-pair-sum (Pair64 10 20))")))
      (is (= 42 (codegen-test-eval "(portable-scalar-inc 41)"))))

    ;; Verify optimization hint printing when *print-calling-convention-occurrences* is enabled
    (let ((output
            (with-output-to-string (*standard-output*)
              (let ((coalton-impl/settings:*print-calling-convention-occurrences* t))
                (codegen-test-compile
                 "(abi :unpacked)
                  (declare logged-swap (Tuple Integer Integer -> Tuple Integer Integer))
                  (define (logged-swap (Tuple a b))
                    (Tuple b a))

                  (declare logged-caller (Tuple Integer Integer -> Tuple Integer Integer))
                  (define (logged-caller p)
                    (logged-swap p))")))))
      (is (search "Generating :UNPACKED entrypoint" output))
      (is (search "LOGGED-SWAP%UNPACKED" output))
      (is (search "Calling" output))
      (is (search "via :UNPACKED entrypoint" output)))

    ;; Invalid abi attributes must signal parse errors
    (signals parser:parse-error
      (codegen-test-compile
       "(abi :invalid)
        (define (bad-fn x) x)"))
    (signals parser:parse-error
      (codegen-test-compile
       "(abi :lisp)
        (abi :unpacked)
        (define (dup-fn x) x)"))
    (signals parser:parse-error
      (codegen-test-compile
       "(abi :lisp)
        (define *bad-val* 42)"))
    (signals parser:parse-error
      (codegen-test-compile
       "(abi :lisp)
        (define-type BadType)"))))

(deftest codegen-calling-convention-composite-and-unboxed-correctness ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(define-struct Vec2 (vx F64) (vy F64))
      (define-struct SharedTag (label String))
      (define-struct TaggedVec (tag SharedTag) (vec Vec2))

      (abi :unboxed)
      (declare vec2-add (Vec2 * Vec2 -> Vec2))
      (define (vec2-add a b)
        (Vec2 (+ (.vx a) (.vx b))
              (+ (.vy a) (.vy b))))

      (abi :unboxed)
      (declare vec2-dot (Vec2 * Vec2 -> F64))
      (define (vec2-dot a b)
        (+ (* (.vx a) (.vx b))
           (* (.vy a) (.vy b))))

      (abi :unboxed)
      (declare safe-div-u64 (U64 * U64 -> Optional U64))
      (define (safe-div-u64 a b)
        (if (== b 0)
            (return None)
            (Some (coalton/math/integral:quot a b))))

      (abi :unboxed)
      (declare checked-step (U64 * U64 -> Result U64 (Tuple U64 U64)))
      (define (checked-step x limit)
        (if (>= x limit)
            (Err x)
            (Ok (Tuple (+ x 1) (* x 2)))))

      ;; Mutual recursion across unpacked/unboxed entrypoints
      (abi :unboxed)
      (declare even-accum (U64 * Tuple U64 U64 -> Tuple U64 U64))
      (define (even-accum n (Tuple a b))
        (if (== n 0)
            (Tuple a b)
            (odd-accum (- n 1) (Tuple (+ a n) b))))

      (abi :unboxed)
      (declare odd-accum (U64 * Tuple U64 U64 -> Tuple U64 U64))
      (define (odd-accum n (Tuple a b))
        (if (== n 0)
            (Tuple a b)
            (even-accum (- n 1) (Tuple a (+ b n)))))

      ;; Object identity preservation for parameters that escape as whole values
      (abi :unpacked)
      (declare retag-vec (TaggedVec * Vec2 -> TaggedVec))
      (define (retag-vec tv new-vec)
        (TaggedVec (.tag tv) (vec2-add (.vec tv) new-vec)))

      (declare same-tag? (TaggedVec * TaggedVec -> Boolean))
      (define (same-tag? a b)
        (let ((ta (.tag a))
              (tb (.tag b)))
          (lisp (-> Boolean) (ta tb) (cl:eq ta tb))))")
    (is (= 12.0d0
           (codegen-test-eval
            "(vec2-dot (vec2-add (Vec2 1.0d0 2.0d0) (Vec2 0.5d0 1.0d0))
                       (Vec2 2.0d0 3.0d0))")))
    (is (eq coalton:True
            (codegen-test-eval "(== (safe-div-u64 21 3) (Some 7))")))
    (is (eq coalton:None
            (codegen-test-eval "(safe-div-u64 21 0)")))
    (is (eq coalton:True
            (codegen-test-eval "(== (checked-step 4 10) (Ok (Tuple 5 8)))")))
    (is (eq coalton:True
            (codegen-test-eval "(== (checked-step 10 10) (Err 10))")))
    (is (eq coalton:True
            (codegen-test-eval "(== (even-accum 5 (Tuple 0 0)) (Tuple 9 6))")))
    ;; Verify physical EQ identity of SharedTag is preserved across unpacked calls
    (is (eq coalton:True
            (codegen-test-eval
             "(let ((base (TaggedVec (SharedTag \"origin\") (Vec2 1.0d0 2.0d0))))
                (same-tag? (retag-vec base (Vec2 3.0d0 4.0d0))
                           (retag-vec base (Vec2 5.0d0 6.0d0))))")))))

(deftest codegen-calling-convention-higher-order-and-interop ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(abi :unboxed)
      (declare pair-total (Tuple U64 U64 -> U64))
      (define (pair-total (Tuple a b))
        (+ a b))

      (declare apply-to-pairs ((Tuple U64 U64 -> U64) * List (Tuple U64 U64) -> List U64))
      (define (apply-to-pairs f xs)
        (map f xs))

      (declare scale-with-key (Tuple U64 U64 &key (:factor U64) -> U64))
      (define (scale-with-key p &key (factor 2))
        (* factor (pair-total p)))")
    ;; Direct call uses worker; higher-order call and call-coalton-function use :lisp wrapper
    (is (= 30 (codegen-test-eval "(pair-total (Tuple 10 20))")))
    (is (equal '(3 7 11)
               (codegen-test-eval
                "(apply-to-pairs pair-total (make-list (Tuple 1 2) (Tuple 3 4) (Tuple 5 6)))")))
    (is (= 77
           (coalton:call-coalton-function
            (symbol-function (intern "PAIR-TOTAL"))
            (coalton-prelude:Tuple 30 47))))
    (is (= 60 (codegen-test-eval "(scale-with-key (Tuple 10 20))")))
    (is (= 90 (codegen-test-eval "(scale-with-key (Tuple 10 20) :factor 3)")))))

(deftest codegen-calling-convention-redefinition ()
  (with-codegen-test-environment
    ;; 1. Same-type redefinition updates worker and wrapper entrypoints for existing callers
    (codegen-test-compile
     "(abi :unboxed)
      (declare combine-pair (Tuple U64 U64 -> U64))
      (define (combine-pair (Tuple a b))
        (+ a b))

      (declare caller (U64 * U64 -> U64))
      (define (caller x y)
        (combine-pair (Tuple x y)))")
    (is (= 15 (codegen-test-eval "(caller 5 10)")))
    (codegen-test-compile
     "(abi :unboxed)
      (declare combine-pair (Tuple U64 U64 -> U64))
      (define (combine-pair (Tuple a b))
        (* a b))")
    (is (= 50 (codegen-test-eval "(caller 5 10)")))

    ;; 2. Type-changing redefinition installs forwarding stubs on retired worker symbols
    (let ((old-unpacked-sym (intern "COMBINE-PAIR%UNPACKED")))
      (is (fboundp old-unpacked-sym))
      (let ((coalton-impl/settings:*auto-continue-redefinition* t))
        (handler-bind ((warning #'muffle-warning))
          (codegen-test-compile
           "(declare combine-pair (U64 -> U64))
            (define (combine-pair x)
              (+ x 100))")))
      (is (= 105 (codegen-test-eval "(combine-pair 5)")))
      (is (fboundp old-unpacked-sym)))))

#+sbcl
(deftest codegen-calling-convention-zero-consing ()
  (with-codegen-test-environment
    (let ((*features* (adjoin ':coalton-release *features*))
          (coalton-impl/settings:*emit-type-annotations* t)
          (coalton-impl/settings:*coalton-disable-specialization* nil)
          (sb-ext:*block-compile-default* nil))
      (multiple-value-bind (source-file fasl-file)
          (compile-and-load-forms
           (list
            `(in-package ,(package-name *package*))
            (read-from-string
             "(coalton-toplevel
                (define-struct Point2I (ix I32) (iy I32))

                (abi :unpacked)
                (declare step-point-i32 (Point2I * I32 -> Point2I))
                (define (step-point-i32 p delta)
                  (Point2I (+ (.ix p) delta)
                           (- (.iy p) delta)))

                (abi :unpacked)
                (declare maybe-step-i32 (I32 * I32 -> Optional I32))
                (define (maybe-step-i32 x limit)
                  (if (< x limit)
                      (Some (+ x 3))
                      None))

                (abi :unpacked)
                (declare loop-points-i32 (I32 * I32 * Point2I -> I32))
                (define (loop-points-i32 i n acc)
                  (match (maybe-step-i32 i n)
                    ((Some next-i)
                     (loop-points-i32 next-i n (step-point-i32 acc 2)))
                    ((None)
                     (+ (.ix acc) (.iy acc)))))

                (abi :unpacked)
                (declare run-unpacked-allocation-free-loop (I32 -> I32))
                (define (run-unpacked-allocation-free-loop n)
                  (loop-points-i32 0 n (Point2I 0 0)))

                (define-struct Point2D (px F64) (py F64))

                (abi :unboxed)
                (declare step-point (Point2D * F64 -> Point2D))
                (define (step-point p delta)
                  (Point2D (+ (.px p) delta)
                           (- (.py p) delta)))

                (abi :unboxed)
                (declare maybe-step-u64 (U64 * U64 -> Optional U64))
                (define (maybe-step-u64 x limit)
                  (if (< x limit)
                      (Some (+ x 3))
                      None))

                (abi :unboxed)
                (declare loop-points (U64 * U64 * Point2D -> F64))
                (define (loop-points i n acc)
                  (match (maybe-step-u64 i n)
                    ((Some next-i)
                     (loop-points next-i n (step-point acc 1.5d0)))
                    ((None)
                     (+ (.px acc) (.py acc)))))

                (abi :unboxed)
                (declare run-allocation-free-loop (U64 -> Boolean))
                (define (run-allocation-free-loop n)
                  ;; Consume the float before crossing the boxed Lisp boundary.
                  (== (loop-points 0 n (Point2D 0.0d0 0.0d0)) 0.0d0)))")))
        (unwind-protect
             (progn
               (let ((unpacked-fn (symbol-function (intern "RUN-UNPACKED-ALLOCATION-FREE-LOOP" *package*))))
                 (is (= 0 (funcall unpacked-fn 300)))
                 ;; Account for allocations still in the thread's open region.
                 (sb-vm::close-thread-alloc-region)
                 (let ((before (sb-ext:get-bytes-consed)))
                   (dotimes (_ 100)
                     (funcall unpacked-fn 3000))
                   (sb-vm::close-thread-alloc-region)
                   (let ((consed (- (sb-ext:get-bytes-consed) before)))
                     (is (zerop consed)))))
               (when (coalton-impl/codegen/calling-convention::sbcl-specialized-xep-available-p)
                 (let ((fn (symbol-function (intern "RUN-ALLOCATION-FREE-LOOP" *package*))))
                   ;; Warm up
                   (is (eq t (funcall fn 300)))
                   (sb-vm::close-thread-alloc-region)
                   (let ((before (sb-ext:get-bytes-consed)))
                     (dotimes (_ 100)
                       (funcall fn 3000))
                     (sb-vm::close-thread-alloc-region)
                     (let ((consed (- (sb-ext:get-bytes-consed) before)))
                       (is (zerop consed)))))))
          (delete-file source-file)
          (delete-file fasl-file))))))

(deftest codegen-rank-n-polymorphism ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(derive Eq)
      (define-type (Dual :a) (Dual :a :a))
      (define (primal-part (Dual x _)) x)
      (define (dual-part (Dual _ dx)) dx)
      (define-instance (Num :a => Num (Dual :a))
        (define (+ (Dual a da) (Dual b db)) (Dual (+ a b) (+ da db)))
        (define (- (Dual a da) (Dual b db)) (Dual (- a b) (- da db)))
        (define (* (Dual a da) (Dual b db)) (Dual (* a b) (+ (* a db) (* da b))))
        (define (fromInt n) (Dual (fromInt n) (fromInt 0))))

      (declare differentiate ((forall (:a) (Num :a => (:a -> :a))) -> (F32 -> F32)))
      (define (differentiate f)
        (fn (x)
          (dual-part (f (Dual x 1)))))

      (declare val-and-deriv ((forall (:a) (Num :a => (:a -> :a))) -> (F32 -> Tuple F32 F32)))
      (define (val-and-deriv f)
        (fn (x)
          (Tuple (f x) (dual-part (f (Dual x 1))))))

      (declare square (Num :a => (:a -> :a)))
      (define (square x) (* x x))

      (define-type NumFnBox
        (NumFnBox (forall (:a) (Num :a => (:a -> :a)))))

      (declare diff-box (NumFnBox -> (F32 -> F32)))
      (define (diff-box (NumFnBox f))
        (differentiate f))

      (declare make-scaler (Integer -> (forall (:a) (Num :a => (:a -> :a)))))
      (define (make-scaler k)
        (fn (x) (* (fromInt k) x)))

      (declare rank3 ((((forall (:a) (:a -> :a)) -> Tuple Integer String) -> Tuple Integer String)))
      (define (rank3 k)
        (k (fn (x) x)))

      (define gg (the (forall (:a) (Num :a => (:a -> :a))) (fn (x) (+ x 1))))

      (declare apply3 ((forall (:a) (Num :a => (:a &key (:scale :a) -> :a))) -> Integer))
      (define (apply3 f) (f 3))

      (declare apply3-scaled ((forall (:a) (Num :a => (:a &key (:scale :a) -> :a))) -> Integer))
      (define (apply3-scaled f) (f 3 :scale 10))

      (declare scale-it (Num :a => (:a &key (:scale :a) -> :a)))
      (define (scale-it x &key (scale 2)) (* x scale))

      (declare vd ((forall (:a) (Num :a => (Void -> :a))) -> Tuple Integer F64))
      (define (vd f) (Tuple (f) (f)))

      (declare three (Num :a => Void -> :a))
      (define (three) (fromInt 3))

      ;; Monomorphize on a Rank-2 function with an outer Num :b constraint
      (monomorphize)
      (declare diff-at (Num :b => (forall (:a) (Num :a => (:a -> :a))) * :b -> :b))
      (define (diff-at f x)
        (dual-part (f (Dual x 1))))

      (monomorphize)
      (declare call-diff-at-f32 (F32 -> F32))
      (define (call-diff-at-f32 x)
        (diff-at square x))

      ;; Specialize on a Rank-2 function
      (declare diff-spec-generic (Num :b => (forall (:a) (Num :a => (:a -> :a))) * :b -> :b))
      (define (diff-spec-generic f x)
        (dual-part (f (Dual x 1))))

      (declare diff-spec-f32 ((forall (:a) (Num :a => (:a -> :a))) * F32 -> F32))
      (define (diff-spec-f32 f x)
        (+ 100.0f0 (dual-part (f (Dual x 1)))))

      (specialize diff-spec-generic diff-spec-f32 ((forall (:a) (Num :a => (:a -> :a))) * F32 -> F32))

      (declare call-diff-spec (F32 -> F32))
      (define (call-diff-spec x)
        (diff-spec-generic square x))

      ;; Specialize on a polymorphic function passed into an inlined Rank-2 function
      (inline)
      (declare inline-val-and-deriv ((forall (:a) (Num :a => (:a -> :a))) * F32 -> Tuple F32 F32))
      (define (inline-val-and-deriv f x)
        (Tuple (f x) (dual-part (f (Dual x 1)))))

      (declare poly-fn (Num :a => (:a -> :a)))
      (define (poly-fn x) (+ x x))

      (declare poly-fn-f32 (F32 -> F32))
      (define (poly-fn-f32 x) (+ 1000.0f0 (+ x x)))

      (specialize poly-fn poly-fn-f32 (F32 -> F32))

      (declare call-spec-in-rank2 (F32 -> Tuple F32 F32))
      (define (call-spec-in-rank2 x)
        (inline-val-and-deriv poly-fn x))

      ;; Unpacked calling convention on a product type with a Rank-2 field
      (define-type NumFnPair
        (NumFnPair (forall (:a) (Num :a => (:a -> :a))) F32))

      (abi :unpacked)
      (declare eval-num-fn-pair (NumFnPair -> F32))
      (define (eval-num-fn-pair (NumFnPair f x))
        (dual-part (f (Dual x 1))))

      (declare call-eval-pair (F32 -> F32))
      (define (call-eval-pair x)
        (eval-num-fn-pair (NumFnPair square x)))")
    (is (= 6.0f0 (codegen-test-eval "((differentiate square) 3.0f0)")))
    (is (= 12.0f0 (codegen-test-eval "((differentiate (fn (x) (+ (* x x) (* 2 x)))) 5.0f0)")))
    (is (eq t (codegen-test-eval "(== (Tuple 16.0f0 8.0f0) ((val-and-deriv square) 4.0f0))")))
    (is (= 12.0f0 (codegen-test-eval "((diff-box (NumFnBox (fn (x) (* x (* x x))))) 2.0f0)")))
    (is (= 7.0f0 (codegen-test-eval "((differentiate (make-scaler 7)) 3.0f0)")))
    (is (= 21 (codegen-test-eval "((make-scaler 7) (the Integer 3))")))
    (is (= 21.0f0 (codegen-test-eval "((make-scaler 7) (the F32 3.0f0))")))
    (is (eq t (codegen-test-eval "(== (Tuple 10 \"ten\") (rank3 (fn (f) (Tuple (f 10) (f \"ten\")))))")))
    (is (= 42 (codegen-test-eval "((the (forall (:a) (Num :a => (:a -> :a))) (fn (x) (+ x 1))) (the Integer 41))")))
    (is (= 49 (codegen-test-eval "((the (forall (:a) (Num :a => (:a -> :a))) square) (the Integer 7))")))
    (is (= 11 (codegen-test-eval "(gg (the Integer 10))")))
    (is (= 6 (codegen-test-eval "(apply3 scale-it)")))
    (is (= 30 (codegen-test-eval "(apply3-scaled scale-it)")))
    (is (eq t (codegen-test-eval "(== (Tuple 3 3.0d0) (vd (fn () (fromInt 3))))")))
    (is (eq t (codegen-test-eval "(== (Tuple 3 3.0d0) (vd three))")))
    (is (= 3 (codegen-test-eval "((the (forall (:a) (Num :a => (Void -> :a))) (fn () (fromInt 3))))")))
    (is (= 6.0f0 (codegen-test-eval "(call-diff-at-f32 3.0f0)")))
    (is (= (if coalton-impl/settings:*coalton-disable-specialization*
               6.0f0
               106.0f0)
           (codegen-test-eval "(call-diff-spec 3.0f0)")))
    (is (eq t (codegen-test-eval
               (if coalton-impl/settings:*coalton-disable-specialization*
                   "(== (Tuple 10.0f0 2.0f0) (call-spec-in-rank2 5.0f0))"
                   "(== (Tuple 1010.0f0 2.0f0) (call-spec-in-rank2 5.0f0))"))))
    (is (= 8.0f0 (codegen-test-eval "(call-eval-pair 4.0f0)")))))

(deftest codegen-toplevel-values-translate-to-global-variables ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(declare global-pair (Tuple Integer Integer))
      (define global-pair (Tuple 10 20))
      (declare get-global-pair (Void -> (Tuple Integer Integer)))
      (define (get-global-pair)
        global-pair)
      (declare propagated-boolean (Void -> Boolean))
      (define (propagated-boolean)
        (let ((b True))
          b))")
    (let ((body (ast:node-abstraction-subexpr
                 (tc:lookup-code entry:*global-environment*
                                 (intern "GET-GLOBAL-PAIR")))))
      (is (ast:node-global-variable-p body))
      (is (eq (intern "GLOBAL-PAIR") (ast:node-variable-value body))))
    (let ((body (ast:node-abstraction-subexpr
                 (tc:lookup-code entry:*global-environment*
                                 (intern "PROPAGATED-BOOLEAN")))))
      (is (ast:node-global-variable-p body))
      (is (eq 'coalton:True (ast:node-variable-value body)))
      (is (equal ''t
                 (coalton-impl/codegen/codegen-expression:codegen-expression
                  body
                  entry:*global-environment*))))
    (codegen-test-compile
     "(repr :enum)
      (define-type Direction North South)
      (declare propagated-direction (Void -> Direction))
      (define (propagated-direction)
        (let ((d North))
          d))")
    (let ((body (ast:node-abstraction-subexpr
                 (tc:lookup-code entry:*global-environment*
                                 (intern "PROPAGATED-DIRECTION")))))
      (is (ast:node-global-variable-p body))
      (is (eq (intern "NORTH") (ast:node-variable-value body)))
      (is (equal `(quote ,(intern "DIRECTION/NORTH"))
                 (coalton-impl/codegen/codegen-expression:codegen-expression
                  body
                  entry:*global-environment*))))
    (is (eq coalton:True (codegen-test-eval "(== (get-global-pair) (Tuple 10 20))")))
    (is (eq coalton:True (codegen-test-eval "(propagated-boolean)")))
    (is (eq (intern "DIRECTION/NORTH") (codegen-test-eval "(propagated-direction)")))))

(deftest codegen-constant-propagation-bind-and-release-globals ()
  (with-codegen-test-environment
    ;; Sequential `let x = ...` (node-bind) and constant `node-lisp` forms propagate.
    (codegen-test-compile
     "(declare sequential-bind-const (Void -> Integer))
      (define (sequential-bind-const)
        (let x = 42)
        x)
      (declare constant-lisp-form (Void -> Integer))
      (define (constant-lisp-form)
        (let ((x (lisp (-> Integer) () 99)))
          x))")
    (let ((bind-body (ast:node-abstraction-subexpr
                      (tc:lookup-code entry:*global-environment*
                                      (intern "SEQUENTIAL-BIND-CONST"))))
          (lisp-body (ast:node-abstraction-subexpr
                      (tc:lookup-code entry:*global-environment*
                                      (intern "CONSTANT-LISP-FORM")))))
      (is (ast:node-literal-p bind-body))
      (is (= 42 (ast:node-literal-value bind-body)))
      (is (ast:node-lisp-p lisp-body))
      (is (equal '(99) (ast:node-lisp-form lisp-body))))
    ;; In development mode, top-level scalar bindings remain global variable reads
    ;; so interactive redefinition is observed by callers.
    (let ((*features* (remove ':coalton-release *features*)))
      (codegen-test-compile
       "(declare dev-const Integer)
        (define dev-const 7)
        (declare read-dev-const (Void -> Integer))
        (define (read-dev-const)
          dev-const)")
      (let ((body (ast:node-abstraction-subexpr
                   (tc:lookup-code entry:*global-environment*
                                   (intern "READ-DEV-CONST")))))
        (is (ast:node-global-variable-p body))))
    ;; In release mode, top-level scalar constants propagate into callers and
    ;; `lisp` variable bindings.
    (let ((*features* (cons ':coalton-release *features*)))
      (codegen-test-compile
       "(declare rel-const Integer)
        (define rel-const 7)
        (declare read-rel-const (Void -> Integer))
        (define (read-rel-const)
          rel-const)
        (declare lisp-rel-const (Integer -> Integer))
        (define (lisp-rel-const x)
          (lisp (-> Integer) (x rel-const)
            (cl:+ x rel-const)))")
      (let ((body (ast:node-abstraction-subexpr
                   (tc:lookup-code entry:*global-environment*
                                   (intern "READ-REL-CONST"))))
            (lisp-caller-body (ast:node-abstraction-subexpr
                               (tc:lookup-code entry:*global-environment*
                                               (intern "LISP-REL-CONST")))))
        (is (ast:node-literal-p body))
        (is (= 7 (ast:node-literal-value body)))
        (is (ast:node-let-p lisp-caller-body))
        (let ((binding-val (cdar (ast:node-let-bindings lisp-caller-body))))
          (is (ast:node-literal-p binding-val))
          (is (= 7 (ast:node-literal-value binding-val)))))
      (is (= 17 (codegen-test-eval "(lisp-rel-const 10)"))))))

(defun count-tree-occurrences (target form)
  (cond
    ((equal target form) 1)
    ((consp form)
     (+ (count-tree-occurrences target (car form))
        (count-tree-occurrences target (cdr form))))
    (t 0)))

(deftest codegen-match-decision-dag ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(repr :enum)
      (define-type RBColor RBRed RBBlack)
      (define-type (RBTree :a)
        RBEmpty
        (RBNode RBColor (RBTree :a) :a (RBTree :a)))
      (declare rb-classify (RBTree Integer -> Integer))
      (define (rb-classify t)
        (match t
          ((RBNode (RBBlack) (RBNode (RBRed) (RBNode (RBRed) _ x _) y _) z _)
           (+ x (+ y z)))
          ((RBNode (RBBlack) (RBNode (RBRed) _ x (RBNode (RBRed) _ y _)) z _)
           (+ x (+ y z)))
          ((RBNode (RBBlack) _ x (RBNode (RBRed) (RBNode (RBRed) _ y _) z _))
           (+ x (+ y z)))
          ((RBNode (RBBlack) _ x (RBNode (RBRed) _ y (RBNode (RBRed) _ z _)))
           (+ x (+ y z)))
          ((RBNode (RBBlack) (RBNode (RBBlack) _ x _) y (RBNode (RBBlack) _ z _))
           (+ x (+ y z)))
          ((RBNode _ _ x _)
           x)
          ((RBEmpty)
           0)))")
    (let* ((node (tc:lookup-code entry:*global-environment* (intern "RB-CLASSIFY")))
           (lisp-form (coalton-impl/codegen/codegen-expression:codegen-expression
                       (ast:node-abstraction-subexpr node)
                       entry:*global-environment*))
           (rbnode-class (intern "RBTREE/RBNODE"))
           (slot-0-reader (intern "RBTREE/RBNODE-_0")))
      ;; The root constructor test and root color reader are emitted only once,
      ;; and the right-child Red-Red DAG subtree is shared across left-child
      ;; failure paths rather than duplicated across all 7 branches.
      (is (= 8 (count-tree-occurrences `(quote ,rbnode-class) lisp-form)))
      (is (= 8 (count-tree-occurrences slot-0-reader lisp-form))))
    (is (= 0 (codegen-test-eval "(rb-classify RBEmpty)")))
    (is (= 90 (codegen-test-eval
               "(let ((b (RBNode RBBlack RBEmpty 10 RBEmpty))
                      (r (RBNode RBRed RBEmpty 20 RBEmpty)))
                  (rb-classify (RBNode RBBlack (RBNode RBRed r 30 b) 40 b)))")))
    (is (= 90 (codegen-test-eval
               "(let ((b (RBNode RBBlack RBEmpty 10 RBEmpty))
                      (r (RBNode RBRed RBEmpty 20 RBEmpty)))
                  (rb-classify (RBNode RBBlack (RBNode RBRed b 30 r) 40 b)))")))
    ;; When the left child is Red with Black children, the match must still test
    ;; the right child for Red-Red violations via the shared DAG join node.
    (is (= 110 (codegen-test-eval
                "(let ((b (RBNode RBBlack RBEmpty 10 RBEmpty))
                       (r (RBNode RBRed RBEmpty 20 RBEmpty)))
                   (rb-classify (RBNode RBBlack (RBNode RBRed b 30 b) 40 (RBNode RBRed r 50 b))))")))
    (is (= 110 (codegen-test-eval
                "(let ((b (RBNode RBBlack RBEmpty 10 RBEmpty))
                       (r (RBNode RBRed RBEmpty 20 RBEmpty)))
                   (rb-classify (RBNode RBBlack (RBNode RBRed b 30 b) 40 (RBNode RBRed b 50 r))))")))
    (is (= 60 (codegen-test-eval
               "(let ((b (RBNode RBBlack RBEmpty 10 RBEmpty)))
                  (rb-classify (RBNode RBBlack b 40 b)))")))
    (is (= 40 (codegen-test-eval
               "(let ((b (RBNode RBBlack RBEmpty 10 RBEmpty)))
                  (rb-classify (RBNode RBRed b 40 b)))")))))




