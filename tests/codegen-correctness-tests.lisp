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
