(defpackage #:coalton-tests/redef-detection
  (:use #:cl
        #:fiasco)
  (:local-nicknames
   (#:redef-detection #:coalton-impl/redef-detection)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)
   (#:source #:coalton-impl/source)
   (#:settings #:coalton-impl/settings))
  (:export
   #:redef-detection-tests))
(in-package #:coalton-tests/redef-detection)

(deftest test-dependency-tracking ()
  "Test that dependencies are correctly tracked"
  (let ((redef-detection:*dependency-registry*
          (redef-detection:make-dependency-registry)))

    ;; Define a helper function
    (eval '(coalton:coalton-toplevel
            (coalton:define (test-helper x) (coalton-prelude:+ x 1))))

    ;; Define a user function that calls helper
    (eval '(coalton:coalton-toplevel
            (coalton:define (test-user y) (test-helper y))))

    ;; Check dependencies were recorded
    (let ((callers (redef-detection:get-function-callers 'test-helper)))
      (is (member 'test-user callers)
          "test-user should be recorded as calling test-helper"))))

(deftest test-type-compatibility ()
  "Test type compatibility checking"
  ;; Define two functions with the same type signature
  (eval '(coalton:coalton-toplevel
          (coalton:declare test-compat-foo (coalton:Integer coalton:-> coalton:Integer))
          (coalton:define (test-compat-foo x) x)))
  (eval '(coalton:coalton-toplevel
          (coalton:declare test-compat-bar (coalton:Integer coalton:-> coalton:Integer))
          (coalton:define (test-compat-bar x) x)))

  (let* ((env entry:*global-environment*)
         (foo-type (tc:lookup-value-type env 'test-compat-foo))
         (bar-type (tc:lookup-value-type env 'test-compat-bar)))

    ;; Same concrete type (Integer -> Integer) should be compatible
    (is (redef-detection:types-compatible-p foo-type bar-type)
        "Same concrete type should be compatible"))

  ;; Define a function with a different type signature
  (eval '(coalton:coalton-toplevel
          (coalton:declare test-compat-str (coalton:String coalton:-> coalton:String))
          (coalton:define (test-compat-str x) x)))

  (let* ((env entry:*global-environment*)
         (foo-type (tc:lookup-value-type env 'test-compat-foo))
         (str-type (tc:lookup-value-type env 'test-compat-str)))

    ;; Different types (Integer -> Integer vs String -> String) should not be compatible
    (is (not (redef-detection:types-compatible-p foo-type str-type))
        "Different types should not be compatible")))

(deftest test-redefinition-error ()
  "Test that incompatible redefinition raises an error"
  (let ((redef-detection:*dependency-registry*
          (redef-detection:make-dependency-registry)))

    ;; Define a function with Integer -> Integer type
    (eval '(coalton:coalton-toplevel
            (coalton:declare test-redef-foo (coalton:Integer coalton:-> coalton:Integer))
            (coalton:define (test-redef-foo x) (coalton-prelude:+ x 1))))

    ;; Define a caller to ensure there are affected functions
    (eval '(coalton:coalton-toplevel
            (coalton:define (test-redef-caller y) (test-redef-foo y))))

    ;; Try to redefine with String -> String type (should error)
    (signals redef-detection:incompatible-redefinition
             (eval '(coalton:coalton-toplevel
                     (coalton:declare test-redef-foo (coalton:String coalton:-> coalton:String))
                     (coalton:define (test-redef-foo x) x))))))

(deftest test-affected-functions ()
  "Test finding transitively affected functions"
  (let ((redef-detection:*dependency-registry*
          (redef-detection:make-dependency-registry)))

    ;; Define: a calls b, b calls c
    (eval '(coalton:coalton-toplevel
            (coalton:define (test-c x) (coalton-prelude:+ x 1))))

    (eval '(coalton:coalton-toplevel
            (coalton:define (test-b y) (test-c y))))

    (eval '(coalton:coalton-toplevel
            (coalton:define (test-a z) (test-b z))))

    ;; Changing c should affect both b and a
    (let ((affected (redef-detection:find-affected-functions 'test-c)))
      (is (member 'test-b affected)
          "test-b should be affected by changes to test-c")
      (is (member 'test-a affected)
          "test-a should be transitively affected by changes to test-c"))))

(deftest test-same-type-redefinition ()
  "Test that redefining with same type is allowed"
  (let ((redef-detection:*dependency-registry*
          (redef-detection:make-dependency-registry)))

    ;; Define a function
    (eval '(coalton:coalton-toplevel
            (coalton:define (test-same-type x) (coalton-prelude:+ x 1))))

    ;; Redefine with same type (should succeed)
    (finishes
     (eval '(coalton:coalton-toplevel
             (coalton:define (test-same-type x) (coalton-prelude:+ x 2)))))))

(deftest test-mutual-recursion-deps ()
  "Test dependency tracking for mutually recursive functions"
  (let ((redef-detection:*dependency-registry*
          (redef-detection:make-dependency-registry)))

    ;; Define mutually recursive even? and odd?
    (eval '(coalton:coalton-toplevel
            (coalton:declare test-even? (coalton:Integer coalton:-> coalton:Boolean))
            (coalton:define (test-even? n)
              (coalton:if (coalton-prelude:== n 0)
                          coalton:True
                          (test-odd? (coalton-prelude:- n 1))))

            (coalton:declare test-odd? (coalton:Integer coalton:-> coalton:Boolean))
            (coalton:define (test-odd? n)
              (coalton:if (coalton-prelude:== n 0)
                          coalton:False
                          (test-even? (coalton-prelude:- n 1))))))

    ;; Check that dependencies were recorded correctly
    (let ((even-calls (gethash 'test-even?
                               (redef-detection:dependency-registry-forward-deps
                                redef-detection:*dependency-registry*)))
          (odd-calls (gethash 'test-odd?
                              (redef-detection:dependency-registry-forward-deps
                               redef-detection:*dependency-registry*))))
      (is (member 'test-odd? even-calls)
          "test-even? should call test-odd?")
      (is (member 'test-even? odd-calls)
          "test-odd? should call test-even?"))))

(deftest test-no-self-reference ()
  "Test that recursive calls don't create self-references in dependency tracking"
  (let ((redef-detection:*dependency-registry*
          (redef-detection:make-dependency-registry)))

    ;; Define a recursive factorial function
    (eval '(coalton:coalton-toplevel
            (coalton:declare test-factorial (coalton:Integer coalton:-> coalton:Integer))
            (coalton:define (test-factorial n)
              (coalton:if (coalton-prelude:<= n 1)
                          1
                          (coalton-prelude:* n (test-factorial (coalton-prelude:- n 1)))))))

    ;; Check that factorial doesn't list itself as a dependency
    (let ((deps (gethash 'test-factorial
                         (redef-detection:dependency-registry-forward-deps
                          redef-detection:*dependency-registry*))))
      (is (not (member 'test-factorial deps))
          "test-factorial should not list itself as a dependency"))))

(deftest test-abort-redefinition ()
  "Test that abort-redefinition restart works"
  (let ((redef-detection:*dependency-registry*
          (redef-detection:make-dependency-registry)))

    ;; Define initial function
    (eval '(coalton:coalton-toplevel
            (coalton:declare test-abort-foo (coalton:Integer coalton:-> coalton:Integer))
            (coalton:define (test-abort-foo x) (coalton-prelude:+ x 1))))

    ;; Define a caller to ensure there are affected functions
    (eval '(coalton:coalton-toplevel
            (coalton:define (test-abort-caller y) (test-abort-foo y))))

    ;; Try to redefine with incompatible type and invoke abort restart
    (handler-bind
        ((redef-detection:incompatible-redefinition
           (lambda (c)
             (declare (ignore c))
             (let ((restart (find-restart 'abort-redefinition)))
               (when restart
                 (invoke-restart restart))))))
      (signals error
               (eval '(coalton:coalton-toplevel
                       (coalton:declare test-abort-foo (coalton:String coalton:-> coalton:String))
                       (coalton:define (test-abort-foo x) x)))))))

(deftest test-circular-deps ()
  "Test that circular dependencies are handled correctly"
  (let ((redef-detection:*dependency-registry*
          (redef-detection:make-dependency-registry)))

    ;; Create circular dependency: a -> b -> c -> a
    (eval '(coalton:coalton-toplevel
            (coalton:declare test-circ-a (coalton:Integer coalton:-> coalton:Integer))
            (coalton:define (test-circ-a x)
              (coalton:if (coalton-prelude:== x 0)
                          0
                          (test-circ-b (coalton-prelude:- x 1))))

            (coalton:declare test-circ-b (coalton:Integer coalton:-> coalton:Integer))
            (coalton:define (test-circ-b x)
              (coalton:if (coalton-prelude:== x 0)
                          0
                          (test-circ-c (coalton-prelude:- x 1))))

            (coalton:declare test-circ-c (coalton:Integer coalton:-> coalton:Integer))
            (coalton:define (test-circ-c x)
              (coalton:if (coalton-prelude:== x 0)
                          0
                          (test-circ-a (coalton-prelude:- x 1))))))

    ;; Check that find-affected-functions doesn't infinite loop
    (let ((affected (redef-detection:find-affected-functions 'test-circ-a)))
      (is (member 'test-circ-b affected)
          "Circular deps: test-circ-b should be affected")
      (is (member 'test-circ-c affected)
          "Circular deps: test-circ-c should be affected"))))

(deftest test-location-tracking ()
  "Test that source locations are tracked for functions"
  (eval '(coalton:coalton-toplevel
          (coalton:define (test-loc-func x) (coalton-prelude:+ x 1))))

  (let ((location (redef-detection:get-function-location 'test-loc-func entry:*global-environment*)))
    (is (not (null location))
        "Location should be available for test-loc-func")

    (let ((source (source:location-source location)))
      (is (typep location 'source:location))
      (is (string= (source:source-name source) "<macroexpansion>")
          "REPL function source name should be '<macroexpansion>'"))))

(deftest test-continue-anyway ()
  "Test that continue-anyway restart allows incompatible redefinition"
  (let ((redef-detection:*dependency-registry*
          (redef-detection:make-dependency-registry)))

    ;; Define initial function
    (eval '(coalton:coalton-toplevel
            (coalton:declare test-continue-foo (coalton:Integer coalton:-> coalton:Integer))
            (coalton:define (test-continue-foo x) (coalton-prelude:+ x 1))))

    ;; Define a caller to ensure there are affected functions
    (eval '(coalton:coalton-toplevel
            (coalton:define (test-continue-caller y) (test-continue-foo y))))

    ;; Try to redefine with incompatible type and invoke continue-anyway restart
    (handler-bind
        ((redef-detection:incompatible-redefinition
           (lambda (c)
             (declare (ignore c))
             (let ((restart (find-restart 'continue-anyway)))
               (when restart
                 (invoke-restart restart))))))
      (eval '(coalton:coalton-toplevel
              (coalton:declare test-continue-foo (coalton:String coalton:-> coalton:String))
              (coalton:define (test-continue-foo x) x))))

    ;; Verify the new definition is active
    (let ((new-type (tc:lookup-value-type entry:*global-environment* 'test-continue-foo)))
      (is (not (null new-type))
          "New type should be in environment")
      (is (string= (format nil "~A" new-type) "(STRING -> STRING)")
          "Type should be updated to String -> String"))))

(deftest test-continue-anyway-updates-env ()
  "Test that continue-anyway actually updates the environment with new type"
  (let ((redef-detection:*dependency-registry*
          (redef-detection:make-dependency-registry)))

    ;; Define initial function with Integer type
    (eval '(coalton:coalton-toplevel
            (coalton:declare test-env-update (coalton:Integer coalton:-> coalton:Integer))
            (coalton:define (test-env-update x) x)))

    ;; Define caller
    (eval '(coalton:coalton-toplevel
            (coalton:define (test-env-caller z) (test-env-update z))))

    ;; Check initial type
    (let ((old-type (tc:lookup-value-type entry:*global-environment* 'test-env-update)))
      (is (string= (format nil "~A" old-type) "(INTEGER -> INTEGER)")
          "Initial type should be Integer -> Integer"))

    ;; Force redefinition with continue-anyway
    (handler-bind
        ((redef-detection:incompatible-redefinition
           (lambda (c)
             (declare (ignore c))
             (invoke-restart 'continue-anyway))))
      (eval '(coalton:coalton-toplevel
              (coalton:declare test-env-update (coalton:Boolean coalton:-> coalton:Boolean))
              (coalton:define (test-env-update x) x))))

    ;; Verify type was updated
    (let ((new-type (tc:lookup-value-type entry:*global-environment* 'test-env-update)))
      (is (string= (format nil "~A" new-type) "(BOOLEAN -> BOOLEAN)")
          "Type should be updated to Boolean -> Boolean"))))

(deftest test-auto-continue-redefinition ()
  "Test that *auto-continue-redefinition* automatically continues without debugger"
  (let ((redef-detection:*dependency-registry*
          (redef-detection:make-dependency-registry))
        (settings:*auto-continue-redefinition* t))

    ;; Define initial function
    (eval '(coalton:coalton-toplevel
            (coalton:declare test-auto-foo (coalton:Integer coalton:-> coalton:Integer))
            (coalton:define (test-auto-foo x) (coalton-prelude:+ x 1))))

    ;; Define a caller
    (eval '(coalton:coalton-toplevel
            (coalton:define (test-auto-caller y) (test-auto-foo y))))

    ;; Redefine with incompatible type - should auto-continue without error
    (eval '(coalton:coalton-toplevel
            (coalton:declare test-auto-foo (coalton:String coalton:-> coalton:String))
            (coalton:define (test-auto-foo x) x)))

    ;; Verify the new definition is active
    (let ((new-type (tc:lookup-value-type entry:*global-environment* 'test-auto-foo)))
      (is (not (null new-type))
          "New type should be in environment")
      (is (string= (format nil "~A" new-type) "(STRING → STRING)")
          "Type should be updated to STRING → STRING"))))
