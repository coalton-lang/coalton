(cl:in-package #:coalton-native-tests)

(coalton-toplevel

  (define-type Egg
    ;;     cracked? cooked?
    (Goose Boolean Boolean)
    (Xenomorph))

  (define-exception BadEgg
    "Uncracked Eggception" 
    (UnCracked Egg)
    "Deadly Eggception"
    (DeadlyEgg Egg))

  (define-resumption SkipEgg)
  (define-resumption (ServeRaw Egg)
    "Suggest that the egg be served raw.")

  (define-type-alias MadEgg BadEgg)
  
  (declare crack (Egg -> Egg))
  (define (crack egg-o)
    (match egg-o
      ((Goose _cracked? cooked?)
       (Goose True cooked?))
      ((Xenomorph)
       (throw (DeadlyEgg egg-o)))))

  (declare crack-safely (Egg -> (Result MadEgg Egg)))
  (define (crack-safely egg-i)
    (catch (Ok (crack egg-i))
      ((DeadlyEgg _) (Err (DeadlyEgg egg-i)))
      ((UnCracked _) (Err (UnCracked egg-i)))))

  (declare cook (Egg -> Egg))
  (define (cook egg-k)
    (let ((badegg (Uncracked egg-k)))     ; exceptions can be constructed outside throw
      (match egg-k
        ((Goose (True) _)  (Goose True True))
        ((Goose (False) _) (throw badegg))
        ((Xenomorph)       (throw (DeadlyEgg egg-k))))))

  (declare make-breakfast-with (Egg -> (Optional Egg)))
  (define (make-breakfast-with egg-x)
    (resumable (Some (cook (crack egg-x)))
      ((SkipEgg) None)
      ((ServeRaw _) (Some egg-x))))

  (declare make-breakfast-for (UFix -> (Vector Egg)))
  (define (make-breakfast-for n)
    (let ((eggs (vector:make))
          (skip  SkipEgg))
      (for ((declare i UFix)
            (i 0 (1+ i)))
        :repeat n
        (let moocow = (if (zero? (mod i 5))
                          Xenomorph
                          (Goose False False)))
        (do
         ;; HANDLE runs its branches before unwinding, so they can resume
         ;; to the resumptions established by MAKE-BREAKFAST-WITH.
         (cooked <- (handle (make-breakfast-with moocow)
                      ((DeadlyEgg _)      (resume-to skip))
                      ((UnCracked egg-y)  (resume-to (ServeRaw egg-y)))))
         (pure (vector:push! cooked eggs))))
      eggs))

  (declare th (MadEgg -> :a))
  (define (th a) (throw a))

  (declare rs (ServeRaw -> :a))
  (define (rs a) (resume-to a))

  (define (serve-raw egg)
    (resume-to (ServeRaw egg)))

  ;; toplevel end
  )

(define-test test-throw-catch ()
  (match (crack-safely Xenomorph)
    ((Err (DeadlyEgg e)) (is True))
    ((Ok e) (is False)))

  (match (crack-safely (Goose False False))
    ((Ok e) (is True))
    ((Err exc) (is False))))


(define-test test-resume ()
  ;; every 5th egg will be deadly and therefore skipped
  (is (== 8 (vector:length (make-breakfast-for 10))))
  
  ;; test that non-nullary resumptions work ex expected.
  (let ((egg1 (Goose False False))
        (caught? (cell:new False))
        (resumed? (cell:new False)))
    (let (Goose cracked? cooked?) = (resumable
                                       (catch (cook egg1)
                                         ((UnCracked _)
                                          (cell:write! caught? True)
                                          (serve-raw egg1)))
                                     ((ServeRaw egg2)
                                      (cell:write! resumed? True)
                                      egg2)))
    (is (not cracked?))
    (is (not cooked?))
    (is (cell:read caught?))
    (is (cell:read resumed?))))

(define-test test-catch-all ()
  (let v = (vector:make))
  (for ()
    :repeat 1000
    (let n =
      (catch (lisp (-> integer) () (cl:/ 10 (cl:random 2)))
        (_ 0)))
    (vector:push! n v))
  (is (== 1000 (vector:length v)))
  
  (for ()
    :repeat 1000
    (let n =
      (catch (lisp (-> integer) () (cl:/ 10 (cl:random 2)))
        (_ 0)))
    (vector:push! n v))

  (is (== 2000 (vector:length v))))

;;;
;;; Exceptions represented by existing Lisp condition types
;;;

;; Defined at compile time for the (repr :native widget-failure) attribute
;; below, which checks the type when it is compiled.
(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:define-condition widget-failure (cl:error)
    ((code :initarg :code :reader widget-failure-code))
    (:report (cl:lambda (condition stream)
               (cl:format stream "widget failed with code ~D"
                          (widget-failure-code condition))))))

(coalton-toplevel
  (repr :native cl:division-by-zero)
  (define-exception DivisionByZero
    "Lisp's DIVISION-BY-ZERO condition.")

  (repr :native cl:arithmetic-error)
  (define-exception ArithmeticError)

  (repr :native widget-failure)
  (define-exception WidgetFailure)

  (declare lisp-divide (Integer * Integer -> Fraction))
  (define (lisp-divide a b)
    (lisp (-> Fraction) (a b)
      (cl:/ a b)))

  (declare division-operands (DivisionByZero -> (List Integer)))
  (define (division-operands e)
    (lisp (-> (List Integer)) (e)
      (cl:arithmetic-error-operands e)))

  (declare make-widget-failure (Integer -> WidgetFailure))
  (define (make-widget-failure code)
    (lisp (-> WidgetFailure) (code)
      (cl:make-condition 'widget-failure :code code)))

  (declare failure-code (WidgetFailure -> Integer))
  (define (failure-code e)
    (lisp (-> Integer) (e)
      (widget-failure-code e)))

  (declare throw-widget-failure (Integer -> Integer))
  (define (throw-widget-failure code)
    (throw (make-widget-failure code))))

(define-test test-catch-native-exception ()
  (is (== (Ok 3)
          (catch (Ok (lisp-divide 6 2))
            ((the DivisionByZero e) (Err (division-operands e))))))
  (is (== (Err (make-list 6 0))
          (catch (Ok (lisp-divide 6 0))
            ((the DivisionByZero e) (Err (division-operands e)))))))

(define-test test-catch-native-exception-order ()
  ;; The first branch whose type matches wins, and a branch for a Lisp
  ;; superclass also catches conditions of its subclasses.
  (is (== 1 (catch (lisp-divide 1 0)
              ((the DivisionByZero _) 1)
              ((the ArithmeticError _) 2))))
  (is (== 2 (catch (lisp-divide 1 0)
              ((the ArithmeticError _) 2)
              ((the DivisionByZero _) 1)))))

(define-test test-throw-native-exception ()
  ;; Throwing and rethrowing signal the same condition object.
  (let failure = (make-widget-failure 42))
  (let inner = (cell:new None))
  (is (catch (the Boolean (catch (the Boolean (throw failure))
                            ((the WidgetFailure e)
                             (cell:write! inner (Some e))
                             (throw e))))
        ((the WidgetFailure e)
         (match (cell:read inner)
           ((Some original)
            (lisp (-> Boolean) (e original failure)
              (cl:and (cl:eq e original) (cl:eq e failure))))
           ((None) False)))))
  (is (== 42 (catch (throw-widget-failure 42)
               ((the WidgetFailure e) (failure-code e)))))
  ;; Uncaught native exceptions reach Lisp handlers unchanged.
  (is (== 7 (lisp (-> Integer) ()
              (cl:handler-case (throw-widget-failure 7)
                (widget-failure (c) (widget-failure-code c)))))))

(define-test test-catch-exception-by-type ()
  ;; A (the T var) branch catches every constructor of T, including
  ;; through a type alias, and binds the exception itself.
  (let cook-or-explain =
    (fn (egg)
      (catch (progn (cook egg) "cooked")
        ((the MadEgg e)
         (match e
           ((UnCracked _) "uncracked")
           ((DeadlyEgg _) "deadly"))))))
  (is (== "cooked" (cook-or-explain (Goose True False))))
  (is (== "uncracked" (cook-or-explain (Goose False False))))
  (is (== "deadly" (cook-or-explain Xenomorph)))
  ;; Rethrowing a bound exception preserves its constructor.
  (is (== "deadly"
          (catch (catch (progn (crack Xenomorph) "cracked")
                   ((the BadEgg e) (throw e)))
            ((DeadlyEgg _) "deadly")))))

;;;
;;; CATCH branches run after unwinding; HANDLE branches run before
;;;

(coalton-toplevel
  (define-exception Retry
    (Retry UFix))

  (define-resumption (UseValue UFix))

  (declare *handler-depth* UFix)
  (define *handler-depth* 0)

  (declare fail-until (UFix * UFix -> UFix))
  (define (fail-until i limit)
    (if (< i limit)
        (throw (Retry i))
        i))

  (declare retry-from (UFix * UFix -> UFix))
  (define (retry-from i limit)
    (catch (fail-until i limit)
      ((Retry j) (retry-from (+ j 1) limit))))

  (declare fail-or-use-value (Void -> UFix))
  (define (fail-or-use-value)
    (resumable (fail-until 0 1)
      ((UseValue v) v))))

(define-test test-catch-unwinds-before-branch ()
  ;; The branch runs in the dynamic environment of the CATCH.
  (is (== 0 (catch (dynamic-bind ((*handler-depth* 9))
                     (throw (Retry 0)))
              ((Retry _) *handler-depth*))))
  ;; Each retry runs after the previous attempt has unwound.
  (is (== 1000 (retry-from 0 1000)))
  ;; A resumption established outside the CATCH is still available.
  (is (== 7 (resumable (catch (fail-until 0 1)
                         ((Retry _) (resume-to (UseValue 7))))
              ((UseValue v) v))))
  ;; One established inside it has been unwound by the time the branch runs.
  (is (== 99 (catch (catch (fail-or-use-value)
                      ((Retry _) (resume-to (UseValue 5))))
               (_ 99)))))

(define-test test-handle ()
  ;; The branch runs before unwinding, so it can resume.
  (is (== 5 (handle (fail-or-use-value)
              ((Retry _) (resume-to (UseValue 5))))))
  ;; The branch runs in the dynamic environment of the THROW.
  (is (== 9 (handle (dynamic-bind ((*handler-depth* 9))
                      (throw (Retry 0)))
              ((Retry _) *handler-depth*))))
  ;; A branch that finishes normally returns its value from HANDLE.
  (is (== 10 (handle (fail-until 0 1)
               ((Retry j) (+ j 10)))))
  (is (== 20 (handle (fail-until 0 1)
               ((the Retry e)
                (match e
                  ((Retry j) (+ j 20)))))))
  ;; Exceptions that match no branch keep propagating.
  (is (== 2 (catch (handle (fail-until 0 1)
                     ((Retry 5) 1))
              ((Retry _) 2)))))

;;;
;;; The Exception class
;;;

(coalton-toplevel
  ;; Inferred as (Exception :e => :e -> :a).
  (define (rethrow-any e)
    (throw e))

  (declare retry-number (Retry -> UFix))
  (define (retry-number (Retry n))
    n))

(define-test test-exception-class ()
  ;; THROW is polymorphic over exception types.
  (is (== 7 (catch (the UFix (rethrow-any (Retry 7)))
              ((Retry n) n))))
  (is (== 8 (catch (the Integer (rethrow-any (make-widget-failure 8)))
              ((the WidgetFailure e) (failure-code e)))))

  ;; TRY returns exceptions of the requested type in ERR, and lets others
  ;; propagate.
  (is (== (Ok 4)
          (result:map-err retry-number (result:try (fn () (fail-until 4 4))))))
  (is (== (Err 3)
          (result:map-err retry-number (result:try (fn () (fail-until 3 4))))))
  (is (== "deadly"
          (catch (progn
                   (the (Result Retry Egg) (result:try (fn () (crack Xenomorph))))
                   "not thrown")
            ((DeadlyEgg _) "deadly"))))
  (is (== (Err (make-list 1 0))
          (result:map-err division-operands
                          (result:try (fn () (lisp-divide 1 0))))))

  ;; OK-OR-THROW is the inverse of TRY.
  (is (== 5 (result:ok-or-throw (the (Result Retry UFix) (Ok 5)))))
  (is (== 6 (catch (result:ok-or-throw (the (Result Retry UFix) (Err (Retry 6))))
              ((Retry n) n)))))
