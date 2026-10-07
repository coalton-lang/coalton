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

;;;
;;; The Resumption class
;;;

(coalton-toplevel
  ;; Inferred as (Resumption :r => :r -> :a).
  (define (resume-any r)
    (resume-to r))

  (declare cook-or-skip (Egg -> (Optional Egg)))
  (define (cook-or-skip egg)
    (handle (make-breakfast-with egg)
      ((DeadlyEgg _) (resume-any SkipEgg)))))

(define-test test-resumption-class ()
  ;; RESUME-TO is polymorphic over resumption types, with and without
  ;; payloads.
  (is (none? (cook-or-skip Xenomorph)))
  (is (some? (cook-or-skip (Goose False False))))
  (is (== 5 (handle (fail-or-use-value)
              ((Retry _) (resume-any (UseValue 5)))))))

;;;
;;; Panics
;;;

(coalton-toplevel
  (declare panic-text (Panic -> String))
  (define (panic-text p)
    (lisp (-> String) (p)
      (cl:princ-to-string p)))

  (declare panic-message-of ((Void -> Integer) -> String))
  (define (panic-message-of thunk)
    (catch (progn (thunk) "no panic")
      ((the Panic p) (panic-text p))))

  (declare check-large (Integer -> Unit))
  (define (check-large x)
    (assert (> x 10) "x was ~A, 100% ~~ too small" x)))

(define-test test-panic ()
  ;; Messages are never treated as format control strings.
  (is (== "100% ~ done" (panic-message-of (fn () (error "100% ~ done")))))
  (is (== "missing ~/config"
          (panic-message-of (fn () (expect "missing ~/config" (the (Optional Integer) None))))))
  (is (== "bad state 42" (panic-message-of (fn () (unreachable "bad state ~A" 42)))))
  (is (== "Undefined" (panic-message-of (fn () (undefined Unit)))))
  (is (lisp (-> Boolean) ()
        (cl:and (cl:search "Unexpected"
                           (coalton (panic-message-of (fn () (unwrap (the (Optional Integer) None))))))
                cl:t)))
  ;; UNWRAP on an Err panics with a message that includes the error.
  (is (lisp (-> Boolean) ()
        (cl:and (cl:search "no such key"
                           (coalton (panic-message-of
                                     (fn () (unwrap (the (Result String Integer) (Err "no such key")))))))
                cl:t)))
  (is (lisp (-> Boolean) ()
        (cl:and (cl:search "failed: x was 3, 100% ~ too small"
                           (coalton (panic-message-of (fn () (check-large 3) 0))))
                cl:t)))
  ;; Wildcard branches still catch panics, as they catch every Lisp error.
  (is (== -1 (catch (the Integer (error "x")) (_ -1))))
  ;; Lisp code can handle panics by their condition type.
  (is (lisp (-> Boolean) ()
        (cl:handler-case (coalton (the Boolean (error "from Coalton")))
          (coalton/classes:panic () cl:t)))))

;;;
;;; Returning failures early with NEED
;;;

(coalton-toplevel
  (declare parse-number (String -> (Result String Integer)))
  (define (parse-number s)
    (match (string:parse-int s)
      ((Some n) (Ok n))
      ((None) (Err (<> "not a number: " s)))))

  (declare add-parsed (String * String -> (Result String Integer)))
  (define (add-parsed a b)
    (let x = (need (parse-number a)))
    (let y = (need (parse-number b)))
    (Ok (+ x y)))

  (declare sum-parsed ((List String) -> (Result String Integer)))
  (define (sum-parsed strings)
    (let total = (cell:new 0))
    (for ((rest strings (list:cdr rest)))
      :until (list:null? rest)
      (cell:write! total (+ (cell:read total)
                            (need (parse-number (list:car rest))))))
    (Ok (cell:read total)))

  (declare sum-first-two ((List Integer) -> (Optional Integer)))
  (define (sum-first-two xs)
    (let a = (need (list:head xs)))
    (let b = (need (list:head (need (list:tail xs)))))
    (Some (+ a b)))

  (declare successors ((List String) -> (List (Optional Integer))))
  (define (successors strings)
    ;; NEED inside a function literal returns from the function literal.
    (map (fn (s) (Some (+ 1 (need (string:parse-int s)))))
         strings))

  (declare retry-result (UFix -> (Result Retry UFix)))
  (define (retry-result n)
    ;; NEED does not catch exceptions, but composes with TRY.
    (Ok (+ 1 (need (result:try (fn () (fail-until n 3))))))))

(define-test test-need ()
  (is (== (Ok 5) (add-parsed "2" "3")))
  (is (== (Err "not a number: x") (add-parsed "2" "x")))
  (is (== (Err "not a number: y") (add-parsed "y" "x")))
  (is (== (Ok 6) (sum-parsed (make-list "1" "2" "3"))))
  (is (== (Err "not a number: two") (sum-parsed (make-list "1" "two" "3"))))
  (is (== (Some 3) (sum-first-two (make-list 1 2 7))))
  (is (== None (sum-first-two (make-list 1))))
  (is (== None (sum-first-two Nil)))
  (is (== (make-list (Some 2) None (Some 4)) (successors (make-list "1" "x" "3"))))
  (is (== (Ok 6) (result:map-err retry-number (retry-result 5))))
  (is (== (Err 1) (result:map-err retry-number (retry-result 1)))))

;;;
;;; Cleanup with PROTECT
;;;

(coalton-toplevel
  (declare *protect-trail* (cell:Cell (List String)))
  (define *protect-trail* (cell:new Nil))

  (declare trail-note (String -> Unit))
  (define (trail-note s)
    (cell:write! *protect-trail* (Cons s (cell:read *protect-trail*)))
    Unit)

  (declare take-trail (Void -> (List String)))
  (define (take-trail)
    (let notes = (reverse (cell:read *protect-trail*)))
    (cell:write! *protect-trail* Nil)
    notes)

  (declare protect-normally (Void -> UFix))
  (define (protect-normally)
    (protect (progn (trail-note "body") 1)
      (trail-note "cleanup")))

  (declare protect-throw (Void -> UFix))
  (define (protect-throw)
    (catch (protect (progn (trail-note "body") (fail-until 0 1))
             (trail-note "cleanup"))
      ((Retry _) (trail-note "caught") 2)))

  (declare protect-return (Boolean -> UFix))
  (define (protect-return early?)
    (protect (progn
               (when early?
                 (return 3))
               (trail-note "late")
               4)
      (trail-note "cleanup")))

  (declare protect-break (Void -> UFix))
  (define (protect-break)
    (let ((cleanups (cell:new 0)))
      (for ((i 0 (+ i 1)))
        :until (>= i 10)
        (protect (when (== i 3)
                   (break))
          (cell:write! cleanups (+ 1 (cell:read cleanups)))))
      (cell:read cleanups)))

  (declare protect-values (Void -> (Tuple UFix UFix)))
  (define (protect-values)
    (let (values a b) = (protect (values 5 6) (trail-note "cleanup")))
    (Tuple a b)))

(define-test test-protect ()
  (is (== 1 (protect-normally)))
  (is (== (make-list "body" "cleanup") (take-trail)))
  ;; The cleanup runs while CATCH unwinds, before its branch.
  (is (== 2 (protect-throw)))
  (is (== (make-list "body" "cleanup" "caught") (take-trail)))
  (is (== 3 (protect-return True)))
  (is (== (make-list "cleanup") (take-trail)))
  (is (== 4 (protect-return False)))
  (is (== (make-list "late" "cleanup") (take-trail)))
  (is (== 4 (protect-break)))
  (is (== (Tuple 5 6) (protect-values)))
  (is (== (make-list "cleanup") (take-trail))))

(define-test test-with-open-file-closes-on-throw ()
  ;; WITH-OPEN-FILE closes its stream even when its function throws.
  ;; The file exists before it is opened, because CCL's OPEN-STREAM-P
  ;; stays true for a stream closed with :ABORT whose file OPEN created
  ;; or superseded.
  (let saved = (the (cell:Cell (Optional (file:FileStream Char)))
                    (cell:new None)))
  (let caught =
    (file:with-temp-directory
     (fn (directory)
       (let path = (file:merge directory "closes-on-throw.txt"))
       (need (file:with-open-file path
               (fn (stream) (file:write-string stream "x"))
               :direction file:Output
               :if-exists file:Supersede))
       (Ok (catch (unwrap (file:with-open-file path
                            (fn (stream)
                              (cell:write! saved (Some stream))
                              (throw (Retry 7)))
                            :direction file:Output
                            :if-exists file:Append))
             ((Retry n) n))))))
  (is (match caught
        ((Ok 7) True)
        (_ False)))
  (is (match (cell:read saved)
        ((Some stream)
         (lisp (-> Boolean) (stream)
           (cl:not (cl:open-stream-p stream))))
        ((None) False))))

;;;
;;; The coalton/exception package
;;;

(coalton-toplevel
  (declare divide-with-message (Integer * Integer -> (Result String Fraction)))
  (define (divide-with-message a b)
    (catch (Ok (lisp-divide a b))
      ((the exception:ArithmeticError e) (Err (exception:message e)))))

  (declare arithmetic-error-kind (exception:ArithmeticError -> String))
  (define (arithmetic-error-kind e)
    (match (the (Optional exception:DivisionByZero) (exception:cast e))
      ((Some _) "division by zero")
      ((None)
       (match (the (Optional exception:LispFileError) (exception:cast e))
         ((Some _) "file error")
         ((None) "other arithmetic error")))))

  (declare classify-division (Integer * Integer -> String))
  (define (classify-division a b)
    ;; The quotient is used, so the division cannot be optimized away.
    (catch (if (== 0 (lisp-divide a b)) "zero" "ok")
      ((the exception:ArithmeticError e) (arithmetic-error-kind e)))))

(define-test test-exception-package ()
  ;; A branch for a Lisp condition type catches its Lisp subtypes, and
  ;; MESSAGE returns the Lisp report.
  (is (== (Ok 2) (divide-with-message 4 2)))
  (is (match (divide-with-message 1 0)
        ((Err text)
         (lisp (-> Boolean) (text)
           (cl:and (cl:search "DIVISION-BY-ZERO" text) cl:t)))
        ((Ok _) False)))
  ;; CAST recovers a more specific type, if the exception has it.
  (is (== "ok" (classify-division 4 2)))
  (is (== "division by zero" (classify-division 1 0)))
  ;; CAST to an exception's own type succeeds, and to an unrelated type fails.
  (is (== (Some 4)
          (map retry-number (the (Optional Retry) (exception:cast (Retry 4))))))
  (is (none? (the (Optional exception:EndOfFile) (exception:cast (Retry 4)))))
  ;; LispError catches Coalton exceptions and panics.
  (is (== "boom"
          (catch (the String (error "boom"))
            ((the exception:LispError e) (exception:message e)))))
  (is (== 9 (catch (the UFix (throw (Retry 9)))
              ((the exception:LispError e)
               (match (the (Optional Retry) (exception:cast e))
                 ((Some r) (retry-number r))
                 ((None) 0))))))
  ;; FILE:LispError holds a Lisp error as an exception, which can be rethrown.
  (is (match (file:system-relative-pathname "coalton-no-such-system" "")
        ((Err (file:LispError e))
         (== "rethrown"
             (catch (throw e)
               ((the exception:LispError _) "rethrown"))))
        (_ False))))
