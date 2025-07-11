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
  
  (declare crack (Egg -> Egg))
  (define (crack egg-o)
    (match egg-o
      ((Goose _cracked? cooked?)
       (Goose True cooked?))
      ((Xenomorph)
       (throw (DeadlyEgg egg-o)))))

  (declare crack-safely (Egg -> (Result BadEgg Egg)))
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
      ((ServeRaw rawegg) (Some rawegg))))

  (declare make-breakfast-for (UFix -> (Vector Egg)))
  (define (make-breakfast-for n)
    (let ((eggs (vector:make))
          (skip  SkipEgg))              ; can construct outside of resume-to
      (for i in (iter:up-to n)
        (let moocow = (if (== 0 (mod i 5)) Xenomorph (Goose False False)))
        (do
         (cooked <- (catch (make-breakfast-with moocow)
                      ((DeadlyEgg _)    (resume-to skip))
                      ((UnCracked egg-y)  (resume-to (ServeRaw egg-y)))))
         (pure (vector:push! cooked eggs))))
      eggs))

  (declare th (BadEgg -> :a))
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
  (is (== 8 (vector:length (make-breakfast-for 10)))))

(define-test test-catch-all ()
  (let v = (vector:make))

  (for _ in (iter:up-to 1000) 
    (let n = 
      (catch (lisp integer () (cl:/ 10 (cl:random 2)))
        (_ 0)))
    (vector:push! n v))
  (is (== 1000 (vector:length v)))
  
  (for _ in (iter:up-to 1000) 
    (let n = 
      (catch (lisp integer () (cl:/ 10 (cl:random 2)))
        (_ 0)))
    (vector:push! n v))

  (is (== 2000 (vector:length v))))


