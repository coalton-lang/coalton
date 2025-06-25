(cl:in-package #:coalton-native-tests)

(coalton-toplevel

  (define-type Egg
    ;;     cracked? cooked?
    (Goose Boolean Boolean)
    (Xenomorph))

  (define-exception BadEgg
    (UnCracked Egg)
    (DeadlyEgg Egg))

  (define-resumption SkipEgg (SkipEgg))
  (define-resumption ServeRaw (ServeRaw Egg))
  
  (declare crack (Egg -> Egg))
  (define (crack egg)
    (match egg
      ((Goose _cracked? cooked?)
       (Goose True cooked?))
      ((Xenomorph)
       (throw (DeadlyEgg egg)))))

  (declare crack-safely (Egg -> (Result BadEgg Egg)))
  (define (crack-safely egg)
    (catch (Ok (crack egg))
      ((DeadlyEgg e) (Err (DeadlyEgg e)))
      ((UnCracked e) (Err (UnCracked e)))))

  (declare cook (Egg -> Egg))
  (define (cook egg)
    (let ((badegg (Uncracked egg)))     ; exceptions can be constructed outside throw
      (match egg
        ((Goose (True) _)  (Goose True True))
        ((Goose (False) _) (throw badegg))
        ((Xenomorph)       (throw (DeadlyEgg egg))))))


  (declare make-breakfast-with (Egg -> (Optional Egg)))
  (define (make-breakfast-with egg)
    (Some (cook (crack egg))))

  (declare make-breakfast-for (UFix -> (Vector Egg)))
  (define (make-breakfast-for n)
    (let ((eggs (vector:make))
          (skip  SkipEgg))              ; can construct outside of resume
      (for i in (iter:up-to n)
        (let moocow = (if (== 0 (mod i 5)) Xenomorph (Goose False False)))
        (do
         (cooked <- (catch (make-breakfast-with moocow)
                      ((DeadlyEgg _)    (resume-to skip))
                      ((UnCracked egg)  (resume-to (ServeRaw egg)))))
         (pure (vector:push! cooked eggs))))
      eggs))

  ;; toplevel end
  )

(define-test test-throw-catch ()
  (match (crack-safely Xenomorph)
    ((Err (DeadlyEgg e)) (is True))
    ((Ok e) (is False)))

  (match (crack-safely (Goose False False))
    ((Ok e) (is True))
    ((Err exc) (is False))))

