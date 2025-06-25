(cl:in-package #:coalton-native-tests)

(coalton-toplevel

  (define-type Egg
    ;;     cracked? cooked?
    (Goose Boolean Boolean)
    (Xenomorph))

  (define-exception BadEgg
    (UnCracked Egg)
    (DeadlyEgg Egg))

  (declare crack (Egg -> Egg))
  (define (crack egg)
    (match egg
      ((Goose _cracked? cooked?)
       (Goose True cooked?))
      ((Xenomorph)
       (throw (DeadlyEgg egg)))))

  (declare safe-crack (Egg -> (Result BadEgg Egg)))
  (define (safe-crack egg)
    (catch (Ok (crack egg))
      ((DeadlyEgg e) (Err (DeadlyEgg e)))
      ((UnCracked e) (Err (UnCracked e)))))

  (declare cook (Egg -> Egg))
  (define (cook egg)
    (match egg
      ((Goose (True) _)  (Goose True True))
      ((Goose (False) _) (throw (UnCracked egg)))
      ((Xenomorph)       (throw (DeadlyEgg egg)))))


  ;; toplevel end
  )

(define-test test-throw-catch ()
  (match (safe-crack Xenomorph)
    ((Err (DeadlyEgg e)) (is True))
    ((Ok e) (is False)))

  (match (safe-crack (Goose False False))
    ((Ok e) (is True))
    ((Err exc) (is False))))

