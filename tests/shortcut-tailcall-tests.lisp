(cl:in-package #:coalton-native-tests)

(coalton-toplevel
  (declare and-tailcall (Integer -> Boolean))
  (define (and-tailcall n) (and (> n 0) (and-tailcall (1- n))))

  (declare or-tailcall (Integer -> Boolean))
  (define (or-tailcall n) (or (== n 0) (or-tailcall (1- n))))
  )

(define-test test-shortcut-tailcall ()
  ;; This causes stack overflow with buggy compiler
  (is (== False (and-tailcall 1000000)))
  (is (== True (or-tailcall 1000000)))
  )

;;; A `rec' self-call inside an exhaustive `match' whose last branch is
;;; a constructor pattern must still be a tail call. In release mode,
;;; such matches used to be emitted without a fallback branch, leaving
;;; an implicit NIL return path that prevented tail-call optimization.

(coalton-toplevel
  (define-type TailcallMatchStep
    (TailcallMatchGo UFix)
    (TailcallMatchStop UFix))

  (repr :enum)
  (define-type TailcallMatchEnum
    TailcallMatchMore
    TailcallMatchDone)

  (declare tailcall-match-step (UFix -> TailcallMatchStep))
  (define (tailcall-match-step i)
    (TailcallMatchGo (+ i 1)))

  (declare tailcall-match-step-result (UFix -> Result String UFix))
  (define (tailcall-match-step-result i)
    (Ok (+ i 1)))

  (declare tailcall-match-step-enum (UFix -> TailcallMatchEnum))
  (define (tailcall-match-step-enum _i)
    TailcallMatchMore)

  (declare tailcall-match-adt (UFix -> UFix))
  (define (tailcall-match-adt n)
    (rec % ((i 0))
      (if (>= i n)
          i
          (match (tailcall-match-step i)
            ((TailcallMatchGo j) (% j))
            ((TailcallMatchStop j) j)))))

  (declare tailcall-match-result (UFix -> Result String UFix))
  (define (tailcall-match-result n)
    (rec % ((i 0))
      (if (>= i n)
          (Ok i)
          (match (tailcall-match-step-result i)
            ((Ok j) (% j))
            ((Err e) (Err e))))))

  (declare tailcall-match-enum (UFix -> UFix))
  (define (tailcall-match-enum n)
    (rec % ((i 0))
      (if (>= i n)
          i
          (match (tailcall-match-step-enum i)
            ((TailcallMatchMore) (% (+ i 1)))
            ((TailcallMatchDone) 0))))))

(define-test test-match-last-constructor-tailcall ()
  ;; This causes stack overflow with buggy compiler
  (is (== 1000000 (tailcall-match-adt 1000000)))
  (is (== (Ok 1000000) (tailcall-match-result 1000000)))
  (is (== 1000000 (tailcall-match-enum 1000000))))
