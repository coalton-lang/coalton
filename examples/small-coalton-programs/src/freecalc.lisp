;;;; freecalc.lisp -- An example of the Free Monad + Interpreter pattern in Coalton

;;; This example follows
;;; https://serokell.io/blog/introduction-to-free-monads#free-monads-for-edsls

(defpackage #:freecalc
  (:import-from
   #:common-lisp
   #:describe
   #:disassemble)
  (:use #:coalton #:coalton-prelude)
  (:shadow #:add)
  (:local-nicknames
   (#:vector #:coalton-library/collections/mutable/vector)
   (#:st #:coalton-library/monad/state)
   (#:free #:coalton-library/monad/free))
  (:export #:run-a-program))

(named-readtables:in-readtable coalton:coalton)

(in-package #:freecalc)

(coalton-toplevel
  
  (define-type (ArithExprF :t :a)
    "An arithmetic expression type. The F stands for functor. Each
expression carries a continuation object. It is this continuation that
we map over."
    (AddE :t :t (:t -> :a))
    (SubE :t :t (:t -> :a))
    (InputE (:t -> :a)))

  (define-instance (Functor (ArithExprF :t))
    (define (map f ast)
      (match ast
        ((AddE t1 t2 cont) (AddE t1 t2 (map f cont)))
        ((SubE t1 t2 cont) (SubE t1 t2 (map f cont)))
        ((InputE cont) (InputE (map f cont))))))


  ;;
  ;; Here we lift our expression language into the free monad on the
  ;; functor
  ;; 

  (declare add (:t -> :t -> free:Free (ArithExprF :t) :t))
  (define (add x y)
    (free:liftf (AddE x y id)))

  (declare sub (:t -> :t -> free:Free (ArithExprF :t) :t))
  (define (sub x y)
    (free:liftf (SubE x y id)))
  
  (declare input (Unit -> free:Free (ArithExprF :t) :t))
  (define (input)
    (free:liftf (InputE id)))

  ;; 
  ;; This lets us define programs in our arithmetic language
  ;; 

  (define a-program
    (do
     (x <- (input))
     (y <- (input))
     (res1 <- (add x y))
     (res2 <- (sub res1 3))
     (pure res2)))

  ;;
  ;; We now define an interpreter for our language. Uses a natural
  ;; transformation from the free monad over our expression functor
  ;; into a State monad. We use a vector of numbers for our inputs.
  ;;
  
  (define compute-from-vector-inputs
    (free:foldfree
     (fn (arg) (match arg
                 ((AddE x y next) (pure (next (+ x y))))
                 ((SubE x y next) (pure (next (- x y))))
                 ((InputE next)
                  (map (compose next (fn (vec) (defaulting-unwrap (vector:pop! vec))))
                       st:get))))))


  ;;
  ;; Finally we run our program.
  ;; 

  (define (run-a-program)
    "Should return (Tuple #() 18)"
    (st:run (compute-from-vector-inputs a-program)
            (vector:make 11 10))))
