(in-package #:small-coalton-programs)

;;;; This program computes symbolic derivatives of simple
;;;; expressions. It uses a new Symbol data type for demonstration
;;;; purposes.

(coalton-toplevel
  (define-type Symbol
    (Symbol String))

  (define (symbol-name sym)
    (match sym
      ((Symbol s) s)))

  (define-instance (Eq Symbol)
    (define (== a b)
      (== (symbol-name a) (symbol-name b)))
    (define (/= a b)
      (not (== a b))))
  
  (define-type Expr
    (EConst Integer)
    (EVar   Symbol)
    (E+     Expr Expr)
    (E*     Expr Expr))

  (declare diff (Symbol -> Expr -> Expr))
  (define (diff x f)
    (match f
      ((EConst _)
       (EConst 0))
      ((EVar s)
       (if (== s x) (EConst 1) (EConst 0)))
      ((E+ a b)
       (E+ (diff x a) (diff x b)))
      ((E* a b)
       (E+ (E* (diff x a) b)
           (E* a          (diff x b)))))))

(coalton-toplevel
  (define two-x 
    (let ((x (Symbol "x")))
      (diff x (E* (EVar x) (EVar x))))))
