(in-package #:mine-tests)

(defpackage #:mine-tests/syntax
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:lexer #:mine/syntax/lexer)
   (#:tok #:mine/syntax/token))
  (:export
   #:short-lambda-introducer-highlights-as-fn?))

(in-package #:mine-tests/syntax)

(coalton-toplevel
  (declare short-lambda-introducer-highlights-as-fn? (Void -> Boolean))
  (define (short-lambda-introducer-highlights-as-fn?)
    (match (lexer:lex-line (lexer:lexer-state-new 0) "ƒx.x" 0)
      ((Cons (tok:Token tok:TokCoaltonKeyword 0 1)
             (Cons (tok:Token tok:TokSymbol 1 4)
                   Nil))
       True)
      (_ False))))

(in-package #:mine-tests)

(defun check-symbol-input-fn-alias ()
  (let ((replacement (symbols:latex-symbol-lookup "fn")))
    (%check (and (stringp replacement)
                 (string= replacement "ƒ"))
            "Expected Ctrl+\\ fn to insert ƒ, got ~S"
            replacement)))

(defun check-short-lambda-introducer-highlights-as-fn ()
  (%check (mine-tests/syntax:short-lambda-introducer-highlights-as-fn?)
          "Expected ƒx.x to lex as a TokCoaltonKeyword introducer followed by a symbol body"))
