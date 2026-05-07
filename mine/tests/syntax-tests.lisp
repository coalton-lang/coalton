(in-package #:mine-tests)

(defpackage #:mine-tests/syntax
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:lexer #:mine/syntax/lexer)
   (#:tok #:mine/syntax/token))
  (:export
   #:short-lambda-introducer-highlights-as-fn?
   #:chained-short-lambda-introducers-each-highlight?))

(in-package #:mine-tests/syntax)

(coalton-toplevel
  (declare token-kinds ((coalton:List tok:Token) -> (coalton:List tok:TokenKind)))
  (define (token-kinds toks)
    (map (fn (t) (tok:.token-kind t)) toks))

  (declare short-lambda-introducer-highlights-as-fn? (Void -> Boolean))
  (define (short-lambda-introducer-highlights-as-fn?)
    (let toks = (lexer:lex-line (lexer:lexer-state-new 0) "ƒx.x" 0))
    (== (token-kinds toks)
        (make-list tok:TokCoaltonKeyword
                   tok:TokSymbol)))

  (declare chained-short-lambda-introducers-each-highlight? (Void -> Boolean))
  (define (chained-short-lambda-introducers-each-highlight?)
    (let toks = (lexer:lex-line (lexer:lexer-state-new 0) "ƒx.ƒy.(+ x y)" 0))
    (== (token-kinds toks)
        (make-list tok:TokCoaltonKeyword tok:TokSymbol
                   tok:TokCoaltonKeyword tok:TokSymbol
                   tok:TokOpenParen tok:TokOperator tok:TokWhitespace
                   tok:TokSymbol tok:TokWhitespace tok:TokSymbol
                   tok:TokCloseParen))))

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

(defun check-chained-short-lambda-introducers-each-highlight ()
  (%check (mine-tests/syntax:chained-short-lambda-introducers-each-highlight?)
          "Expected each ƒ in `ƒx.ƒy.(+ x y)` to lex as a TokCoaltonKeyword"))
