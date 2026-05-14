(in-package #:mine-tests)

(defpackage #:mine-tests/syntax
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:lexer #:mine/syntax/lexer)
   (#:input #:mine/term/input)
   (#:vec #:coalton/vector)
   (#:tok #:mine/syntax/token))
  (:export
   #:short-lambda-introducer-highlights-as-fn?
   #:chained-short-lambda-introducers-each-highlight?
   #:resize-sequence-emits-resize?
   #:resize-sequence-consumes-before-next-key?
   #:partial-resize-sequence-is-preserved?))

(in-package #:mine-tests/syntax)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare token-kinds ((coalton:List tok:Token) -> (coalton:List tok:TokenKind)))
  (define (token-kinds toks)
    (map (fn (t) (tok:.token-kind t)) toks))

  (declare short-lambda-introducer-highlights-as-fn? (Void -> Boolean))
  (define (short-lambda-introducer-highlights-as-fn?)
    (let toks = (lexer:lex-line (lexer:lexer-state-new 0) "ƒx.x" 0))
    (== (token-kinds toks)
        [ tok:TokCoaltonKeyword
          tok:TokSymbol ]))

  (declare chained-short-lambda-introducers-each-highlight? (Void -> Boolean))
  (define (chained-short-lambda-introducers-each-highlight?)
    (let toks = (lexer:lex-line (lexer:lexer-state-new 0) "ƒx.ƒy.(+ x y)" 0))
    (== (token-kinds toks)
        [ tok:TokCoaltonKeyword tok:TokSymbol
          tok:TokCoaltonKeyword tok:TokSymbol
          tok:TokOpenParen tok:TokOperator tok:TokWhitespace
          tok:TokSymbol tok:TokWhitespace tok:TokSymbol
          tok:TokCloseParen ]))

  (declare resize-sequence-emits-resize? (Void -> Boolean))
  (define (resize-sequence-emits-resize?)
    (match (input:parse-input [27 91 56 59 52 48 59 49 50 48 116])
      ((Tuple events remainder)
       (and (== (vec:length remainder) 0)
            (match events
              ((Cons (input:IEvResize cols rows) (Nil))
               (and (== cols 120) (== rows 40)))
              (_ False))))))

  (declare resize-sequence-consumes-before-next-key? (Void -> Boolean))
  (define (resize-sequence-consumes-before-next-key?)
    (match (input:parse-input [27 91 56 59 52 48 59 49 50 48 116 97])
      ((Tuple events remainder)
       (and (== (vec:length remainder) 0)
            (match events
              ((Cons (input:IEvResize cols rows)
                     (Cons (input:IEvKey (input:KeyChar ch) (input:ModNone))
                           (Nil)))
               (and (== cols 120) (and (== rows 40) (== ch #\a))))
              (_ False))))))

  (declare partial-resize-sequence-is-preserved? (Void -> Boolean))
  (define (partial-resize-sequence-is-preserved?)
    (match (input:parse-input [27 91 56 59 52])
      ((Tuple events remainder)
       (and (== (vec:length remainder) 5)
            (match events
              ((Nil) True)
              (_ False)))))))

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

(defun check-resize-sequence-emits-resize ()
  (%check (mine-tests/syntax:resize-sequence-emits-resize?)
          "Expected CSI 8;rows;cols t to emit an IEvResize event"))

(defun check-resize-sequence-consumes-before-next-key ()
  (%check (mine-tests/syntax:resize-sequence-consumes-before-next-key?)
          "Expected a resize sequence followed by a key to emit resize then key events"))

(defun check-partial-resize-sequence-is-preserved ()
  (%check (mine-tests/syntax:partial-resize-sequence-is-preserved?)
          "Expected an incomplete resize sequence to remain buffered"))

(defun check-terminal-input-zero-timeout-is-nonblocking ()
  (let* ((runtime (mine/term/terminal::make-%terminal-input-runtime))
         (mailbox (mine/term/terminal::%terminal-input-runtime-mailbox runtime)))
    (%check (null (mine/term/terminal::%terminal-input-runtime-read-batch-timeout runtime 0))
            "Expected zero-timeout input read on an empty mailbox to return NIL")
    (sb-concurrency:send-message mailbox :event)
    (%check (equal '(:event)
                   (mine/term/terminal::%terminal-input-runtime-read-batch-timeout runtime 0))
            "Expected zero-timeout input read to drain pending mailbox events")))
