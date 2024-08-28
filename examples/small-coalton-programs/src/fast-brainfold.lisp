;;;;
;;;; A Brainfold/Brainf*** interpreter implemented in Coalton
;;;;
;;;; This interpreter handles all standard bf commands:
;;;; ( > < + - . , [ ] ).
;;;;
;;;;
;;;; Run Brainfold programs with (run-program "+++++[-]+++")
;;;;
;;;; or
;;;;
;;;; (run-file "/path/to/your/file.bf")
;;;;
;;;; try (coalton (hello-world)) in the REPL!
;;;;

(cl:defpackage #:brainfold
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:iter #:coalton-library/iterator)
   (#:cell #:coalton-library/cell)
   (#:char #:coalton-library/char)
   (#:str #:coalton-library/string)
   (#:list #:coalton-library/list)
   (#:arith #:coalton-library/math)
   (#:state #:coalton-library/monad/state)
   (#:file #:coalton-library/file))
  (:export
   #:eval
   #:run-program
   #:run-file

   ;; Examples
   #:hello-world
   #:gnarly-hello-world
   #:squares))

(in-package #:brainfold)

(named-readtables:in-readtable coalton:coalton)

(cl:declaim (cl:optimize (cl:speed 3)))
;;;
;;; State/Env
;;;

(coalton-toplevel

  (define-struct BF-State
    (memory       "The brainfold memory array."        (Vector Integer))
    (pointer      "A pointer to the current register." (Cell UFix)))

  ;;
  ;; Generating a Brainfold memory vector
  ;;

  (declare bf-vector-size UFix)
  (define bf-vector-size 1000)

  ;;
  ;; Generating a default BF-State:
  ;;

  (define-instance (Default BF-State)
    (define (default)
      (BF-State (vec:with-initial-element bf-vector-size 0)
                (cell:new 0))))

  ;;
  ;; Accessing the current value
  ;;

  (declare value-at-pointer (BF-State -> Integer))
  (define (value-at-pointer bfs)
    "Returns the value at the current pointer."
    (vec:index-unsafe (cell:read (.pointer bfs))
		      (.memory bfs))))

;;;
;;; Commands (Functions called by Brainfold Cmds)
;;;

(coalton-toplevel

  ;;
  ;; Navigating through bf-cells (> <)
  ;;

  (declare move-right (BF-State -> Unit))
  (define (move-right state)
    "Moves the pointer one bf-cell to the right."
    (cell:increment! (.pointer state))
    Unit)

  (declare move-left (BF-State -> Unit))
  (define (move-left state)
    "Moves the pointer one bf-cell to the left."
    (cell:decrement! (.pointer state))
    Unit)

  ;;
  ;; Changing bf-cell values (+ -)
  ;;

  (declare incr (BF-State -> Unit))
  (define (incr state)
    "Increments the value for the current bf-cell."
    (vec:set! (cell:read (.pointer state))
                     (1+ (value-at-pointer state))
                     (.memory state))
    Unit)

  (declare decr (BF-State -> Unit))
  (define (decr state)
    "Decrements the value for the current bf-cell."
    (vec:set! (cell:read (.pointer state))
                     (1- (value-at-pointer state))
                     (.memory state))
    Unit)

  ;;
  ;; Printing Cells (.)
  ;;

  (declare display (BF-State -> Unit))
  (define (display state)
    "Prints the value at the pointer to the print buffer."
    (let val = (value-at-pointer state))
    (lisp Unit (val)
      (cl:format cl:*standard-output* "~a" (cl:code-char val))
      Unit))

  ;;
  ;; Taking Input (,)
  ;;
  ;; Currently takes individual characters one at a time as prompted

  (define (prompt-char)
    "A prompt for obtaining one character as input."
    (Lisp Char ()
      (cl:format cl:*query-io* "Input a character: ")
      (cl:finish-output cl:*query-io*)
      (cl:read-char cl:*query-io*)))

  (declare take-input (BF-State -> Unit))
  (define (take-input state)
    "Takes and stores a character as an ascii code at the pointer."
    (vec:set! (cell:read (.pointer state))
                     (into (char:char-code (prompt-char)))
                     (.memory state))))

;;;
;;; Parsing/Lexing
;;;

(coalton-toplevel

  (define-type Cmd
    BFRight
    BFLeft
    BFPlus
    BFMinus
    BFPrint
    BFInput
    (BFLoop (Vector Cmd)))

  (declare parse (String -> (Vector Cmd)))
  (define (parse input-string)
    "Parses a Brainfold instruction string, returns a Vector of Brainfold Commands."
    (let cmds = (vec:new))
    (let vecs = (vec:new))
    ;(let counter = (cell:new 0))
    (let ((parser (fn (input-string v)
     ;;               (traceobject "cmd#" (cell:read counter))
      ;;              (cell:increment! counter!)
                    (let ((head-tail (str:split 1 input-string)))
                      (match (fst head-tail)
                        ("" cmds)
                        (">"
                         (vec:push! BFRight v)
                         (parser (snd head-tail) v))
                        ("<"
                         (vec:push! BFLeft v)
                         (parser (snd head-tail) v))
                        ("+"
                         (vec:push! BFPlus v)
                         (parser (snd head-tail) v))
                        ("-"
                         (vec:push! BFMinus v)
                         (parser (snd head-tail) v))
                        ("."
                         (vec:push! BFPrint v)
                         (parser (snd head-tail) v))
                        (","
                         (vec:push! BFInput v)
                         (parser (snd head-tail) v))
                        ("["
                         (vec:push! v vecs)
                         (parser (snd head-tail) (vec:new)))
                        ("]"
                         (vec:push! (BFLoop v) (unwrap (vec:last vecs)))
                         (parser (snd head-tail) (unwrap (vec:pop! vecs))))
                        (_ (parser (snd head-tail) v)))))))
      (parser input-string cmds))))

;;;
;;; Evaluation
;;;

(coalton-toplevel

  (declare exec (BF-State -> Cmd -> Unit))
  (define (exec state cmd)
    "Executes a given bf command."
    ;;(traceobject "pos, current-val" (Tuple (cell:read (.pointer state)) (value-at-pointer state)))
    (match cmd
      ((BFRight) (move-right state))
      ((BFLeft) (move-left state))
      ((BFPlus) (incr state))
      ((BFMinus) (decr state))
      ((BFPRint) (display state))
      ((BFInput) (take-input state))
      ((BFLoop v) (exec-loop state v))))


  (declare exec-cmds (BF-State -> (Vector Cmd) -> Unit))
  (define (exec-cmds state cmds)
    "Executes a list of bf-commands."
    (for cmd in cmds
      (exec state cmd)))

  (declare exec-loop (BF-State -> (Vector Cmd) -> Unit))
  (define (exec-loop state cmds)
    "Executes a list of commands until the value at the pointer is 0."
    (match (value-at-pointer state)
      (0 Unit)
      (_ (exec-cmds state cmds)
         (exec-loop state cmds))))

  (declare eval (String -> Unit))
  (define (eval input-string)
    "Parses and evaluates a string of brainfold input."
    (exec-cmds (default) (into (parse input-string)))))


;;;
;;; Top Level
;;;

(coalton-toplevel

  (declare run-program (String -> Unit))
  (define (run-program bf-string)
    "Evaluates and executes a bf-command string on a fresh state."
    (eval bf-string))

  (define (run-file filepath)
    "Loads and executes the brainfold file at the given filepath."
    ;;(run-program (unwrap (file:read-file-to-string filepath)))
    (eval (unwrap (file:read-file-to-string filepath)))))


;;;
;;; Sample test programs
;;;

(coalton-toplevel

  ;; from https://esolangs.org/wiki/Brainfuck

  (define (hello-world)
    (run-program
     "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+."))

  (define (gnarly-hello-world)
    (run-program
     ">++++++++[-<+++++++++>]<.>>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.>->+++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+."))

  ;; from https://github.com/saulpw/brainfuck/tree/master/tests

  (define (squares)
    (run-program "++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+>>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]<<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]")))
