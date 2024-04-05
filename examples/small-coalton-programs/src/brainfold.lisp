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
   (#:arith #:coalton-library/math))

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

;;;
;;; State/Env
;;;

(coalton-toplevel

  (define-struct State
    (memory (Vector Integer))
    (pointer (Cell UFix))
    (print-buffer (Cell String)))
  
  ;;
  ;; Generating a Brainfold memory vector
  ;;

  (declare bf-vector-size UFix)
  (define bf-vector-size 1000)
  
  (define (generate-bf-vector)
    "Initializes the brainfold array."
    (let v = (vec:new))
    (vec:extend!  v (iter:repeat-for 0 bf-vector-size))
    v)

  ;;
  ;; Generating a default State:
  ;;

  (define (new-state)
    "Generates a new, blank Brainfold State."
    (State (generate-bf-vector) (cell:new 0) (cell:new "")))

  ;;
  ;; Accessing the current value
  ;;

  (declare value-at-pointer (State -> Integer))
  (define (value-at-pointer State)
    "Returns the value at the current pointer."
    (vec:index-unsafe (cell:read (.pointer State))
		      (.memory State))))


;;;
;;; Commands (Functions called by Brainfold Cmds)
;;;

(coalton-toplevel

  ;;
  ;; Navigating through bf-cells (> <)
  ;;
  
  (declare move-right (State -> State))
  (define (move-right s)
    "Moves the pointer one bf-cell to the right."
    (cell:increment! (.pointer s))
    s)

  (declare move-left (State -> State))
  (define (move-left s)
    "Moves the pointer one bf-cell to the left."
    (cell:decrement! (.pointer s))
    s)

  ;;
  ;; Changing bf-cell values (+ -)
  ;;

  (declare incr (State -> State))
  (define (incr s)
    "Increments the value for the current bf-cell."
    (vec:set! (cell:read (.pointer s))
              (1+ (value-at-pointer s))
              (.memory s))
    s)

  (declare decr (State -> State))
  (define (decr s)
    "Decrements the value for the current bf-cell."
    (vec:set! (cell:read (.pointer s))
              (1- (value-at-pointer s))
              (.memory s))
    s)

  ;;
  ;; Printing Cells (.)
  ;;
  
  (declare find-ascii (UFix -> String))
  (define (find-ascii ascii-value)
    "Converts an ASCII character code into a string."
    (into (make-list (unwrap (char:code-char ascii-value)))))

  (declare print (State -> State))
  (define (print s)
    "Prints the value at the pointer to the print buffer."
    (cell:write! (.print-buffer s)
                 (str:concat
                  (cell:read (.print-buffer s))
                  (find-ascii
                   (fromint (abs (value-at-pointer s))))))
    s)

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

  (declare take-input (State -> State))
  (define (take-input s)
    "Takes and stores a character as an ascii code at the pointer."
    (vec:set! (cell:read (.pointer s))
              (into (char:char-code (prompt-char)))
              (.memory s))
    s))


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
    (let ((parser (fn (input-string v)
                    (let ((head-tail (str:bisect 1 input-string)))
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

  (declare exec (Cmd -> (State -> State))) 
  (define (exec cmd)
    "Executes a given bf command."
    (match cmd
      ((BFRight) move-right)
      ((BFLeft) move-left)
      ((BFPlus) incr)
      ((BFMinus) decr)
      ((BFPRint) print)
      ((BFInput) take-input)
      ((BFLoop v) (exec-loop v))))

  (declare exec-cmds ((Vector Cmd) -> State -> State))
  (define (exec-cmds cmds s)
    "Executes a list of bf-commands."
    (let l = (1- (vec:length cmds)))
    (let ((f (fn (index s)
                (if (== l index)
                    (exec (vec:index-unsafe index cmds) s)
                    (f (1+ index) (exec (vec:index-unsafe index cmds) s))))))
      (f 0 s)))

  (declare exec-loop ((Vector Cmd) -> State -> State))
  (define (exec-loop cmds s)
    "Loops commands until the value at the pointer is 0."
    (match (value-at-pointer s)
      (0 s)
      (_ (exec-loop cmds (exec-cmds cmds s)))))

  (define (eval input-string s)
    "Parses and evaluates a string of brainfold input."
    (exec-cmds (parse input-string) s)))


;;;
;;; Top Level
;;;

(coalton-toplevel

  (declare run-program (String -> String))
  (define (run-program bf-string)
    "Evaluates and executes a bf-command string on a fresh state."
    (cell:read (.print-buffer (eval bf-string (new-state)))))

  (define (run-file filepath)
    "Loads and executes the brainfold file at the given filepath."
    (run-program (Lisp String (filepath)
                   (uiop:read-file-string filepath)))))


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
