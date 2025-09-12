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
   (#:vec #:coalton-library/collections/mutable/vector)
   (#:iter #:coalton-library/iterator)
   (#:cell #:coalton-library/cell)
   (#:char #:coalton-library/char)
   (#:str #:coalton-library/string)
   (#:list #:coalton-library/collections/immutable/list)
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

;;;
;;; State/Env
;;;

(coalton-toplevel

  (define-struct BF-State
    (memory       "The brainfold memory array."        (Vector Integer))
    (pointer      "A pointer to the current register." (Cell UFix))
    (print-buffer "The print buffer."                  (Cell String)))
  
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
      (BF-State (new-repeat bf-vector-size 0)
                (cell:new 0)
                (cell:new ""))))

  ;;
  ;; Accessing the current value
  ;;

  (declare value-at-pointer (BF-State -> Integer))
  (define (value-at-pointer bfs)
    "Returns the value at the current pointer."
    (at# (cell:read (.pointer bfs))
         (.memory bfs))))

;;;
;;; Commands (Functions called by Brainfold Cmds)
;;;

(coalton-toplevel

  ;;
  ;; Navigating through bf-cells (> <)
  ;;

  (declare move-right (Unit -> (state:ST BF-State Unit)))
  (define (move-right)
    "Moves the pointer one bf-cell to the right."
    (do
     (bfs <- state:get)
     (pure (cell:increment! (.pointer bfs)))
      (state:put bfs)))

  (define (dec! cell)
    (let ((value (cell:read cell)))
      (if (arith:zero? value)
          0
          (cell:write! cell (1- value)))))

  (declare move-left (Unit -> (state:ST BF-State Unit)))
  (define (move-left)
    "Moves the pointer one bf-cell to the left."
    (do
     (bfs <- state:get)
     (pure (dec! (.pointer bfs)))
      (state:put bfs)))

  ;;
  ;; Changing bf-cell values (+ -)
  ;;

  (declare incr (Unit -> (state:ST BF-State Unit)))
  (define (incr)
    "Increments the value for the current bf-cell."
    (do
     (bfs <- state:get)
     (pure (set-at! (cell:read (.pointer bfs))
                     (1+ (value-at-pointer bfs))
                     (.memory bfs)))
     (pure Unit)))

  (declare decr (Unit -> (state:ST BF-State Unit)))
  (define (decr)
    "Decrements the value for the current bf-cell."
    (do
     (bfs <- state:get)
     (pure (set-at! (cell:read (.pointer bfs))
                     (1- (value-at-pointer bfs))
                     (.memory bfs)))
     (pure Unit)))

  ;;
  ;; Printing Cells (.)
  ;;

  (declare find-ascii (UFix -> String))
  (define (find-ascii ascii-value)
    "Converts an ASCII character code into a string."
    (into (make-list (unwrap (char:code-char ascii-value)))))

  (declare display (Unit -> (state:ST BF-State Unit)))
  (define (display)
    "Prints the value at the pointer to the print buffer."
    (do
     (bfs <- state:get)
     (pure (cell:write! (.print-buffer bfs)
                        (str:concat
                         (cell:read (.print-buffer bfs))
                         (find-ascii
                          (fromint (abs (value-at-pointer bfs)))))))
      (state:put bfs)))

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

  (declare take-input (Unit -> (state:ST BF-State Unit)))
  (define (take-input)
    "Takes and stores a character as an ascii code at the pointer."
    (do
     (bfs <- state:get)
     (pure (set-at! (cell:read (.pointer bfs))
                     (into (char:char-code (prompt-char)))
                     (.memory bfs)))
      (state:put bfs))))

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
    (let cmds = (new-collection))
    (let vecs = (the (Vector (Vector Cmd)) (vec:make)))
    (let ((parser (fn (input-string v)
                    (let ((head-tail (str:split 1 input-string)))
                      (match (fst head-tail)
                        ("" cmds)
                        (">"
                         (push-end! BFRight v)
                         (parser (snd head-tail) v))
                        ("<"
                         (push-end! BFLeft v)
                         (parser (snd head-tail) v))
                        ("+"
                         (push-end! BFPlus v)
                         (parser (snd head-tail) v))
                        ("-"
                         (push-end! BFMinus v)
                         (parser (snd head-tail) v))
                        ("."
                         (push-end! BFPrint v)
                         (parser (snd head-tail) v))
                        (","
                         (push-end! BFInput v)
                         (parser (snd head-tail) v))
                        ("["
                         (push-end! v vecs)
                         (parser (snd head-tail) (new-collection)))
                        ("]"
                         (push-end! (BFLoop v) (unwrap (last vecs)))
                         (parser (snd head-tail) (unwrap (pop-end! vecs))))
                        (_ (parser (snd head-tail) v)))))))
      (parser input-string cmds))))

;;;
;;; Evaluation
;;;

(coalton-toplevel

  (declare exec (Cmd -> (state:ST BF-State Unit)))
  (define (exec cmd)
    "Executes a given bf command."
    (match cmd
      ((BFRight) (move-right))
      ((BFLeft) (move-left))
      ((BFPlus) (incr))
      ((BFMinus) (decr))
      ((BFPRint) (display))
      ((BFInput) (take-input))
      ((BFLoop v) (exec-loop (into v)))))


  (declare exec-cmds ((List Cmd) -> (state:ST BF-State Unit)))
  (define (exec-cmds cmds)
    "Executes a list of bf-commands."
    (match (into cmds)
      ((Cons x xs)
       (do
        (exec x)
        (exec-cmds xs)))
      ((Nil)
       (do
        (pure Unit)))))

  (declare exec-loop ((List Cmd) -> (state:ST BF-State Unit)))
  (define (exec-loop cmds)
    "Executes a list of commands until the value at the pointer is 0."
    (do
     (bfs <- state:get)
     (match (value-at-pointer bfs)
       (0 (pure Unit))
       (_ (do (exec-cmds cmds)
              (exec-loop cmds))))))

  (declare eval (String -> (state:ST BF-State Unit)))
  (define (eval input-string)
    "Parses and evaluates a string of brainfold input."
    (exec-cmds (into (parse input-string)))))


;;;
;;; Top Level
;;;

(coalton-toplevel

  (declare run-program (String -> String))
  (define (run-program bf-string)
    "Evaluates and executes a bf-command string on a fresh state."
    (cell:read (.print-buffer (fst (state:run (eval bf-string) (default))))))

  (define (run-file filepath)
    "Loads and executes the brainfold file at the given filepath."
    (run-program (unwrap (file:read-file-to-string filepath)))))


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
