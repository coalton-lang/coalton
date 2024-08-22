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

(cl:require :sb-sprof)

(cl:setf coalton-impl/settings:*coalton-heuristic-inlining* cl:t)

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

(coalton-toplevel

  (declare current-cell ((Vector Integer) -> UFix -> Integer))
  (define (current-cell tape pos)
    "Returns the value at the current cell."
    (vec:index-unsafe pos tape))

  (declare incr ((Vector Integer) -> UFix -> UFix))
  (define (incr tape pos)
    (vec:set! pos (1+ (current-cell tape pos)) tape)
    pos)

  (declare decr ((Vector Integer) -> UFix -> UFix))
  (define (decr tape pos)
    (vec:set! pos (1- (current-cell tape pos)) tape)
    pos)

  (declare display ((Vector Integer) -> UFix -> UFix))
  (define (display tape pos)
    "Prints the value at the pointer to the print buffer."
    (let val = (current-cell tape pos))
    (lisp Unit (val)
      (cl:format cl:*standard-output* "~a" (cl:code-char val))
      Unit)
    pos)

  (declare run (String -> UFix))
  (define (run input-string)
    "Parses and runs a Brainfold instruction string, returns the length of the input string."
    (let tape = (vec:with-initial-element 30000 0))
    (let stack = (vec:new))
    (let ((next-end (fn (i)
                      (if (== (unwrap (str:ref input-string i))
                              #\])
                          i
                          (next-end (1+ i)))))
          (parse-and-run (fn (i pos)
                           (unwrap-or-else
                            (fn (x)
                              (traceobject "cmd - val" (Tuple x (current-cell tape pos)))
                              (match x
                                (#\>
                                 (parse-and-run (1+ i) (1+ pos)))
                                (#\<
                                 (parse-and-run (1+ i) (1- pos)))
                                (#\+
                                 (parse-and-run (1+ i) (incr tape pos)))
                                (#\-
                                 (parse-and-run (1+ i) (decr tape pos)))
                                (#\.
                                 (parse-and-run (1+ i) (display tape pos)))
                                (#\[

                                 (cond ((zero? (current-cell tape pos))
                                        (trace "skipped")
                                        (parse-and-run (1+ (next-end i)) pos))
                                       (True
                                        (vec:push! i stack)
                                        (traceobject "stacked " stack)
                                        (parse-and-run (1+ i) pos))))
                                (#\]
                                 (let popped = (vec:pop-unsafe! stack))
                                 (traceobject "popped" stack;;(Tuple (current-cell tape pos) stack)
                                              )
                                 (cond ((zero? (current-cell tape pos))
                                        (trace "continued")
                                        (parse-and-run (1+ i) pos))
                                       (True
                                        (parse-and-run popped pos ;;(vec:pop-unsafe! stack) pos
                                                       ))))
                                (_ (parse-and-run (1+ i) pos))))
                            (fn ()
                              i)
                            (str:ref input-string i)))))
      (parse-and-run 0 0)))

  ;; make another run that has cells for pos and idx
  (declare run-file (String -> UFix))
  (define (run-file filename)
    (run (unwrap (file:read-file-to-string filename)))))

;;;
;;; Sample test programs
;;;

(coalton-toplevel

  ;; from https://esolangs.org/wiki/Brainfuck
  (define (hello-world)
    (run
     "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+."))

  (define (gnarly-hello-world)
    (run
     ">++++++++[-<+++++++++>]<.>>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.>->+++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+."))

  ;; from https://github.com/saulpw/brainfuck/tree/master/tests

  (define (squares)
    (run "++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>[<<+>>-]>>>[-]++>[-]+>>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]<<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]")))

(cl:defun profile-bench ()
  (sb-sprof:with-profiling (:max-samples 100000
                            :sample-interval 0.001
                            :report :flat
                            :loop cl:nil)
    (coalton (run-file "bench.b"))))
#+ig
(cl:defun profile-hello-world ()
  (sb-sprof:with-profiling (:max-samples 1000
                            :report :flat
                            :loop cl:nil)
    (coalton (brainfold:hello-world))))
