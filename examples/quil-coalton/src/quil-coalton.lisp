(cl:in-package #:quil-coalton)

(coalton-toplevel
  ;; NOTE: This might have some issues with escaping quotes
  (declare parse-quil-string (Parser String))
  (define parse-quil-string
    (map3
     (fn (a b c) (pack-string b))
     (char #\")
     (many0
      (alt
       (not-char #\")
       (map2 (fn (a b) b)
             (char #\\)
             (char #\"))))
     (char #\")))

  (define-type Name
    (Name String))

  (declare parse-quil-name (Parser Name))
  (define parse-quil-name
    (let ((alpha_ (alt alpha (char #\_)))
          (alphanumeric_ (alt alphanumeric (char #\_)))
          (alphanumeric_- (alt alphanumeric_ (char #\-))))
      (with-context "parsing quil name"
        (map1
         (fn (str)
           (Name (pack-string str)))
         ;; NOTE: Due to not having backtracking, we have to check for
         ;;       hyphens at the end of names with verify.
         (map2
          (fn (a b)
            (append a b))
          (map1 (fn (x) (Cons x Nil)) alpha_)
          (let ((f (fn (xs)
                     (match xs
		       ((Cons x (Nil)) (not (== x #\-)))
		       ((Cons _ xs) (f xs))
		       ((Nil) True)))))
            (verify f (many0 alphanumeric_-))))))))

  (define-type Qubit
    (Qubit Integer))

  (declare parse-quil-qubit (Parser Qubit))
  (define parse-quil-qubit
    (map1 Qubit natural))


  ;;
  ;; Gate Parsing
  ;;

  (define-type SimpleGate
    (SimpleGate Name (List Qubit)))

  (declare parse-quil-simple-gate (Parser SimpleGate))
  (define parse-quil-simple-gate
    (map2 SimpleGate
          parse-quil-name
          (many1 (map2 (fn (_ x) x)
		       whitespace
		       parse-quil-qubit))))

  (define-type Expression
    (Expression String))

  ;; NOTE: Without a formal spec, we are assuming expressions are
  ;;       allowed to be anything without parenthesis.
  (declare parse-quil-expression (Parser Expression))
  (define parse-quil-expression
    (map1 (fn (x) (Expression (pack-string x)))
          (many1 (verify
                  (fn (c)
                    (not (or (== c #\()
                             (== c #\)))))
                  take))))

  (define-type ParametricGate
    (ParametricGate Name (List Expression) (List Qubit)))

  (declare parse-parametric-gate (Parser ParametricGate))
  (define parse-parametric-gate
    (map5
     (fn (name _ expr __ qubits)
       (ParametricGate name expr qubits))
     parse-quil-name
     (char #\()
     (many1 parse-quil-expression)
     (char #\))
     (many1 (map2 (fn (_ x) x)
                  whitespace
                  parse-quil-qubit))))

  (define-type Gate
    (Simple SimpleGate)
    (Parametric ParametricGate)
    (Controlled Gate)
    (Dagger Gate)
    (Forked Gate))

  (define non-newline-whitespace
    (alt (char #\Space)
         (char #\Tab)))

  (declare parse-quil-gate (Parser Gate))
  (define parse-quil-gate
    (Parser
     (fix
      (fn (f str)
	(let ((parser_
		(alt*
		 (make-list
		  (map3 (fn (_ __ g) (Controlled g))
			(parse-string (make-string-view "CONTROLLED"))
                        (many1 non-newline-whitespace)
			(Parser f))
		  (map3 (fn (_ __ g) (Dagger g))
			(parse-string (make-string-view "DAGGER"))
                        (many1 non-newline-whitespace)
			(Parser f))
		  (map3 (fn (_ __ g) (Forked g))
			(parse-string (make-string-view "FORKED"))
                        (many1 non-newline-whitespace)
			(Parser f))
		  (map1 Parametric parse-parametric-gate)
		  (map1 Simple parse-quil-simple-gate)))))
	  ((get-parser parser_) str))))))


  ;;
  ;; Measurement Parsing
  ;;

  (define-type ClassicalMem
    (ClassicalMem Name (Optional Integer)))

  (declare parse-quil-classical-mem (Parser ClassicalMem))
  (define parse-quil-classical-mem
    (map2
     ClassicalMem
     parse-quil-name
     (alt
      (map3
       (fn (_ i __)
         (Some i))
       (char #\[)
       natural
       (char #\])
       )
      (const-value None))))

  (define-type Measurement
    (Measurement Qubit ClassicalMem))

  (declare parse-quil-measure (Parser Measurement))
  (define parse-quil-measure
    (map5
     (fn (_ __ qubit ___ mem)
       (Measurement qubit mem))
     (parse-string (make-string-view "MEASURE"))
     (many1 non-newline-whitespace)
     parse-quil-qubit
     (many1 non-newline-whitespace)
     parse-quil-classical-mem))

  ;;
  ;; Parsing resets
  ;;

  (define-type Reset
    (Reset Qubit)
    StateReset)

  (declare parse-quil-reset (Parser Reset))
  (define parse-quil-reset
    (alt
     (map3 (fn (_ __ q) (Reset q))
           (parse-string (make-string-view "RESET"))
           (many1 non-newline-whitespace)
           parse-quil-qubit)
     (map1 (const StateReset) (parse-string (make-string-view "RESET")))))

  ;;
  ;; Parsing labels
  ;;

  (declare parse-quil-label-name (Parser Name))
  (define parse-quil-label-name
    (map2 (fn (_ a) a)
          (char #\@)
          parse-quil-name))

  (define-type Label
    (Label Name))

  (declare parse-quil-label (Parser Label))
  (define parse-quil-label
    (map3 (fn (_ __ name) (Label name))
          (parse-string (make-string-view "LABEL"))
          (many1 non-newline-whitespace)
          parse-quil-label-name))

  (define-type Control
    (Jump Name)
    (JumpWhen Name ClassicalMem)
    (JumpUnless Name ClassicalMem)
    Halt
    Wait)

  (declare parse-quil-control (Parser Control))
  (define parse-quil-control
    (alt*
     (make-list
      (map3 (fn (_ __ name) (Jump name))
            (parse-string (make-string-view "JUMP"))
            (many1 non-newline-whitespace)
            parse-quil-label-name)
      (map5 (fn (_ __ name ___ mem) (JumpWhen name mem))
            (parse-string (make-string-view "JUMP-WHEN"))
            (many1 non-newline-whitespace)
            parse-quil-label-name
            (many1 non-newline-whitespace)
            parse-quil-classical-mem)
      (map5 (fn (_ __ name ___ mem) (JumpUnless name mem))
            (parse-string (make-string-view "JUMP-UNLESS"))
            (many1 non-newline-whitespace)
            parse-quil-label-name
            (many1 non-newline-whitespace)
            parse-quil-classical-mem)
      (map1 (const Halt) (parse-string (make-string-view "HALT")))
      (map1 (const Wait) (parse-string (make-string-view "WAIT")))
      )))

  ;;
  ;; Parsing full quil statements
  ;;

  (define-type QuilStatement
    (QuilMeas Measurement)
    (QuilGate Gate)
    (QuilReset Reset)
    (QuilLabel Label)
    (QuilControl Control))

  (declare parse-quil-statement (Parser QuilStatement))
  (define parse-quil-statement
    (alt*
     (make-list
      (map1 QuilMeas parse-quil-measure)
      (map1 QuilReset parse-quil-reset)
      (map1 QuilControl parse-quil-control)
      (map1 QuilLabel parse-quil-label)
      (map1 QuilGate parse-quil-gate))))
  
  ;;
  ;; Comments
  ;;
  
  (declare parse-quil-comment (Parser Unit))
  (define parse-quil-comment
    (map3 (fn (a b c) Unit)
          (many0 non-newline-whitespace)
          (char #\#)
          (many0 (not-char #\Newline))))

  (declare parse-quil-comment-line (Parser Unit))
  (define parse-quil-comment-line
    (map3 (fn (_ __ ___) Unit)
          (many0 whitespace)
          parse-quil-comment
          (alt (map1 (const Unit) (char #\Newline))
               eof)))

  ;;
  ;; Parsing full quil program
  ;;

  (define-type QuilProgram
    (QuilProgram (List QuilStatement)))

  (declare parse-quil-program (Parser QuilProgram))
  (define parse-quil-program
    (map1
     QuilProgram
     (map2 const
           (many0
            (map2 (fn (a _) a)
                  ;; Quil statements
                  (map4
                   (fn (_ a __ ___) a)
                   ;; Allow leading whitespace (including newlines) and comments
                   (many0 (alt parse-quil-comment-line whitespace))
                   parse-quil-statement
                   ;; Allow comments after
                   (many0 non-newline-whitespace)
                   (option parse-quil-comment))
                  ;; End of lines
                  (alt eof
                       (map1 (const Unit)
                             (alt (char #\;)
                                  (char #\Newline))))))
           ;; Grab any trailing whitespace or comments
           (many0 (alt whitespace parse-quil-comment-line)))))

  ;; Now, we can expose the functionality to the world
  (define (run-quil-parser str)
    (run-parser parse-quil-program (make-string-view str))))
