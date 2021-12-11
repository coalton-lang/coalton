(cl:in-package #:quil-coalton)

(coalton-toplevel
  (define-type ParseError
    (ParseError String)
    (Context String ParseError))

  (define parse-error-eof (ParseError "EOF"))

  (declare incomplete-parse-error (String -> ParseError))
  (define (incomplete-parse-error str)
    (ParseError (lisp String (str) (cl:format cl:nil "Parser did not complete: ~A" str))))

  (define-type (Parser :a)
    (Parser (StringView -> (Result ParseError (Tuple :a StringView)))))

  (declare get-parser ((Parser :a) -> StringView -> (Result ParseError (Tuple :a StringView))))
  (define (get-parser p)
    (match p
      ((Parser x) x)))

  (declare run-parser ((Parser :a) -> StringView -> (Result ParseError :a)))
  (define (run-parser p_ str)
    ;; Unwrap Parser function
    (let ((p (get-parser p_)))
      (match (p str)
        ((Err e) (Err e))
        ((Ok (Tuple a str))
         (if (string-view-empty-p str)
             (Ok a)
             (Err (incomplete-parse-error (string-view-get str))))))))

  ;;
  ;; Error tracking
  ;;
  
  (declare with-context (String -> (Parser :a) -> (Parser :a)))
  (define (with-context s p)
    (map-error (Context s) p))

  (declare map-error ((ParseError -> ParseError) -> (Parser :a) -> (Parser :a)))
  (define (map-error f p_)
    (let ((p (get-parser p_)))
      (Parser
       (fn (str)
         (match (p str)
           ((Err e) (Err (f e)))
           ((Ok (Tuple a str))
            (Ok (Tuple a str))))))))

  ;;
  ;; Some basic parsers
  ;;

  (declare const-value (:a -> (Parser :a)))
  (define (const-value x)
    (Parser
     (fn (str)
       (Ok (Tuple x str))))))
