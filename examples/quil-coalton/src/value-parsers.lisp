(cl:in-package #:quil-coalton)

(coalton-toplevel
  ;;
  ;; Define some basic value parsers
  ;;

  (declare eof (Parser Unit))
  (define eof
    (Parser
     (fn (str)
       (match (next-char str)
         ((Some (Tuple read-char _)) (Err (Error (lisp String (cl:format cl:nil "Unexpected character '~A' expected EOF" read-char)))))
         ((None) (Ok (Tuple Unit str)))))))

  (declare take (Parser Char))
  (define take
    (Parser
     (fn (str)
       (match (next-char str)
         ((Some t_)
          (Ok t_))
         ((None) (Err parse-error-eof))))))

  (declare char (Char -> (Parser Char)))
  (define (char c)
    (Parser
     (fn (str)
       (match (next-char str)
         ((Some t_)
          (let ((read-char (fst t_)))
            (if (== c read-char)
                (Ok t_)
                (Err (Error (lisp String (cl:format cl:nil "Unexpected character '~A' expected '~A'" read-char c)))))))
         ((None) (Err parse-error-eof))))))

  (declare not-char (Char -> (Parser Char)))
  (define (not-char c)
    (Parser
     (fn (str)
       (match (next-char str)
         ((Some t_)
          (let ((read-char (fst t_)))
            (if (== c read-char)
                (Err (Error (lisp String (cl:format cl:nil "Unexpected character '~A' expected not '~A'" read-char c))))
                (Ok t_))))
         ((None) (Err parse-error-eof))))))

  (declare parse-string (StringView -> (Parser StringView)))
  (define (parse-string str)
    (let ((f (fn (s)
	       (match (next-char s)
                 ((Some (Tuple c s))
                  (and-then (fn (_) (f s)) (char c)))
                 ((None) (const-value str))))))
      (f str)))

  (declare whitespace (Parser Unit))
  (define whitespace
    (map1 (fn (_) Unit)
          (alt (char #\Space)
	       (char #\Return))))

  (declare digit (Parser Char))
  (define digit
    (map-error
     (fn (_) (Error "Invalid digit"))
     (verify
      (fn (x) (and (>= x #\0)
                   (<= x #\9)))
      take)))

  (declare lowercase (Parser Char))
  (define lowercase
    (map-error
     (fn (_) (Error "Invalid lowercase character"))
     (verify
      (fn (x) (and (>= x #\a)
                   (<= x #\z)))
      take)))

  (declare uppercase (Parser Char))
  (define uppercase
    (map-error
     (fn (_) (Error "Invalid uppercase character"))
     (verify
      (fn (x) (and (>= x #\A)
                   (<= x #\Z)))
      take)))

  (declare alpha (Parser Char))
  (define alpha (alt lowercase uppercase))

  (declare alphanumeric (Parser Char))
  (define alphanumeric (alt alpha digit))

  (declare natural (Parser Int))
  (define natural
    (with-context "While parsing natural number"
      (and-then
       (fn (i)
         (match i
           ((Some a) (const-value a))
           ((None) (pfail "Invalid integer"))))
       (map1 parse-int (map1 pack-string (many1 digit)))))))
