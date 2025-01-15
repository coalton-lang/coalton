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
         ((Some (Tuple read-char _)) (Err (ParseError (fmt:format "Unexpected character " (into read-char) " expected EOF"))))
         ((None) (Ok (Tuple Unit str)))))))

  (declare take (Parser coalton:Char))
  (define take
    (Parser
     (fn (str)
       (match (next-char str)
         ((Some t_)
          (Ok t_))
         ((None) (Err parse-error-eof))))))

  (declare char (coalton:Char -> (Parser coalton:Char)))
  (define (char c)
    (Parser
     (fn (str)
       (match (next-char str)
         ((Some t_)
          (let ((read-char (fst t_)))
            (if (== c read-char)
                (Ok t_)
                (Err (ParseError (fmt:format "Unexpected character " (into read-char) " expected " (into c)))))))
         ((None) (Err parse-error-eof))))))

  (declare not-char (coalton:Char -> (Parser coalton:Char)))
  (define (not-char c)
    (Parser
     (fn (str)
       (match (next-char str)
         ((Some t_)
          (let ((read-char (fst t_)))
            (if (== c read-char)
                (Err (ParseError (fmt:format "Unexpected character " (into read-char) " expected not " (into c))))
                (Ok t_))))
         ((None) (Err parse-error-eof))))))

  (declare parse-string (StringView -> (Parser StringView)))
  (define (parse-string str)
    (let ((f (fn (s)
               (match (next-char s)
                 ((Some (Tuple c s))
                  (>>= (char c) (fn (_) (f s))))
                 ((None) (const-value str))))))
      (f str)))

  (declare whitespace (Parser Unit))
  (define whitespace
    (map (fn (_) Unit)
         (alt (char #\Space)
              (char #\Return))))

  (declare digit (Parser coalton:Char))
  (define digit
    (map-error
     (fn (_) (ParseError "Invalid digit"))
     (verify
      (fn (x) (and (>= x #\0)
                   (<= x #\9)))
      take)))

  (declare lowercase (Parser coalton:Char))
  (define lowercase
    (map-error
     (fn (_) (ParseError "Invalid lowercase character"))
     (verify
      (fn (x) (and (>= x #\a)
                   (<= x #\z)))
      take)))

  (declare uppercase (Parser coalton:Char))
  (define uppercase
    (map-error
     (fn (_) (ParseError "Invalid uppercase character"))
     (verify
      (fn (x) (and (>= x #\A)
                   (<= x #\Z)))
      take)))

  (declare alpha (Parser coalton:Char))
  (define alpha (alt lowercase uppercase))

  (declare alphanumeric (Parser coalton:Char))
  (define alphanumeric (alt alpha digit))

  (declare natural (Parser Integer))
  (define natural
    (with-context "While parsing natural number"
      (>>= (map string:parse-int (map into (many1 digit)))
           (fn (i)
             (match i
               ((Some a) (const-value a))
               ((None) (fail "Invalid integer"))))))))
