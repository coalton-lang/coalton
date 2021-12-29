(in-package #:coalton-library)

;;;
;;; String
;;;

(coalton-toplevel
  (declare concat-string (String -> String -> String))
  (define (concat-string str1 str2)
    "Concatenate STR1 and STR2 together, returning a new string."
    (lisp String (str1 str2)
      (cl:concatenate 'cl:string str1 str2)))

  (declare reverse-string (String -> String))
  (define (reverse-string s)
    "Reverse a string."
    (lisp String (s) (cl:reverse s)))

  (declare string-length (String -> Integer))
  (define (string-length str)
    "The length of a string STR."
    (lisp Integer (str)
      (cl:length str)))

  (declare substring (String -> Integer -> Integer -> String))
  (define (substring str start end)
    "Compute a substring of a string bounded by given indices."
    (let ((real-start (max 0 (min start end)))
          (real-end (min (string-length str) (max start end))))
      (lisp String (real-start real-end str)
        (cl:subseq str real-start real-end))))

  (declare parse-int (String -> (Optional Integer)))
  (define (parse-int str)
    "Parse the integer in string STR."
    (lisp (Optional Integer) (str)
      (cl:let ((x (cl:parse-integer str :junk-allowed cl:t)))
        (cl:if x
               (Some x)
               None))))
  ;;
  ;; String Instances
  ;;

  (define-instance (Eq String)
    (define (== a b)
      (lisp Boolean (a b) (to-boolean (cl:string= a b)))))

  (define-instance (Ord String)
    (define (<=> a b)
      (lisp Ord (a b)
         (cl:cond
           ((cl:string> a b) GT)
           ((cl:string< a b) LT)
           (cl:t EQ)))))

  (define-instance (Semigroup String)
    (define (<> a b) (concat-string a b)))

  (define-instance (Monoid String)
    (define mempty ""))

  (define-instance (Into String (List Char))
    (define (into str)
      (lisp (List Char) (str)
        (cl:coerce str 'cl:list))))

  (define-instance (Into (List Char) String)
    (define (into lst)
      (lisp String (lst)
        (cl:coerce lst 'cl:string))))

  (define-instance (Iso (List Char) String)))

(define-sxhash-hasher String)
