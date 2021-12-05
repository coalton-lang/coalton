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

  (declare unpack-string (String -> (List Char)))
  (define (unpack-string str)
    "Unpack a string into a list of characters."
    (lisp (List Char) (str)
       (cl:coerce str 'cl:list)))

  (declare pack-string ((List Char) -> String))
  (define (pack-string xs)
    "Pack a list of charactes into a string."
    (lisp String (xs)
      (cl:coerce xs 'cl:string)))

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
    (define into unpack-string))

  (define-instance (Into (List Char) String)
    (define into pack-string))

  (define-instance (Iso (List Char) String)))
