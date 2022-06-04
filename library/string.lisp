(coalton-library/utils:defstdlib-package #:coalton-library/string
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:export
   #:concat
   #:reverse
   #:length
   #:substring
   #:strip-prefix
   #:strip-suffix
   #:parse-int
   #:ref
   #:ref-unchecked))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(in-package #:coalton-library/string)

;;;
;;; String
;;;

(coalton-toplevel
  (declare concat (String -> String -> String))
  (define (concat str1 str2)
    "Concatenate STR1 and STR2 together, returning a new string."
    (lisp String (str1 str2)
      (cl:concatenate 'cl:string str1 str2)))

  (declare reverse (String -> String))
  (define (reverse s)
    "Reverse a string."
    (lisp String (s) (cl:reverse s)))

  (declare length (String -> Integer))
  (define (length str)
    "The length of a string STR."
    (lisp Integer (str)
      (cl:length str)))

  (declare substring (String -> Integer -> Integer -> String))
  (define (substring str start end)
    "Compute a substring of a string bounded by given indices."
    (let ((real-start (max 0 (min start end)))
          (real-end (min (length str) (max start end))))
      (lisp String (real-start real-end str)
        (cl:subseq str real-start real-end))))

  (declare strip-prefix (String -> String -> (Optional String)))
  (define (strip-prefix prefix str)
    "Returns a string without a give prefix, or None if the string
does not have that suffix."
    (let prefix-len = (length prefix))
    (let substr = (substring str 0 prefix-len))
    (if (== substr prefix)
        (Some (substring str prefix-len (length str)))
        None))

  (declare strip-suffix (String -> String -> (Optional String)))
  (define (strip-suffix suffix str)
    "Returns a string without a give suffix, or None if the string
does not have that suffix."
    (let suffix-len = (length suffix))
    (let str-len = (length str))
    (let substr = (substring str (- str-len suffix-len) str-len))
    (if (== substr suffix)
        (Some (substring str 0 (- str-len suffix-len)))
        None))

  (declare parse-int (String -> (Optional Integer)))
  (define (parse-int str)
    "Parse the integer in string STR."
    (lisp (Optional Integer) (str)
      (cl:let ((x (cl:parse-integer str :junk-allowed cl:t)))
        (cl:if x
               (Some x)
               None))))
  
  (declare ref-unchecked (String -> UFix -> Char))
  (define (ref-unchecked str idx)
    (lisp Char (str idx)
      (cl:char str idx)))

  (declare ref (String -> UFix -> (Optional Char)))
  (define (ref str idx)
    (if (< idx (fromInt (length str)))
        (Some (ref-unchecked str idx))
        None))

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
    (define <> concat))

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

  (define-instance (Iso (List Char) String))

  (define-instance (Into Integer String)
    (define (into z)
      (lisp String (z)
        (cl:format cl:nil "~D" z))))

  (define-instance (TryInto String Integer)
    (define (tryInto s)
      (lisp (Result String Integer) (s)
        (cl:let ((z (cl:ignore-errors (cl:parse-integer s))))
          (cl:if (cl:null z)
                 (Err (concat "Cannot parse string as integer: " s))
                 (Ok z)))))))

(define-sxhash-hasher String)

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/STRING")
