(coalton-library/utils:defstdlib-package #:coalton-library/string
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:import-from
   #:coalton-library/hash
   #:define-sxhash-hasher)
  (:import-from
   #:coalton-library/vector
   #:Vector)
  (:local-nicknames
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator))
  (:export
   #:concat
   #:reverse
   #:length
   #:substring
   #:split
   #:strip-prefix
   #:strip-suffix
   #:parse-int
   #:ref
   #:ref-unchecked
   #:substring-index
   #:substring?
   #:chars))


(in-package #:coalton-library/string)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;;
;;; String
;;;


(cl:defmacro define-instance-into-integral-string (type)
  `(coalton-toplevel
     (define-instance (Into ,type String)
       (define (into z)
         (lisp String (z)
           (cl:format cl:nil "~D" z))))))

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

  (declare length (String -> UFix))
  (define (length str)
    "The length of a string STR."
    (lisp UFix (str)
      (cl:length str)))

  (declare substring (String -> UFix -> UFix -> String))
  (define (substring str start end)
    "Compute a substring of a string bounded by given indices."
    (let ((real-start (max 0 (min start end)))
          (real-end (min (length str) (max start end))))
      (lisp String (real-start real-end str)
        (cl:subseq str real-start real-end))))

  (declare split (UFix -> String -> (Tuple String String)))
  (define (split n str)
    "Splits a string into a head and tail at the nth index."
    (Tuple (substring str 0 n)
           (substring str n (length str))))
  
  (declare strip-prefix (String -> String -> (Optional String)))
  (define (strip-prefix prefix str)
    "Returns a string without a give prefix, or `None` if the string
does not have that suffix."
    (let prefix-len = (length prefix))
    (let substr = (substring str 0 prefix-len))
    (if (== substr prefix)
        (Some (substring str prefix-len (length str)))
        None))

  (declare strip-suffix (String -> String -> (Optional String)))
  (define (strip-suffix suffix str)
    "Returns a string without a give suffix, or `None` if the string
does not have that suffix."
    (let suffix-len = (length suffix))
    (let str-len = (length str))
    (let substr = (substring str (- str-len suffix-len) str-len))
    (if (== substr suffix)
        (Some (substring str 0 (- str-len suffix-len)))
        None))

  (declare parse-int (String -> (Optional Integer)))
  (define (parse-int str)
    "Parse the integer in string `str`."
    (lisp (Optional Integer) (str)
      (cl:let ((x (cl:parse-integer str :junk-allowed cl:t)))
        (cl:if x
               (Some x)
               None))))
  
  (declare ref-unchecked (String -> UFix -> Char))
  (define (ref-unchecked str idx)
    "Return the `idx`th character of `str`. This function is partial."
    (lisp Char (str idx)
      (cl:char str idx)))

  (declare ref (String -> UFix -> (Optional Char)))
  (define (ref str idx)
    "Return the `idx`th character of `str`."
    (if (< idx (length str))
        (Some (ref-unchecked str idx))
        None))

  (declare substring-index (String -> String -> (Optional UFix)))
  (define (substring-index small big)
    "If the first argument appears as a substring within the second argument, return the starting index into the second argument."
    (lisp (Optional UFix) (small big)
      (alexandria:if-let (idx (cl:search small big))
        (Some idx)
        None)))

  (declare substring? (String -> String -> Boolean))
  (define (substring? small big)
    "Return true if the first argument appears as a substring within the second argument."
    ;; not a call to `optional:some?' because this file is loaded before optional.lisp
    (match (substring-index small big)
      ((None) False)
      ((Some _) True)))

  (declare chars (String -> iter:Iterator Char))
  (define (chars str)
    "Returns an iterator over the characters in `str`."
    (iter:into-iter str))

  ;;
  ;; Instances
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

  (define-instance (iter:IntoIterator String Char)
    (define (iter:into-iter str)
      (map (ref-unchecked str)
           (iter:up-to (length str)))))

  (define-instance (iter:FromIterator String Char)
    (define (iter:collect! iter)
      (let vec = (the (Vector Char) (iter:collect! iter)))
      (lisp String (vec)
        (cl:coerce vec 'cl:string))))

  ;;
  ;; Conversions
  ;;

  (define-instance (Into String (List Char))
    (define (into str)
      (lisp (List Char) (str)
        (cl:coerce str 'cl:list))))

  (define-instance (Into Char String)
    (define (into chr)
      (lisp String (chr)
        (cl:string chr))))

  (define-instance (Into (List Char) String)
    (define (into lst)
      (lisp String (lst)
        (cl:coerce lst 'cl:string))))

  (define-instance (Iso (List Char) String)))

(define-instance-into-integral-string Integer)
(define-instance-into-integral-string IFix)
(define-instance-into-integral-string UFix)
(define-instance-into-integral-string I8)
(define-instance-into-integral-string U8)
(define-instance-into-integral-string I16)
(define-instance-into-integral-string U16)
(define-instance-into-integral-string I32)
(define-instance-into-integral-string U32)
(define-instance-into-integral-string I64)
(define-instance-into-integral-string U64)

(coalton-toplevel
  (define-instance (Into Fraction String)
    (define (into frc)
      (lisp String (frc)
        (cl:format cl:nil "~A" frc))))

  (define-instance (Into Single-Float String)
    (define (into z)
      (lisp String (z)
        (cl:prin1-to-string z))))

  (define-instance (Into Double-Float String)
    (define (into z)
      (lisp String (z)
        (cl:prin1-to-string z))))

  (define-instance (TryInto String Integer String)
    (define (tryInto s)
      (lisp (Result String Integer) (s)
        (cl:let ((z (cl:ignore-errors (cl:parse-integer s))))
          (cl:if (cl:null z)
                 (Err (concat "Cannot parse string as integer: " s))
                 (Ok z))))))

  (define-instance (Default String)
    (define (default) ""))

  (define-sxhash-hasher String))



#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/STRING")
