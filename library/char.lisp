(coalton-library/utils:defstdlib-package #:coalton-library/char
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/builtin
   #:coalton-library/functions)
  (:import-from
   #:coalton-library/hash
   #:define-sxhash-hasher)
  (:local-nicknames
   (#:iter #:coalton-library/iterator))
  (:export
   #:char-code
   #:char-code-unchecked
   #:code-char
   #:alpha?
   #:ascii-alpha?
   #:digit?
   #:ascii-digit?
   #:ascii-alphanumeric?
   #:uppercase?
   #:ascii-uppercase?
   #:lowercase?
   #:ascii-lowercase?
   #:upcase
   #:downcase
   #:range))

(in-package #:coalton-library/char)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (declare char-code (Char -> UFix))
  (define (char-code char)
    "Convert a character to its ASCII representation."
    (lisp UFix (char)
      (cl:char-code char)))

  (declare code-char-unchecked (UFix -> Char))
  (define (code-char-unchecked code)
    "Convert a number to its ASCII character. This function is partial."
    (lisp Char (code)
      (cl:code-char code)))

  (declare code-char (UFix -> (Optional Char)))
  (define (code-char code)
    "Convert a number to its ASCII character, returning None on failure."
    (lisp (Optional Char) (code)
      ;; It is not sufficient to compare against `cl:char-code-limit`,
      ;; because the char-code space may be sparse.
      (alexandria:if-let (char (cl:code-char code))
        (Some char)
        None)))

  (define-instance (Eq Char)
    (define (== x y)
      (lisp Boolean (x y)
        (to-boolean (cl:char= x y)))))

  (define-instance (Ord Char)
    (define (<=> x y)
      (if (== x y)
          EQ
          (if (lisp Boolean (x y) (to-boolean (cl:char> x y)))
              GT
              LT))))

  (declare alpha? (Char -> Boolean))
  (define (alpha? c)
    "Is `c` an alphabetic character?"
    (lisp Boolean (c)
      (cl:alpha-char-p c)))

  (declare ascii-alpha? (Char -> Boolean))
  (define (ascii-alpha? c)
    "Is `c` an ASCII alphabetic character?"
    (lisp Boolean (c)
      (cl:or
       (cl:<= 65 (cl:char-code c) 90)
       (cl:<= 97 (cl:char-code c) 122))))

  (declare digit? (Char -> Boolean))
  (define (digit? c)
    "Is `c` a digit character?"
    (lisp Boolean (c)
      (to-boolean (cl:digit-char-p c))))

  (declare ascii-digit? (Char -> Boolean))
  (define (ascii-digit? c)
    "Is `c` an ASCII digit character?"
    (lisp Boolean (c)
      (cl:<= 48 (cl:char-code c) 57)))

  (declare ascii-alphanumeric? (Char -> Boolean))
  (define (ascii-alphanumeric? c)
    "Is `c` an ASCII alphanumeric character?"
    (or (ascii-alpha? c)
        (ascii-digit? c)))

  (declare uppercase? (Char -> Boolean))
  (define (uppercase? c)
    "Is `c` an uppercase character?"
    (lisp Boolean (c)
      (cl:upper-case-p c)))

  (declare ascii-uppercase? (Char -> Boolean))
  (define (ascii-uppercase? c)
    "Is `c` an ASCII uppercase character?"
    (lisp Boolean (c)
      (cl:or
       (cl:<= 65 (cl:char-code c) 90))))

  (declare lowercase? (Char -> Boolean))
  (define (lowercase? c)
    "Is `c` a lowercase character?"
    (lisp Boolean (c)
      (cl:lower-case-p c)))

  (declare ascii-lowercase? (Char -> Boolean))
  (define (ascii-lowercase? c)
    "Is `c` an ASCII lowercase character?"
    (lisp Boolean (c)
      (cl:or
       (cl:<= 97 (cl:char-code c) 122))))

  (declare upcase (Char -> Char))
  (define (upcase c)
    "Returns the upcased version of `c`, returning `c` when there is none."
    (lisp Char (c)
      (cl:char-upcase c)))

  (declare downcase (Char -> Char))
  (define (downcase c)
    "Returns the downcased version of `c`, returning `c` when there is none."
    (lisp Char (c)
      (cl:char-downcase c)))
  
  (declare range (Char -> Char -> iter:Iterator Char))
  (define (range start end)
    "An inclusive range of characters from `start` to `end` by `char-code`."
    (iter:filter-map!
     code-char
     (iter:range-increasing
      1
      (char-code start)
      (+ 1 (char-code end)))))

  (define-sxhash-hasher Char))


#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/CHAR")
