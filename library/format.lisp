(defpackage #:coalton-library/format
  (:use
   #:coalton
   #:coalton-library/classes)
  (:local-nicknames
   (#:str #:coalton-library/string)
   (#:list #:coalton-library/list)
   (#:iter #:coalton-library/iterator)
   (#:math #:coalton-library/math))
  (:export
   #:format
   #:\n
   #:as-string
   #:format-with-delimiter
   #:as-string-with-delimiter
   #:quoted
   #:ticked
   #:Radix
   #:radix-directive
   #:radix
   #:radix-padded
   #:bin
   #:bin-padded
   #:oct
   #:oct-padded
   #:dec
   #:dec-padded
   #:hex
   #:hex-padded
   #:eng
   #:rom
   #:fixed-point
   #:fixed-point-digits
   #:exponential
   #:exponential-digits
   #:currency))

(in-package #:coalton-library/format)

(cl:defmacro format (cl:&rest forms)
  "Format takes strings as arguments and combines them into one string."
  (cl:let* ((forms (cl:append '(make-list) forms)))
    `(mconcat ,forms)))
;;;
;;; Special characters
;;;

(coalton-toplevel

  (define \n
    "A new line."
    "
"))

;;;
;;; Formatting objects and collections of objects
;;;

(coalton-toplevel

  (declare as-string ((Into :a String) => :a -> String))
  (define (as-string x)
    "Returns the object as a string."
    (into x))

  (declare format-with-delimiter ((Functor :c)
                                  (iter:IntoIterator (:c :a) :a) =>
                                  (:a -> String) -> String -> (:c :a) -> String))
  (define (format-with-delimiter f delimiter collection)
    "Formats `collection` according to a string function `f` separated by a string `delimiter`."
    ;; This is a little bit gross, but necessary for checking iterator length without losing the iter.
    (let lst = (iter:collect! (iter:into-iter collection)))
    (let delim-length = (list:length lst))
    (iter:fold! str:concat "" (iter:interleave!
                               (map f (iter:into-iter lst))
                               (iter:repeat-for delimiter
                                                delim-length))))

  (declare as-string-with-delimiter ((Functor :c)
                                     (iter:IntoIterator (:c :a) :a)
                                     (Into :a String) =>
                                     String -> (:c :a) -> String))
  (define (as-string-with-delimiter delimiter collection)
    "Formats `collection` as a string with elements separated by a string `delimiter`."
    (format-with-delimiter as-string delimiter collection))

  (declare quoted ((Into :a String) => :a -> String))
  (define (quoted x)
    "Formats the object as a string quoted within a string."
    (let str = (as String x))
    (lisp String (str)
      (cl:format cl:nil "~S" str)))

  (declare ticked ((Into :a String) => :a -> String))
  (define (ticked x)
    "Formats the object as a string backticked within a string."
    (let str = (as String x))
    (lisp String (str)
      (cl:format cl:nil "`~A`" str))))

;;;
;;; Formatting Integer types
;;;

(coalton-toplevel

  (define-type FormatFlag
    "Flag for string formatting."
    Binary      "Binary (base 2)"
    Octal       "Octal (base 8)"
    Decimal     "Decimal (base 10"
    Hexadecimal "Hexadecimal (base 16)"
    English     "English number word"
    Roman       "Roman numeral")

  (declare flag-directive (FormatFlag -> String))
  (define (flag-directive flag)
    "Returns the format directive for the given flag."
    (match flag
      ((Binary)      "B")
      ((Octal)       "O")
      ((Decimal)     "D")
      ((Hexadecimal) "X")
      ((English)     "R")
      ((Roman)       "@R")))

  (declare flag ((math:Integral :a) => FormatFlag -> :a -> String))
  (define (flag flag x)
    "Prints an integral type according to the given flag."
    (let control-string = (str:concat "~" (flag-directive flag)))
    (lisp String (control-string x)
      (cl:format cl:nil control-string x)))

  (declare flag-padded ((math:Integral :a) => FormatFlag -> UFix -> :a -> String))
  (define (flag-padded flag width x)
    "Prints an integral type according to the given flag, preserving leading zeroes."
    (let directive = (flag-directive flag))
    (let control-string = (lisp String (width directive)
                            (cl:format cl:nil "~a~D,'0~a"
                                       "~" width directive)))
    (lisp String (control-string x)
      (cl:format cl:nil control-string x)))

;;;
;;; Toplevel flag formatting functions
;;;

  (declare bin ((math:Integral :a) => :a -> String))
  (define (bin x)
    "Prints an integral type in radix 2 (binary)."
    (flag Binary x))

  (declare bin-padded ((math:Integral :a) => UFix -> :a -> String))
  (define (bin-padded width x)
    "Prints an integral type in radix 2 (binary), with leading zeroes."
    (flag-padded Binary width x))

  (declare oct ((math:Integral :a) => :a -> String))
  (define (oct x)
    "Prints an integral type in radix 8 (octal)."
    (flag Octal x))

  (declare oct-padded ((math:Integral :a) => UFix -> :a -> String))
  (define (oct-padded width x)
    "Prints an integral type in radix 8 (octal), with leading zeroes."
    (flag-padded Octal width x))

  (declare dec ((math:Integral :a) => :a -> String))
  (define (dec x)
    "Prints an integral type in radix 10 (decimal)."
    (flag Decimal x))

  (declare dec-padded ((math:Integral :a) => UFix -> :a -> String))
  (define (dec-padded width x)
    "Prints an integral type in radix 10 (decimal), with leading zeroes."
    (flag-padded Decimal width x))

  (declare hex ((math:Integral :a) => :a -> String))
  (define (hex x)
    "Prints an integral type in radix 16 (hexadecimal)."
    (flag Hexadecimal x))

  (declare hex-padded ((math:Integral :a) => UFix -> :a -> String))
  (define (hex-padded width x)
    "Prints an integral type in radix 16 (hexadecimal), with leading zeroes."
    (flag-padded Hexadecimal width x))

  (declare eng ((math:Integral :a) => :a -> String))
  (define (eng x)
    "Prints an integral type as an english word."
    (flag English x))

  (declare rom ((math:Integral :a) => :a -> String))
  (define (rom x)
    "Prints an integral type as a roman numeral."
    (flag Roman x)))

;;;
;;; Formatting floating point numbers
;;;

(coalton-toplevel

  (declare fixed-point ((math:Real :a) => :a -> String))
  (define (fixed-point x)
    "Prints a floating point number in fixed-point notation."
    (lisp String (x)
      (cl:format cl:nil "~F" x)))

  (declare fixed-point-digits ((math:Real :a) => Ufix -> :a -> String))
  (define (fixed-point-digits digits x)
    "Prints a floating point number in fixed-point notation with the desired number of digits after the point."
    (let control-string = (lisp String (digits)
                            (cl:format cl:nil "~~0,~D,F" digits)))
    (lisp String (control-string x)
      (cl:format cl:nil control-string x)))

  (declare exponential ((math:Real :a) => :a -> String))
  (define (exponential x)
    "Prints a floating point number in exponential notation."
    (lisp String (x)
      (cl:format cl:nil "~E" x)))

  (declare exponential-digits ((math:Real :a) => Ufix -> :a -> String))
  (define (exponential-digits digits x)
    "Prints a floating point number in exponential notation with the desired number of digits after the point."
    (let control-string = (lisp String (digits)
                            (cl:format cl:nil "~~0,~D,E" digits)))
    (lisp String (control-string x)
      (cl:format cl:nil control-string x)))

  (declare currency ((math:Real :a) => :a -> String))
  (define (currency x)
    "Prints a floating point number as currency."
    (lisp String (x)
      (cl:format cl:nil "~$" x))))
