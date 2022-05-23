(coalton-library/utils:defstdlib-package #:coalton-library/char
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/builtin)
  (:export
   #:char-code
   #:char-code-unchecked
   #:code-char))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(in-package #:coalton-library/char)

(coalton-toplevel
  (declare char-code (Char -> UFix))
  (define (char-code char)
    (lisp UFix (char)
      (cl:char-code char)))

  (declare code-char-unchecked (UFix -> Char))
  (define (code-char-unchecked code)
    (lisp Char (code)
      (cl:code-char code)))

  (declare code-char (UFix -> (Optional Char)))
  (define (code-char code)
    (lisp (Optional Char) (code)
      ;; not sufficient to compare against `char-code-limit', because the char-code space may be sparse.
      (alexandria:if-let (char (cl:code-char code))
        (Some char)
        None)))

  (define-instance (Eq Char)
    (define (== x y)
      (lisp Boolean (x y) (to-boolean (cl:char= x y)))))

  (define-instance (Ord Char)
    (define (<=> x y)
      (if (== x y)
          EQ
          (if (lisp Boolean (x y) (to-boolean (cl:char> x y)))
              GT
              LT)))))

(define-sxhash-hasher Char)

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/CHAR")
