;;;; struct-tests.lisp

(in-package #:coalton-tests)

(deftest test-accessor-equations-share-substitutions ()
  (dolist (branches '("(.num v) (.text v)" "(.text v) (.num v)"))
    (signals tc:tc-error
      (check-coalton-types
       (format nil
               "(define-struct Mixed (num Integer) (text String))
                (define (choose flag)
                  (let ((v (Mixed 42 ~S)))
                    (if flag ~A)))"
               "text" branches))))
  (check-coalton-types
   "(define-struct (Boxed :a) (item :a))
    (define (unbox-both)
      (Tuple (.item (Boxed (the Integer 42)))
             (.item (Boxed \"text\"))))
    (define (unbox-nested)
      (.item (.item (Boxed (Boxed \"text\")))))"
   '("unbox-both" . "(Void -> Tuple Integer String)")
   '("unbox-nested" . "(Void -> String)")))

(deftest test-struct-definition ()
  (check-coalton-types
   "(define-struct Point
      (x \"The x value.\" Integer)
      (y Integer))")

  (check-coalton-types
   "(define-struct (Point :a)
      (x :a)
      (y \"The y value.\" :a))"))

(deftest test-struct-accessors ()
  (check-coalton-types
   "(define-struct Point
      (x Integer)
      (y Integer))

    (define p (Point 1 2))
    (define x (.x p))
    (define y (.y p))"

   '("p" . "Point")
   '("x" . "Integer")
   '("y" . "Integer"))

  (check-coalton-types
   "(define-struct (Wrapper :a)
      (inner :a))

    (define x (.inner (Wrapper #\\X)))"

   '("x" . "Char"))

  (check-coalton-types
   "(define-struct (Wrapper :a)
      (inner :a))

    (define x (.inner (.inner (.inner (Wrapper (Wrapper (Wrapper #\\X)))))))"

   '("x" . "Char")))

(deftest test-struct-accessors-as-functions ()
  (check-coalton-types
   "(define-struct Point
      (x Integer)
      (y Integer))

    (define xs (map .x (make-list (Point 1 2) (Point 3 4) (Point 5 6))))"

   '("xs" . "(List Integer)"))

  (check-coalton-types
   "(define-struct Point
      (x Integer)
      (y Integer))

    (declare f (Point -> Integer))
    (define f .x)"))

;; See gh #959
(deftest test-accessor-on-argument-let-binding ()
  (check-coalton-types
   "(define-struct (Wrapper :a)
     (inner :a))

    (declare f (Wrapper :a -> :a))
    (define (f x)
      (let ((y (.inner x)))
       y))"))
