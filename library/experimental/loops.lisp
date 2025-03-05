;;;; loop.lisp

(defpackage #:coalton-library/experimental/loops
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/math/arith)
  (:export
   #:named-let
   #:repeat
   #:dotimes
   #:everytimes
   #:sometimes
   #:sumtimes
   #:prodtimes
   #:collectimes
   #:dolist
   #:dolists
   #:dolist-enumerated
   #:dorange)
  (:documentation "A Coalton package of loop macros.

Note: `(return)`, `(break)`, and `(continue)` do not work inside _any_ of these loop macros."))

(in-package #:coalton-library/experimental/loops)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(cl:defmacro named-let (name bindings cl:&body body)
  (cl:let ((variables (cl:mapcar #'cl:first bindings)))
    `(let (,@bindings
           (,name (fn (,@variables) ,@body)))
       (,name ,@variables))))

(coalton-toplevel

  (declare %repeat (UFix -> (Unit -> :t) -> Unit))
  (define (%repeat n func)
    "Do `func` `n` times."
    (named-let rec ((i 0))
      (unless (== i n)
        (func)
        (rec (1+ i)))))

  (declare %dotimes (UFix -> (UFix -> :t) -> Unit))
  (define (%dotimes n func)
    "Apply `func` to every `UFix` in `[0, n)`."
    (named-let rec ((i 0))
      (unless (== i n)
        (func i)
        (rec (1+ i)))))

  (declare %everytimes (UFix -> (UFix -> Boolean) -> Boolean))
  (define (%everytimes n pred)
    "Is `pred` `True` for all `UFix`s in `[0, n)`? Returns `True` for `n = 0`."
    (named-let rec ((i 0))
      (if (== i n)
          True
          (if (pred i)
              (rec (1+ i))
              False))))

  (declare %sometimes (UFix -> (UFix -> Boolean) -> Boolean))
  (define (%sometimes n pred)
    "Is `pred` `True` for some `UFix` in `[0, n)`? Returns `False` for `n = 0`."
    (named-let rec ((i 0))
      (if (== i n)
          False
          (if (pred i)
              True
              (rec (1+ i))))))

  (declare %sumtimes (Num :t => UFix -> (UFix -> :t) -> :t))
  (define (%sumtimes n func)
    "Sum the evaluations of `func` applied to every `UFix` in `[0, n)`. Returns 0 for `n = 0`."
    (named-let rec ((i 0) (acc 0))
      (if (== i n)
          acc
          (rec (1+ i) (+ acc (func i))))))

  (declare %prodtimes (Num :t => UFix -> (UFix -> :t) -> :t))
  (define (%prodtimes n func)
    "Multiply the evaluations of `func` applied to every `UFix` in `[0, n)`. Returns 1 for `n = 0`."
    (named-let rec ((i 0) (acc 1))
      (if (== i n)
          acc
          (rec (1+ i) (* acc (func i))))))

  (inline)
  (declare %set-cdr (List :t -> List :t -> List :t))
  (define (%set-cdr xs ys)
    "Set the cdr of `xs` to `ys` and return `ys`.
Unsafe: `xs` must be a `Cons` cell."
    (lisp (List :t) (xs ys)
      (cl:setf (cl:cdr xs) ys)))

  (declare %collecttimes (UFix -> (UFix -> :t) -> List :t))
  (define (%collecttimes n func)
    "Collect the applications of `func` to every `UFix` in `[0, n)` as a `List`."
    (cond
      ((zero? n)
       Nil)
      (True
       (let ((res (Cons (func 0) Nil)))
         (named-let rec ((i 1) (last res))
           (unless (== i n)
             (rec (1+ i) (%set-cdr last (Cons (func i) Nil)))))
         res))))

  (declare %dolist (List :t1 -> (:t1 -> :t2) -> Unit))
  (define (%dolist lis func)
    "Apply `func` to every element of `lis`."
    (named-let rec ((xs lis))
      (match xs
        ((Cons x xs)
         (func x)
         (rec xs))
        ((Nil)
         Unit))))

  (declare %dolist-enumerated (List :t1 -> (UFix -> :t1 -> :t2) -> Unit))
  (define (%dolist-enumerated lis func)
    "Apply `func` to every element of `lis` and its index, as `(func index element)`."
    (named-let rec ((i 0) (xs lis))
      (match xs
        ((Cons x xs)
         (func i x)
         (rec (1+ i) xs))
        ((Nil)
         Unit))))

  (declare %dorange ((Ord :t1) (Num :t1) => :t1 -> :t1 -> :t1 -> (:t1 -> :t2) -> Unit))
  (define (%dorange from to step func)
    "Apply `func` to every number in `[from, to)` in increments/decrements of `step`.

Examples:
> (coalton (%dorange 0 3 1 (fn (i) (print i))))
0
1
2
COALTON::UNIT/UNIT
> (coalton (%dorange 0 3 -1 (fn (i) (print i))))
COALTON::UNIT/UNIT
> (coalton (%dorange 0 -5 -2 (fn (i) (print i))))
0
-2
-4
COALTON::UNIT/UNIT"
    (cond
      ((positive? step)
       (named-let rec ((i from))
         (when (< i to)
           (func i)
           (rec (+ step i)))))
      ((negative? step)
       (named-let rec ((i from))
         (when (> i to)
           (func i)
           (rec (+ step i)))))
      (True
       Unit)))

  (declare %dorange-increasing. ((Ord :t1) (Num :t1) => :t1 -> :t1 -> :t1 -> (:t1 -> :t2) -> Unit))
  (define (%dorange-increasing. from to step func)
    "Unsafe. Apply `func` to every number in `[from, to)` in increments of `step`."
    (named-let rec ((i from))
      (when (< i to)
        (func i)
        (rec (+ step i)))))

  (declare %dorange-increasing ((Ord :t1) (Num :t1) => :t1 -> :t1 -> :t1 -> (:t1 -> :t2) -> Unit))
  (define (%dorange-increasing from to step func)
    "Apply `func` to every number in `[from, to)` in increments of `step`."
    (when (positive? step) (%dorange-increasing. from to step func)))

  (declare %dorange-decreasing. ((Ord :t1) (Num :t1) => :t1 -> :t1 -> :t1 -> (:t1 -> :t2) -> Unit))
  (define (%dorange-decreasing. from to step func)
    "Apply `func` to every number in `(to, from]`, starting with `from`, in decrements of `step`."
    (named-let rec ((i from))
      (when (> i to)
        (func i)
        (rec (+ step i)))))

  (declare %dorange-decreasing ((Ord :t1) (Num :t1) => :t1 -> :t1 -> :t1 -> (:t1 -> :t2) -> Unit))
  (define (%dorange-decreasing from to step func)
    "Apply `func` to every number in `(to, from]`, starting with `from`, in decrements of `step`."
    (when (negative? step) (%dorange-decreasing. from to step func))))

(cl:defmacro repeat ((count) cl:&body body)
  `(%repeat ,count (fn () ,@body)))

(cl:defmacro dotimes ((variable count) cl:&body body)
  `(%dotimes ,count (fn (,variable) ,@body)))

(cl:defmacro everytimes ((variable count) cl:&body body)
  `(%everytimes ,count (fn (,variable) ,@body)))

(cl:defmacro sometimes ((variable count) cl:&body body)
  `(%sometimes ,count (fn (,variable) ,@body)))

(cl:defmacro sumtimes ((variable count) cl:&body body)
  `(%sumtimes ,count (fn (,variable) ,@body)))

(cl:defmacro prodtimes ((variable count) cl:&body body)
  `(%prodtimes ,count (fn (,variable) ,@body)))

(cl:defmacro collecttimes ((variable count) cl:&body body)
  `(%collecttimes ,count (fn (,variable) ,@body)))

(cl:defmacro dolist ((variable lis) cl:&body body)
  `(%dolist ,lis (fn (,variable) ,@body)))

(cl:defmacro dolists (variables-and-lists cl:&body body)
  "Bind a set of variables to the respective elements of a set of lists with those bindings.

Example:

> (coalton (dolists ((x (make-list 1 2 3))
                     (y (make-list 10 20 30))
                     (z (make-list 100 200 300 400)))
             (print (+ x (+ y z))))
111
222
333
COALTON::UNIT/UNIT"
  (cl:declare (cl:type cl:list variables-and-lists))
  (cl:let ((rec (cl:gensym "REC"))
           (variables (cl:mapcar #'cl:first variables-and-lists))
           (lists (cl:mapcar #'cl:second variables-and-lists))
           (xss (cl:loop
                   :repeat (cl:length variables-and-lists)
                   :collect (cl:gensym "XS"))))
    (cl:labels ((populate-body (remaining-variables remaining-xss)
                  (cl:cond
                    ((cl:null remaining-variables)
                     `(progn ,@body (,rec ,@xss)))
                    (cl:t
                     (cl:let ((x (cl:first remaining-variables))
                              (xs (cl:first remaining-xss)))
                       `(match ,xs
                          ((Nil)
                           Unit)
                          ((Cons ,x ,xs)
                           ,(populate-body (cl:rest remaining-variables)
                                           (cl:rest remaining-xss)))))))))
      `(named-let ,rec (,@(cl:mapcar (cl:lambda (xs lis) (cl:list xs lis)) xss lists))
         ,(populate-body variables xss)))))

(cl:defmacro dolist-enumerated ((index-variable element-variable lis) cl:&body body)
  `(%dolist-enumerated ,lis (fn (,index-variable ,element-variable) ,@body)))

(cl:defmacro dorange ((variable start-or-stop cl:&optional stop step) cl:&body body)
  (cl:let ((func `(fn (,variable) ,@body)))
    (cl:cond
      ((cl:null stop)
       `(%dotimes ,start-or-stop ,func))
      ((cl:null step)
       (cl:cond
         ((cl:and (cl:typep start-or-stop 'cl:fixnum)
                  (cl:typep stop 'cl:fixnum))
          (cl:if (cl:< start-or-stop stop)
                 `(%dorange-increasing. ,start-or-stop ,stop 1 ,func)
                 `(%dorange-decreasing. ,start-or-stop ,stop -1 ,func)))
         (cl:t
          `(%dorange ,start-or-stop ,stop (sign (- ,stop ,start-or-stop)) ,func))))
      (cl:t
       (cl:cond
         ((cl:and (cl:typep start-or-stop 'cl:fixnum)
                  (cl:typep stop 'cl:fixnum)
                  (cl:typep step 'cl:fixnum))
          (cl:cond
            ((cl:and (cl:plusp step) (cl:< start-or-stop stop))
             `(%dorange-increasing. ,start-or-stop ,stop ,step ,func))
            ((cl:and (cl:minusp step) (cl:> start-or-stop stop))
             `(%dorange-decreasing. ,start-or-stop ,stop ,step ,func))
            (cl:t
             'Unit)))
         ((cl:and (cl:typep start-or-stop 'cl:fixnum)
                  (cl:typep stop 'cl:fixnum))
          (cl:if (cl:< start-or-stop stop)
                 `(%dorange-increasing ,start-or-stop ,stop ,step ,func)
                 `(%dorange-decreasing ,start-or-stop ,stop ,step ,func)))
         ((cl:typep step 'cl:fixnum)
          (cl:if (cl:plusp step)
                 `(%dorange-increasing. ,start-or-stop ,stop ,step ,func)
                 `(%dorange-decreasing. ,start-or-stop ,stop ,step ,func)))
         (cl:t
          `(%dorange ,start-or-stop ,stop ,step ,func)))))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/EXPERIMENTAL/LOOPS")

