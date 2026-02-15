;;;; loops.lisp

(defpackage #:coalton/experimental/loops
  (:nicknames
   #:coalton-library/experimental/loops)
  (:use
   #:coalton
   #:coalton/classes
   #:coalton/math/arith)
  (:export
   #:repeat
   #:dotimes
   #:everytimes
   #:sometimes
   #:sumtimes
   #:prodtimes
   #:collecttimes
   #:besttimes
   #:argbesttimes
   #:dolist
   #:dolists
   #:dolist-enumerated
   #:dorange)
  (:documentation "A Coalton package of loop macros.

Note: `(return)`, `(break)`, and `(continue)` do not work inside _any_ of these loop macros."))

(in-package #:coalton/experimental/loops)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  (inline)
  (declare %repeat (UFix -> (Unit -> :t) -> Unit))
  (define (%repeat n func)
    "Do `func` `n` times."
    (rec % ((i 0))
      (cond
        ((== i n)
         Unit)
        (True
         (func)
         (% (1+ i))))))

  (inline)
  (declare %dotimes (UFix -> (UFix -> :t) -> Unit))
  (define (%dotimes n func)
    "Apply `func` to every `UFix` in `[0, n)`."
    (rec % ((i 0))
      (cond
        ((== i n)
         Unit)
        (True
         (func i)
         (% (1+ i))))))

  (inline)
  (declare %everytimes (UFix -> (UFix -> Boolean) -> Boolean))
  (define (%everytimes n pred)
    "Is `pred` `True` for all `UFix`s in `[0, n)`? Returns `True` for `n = 0`."
    (rec % ((i 0))
      (if (== i n)
          True
          (if (pred i)
              (% (1+ i))
              False))))

  (inline)
  (declare %sometimes (UFix -> (UFix -> Boolean) -> Boolean))
  (define (%sometimes n pred)
    "Is `pred` `True` for some `UFix` in `[0, n)`? Returns `False` for `n = 0`."
    (rec % ((i 0))
      (if (== i n)
          False
          (if (pred i)
              True
              (% (1+ i))))))

  (inline)
  (declare %sumtimes (Num :t => UFix -> (UFix -> :t) -> :t))
  (define (%sumtimes n func)
    "Sum the evaluations of `func` applied to every `UFix` in `[0, n)`. Returns 0 for `n = 0`."
    (rec % ((i 0) (acc 0))
      (if (== i n)
          acc
          (% (1+ i) (+ acc (func i))))))

  (inline)
  (declare %prodtimes (Num :t => UFix -> (UFix -> :t) -> :t))
  (define (%prodtimes n func)
    "Multiply the evaluations of `func` applied to every `UFix` in `[0, n)`. Returns 1 for `n = 0`."
    (rec % ((i 0) (acc 1))
      (if (== i n)
          acc
          (% (1+ i) (* acc (func i))))))

  (inline)
  (declare %reverse! (List :t -> List :t))
  (define (%reverse! xs)
    "Reverse `xs` by mutation."
    (lisp (List :t) (xs)
      (cl:nreverse xs)))

  (inline)
  (declare %collecttimes (UFix -> (UFix -> :t) -> List :t))
  (define (%collecttimes n func)
    "Collect the applications of `func` to every `UFix` in `[0, n)` as a `List`."
    (rec % ((i 0) (acc Nil))
      (if (== i n)
          (%reverse! acc)
          (% (1+ i) (Cons (func i) acc)))))

  ;; The following functions are not used because it seems, for now
  ;; method which uses %reverse!  seems to be more performant. However,
  ;; ideally, we would like to avoid traversing the list a second time.

  ;; (inline)
  ;; (declare %rplacd! (List :t -> List :t -> List :t))
  ;; (define (%rplacd! xs ys)
  ;;   (lisp (List :t) (xs ys)
  ;;     (cl:locally (cl:declare (cl:optimize (cl:safety 0)))
  ;;       (cl:rplacd xs ys))))

  ;; (inline)
  ;; (declare %cdr (List :t -> List :t))
  ;; (define (%cdr xs)
  ;;   (lisp (List :t) (xs)
  ;;     (cl:locally (cl:declare (cl:optimize (cl:safety 0)))
  ;;       (cl:cdr xs))))

  ;; (inline)
  ;; (declare %collecttimes (UFix -> (UFix -> :t) -> List :t))
  ;; (define (%collecttimes n func)
  ;;   (let ((res (lisp (List :t) () (cl:cons cl:nil cl:nil))))
  ;;     (rec % ((i 0) (last res))
  ;;       (cond
  ;;         ((== i n)
  ;;          (%cdr res))
  ;;         (True
  ;;          (let ((new-last (Cons (func i) Nil)))
  ;;            (%rplacd! last new-last)
  ;;            (% (1+ i) new-last)))))))

  (inline)
  (declare %besttimes (UFix -> (:t -> :t -> Boolean) -> (UFix -> :t) -> :t))
  (define (%besttimes n better? func)
    "Of the applications of `func` to every `UFix` in `[0, n)`, find the one that is `better?` than the rest."
    (cond
      ((zero? n)
       (error "`n` must be strictly greater than zero."))
      (True
       (rec % ((i 1) (best (func 0)))
         (cond
           ((== i n)
            best)
           (True
            (let ((candidate (func i)))
              (if (better? candidate best)
                  (% (1+ i) candidate)
                  (% (1+ i) best)))))))))

  (inline)
  (declare %argbesttimes (UFix -> (:t -> :t -> Boolean) -> (UFix -> :t) -> UFix))
  (define (%argbesttimes n better? func)
    "Find the `UFix` in `[0, n)` whose application of `func` is `better?` than the rest."
    (cond
      ((zero? n)
       (error "`n` must be strictly greater than zero."))
      (True
       (rec % ((i 1) (argbest 0) (best (func 0)))
         (cond
           ((== i n)
            argbest)
           (True
            (let ((candidate (func i)))
              (if (better? candidate best)
                  (% (1+ i) i candidate)
                  (% (1+ i) argbest best)))))))))

  (inline)
  (declare %dolist (List :t1 -> (:t1 -> :t2) -> Unit))
  (define (%dolist lis func)
    "Apply `func` to every element of `lis`."
    (rec % ((xs lis))
      (match xs
        ((Nil)
         Unit)
        ((Cons x xs)
         (func x)
         (% xs)))))

  (inline)
  (declare %dolist-enumerated (List :t1 -> (UFix -> :t1 -> :t2) -> Unit))
  (define (%dolist-enumerated lis func)
    "Apply `func` to every element of `lis` and its index, as `(func index element)`."
    (rec % ((i 0) (xs lis))
      (match xs
        ((Nil)
         Unit)
        ((Cons x xs)
         (func i x)
         (% (1+ i) xs)))))

  (inline)
  (declare %dorange ((Ord :t1) (Num :t1) => :t1 -> :t1 -> :t1 -> (:t1 -> :t2) -> Unit))
  (define (%dorange from to step func)
    "Apply `func` to every number in `[from, to)` in increments/decrements of `step`.

Examples:

```
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
COALTON::UNIT/UNIT
```
"
    (cond
      ((positive? step)
       (rec % ((i from))
         (cond
           ((< i to)
            (func i)
            (% (+ step i)))
           (True
            Unit))))
      ((negative? step)
       (rec % ((i from))
         (cond
           ((> i to)
            (func i)
            (% (+ step i)))
           (True
            Unit))))
      (True
       Unit)))

  (inline)
  (declare %dorange-increasing. ((Ord :t1) (Num :t1) => :t1 -> :t1 -> :t1 -> (:t1 -> :t2) -> Unit))
  (define (%dorange-increasing. from to step func)
    "Unsafe. Apply `func` to every number in `[from, to)` in increments of `step`."
    (rec % ((i from))
      (cond
        ((< i to)
         (func i)
         (% (+ step i)))
        (True
         Unit))))

  (inline)
  (declare %dorange-increasing ((Ord :t1) (Num :t1) => :t1 -> :t1 -> :t1 -> (:t1 -> :t2) -> Unit))
  (define (%dorange-increasing from to step func)
    "Apply `func` to every number in `[from, to)` in increments of `step`."
    (cond
      ((positive? step)
       (%dorange-increasing. from to step func))
      (True
       Unit)))

  (inline)
  (declare %dorange-decreasing. ((Ord :t1) (Num :t1) => :t1 -> :t1 -> :t1 -> (:t1 -> :t2) -> Unit))
  (define (%dorange-decreasing. from to step func)
    "Apply `func` to every number in `(to, from]`, starting with `from`, in decrements of `step`."
    (rec % ((i from))
      (cond
        ((> i to)
         (func i)
         (% (+ step i)))
        (True
         Unit))))

  (inline)
  (declare %dorange-decreasing ((Ord :t1) (Num :t1) => :t1 -> :t1 -> :t1 -> (:t1 -> :t2) -> Unit))
  (define (%dorange-decreasing from to step func)
    "Apply `func` to every number in `(to, from]`, starting with `from`, in decrements of `step`."
    (cond
      ((negative? step)
       (%dorange-decreasing. from to step func))
      (True
       Unit))))

(defmacro repeat ((count) cl:&body body)
  "Perform `body` `count` times."
  `(%repeat ,count (fn () ,@body)))

(defmacro dotimes ((variable count) cl:&body body)
  "Perform `body` with `variable` bound to every `UFix` in [0, `count`) sequentially."
  `(%dotimes ,count (fn (,variable) ,@body)))

(defmacro everytimes ((variable count) cl:&body body)
  "Does `body` evaluate to `True` for `variable` bound to every `UFix` in [0, `count`). Returns `True` if `(zero? count)`."
  `(%everytimes ,count (fn (,variable) ,@body)))

(defmacro sometimes ((variable count) cl:&body body)
  "Does `body` evaluate to `True` for `variable` bound to some `UFix` in [0, `count`). Returns `False` if `(zero? count)`."
  `(%sometimes ,count (fn (,variable) ,@body)))

(defmacro sumtimes ((variable count) cl:&body body)
  "The sum of `body` for `variable` bound to every `UFix` in [0, `count`)."
  `(%sumtimes ,count (fn (,variable) ,@body)))

(defmacro prodtimes ((variable count) cl:&body body)
  "The product of `body` for `variable` bound to every `UFix` in [0, `count`)."
  `(%prodtimes ,count (fn (,variable) ,@body)))

(defmacro collecttimes ((variable count) cl:&body body)
  "Collect the results of evaluating `body` for `variable` bound to every `UFix` in [0, `count`) as a `List`."
  `(%collecttimes ,count (fn (,variable) ,@body)))

(defmacro besttimes ((variable count better?) cl:&body body)
  "The result of evaluating `body` with `variable` bound to a `UFix` in [0, `count`) that is `better?` than the result of evaluating `body` with `variable` bound to the rest of the `UFix`s in [0, `count`)."
  `(%besttimes ,count ,better? (fn (,variable) ,@body)))

(defmacro argbesttimes ((variable count better?) cl:&body body)
  "The `UFix` in [0, `count`) which, when `variable` is bound to it, results in the evaluation of `body` which is better than the same for the rest of the `UFix`s in [0, `count`)."
  `(%argbesttimes ,count ,better? (fn (,variable) ,@body)))

(defmacro dolist ((variable lis) cl:&body body)
  "Perform `body` with `variable` bound to every element of `lis`."
  `(%dolist ,lis (fn (,variable) ,@body)))

(defmacro dolists (variables-and-lists cl:&body body)
  "Perform `body` with the variables bound to the elements of the lists. See the example below.

Example:

```
> (coalton (dolists ((x (make-list 1 2 3))
                     (y (make-list 10 20 30))
                     (z (make-list 100 200 300 400)))
             (print (+ x (+ y z))))
111
222
333
COALTON::UNIT/UNIT
```
"
  (cl:declare (cl:type cl:list variables-and-lists))
  (cl:let ((% (cl:gensym "%"))
           (func (cl:gensym "FUNC"))
           (variables (cl:mapcar #'cl:first variables-and-lists))
           (lists (cl:mapcar #'cl:second variables-and-lists))
           (xss (cl:loop
                   :repeat (cl:length variables-and-lists)
                   :collect (cl:gensym "XS"))))
    (cl:labels ((populate-body (remaining-variables remaining-xss)
                  (cl:cond
                    ((cl:null remaining-variables)
                     `(progn (,func ,@variables)
                             (,% ,@xss)))
                    (cl:t
                     (cl:let ((x (cl:first remaining-variables))
                              (xs (cl:first remaining-xss)))
                       `(match ,xs
                          ((Nil)
                           Unit)
                          ((Cons ,x ,xs)
                           ,(populate-body (cl:rest remaining-variables)
                                           (cl:rest remaining-xss)))))))))
      `(let ((,func (fn (,@variables) ,@body)))
         (rec ,% (,@(cl:mapcar (cl:lambda (xs lis) (cl:list xs lis)) xss lists))
           ,(populate-body variables xss))))))

(defmacro dolist-enumerated ((index-variable element-variable lis) cl:&body body)
  "Perform `body` with `element-variable` bound to the elements of `lis` and `index-variable` bound to their indices."
  `(%dolist-enumerated ,lis (fn (,index-variable ,element-variable) ,@body)))

(defmacro dorange ((variable start-or-stop cl:&optional stop step) cl:&body body)
  "Perform `body` with `variable` bound to elements of a discrete range.

If only `start-or-stop` is supplied, then the range is from 0 (inclusive) to `start-or-stop` (exclusive) by increments or decrements of 1.

```
> (coalton (dorange (x 3) (print x)))
0
1
2
COALTON::UNIT/UNIT
> (coalton (dorange (x -3) (print x)))
0
-1
-2
COALTON::UNIT/UNIT
```

If only `start-or-stop` and `stop` are supplied, then the range is from `start-or-stop` (inclusive) to `stop` (exclusive) by increments or decrements of 1.

```
> (coalton (dorange (x -2 2) (print x)))
-2 
-1
0
1
COALTON::UNIT/UNIT
> (coalton (dorange (x 0.5 -2) (print x)))
0.5
-0.5
-1.5
COALTON::UNIT/UNIT
```

Otherwise, the range is from `start-or-stop` (inclusive) to `stop` (exclusive) by `step`. `step` must be the correct sign, or `dorange` does nothing.

```
> (coalton (dorange -2 2 2) (print x))
-2 
0
COALTON::UNIT/UNIT
> (coalton (dorange -2 2 -1) (print x))
COALTON::UNIT/UNIT
```
"
  (cl:let ((func `(fn (,variable) ,@body)))
    (cl:cond
      ((cl:null stop)
       (cl:cond
         ((cl:typep start-or-stop 'cl:fixnum)
          (cl:if (cl:plusp start-or-stop)
                 `(%dorange-increasing. 0 ,start-or-stop 1 ,func)
                 `(%dorange-decreasing. 0 ,start-or-stop -1 ,func)))
         (cl:t
          `(%dorange 0 ,start-or-stop (sign ,start-or-stop) ,func))))
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
(sb-ext:lock-package "COALTON/EXPERIMENTAL/LOOPS")
