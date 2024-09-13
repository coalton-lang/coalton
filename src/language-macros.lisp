(in-package #:coalton)

;;;; Macros used to implement the Coalton language

(cl:defmacro as (type cl:&optional (expr cl:nil expr-supplied-p))
  "A syntactic convenience for type casting.

    (as <type> <expr>)

is equivalent to

    (the <type> (into <expr>))

and

    (as <type>)

is equivalent to

    (fn (expr) (the <type> (into expr))).

Note that this may copy the object or allocate memory."

  (cl:let ((into (cl:ignore-errors (cl:find-symbol "INTO" "COALTON-LIBRARY/CLASSES"))))
    (cl:assert into () "`as` macro does not have access to `into` yet.")
    (cl:if expr-supplied-p
           `(the ,type (,into ,expr))
           (alexandria:with-gensyms (lexpr)
             `(fn (,lexpr)
                (the ,type (,into ,lexpr)))))))

(cl:defmacro try-as (type cl:&optional (expr cl:nil expr-supplied-p))
  "A syntactic convenience for type casting.

    (try-as <type> <expr>)

is equivalent to

    (the (Result :_ <type>) (tryInto <expr>))

and

    (try-as <type>)

is equivalent to

    (fn (expr) (the (Result :_ <type>) (tryInto expr))).

Note that this may copy the object or allocate memory."

  (cl:let ((try-into (cl:ignore-errors (cl:find-symbol "TRYINTO" "COALTON-LIBRARY/CLASSES")))
           (Result (cl:ignore-errors (cl:find-symbol "RESULT" "COALTON-LIBRARY/CLASSES"))))
    (cl:assert try-into () "`try-as` macro does not have access to `try-into` yet.")
    (cl:assert Result () "`try-as` macro does not have access to `Result` yet.")
    (cl:if expr-supplied-p
           `(the (,Result :_ ,type) (,try-into ,expr))
           (alexandria:with-gensyms (lexpr)
             `(fn (,lexpr)
                (the (,Result :_ ,type) (,try-into ,lexpr)))))))

(cl:defmacro unwrap-as (type cl:&optional (expr cl:nil expr-supplied-p))
  "A syntactic convenience for type casting.

    (unwrap-as <type> <expr>)

is equivalent to

    (the <type> (uwrap (tryInto <expr>)))

and

    (unwrap-as <type>)

is equivalent to

    (fn (expr) (the <type> (unwrap (tryInto expr)))).

Note that this may copy the object or allocate memory."

  (cl:let ((try-into (cl:ignore-errors (cl:find-symbol "TRYINTO" "COALTON-LIBRARY/CLASSES")))
           (unwrap (cl:ignore-errors (cl:find-symbol "UNWRAP" "COALTON-LIBRARY/CLASSES"))))
    (cl:assert try-into () "`try-as` macro does not have access to `try-into` yet.")
    (cl:assert unwrap () "`unwrap` macro does not have access to `unwrap` yet.")
    (cl:if expr-supplied-p
           `(the ,type (,unwrap (,try-into ,expr)))
           (alexandria:with-gensyms (lexpr)
             `(fn (,lexpr)
                (the ,type (,unwrap (,try-into ,lexpr))))))))

(cl:defmacro nest (cl:&rest items)
  "A syntactic convenience for function application. Transform

    (NEST f g h x)

to

    (f (g (h x)))."
  (cl:assert (cl:<= 2 (cl:list-length items)))
  (cl:let ((last (cl:last items))
           (butlast (cl:butlast items)))
    (cl:reduce (cl:lambda (x acc)
                 (cl:list x acc))
               butlast :from-end cl:t :initial-value (cl:first last))))

(cl:defmacro pipe (cl:&rest items)
  "A syntactic convenience for function application, sometimes called a \"threading macro\". Transform

    (PIPE x h g f)

to

    (f (g (h x)))."
  (cl:assert (cl:<= 2 (cl:list-length items)))
  `(nest ,@(cl:reverse items)))

(cl:defmacro apply (cl:&rest items)
  "A syntactic convenience for function application. Transform

    (APPLY w x y z)

to

    (fn (f) (f w x y z))."

  (alexandria:with-gensyms (f)
    `(fn (,f)
       (,f ,@items))))

(cl:defmacro .< (cl:&rest items)
  "Right associative compose operator. Creates a new functions that will run the
functions right to left when applied. This is the same as the NEST macro without supplying
the value. The composition is thus the same order as COMPOSE.

`(.< f g h)` creates the function `(fn (x) (f (g (h x))))"
  (alexandria:with-gensyms (x)
    `(fn (,x)
       (nest ,@items ,x))))

(cl:defmacro .> (cl:&rest items)
  "Left associative compose operator. Creates a new functions that will run the
functions left to right when applied. This is the same as the PIPE macro without supplying
the value. The composition is thus the reverse order of COMPOSE.

`(.> f g h)` creates the function `(fn (x) (h (g (f x))))"
  (alexandria:with-gensyms (x)
    `(fn (,x)
       (pipe ,x ,@items))))

(cl:defmacro make-list (cl:&rest forms)
  (cl:labels
      ((list-helper (forms)
         (cl:if (cl:endp forms)
                `coalton:Nil
                `(coalton:Cons ,(cl:car forms) ,(list-helper (cl:cdr forms))))))
    (list-helper forms)))

(cl:defmacro to-boolean (expr)
  "Convert the Lisp expression EXPR, representing a generalized boolean, to a
Coalton boolean."
  `(cl:and ,expr cl:t))

(cl:defmacro assert (datum cl:&optional (format-string "") cl:&rest format-data)
  "Signal an error unless DATUM is `True'.
If the assertion fails, the signaled error will apply the FORMAT-DATA to the FORMAT-STRING via `cl:format' to
produce an error message."
  ;; OPTIMIZE: lazily evaluate the FORMAT-DATA only when the assertion fails
  (cl:check-type format-string cl:string)
  (cl:let* ((datum-temp (cl:gensym "ASSERT-DATUM-"))
            (format-data-temps (alexandria:make-gensym-list (cl:length format-data)
                                                            "ASSERT-FORMAT-DATUM-")))
    `(let ((,datum-temp ,datum)
           ,@(cl:mapcar #'cl:list format-data-temps format-data))
       (progn
         (lisp :any (,datum-temp ,@format-data-temps)
           (cl:assert ,datum-temp ()
                      ,(cl:format cl:nil
                                  "Assertion ~a failed: ~a"
                                  datum format-string)
                      ,@format-data-temps))
         Unit))))
