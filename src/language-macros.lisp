(in-package #:coalton)

;;;; Macros used to implement the Coalton language

(define-expression-macro as (type cl:&optional (expr cl:nil expr-supplied-p))
  "A syntactic convenience for type casting.

    (as <type> <expr>)

is equivalent to

    (the <type> (into <expr>))

and

    (as <type>)

is equivalent to

    (fn (expr) (the <type> (into expr))).

Note that this may copy the object or allocate memory."

  (cl:let ((into (cl:ignore-errors (cl:find-symbol "INTO" "COALTON/CLASSES"))))
    (cl:assert into () "`as` macro does not have access to `into` yet.")
    (cl:if expr-supplied-p
           `(the ,type (,into ,expr))
           (alexandria:with-gensyms (lexpr)
             `(fn (,lexpr)
                (the ,type (,into ,lexpr)))))))

(define-expression-macro try-as (type cl:&optional (expr cl:nil expr-supplied-p))
  "A syntactic convenience for type casting.

    (try-as <type> <expr>)

is equivalent to

    (the (Optional <type>) (tryInto <expr>))

and

    (try-as <type>)

is equivalent to

    (fn (expr) (the (Optional <type>) (tryInto expr))).

Note that this may copy the object or allocate memory."

  (cl:let ((try-into (cl:ignore-errors (cl:find-symbol "TRYINTO" "COALTON/CLASSES")))
           (Optional (cl:ignore-errors (cl:find-symbol "OPTIONAL" "COALTON/CLASSES"))))
    (cl:assert try-into () "`try-as` macro does not have access to `try-into` yet.")
    (cl:assert Optional () "`try-as` macro does not have access to `Optional` yet.")
    (cl:if expr-supplied-p
           `(the (,Optional ,type) (,try-into ,expr))
           (alexandria:with-gensyms (lexpr)
             `(fn (,lexpr)
                (the (,Optional ,type) (,try-into ,lexpr)))))))

(define-expression-macro unwrap-as (type cl:&optional (expr cl:nil expr-supplied-p))
  "A syntactic convenience for type casting.

    (unwrap-as <type> <expr>)

is equivalent to

    (the <type> (uwrap (tryInto <expr>)))

and

    (unwrap-as <type>)

is equivalent to

    (fn (expr) (the <type> (unwrap (tryInto expr)))).

Note that this may copy the object or allocate memory."

  (cl:let ((try-into (cl:ignore-errors (cl:find-symbol "TRYINTO" "COALTON/CLASSES")))
           (unwrap (cl:ignore-errors (cl:find-symbol "UNWRAP" "COALTON/CLASSES"))))
    (cl:assert try-into () "`try-as` macro does not have access to `try-into` yet.")
    (cl:assert unwrap () "`unwrap` macro does not have access to `unwrap` yet.")
    (cl:if expr-supplied-p
           `(the ,type (,unwrap (,try-into ,expr)))
           (alexandria:with-gensyms (lexpr)
             `(fn (,lexpr)
                (the ,type (,unwrap (,try-into ,lexpr))))))))

(define-expression-macro need (expr)
  "Take the value held by EXPR, a `Fallible` container such as a `Result` or an `Optional`. If EXPR holds a failure instead, such as an `Err` or `None`, return that failure from the enclosing function, whose result must be a container of the same kind.

    (need <expr>)

is equivalent to

    (match (split-failure <expr>)
      ((Ok value) value)
      ((Err failure) (return failure)))

`need` does not catch exceptions; use `coalton/result:try` to turn thrown exceptions into `Result` values first."
  (cl:let ((split-failure (cl:ignore-errors (cl:find-symbol "SPLIT-FAILURE" "COALTON/CLASSES")))
           (ok (cl:ignore-errors (cl:find-symbol "OK" "COALTON/CLASSES")))
           (err (cl:ignore-errors (cl:find-symbol "ERR" "COALTON/CLASSES"))))
    (cl:assert (cl:and split-failure ok err) ()
               "`need` macro does not have access to `split-failure` yet.")
    (alexandria:with-gensyms (value failure)
      `(match (,split-failure ,expr)
         ((,ok ,value) ,value)
         ((,err ,failure) (return ,failure))))))

(define-expression-macro nest (cl:&rest items)
  "A syntactic convenience for function application. Transform

    (nest f g h x)

to

    (f (g (h x)))."
  (cl:assert (cl:<= 2 (cl:list-length items)))
  (cl:let ((last (cl:last items))
           (butlast (cl:butlast items)))
    (cl:reduce (cl:lambda (x acc)
                 (cl:list x acc))
               butlast :from-end cl:t :initial-value (cl:first last))))

(define-expression-macro pipe (cl:&rest items)
  "A syntactic convenience for function application, sometimes called a \"threading macro\". Transform

    (pipe x h g f)

to

    (f (g (h x)))."
  (cl:assert (cl:<= 2 (cl:list-length items)))
  `(nest ,@(cl:reverse items)))

(define-expression-macro .< (cl:&rest items)
  "Right associative compose operator. Creates a new functions that will run the
functions right to left when applied. This is the same as the `nest` macro without supplying
the value. The composition is thus the same order as `compose`.

`(.< f g h)` creates the function `(fn (x) (f (g (h x))))`."
  (alexandria:with-gensyms (x)
    `(fn (,x)
       (nest ,@items ,x))))

(define-expression-macro .> (cl:&rest items)
  "Left associative compose operator. Creates a new functions that will run the
functions left to right when applied. This is the same as the `pipe` macro without supplying
the value. The composition is thus the reverse order of `compose`.

`(.> f g h)` creates the function `(fn (x) (h (g (f x))))`."
  (alexandria:with-gensyms (x)
    `(fn (,x)
       (pipe ,x ,@items))))

(define-expression-macro make-list (cl:&rest forms)
  "Create a heterogeneous Coalton `List` of objects. This macro is
deprecated; use `coalton/list:make`."
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

(define-expression-macro assert (datum cl:&optional (format-string "") cl:&rest format-data)
  "Signal a `Panic` unless `datum` is `True`.

If the assertion fails, the panic's message describes `datum` and applies the
`format-data` to the `format-string` via `cl:format`."
  ;; OPTIMIZE: lazily evaluate the FORMAT-DATA only when the assertion fails
  (cl:check-type format-string cl:string)
  (cl:let* ((datum-temp (cl:gensym "ASSERT-DATUM-"))
            (format-data-temps (alexandria:make-gensym-list (cl:length format-data)
                                                            "ASSERT-FORMAT-DATUM-"))
            (panic (cl:find-symbol "PANIC" "COALTON/CLASSES"))
            (message `(cl:format cl:nil "Assertion ~A failed: ~?"
                                 ',datum ,format-string (cl:list ,@format-data-temps))))
    `(let ((,datum-temp ,datum)
           ,@(cl:mapcar #'cl:list format-data-temps format-data))
       (progn
         (lisp (-> :any) (,datum-temp ,@format-data-temps)
           (cl:unless ,datum-temp
             ,(cl:if panic
                     `(cl:error ',panic :message ,message)
                     `(cl:error "~A" ,message))))
         Unit))))
