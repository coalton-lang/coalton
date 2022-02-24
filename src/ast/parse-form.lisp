(in-package #:coalton-impl/ast)

(defun parse-form (expr m package)
  "Parse the value form FORM into a NODE structure. This also performs macro-expansion.

This does not attempt to do any sort of analysis whatsoever. It is suitable for parsing expressions irrespective of environment."
  (declare (type immutable-map m)
           (values node &optional)
           (type package package))
  (cond
    ((atom expr)
     (etypecase expr
       (null    (error-parsing
                 expr
                 "The empty list literal (), also written as the symbol ~
                 COMMON-LISP:NIL, is not valid in Coalton.~&~%This error is ~
                 often triggered by expanding a buggy macro."))
       (symbol  (parse-variable expr m))
       (literal-value
        (parse-atom expr))))
    ((alexandria:proper-list-p expr)
     (alexandria:destructuring-case expr
       ;; Abstraction
       (((or coalton:fn coalton:λ) &rest args)
        (unless (<= 2 (length args))
          (error-parsing
           expr
           "Invalid anonymous function expression.~&~%~
            Usage: (~S (VARS*) BODY)"
           (car expr)))
        (parse-abstraction expr (first args) (rest args) m package))

       ;; Let
       ((coalton:let &rest args)
        (unless (= 2 (length args))
          (error-parsing expr "Invalid binding expression."))
        (parse-let expr (first args) (second args) m package))

       ;; Lisp
       ((coalton:lisp &rest args)
        (unless (<= 3 (length args))
          (error-parsing expr "Invalid embedded Lisp expression."))
        (parse-lisp expr (first args) (second args) (nthcdr 2 args) m))

       ;; Match
       ((coalton:match expr_ &rest patterns)
        (parse-match expr expr_ patterns m package))

       ;; Seq
       ((coalton:seq &rest subnodes)
        (parse-seq expr subnodes m package))

       ;; The
       ((coalton:the &rest args)
        (unless (= 2 (length args))
          (error-parsing expr "Invalid type assertion."))
        (parse-the expr (first args) (second args) m package))

       ;; Application
       ((t &rest rands)
        (cond
          ((and (symbolp (first expr)) (macro-function (first expr)))
           (let ((expansion (funcall (macro-function (first expr)) expr nil)))
             (parse-form expansion m package)))
          ((null rands)
           (let ((unit (alexandria:ensure-symbol
                        "UNIT"
                        (find-package "COALTON-LIBRARY/CLASSES"))))
             (parse-application expr (first expr) (list unit) m package)))

          (t (parse-application expr (first expr) rands m package))))))
    ((listp expr) ;; EXPR already flunked PROPER-LIST-P, so it's a dotted list.
     (error-parsing expr "Dotted lists are not valid Coalton syntax."))
    (t
     (unreachable))))

(defun invert-alist (alist)
  (loop :for (key . value) :in alist
        :collect (cons value key)))

(defun lookup-or-key (m key)
  (declare (type immutable-map m)
           (type symbol key))
  (or (immutable-map-lookup m key)
      key))

(defun parse-variable (var m)
  (declare (type symbol var)
           (type immutable-map m)
           (values node-variable))
  (node-variable var (lookup-or-key m var)))

(defun make-local-vars (vars package)
  (declare (type symbol-list vars)
           (type package package))
  (loop :for var :in vars
        :collect
        (cons
         var
         (alexandria:ensure-symbol (gensym (concatenate 'string (symbol-name var) "-")) package))))

(defun parse-abstraction (unparsed vars subexprs m package)
  (declare (type t unparsed)
           (type symbol-list vars)
           (type list subexprs)
           (type immutable-map m)
           (type package package))

  (let ((nullary-fn nil)
        (var nil))
    (when (null vars)
      (setf var (alexandria:ensure-symbol (gensym) package))
      (setf vars (list var))
      (setf nullary-fn t))

    (let* ((binding-local-names (make-local-vars vars package))
           (new-m (immutable-map-set-multiple m binding-local-names))
           (subform (parse-form
                     (funcall (macro-function 'coalton:progn) (cons 'coalton:progn subexprs) nil)
                     new-m
                     package)))

       (node-abstraction
        unparsed
        (mapcar #'cdr binding-local-names)
        (if nullary-fn
            (node-seq
             unparsed
             (list
              (node-the
               `(the Unit ,var)
               `coalton:Unit
               (node-variable
                var
                (immutable-map-lookup new-m var)))
              subform))
            subform)
        (invert-alist binding-local-names)))))

(defun parse-let (unparsed binding-forms subexpr m package)
  (declare (type t unparsed)
           (type list binding-forms)
           (type t subexpr)
           (type immutable-map m)
           (type package package)
           (values node-let))

  (let ((bindings nil)
        (declared-types nil))
    ;; Separate bindings from type declarations
    (loop :for form :in binding-forms :do
      (cond
        ((not (listp form))
         (error-parsing form "Invalid let binding form"))
        ((= 2 (length form))
         (unless (symbolp (first form))
           (error-parsing form "Invalid let binding form"))
         (push (cons (first form)
                     (second form))
               bindings))
        ((= 3 (length form))
         (unless (eql 'coalton:declare (first form))
           (error-parsing form "Invalid let binding form"))
         (unless (symbolp (second form))
           (error-parsing form "Invalid let binding declaration form"))
         (push (cons (second form)
                     (third form))
               declared-types))
        (t
         (error-parsing form "Invalid let binding form"))))

    ;; Check that all declarations have a matching binding
    (loop :for (bind-var . bind-type) :in declared-types :do
      (unless (member bind-var bindings :key #'car)
        (error-parsing unparsed "Orphan type declaration for variable ~S" bind-var)))

    ;; Perform variable renaming
    (let* ((binding-names (mapcar #'car bindings))
           (binding-local-names (make-local-vars binding-names package))
           (new-m (immutable-map-set-multiple m binding-local-names)))

      (node-let
       unparsed
       (loop :for (bind-var . bind-val) :in bindings
             :collect (cons
                       (lookup-or-key new-m bind-var)
                       (parse-form bind-val new-m package)))
       (loop :for (bind-var . bind-type) :in declared-types
             :collect (cons
                       (lookup-or-key new-m bind-var)
                       bind-type))
       (parse-form subexpr new-m package)
       (invert-alist binding-local-names)))))

(defun parse-lisp (unparsed type variables lisp-exprs m)
  (declare (type immutable-map m))
  (node-lisp
   unparsed
   type
   (loop :for var :in variables
         :collect (cons
                   var
                   (or (immutable-map-lookup m var)
                       (error-parsing unparsed "Unknown variable ~A in lisp node~%" var))))
   ;; Do *NOT* parse LISP-EXPR!
   lisp-exprs))

(defun parse-application (unparsed rator rands m package)
  (declare (type immutable-map m)
           (type package package))
     (node-application
      unparsed
      (parse-form rator m package)
      (mapcar
       (lambda (rand)
         (parse-form rand m package))
       rands)))

(defun parse-match-branch (branch m package)
  (declare (type immutable-map m)
           (type package package))
  (assert (= 2 (length branch))
          () "Malformed match branch ~A" branch)
  (let* ((parsed-pattern (parse-pattern (first branch)))
         (pattern-vars (pattern-variables parsed-pattern))
         (local-vars (make-local-vars pattern-vars package))
         (new-m (immutable-map-set-multiple m local-vars))
         (parsed-pattern (rewrite-pattern-vars parsed-pattern new-m))
         (parsed-expr (parse-form (second branch) new-m package)))
    (make-match-branch
     :unparsed branch
     :pattern parsed-pattern
     :subexpr parsed-expr
     :name-map (invert-alist local-vars))))

(defun parse-match (unparsed expr branches m package)
  (declare (type immutable-map m)
           (type package package))
  (let ((parsed-expr (parse-form expr m package))
        (parsed-branches
          (mapcar
           (lambda (branch)
             (parse-match-branch branch m package))
           branches)))
    (node-match unparsed parsed-expr parsed-branches)))

(defun parse-pattern (pattern)
  (cond
    ((typep pattern 'literal-value)
     (pattern-literal pattern))
    ((and (symbolp pattern)
          (eql 'coalton:_ pattern))
     (pattern-wildcard))
    ((symbolp pattern)
     (pattern-var pattern))
    ((listp pattern)
     (let ((ctor (first pattern))
           (args (rest pattern)))
       (pattern-constructor ctor (mapcar #'parse-pattern args))))))

(defun parse-atom (atom)
  ;; Convert integer literals into fromInt calls. This allows for
  ;; "overloaded" number literals. Other literals are left as is.
  (let ((fromInt
          (alexandria:ensure-symbol
           "FROMINT"
           (find-package "COALTON-LIBRARY/CLASSES"))))
    (etypecase atom
      (integer (node-application
         atom
         (node-variable fromInt fromInt)
         (list (node-literal atom atom))))
      (t (node-literal atom atom)))))

(defun parse-seq (expr subnodes m package)
  (declare (type t expr)
           (type list subnodes)
           (type immutable-map m)
           (type package package))
  (assert (< 0 (length subnodes))
          ()  "Seq form must have at least one node")
  (node-seq
   expr
   (mapcar (lambda (node)
             (parse-form node m package))
           subnodes)))

(defun parse-the (expr type form m package)
  (declare (type t expr)
           (type t type)
           (type t form)
           (type immutable-map m)
           (type package package))
  (node-the
   expr
   type
   (parse-form form m package)))
