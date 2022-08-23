(defpackage #:coalton-impl/ast/parse-form
  (:use
   #:cl
   #:coalton-impl/algorithm
   #:coalton-impl/ast/pattern
   #:coalton-impl/ast/node
   #:coalton-impl/ast/parse-error)
  (:local-nicknames
   (:util #:coalton-impl/util))
  (:export
   #:parse-form))

(in-package #:coalton-impl/ast/parse-form)

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
       (util:literal-value
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
        (when (null args)
          (error-parsing
           expr
           "Let forms must include a binding list"))
        (parse-let expr (first args) (rest args) m package))

       ;; Lisp
       ((coalton:lisp &rest args)
        (unless (<= 3 (length args))
          (error-parsing expr "Invalid embedded Lisp expression."))
        (parse-lisp expr (first args) (second args) (nthcdr 2 args) m))

       ;; Match
       ((coalton:match expr_ &rest patterns)
        (parse-match expr expr_ patterns m package))

       ;; Seq
       ((coalton::seq &rest subnodes)
        (parse-seq expr subnodes m package))

       ;; The
       ((coalton:the &rest args)
        (unless (= 2 (length args))
          (error-parsing expr "Invalid type assertion."))
        (parse-the expr (first args) (second args) m package))

       ;; Return
       ((coalton:return form)
        (parse-return expr form m package))

       ;; Bind
       ((coalton::bind &rest args)
        (unless (= 3 (length args))
          (error-parsing expr "Invalid bind expression"))
        (parse-bind expr (first args) (second args) (third args) m package))

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
     (util:unreachable))))

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
  (make-node-variable
   :unparsed var
   :name (lookup-or-key m var)))

(defun make-local-vars (vars package)
  (declare (type util:symbol-list vars)
           (type package package))
  (loop :for var :in vars
        :collect
        (cons
         var
         (gentemp (concatenate 'string (symbol-name var) "-") package))))

(defun parse-abstraction (unparsed vars subexprs m package)
  (declare (type t unparsed)
           (type util:symbol-list vars)
           (type list subexprs)
           (type immutable-map m)
           (type package package))

  (let ((nullary-fn nil)
        (var nil))
    (when (null vars)
      (setf var (gentemp "G" package))
      (setf vars (list var))
      (setf nullary-fn t))

    (let* ((binding-local-names (make-local-vars vars package))
           (new-m (immutable-map-set-multiple m binding-local-names))
           (subform (parse-form
                     (funcall (macro-function 'coalton:progn) (cons 'coalton:progn subexprs) nil)
                     new-m
                     package)))

      (make-node-abstraction
       :unparsed unparsed
       :vars (mapcar #'cdr binding-local-names)
       :subexpr (if nullary-fn
                    (make-node-seq
                     :unparsed unparsed
                     :subnodes (list
                                (make-node-the
                                 :unparsed `(the Unit ,var)
                                 :type `coalton:Unit
                                 :subnode (make-node-variable
                                           :unparsed var
                                           :name (immutable-map-lookup new-m var)))
                                subform))
                    subform)
       :name-map (invert-alist binding-local-names)))))

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

      (make-node-let
       :unparsed unparsed
       :bindings (loop :for (bind-var . bind-val) :in bindings
                       :collect (cons
                                 (lookup-or-key new-m bind-var)
                                 (parse-form bind-val new-m package)))
       :declared-types (loop :for (bind-var . bind-type) :in declared-types
                             :collect (cons
                                       (lookup-or-key new-m bind-var)
                                       bind-type))
       :subexpr (parse-form (cons 'coalton:progn subexpr) new-m package)
       :name-map (invert-alist binding-local-names)))))

(defun parse-lisp (unparsed type variables lisp-exprs m)
  (declare (type immutable-map m))
  (make-node-lisp
   :unparsed unparsed
   :type type
   :variables (loop :for var :in variables
                    :collect (cons
                              var
                              (or (immutable-map-lookup m var)
                                  (error-parsing unparsed "Unknown variable ~A in lisp node~%" var))))
   ;; Do *NOT* parse LISP-EXPRS!
   :form lisp-exprs))

(defun parse-application (unparsed rator rands m package)
  (declare (type immutable-map m)
           (type package package))
  (make-node-application
   :unparsed unparsed
   :rator (parse-form rator m package)
   :rands (mapcar
           (lambda (rand)
             (parse-form rand m package))
           rands)))

(defun parse-match-branch (branch m package)
  (declare (type immutable-map m)
           (type package package))
  (assert (<= 2 (length branch))
          () "Malformed match branch ~A" branch)
  (let* ((parsed-pattern (parse-pattern (first branch)))
         (pattern-vars (pattern-variables parsed-pattern))
         (local-vars (make-local-vars pattern-vars package))
         (new-m (immutable-map-set-multiple m local-vars))
         (parsed-pattern (rewrite-pattern-vars parsed-pattern new-m))
         (parsed-expr (parse-form (funcall (macro-function 'coalton:progn) (cons 'coalton:progn (cdr branch)) nil) new-m package)))
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
    (make-node-match
     :unparsed unparsed
     :expr parsed-expr
     :branches parsed-branches)))

(defun parse-pattern (pattern)
  (cond
    ((typep pattern 'util:literal-value)
     (make-pattern-literal :value pattern))
    ((and (symbolp pattern)
          (eql 'coalton:_ pattern))
     (make-pattern-wildcard))
    ((symbolp pattern)
     (make-pattern-var :id pattern))
    ((listp pattern)
     (let ((ctor (first pattern))
           (args (rest pattern)))
       (make-pattern-constructor
        :name ctor
        :patterns (mapcar #'parse-pattern args))))))

(defun parse-atom (atom)
  ;; Convert integer literals into fromInt calls. This allows for
  ;; "overloaded" number literals. Other literals are left as is.
  (let ((fromInt (alexandria:ensure-symbol "FROMINT" (find-package "COALTON-LIBRARY/CLASSES"))))
    (etypecase atom
      (integer (make-node-application
                :unparsed atom
                :rator (make-node-variable
                        :unparsed fromInt
                        :name fromInt)
                :rands (list
                        (make-node-literal
                         :unparsed atom
                         :value atom))))
      (t (make-node-literal
          :unparsed atom
          :value atom)))))

(defun parse-seq (expr subnodes m package)
  (declare (type t expr)
           (type list subnodes)
           (type immutable-map m)
           (type package package))
  (assert (< 0 (length subnodes))
      ()  "Seq form must have at least one node")
  (make-node-seq
   :unparsed expr
   :subnodes (mapcar (lambda (node)
                       (parse-form node m package))
                     subnodes)))

(defun parse-the (expr type form m package)
  (declare (type t expr)
           (type t type)
           (type t form)
           (type immutable-map m)
           (type package package))
  (make-node-the
   :unparsed expr
   :type type
   :subnode (parse-form form m package)))


(defun parse-return (expr form m package)
  (declare (type t expr)
           (type t form)
           (type immutable-map m)
           (type package package))
  (make-node-return
   :unparsed expr
   :expr (parse-form form m package)))

(defun parse-bind (expr name node body m package)
  (declare (type t expr)
           (type symbol name)
           (type t node)
           (type t body)
           (type immutable-map m)
           (type package package))
  (let* ((name (first (make-local-vars (list name) package)))
         (new-m (immutable-map-set m (car name) (cdr name))))
    (make-node-bind
     :unparsed expr
     :name (cdr name)
     :expr (parse-form node m package)
     :body (parse-form body new-m package))))
