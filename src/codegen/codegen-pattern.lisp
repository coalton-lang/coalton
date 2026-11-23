(defpackage #:coalton-impl/codegen/codegen-pattern
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:settings #:coalton-impl/settings)
   (#:tc #:coalton-impl/typechecker)
   (#:rt #:coalton-impl/runtime))
  (:export
   #:codegen-pattern
   #:codegen-pattern-test))

(in-package #:coalton-impl/codegen/codegen-pattern)

(defgeneric codegen-pattern-test (test expr env)
  (:documentation "Emit a literal's translated comparison against Lisp EXPR."))


(defun codegen-pattern (pattern expr expr-type env)
  "Codegen the match branch PATTERN on lisp value EXPR of (Coalton) type
EXPR-TYPE, returning (VALUES PREDICATE BINDINGS BINDING-TYPES).

PREDICATE runs its tests left to right in source order, with one exception.
A constructor tag test accepts any object, but a literal comparison such as
`string=' or a user `==' assumes that its operand has the literal's type.
When that type mentions a type variable introduced by a constructor in
PATTERN (an existential field of a GADT), the type is justified only by the
tag of another constructor among that constructor's fields, possibly one to
the right of the literal. The same holds for a literal whose own type is
concrete but which is reached through a projection that does not check the
class of its operand (a newtype, Some or Cons): a constant field type of
such a constructor says nothing about a value of a hidden type. Such a
literal is moved to the end of that enclosing constructor's tests, keeping
its order relative to the other moved literals. Every other test keeps its
source position."
  (declare (type pattern pattern)
           (type tc:ty expr-type)
           (type tc:environment env))
  (multiple-value-bind (tests deferred bindings binding-types)
      (pattern-tests pattern expr expr-type (tc:type-variables expr-type) nil env)
    (let ((tests (append tests (mapcar #'car deferred))))
      (values (if (rest tests) `(and ,@tests) (or (first tests) t))
              bindings
              binding-types))))

(defun constructor-field-types (ctor-name expr-type env)
  "Return (VALUES FIELD-TYPES INTRODUCED) for constructor CTOR-NAME tested
against a value whose static type is EXPR-TYPE.

FIELD-TYPES are what CTOR-NAME's tag alone proves about its fields.
INTRODUCED lists the type variables of FIELD-TYPES that do not occur in
EXPR-TYPE; tag tests among the fields can refine them. When EXPR-TYPE does not
expose the constructor's type head (it is a variable hidden by an enclosing
constructor), nothing relates the fields to it, so the field types are left
unrefined and INTRODUCED is empty."
  (declare (type symbol ctor-name)
           (type tc:ty expr-type)
           (type tc:environment env)
           (values tc:ty-list tc:tyvar-list &optional))
  (let* ((ctor-type (tc:qualified-ty-type
                     (tc:fresh-inst (tc:lookup-value-type env ctor-name))))
         (result (tc:function-return-type ctor-type)))
    (cond
      ((tc:ty= (tc:base-type result) (tc:base-type expr-type))
       (let ((field-types (tc:apply-substitution
                           (tc:unify-pattern-result nil result expr-type :all)
                           (tc:function-type-arguments ctor-type))))
         (values field-types
                 (set-difference (tc:type-variables field-types)
                                 (tc:type-variables expr-type)
                                 :test #'tc:ty=))))
      (t
       (values (tc:function-type-arguments ctor-type) nil)))))

(defun unresolved-type-variables (ty known)
  "Return the type variables of TY that are not among KNOWN."
  (declare (type tc:ty ty)
           (type tc:tyvar-list known)
           (values tc:tyvar-list &optional))
  (remove-if (lambda (tv) (member tv known :test #'tc:ty=))
             (tc:type-variables ty)))

(defgeneric pattern-tests (pattern expr expr-type known deps env)
  (:documentation "Return (VALUES TESTS DEFERRED BINDINGS BINDING-TYPES) for PATTERN on
lisp value EXPR, whose static type is EXPR-TYPE.

TESTS are forms to run in order; each may access fields established by the
tag tests before it. DEFERRED is a list of (FORM . TYVARS) literal tests,
in source order, that may run only once TYVARS are established by an
enclosing constructor's field tags. TYVARS are the type variables of the
operand's type that are not among KNOWN (the variables of the whole
scrutinee's type), together with DEPS: the unresolved evidence inherited
from enclosing projections that do not check the class of their operand.")
  (:method ((pattern pattern-var) expr expr-type known deps env)
    (declare (ignore known deps)
             (type tc:environment env))
    (values
     nil
     nil
     (list (list (pattern-var-name pattern) expr))
     (list (tc:lisp-type expr-type env))))

  (:method ((pattern pattern-wildcard) expr expr-type known deps env)
    (declare (ignore pattern expr expr-type known deps env))
    (values nil nil nil nil))

  (:method ((pattern pattern-binding) expr expr-type known deps env)
    (multiple-value-bind (tests deferred bindings binding-types)
        (pattern-tests (pattern-binding-pattern pattern) expr expr-type known deps env)
      (values
       tests
       deferred
       (cons (list (pattern-var-name (pattern-binding-var pattern))
                   expr)
             bindings)
       (cons (tc:lisp-type expr-type env)
             binding-types))))

  (:method ((pattern pattern-literal) expr expr-type known deps env)
    (let ((form (cond
                  ((pattern-literal-test pattern)
                   (codegen-pattern-test (pattern-literal-test pattern) expr env))
                  ((stringp (pattern-literal-value pattern))
                   `(string= ,(pattern-literal-value pattern) ,expr))
                  (t `(eql ,(pattern-literal-value pattern) ,expr))))
          (pending (union deps (unresolved-type-variables expr-type known) :test #'tc:ty=)))
      (if pending
          (values nil (list (cons form pending)) nil nil)
          (values (list form) nil nil nil))))

  (:method ((pattern pattern-constructor) expr expr-type known deps env)
    (declare (type coalton-impl/typechecker::environment env))
    (let* ((name (pattern-constructor-name pattern))
           (entry (tc:lookup-constructor env name))
           (type (tc:lookup-type env (tc:constructor-entry-constructs entry)))
           (package (symbol-package name)))
      (flet ((with-fields (tag field-exprs class-tag-p)
               ;; TAG guards every field access, so it comes first. Only a
               ;; class test proves that the value was built by this
               ;; constructor, and so that its fields have the constructor's
               ;; field types. Any other projection keeps the evidence that
               ;; the value's own type still awaits.
               (multiple-value-bind (field-types introduced)
                   (constructor-field-types name expr-type env)
                 (let ((field-deps (if class-tag-p
                                       nil
                                       (union deps (unresolved-type-variables expr-type known)
                                              :test #'tc:ty=)))
                       (tests (and tag (list tag)))
                       (deferred nil) (bindings nil) (binding-types nil))
                   (loop :for field :in (pattern-constructor-patterns pattern)
                         :for field-expr :in field-exprs
                         :for field-type :in field-types
                         :do (multiple-value-bind (field-tests field-deferred field-bindings field-binding-types)
                                 (pattern-tests field field-expr field-type known field-deps env)
                               (setf tests (append tests field-tests))
                               (setf deferred (append deferred field-deferred))
                               (setf bindings (append bindings field-bindings))
                               (setf binding-types (append binding-types field-binding-types))))
                   ;; Every field tag has now run, so literals that depend
                   ;; only on this constructor's own type variables are
                   ;; justified.
                   (flet ((ready-p (d) (subsetp (cdr d) introduced :test #'tc:ty=)))
                     (values (append tests (mapcar #'car (remove-if-not #'ready-p deferred)))
                             (remove-if #'ready-p deferred)
                             bindings
                             binding-types))))))
        (cond
          ;; For enums we can just check symbol equality
          ((tc:type-entry-enum-repr type)
           (values
            (list `(eql ',(tc:constructor-entry-compressed-repr entry) ,expr))
            nil
            nil
            nil))

          ;; A newtype has no tag; its field is the value itself.
          ((tc:type-entry-newtype type)
           (unless (= 1 (length (pattern-constructor-patterns pattern)))
             (util:coalton-bug "Unexpected number of fields in newtype pattern.~%    Expected: 1~%    Received: ~A~%"
                               (length (pattern-constructor-patterns pattern))))
           (with-fields nil (list expr) nil))

          ;; Check Cons directly
          ((eql name 'coalton:Cons)
           (with-fields `(cl:consp ,expr) (list `(car ,expr) `(cdr ,expr)) nil))

          ;; Check Nil directly
          ((eql name 'coalton:Nil)
           (values (list `(null ,expr)) nil nil nil))

          ;; Check Some directly. The payload is EXPR itself only when its
          ;; type is known not to be represented by an optional.
          ((eql name 'coalton:Some)
           (let* ((inner-type (first (constructor-field-types name expr-type env)))
                  (inner-lisp-type (tc:lisp-type inner-type env))
                  (unwrap-expr
                    (if (and (not (eq inner-lisp-type 't))
                             (ignore-errors
                              (subtypep inner-lisp-type
                                        '(not coalton-impl/runtime/optional::cl-optional))))
                        expr
                        `(rt:unwrap-cl-some ,expr))))
             (with-fields `(rt:cl-some-p ,expr) (list unwrap-expr) nil)))

          ;; Check None directly
          ((eql name 'coalton:None)
           (values (list `(rt:cl-none-p ,expr)) nil nil nil))

          ;; Otherwise we need to bring out the big guns
          (t
           (let ((lisp-type-string (format nil "~A/~A" (tc:constructor-entry-constructs entry) name)))
             (with-fields `(typep ,expr ',(alexandria:format-symbol package "~A" lisp-type-string))
                          (loop :for i :from 0 :below (length (pattern-constructor-patterns pattern))
                                :collect `(,(alexandria:format-symbol package "~A-_~D" lisp-type-string i) ,expr))
                          t))))))))
