(defpackage #:coalton-impl/codegen/codegen-match
  (:use #:cl)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:settings #:coalton-impl/settings)
   (#:ast #:coalton-impl/codegen/ast)
   (#:pattern #:coalton-impl/codegen/pattern)
   (#:codegen-pattern #:coalton-impl/codegen/codegen-pattern))
  (:export
   #:codegen-cond-case
   #:codegen-case-case
   #:codegen-cond-fallback
   #:codegen-case-fallback
   #:codegen-cond-match
   #:codegen-case-match))

(in-package #:coalton-impl/codegen/codegen-match)

(defun codegen-cond-case (expr pattern match-var match-expr-type env)
  (declare (type t expr)
           (type pattern:pattern pattern)
           (type symbol match-var)
           (type tc:ty match-expr-type)
           (type tc:environment env)
           (values t &optional))

  (multiple-value-bind (pred bindings types)
      (codegen-pattern:codegen-pattern pattern match-var match-expr-type env)

    `(,pred
      ,(cond
         ((null bindings)
          expr)
         (t
          `(let ,bindings
             (declare (ignorable ,@(mapcar #'car bindings))
                      ,@(cond
                          (settings:*emit-type-annotations*
                           (loop :for binding :in bindings
                                 :for var := (car binding)
                                 :for type :in types
                                 :collect `(type ,type ,var)))
                          (t
                           nil)))
             ,expr))))))

(defun codegen-case-case (expr pattern match-var match-expr-type env)
  (declare (type t expr)
           (type pattern:pattern pattern)
           (type symbol match-var)
           (type tc:ty match-expr-type)
           (type tc:environment env)
           (values t &optional))

  (multiple-value-bind (pred bindings types)
      (codegen-pattern:codegen-pattern pattern match-var match-expr-type env)
    (declare (ignore pred))

    (cond
      ((pattern:pattern-literal-p pattern)
       `(,(pattern:pattern-literal-value pattern)
         ,expr))
      ((pattern:pattern-constructor-p pattern)
       (let* ((name (pattern:pattern-constructor-name pattern))
              (entry (tc:lookup-constructor env name)))
         `(,(tc:constructor-entry-compressed-repr entry)
           ,expr)))
      (t
       `(otherwise
         (let ,bindings
           (declare (ignorable ,@(mapcar #'car bindings))
                    ,@(cond
                        (settings:*emit-type-annotations*
                         (loop :for binding :in bindings
                               :for var := (car binding)
                               :for type :in types
                               :collect `(type ,type ,var)))
                        (t
                         nil)))
           ,expr))))))

(defun codegen-cond-fallback ()
  (declare (values t &optional))

  `(t
    (error "Pattern match not exhaustive error.")))

(defun codegen-case-fallback ()
  (declare (values t &optional))

  `(otherwise
    (error "Pattern match not exhaustive error.")))

(defun codegen-cond-match (cases &optional fallback)
  `(cond
     ,@cases
     ,@(if fallback
           (list fallback)
           '())))

(defun codegen-case-match (match-expr cases &optional fallback)
  `(case ,match-expr
     ,@cases
     ,@(if fallback
           (list fallback)
           '())))
