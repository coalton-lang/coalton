(defpackage #:coalton-impl/codegen/codegen-pattern
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:codegen-pattern))

(in-package #:coalton-impl/codegen/codegen-pattern)

(defgeneric codegen-pattern (pattern expr env)
  (:documentation "Codegen the match branch PATTERN on lisp value EXPR, returning (VALUES PREDICATE BINDINGS).")
  (:method ((pattern pattern-var) expr env)
    (declare (type tc:environment env)
             (ignore env))
    (values
     `t
     (list (list (pattern-var-name pattern) expr))))

  (:method ((pattern pattern-wildcard) expr env)
    (declare (type coalton-impl/typechecker::environment env))
    (values `t nil))

  (:method ((pattern pattern-literal) expr env)
    (declare (type coalton-impl/typechecker::environment env))
    (values
     `(eql ,(pattern-literal-value pattern) ,expr)
     nil))

  (:method ((pattern pattern-constructor) expr env)
    (declare (type coalton-impl/typechecker::environment env))
    (let* ((name (pattern-constructor-name pattern))
           (entry (tc:lookup-constructor env name))
           (type (tc:lookup-type env (tc:constructor-entry-constructs entry)))
           (package (symbol-package name)))

      (cond
        ;; For enums we can just check symbol equality
        ((tc:type-entry-enum-repr type)
         (values
          `(eql ',(tc:constructor-entry-compressed-repr entry) ,expr)
          nil))

        ;; For newtypes we can just codegen patterns for the inner type
        ((tc:type-entry-newtype type)
         (unless (= 1 (length (pattern-constructor-patterns pattern)))
           (util:coalton-bug "Unexpected number of fields in newtype pattern.~%    Expected: 1~%    Received: ~A~%"
                             (length (pattern-constructor-patterns pattern))))
         (codegen-pattern (first (pattern-constructor-patterns pattern)) expr env))

        ;; Check cons directly
        ((eql (pattern-constructor-name pattern) 'coalton:Cons)

         (multiple-value-bind (car-pred car-bindings)
             (codegen-pattern (first (pattern-constructor-patterns pattern))
                              `(car ,expr)
                              env)
           (multiple-value-bind (cdr-pred cdr-bindings)
               (codegen-pattern (second (pattern-constructor-patterns pattern))
                                `(cdr ,expr)
                                env)
             (values
              `(and (cl:consp ,expr)
                    ,car-pred
                    ,cdr-pred)
              (append car-bindings cdr-bindings)))))

        ;; Check nil directly
        ((eql (pattern-constructor-name pattern) 'coalton:Nil)
         (values
          `(null ,expr)
          nil))

        ;; Otherwise we need to bring out the big guns
        (t
         (let* ((ctor-name (pattern-constructor-name pattern))
                (ctor (tc:lookup-constructor env ctor-name))
                (lisp-type-string (format nil "~A/~A" (tc:constructor-entry-constructs ctor) ctor-name)))
           (multiple-value-bind (preds bindings)
               (loop :with preds := nil
                     :with bindings := nil

                     :for field :in (pattern-constructor-patterns pattern)
                     :for i :from 0

                     :for accessor := (alexandria:format-symbol package "~A-_~D" lisp-type-string i)

                     :do (multiple-value-bind (field-pred field-bindings)
                             (codegen-pattern
                              field
                              `(,accessor ,expr)
                              env)
                           (push field-pred preds)
                           (push field-bindings bindings))
                     :finally (return (values (nreverse preds)
                                              (nreverse bindings))))

             (values
              `(and
                (typep ,expr ',(alexandria:format-symbol package "~A" lisp-type-string))
                ,@preds)
              (reduce #'append bindings)))))))))
