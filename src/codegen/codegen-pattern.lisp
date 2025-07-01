(defpackage #:coalton-impl/codegen/codegen-pattern
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:settings #:coalton-impl/settings)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:codegen-pattern))

(in-package #:coalton-impl/codegen/codegen-pattern)

(defgeneric codegen-pattern (pattern expr expr-type env)
  (:documentation "Codegen the match branch PATTERN on lisp value EXPR of (Coalton) type
EXPR-TYPE, returning (VALUES PREDICATE BINDINGS BINDING-TYPES).")
  (:method ((pattern pattern-var) expr expr-type env)
    (declare (type tc:environment env))
    (values
     `t
     (list (list (pattern-var-name pattern) expr))
     (list (tc:lisp-type expr-type env))))

  (:method ((pattern pattern-wildcard) expr expr-type env)
    (declare (ignore pattern expr expr-type env))
    (values `t nil nil))

  (:method ((pattern pattern-literal) expr expr-type env)
    (declare (ignore expr-type env))
    (values
     (if (stringp (pattern-literal-value pattern))
         `(string= ,(pattern-literal-value pattern) ,expr)
         `(eql ,(pattern-literal-value pattern) ,expr))
     nil
     nil))

  (:method ((pattern pattern-constructor) expr expr-type env)
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
          nil
          nil))

        ;; For newtypes we can just codegen patterns for the inner type
        ((tc:type-entry-newtype type)
         (unless (= 1 (length (pattern-constructor-patterns pattern)))
           (util:coalton-bug "Unexpected number of fields in newtype pattern.~%    Expected: 1~%    Received: ~A~%"
                             (length (pattern-constructor-patterns pattern))))
         (codegen-pattern (first (pattern-constructor-patterns pattern)) expr expr-type env))

        ;; Check cons directly
        ((eql (pattern-constructor-name pattern) 'coalton:Cons)

         (multiple-value-bind (car-pred car-bindings car-types)
             (codegen-pattern (first (pattern-constructor-patterns pattern))
                              `(car ,expr)
                              (tc:tapp-to expr-type)
                              env)
           (multiple-value-bind (cdr-pred cdr-bindings cdr-types)
               (codegen-pattern (second (pattern-constructor-patterns pattern))
                                `(cdr ,expr)
                                expr-type
                                env)
             (values
              `(and (cl:consp ,expr)
                    ,car-pred
                    ,cdr-pred)
              (append car-bindings cdr-bindings)
              (append car-types cdr-types)))))

        ;; Check nil directly
        ((eql (pattern-constructor-name pattern) 'coalton:Nil)
         (values
          `(null ,expr)
          nil
          nil))

        ;; Otherwise we need to bring out the big guns
        (t
         (let* ((ctor-name (pattern-constructor-name pattern))
                (ctor (tc:lookup-constructor env ctor-name))
                (generic-ctor-type (tc:qualified-ty-type
                                    (tc:fresh-inst
                                     (tc:lookup-value-type env ctor-name))))
                (ctor-type (tc:apply-substitution
                            (tc:match (tc:function-return-type generic-ctor-type) expr-type)
                            generic-ctor-type))
                (lisp-type-string (format nil "~A/~A" (tc:constructor-entry-constructs ctor) ctor-name)))
           (multiple-value-bind (preds bindings types)
               (loop :with preds := nil
                     :with bindings := nil
                     :with types := nil

                     :for field :in (pattern-constructor-patterns pattern)
                     :for i :from 0

                     :for accessor := (alexandria:format-symbol package "~A-_~D" lisp-type-string i)
                     :for field-expr-type :in (tc:function-type-arguments ctor-type)

                     :do (multiple-value-bind (field-pred field-bindings field-types)
                             (codegen-pattern
                              field
                              `(,accessor ,expr)
                              field-expr-type
                              env)
                           (push field-pred preds)
                           (push field-bindings bindings)
                           (push field-types types))
                     :finally (return (values (nreverse preds)
                                              (nreverse bindings)
                                              (nreverse types))))

             (values
              `(and
                (typep ,expr ',(alexandria:format-symbol package "~A" lisp-type-string))
                ,@preds)
              (reduce #'append bindings)
              (reduce #'append types)))))))))
