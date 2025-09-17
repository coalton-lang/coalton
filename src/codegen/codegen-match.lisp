(defpackage #:coalton-impl/codegen/codegen-match
  (:use #:cl)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:settings #:coalton-impl/settings)
   (#:ast #:coalton-impl/codegen/ast)
   (#:pattern #:coalton-impl/codegen/pattern)
   (#:codegen-pattern #:coalton-impl/codegen/codegen-pattern))
  (:export
   #:match-emit-jumptable-p
   #:match-emit-fallback-p
   #:match-emit-branchless-p
   #:codegen-branch
   #:codegen-cond-match
   #:codegen-case-match))

(in-package #:coalton-impl/codegen/codegen-match)

;;;
;;; Optimization Predicates
;;;

(defun match-exhaustive-p (expr env)
  (declare (type ast:node-match expr)
           (type tc:environment env)
           (values t &optional))

  (pattern:patterns-exhaustive-p
   (mapcar #'ast:match-branch-pattern (ast:node-match-branches expr))
   (ast:node-type (ast:node-match-expr expr))
   env))

(defun match-has-catch-all-p (expr)
  (declare (type ast:node-match expr)
           (values t &optional))

  (member-if (lambda (pat)
               (or (pattern:pattern-wildcard-p pat)
                   (pattern:pattern-var-p pat)))
             (ast:node-match-branches expr)
             :key #'ast:match-branch-pattern))

;; TODO: emit `case' on numbers, symbols, and characters
(defun match-emit-jumptable-p (expr env)
  "Emit a `case' instead of a `cond' when matching on an enum type."
  (declare (type ast:node-match expr)
           (type tc:environment env)
           (values t &optional))

  (let ((ty (ast:node-type (ast:node-match-expr expr)))) 
    (and (tc:tycon-p ty)
         (tc:type-entry-enum-repr
          (tc:lookup-type env (tc:tycon-name ty))))))

(defun match-emit-fallback-p (expr env)
  "Emit a fallback branch when there is no catch-all branch or if
the settings demand it."
  (declare (type ast:node-match expr)
           (type tc:environment env)
           (values t &optional))

  (let ((exhaustivep (match-exhaustive-p expr env))) 
    (not
     (or (match-has-catch-all-p expr)
         (and (settings:coalton-release-p)
              exhaustivep)
         (and (match-emit-jumptable-p expr env)
              exhaustivep)))))

(defun match-emit-branchless-p (expr env)
  "Emit no conditional branching in cases where match is not used
for control flow."
  (declare (type ast:node-match expr)
           (type tc:environment env)
           (values t &optional))

  (let ((one-branch-p (= 1 (length (ast:node-match-branches expr)))))
    (and one-branch-p 
         (or
          ;; Case #1: Trivial cases of
          ;;  (match x (_ y))
          ;;  (match x (var y))
          (match-has-catch-all-p expr)
          ;; Case #2: An exhaustive one-branch case.
          ;;  (match x (PAT y))
          (and (settings:coalton-release-p)
               (match-exhaustive-p expr env))))))

;;;
;;; Codegen Functions
;;;

(defun codegen-cond-branch (expr pattern match-var match-expr-type env)
  "Generate code for a `cond' branch."
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

(defun codegen-case-branch (expr pattern match-var match-expr-type env)
  "Generate code for a `case' branch."
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
       `((,(pattern:pattern-literal-value pattern))
         ,expr))
      ((pattern:pattern-constructor-p pattern)
       (let* ((name (pattern:pattern-constructor-name pattern))
              (entry (tc:lookup-constructor env name)))
         `((,(tc:constructor-entry-compressed-repr entry))
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

(defun codegen-branch (expr pattern match-var match-expr-type env jumptablep)
  (declare (type t expr)
           (type pattern:pattern pattern)
           (type symbol match-var)
           (type tc:ty match-expr-type)
           (type tc:environment env)
           (type boolean jumptablep)
           (values t &optional))

  (funcall (if jumptablep #'codegen-case-branch #'codegen-cond-branch)
           expr
           pattern
           match-var
           match-expr-type
           env))

(defun codegen-cond-fallback ()
  "Generate code for a `cond' fallback."
  (declare (values t &optional))

  `(t
    (error "Pattern match not exhaustive error.")))

(defun codegen-case-fallback ()
  "Generate code for a `case' fallback."
  (declare (values t &optional))

  `(otherwise
    (error "Pattern match not exhaustive error.")))

(defun codegen-cond-match (branches fallbackp)
  (declare (values t &optional))

  `(cond
     ,@branches
     ,@(if fallbackp
           (list (codegen-cond-fallback))
           '())))

(defun codegen-case-match (match-var branches fallbackp)
  (declare (values t &optional))

  `(case ,match-var
     ,@branches
     ,@(if fallbackp
           (list (codegen-case-fallback))
           '())))
