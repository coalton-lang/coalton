(defpackage #:coalton-impl/codegen/codegen-match
  (:use
   #:cl)
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
   #:match-emit-if-p
   #:codegen-match-branch
   #:codegen-match))

(in-package #:coalton-impl/codegen/codegen-match)

;;;
;;; Optimization Predicates
;;;

(defun match-exhaustive-p (match env)
  "Check if match branches cover all cases of the type of the match
subexpression."
  (declare (type ast:node-match match)
           (type tc:environment env)
           (values t &optional))

  (pattern:patterns-exhaustive-p
   (mapcar #'ast:match-branch-pattern (ast:node-match-branches match))
   (ast:node-type (ast:node-match-expr match))
   env))

(defun match-has-catch-all-p (match)
  "Check if match expression has a branch with a wildcard or variable
pattern to catch all inputs."
  (declare (type ast:node-match match)
           (values t &optional))

  (member-if (lambda (pat)
               (or (pattern:pattern-wildcard-p pat)
                   (pattern:pattern-var-p pat)))
             (ast:node-match-branches match)
             :key #'ast:match-branch-pattern))

;; TODO: emit `cl:case' on numbers, symbols, and characters
(defun match-emit-jumptable-p (match env)
  "Emit a `cl:case' instead of a `cl:cond' when matching on an enum type."
  (declare (type ast:node-match match)
           (type tc:environment env)
           (values t &optional))

  (let ((ty (ast:node-type (ast:node-match-expr match))))
    (and (tc:tycon-p ty)
         (tc:type-entry-enum-repr
          (tc:lookup-type env (tc:tycon-name ty))))))

(defun match-emit-fallback-p (match env)
  "Emit a fallback branch when there is no catch-all branch or if
the settings demand it."
  (declare (type ast:node-match match)
           (type tc:environment env)
           (values t &optional))

  (let ((exhaustivep (match-exhaustive-p match env)))
    (not
     (or (match-has-catch-all-p match)
         (and (settings:coalton-release-p)
              exhaustivep)
         (and (match-emit-jumptable-p match env)
              exhaustivep)))))

(defun match-emit-branchless-p (match env)
  "Emit no conditional branching in cases where match is not used
for control flow."
  (declare (type ast:node-match match)
           (type tc:environment env)
           (values t &optional))

  (let ((one-branch-p (= 1 (length (ast:node-match-branches match)))))
    (and one-branch-p
         (or
          ;; Case #1:
          ;;
          ;; Trivial cases of
          ;;  (match x (_ y))
          ;;  (match x (var y))
          (match-has-catch-all-p match)

          ;; Case #2:
          ;;
          ;; An exhaustive one-branch case
          ;;  (match x (PAT y))
          (and (settings:coalton-release-p)
               (match-exhaustive-p match env))))))

(defun match-emit-if-p (match)
  "Emit match as `cl:if' in cases where the subexpression is a `Boolean'
and branches exhaustively pattern match with constructor branches.

When true, returns two `ast:node' objects representing then/else branches."
  (declare (type ast:node-match match)
           (values boolean (or null ast:node) (or null ast:node) &optional))

  (let* ((branches (ast:node-match-branches match))
         (true-pattern
           (pattern:make-pattern-constructor :type tc:*boolean-type*
                                             :name 'coalton:True
                                             :patterns nil))
         (false-pattern
           (pattern:make-pattern-constructor :type tc:*boolean-type*
                                             :name 'coalton:False
                                             :patterns nil)))

    (cond
      ;; Case #1:
      ;;
      ;; Do not emit `cl:if' unless matching on a boolean with two branches.
      ((not (and
             (equalp (ast:node-type (ast:node-match-expr match)) tc:*boolean-type*)
             (= 2 (length branches))))
       (values nil nil nil))

      ;; Case #2:
      ;;
      ;; Emit `cl:if' when first branch is `True' and second branch is `False'
      ((and (equalp true-pattern (ast:match-branch-pattern (first branches)))
            (equalp false-pattern (ast:match-branch-pattern (second branches))))
       (values t
               (ast:match-branch-body (first branches))
               (ast:match-branch-body (second branches))))

      ;; Case #3:
      ;;
      ;; Emit `cl:if' when second branch is `True' and first branch is `False'
      ((and (equalp true-pattern (ast:match-branch-pattern (second branches)))
            (equalp false-pattern (ast:match-branch-pattern (first branches))))
       (values t
               (ast:match-branch-body (second branches))
               (ast:match-branch-body (first branches))))

      ;; Case #4:
      ;;
      ;; Do not emit `cl:if' unless exhaustively matching boolean with
      ;; constructor pattern branches.
      (t
       (values nil nil nil)))))

;;;
;;; Codegen Functions
;;;

(defun codegen-binding-types (bindings types)
  "Generate type declarations for match bindings."
  (declare (type list bindings)
           (type t types)
           (values t &optional))

  `(declare
    (ignorable ,@(mapcar #'car bindings))
    ,@(if (not settings:*emit-type-annotations*)
          nil
          (loop :for binding :in bindings
                :for var := (car binding)
                :for type :in types
                :collect `(type ,type ,var)))))

(defun codegen-cond-match-branch (code pattern match-var match-expr-type env)
  "Generate code for a `cl:cond' branch."
  (declare (type t code)
           (type pattern:pattern pattern)
           (type symbol match-var)
           (type tc:ty match-expr-type)
           (type tc:environment env)
           (values t &optional))

  (multiple-value-bind (pred bindings types)
      (codegen-pattern:codegen-pattern pattern match-var match-expr-type env)

    `(,pred
      ,(if (null bindings)
           code
           `(let ,bindings
              ,(codegen-binding-types bindings types)
              ,code)))))

(defun codegen-case-match-branch (code pattern match-var match-expr-type env)
  "Generate code for a `cl:case' branch."
  (declare (type t code)
           (type pattern:pattern pattern)
           (type symbol match-var)
           (type tc:ty match-expr-type)
           (type tc:environment env)
           (values t &optional))

  (multiple-value-bind (pred bindings types)
      (codegen-pattern:codegen-pattern pattern match-var match-expr-type env)
    (declare (ignore pred))

    (cond
      ;; Case #1:
      ;;
      ;; Emit a list of a single literal pattern to match on.
      ((pattern:pattern-literal-p pattern)
       `((,(pattern:pattern-literal-value pattern))
         ,code))

      ;; Case #2:
      ;;
      ;; Emit a list of a single enum symbol to match on.
      ((pattern:pattern-constructor-p pattern)
       (let* ((name (pattern:pattern-constructor-name pattern))
              (entry (tc:lookup-constructor env name)))
         `((,(tc:constructor-entry-compressed-repr entry))
           ,code)))

      ;; Case #3:
      ;;
      ;; Emit a variable or wildcard binding in a catch-all branch.
      (t
       `(otherwise
         (let ,bindings
           ,(codegen-binding-types bindings types)
           ,code))))))

(defun codegen-match-branch (code pattern match-var match-expr-type env jumptablep)
  "Generate code for a match branch."
  (declare (type t code)
           (type pattern:pattern pattern)
           (type symbol match-var)
           (type tc:ty match-expr-type)
           (type tc:environment env)
           (type boolean jumptablep)
           (values t &optional))

  (funcall (if jumptablep
               #'codegen-case-match-branch
               #'codegen-cond-match-branch)
           code
           pattern
           match-var
           match-expr-type
           env))

(defun codegen-cond-match (branches fallbackp)
  "Generate code to switch branches with `cl:cond'."
  (declare (type list branches)
           (values t &optional))

  `(cond
     ,@branches
     ,@(if (not fallbackp)
           '()
           (list '(t (error "Pattern match not exhaustive error."))))))

(defun codegen-case-match (match-var branches fallbackp)
  "Generate code to switch branches with `cl:case'."
  (declare (type symbol match-var)
           (type list branches)
           (values t &optional))

  `(case ,match-var
     ,@branches
     ,@(if (not fallbackp)
           '()
           (list '(otherwise (error "Pattern match not exhaustive error."))))))

(defun codegen-match (match-var subexpr-code subexpr-type branches env jumptablep fallbackp branchlessp)
  "Generate code for a match expression."
  (declare (type symbol match-var)
           (type t subexpr-code)
           (type tc:ty subexpr-type)
           (type list branches)
           (type tc:environment env)
           (type t jumptablep)
           (type t fallbackp)
           (type t branchlessp)
           (values t &optional))

  `(let ((,match-var ,subexpr-code))
     (declare ,@(list*
                 `(ignorable ,match-var)
                 (if (not settings:*emit-type-annotations*)
                     '()
                     (list `(type ,(tc:lisp-type subexpr-type env) ,match-var)))))

     (locally
         #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
         ,(cond
            ;; Case #1:
            ;;
            ;; When match is not being used for control flow, don't
            ;; codegen `cond' or `case'.
            (branchlessp
             (destructuring-bind ((cond-test cond-result)) branches
               (declare (ignore cond-test))
               cond-result))

            ;; Case #2:
            ;;
            ;; When matching on a type we can use `cl:eql' on can be
            ;; simplified to a jumptable using `cl:case'.
            ;; Currently this is only applied to types declared as enum.
            (jumptablep
             (codegen-case-match match-var branches fallbackp))

            ;; Case #3:
            ;;
            ;; Default to generating a `cl:cond' expression.
            (t
             (codegen-cond-match branches fallbackp))))))
