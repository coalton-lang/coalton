(defpackage #:coalton-impl/analysis/analysis
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/analysis/pattern-exhaustiveness)
  (:import-from
   #:coalton-impl/analysis/unused-variables
   #:find-unused-variables)
  (:import-from
   #:coalton-impl/analysis/underapplied-values
   #:find-underapplied-values)
  (:import-from
   #:coalton-impl/analysis/discarded-results
   #:find-discarded-results)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:analyze-translation-unit))

(in-package #:coalton-impl/analysis/analysis)

(defun check-pattern-exhaustiveness (pattern input-type env)
  (declare (type tc:pattern pattern)
           (type tc:ty input-type)
           (type tc:environment env))
  (let* ((pattern (collapse-binding-patterns pattern))
         (missing (find-non-matching-value (list (list pattern)) (list input-type) env)))
    (unless (eq t missing)
      (tc-error "Non-exhaustive match"
                (source:secondary-note pattern "missing case ~w"
                                       (print-pattern (first missing)))))))

(defun check-parameter-exhaustiveness (patterns function-type env)
  (when patterns
    (let ((inputs (tc:function-type-arguments (tc:qualified-ty-type function-type))))
      (cond ((null (rest patterns))
             (check-pattern-exhaustiveness (first patterns) (first inputs) env))
            (t
             (let ((missing (find-non-matching-value (list (mapcar #'collapse-binding-patterns patterns))
                                                     inputs env)))
               (unless (eq t missing)
                 (tc-error "Non-exhaustive match"
                           (source:secondary-note (first patterns) "missing arguments ~w"
                                                  (mapcar #'print-pattern missing))))))))))

(defun analyze-translation-unit (translation-unit env)
  "Perform analysis passes on TRANSLATION-UNIT, potentially producing errors or warnings."
  (declare (type tc:translation-unit translation-unit)
           (type tc:environment env))
  (flet ((analysis-match-traversal (node)
           (let ((scrutinee-type (tc:qualified-ty-type (tc:node-type
                                                        (tc:node-match-expr node))))
                 (patterns
                   ;; for exhaustiveness checking, we can collapse
                   ;; binding patterns, (@ VAR PAT) is part of an
                   ;; exhaustive match expression iff PAT is.
                   (loop :for branch :in (tc:node-match-branches node)
                         :for pat := (tc:node-match-branch-pattern branch)
                         :collect (collapse-binding-patterns pat))))
             (loop :for pattern :in patterns
                   :do (check-for-var-matching-constructor pattern env))

             (let ((exhaustive-or-missing
                     (find-non-matching-value (mapcar #'list patterns)
                                              (list scrutinee-type)
                                              env)))
               (unless (eq t exhaustive-or-missing)
                 (let ((head-note (source:note node "non-exhaustive match"))
                       (tail-notes (if (first exhaustive-or-missing)
                                       (list (source:secondary-note
                                              node
                                              "missing case ~w"
                                              (print-pattern (first exhaustive-or-missing))))
                                       nil)))
                   (apply #'source:warn "non-exhaustive match" (list* head-note tail-notes))))
               (loop :for pattern :in patterns
                     :unless (useful-pattern-p patterns pattern env scrutinee-type) :do
                       (source:warn "Useless match case"
                                    (source:note pattern "useless match case")
                                    (source:note node "in this match"))))))

         (analysis-abstraction-traversal (node)
           (declare (type tc:node-abstraction node))
           (check-parameter-exhaustiveness (tc:node-abstraction-params node) (tc:node-type node) env))
         (analysis-bind-traversal (node)
           (declare (type tc:node-bind node))
           (check-pattern-exhaustiveness (tc:node-bind-pattern node)
                                         (tc:qualified-ty-type (tc:node-type (tc:node-bind-expr node)))
                                         env)))

    (let ((analysis-traverse-block
            (tc:make-traverse-block
             :match #'analysis-match-traversal
             :abstraction #'analysis-abstraction-traversal
             :bind #'analysis-bind-traversal)))

      ;; Run analysis on definitions
      (loop :for define :in (tc:translation-unit-definitions translation-unit)
            :do (tc:visit (tc:toplevel-define-body define) analysis-traverse-block)
            :do (find-unused-variables define)
            :do (find-underapplied-values define)
            :do (find-discarded-results define)
            :do (check-parameter-exhaustiveness (tc:binding-parameters define) (tc:binding-type define) env))

      ;; Run analysis on instance definitions
      (loop :for instance :in (tc:translation-unit-instances translation-unit) :do
        (loop :for method :being :the :hash-value :of (tc:toplevel-define-instance-methods instance)
              :do (tc:visit (tc:instance-method-definition-body method) analysis-traverse-block)
              :do (find-underapplied-values method)
              :do (find-discarded-results method)
              :do (check-parameter-exhaustiveness (tc:binding-parameters method) (tc:binding-type method) env))))))

(defgeneric print-pattern (pat)
  (:method ((pat tc:pattern-constructor))
    (cons (tc:pattern-constructor-name pat) (mapcar #'print-pattern (tc:pattern-constructor-patterns pat))))

  (:method ((pat tc:pattern-wildcard))
    "_"))

(defgeneric check-for-var-matching-constructor (pat env)
  (:method ((pat tc:pattern-var) env)
    (declare (type tc:environment env))

    (let ((ctor (tc:lookup-constructor env (tc:pattern-var-orig-name pat) :no-error t)))
      (when ctor
        (source:warn "Pattern warning"
                     (source:note pat "pattern variable matches constructor name")))))

  (:method ((pat tc:pattern-literal) env)
    (declare (ignore env)))

  (:method ((pat tc:pattern-wildcard) env)
    (declare (ignore env)))

  (:method ((pat tc:pattern-binding) env)
    (check-for-var-matching-constructor (tc:pattern-binding-var pat) env)
    (check-for-var-matching-constructor (tc:pattern-binding-pattern pat) env))

  (:method ((pat tc:pattern-constructor) env)
    (loop :for pat :in (tc:pattern-constructor-patterns pat)
          :do (check-for-var-matching-constructor pat env))))
