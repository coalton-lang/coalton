(defpackage #:coalton-impl/analysis/analysis
  (:use
   #:cl
   #:coalton-impl/analysis/pattern-exhaustiveness)
  (:import-from
   #:coalton-impl/analysis/unused-variables
   #:find-unused-variables)
  (:import-from
   #:coalton-impl/analysis/underapplied-values
   #:find-underapplied-values)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:analyze-translation-unit))

(in-package #:coalton-impl/analysis/analysis)

(define-condition non-exhaustive-match-warning (error:coalton-base-warning)
  ())

(define-condition useless-pattern-warning (error:coalton-base-warning)
  ())

(define-condition pattern-var-matches-constructor (error:coalton-base-warning)
  ())

(defun check-pattern-exhaustiveness (pattern env file)
  (declare (type tc:pattern pattern)
           (type tc:environment env)
           (type error:coalton-file file))

  (let ((missing (find-non-matching-value (list (list pattern)) 1 env)))
    (unless (eq t missing)
      (error 'tc:tc-error
             :err (error:coalton-error
                   :file file
                   :span (tc:pattern-source pattern)
                   :message "Non-exaustive match"
                   :primary-note (format nil "Missing case ~W"
                                         (print-pattern (first missing))))))))

(defun analyze-translation-unit (translation-unit env file)
  "Perform analysis passes on TRANSLATION-UNIT, potentially producing errors or warnings."
  (declare (type tc:translation-unit translation-unit)
           (type tc:environment env)
           (type error:coalton-file file))

  (let ((analysis-traverse-block
          (tc:make-traverse-block
           :match (lambda (node)
                    (let ((patterns (mapcar #'tc:node-match-branch-pattern (tc:node-match-branches node))))
                      (loop :for pattern :in patterns
                            :do (check-for-var-matching-constructor pattern env file))
                      
                      (let ((exhaustive-or-missing
                              (find-non-matching-value (mapcar #'list patterns) 1 env)))
                        (unless (eq t exhaustive-or-missing)
                          (warn 'non-exhaustive-match-warning
                                :err (error:coalton-error
                                      :type :warn
                                      :file file
                                      :span (tc:node-source node)
                                      :message "Non-exhaustive match"
                                      :primary-note "non-exaustive match"
                                      :notes (when (first exhaustive-or-missing)
                                               (list
                                                (error:make-coalton-error-note
                                                 :type :secondary
                                                 :span (tc:node-source node)
                                                 :message (format nil "Missing case ~W"
                                                                  (print-pattern (first exhaustive-or-missing)))))))))
                        (loop :for pattern :in patterns
                              :unless (useful-pattern-p patterns pattern env) :do
                                (warn 'useless-pattern-warning
                                      :err (error:coalton-error
                                            :type :warn
                                            :file file
                                            :span (tc:pattern-source pattern)
                                            :message "Useless match case"
                                            :primary-note "useless match case"
                                            :notes
                                            (list
                                             (error:make-coalton-error-note
                                              :type :secondary
                                              :span (tc:node-source node)
                                              :message "in this match")))))))
                    node)
           :abstraction (lambda (node)
                          (declare (type tc:node-abstraction node))
                          (loop :for pattern :in (tc:node-abstraction-params node)
                                :do (check-pattern-exhaustiveness pattern env file))
                          node)
           :bind (lambda (node)
                   (declare (type tc:node-bind node))
                   (check-pattern-exhaustiveness (tc:node-bind-pattern node) env file)
                   node))))

    ;; Run analysis on definitions
    (loop :for define :in (tc:translation-unit-definitions translation-unit)
          :do (tc:traverse (tc:toplevel-define-body define) analysis-traverse-block)
          :do (find-unused-variables define file)
          :do (find-underapplied-values define file)
          :do (loop :for pattern :in (tc:binding-parameters define)
                    :do (check-pattern-exhaustiveness pattern env file)))

    ;; Run analysis on instance definitions
    (loop :for instance :in (tc:translation-unit-instances translation-unit) :do
      (loop :for method :being :the :hash-value :of (tc:toplevel-define-instance-methods instance)
            :do (tc:traverse (tc:instance-method-definition-body method) analysis-traverse-block)
            :do (find-underapplied-values method file)
            :do (find-underapplied-values method file)
            :do (loop :for pattern :in (tc:binding-parameters method)
                      :do (check-pattern-exhaustiveness pattern env file))))))

(defgeneric print-pattern (pat)
  (:method ((pat tc:pattern-constructor))
    (cons (tc:pattern-constructor-name pat) (mapcar #'print-pattern (tc:pattern-constructor-patterns pat))))

  (:method ((pat tc:pattern-wildcard))
    "_"))

(defgeneric check-for-var-matching-constructor (pat env file)
  (:method ((pat tc:pattern-var) env file)
    (declare (type tc:environment env)
             (type error:coalton-file file))

    (let ((ctor (tc:lookup-constructor env (tc:pattern-var-orig-name pat) :no-error t)))
      (when ctor
        (warn 'pattern-var-matches-constructor
              :err (error:coalton-error
                    :type :warn
                    :file file
                    :span (tc:pattern-source pat)
                    :message "Pattern warning"
                    :primary-note "pattern variable matches constructor name")))))

  (:method ((pat tc:pattern-literal) env file)
    (declare (ignore env file)))

  (:method ((pat tc:pattern-wildcard) env file)
    (declare (ignore env file)))

  (:method ((pat tc:pattern-constructor) env file)
    (loop :for pat :in (tc:pattern-constructor-patterns pat)
          :do (check-for-var-matching-constructor pat env file))))
