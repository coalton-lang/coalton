(defpackage #:coalton-impl/typechecker2/base
  (:use
   #:cl)
  (:import-from
   #:coalton-impl/parser/base
   #:coalton-error
   #:make-coalton-error-note
   #:make-coalton-error-help)
  (:export
   #:coalton-error
   #:make-coalton-error-note
   #:make-coalton-error-help)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser))
  (:export
   #:tc-error                           ; CONDITION
   #:collect-variables                  ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker2/base)

(define-condition tc-error (error)
  ((err :reader tc-error-err
        :initarg :err
        :type parser:coalton-error))
  (:report (lambda (c s)
             (parser:display-coalton-error s (tc-error-err c)))))

(defun collect-variables (node)
  "Returns a deduplicated list of all `PARSER:NODE-VARIABLE's referenced
in expressions. May not include all bound variables."
  (declare (type t node)
           (values parser:node-variable-list &optional))

  (delete-duplicates (collect-variables-generic% node) :test #'eq))

(defgeneric collect-variables-generic% (node)
  (:method ((node parser:node-variable))
    (declare (values parser:node-variable-list))
    (list node))

  (:method ((node parser:node-literal))
    (declare (values parser:node-variable-list))
    nil)

  (:method ((node parser:node-integer-literal))
    (declare (values parser:node-variable-list))
    nil)

  (:method ((node parser:node-bind))
    (declare (values parser:node-variable-list))
    (nconc
     (collect-variables-generic% (parser:node-bind-pattern node))
     (collect-variables-generic% (parser:node-bind-expr node))))

  (:method ((node parser:node-body))
    (declare (values parser:node-variable-list &optional))
    (collect-variables-generic% (parser:node-body-last-node node)))

  (:method ((node parser:node-abstraction))
    (declare (values parser:node-variable-list &optional))
    (collect-variables-generic% (parser:node-abstraction-body node)))

  (:method ((node parser:node-let-binding))
    (declare (values parser:node-variable-list &optional))
    (collect-variables-generic% (parser:node-let-binding-value node)))

  (:method ((node parser:node-let))
    (declare (values parser:node-variable-list))
    (nconc
     (mapcan #'collect-variables-generic% (parser:node-let-bindings node))
     (collect-variables-generic% (parser:node-let-body node))))

  (:method ((node parser:node-lisp))
    (declare (values parser:node-variable-list))
    (copy-list (parser:node-lisp-vars node)))

  (:method ((node parser:node-match-branch))
    (declare (values parser:node-variable-list &optional))
    (collect-variables-generic% (parser:node-match-branch-body node)))

  (:method ((node parser:node-match))
    (declare (values parser:node-variable-list))
    (nconc
     (collect-variables-generic% (parser:node-match-expr node))
     (mapcan #'collect-variables-generic% (parser:node-match-branches node))))

  (:method ((node parser:node-progn))
    (declare (values parser:node-variable-list &optional))
    (collect-variables-generic% (parser:node-progn-body node)))

  (:method ((node parser:node-the))
    (declare (values parser:node-variable-list &optional))
    (collect-variables-generic% (parser:node-the-expr node)))

  (:method ((node parser:node-return))
    (declare (values parser:node-variable-list &optional))
    (collect-variables-generic% (parser:node-return-expr node)))

  (:method ((node parser:node-application))
    (declare (values parser:node-variable-list &optional))
    (nconc
     (collect-variables-generic% (parser:node-application-rator node))
     (mapcan #'collect-variables-generic% (parser:node-application-rands node))))

  (:method ((node parser:node-or))
    (declare (values parser:node-variable-list))
    (mapcan #'collect-variables-generic% (parser:node-or-nodes node)))

  (:method ((node parser:node-and))
    (declare (values parser:node-and))
    (mapcan #'collect-variables-generic% (parser:node-and-nodes node)))

  (:method ((node parser:node-if))
    (declare (values parser:node-variable-list))
    (nconc
     (collect-variables-generic% (parser:node-if-expr node))
     (collect-variables-generic% (parser:node-if-then node))
     (collect-variables-generic% (parser:node-if-else node))))

  (:method ((node parser:node-when))
    (declare (values parser:node-variable-list))
    (nconc
     (parser:node-when-expr node)
     (parser:node-when-body node)))

  (:method ((node parser:node-unless))
    (declare (values parser:node-variable-list))
    (nconc
     (parser:node-unless-expr node)
     (parser:node-unless-body node)))

  (:method ((node parser:node-cond-clause))
    (declare (values parser:node-cond-clause))
    (nconc
     (collect-variables-generic% (parser:node-cond-clause-expr node))
     (collect-variables-generic% (parser:node-cond-clause-body node))))

  (:method ((node parser:node-cond))
    (declare (values parser:node-variable-list))
    (mapcan #'collect-variables-generic% (parser:node-cond-clauses node)))

  (:method ((node parser:node-do-bind))
    (declare (values parser:node-variable-list &optional))
    (collect-variables-generic% (parser:node-do-bind-expr node)))

  (:method ((node parser:node-do))
    (declare (values parser:node-variable-list))
    (nconc
     (mapcan #'collect-variables-generic% (parser:node-do-nodes node))
     (collect-variables-generic% (parser:node-do-last-node node)))))
