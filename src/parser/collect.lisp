;;;;
;;;; Generic traversals for collecting lists of items referenced from
;;;; within a nested AST structure.
;;;;

(defpackage #:coalton-impl/parser/collect
  (:use
   #:cl
   #:coalton-impl/parser/base
   #:coalton-impl/parser/types
   #:coalton-impl/parser/expression
   #:coalton-impl/parser/toplevel)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:export
   #:collect-referenced-types           ; FUNCTION
   #:collect-type-variables             ; FUNCTION
   #:collect-variables                  ; FUNCTION
   #:collect-referenced-classes         ; FUNCTION
   ))

(in-package #:coalton-impl/parser/collect)

(defun collect-referenced-types (type)
  "Returns a deduplicated list of all `TYCON's in TYPE."
  (declare (type t type)
           (values tycon-list))
  (delete-duplicates (collect-referenced-types-generic% type) :test #'eq))

(defgeneric collect-referenced-types-generic% (type)
  (:method ((type tyvar))
    (declare (values tycon-list))
    nil)

  (:method ((type tycon))
    (declare (values tycon-list))
    (list type))

  (:method ((type tapp))
    (declare (values tycon-list))
    (nconc (collect-referenced-types-generic% (tapp-from type))
           (collect-referenced-types-generic% (tapp-to type))))

  (:method ((pred ty-predicate))
    (declare (values tycon-list))
    (mapcan #'collect-referenced-types-generic% (ty-predicate-types pred)))

  (:method ((type qualified-ty))
    (declare (values tycon-list))
    (nconc
     (collect-referenced-types-generic% (qualified-ty-type type))
     (mapcan #'collect-referenced-types-generic% (qualified-ty-predicates type))))

  (:method ((ctor constructor))
    (declare (values tycon-list))
    (mapcan #'collect-referenced-types-generic% (constructor-fields ctor)))

  (:method ((type toplevel-define-type))
    (declare (values tycon-list))
    (mapcan #'collect-referenced-types-generic% (toplevel-define-type-ctors type)))

  (:method ((alias toplevel-define-alias))
    (collect-referenced-types-generic% (toplevel-define-alias-base-type alias)))

  (:method ((field struct-field))
    (declare (values tycon-list &optional))
    (collect-referenced-types-generic% (struct-field-type field)))

  (:method ((type toplevel-define-struct))
    (declare (values tycon-list))
    (mapcan #'collect-referenced-types-generic% (toplevel-define-struct-fields type))))

(defun collect-type-variables (type)
  "Returns a deduplicated list of all `TYVAR's in TYPE."
  (declare (type t type)
           (values tyvar-list))
  (delete-duplicates (collect-type-variables-generic% type) :test #'eq))

(defgeneric collect-type-variables-generic% (type)
  (:method ((type tyvar))
    (declare (values tyvar-list))
    (list type))

  (:method ((type tycon))
    (declare (values tyvar-list))
    nil)

  (:method ((type tapp))
    (declare (values tyvar-list))
    (nconc (collect-type-variables-generic% (tapp-from type))
           (collect-type-variables-generic% (tapp-to type))))

  (:method ((pred ty-predicate))
    (declare (values tyvar-list))
    (mapcan #'collect-type-variables-generic% (ty-predicate-types pred)))

  (:method ((type qualified-ty))
    (declare (values tyvar-list))
    (nconc
     (collect-type-variables-generic% (qualified-ty-type type))
     (mapcan #'collect-type-variables-generic% (qualified-ty-predicates type))))

  (:method ((ctor constructor))
    (declare (values tyvar-list))
    (mapcan #'collect-type-variables-generic% (constructor-fields ctor)))

  (:method ((type toplevel-define-type))
    (declare (values tyvar-list))
    (mapcan #'collect-type-variables-generic% (toplevel-define-type-ctors type)))

  (:method ((method method-definition))
    (declare (values tyvar-list &optional))
    (collect-type-variables-generic% (method-definition-type method)))

  (:method ((list list))
    (declare (values tyvar-list &optional))
    (mapcan #'collect-type-variables-generic% list)))

(defun collect-variables (node)
  "Returns a deduplicated list of all `NODE-VARIABLE's referenced
in expressions. May not include all bound variables."
  (declare (type t node)
           (values node-variable-list &optional))

  (delete-duplicates (collect-variables-generic% node) :test #'eq))

(defgeneric collect-variables-generic% (node)
  (:method ((node node-variable))
    (declare (values node-variable-list))
    (list node))

  (:method ((node node-accessor))
    (declare (values node-variable-list))
    nil)

  (:method ((node node-literal))
    (declare (values node-variable-list))
    nil)

  (:method ((node node-integer-literal))
    (declare (values node-variable-list))
    nil)

  (:method ((node node-bind))
    (declare (values node-variable-list &optional))
    (collect-variables-generic% (node-bind-expr node)))

  (:method ((node node-body))
    (declare (values node-variable-list &optional))
    (nconc
     (mapcan #'collect-variables-generic% (node-body-nodes node))
     (collect-variables-generic% (node-body-last-node node))))

  (:method ((node node-abstraction))
    (declare (values node-variable-list &optional))
    (collect-variables-generic% (node-abstraction-body node)))

  (:method ((node node-let-binding))
    (declare (values node-variable-list &optional))
    (collect-variables-generic% (node-let-binding-value node)))

  (:method ((node node-let))
    (declare (values node-variable-list))
    (nconc
     (mapcan #'collect-variables-generic% (node-let-bindings node))
     (collect-variables-generic% (node-let-body node))))

  (:method ((node node-lisp))
    (declare (values node-variable-list))
    (copy-list (node-lisp-vars node)))

  (:method ((node node-match-branch))
    (declare (values node-variable-list &optional))
    (collect-variables-generic% (node-match-branch-body node)))

  (:method ((node node-match))
    (declare (values node-variable-list))
    (nconc
     (collect-variables-generic% (node-match-expr node))
     (mapcan #'collect-variables-generic% (node-match-branches node))))

  (:method ((node node-progn))
    (declare (values node-variable-list &optional))
    (collect-variables-generic% (node-progn-body node)))

  (:method ((node node-the))
    (declare (values node-variable-list &optional))
    (collect-variables-generic% (node-the-expr node)))

  (:method ((node node-return))
    (declare (values node-variable-list &optional))
    (collect-variables-generic% (node-return-expr node)))

  (:method ((node node-application))
    (declare (values node-variable-list &optional))
    (nconc
     (collect-variables-generic% (node-application-rator node))
     (mapcan #'collect-variables-generic% (node-application-rands node))))

  (:method ((node node-or))
    (declare (values node-variable-list))
    (mapcan #'collect-variables-generic% (node-or-nodes node)))

  (:method ((node node-and))
    (declare (values node-variable-list))
    (mapcan #'collect-variables-generic% (node-and-nodes node)))

  (:method ((node node-if))
    (declare (values node-variable-list))
    (nconc
     (collect-variables-generic% (node-if-expr node))
     (collect-variables-generic% (node-if-then node))
     (collect-variables-generic% (node-if-else node))))

  (:method ((node node-when))
    (declare (values node-variable-list))
    (nconc
     (collect-variables-generic% (node-when-expr node))
     (collect-variables-generic% (node-when-body node))))

  (:method ((node node-unless))
    (declare (values node-variable-list))
    (nconc
     (collect-variables-generic% (node-unless-expr node))
     (collect-variables-generic% (node-unless-body node))))

  (:method ((node node-cond-clause))
    (declare (values node-variable-list))
    (nconc
     (collect-variables-generic% (node-cond-clause-expr node))
     (collect-variables-generic% (node-cond-clause-body node))))

  (:method ((node node-cond))
    (declare (values node-variable-list))
    (mapcan #'collect-variables-generic% (node-cond-clauses node)))

  (:method ((node node-while))
    (declare (values node-variable-list))
    (nconc (collect-variables-generic% (node-while-expr node))
           (collect-variables-generic% (node-while-body node))))

  (:method ((node node-while-let))
    (declare (values node-variable-list))
    (nconc (collect-variables-generic% (node-while-let-expr node))
           (collect-variables-generic% (node-while-let-body node))))

  (:method ((node node-for))
    (declare (values node-variable-list))
    (nconc (collect-variables-generic% (node-for-expr node))
           (collect-variables-generic% (node-for-body node))))  
  
  (:method ((node node-loop))
    (declare (values node-variable-list))
    (collect-variables-generic% (node-loop-body node)))

  (:method ((node node-break))
    (declare (values node-variable-list &optional))
    nil)

  (:method ((node node-continue))
    (declare (values node-variable-list &optional))
    nil)

  (:method ((node node-do-bind))
    (declare (values node-variable-list &optional))
    (collect-variables-generic% (node-do-bind-expr node)))

  (:method ((node node-do))
    (declare (values node-variable-list))
    (nconc
     (mapcan #'collect-variables-generic% (node-do-nodes node))
     (collect-variables-generic% (node-do-last-node node)))))

(defun collect-referenced-classes (ty)
  "Returns a deduplicated list of all classes referenced in TY."
  (declare (type t ty)
           (values identifier-src-list))
  (delete-duplicates (collect-referenced-classes-generic% ty) :test #'eq))

(defgeneric collect-referenced-classes-generic% (ty)
  (:method ((pred ty-predicate))
    (declare (values identifier-src-list))
    (list (ty-predicate-class pred)))

  (:method ((qual-ty qualified-ty))
    (declare (values identifier-src-list))
    (mapcan #'collect-referenced-classes-generic%
            (qualified-ty-predicates qual-ty)))

  (:method ((method method-definition))
    (declare (values identifier-src-list &optional))
    (collect-referenced-classes-generic% (method-definition-type method)))

  (:method ((toplevel toplevel-define-class))
    (declare (values identifier-src-list))
    (nconc
     (mapcan #'collect-referenced-classes-generic%
             (toplevel-define-class-preds toplevel))
     (mapcan #'collect-referenced-classes-generic%
             (toplevel-define-class-methods toplevel))))

  (:method ((list list))
    (declare (values identifier-src-list))
    (mapcan #'collect-referenced-classes-generic% list)))
