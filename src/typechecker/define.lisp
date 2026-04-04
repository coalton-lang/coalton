;;;
;;;; Type inference for toplevel definitions and expressions. The
;;;; implementation is based on the paper "Typing Haskell in Haskell"
;;;; by Jones.
;;;;
;;;; This implementation makes use of two insights. The first is that
;;;; by renaming all local bindings the names of variables will never
;;;; conflict, so bindings can be stored in a hash table and variable
;;;; shadowing can be ignored entirely. The second is that the "base
;;;; environment" that stores type types of bindings from previous
;;;; compiler invocations will never contain any free variables. This
;;;; allows skipping searching the base environment for type variables
;;;; and applying substitutions to it.
;;;;

(defpackage #:coalton-impl/typechecker/define
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/parse-type
   #:coalton-impl/typechecker/pattern
   #:coalton-impl/typechecker/expression
   #:coalton-impl/typechecker/traverse
   #:coalton-impl/typechecker/toplevel
   #:coalton-impl/typechecker/accessor
   #:coalton-impl/typechecker/tc-env)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker/stage-1)
   (#:type-string #:coalton-impl/typechecker/type-string)
   (#:types #:coalton-impl/typechecker/types)
   (#:redef #:coalton-impl/redef-detection)
   (#:settings #:coalton-impl/settings))
  (:export
   #:infer-expression-type              ; FUNCTION
   #:infer-expl-binging-type            ; FUNCTION
   #:check-bindings-for-invalid-recursion ; FUNCTION
   #:attach-explicit-binding-type       ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/define)

(defstruct (return-block-info
            (:constructor make-return-block-info (&key type first-return)))
  (type (util:required 'type) :type tc:ty              :read-only t)
  (first-return nil            :type (or null cons)    :read-only nil))

(defparameter *return-blocks* nil)

(declaim (type (or null parser:node) *direct-values-node*))
(defparameter *direct-values-node* nil)

(declaim (type (or null tc:ty-list) *direct-values-output-types*))
(defparameter *direct-values-output-types* nil)

(declaim (type (or null tc:environment) *type-string-environment*))
(defparameter *type-string-environment* nil)

(defun %type-string-environment (env)
  (typecase env
    (null
     nil)
    (tc-env
     (tc-env-env env))
    (t
     env)))

(defun parse-the-type-annotation (annotation env)
  (declare (type parser:qualified-ty annotation)
           (type tc-env env)
           (values tc:qualified-ty tc:tyvar-list boolean &optional))
  (multiple-value-bind (qual-ty explicit-tvars explicit-p)
      (parse-qualified-type-info annotation (tc-env-parser-env env))
    (when (tc:qualified-ty-predicates qual-ty)
      (tc-error "Invalid type annotation"
                (tc-note annotation
                         "`the` annotations cannot include predicate contexts")))
    (values qual-ty explicit-tvars explicit-p)))

(defmacro with-type-string-environment ((env) &body body)
  `(let ((*type-string-environment*
           (or *type-string-environment*
               (%type-string-environment ,env))))
     ,@body))

(defun type-object-string (object &optional env)
  (let ((settings:*coalton-print-unicode* nil))
    (type-string:type-to-string object
                                (or (%type-string-environment env)
                                    *type-string-environment*))))

(defun return-block-info (name)
  (declare (type symbol name)
           (values return-block-info &optional))
  (or (cdr (assoc name *return-blocks* :test #'eq))
      (util:coalton-bug "Unknown return target ~S during type inference" name)))

(defun error-ambiguous-pred (pred)
  (declare (type tc:ty-predicate pred))

  (unless (source:location pred)
    (util:coalton-bug "Predicate ~S does not have source information" pred))

  (tc-error "Ambiguous predicate"
            (tc-note pred "Ambiguous predicate ~A"
                     (type-object-string pred))))

(defun error-unknown-pred (pred)
  (declare (type tc:ty-predicate pred))

  (unless (source:location pred)
    (util:coalton-bug "Predicate ~S does not have source information" pred))

  (tc-error "Unknown instance"
            (tc-note pred "Unknown instance ~A"
                     (type-object-string pred))))

(defun standard-expression-type-mismatch-error (node subs expected-type ty)
  "Utility for signalling a type-mismatch error in INFER-EXPRESSION-TYPE"
  (tc-error "Type mismatch"
            (tc-note node "Expected type '~A' but got '~A'"
                     (type-object-string (tc:apply-substitution subs expected-type))
                     (type-object-string (tc:apply-substitution subs ty)))))

(defun required-builder-symbol (package-name symbol-name)
  (or (first (util:find-symbol? symbol-name package-name))
      (util:coalton-bug "Unable to find builder helper symbol ~A::~A"
                        package-name
                        symbol-name)))

(defun make-parser-variable-node (name location)
  (parser:make-node-variable
   :name name
   :location location))

(defun make-parser-integer-literal-node (value location)
  (parser:make-node-integer-literal
   :value value
   :location location))

(defun make-parser-pattern-var (name location)
  (parser:make-pattern-var
   :name name
   :orig-name name
   :location location))

(defun make-parser-body (last-node)
  (parser:make-node-body
   :nodes nil
   :last-node last-node))

(defun make-parser-zero-values-node (location)
  (parser:make-node-values
   :nodes nil
   :location location))

(defun make-parser-application (rator rands location &optional (keyword-rands nil))
  (parser:make-node-application
   :rator rator
   :rands rands
   :keyword-rands keyword-rands
   :location location))

(defun builder-call-node (package-name symbol-name args location)
  "Build a parser application node that calls PACKAGE-NAME::SYMBOL-NAME with ARGS."
  (make-parser-application
   (make-parser-variable-node (required-builder-symbol package-name symbol-name) location)
   args
   location))

(defun make-builder-size-node (size location)
  (declare (type alexandria:non-negative-fixnum size))
  (make-parser-integer-literal-node size location))

(defun make-builder-size-hint-node (size location)
  (declare (type (or null alexandria:non-negative-fixnum) size))
  (if size
      (builder-call-node "COALTON/CLASSES" "SOME"
                         (list (make-builder-size-node size location))
                         location)
      (make-parser-variable-node (required-builder-symbol "COALTON/CLASSES" "NONE")
                                 location)))

(defun make-builder-proxy-node (location)
  (make-parser-variable-node (required-builder-symbol "COALTON/TYPES" "PROXY")
                             location))

(defun with-builder-proxy-node (location body)
  (declare (type source:location location)
           (type function body)
           (values parser:node &optional))
  (let* ((proxy-name (gensym "BUILDER-PROXY-"))
         (proxy-node (make-parser-variable-node proxy-name location))
         (fn-node (parser:make-node-abstraction
                   :params (list (make-parser-pattern-var proxy-name location))
                   :keyword-params nil
                   :body (make-parser-body (funcall body proxy-node))
                   :location location)))
    (make-parser-application fn-node
                             (list (make-builder-proxy-node location))
                             location)))

(defun leading-builder-for-clause (clauses)
  (declare (type list clauses)
           (values (or null parser:builder-for-clause) &optional))
  (let ((clause (first clauses)))
    (and clause
         (typep clause 'parser:builder-for-clause)
         clause)))

(defun with-leading-builder-size-hint (clauses location body)
  (declare (type list clauses)
           (type source:location location)
           (type function body)
           (values parser:node &optional))
  (let ((leading-for (leading-builder-for-clause clauses)))
    (if (null leading-for)
        (funcall body (make-builder-size-hint-node nil location) nil)
        (let* ((clause-location (source:location leading-for))
               (source-name (gensym "BUILDER-SOURCE-"))
               (source-node (make-parser-variable-node source-name clause-location))
               (iter-node (builder-call-node "COALTON/ITERATOR" "INTO-ITER"
                                             (list source-node)
                                             clause-location))
               (size-hint-node (builder-call-node "COALTON/ITERATOR" "SIZE-HINT"
                                                  (list iter-node)
                                                  clause-location)))
          (parser:make-node-let
           :bindings (list
                      (parser:make-node-let-binding
                       :name source-node
                       :value (parser:builder-for-clause-expr leading-for)
                       :location clause-location))
           :declares nil
           :body (make-parser-body
                  (funcall body
                           size-hint-node
                           (list (cons leading-for source-node))))
           :location clause-location)))))

(defun desugar-builder-clauses-to-parser-node (clauses state base-form
                                               &optional replacements)
  (if (endp clauses)
      (funcall base-form state)
      (let ((clause (first clauses)))
        (typecase clause
          (parser:builder-with-clause
            (parser:make-node-let
             :bindings (list
                        (parser:make-node-let-binding
                         :name (parser:builder-with-clause-binder clause)
                         :value (parser:builder-with-clause-expr clause)
                         :location (source:location clause)))
             :declares nil
             :body (make-parser-body
                    (desugar-builder-clauses-to-parser-node
                     (rest clauses)
                     state
                     base-form
                     replacements))
             :location (source:location clause)))
          (parser:builder-when-clause
            (parser:make-node-if
             :expr (parser:builder-when-clause-expr clause)
             :then (desugar-builder-clauses-to-parser-node
                    (rest clauses)
                    state
                    base-form
                    replacements)
             :else state
             :location (source:location clause)))
          (parser:builder-for-clause
            (let* ((clause-location (source:location clause))
                   (for-expr (or (cdr (assoc clause replacements :test #'eq))
                                 (parser:builder-for-clause-expr clause)))
                   (state-name (gensym "COLL-STATE-"))
                   (state-node (make-parser-variable-node state-name clause-location))
                   (body-node (desugar-builder-clauses-to-parser-node
                               (rest clauses)
                               state-node
                               base-form
                               replacements))
                   (fn-node (parser:make-node-abstraction
                             :params (list (make-parser-pattern-var state-name clause-location)
                                           (make-parser-pattern-var
                                            (parser:node-variable-name (parser:builder-for-clause-binder clause))
                                            (source:location (parser:builder-for-clause-binder clause))))
                             :keyword-params nil
                             :body (make-parser-body body-node)
                             :location clause-location)))
              (builder-call-node "COALTON/ITERATOR" "FOLD!"
                                 (list fn-node
                                       state
                                       (builder-call-node "COALTON/ITERATOR" "INTO-ITER"
                                                          (list for-expr)
                                                          clause-location))
                                 clause-location)))
          (parser:builder-below-clause
            (let* ((clause-location (source:location clause))
                   (initial-state-name (gensym "COLL-INITIAL-STATE-"))
                   (initial-state-node (make-parser-variable-node initial-state-name clause-location))
                   (state-name (gensym "COLL-STATE-"))
                   (state-node (make-parser-variable-node state-name clause-location))
                   (limit-name (gensym "COLL-LIMIT-"))
                   (limit-node (make-parser-variable-node limit-name clause-location))
                   (index-name (parser:node-variable-name (parser:builder-below-clause-binder clause)))
                   (index-location (source:location (parser:builder-below-clause-binder clause)))
                   (index-node (make-parser-variable-node index-name index-location)))
              (make-parser-application
               (parser:make-node-abstraction
                :params (list (make-parser-pattern-var initial-state-name clause-location))
                :keyword-params nil
                :body (make-parser-body
                       (parser:make-node-for
                        :label :builder-below
                        :bindings
                        (list
                         (parser:make-node-for-binding
                          :name state-node
                          :init initial-state-node
                          :step (desugar-builder-clauses-to-parser-node
                                 (rest clauses)
                                 state-node
                                 base-form
                                 replacements)
                          :location clause-location)
                         (parser:make-node-for-binding
                          :name index-node
                          :init (make-parser-integer-literal-node 0 index-location)
                          :step (builder-call-node "COALTON/MATH/ARITH" "1+"
                                                   (list index-node)
                                                   index-location)
                          :location index-location)
                         (parser:make-node-for-binding
                          :name limit-node
                          :init (parser:builder-below-clause-expr clause)
                          :location clause-location))
                        :declares nil
                        :returns state-node
                        :termination-kind :while
                        :termination-expr (builder-call-node "COALTON/CLASSES" "<"
                                                             (list index-node limit-node)
                                                             clause-location)
                        :body (make-parser-body
                               (make-parser-zero-values-node clause-location))
                        :location clause-location))
                :location clause-location)
               (list state)
               clause-location)))
          (t
            (util:coalton-bug "Unexpected builder clause ~S" clause))))))

(defun desugar-parser-builder-node (node)
  (typecase node
    (parser:node-collection-builder
      (let* ((elements (parser:node-collection-builder-elements node))
             (location (source:location node)))
        (with-builder-proxy-node
            location
          (lambda (proxy-node)
            (let* ((size (make-builder-size-node (length elements) location))
                   (builder-state
                     (loop :with state := (builder-call-node "COALTON/CLASSES"
                                                            "BEGIN-COLLECTION-BUILDER"
                                                            (list proxy-node size)
                                                            location)
                           :for element :in elements
                           :for index :from 0
                           :do (setf state
                                     (builder-call-node
                                      "COALTON/CLASSES"
                                      "ADJOIN-TO-COLLECTION-BUILDER"
                                      (list proxy-node
                                            state
                                            (make-builder-size-node index (source:location element))
                                            element)
                                      (source:location element)))
                           :finally (return state))))
              (builder-call-node "COALTON/CLASSES" "FINALIZE-COLLECTION-BUILDER"
                                 (list proxy-node builder-state)
                                 location))))))
    (parser:node-association-builder
      (let* ((entries (parser:node-association-builder-entries node))
             (location (source:location node)))
        (with-builder-proxy-node
            location
          (lambda (proxy-node)
            (let* ((size (make-builder-size-node (length entries) location))
                   (builder-state
                     (loop :with state := (builder-call-node "COALTON/CLASSES"
                                                            "BEGIN-ASSOCIATION-BUILDER"
                                                            (list proxy-node size)
                                                            location)
                           :for entry :in entries
                           :for index :from 0
                           :do (setf state
                                     (builder-call-node
                                      "COALTON/CLASSES"
                                      "ADJOIN-TO-ASSOCIATION-BUILDER"
                                      (list proxy-node
                                            state
                                            (make-builder-size-node index (source:location entry))
                                            (parser:association-entry-key entry)
                                            (parser:association-entry-value entry))
                                      (source:location entry)))
                           :finally (return state))))
              (builder-call-node "COALTON/CLASSES" "FINALIZE-ASSOCIATION-BUILDER"
                                 (list proxy-node builder-state)
                                 location))))))
    (parser:node-collection-comprehension
      (let ((location (source:location node))
            (clauses (parser:node-collection-comprehension-clauses node))
            (head (parser:node-collection-comprehension-head node)))
        (with-leading-builder-size-hint
         clauses
         location
          (lambda (size-hint replacements)
            (let ((head-location (source:location head)))
              (with-builder-proxy-node
                  location
                (lambda (proxy-node)
                  (builder-call-node "COALTON/CLASSES" "FINALIZE-COLLECTION-COMPREHENSION"
                                     (list
                                      proxy-node
                                      (desugar-builder-clauses-to-parser-node
                                       clauses
                                       (builder-call-node "COALTON/CLASSES"
                                                          "BEGIN-COLLECTION-COMPREHENSION"
                                                          (list proxy-node size-hint)
                                                          location)
                                       (lambda (state)
                                         (builder-call-node
                                          "COALTON/CLASSES"
                                          "ADJOIN-TO-COLLECTION-COMPREHENSION"
                                          (list proxy-node state head)
                                          head-location))
                                       replacements))
                                     location))))))))
    (parser:node-association-comprehension
      (let ((location (source:location node))
            (clauses (parser:node-association-comprehension-clauses node)))
        (with-leading-builder-size-hint
         clauses
         location
          (lambda (size-hint replacements)
            (with-builder-proxy-node
                location
              (lambda (proxy-node)
                (builder-call-node "COALTON/CLASSES" "FINALIZE-ASSOCIATION-COMPREHENSION"
                                   (list
                                    proxy-node
                                    (desugar-builder-clauses-to-parser-node
                                     clauses
                                     (builder-call-node "COALTON/CLASSES"
                                                        "BEGIN-ASSOCIATION-COMPREHENSION"
                                                        (list proxy-node size-hint)
                                                        location)
                                     (lambda (state)
                                       (builder-call-node
                                        "COALTON/CLASSES"
                                        "ADJOIN-TO-ASSOCIATION-COMPREHENSION"
                                        (list proxy-node
                                              state
                                              (parser:node-association-comprehension-key node)
                                              (parser:node-association-comprehension-value node))
                                        location))
                                     replacements))
                                   location)))))))
    (t
      (util:coalton-bug "Cannot desugar non-builder node ~S" node))))

(defun homogeneous-builder-item-error (kind expected-type actual-type current-node first-node)
  (tc-error (ecase kind
              (:collection "Collection element type mismatch")
              (:association-key "Association key type mismatch")
              (:association-value "Association value type mismatch"))
            (tc-note current-node
                     (ecase kind
                       (:collection
                        "collection elements must all have the same type; expected '~A' but got '~A'")
                       (:association-key
                        "association keys must all have the same type; expected '~A' but got '~A'")
                       (:association-value
                        "association values must all have the same type; expected '~A' but got '~A'"))
                     (type-object-string expected-type)
                     (type-object-string actual-type))
            (tc-secondary-note first-node
                               (ecase kind
                                 (:collection "earlier element here")
                                 (:association-key "earlier key here")
                                 (:association-value "earlier value here")))))

(defun check-homogeneous-builder-items (kind items subs env)
  (declare (type list items)
           (type tc:substitution-list subs)
           (type tc-env env)
           (values tc:substitution-list &optional))
  (let ((shared-ty (tc:make-variable))
        (first-node nil))
    (dolist (item items subs)
      (multiple-value-bind (item-ty preds accessors typed-item subs_)
          (infer-expression-type item (tc:make-variable) subs env)
        (declare (ignore preds accessors typed-item))
        (setf subs subs_)
        (handler-case
            (setf subs (tc:unify subs shared-ty item-ty))
          (tc:coalton-internal-type-error ()
            (homogeneous-builder-item-error kind
                                            (tc:apply-substitution subs shared-ty)
                                            (tc:apply-substitution subs item-ty)
                                            item
                                            first-node)))
        (unless first-node
          (setf first-node item))))))

(defun values-result-type (component-types)
  (declare (type tc:ty-list component-types)
           (values tc:ty &optional))
  (tc:output-types-result-type component-types))

(defun zero-result-type ()
  (declare (values tc:ty &optional))
  (tc:output-types-result-type nil))

(defun zero-result-type-p (type)
  (declare (type tc:ty type)
           (values boolean &optional))
  (and (typep type 'tc:result-ty)
       (null (tc:result-ty-output-types type))))

(defun direct-call-output-context (node expected-type)
  (declare (type parser:node node)
           (type tc:ty expected-type)
           (values boolean (or null tc:ty-list) &optional))
  (cond
    ((eq node *direct-values-node*)
     (values t *direct-values-output-types*))
    ((zero-result-type-p expected-type)
     (values t nil))
    (t
     (values nil nil))))

(defun function-keyword-entry (type keyword)
  (declare (type tc:function-ty type)
           (type keyword keyword)
           (values (or null tc:keyword-ty-entry) &optional))
  (find keyword
        (tc:function-ty-keyword-input-types type)
        :key #'tc:keyword-ty-entry-keyword
        :test #'eq))

(defun keyword-entry-sort-key (entry)
  (declare (type tc:keyword-ty-entry entry)
           (values string &optional))
  (symbol-name (tc:keyword-ty-entry-keyword entry)))

(defun normalize-keyword-entries (entries)
  (declare (type list entries)
           (values tc:keyword-ty-entry-list &optional))
  (sort (copy-list entries)
        #'string<
        :key #'keyword-entry-sort-key))

(defun rebuild-function-type (template positional-input-types keyword-input-types keyword-open-p output-types)
  (declare (type tc:function-ty template)
           (type tc:ty-list positional-input-types)
           (type list keyword-input-types)
           (type boolean keyword-open-p)
           (type (or null tc:ty-list) output-types)
           (values tc:function-ty &optional))
  (tc:make-function-ty
   :alias (tc:ty-alias template)
   :positional-input-types positional-input-types
   :keyword-input-types (normalize-keyword-entries keyword-input-types)
   :keyword-open-p keyword-open-p
   :output-types output-types))

(defun extend-function-type-keywords (type new-entries)
  (declare (type tc:function-ty type)
           (type list new-entries)
           (values tc:function-ty &optional))
  (let ((entries (copy-list (tc:function-ty-keyword-input-types type))))
    (dolist (entry new-entries)
      (unless (function-keyword-entry type (tc:keyword-ty-entry-keyword entry))
        (push entry entries)))
    (rebuild-function-type type
                           (tc:function-ty-positional-input-types type)
                           entries
                           (tc:function-ty-keyword-open-p type)
                           (tc:function-ty-output-types type))))

(defun project-function-type-keywords (type keyword-entries keyword-open-p)
  (declare (type tc:function-ty type)
           (type list keyword-entries)
           (type boolean keyword-open-p)
           (values tc:function-ty &optional))
  (rebuild-function-type type
                         (tc:function-ty-positional-input-types type)
                         keyword-entries
                         keyword-open-p
                         (tc:function-ty-output-types type)))

(defun maybe-update-local-function-type (env node type)
  "Refine a locally-bound function's type when keyword extension discovers new
keywords at a call site.  Only updates schemes that still contain free type
variables (i.e., unresolved placeholders from let-bound lambdas).  Monomorphic
schemes and already-quantified schemes are left alone -- their types are fully
determined and must not be widened."
  (declare (type tc-env env)
           (type parser:node node)
           (type tc:function-ty type)
           (values null))
  (when (typep node 'parser:node-variable)
    (let* ((name (parser:node-variable-name node))
           (scheme (gethash name (tc-env-ty-table env))))
      (when (and scheme
                 (tc:type-variables scheme))
        (tc-env-replace-type env name (tc:to-scheme type)))))
  nil)

(defun body-return-block-node (body)
  "Return BODY's outer return block when BODY is exactly a single block wrapper.

BODY may be either a parser NODE-BODY or a typed NODE-BODY."
  (declare (type (or parser:node-body node-body) body)
           (values (or null parser:node-block node-block) &optional))
  (etypecase body
    (parser:node-body
     (and (null (parser:node-body-nodes body))
          (typep (parser:node-body-last-node body) 'parser:node-block)
          (parser:node-body-last-node body)))
    (node-body
     (and (null (node-body-nodes body))
          (typep (node-body-last-node body) 'node-block)
          (node-body-last-node body)))))

(defun merge-keyword-prefix-nodes-into-body (prefix-nodes body)
  (declare (type list prefix-nodes)
           (type node-body body)
           (values node-body &optional))
  (if (null prefix-nodes)
      body
      (let ((outer-block (body-return-block-node body)))
        (if outer-block
            (let ((inner-body (node-block-body outer-block)))
              (make-node-body
               :nodes nil
               :last-node
               (make-node-block
                :type (node-type outer-block)
                :location (source:location outer-block)
                :name (node-block-name outer-block)
                :body (make-node-body
                       :nodes (append prefix-nodes (node-body-nodes inner-body))
                       :last-node (node-body-last-node inner-body)))))
            (make-node-body
             :nodes (append prefix-nodes (node-body-nodes body))
             :last-node (node-body-last-node body))))))

(defun infer-keyword-params (keyword-params expected-keyword-table subs env)
  "Infer types for keyword parameters and their defaults.

Each keyword's default is checked before the binder is added to the local
environment, ensuring sequential visibility (later defaults can reference
earlier keywords, but not themselves)."
  (declare (type parser:keyword-param-list keyword-params)
           (type hash-table expected-keyword-table)
           (type tc:substitution-list subs)
           (type tc-env env)
           (values list tc:keyword-ty-entry-list list
                   tc:ty-predicate-list accessor-list
                   tc:substitution-list &optional))
  (let ((typed-keyword-params nil)
        (keyword-entry-types nil)
        (keyword-prefix-nodes nil)
        (preds nil)
        (accessors nil))
    (dolist (param keyword-params)
      (let* ((binder (parser:keyword-param-binder param))
             (binder-name (parser:node-variable-name binder))
             (key (parser:keyword-src-name (parser:keyword-param-keyword param)))
             (expected-entry (gethash key expected-keyword-table))
             (visible-ty (or (and expected-entry
                                  (tc:keyword-ty-entry-type expected-entry))
                             (tc:make-variable)))
             (present-name (gensym "KW-PRESENT-"))
             (value-name (gensym "KW-VALUE-"))
             (location (source:location param)))
        (multiple-value-bind (default-ty preds_ accessors_ default-node subs_)
            (infer-expression-type (parser:keyword-param-default param)
                                   visible-ty
                                   subs
                                   env)
          (declare (ignore default-ty))
          (setf subs subs_)
          (setf visible-ty (tc:apply-substitution subs visible-ty))
          ;; Add the keyword binder to the local environment only after
          ;; its default has been checked, so later defaults can see it.
          (tc-env-add-variable env binder-name)
          (tc-env-replace-type env binder-name (tc:to-scheme visible-ty))
          (push (make-keyword-param
                 :keyword key
                 :value-var value-name
                 :supplied-p-var present-name)
                typed-keyword-params)
          (push (tc:make-keyword-ty-entry
                 :keyword key
                 :type visible-ty)
                keyword-entry-types)
          (push (make-node-bind
                 :pattern (typed-pattern-var binder-name visible-ty (source:location binder))
                 :expr (make-node-if
                        :type (tc:qualify nil visible-ty)
                        :location location
                        :expr (typed-node-variable present-name tc:*boolean-type* location)
                        :then (typed-node-variable value-name visible-ty location)
                        :else default-node)
                 :location location)
                keyword-prefix-nodes)
          (setf preds (append preds preds_))
          (setf accessors (append accessors accessors_)))))
    (values typed-keyword-params keyword-entry-types keyword-prefix-nodes
            preds accessors subs)))

(defun resolve-tyvar-application-type (fun-ty node expected-type subs
                                       positional-rands keyword-rands)
  "When a function application's operator is a type variable, construct a fresh
function type matching the call site shape and unify it with the operator."
  (declare (type tc:tyvar fun-ty)
           (type parser:node node)
           (type tc:ty expected-type)
           (type tc:substitution-list subs)
           (type list positional-rands keyword-rands)
           (values tc:function-ty tc:substitution-list &optional))
  (multiple-value-bind (has-output-context-p output-types)
      (direct-call-output-context node (tc:apply-substitution subs expected-type))
    (let* ((new-froms (loop :repeat (length positional-rands)
                            :collect (tc:make-variable)))
           (new-keywords (loop :for arg :in keyword-rands
                               :collect (tc:make-keyword-ty-entry
                                         :keyword (parser:keyword-src-name
                                                   (parser:node-application-keyword-arg-keyword arg))
                                         :type (tc:make-variable))))
           ;; keyword-open-p is an inference artifact, not a definition
           ;; property: it marks call sites where the callee may accept
           ;; additional keywords discovered later during unification.
           ;; Calls that mention no keywords stay closed so that
           ;; function-value coercion can accept callee values carrying
           ;; additional optional keywords without requiring exact match.
           (keyword-open-p (consp keyword-rands))
           (new-to (and (not has-output-context-p)
                        (tc:make-variable)))
           (new-ty (tc:make-function-ty
                    :positional-input-types new-froms
                    :keyword-input-types (normalize-keyword-entries new-keywords)
                    :keyword-open-p keyword-open-p
                    :output-types (if has-output-context-p
                                      output-types
                                      (list new-to)))))
      (setf subs (tc:unify subs fun-ty new-ty))
      (values (tc:apply-substitution subs new-ty) subs))))

(defun coerce-function-value-type (actual-type expected-type subs)
  "Coerce a function value's type to match an expected function type.

A function value can be viewed through any interface that uses a subset of its
optional keywords.  For example, a function of type (A &key (:x B) (:y C) -> D)
is usable where (A &key (:x B) -> D) is expected -- the :y keyword is simply
hidden from the caller's view.

For each keyword the expected-type mentions:
  - If actual-type already has it, include it in the visible interface.
  - If actual-type is keyword-open, extend it with a fresh type variable.
  - Otherwise, signal a unification error.

Both types are then PROJECTED to only the shared keyword interface (with
keyword-open-p=nil) before unification.  The projections are closed views,
not the original types.

Returns four values:
  1. Updated substitution list
  2. The visible expected-type (projected to shared keywords, closed)
  3. The effective actual-type (full type, possibly extended with new keywords)
  4. Boolean: whether a runtime wrapper is needed (currently always nil)"
  (declare (type tc:function-ty actual-type expected-type)
           (type tc:substitution-list subs)
           (values tc:substitution-list tc:function-ty tc:function-ty boolean &optional))
  (let ((effective-type actual-type)
        (actual-visible-entries nil)
        (expected-visible-entries nil))
    (dolist (expected-entry (tc:function-ty-keyword-input-types expected-type))
      (let ((actual-entry (function-keyword-entry effective-type
                                                 (tc:keyword-ty-entry-keyword expected-entry))))
        (cond
          (actual-entry
           (push (tc:make-keyword-ty-entry
                  :keyword (tc:keyword-ty-entry-keyword actual-entry)
                  :type (tc:keyword-ty-entry-type actual-entry))
                 actual-visible-entries)
           (push expected-entry expected-visible-entries))
          ((tc:function-ty-keyword-open-p effective-type)
           (setf effective-type
                 (extend-function-type-keywords effective-type (list expected-entry)))
           (push expected-entry actual-visible-entries)
           (push expected-entry expected-visible-entries))
          (t
           (error 'tc:unification-error :type1 actual-type :type2 expected-type)))))
    (let* ((visible-actual-type
             (project-function-type-keywords effective-type
                                             (nreverse actual-visible-entries)
                                             nil))
           (visible-expected-type
             (project-function-type-keywords expected-type
                                             (nreverse expected-visible-entries)
                                             nil))
           (subs (tc:unify subs visible-actual-type visible-expected-type)))
      (values subs
              (the tc:function-ty (tc:apply-substitution subs visible-expected-type))
              (the tc:function-ty (tc:apply-substitution subs effective-type))
              nil))))

(defun typed-node-variable (name type location)
  (declare (type symbol name)
           (type tc:ty type)
           (type source:location location)
           (values node-variable &optional))
  (make-node-variable
   :type (tc:qualify nil type)
   :location location
   :name name))

(defun typed-pattern-var (name type location)
  (declare (type symbol name)
           (type tc:ty type)
           (type source:location location)
           (values pattern-var &optional))
  (make-pattern-var
   :type (tc:qualify nil type)
   :location location
   :name name
   :orig-name name))

(defun ensure-no-duplicate-function-parameters (patterns keyword-params)
  (declare (type parser:pattern-list patterns)
           (type parser:keyword-param-list keyword-params))
  (let ((seen (make-hash-table :test #'eq)))
    (dolist (pat (parser:pattern-variables patterns))
      (let ((name (parser:pattern-var-name pat)))
        (when (gethash name seen)
          (tc-error "Duplicate parameters name"
                    (tc-note (gethash name seen) "first parameter here")
                    (tc-note pat "second parameter here")))
        (setf (gethash name seen) pat)))
    (dolist (param keyword-params)
      (let* ((binder (parser:keyword-param-binder param))
             (name (parser:node-variable-name binder)))
        (when (gethash name seen)
          (tc-error "Duplicate parameters name"
                    (tc-note (gethash name seen) "first parameter here")
                    (tc-note binder "second parameter here")))
        (setf (gethash name seen) binder)))))

(defun inferred-node-output-types (type node)
  (declare (type tc:ty type)
           (type t node)
           (values (or null tc:ty-list) &optional))
  (let ((output-arity (node-output-arity node)))
    (cond
      ((zerop output-arity)
       nil)
      ((typep type 'tc:result-ty)
       (copy-list (tc:result-ty-output-types type)))
      (t
       (list type)))))

;;;
;;; Entrypoint
;;;

(defun toplevel-define (defines declares env)
  "Entrypoint for typechecking a group of parsed defines and declares."
  (declare (type parser:toplevel-define-list defines)
           (type parser:toplevel-declare-list declares)
           (type tc:environment env)
           (values toplevel-define-list tc:environment))

  ;; Ensure that all defines are in the current package
  (check-package defines
                 (alexandria:compose #'parser:node-variable-name
                                     #'parser:toplevel-define-name)
                 (alexandria:compose #'source:location
                                     #'parser:toplevel-define-name))

  ;; Ensure that there are no duplicate definitions
  (check-duplicates
   defines
   (alexandria:compose #'parser:node-variable-name #'parser:toplevel-define-name)
   (lambda (first second)
     (tc-error "Duplicate definition"
               (tc-note (parser:toplevel-define-name first)
                        "first definition here")
               (tc-note (parser:toplevel-define-name second)
                        "second definition here"))))

  ;; Ensure that there are no duplicate declarations
  (check-duplicates
   declares
   (alexandria:compose #'parser:identifier-src-name #'parser:toplevel-declare-name)
   (lambda (first second)
     (tc-error "Duplicate declaration"
               (tc-note (parser:toplevel-declare-name first)
                        "first declaration here")
               (tc-note (parser:toplevel-declare-name second)
                        "second declaration here"))))

  ;; Ensure that each declaration has an associated definition
  (loop :with def-table
          := (loop :with table := (make-hash-table :test #'eq)

                   :for def :in defines
                   :for name := (parser:node-variable-name
                                 (parser:toplevel-define-name def))

                   :do (setf (gethash name table) def)

                   :finally (return table))

        :for declare :in declares
        :for name := (parser:identifier-src-name (parser:toplevel-declare-name declare))

        :unless (gethash name def-table)
          :do (tc-error "Orphan declaration"
                        (tc-note (parser:toplevel-declare-name declare)
                                 "declaration does not have an associated definition")))

  (let ((dec-table (make-hash-table :test #'eq))

        (tc-env (make-tc-env :env env)))

    (loop :for declare :in declares
          :for name := (parser:identifier-src-name (parser:toplevel-declare-name declare))
          :for ty := (parser:toplevel-declare-type declare)
          :do (setf (gethash name dec-table) ty))

    ;; Infer binding types, returning the typed nodes.
    (multiple-value-bind (preds accessors binding-nodes subs)
        (infer-bindings-type defines dec-table nil tc-env)
      (assert (null preds))
      (assert (null accessors))

      (let (;; Attach explicit types to any explicit bindings

            (binding-nodes
              (loop :for node :in binding-nodes
                    :for explicit-type := (gethash (toplevel-define-name node) dec-table)
                    :if explicit-type
                      :collect (attach-explicit-binding-type node explicit-type)
                    :else
                      :collect node)))

        ;; Check types and update environment
        (when (catch 'redef:abort-redef
                (handler-bind
                    ((redef:incompatible-redefinition
                       (lambda (c)
                         (declare (ignore c))
                         (when settings:*auto-continue-redefinition*
                           (let ((restart (find-restart 'redef:continue-anyway)))
                             (when restart
                               (invoke-restart restart)))))))
                  (loop :for define :in defines
                        :for name := (parser:node-variable-name (parser:binding-name define))
                        :for scheme := (tc:remove-source-info (gethash name (tc-env-ty-table tc-env)))

                        :when (tc:type-variables scheme)
                          :do (util:coalton-bug "Scheme ~S should not have any free type variables." scheme)

                        ;; Check for incompatible redefinition before updating environment
                        :do (let ((old-type (tc:lookup-value-type env name :no-error t)))
                              (when old-type
                                (unless (redef:types-compatible-p old-type scheme)
                                  (let ((affected (redef:find-affected-functions name)))
                                    (when affected
                                      (redef:raise-redefinition-error
                                       :function-name name
                                       :old-type old-type
                                       :new-type scheme
                                       :affected-functions affected
                                       :environment env))))))

                            (setf env (tc:set-value-type env name scheme))

                            (setf env (tc:set-name env name (tc:make-name-entry
                                                             :name name
                                                             :type :value
                                                             :docstring (source:docstring define)
                                                             :location (source:location define))))

                        :if (parser:toplevel-define-orig-params define)
                          :do (setf env (tc:set-function-source-parameter-names
                                         env
                                         name
                                         (parser:toplevel-define-orig-params define)))
                        :else
                          :if (tc:lookup-function-source-parameter-names env name)
                            :do (setf env (tc:unset-function-source-parameter-names env name))))
                nil)
          (return-from toplevel-define (values nil env)))

        (let ((binding-function-table
                (loop :with table := (make-hash-table :test #'eq)
                      :for binding :in binding-nodes
                      :for name := (node-variable-name (toplevel-define-name binding))
                      :do (setf (gethash name table) (typed-binding-function-p binding))
                      :finally (return table))))
          (check-bindings-for-invalid-recursion
           defines
           (tc-env-env tc-env)
           :binding-function-p
           (lambda (binding)
             (gethash (parser:node-variable-name (parser:binding-name binding))
                      binding-function-table))))

        ;; Record dependencies
        (loop :for define :in defines
              :for name := (parser:node-variable-name (parser:binding-name define))
              :for code := (parser:binding-value define)
              :do (redef:record-dependencies name code env))

        (values
         (tc:apply-substitution subs binding-nodes)
         env)))))

;;;
;;; Expression Type Inference
;;;

(defun exception-type-p (inferred-ty env)
  (alexandria:when-let* ((ty-name (if (types:tycon-p inferred-ty)
                                      (types:tycon-name inferred-ty)
                                      nil))
                         (entry   (tc:lookup-type (tc-env-env env) ty-name)))
    (tc:type-entry-exception-p entry)))

(defun resumption-type-p (inferred-ty env)
  (alexandria:when-let* ((ty-name (if (types:tycon-p inferred-ty)
                                      (types:tycon-name inferred-ty)
                                      nil))
                         (entry   (tc:lookup-type (tc-env-env env) ty-name)))
    (tc:type-entry-resumption-p entry)))

(defun node-output-arity (node)
  (declare (type t node)
           (values fixnum &optional))
  (typecase node
    (node-body
      (node-output-arity (node-body-last-node node)))
    (node
      (tc:multiple-value-output-arity
       (tc:qualified-ty-type (node-type node))))
    (t
      1)))

(defun reflect-expression-type-scheme (node subs env)
  (declare (type parser:node node)
           (type tc:substitution-list subs)
           (type tc-env env)
           (values tc:ty-scheme tc:substitution-list &optional))
  (multiple-value-bind (ty preds accessors _ subs)
      (infer-expression-type node
                             (tc:make-variable)
                             subs
                             env)
    (declare (ignore _))
    (multiple-value-bind (preds subs)
        (tc:solve-fundeps (tc-env-env env) preds subs)
      (setf accessors (tc:apply-substitution subs accessors))
      (multiple-value-bind (accessors subs_)
          (solve-accessors accessors (tc-env-env env))
        (setf subs (tc:compose-substitution-lists subs subs_))
        (when accessors
          (tc:tc-error "Ambiguous accessor"
                       (source:note (first accessors)
                                    "accessor is ambiguous")))
        (let* ((preds (tc:reduce-context (tc-env-env env) preds subs))
               (ty (tc:apply-substitution subs ty))
               (qual-ty (tc:qualify preds ty)))
          (values
           (tc:remove-source-info
            (tc:quantify (tc:type-variables qual-ty) qual-ty))
           subs))))))

(defun rec-node-bindings-independent-p (node)
  "Return true when REC's synthetic outer init bindings do not depend on each
other, so they can be inferred monomorphically before the recursive body."
  (declare (type parser:node-rec node)
           (values boolean &optional))
  (let ((names (mapcar (alexandria:compose #'parser:node-variable-name
                                           #'parser:node-let-binding-name)
                       (parser:node-rec-bindings node))))
    (every (lambda (binding)
             (null
              (intersection
               names
               (mapcar #'parser:node-variable-name
                       (parser:collect-variables
                        (parser:node-let-binding-value binding)))
               :test #'eq)))
           (parser:node-rec-bindings node))))

(defun rec-node-inner-binding (node)
  "Build the parser let-binding for REC's local recursive function."
  (declare (type parser:node-rec node)
           (values parser:node-let-binding &optional))
  (parser:make-node-let-binding
   :name (parser:node-rec-name node)
   :value (parser:make-node-abstraction
           :params (parser:node-rec-params node)
           :keyword-params nil
           :body (parser:node-rec-body node)
           :location (source:location node))
   :location (source:location node)))

(defun rec-node-declare-table (node)
  "Validate REC's init bindings and declarations, then return a declare table."
  (declare (type parser:node-rec node)
           (values hash-table &optional))
  (let ((def-table (make-hash-table :test #'eq))
        (dec-table (make-hash-table :test #'eq)))
    (loop :for binding :in (parser:node-rec-bindings node)
          :for name := (parser:node-variable-name (parser:node-let-binding-name binding))
          :if (gethash name def-table)
            :do (tc-error "Duplicate binding in rec"
                          (tc-note (parser:node-let-binding-name binding)
                                   "second definition here")
                          (tc-note (parser:node-let-binding-name
                                    (gethash name def-table))
                                   "first definition here"))
          :else
            :do (setf (gethash name def-table) binding))

    (loop :for declare :in (parser:node-rec-declares node)
          :for name := (parser:node-variable-name (parser:node-let-declare-name declare))
          :if (gethash name dec-table)
            :do (tc-error "Duplicate declaration in rec"
                          (tc-note (parser:node-let-declare-name declare)
                                   "second declaration here")
                          (tc-note (parser:node-let-declare-name
                                    (gethash name dec-table))
                                   "first declaration here"))
          :else
            :do (setf (gethash name dec-table) declare))

    (loop :for declare :in (parser:node-rec-declares node)
          :for name := (parser:node-variable-name (parser:node-let-declare-name declare))
          :unless (gethash name def-table)
            :do (tc-error "Orphan declare in rec"
                          (tc-note (parser:node-let-declare-name declare)
                                   "declaration does not have an associated definition")))

    (loop :with table := (make-hash-table :test #'eq)
          :for declare :in (parser:node-rec-declares node)
          :for name := (parser:node-variable-name (parser:node-let-declare-name declare))
          :do (setf (gethash name table) (parser:node-let-declare-type declare))
          :finally (return table))))

(defun infer-rec-init-binding-type (binding declared-type subs env)
  "Infer one REC init binding, preserving init-binding declarations."
  (declare (type parser:node-let-binding binding)
           (type (or null parser:qualified-ty) declared-type)
           (type tc:substitution-list subs)
           (type tc-env env)
           (values tc:ty-predicate-list node-let-binding tc:qualified-ty tc:ty-scheme tc:substitution-list))
  (if declared-type
      (let ((declared-scheme (parse-ty-scheme declared-type (tc-env-parser-env env))))
        (multiple-value-bind (binding-preds typed-binding subs_)
            (infer-expl-binding-type binding
                                     declared-scheme
                                     (source:location (parser:node-let-binding-name binding))
                                     subs
                                     env)
          (setf binding-preds (tc:apply-substitution subs_ binding-preds))
          (setf typed-binding (tc:apply-substitution subs_ typed-binding))
          (let ((binding-qual-ty (node-type (node-let-binding-name typed-binding))))
            (values binding-preds
                    typed-binding
                    binding-qual-ty
                    (tc:apply-substitution subs_ declared-scheme)
                    subs_))))

      (multiple-value-bind (binding-preds binding-accessors typed-binding subs_)
          (infer-binding-type binding (tc:make-variable) subs env)
        (setf subs subs_)
        (setf binding-preds (tc:apply-substitution subs binding-preds))
        (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) binding-preds subs)))
        (setf binding-accessors (tc:apply-substitution subs binding-accessors))

        (multiple-value-bind (binding-accessors subs__)
            (solve-accessors binding-accessors (tc-env-env env))
          (setf subs (tc:compose-substitution-lists subs subs__))

          (when binding-accessors
            (tc-error "Ambiguous accessor"
                      (tc-note (first binding-accessors)
                               "accessor is ambiguous")))

          (setf binding-preds (tc:apply-substitution subs binding-preds))
          (setf typed-binding (tc:apply-substitution subs typed-binding))
          (let* ((binding-name (node-let-binding-name typed-binding))
                 (binding-type (tc:qualified-ty-type (node-type binding-name)))
                 (binding-qual-ty (tc:qualify binding-preds binding-type)))
            (values binding-preds
                    typed-binding
                    binding-qual-ty
                    (tc:to-scheme binding-qual-ty)
                    subs))))))

(defun lower-rec-node-to-let (node)
  "Lower REC to the parser nested-let form used before the refactor."
  (declare (type parser:node-rec node)
           (values parser:node-let &optional))
  (let* ((location (source:location node))
         (inner-binding (rec-node-inner-binding node)))
    (parser:make-node-let
     :bindings (parser:node-rec-bindings node)
     :declares (parser:node-rec-declares node)
     :body (make-parser-body
            (parser:make-node-let
             :bindings (list inner-binding)
             :declares nil
             :body (make-parser-body
                    (parser:make-node-application
                     :rator (parser:node-rec-name node)
                     :rands (parser:node-rec-call-args node)
                     :location location))
             :location location))
     :location location)))

(defun infer-rec-node-type (node expected-type subs env)
  "Infer the type of a parser REC node.

Simple REC forms are handled directly so init binding types can seed the
recursive parameter types before local accessor solving. More complex forms are
lowered back to the ordinary nested-let representation and inferred there."
  (declare (type parser:node-rec node)
           (type tc:ty expected-type)
           (type tc:substitution-list subs)
           (type tc-env env)
           (values tc:ty tc:ty-predicate-list accessor-list node-let tc:substitution-list))

  (when (not (rec-node-bindings-independent-p node))
    (return-from infer-rec-node-type
      (infer-expression-type (lower-rec-node-to-let node)
                             expected-type
                             subs
                             env)))

  (check-duplicates
   (mapcan #'parser:pattern-variables (parser:node-rec-params node))
   #'parser:pattern-var-name
   (lambda (first second)
     (tc-error "Duplicate definition in rec"
               (tc-note first "first definition here")
               (tc-note second "second definition here"))))

  (let ((preds nil)
        (accessors nil)
        (init-binding-info nil)
        (call-env env)
        (declare-table (rec-node-declare-table node)))
    ;; `rec` init bindings are only used to seed the immediate recursive call,
    ;; so keep them monomorphic here instead of routing through ordinary `let`
    ;; generalization/defaulting.
    (dolist (binding (parser:node-rec-bindings node))
      (let ((declared-type
              (gethash (parser:node-variable-name (parser:node-let-binding-name binding))
                       declare-table)))
        (multiple-value-bind (binding-preds typed-binding binding-qual-ty binding-scheme subs_)
            (infer-rec-init-binding-type binding declared-type subs env)
          (setf subs subs_)
          (setf call-env
                (tc-env-shadow-definition call-env
                                          (node-variable-name (node-let-binding-name typed-binding))
                                          binding-scheme))
          (setf preds (append preds binding-preds))
          (push (cons typed-binding binding-qual-ty) init-binding-info))))

    (setf init-binding-info (nreverse init-binding-info))
    (tc:apply-substitution subs call-env)

    (let* ((arg-info
             (loop :for arg :in (parser:node-rec-call-args node)
                   :collect
                     (multiple-value-bind (arg-ty arg-preds)
                         (tc-env-lookup-value call-env arg)
                       (setf preds (append preds arg-preds))
                       (list arg arg-ty arg-preds))))
           (param-types
             (loop :for (_arg arg-ty _preds) :in arg-info
                   :collect (tc:apply-substitution subs arg-ty)))
           (result-type
             (tc:make-variable))
           (rec-function-type
             (tc:make-function-type* param-types result-type))
           (rec-name
             (parser:node-variable-name (parser:node-rec-name node)))
           (inner-binding
             (rec-node-inner-binding node))
           (binding-env
             (tc-env-shadow-definition call-env rec-name (tc:to-scheme rec-function-type))))

      (multiple-value-bind (binding-preds binding-accessors typed-binding subs)
          (infer-binding-type inner-binding rec-function-type subs binding-env)

        (tc:apply-substitution subs binding-env)

        ;; The init bindings above determine the recursive parameter types, but
        ;; class fundeps may still need to fire before accessors are concrete.
        (setf binding-preds (tc:apply-substitution subs binding-preds))
        (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) binding-preds subs)))
        (setf binding-accessors (tc:apply-substitution subs binding-accessors))

        (multiple-value-bind (binding-accessors subs_)
            (solve-accessors binding-accessors (tc-env-env env))
          (setf subs (tc:compose-substitution-lists subs subs_))

          (when binding-accessors
            (tc-error "Ambiguous accessor"
                      (tc-note (first binding-accessors)
                               "accessor is ambiguous")))

          (setf binding-preds (tc:apply-substitution subs binding-preds))
          (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) binding-preds subs)))
          (tc:apply-substitution subs call-env)

          (handler-case
              (setf subs (tc:unify subs (tc:apply-substitution subs result-type) expected-type))
            (tc:coalton-internal-type-error ()
              (standard-expression-type-mismatch-error node subs expected-type result-type)))

          (setf binding-preds (tc:apply-substitution subs binding-preds))
          (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) binding-preds subs)))

          (let* ((ty
                   (tc:apply-substitution subs result-type))
                 (rec-qual-ty
                   (tc:qualify binding-preds
                               (tc:apply-substitution subs rec-function-type)))
                 (rewrite-table
                   (let ((table (make-hash-table :test #'eq)))
                     (setf (gethash rec-name table) rec-qual-ty)
                     table))
                 (typed-inner-binding
                   (rewrite-recursive-calls
                    (attach-explicit-binding-type
                     (tc:apply-substitution subs typed-binding)
                     rec-qual-ty)
                    rewrite-table))
                 (typed-rator
                   (make-node-variable
                    :type rec-qual-ty
                    :location (source:location (parser:node-rec-name node))
                    :name rec-name))
                 (typed-rands
                   (loop :for (arg arg-ty arg-preds) :in arg-info
                         :collect
                           (make-node-variable
                            :type (tc:qualify (tc:apply-substitution subs arg-preds)
                                              (tc:apply-substitution subs arg-ty))
                            :location (source:location arg)
                            :name (parser:node-variable-name arg))))
                 (typed-application
                   (make-node-application
                    :type (tc:qualify nil ty)
                    :location (source:location node)
                    :rator typed-rator
                    :rands typed-rands
                    :keyword-rands nil))
                 (typed-inner-let
                   (make-node-let
                    :type (tc:qualify nil ty)
                    :location (source:location node)
                    :bindings (list typed-inner-binding)
                    :body (make-node-body
                           :nodes nil
                           :last-node typed-application)))
                 (typed-init-bindings
                   (loop :for (binding . qual-ty) :in init-binding-info
                         :collect
                           (attach-explicit-binding-type
                            (tc:apply-substitution subs binding)
                            (tc:apply-substitution subs qual-ty)))))
            (setf preds (tc:apply-substitution subs preds))
            (setf preds (append preds binding-preds))
            (values
             ty
             preds
             accessors
             (make-node-let
              :type (tc:qualify nil ty)
              :location (source:location node)
              :bindings typed-init-bindings
              :body (make-node-body
                     :nodes nil
                     :last-node typed-inner-let))
             subs)))))))

(defgeneric infer-expression-type (node expected-type subs env)
  (:documentation "Infer the type of NODE and then unify against EXPECTED-TYPE

Returns (VALUES INFERRED-TYPE PREDICATES NODE SUBSTITUTIONS)")
  (:method :around (node expected-type subs env)
    (declare (ignore node expected-type subs))
    (with-type-string-environment (env)
      (call-next-method)))
  (:method ((node parser:node-literal) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-literal tc:substitution-list &optional))

    (let ((ty (etypecase (parser:node-literal-value node)
                (ratio        tc:*fraction-type*)
                (single-float tc:*single-float-type*)
                (double-float tc:*double-float-type*)
                (string       tc:*string-type*)
                (character    tc:*char-type*))))

      (handler-case
          (progn
            (setf subs (tc:unify subs ty expected-type))
            (let ((type (tc:apply-substitution subs ty)))
              (values
               type
               nil
               nil
               (make-node-literal
                :type (tc:qualify nil type)
                :location (source:location node)
                :value (parser:node-literal-value node))
               subs)))

        (tc:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node subs expected-type ty)))))

  (:method ((node parser:node-accessor) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-accessor tc:substitution-list))

    (let* ((from-ty
             (tc:make-variable))
           (to-ty
             (tc:make-variable))
           (ty
             (tc:make-function-type from-ty to-ty)))
      (handler-case
          (progn
            (setf subs (tc:unify subs ty expected-type))
            (let ((type (tc:apply-substitution subs ty)))
              (values
               type
               nil
               (list
                (make-accessor
                 :from from-ty
                 :to to-ty
                 :field (parser:node-accessor-name node)
                 :location (source:location node)))
               (make-node-accessor
                :type (tc:qualify nil type)
                :location (source:location node)
                :name (parser:node-accessor-name node))
               subs))))))

  (:method ((node parser:node-integer-literal) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-integer-literal tc:substitution-list &optional))
    
    (let* ((num
             (util:find-symbol "NUM" "COALTON/CLASSES"))
           (tvar
             (tc:make-variable))
           (pred
             (tc:make-ty-predicate :class num :types (list tvar) :location (source:location node))))
      (handler-case
          (progn
            (setf subs (tc:unify subs tvar expected-type))
            (let ((type (tc:apply-substitution subs tvar)))
              (values
               type
               (list pred)
               nil
               (make-node-integer-literal
                :type (tc:qualify nil type)
                :location (source:location node)
                :value (parser:node-integer-literal-value node))
               subs)))
        (tc:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node subs expected-type tvar)))))

  (:method ((node parser:node-variable) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node tc:substitution-list &optional))

    (multiple-value-bind (ty preds)
        (tc-env-lookup-value env node)
      (let ((effective-ty (tc:apply-substitution subs ty)))
        (handler-case
            (progn
              (cond
                ((and (typep effective-ty 'tc:function-ty)
                      (typep expected-type 'tc:function-ty))
                 (multiple-value-bind (subs_ visible-type narrowed-type wrapper-needed-p)
                     (coerce-function-value-type effective-ty expected-type subs)
                   (declare (ignore wrapper-needed-p))
                   (setf subs subs_)
                   (when (and (not (tc:ty= effective-ty narrowed-type))
                              (typep narrowed-type 'tc:function-ty))
                     (maybe-update-local-function-type env node narrowed-type))
                   (let ((preds (tc:apply-substitution subs preds)))
                     (values
                      visible-type
                      preds
                      nil
                      (make-node-variable
                       :type (tc:qualify preds visible-type)
                       :location (source:location node)
                       :name (parser:node-variable-name node))
                      subs))))
                (t
                 (setf subs (tc:unify subs effective-ty expected-type))
                 (let ((type (tc:apply-substitution subs effective-ty))
                       (preds (tc:apply-substitution subs preds)))
                   (values
                    type
                    preds
                    nil
                    (make-node-variable
                     :type (tc:qualify preds type)
                     :location (source:location node)
                     :name (parser:node-variable-name node))
                    subs)))))
          (tc:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node subs expected-type effective-ty))))))

  (:method ((node parser:node-values) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-values tc:substitution-list &optional))

    (let ((preds nil)
          (accessors nil)
          (typed-values nil)
          (component-types nil))
      (dolist (raw-value (parser:node-values-nodes node))
        (multiple-value-bind (component-ty preds_ accessors_ typed-value subs_)
            (infer-expression-type raw-value (tc:make-variable) subs env)
          (setf subs subs_)
          (setf preds (append preds preds_))
          (setf accessors (append accessors accessors_))
          (push typed-value typed-values)
          (push (tc:apply-substitution subs component-ty) component-types)))
      (setf typed-values (nreverse typed-values))
      (setf component-types (nreverse component-types))
      (let ((result-ty (values-result-type component-types)))
        (handler-case
            (progn
              (setf subs (tc:unify subs result-ty expected-type))
              (let ((type (tc:apply-substitution subs result-ty)))
                (values
                 type
                 preds
                 accessors
                 (make-node-values
                  :type (tc:qualify nil type)
                  :location (source:location node)
                  :nodes typed-values)
                 subs)))
          (tc:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node subs expected-type result-ty))))))

  (:method ((node parser:node-application) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-application tc:substitution-list &optional))

    (multiple-value-bind (fun-ty preds accessors rator-node subs)
        (infer-expression-type (parser:node-application-rator node)
                               (tc:make-variable)
                               subs
                               env)

      (let* ((positional-rands (or (parser:node-application-rands node) nil))
             (keyword-rands (or (parser:node-application-keyword-rands node) nil))
             (fun-ty_ (tc:apply-substitution subs fun-ty))
             (typed-rands nil)
             (typed-keyword-rands nil))

        (when (tc:tyvar-p fun-ty_)
          (multiple-value-bind (resolved-ty subs_)
              (resolve-tyvar-application-type fun-ty_ node expected-type subs
                                              positional-rands keyword-rands)
            (setf fun-ty_ resolved-ty
                  subs subs_)))

        (unless (tc:function-type-p fun-ty_)
          (tc-error "Argument error"
                    (tc-note node
                             "Unable to call value of type '~A': it is not a function"
                             (type-object-string (tc:apply-substitution subs fun-ty_)))))

        (unless (= (length positional-rands) (tc:function-input-arity fun-ty_))
          (tc-error "Argument error"
                    (tc-note node
                             "Function call has ~D positional arguments but inferred type '~A' takes ~D"
                             (length positional-rands)
                             (type-object-string (tc:apply-substitution subs fun-ty_))
                             (tc:function-input-arity fun-ty_))))

        (loop :for rand :in positional-rands
              :for expected-rand-ty :in (tc:function-type-arguments fun-ty_)
              :do
                 (multiple-value-bind (ty_ preds_ accessors_ node_ subs_)
                     (infer-expression-type rand expected-rand-ty subs env)
                   (declare (ignore ty_))
                   (setf preds (append preds preds_))
                   (setf accessors (append accessors accessors_))
                   (setf subs subs_)
                   (push node_ typed-rands)))

        (dolist (arg keyword-rands)
          (let* ((key (parser:keyword-src-name (parser:node-application-keyword-arg-keyword arg)))
                 (entry (function-keyword-entry fun-ty_ key)))
            (when (and (null entry)
                       (tc:function-ty-keyword-open-p fun-ty_))
              (let ((new-entry (tc:make-keyword-ty-entry
                                :keyword key
                                :type (tc:make-variable))))
                (setf fun-ty_ (extend-function-type-keywords fun-ty_ (list new-entry)))
                (maybe-update-local-function-type env
                                                  (parser:node-application-rator node)
                                                  fun-ty_)
                (setf entry (function-keyword-entry fun-ty_ key))))
            (unless entry
              (tc-error "Unknown keyword argument"
                        (tc-note arg "unknown keyword argument ~S" key)))
            (multiple-value-bind (ty_ preds_ accessors_ node_ subs_)
                (infer-expression-type (parser:node-application-keyword-arg-value arg)
                                       (tc:keyword-ty-entry-type entry)
                                       subs
                                       env)
             (declare (ignore ty_))
              (setf preds (append preds preds_))
              (setf accessors (append accessors accessors_))
              (setf subs subs_)
              (push (make-node-application-keyword-arg
                     :keyword key
                     :value node_)
                    typed-keyword-rands))))

        (setf typed-rands (nreverse typed-rands)
              typed-keyword-rands (nreverse typed-keyword-rands)
              fun-ty_ (tc:apply-substitution subs fun-ty_))

        (let ((result-ty (tc:function-return-type fun-ty_)))
          (handler-case
              (progn
                (setf subs (tc:unify subs result-ty expected-type))
                (let ((type (tc:apply-substitution subs result-ty)))
                  (values type
                          preds
                          accessors
                          (make-node-application
                           :type (tc:qualify nil type)
                           :location (source:location node)
                           :rator (tc:apply-substitution subs rator-node)
                           :rands (tc:apply-substitution subs typed-rands)
                           :keyword-rands (tc:apply-substitution subs typed-keyword-rands))
                          subs)))
            (tc:coalton-internal-type-error ()
              (standard-expression-type-mismatch-error node subs expected-type result-ty)))))))

  (:method ((node parser:node-bind) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values null tc:ty-predicate-list accessor-list node-bind tc:substitution-list))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-bind-expr node)
                               (tc:make-variable)
                               subs
                               env)

      ;; A let-bound variable requires exactly one value.  Void (zero
      ;; values) and multi-value results cannot be bound to a variable
      ;; -- use `let (values ...) = ...` instead.
      ;; Wildcard patterns (`let _ = ...`) are exempt: the programmer
      ;; is explicitly discarding the result.
      (let ((resolved-ty (tc:apply-substitution subs expr-ty)))
        (when (and (typep resolved-ty 'tc:result-ty)
                   (not (typep (parser:node-bind-pattern node)
                               'parser:pattern-wildcard)))
          (tc-error "Cannot bind to variable"
                    (tc-note node
                             "expression produces ~A but a let binding requires a single value"
                             (type-object-string resolved-ty)))))

      (multiple-value-bind (pat-ty pat-node subs)
          (infer-pattern-type (parser:node-bind-pattern node)
                              expr-ty   ; unify against expr-ty
                              subs
                              env)
        (declare (ignore pat-ty))

        (values nil         ; return nil as this is always thrown away
                preds
                accessors
                (make-node-bind
                 ;; NOTE: We don't attach type here because NODE-BIND has no
                 ;; meaningful type.
                 :location (source:location node)
                 :pattern pat-node
                 :expr expr-node)
                subs))))

  (:method ((node parser:node-values-bind) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values null tc:ty-predicate-list accessor-list node-values-bind tc:substitution-list))

    (let* ((component-types (loop :for _pattern :in (parser:node-values-bind-patterns node)
                                  :collect (tc:make-variable)))
           (expected-expr-type (values-result-type component-types)))
      (multiple-value-bind (expr-ty preds accessors expr-node subs)
          (let ((*direct-values-node* (parser:node-values-bind-expr node))
                (*direct-values-output-types* component-types))
            (infer-expression-type (parser:node-values-bind-expr node)
                                   expected-expr-type
                                   subs
                                   env))
        (let ((actual-arity (tc:multiple-value-output-arity expr-ty))
              (expected-arity (length component-types)))
          (unless (= actual-arity expected-arity)
            (tc-error "Values arity mismatch"
                      (tc-note node
                               "`values` binding expects ~D value~:P, but expression has ~D"
                               expected-arity
                               actual-arity)))
          (check-duplicates (parser:pattern-variables (parser:node-values-bind-patterns node))
                            #'parser:pattern-var-name
                            (lambda (first second)
                              (tc-error "Duplicate pattern variable"
                                        (tc-note first "first definition here")
                                        (tc-note second "second definition here"))))
          (let ((pattern-nodes nil))
            (loop :for subpattern :in (parser:node-values-bind-patterns node)
                  :for component-type :in (mapcar (lambda (type)
                                                    (tc:apply-substitution subs type))
                                                  component-types)
                  :do
                     (unless (typep subpattern '(or parser:pattern-var parser:pattern-wildcard))
                       (tc-error "Invalid values binding pattern"
                                 (tc-note subpattern
                                          "`values` bindings only support variables and `_`")))
                     (multiple-value-bind (_sub-ty sub-node subs_)
                         (infer-pattern-type subpattern component-type subs env)
                       (declare (ignore _sub-ty))
                       (setf subs subs_)
                       (push sub-node pattern-nodes)))
            (setf pattern-nodes (nreverse pattern-nodes))
            (values nil
                    preds
                    accessors
                    (make-node-values-bind
                     :location (source:location node)
                     :patterns pattern-nodes
                     :expr expr-node)
                    subs))))))

  (:method ((node parser:node-body) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-body tc:substitution-list))

    (let* ((preds nil)
           (accessors nil)

           ;; Infer the type of each node
           (body-nodes
             (loop :for node_ :in (parser:node-body-nodes node)
                   :collect (multiple-value-bind (node_ty_ preds_ accessors_ node_ subs_)
                                (infer-expression-type node_ (tc:make-variable) subs env)
                              (declare (ignore node_ty_))
                              (setf subs subs_)
                              (setf preds (append preds preds_))
                              (setf accessors (append accessors accessors_))
                              node_))))

      (multiple-value-bind (ty preds_ accessors_ last-node subs)
          (infer-expression-type (parser:node-body-last-node node) expected-type subs env)
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (values
         ty
         preds
         accessors
         (make-node-body
          :nodes body-nodes
          :last-node last-node)
         subs))))

  (:method ((node parser:node-abstraction) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-abstraction tc:substitution-list &optional))

    (let* ((effective-expected-type (tc:apply-substitution subs expected-type))
           (positional-params (parser:node-abstraction-params node))
           (keyword-params (parser:node-abstraction-keyword-params node)))
      (ensure-no-duplicate-function-parameters positional-params keyword-params)

      (when (and (not (tc:tyvar-p effective-expected-type))
                 (tc:function-type-p effective-expected-type)
                 (/= (length positional-params)
                     (tc:function-input-arity effective-expected-type)))
        (tc-error "Arity mismatch"
                  (tc-note node
                           "Function definition has ~D positional parameter~:P but expected type '~A' takes ~D"
                           (length positional-params)
                           (type-object-string effective-expected-type)
                           (tc:function-input-arity effective-expected-type))))

      (let* ((body-return-block
               (body-return-block-node (parser:node-abstraction-body node)))
             (body-result-ty (tc:make-variable))
             (*return-blocks*
               (if body-return-block
                   (acons (parser:node-block-name body-return-block)
                          body-result-ty
                          *return-blocks*)
                   *return-blocks*))
             (expected-function-type (and (tc:function-type-p effective-expected-type)
                                          effective-expected-type))
             (expected-keyword-table (make-hash-table :test #'eq))
             (expected-positional-input-types
               (and expected-function-type
                    (copy-list (tc:function-type-arguments expected-function-type))))
             (positional-arg-tys
               (loop :for pattern :in positional-params
                     :for expected-arg-ty := (pop expected-positional-input-types)
                     :collect (or expected-arg-ty
                                  (tc:make-variable))))
             (typed-positional-params
               (loop :for pattern :in positional-params
                     :for ty :in positional-arg-tys
                     :collect (multiple-value-bind (ty_ pattern_ subs_)
                                  (infer-pattern-type pattern ty subs env)
                                (declare (ignore ty_))
                                (setf subs subs_)
                                pattern_))))

        (when expected-function-type
          (dolist (entry (tc:function-ty-keyword-input-types expected-function-type))
            (setf (gethash (tc:keyword-ty-entry-keyword entry) expected-keyword-table) entry)))

        (multiple-value-bind (typed-keyword-params keyword-entry-types keyword-prefix-nodes
                              default-preds default-accessors subs)
            (infer-keyword-params keyword-params expected-keyword-table subs env)

          (multiple-value-bind (body-ty preds accessors body-node subs)
              (infer-expression-type (parser:node-abstraction-body node)
                                     body-result-ty
                                     subs
                                     env)
            (setf preds (append default-preds preds))
            (setf accessors (append default-accessors accessors))
            (let* ((body-ty (tc:apply-substitution subs body-ty))
                   (typed-body
                     (merge-keyword-prefix-nodes-into-body
                      (nreverse keyword-prefix-nodes)
                      body-node))
                   (output-types (inferred-node-output-types body-ty typed-body))
                   (ty (tc:make-function-ty
                        :positional-input-types positional-arg-tys
                        :keyword-input-types (normalize-keyword-entries (nreverse keyword-entry-types))
                        :keyword-open-p nil
                        :output-types output-types)))
              (handler-case
                  (progn
                    (let ((effective-expected-type (tc:apply-substitution subs expected-type)))
                    (let ((type nil))
                      (cond
                        ((and (typep ty 'tc:function-ty)
                              (typep effective-expected-type 'tc:function-ty))
                         (multiple-value-bind (subs_ visible-type _effective-type _wrapper-needed-p)
                             (coerce-function-value-type ty effective-expected-type subs)
                           (declare (ignore _effective-type _wrapper-needed-p))
                           (setf subs subs_)
                           (setf type visible-type)))
                        (t
                         (setf subs (tc:unify subs ty effective-expected-type))
                         (setf type (tc:apply-substitution subs ty))))
                      (values
                       type
                       preds
                       accessors
                       (make-node-abstraction
                        :type (tc:qualify nil type)
                        :location (source:location node)
                        :params typed-positional-params
                        :keyword-params (nreverse typed-keyword-params)
                        :body typed-body)
                       subs))))
                (tc:coalton-internal-type-error ()
                  (standard-expression-type-mismatch-error node subs effective-expected-type ty)))))))))

  (:method ((node parser:node-rec) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-let tc:substitution-list))

    (infer-rec-node-type node expected-type subs env))

  (:method ((node parser:node-let) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-let tc:substitution-list))

    ;; Ensure that there are no duplicate let bindings
    (check-duplicates
     (parser:node-let-bindings node)
     (alexandria:compose #'parser:node-variable-name #'parser:node-let-binding-name)
     (lambda (first second)
       (tc-error "Duplicate definition in let"
                 (tc-note first "first definition here")
                 (tc-note second "second definition here"))))

    (multiple-value-bind (preds accessors binding-nodes subs)
        (cond
          ((parser:node-let-sequential-p node)
           (infer-sequential-let-bindings
            (parser:node-let-bindings node)
            (parser:node-let-declares node)
            subs
            env))
          (t
           (infer-let-bindings
            (parser:node-let-bindings node)
            (parser:node-let-declares node)
            subs
            env)))

      (multiple-value-bind (ty preds_ accessors_ body-node subs)
          (infer-expression-type (parser:node-let-body node)
                                 expected-type ; pass through expected type
                                 subs
                                 env)
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (values
         ty
         preds
         accessors
         (if (parser:node-let-sequential-p node)
             (nest-sequential-let-bindings
              binding-nodes
              body-node
              ty
              (source:location node))
             (make-node-let
              :type (tc:qualify nil ty)
              :location (source:location node)
              :bindings binding-nodes
              :body body-node))
         subs))))

  (:method ((node parser:node-dynamic-let) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-dynamic-let tc:substitution-list))

    (check-duplicates
     (parser:node-dynamic-let-bindings node)
     (alexandria:compose #'parser:node-variable-name #'parser:node-dynamic-binding-name)
     (lambda (first second)
       (tc-error "Duplicate binding in dynamic-bind"
                 (tc-note first "first binding here")
                 (tc-note second "second binding here"))))

    (let ((preds nil)
          (binding-nodes nil))
      (dolist (binding (parser:node-dynamic-let-bindings node))
        (multiple-value-bind (preds_ binding-node subs_)
            (infer-dynamic-binding-type binding subs env)
          (setf subs subs_)
          (setf preds (append preds preds_))
          (push binding-node binding-nodes)))

      (multiple-value-bind (ty preds_ accessors body-node subs)
          (infer-expression-type (parser:node-dynamic-let-subexpr node)
                                 expected-type
                                 subs
                                 env)
        (values
         ty
         (append preds preds_)
         accessors
         (make-node-dynamic-let
          :type (tc:qualify nil ty)
          :location (source:location node)
          :bindings (nreverse binding-nodes)
          :subexpr body-node)
         subs))))

  (:method ((node parser:node-lisp) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-lisp tc:substitution-list &optional))

    (let ((declared-ty
            (tc:output-types-result-type
             (mapcar (lambda (output-type)
                       (parse-type output-type (tc-env-parser-env env)))
                     (parser:node-lisp-output-types node)))))

      (handler-case
          (progn
            (setf subs (tc:unify subs declared-ty expected-type))
            (let ((type (tc:apply-substitution subs declared-ty))

                  (var-nodes
                    (mapcar (lambda (var)
                              (multiple-value-bind (var-ty var-preds)
                                  (tc-env-lookup-value env var)
                                (make-node-variable
                                 :type (tc:qualify var-preds var-ty)
                                 :location (source:location var)
                                 :name (parser:node-variable-name var))))
                            (parser:node-lisp-vars node))))
              (values
               type
               nil
               nil
               (make-node-lisp
                :type (tc:qualify nil type)
                :location (source:location node)
                :vars var-nodes
                :var-names (parser:node-lisp-var-names node)
                :body (parser:node-lisp-body node))
               subs)))
        (tc:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node subs expected-type declared-ty)))))

  (:method ((node parser:node-match) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-match tc:substitution-list &optional))

    ;; Infer the type of the expression being cased on
    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-match-expr node)
                               (tc:make-variable)
                               subs
                               env)

      (let* (;; Infer the type of each pattern, unifying against expr-ty
             (pat-nodes
               (loop :for branch :in (parser:node-match-branches node)
                     :for pattern := (parser:node-match-branch-pattern branch)
                     :collect (multiple-value-bind (pat-ty pat-node subs_)
                                  (infer-pattern-type pattern expr-ty subs env)
                                (declare (ignore pat-ty))
                                (setf subs subs_)
                                pat-node)))

             (ret-ty (tc:make-variable))

             ;; Infer the type of each branch, unifying against ret-ty
             (branch-body-nodes
               (loop :for branch :in (parser:node-match-branches node)
                     :for body := (parser:node-match-branch-body branch)
                     :collect (multiple-value-bind (body-ty preds_ accessors_ body-node subs_)
                                  (infer-expression-type body ret-ty subs env)
                                (declare (ignore body-ty))
                                (setf subs subs_)
                                (setf preds (append preds preds_))
                                (setf accessors (append accessors accessors_))
                                body-node)))

             (branch-nodes
               (loop :for branch :in (parser:node-match-branches node)
                     :for pat-node :in pat-nodes
                     :for branch-body-node :in branch-body-nodes
                     :collect (make-node-match-branch
                               :pattern pat-node
                               :body branch-body-node
                               :location (source:location branch)))))

        (handler-case
            (progn
              (setf subs (tc:unify subs ret-ty expected-type))
              (let ((type (tc:apply-substitution subs ret-ty)))
                (values
                 type
                 preds
                 accessors
                 (make-node-match
                  :type (tc:qualify nil type)
                  :location (source:location node)
                  :expr expr-node
                  :branches branch-nodes)
                 subs)))
          (tc:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node subs expected-type ret-ty))))))

  (:method ((node parser:node-catch) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-catch tc:substitution-list &optional))
    ;; Infer type of the expression that may throw an exception
    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-catch-expr node)
                               (tc:make-variable)
                               subs
                               env)

      (let* (;; Infer type of each pattern, ensuring it is an exception type
             (branch-pat-nodes
               (loop
                 :for branch :in (parser:node-catch-branches node)
                 :for pattern := (parser:node-catch-branch-pattern branch)
                 :for (pat-ty pat-node subs_)
                   := (multiple-value-list (infer-pattern-type pattern (tc:make-variable) subs env))
                 :unless (or (exception-type-p pat-ty env)
                             (typep pattern 'parser:pattern-wildcard))
                   :do (tc-error
                        "Invalid catch case"
                        (tc-note
                         pat-node
                         "Catch branch pattern must be an exception constructor pattern or a wildcard."))
                 :else 
                   :do (setf subs subs_)
                   :and :collect pat-node))
             ;; Infer type of each branch body, unifying against expr-ty
             (branch-body-nodes
               (loop
                 :for branch :in (parser:node-catch-branches node)
                 :for body := (parser:node-catch-branch-body branch)
                 :collect (multiple-value-bind (body-ty preds_ accessors_ body-node subs_)
                              (infer-expression-type body expr-ty subs env)
                            (declare (ignore body-ty))
                            (setf subs subs_)
                            (setf preds (append preds preds_))
                            (setf accessors (append accessors accessors_))
                            body-node)))

             (branch-nodes
               (loop
                 :for branch :in (parser:node-catch-branches node)
                 :for pat-node :in branch-pat-nodes
                 :for branch-body-node :in branch-body-nodes
                 :collect (make-node-catch-branch
                           :pattern pat-node
                           :body branch-body-node
                           :location (source:location branch)))))
        (handler-case
            (progn
              (setf subs (tc:unify subs expr-ty expected-type))
              (let ((type (tc:apply-substitution subs expr-ty)))
                (values
                 type
                 preds
                 accessors
                 (make-node-catch
                  :type (tc:qualify nil type)
                  :location (source:location node)
                  :expr expr-node
                  :branches branch-nodes)
                 subs)))
          (tc:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node subs expected-type expr-ty))))))

  (:method ((node parser:node-resumable) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-resumable tc:substitution-list &optional))
    
    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-resumable-expr node)
                               (tc:make-variable)
                               subs
                               env)
      (let* (;; infer type of each pattern, ensuring it is a resumption type
             (branch-pat-nodes
               (loop
                 :for branch :in (parser:node-resumable-branches node)
                 :for pattern := (parser:node-resumable-branch-pattern branch)
                 :for (pat-ty pat-node subs_)
                   := (multiple-value-list (infer-pattern-type pattern (tc:make-variable) subs env))
                 :unless (resumption-type-p pat-ty env)
                   :do (tc-error "Invalid resumable case"
                                 (tc-note pat-node "case pattern must construct a resumption type."))
                 :else 
                   :do (setf subs subs_)
                   :and :collect pat-node))
             ;; Infer type of each branch body, it should unify with the expr/expected type
             (branch-body-nodes
               (loop
                 :for branch :in (parser:node-resumable-branches node)
                 :for body := (parser:node-resumable-branch-body branch)
                 :collect (multiple-value-bind (body-ty preds_ accessors_ body-node subs_)
                              (infer-expression-type body expr-ty subs env)
                            (declare (ignore body-ty))
                            (setf subs subs_)
                            (setf preds (append preds preds_))
                            (setf accessors (append accessors accessors_))
                            body-node)))
             (branch-nodes
               (loop
                 :for branch :in (parser:node-resumable-branches node)
                 :for pat-node :in branch-pat-nodes
                 :for branch-body-node :in branch-body-nodes
                 :collect (make-node-resumable-branch
                           :pattern pat-node
                           :body branch-body-node
                           :location (source:location branch)))))
        (handler-case
            (progn
              (setf subs (tc:unify subs expr-ty expected-type))
              (let ((type (tc:apply-substitution subs expr-ty)))
                (values
                 type
                 preds
                 accessors
                 (make-node-resumable
                  :type (tc:qualify nil type)
                  :location (source:location node)
                  :expr expr-node
                  :branches branch-nodes)
                 subs)))
          (tc:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node subs expected-type expr-ty))))))

  (:method ((node parser:node-progn) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-progn tc:substitution-list))

    (multiple-value-bind (body-ty preds accessors body-node subs)
        (infer-expression-type (parser:node-progn-body node)
                               expected-type
                               subs
                               env)
      (values
       body-ty
       preds
       accessors
       (make-node-progn
        :type (tc:qualify nil body-ty)
        :location (source:location node)
        :body body-node)
       subs)))

  (:method ((node parser:node-type-of) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-lisp tc:substitution-list &optional))

    (let* ((type-scheme-name (util:find-symbol "TYPESCHEME" "COALTON/TYPES"))
           (type-scheme-type (tc:type-entry-type (tc:lookup-type (tc-env-env env) type-scheme-name))))
      (handler-case
          (progn
            (setf subs (tc:unify subs type-scheme-type expected-type))
            (multiple-value-bind (scheme subs)
                (reflect-expression-type-scheme (parser:node-type-of-expr node) subs env)
              (let ((type (tc:apply-substitution subs type-scheme-type)))
                (values
                 type
                 nil
                 nil
                 (make-node-lisp
                  :type (tc:qualify nil type)
                  :location (source:location node)
                  :vars nil
                  :var-names nil
                  :body (list (util:runtime-quote scheme)))
                 subs))))
        (tc:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node subs expected-type type-scheme-type)))))
  (:method ((node parser:node-unsafe) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-unsafe tc:substitution-list))

    (multiple-value-bind (body-ty preds accessors body-node subs)
        (infer-expression-type (parser:node-unsafe-body node)
                               expected-type
                               subs
                               env)
      (values
       body-ty
       preds
       accessors
       (make-node-unsafe
        :type (tc:qualify nil body-ty)
        :location (source:location node)
       :body body-node)
       subs)))

  (:method ((node parser:node-block) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-block tc:substitution-list))

    (let* ((block-type (tc:make-variable))
           (block-info (make-return-block-info :type block-type)))
      (multiple-value-bind (body-ty preds accessors body-node subs)
          (let ((*return-blocks*
                  (acons (parser:node-block-name node)
                         block-info
                         *return-blocks*)))
            (infer-expression-type (parser:node-block-body node)
                                   block-type
                                   subs
                                   env))
        (let ((first-return
                (return-block-info-first-return block-info)))
        (when first-return
          (handler-case
              (setf subs (tc:unify subs (cdr first-return) body-ty))
            (tc:coalton-internal-type-error ()
              (tc-error "Return type mismatch"
                        (tc-note (car first-return)
                                 "First return is of type '~A'"
                                 (type-object-string (tc:apply-substitution subs (cdr first-return))))
                        (tc-note (parser:node-body-last-node (parser:node-block-body node))
                                 "Second return is of type '~A'"
                                 (type-object-string (tc:apply-substitution subs body-ty)))))))
        (handler-case
            (progn
              (setf subs (tc:unify subs block-type expected-type))
              (let ((type (tc:apply-substitution subs block-type)))
                (values
                 type
                 preds
                 accessors
                 (make-node-block
                  :type (tc:qualify nil type)
                 :location (source:location node)
                  :name (parser:node-block-name node)
                  :body body-node)
                 subs)))
          (tc:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node subs expected-type body-ty)))))))

  (:method ((node parser:node-the) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node tc:substitution-list &optional))

    (multiple-value-bind (declared-qual-ty explicit-tvars explicit-p)
        (parse-the-type-annotation (parser:node-the-type node) env)
      (let* ((declared-ty (tc:qualified-ty-type declared-qual-ty))
             (expr-env (if explicit-p
                           (tc-env-extend-type-variable-scope
                            env
                            (remove-duplicates explicit-tvars :test #'tc:ty=))
                           env)))

        (multiple-value-bind (expr-ty preds accessors expr-node subs)
            (infer-expression-type (parser:node-the-expr node)
                                   (tc:make-variable)
                                   subs
                                   expr-env)

          ;; Ensure subs are applied
          (setf expr-ty (tc:apply-substitution subs expr-ty))

          ;; Check that declared-ty and expr-ty unify
          (handler-case
              (setf subs (tc:unify subs declared-ty expr-ty))
            (tc:coalton-internal-type-error ()
              (tc-error "Type mismatch"
                        (tc-note node "Declared type '~A' does not match inferred type '~A'"
                                 (type-object-string (tc:apply-substitution subs declared-ty))
                                 (type-object-string (tc:apply-substitution subs expr-ty))))))

          ;; Check that declared-ty is not more specific than expr-ty
          (handler-case
              (tc:match expr-ty declared-ty)
            (tc:coalton-internal-type-error ()
              (tc-error "Declared type too general"
                        (tc-note node "Declared type '~A' is more general than inferred type '~A'"
                                 (type-object-string (tc:apply-substitution subs declared-ty))
                                 (type-object-string (tc:apply-substitution subs expr-ty))))))

          ;; SAFETY: If declared-ty and expr-ty unify, and expr-ty is
          ;; more general than declared-ty then matching should be
          ;; infallible
          (setf subs (tc:compose-substitution-lists subs (tc:match expr-ty declared-ty)))

          (handler-case
              (progn
                (setf subs (tc:unify subs expr-ty expected-type))
                (values
                 (tc:apply-substitution subs expr-ty)
                 preds
                 accessors
                 expr-node
                 subs))
            (tc:coalton-internal-type-error ()
              (standard-expression-type-mismatch-error node subs expected-type expr-ty)))))))

  (:method ((node parser:node-collection-builder) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node tc:substitution-list &optional))
    (setf subs (check-homogeneous-builder-items
                :collection
                (parser:node-collection-builder-elements node)
                subs
                env))
    (infer-expression-type (desugar-parser-builder-node node) expected-type subs env))

  (:method ((node parser:node-association-builder) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node tc:substitution-list &optional))
    (setf subs (check-homogeneous-builder-items
                :association-key
                (mapcar #'parser:association-entry-key
                        (parser:node-association-builder-entries node))
                subs
                env))
    (setf subs (check-homogeneous-builder-items
                :association-value
                (mapcar #'parser:association-entry-value
                        (parser:node-association-builder-entries node))
                subs
                env))
    (infer-expression-type (desugar-parser-builder-node node) expected-type subs env))

  (:method ((node parser:node-collection-comprehension) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node tc:substitution-list &optional))
    (infer-expression-type (desugar-parser-builder-node node) expected-type subs env))

  (:method ((node parser:node-association-comprehension) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node tc:substitution-list &optional))
    (infer-expression-type (desugar-parser-builder-node node) expected-type subs env))

  (:method ((node parser:node-return) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node tc:substitution-list))

    (declare (ignore expected-type subs env))
    (util:coalton-bug "Unexpected parser:node-return after control-flow resolution: ~S" node))

  (:method ((node parser:node-return-from) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-return-from tc:substitution-list))

    (let ((block-info (return-block-info (parser:node-return-from-name node))))
      (multiple-value-bind (expr-ty preds accessors expr-node subs)
          (infer-expression-type (parser:node-return-from-expr node)
                                 (tc:make-variable)
                                 subs
                                 env)
        (let ((first-return (return-block-info-first-return block-info)))
          (if first-return
              (handler-case
                  (setf subs (tc:unify subs expr-ty (cdr first-return)))
                (tc:coalton-internal-type-error ()
                  (tc-error "Return type mismatch"
                            (tc-note (car first-return)
                                     "First return is of type '~A'"
                                     (type-object-string (tc:apply-substitution subs (cdr first-return))))
                            (tc-note node
                                     "Second return is of type '~A'"
                                     (type-object-string (tc:apply-substitution subs expr-ty))))))
              (setf (return-block-info-first-return block-info)
                    (cons (source:location node)
                          (tc:apply-substitution subs expr-ty)))))
        (let ((type (tc:apply-substitution subs expected-type)))
          (values
           type
           preds
           accessors
           (make-node-return-from
            :type (tc:qualify nil type)
            :location (source:location node)
            :name (parser:node-return-from-name node)
            :expr expr-node)
           subs)))))

  (:method ((node parser:node-throw) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-throw tc:substitution-list))

    (multiple-value-bind (exception-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-throw-expr node)
                               (tc:make-variable)
                               subs
                               env)

      (unless (exception-type-p exception-ty env)
        (tc-error
         "Invalid throw"
         (tc-note
          expr-node
          "Argument to `throw` must be a known exception.")
         (tc-note
          expr-node
          "Not Yet Supported: throw polymorphism.")))
      
      (values
       expected-type
       preds
       accessors
       (make-node-throw
        :type (tc:qualify nil expected-type)
        :location (source:location node)
        :expr expr-node)
       subs)))

  (:method ((node parser:node-resume-to) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-resume-to tc:substitution-list))

    (multiple-value-bind (resumption-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-resume-to-expr node)
                               (tc:make-variable)
                               subs
                               env)

      (unless (resumption-type-p resumption-ty env)
        (tc-error "Invalid resume-to"
                  (tc-note node "Argument to `resume-to` be a known resumption.")
                  (tc-note node "Not Yet Supported: resume-to polymorphism.")))
      
      (values
       expected-type
       preds
       accessors
       (make-node-resume-to
        :type (tc:qualify nil expected-type)
        :location (source:location node)
        :expr expr-node)
       subs)))

  (:method ((node parser:node-or) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-or tc:substitution-list))

    (let* ((preds nil)
           (accessors nil)

           (body-nodes
             (loop :for node_ :in (parser:node-or-nodes node)
                   :collect (multiple-value-bind (node_ty_ preds_ accessors_ node_ subs_)
                                (infer-expression-type node_
                                                       tc:*boolean-type*
                                                       subs
                                                       env)
                              (declare (ignore node_ty_))
                              (setf subs subs_)
                              (setf preds (append preds preds_))
                              (setf accessors (append accessors accessors_))
                              node_))))

      (handler-case
          (progn
            (setf subs (tc:unify subs tc:*boolean-type* expected-type))
            (values
             tc:*boolean-type*
             preds
             accessors
             (make-node-or
              :type (tc:qualify nil tc:*boolean-type*)
              :location (source:location node)
              :nodes body-nodes)
             subs))
        (tc:coalton-internal-type-error ()
          (tc-error "Type mismatch"
                    (tc-note node "Expected type '~A' but 'or' evaluates to '~A'"
                             (type-object-string (tc:apply-substitution subs expected-type))
                             (type-object-string tc:*boolean-type*)))))))
  
  (:method ((node parser:node-and) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-and tc:substitution-list))

    (let* ((preds nil)
           (accessors nil)

           (body-nodes
             (loop :for node_ :in (parser:node-and-nodes node)
                   :collect (multiple-value-bind (node_ty_ preds_ accessors_ node_ subs_)
                                (infer-expression-type node_
                                                       tc:*boolean-type*
                                                       subs
                                                       env)
                              (declare (ignore node_ty_))
                              (setf subs subs_)
                              (setf preds (append preds preds_))
                              (setf accessors (append accessors accessors_))
                              node_))))

      (handler-case
          (progn
            (setf subs (tc:unify subs tc:*boolean-type* expected-type))
            (values
             tc:*boolean-type*
             preds
             accessors
             (make-node-and
              :type (tc:qualify nil tc:*boolean-type*)
              :location (source:location node)
              :nodes body-nodes)
             subs))
        (tc:coalton-internal-type-error ()
          (tc-error "Type mismatch"
                    (tc-note node "Expected type '~A' but 'and' evaluates to '~A'"
                             (type-object-string (tc:apply-substitution subs expected-type))
                             (type-object-string tc:*boolean-type*)))))))

  (:method ((node parser:node-if) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-if tc:substitution-list &optional))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-if-expr node)
                               tc:*boolean-type* ; unify predicate against boolean
                               subs
                               env)
      (declare (ignore expr-ty))

      (multiple-value-bind (then-ty preds_ accessors_ then-node subs)
          (infer-expression-type (parser:node-if-then node)
                                 expected-type
                                 subs
                                 env)
        (declare (ignore then-ty))
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (multiple-value-bind (else-ty preds_ accessors_ else-node subs)
            (infer-expression-type (parser:node-if-else node)
                                   expected-type
                                   subs
                                   env)
          (setf preds (append preds preds_))
          (setf accessors (append accessors accessors_))

          (handler-case
              (let ((type (tc:apply-substitution subs else-ty)))
                (values
                 type
                 preds
                 accessors
                 (make-node-if
                  :type (tc:qualify nil type)
                  :location (source:location node)
                  :expr expr-node
                  :then then-node
                  :else else-node)
                 subs))
            (tc:coalton-internal-type-error ()
              (standard-expression-type-mismatch-error node subs expr-node else-ty)))))))

  (:method ((node parser:node-when) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-when tc:substitution-list &optional))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-when-expr node)
                               tc:*boolean-type*
                               subs
                               env)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
          (infer-expression-type (parser:node-when-body node)
                                 (tc:make-variable)
                                 subs
                                 env)
        (declare (ignore body-ty))
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (let ((result-ty (zero-result-type)))
          (handler-case
              (progn
                (setf subs (tc:unify subs result-ty expected-type))
                (values
                 result-ty
                 preds
                 accessors
                 (make-node-when
                  :type (tc:qualify nil result-ty)
                  :location (source:location node)
                  :expr expr-node
                  :body body-node)
                 subs))
            (tc:coalton-internal-type-error ()
              (standard-expression-type-mismatch-error node subs expected-type result-ty)))))))

  (:method ((node parser:node-unless) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-unless tc:substitution-list &optional))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-unless-expr node)
                               tc:*boolean-type*
                               subs
                               env)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
          (infer-expression-type (parser:node-unless-body node)
                                 (tc:make-variable)
                                 subs
                                 env)
        (declare (ignore body-ty))
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (let ((result-ty (zero-result-type)))
          (handler-case
              (progn
                (setf subs (tc:unify subs result-ty expected-type))
                (values
                 result-ty
                 preds
                 accessors
                 (make-node-unless
                  :type (tc:qualify nil result-ty)
                  :location (source:location node)
                  :expr expr-node
                  :body body-node)
                 subs))
            (tc:coalton-internal-type-error ()
              (standard-expression-type-mismatch-error node subs expected-type result-ty)))))))

  (:method ((node parser:node-for) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-for tc:substitution-list &optional))

    (let ((def-table (make-hash-table :test #'eq))
          (dec-table (make-hash-table :test #'eq))
          (preds nil)
          (accessors nil))

      ;; Ensure that there are no duplicate loop bindings.
      (loop :for binding :in (parser:node-for-bindings node)
            :for name := (parser:node-variable-name (parser:node-for-binding-name binding))
            :if (gethash name def-table)
              :do (tc-error "Duplicate binding in loop"
                            (tc-note (parser:node-for-binding-name binding)
                                     "second definition here")
                            (tc-note (parser:node-for-binding-name
                                      (gethash name def-table))
                                     "first definition here"))
            :else
              :do (setf (gethash name def-table) binding))

      ;; Ensure that there are no duplicate declarations.
      (loop :for declare :in (parser:node-for-declares node)
            :for name := (parser:node-variable-name (parser:node-let-declare-name declare))
            :if (gethash name dec-table)
              :do (tc-error "Duplicate declaration in loop"
                            (tc-note (parser:node-let-declare-name declare)
                                     "second declaration here")
                            (tc-note (parser:node-let-declare-name
                                      (gethash name dec-table))
                                     "first declaration here"))
            :else
              :do (setf (gethash name dec-table) declare))

      ;; Ensure that each declaration has an associated loop binding.
      (loop :for declare :in (parser:node-for-declares node)
            :for name := (parser:node-variable-name (parser:node-let-declare-name declare))
            :unless (gethash name def-table)
              :do (tc-error "Orphan declare in loop"
                            (tc-note (parser:node-let-declare-name declare)
                                     "declaration does not have an associated loop binding")))

      (let* ((declare-table
               (loop :with table := (make-hash-table :test #'eq)
                     :for declare :in (parser:node-for-declares node)
                     :for name := (parser:node-variable-name (parser:node-let-declare-name declare))
                     :do (setf (gethash name table) (parser:node-let-declare-type declare))
                     :finally (return table)))
             (init-bindings
               (loop :for binding :in (parser:node-for-bindings node)
                     :collect (parser:make-node-let-binding
                               :name (parser:node-for-binding-name binding)
                               :value (parser:node-for-binding-init binding)
                               :location (source:location binding))))
             (loop-env
               (make-tc-env
                :env (tc-env-env env)
                :ty-table (alexandria:copy-hash-table (tc-env-ty-table env))
                :typevar-table (alexandria:copy-hash-table (tc-env-typevar-table env))))
             (binding-init-nodes
               (cond
                 ((parser:node-for-sequential-p node)
                  (loop :for binding :in (parser:node-for-bindings node)
                        :append
                        (let* ((name (parser:node-variable-name (parser:node-for-binding-name binding)))
                               (binding-declare-table (make-hash-table :test #'eq))
                               (init-binding
                                 (parser:make-node-let-binding
                                  :name (parser:node-for-binding-name binding)
                                  :value (parser:node-for-binding-init binding)
                                  :location (source:location binding))))
                          (when (gethash name declare-table)
                            (setf (gethash name binding-declare-table)
                                  (gethash name declare-table)))
                          (multiple-value-bind (preds_ accessors_ nodes subs_)
                              (infer-bindings-type (list init-binding) binding-declare-table subs loop-env)
                            (setf subs subs_)
                            (setf preds (append preds preds_))
                            (setf accessors (append accessors accessors_))
                            nodes))))
                 (t
                  (multiple-value-bind (preds_ accessors_ nodes subs_)
                      (infer-bindings-type init-bindings declare-table subs loop-env)
                    (setf subs subs_)
                    (setf preds (append preds preds_))
                    (setf accessors (append accessors accessors_))
                    nodes)))))
        (tc:apply-substitution subs loop-env)
        (let ((binding-init-table (make-hash-table :test #'eq)))
          (labels ((typed-init-binding (binding)
                     (let* ((name (parser:node-variable-name
                                   (parser:node-for-binding-name binding)))
                            (typed-binding (gethash name binding-init-table)))
                       (or typed-binding
                           (util:coalton-bug
                            "Missing typed init binding for loop variable ~S"
                            name)))))
            (loop :for binding :in binding-init-nodes
                  :for name := (node-variable-name (node-let-binding-name binding))
                  :do (setf (gethash name binding-init-table) binding))

            (multiple-value-bind (termination-ty preds_ accessors_ termination-node subs)
                (if (parser:node-for-termination-kind node)
                    (infer-expression-type
                     (parser:node-for-termination-expr node)
                     (ecase (parser:node-for-termination-kind node)
                       ((:while :until) tc:*boolean-type*)
                       (:repeat tc:*ufix-type*))
                     subs
                     loop-env)
                    (values nil nil nil nil subs))
              (declare (ignore termination-ty))
              (setf preds (append preds preds_))
              (setf accessors (append accessors accessors_))

              (multiple-value-bind (result-ty preds_ accessors_ returns-node subs)
                  (if (parser:node-for-returns node)
                      (infer-expression-type (parser:node-for-returns node)
                                             (tc:make-variable)
                                             subs
                                             loop-env)
                      (values (zero-result-type) nil nil nil subs))
                (setf preds (append preds preds_))
                (setf accessors (append accessors accessors_))

                (let ((binding-step-nodes
                        (loop :for binding :in (parser:node-for-bindings node)
                              :for init-binding := (typed-init-binding binding)
                              :for qual-type := (node-type (node-let-binding-name init-binding))
                              :collect (and (parser:node-for-binding-step binding)
                                            (multiple-value-bind (step-ty preds_ accessors_ step-node subs_)
                                                (infer-expression-type (parser:node-for-binding-step binding)
                                                                       (tc:qualified-ty-type qual-type)
                                                                       subs
                                                                       loop-env)
                                              (declare (ignore step-ty))
                                              (setf subs subs_)
                                              (setf preds (append preds preds_))
                                              (setf accessors (append accessors accessors_))
                                              step-node)))))

                  (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
                      (infer-expression-type (parser:node-for-body node)
                                             (zero-result-type)
                                             subs
                                             loop-env)
                    (declare (ignore body-ty))
                    (setf preds (append preds preds_))
                    (setf accessors (append accessors accessors_))

                    (handler-case
                        (progn
                          (setf subs (tc:unify subs result-ty expected-type))
                          (values
                           result-ty
                           preds
                           accessors
                           (make-node-for
                            :type (tc:qualify nil result-ty)
                            :location (source:location node)
                            :label (parser:node-for-label node)
                            :bindings
                            (loop :for binding :in (parser:node-for-bindings node)
                                  :for init-binding := (typed-init-binding binding)
                                  :for qual-type := (node-type (node-let-binding-name init-binding))
                                  :for step-node :in binding-step-nodes
                                  :collect (make-node-for-binding
                                            :name (make-node-variable
                                                   :type qual-type
                                                   :location (source:location
                                                              (parser:node-for-binding-name binding))
                                                   :name (parser:node-variable-name
                                                          (parser:node-for-binding-name binding)))
                                            :init (node-let-binding-value init-binding)
                                            :step step-node
                                            :location (source:location binding)))
                            :sequential-p (parser:node-for-sequential-p node)
                            :returns returns-node
                            :termination-kind (parser:node-for-termination-kind node)
                            :termination-expr termination-node
                            :body body-node)
                           subs))
                      (tc:coalton-internal-type-error ()
                        (standard-expression-type-mismatch-error
                         node subs expected-type result-ty))))))))))))

  (:method ((node parser:node-break) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-break tc:substitution-list &optional))
    (let ((result-ty (zero-result-type)))
      (handler-case
          (progn
            (setf subs (tc:unify subs result-ty expected-type))
            (values
             result-ty
             nil
             nil
             (make-node-break
              :type (tc:qualify nil result-ty)
              :location (source:location node)
              :label (parser:node-break-label node))
             subs))
        (tc:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node subs expected-type result-ty)))))

  (:method ((node parser:node-continue) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-continue tc:substitution-list &optional))
    (let ((result-ty (zero-result-type)))
      (handler-case
          (progn
            (setf subs (tc:unify subs result-ty expected-type))
            (values
             result-ty
             nil
             nil
             (make-node-continue
              :type (tc:qualify nil result-ty)
              :location (source:location node)
              :label (parser:node-continue-label node))
             subs))
        (tc:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node subs expected-type result-ty)))))


  (:method ((node parser:node-cond-clause) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-cond-clause tc:substitution-list))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-cond-clause-expr node)
                               tc:*boolean-type*
                               subs
                               env)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
          (infer-expression-type (parser:node-cond-clause-body node)
                                 expected-type ; unify against expected-type
                                 subs
                                 env)
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (let ((type (tc:apply-substitution subs body-ty)))
          (values
           type
           preds
           accessors
           (make-node-cond-clause
            :location (source:location node)
            :expr expr-node
            :body body-node)
           subs)))))

  (:method ((node parser:node-cond) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-cond tc:substitution-list &optional))

    (let* ((preds nil)
           (accessors nil)

           (ret-ty (tc:make-variable))

           (clause-nodes
             (loop :for clause :in (parser:node-cond-clauses node)
                   :collect (multiple-value-bind (clause-ty preds_ accessors_ clause-node subs_)
                                (infer-expression-type clause
                                                       ret-ty
                                                       subs
                                                       env)
                              (declare (ignore clause-ty))
                              (setf subs subs_)
                              (setf preds (append preds preds_))
                              (setf accessors (append accessors accessors_))
                              clause-node))))

      (handler-case
          (progn
            (setf subs (tc:unify subs ret-ty expected-type))
            (let ((type (tc:apply-substitution subs ret-ty)))
              (values
               type
               preds
               accessors
               (make-node-cond
                :type (tc:qualify nil type)
                :location (source:location node)
                :clauses clause-nodes)
               subs)))
        (tc:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node subs expected-type ret-ty)))))

  (:method ((node parser:node-do-bind) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-do-bind tc:substitution-list))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-do-bind-expr node)
                               expected-type ; unify here so that expr-ty is in the form "m a"
                               subs
                               env)

      (multiple-value-bind (ty_ pattern subs)
          (infer-pattern-type (parser:node-do-bind-pattern node)
                              (tc:tapp-to (tc:apply-substitution subs expr-ty)) ; this should never fail
                              subs
                              env)
        (declare (ignore ty_))

        (handler-case
            (progn
              (setf subs (tc:unify subs expr-ty expected-type))
              (values
               expr-ty
               preds
               accessors
               (make-node-do-bind
                :pattern pattern
                :expr expr-node
                :location (source:location node))
               subs))
          (tc:coalton-internal-type-error ()
            (tc-error "Type mismatch"
                      (tc-note node "Expected type '~A' but got '~A'"
                               (type-object-string (tc:apply-substitution subs expected-type))
                               (type-object-string (tc:apply-substitution subs expr-ty)))))))))

  (:method ((node parser:node-do) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-do tc:substitution-list))

    (let* (;; m-type is the type of the monad and has kind "* -> *"
           (m-type (tc:make-variable (tc:make-kfun :from tc:+kstar+ :to tc:+kstar+)))

           (monad-symbol (util:find-symbol "MONAD" "COALTON/CLASSES"))

           (preds nil)
           (accessors nil)

           (nodes
             (loop :for elem :in (parser:node-do-nodes node)
                   :collect (etypecase elem 
                              ;; Expressions are typechecked normally
                              ;; and then unified against "m a" where
                              ;; "a" is a fresh tyvar each time
                              (parser:node
                               (multiple-value-bind (ty_ preds_ accessors_ node_ subs_)
                                   (infer-expression-type elem
                                                          (tc:make-tapp
                                                           :from m-type
                                                           :to (tc:make-variable))
                                                          subs
                                                          env)
                                 (declare (ignore ty_))
                                 (setf preds (append preds preds_))
                                 (setf accessors (append accessors accessors_))
                                 (setf subs subs_)
                                 node_))

                              ;; Node-binds are typechecked normally
                              (parser:node-bind
                               (multiple-value-bind (ty_ preds_ accessors_ node_ subs_)
                                   (infer-expression-type elem
                                                          (tc:make-variable)
                                                          subs
                                                          env)
                                 (declare (ignore ty_))
                                 (setf preds (append preds preds_))
                                 (setf accessors (append accessors accessors_))
                                 (setf subs subs_)
                                 node_))

                              ;; Values-binds are local bindings, not monadic binds
                              (parser:node-values-bind
                               (multiple-value-bind (ty_ preds_ accessors_ node_ subs_)
                                   (infer-expression-type elem
                                                          (tc:make-variable)
                                                          subs
                                                          env)
                                 (declare (ignore ty_))
                                 (setf preds (append preds preds_))
                                 (setf accessors (append accessors accessors_))
                                 (setf subs subs_)
                                 node_))

                              ;; Node-do-binds are typechecked and unified against "m a"
                              (parser:node-do-bind
                               (multiple-value-bind (ty_ preds_ accessors_ node_ subs_)
                                   (infer-expression-type elem
                                                          (tc:make-tapp
                                                           :from m-type
                                                           :to (tc:make-variable))
                                                          subs
                                                          env)
                                 (declare (ignore ty_))
                                 (setf preds (append preds preds_))
                                 (setf accessors (append accessors accessors_))
                                 (setf subs subs_)
                                 node_))))))

      (multiple-value-bind (ty preds_ accessors_ last-node subs)
          (infer-expression-type (parser:node-do-last-node node)
                                 (tc:make-tapp :from m-type
                                               :to (tc:make-variable))
                                 subs
                                 env)

        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (handler-case
            (progn
              (setf subs (tc:unify subs ty expected-type))
              (values
               ty
               (cons
                (tc:make-ty-predicate
                 :class monad-symbol
                 :types (list m-type)
                 :location (source:location node))
                preds)
               accessors
               (make-node-do
                :type (tc:qualify nil ty)
                :location (source:location node)
                :nodes nodes
                :last-node last-node) 
               subs))
          (tc:coalton-internal-type-error ()
            (tc-error "Type mismatch"
                      (tc-note node "Expected type '~A' but do expression has type '~A'"
                               (type-object-string (tc:apply-substitution subs expected-type))
                               (type-object-string (tc:apply-substitution subs ty))))))))))

;;;
;;; Pattern Type Inference
;;;

(defgeneric infer-pattern-type (pat expected-typ subs env)
  (:documentation "Infer the type of pattern PAT and then unify against EXPECTED-TYPE.

Returns (VALUES INFERRED-TYPE NODE SUBSTITUTIONS)")
  (:method :around (pat expected-type subs env)
    (declare (ignore pat expected-type subs))
    (with-type-string-environment (env)
      (call-next-method)))
  (:method ((pat parser:pattern-binding) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty pattern-binding tc:substitution-list))

    (check-duplicates (parser:pattern-variables pat)
                      #'parser:pattern-var-name
                      (lambda (first second)
                        (tc-error "Duplicate pattern variable"
                                  (tc-note first "first definition here")
                                  (tc-note second "second definition here"))))

    (multiple-value-bind (pat-ty bound subs)
        (infer-pattern-type (parser:pattern-binding-pattern pat) expected-type subs env)
      (multiple-value-bind (pat-ty var subs)
          (infer-pattern-type (parser:pattern-binding-var pat) pat-ty subs env)

        (values
         pat-ty
         (make-pattern-binding
          :type (tc:qualify nil pat-ty)
          :var var
          :pattern bound)
         subs))))

  (:method ((pat parser:pattern-var) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty pattern-var tc:substitution-list))

    (let ((ty (tc-env-add-variable env (parser:pattern-var-name pat))))

      ;; SAFETY: unification against a variable will never fail
      (setf subs (tc:unify subs ty expected-type))

      (let ((type (tc:apply-substitution subs ty)))
        (values
         type
         (make-pattern-var
          :type (tc:qualify nil type)
          :location (source:location pat)
          :name (parser:pattern-var-name pat)
          :orig-name (parser:pattern-var-orig-name pat))
         subs))))

  (:method ((pat parser:pattern-literal) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty pattern-literal tc:substitution-list))

    (let ((ty (etypecase (parser:pattern-literal-value pat)
                (integer (let* ((num
                                  (util:find-symbol "NUM" "COALTON/CLASSES"))
                                (tvar
                                  (tc:make-variable))
                                (pred
                                  (tc:make-ty-predicate :class num :types (list tvar) :location (source:location pat))))
                           (setf subs (tc:compose-substitution-lists (tc:default-subs (tc-env-env env) (list tvar) (list pred)) subs))
                           tvar))
                (ratio tc:*fraction-type*)
                (single-float tc:*single-float-type*)
                (double-float tc:*double-float-type*)
                (string tc:*string-type*)
                (character tc:*char-type*))))

      (handler-case
          (progn
            (setf subs (tc:unify subs ty expected-type))
            (let ((type (tc:apply-substitution subs ty)))
              (values
               type
               (make-pattern-literal
                :type (tc:qualify nil type)
                :location (source:location pat)
                :value (parser:pattern-literal-value pat))
               subs)))
        (tc:coalton-internal-type-error ()
          (tc-error "Type mismatch"
                    (tc-note pat "Expected type '~A' but pattern literal has type '~A'"
                             (type-object-string (tc:apply-substitution subs expected-type))
                             (type-object-string (tc:apply-substitution subs ty))))))))

  (:method ((pat parser:pattern-wildcard) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty pattern-wildcard tc:substitution-list))

    (values
     expected-type
     (make-pattern-wildcard
      :type (tc:qualify nil expected-type)
      :location (source:location pat))
     subs))

  (:method ((pat parser:pattern-constructor) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty pattern-constructor tc:substitution-list))

    (let ((ctor (tc:lookup-constructor (tc-env-env env) (parser:pattern-constructor-name pat) :no-error t)))

      (check-duplicates (parser:pattern-variables pat)
                        #'parser:pattern-var-name
                        (lambda (first second)
                          (tc-error "Duplicate pattern variable"
                                    (tc-note first "first definition here")
                                    (tc-note second "second definition here"))))

      (unless ctor
        (tc-error "Unknown constructor"
                  (tc-note pat "constructor is not known")))

      (let ((arity
              (tc:constructor-entry-arity ctor))
            (num-args
              (length (parser:pattern-constructor-patterns pat))))
        (unless (= arity num-args)
          (tc-error "Argument mismatch"
                    (tc-note pat "Constructor ~A takes ~D arguments but is given ~D"
                             (parser:pattern-constructor-name pat)
                             arity
                             num-args)))

        (let* ((ctor-ty (tc:qualified-ty-type ;; NOTE: Constructors cannot have predicates
                         (tc:fresh-inst
                          (tc:lookup-value-type (tc-env-env env) (parser:pattern-constructor-name pat)))))

               (pat-ty (tc:function-return-type ctor-ty))

               (pattern-nodes
                 (loop :for arg :in (parser:pattern-constructor-patterns pat)
                       :for arg-ty :in (tc:function-type-arguments ctor-ty)
                       :collect (multiple-value-bind (ty_ node_ subs_)
                                    (infer-pattern-type arg arg-ty subs env)
                                  (declare (ignore ty_))
                                  (setf subs subs_)
                                  node_))))

          (handler-case
              (progn
                (setf subs (tc:unify subs pat-ty expected-type))
                (let ((type (tc:apply-substitution subs pat-ty)))
                  (values
                   type
                   (make-pattern-constructor
                    :type (tc:qualify nil pat-ty)
                    :location (source:location pat)
                    :name (parser:pattern-constructor-name pat)
                    :patterns pattern-nodes)
                   subs)))
            (tc:coalton-internal-type-error ()
              (tc-error "Type mismatch"
                        (tc-note pat "Expected type '~A' but pattern has type '~A'"
                                 (type-object-string (tc:apply-substitution subs expected-type))
                                 (type-object-string (tc:apply-substitution subs pat-ty)))))))))))

;;;
;;; Binding Group Type Inference
;;;

;;; ---------------------------------------------------------------------------
;;; Value restriction and weak type variables
;;;
;;; Coalton follows the same safety strategy used by ML-family languages for
;;; mutable state: only non-expansive bindings are generalized.
;;;
;;; - Non-expansive expressions are "values" (variables, literals, lambdas, and
;;;   constructor applications over non-expansive arguments).
;;; - Expansive expressions are everything else: they may allocate, mutate, or
;;;   observe effects, so they introduce weak type variables.
;;;
;;; Any type variables that come from expansive bindings are treated as weak:
;;; they can be solved by unification, and are generalized only when they
;;; appear in covariant positions (relaxed value restriction).
;;;
;;; The expansive/non-expansive check here is intentionally syntactic (as in
;;; OCaml's value-restriction implementation): constructor applications over
;;; non-expansive arguments are non-expansive; ordinary function applications
;;; are treated as expansive.
;;;
;;; References:
;;;   - Andrew K. Wright, "Simple Imperative Polymorphism" (1995)
;;;   - Jacques Garrigue, "Relaxing the Value Restriction" (2004)
;;;   - OCaml manual section "Polymorphism and its limitations"
;;; ---------------------------------------------------------------------------

(defun binding-init-expression (binding)
  "Return BINDING's initializer expression used for value-restriction checks.

For normal `let` bindings this is the right-hand side expression.
For function-shorthand bindings and explicit multi-form bodies there is no
single initializer expression, and this function returns NIL."
  (declare (type (or parser:toplevel-define parser:node-let-binding parser:instance-method-definition) binding)
           (values (or null parser:node) &optional))
  (etypecase binding
    (parser:node-let-binding
      (parser:node-let-binding-value binding))
    (parser:toplevel-define
      (let ((body (parser:toplevel-define-body binding)))
        (when (null (parser:node-body-nodes body))
          (parser:node-body-last-node body))))
    (parser:instance-method-definition
      (let ((body (parser:instance-method-definition-body binding)))
        (when (null (parser:node-body-nodes body))
          (parser:node-body-last-node body))))))

(defun nonexpansive-expression-p (node env)
  "Return T when NODE is syntactically non-expansive.

In this context:
  - non-expansive means an expression that is value-like and can be
    safely generalized;
  - expansive means an expression that may allocate/effect and therefore
    should not be generalized.

Constructor applications are treated as non-expansive only when:
  - the rator resolves to a data constructor;
  - the application is not over-applied; and
  - every argument is itself non-expansive.

All other applications are considered expansive. This is conservative but
matches the value-restriction safety argument."
  (declare (type parser:node node)
           (type tc:environment env)
           (values boolean &optional))
  (typecase node
    ((or parser:node-variable
         parser:node-literal
         parser:node-integer-literal
         parser:node-abstraction
         parser:node-type-of)
     t)
    (parser:node-unsafe
      (let ((body (parser:node-unsafe-body node)))
        (and (null (parser:node-body-nodes body))
             (nonexpansive-expression-p (parser:node-body-last-node body) env))))
    (parser:node-the
      (nonexpansive-expression-p (parser:node-the-expr node) env))
    (parser:node-application
      (and (typep (parser:node-application-rator node) 'parser:node-variable)
           (let* ((name (parser:node-variable-name (parser:node-application-rator node)))
                  (ctor (tc:lookup-constructor env name :no-error t))
                  (rands (parser:node-application-rands node)))
             (and ctor
                  (= (length rands) (tc:constructor-entry-arity ctor))
                  (every (lambda (rand)
                           (nonexpansive-expression-p rand env))
                         rands)))))
    (t nil)))

(defun binding-nonexpansive-p (binding env)
  "Return T when BINDING is eligible for generalization.

Function bindings are always non-expansive because they denote lambdas.
Non-function bindings are eligible only when their initializer is
non-expansive."
  (declare (type (or parser:toplevel-define parser:node-let-binding parser:instance-method-definition) binding)
           (type tc:environment env)
           (values boolean &optional))
  (or (parser:binding-function-p binding)
      (let ((init-expr (binding-init-expression binding)))
        (and init-expr
             (nonexpansive-expression-p init-expr env)))))

(defun weak-binding-type-variables (bindings expr-tys env)
  "Collect weak-variable candidates for this binding group.

Each expansive binding contributes its inferred expression type variables.
Those variables may unify with concrete types later. Covariant occurrences may
still be generalized by the relaxed value restriction."
  (declare (type list bindings)
           (type tc:ty-list expr-tys)
           (type tc:environment env)
           (values tc:tyvar-list &optional))
  (remove-duplicates
   (loop :for binding :in bindings
         :for expr-ty :in expr-tys
         :unless (binding-nonexpansive-p binding env)
           :append (tc:type-variables expr-ty))
   :test #'tc:ty=))

(defun blocked-weak-type-variables (weak-tvars expr-tys retained-preds env)
  "Return weak variables that must remain monomorphic.

Weak variables are blocked from generalization when they are not covariant in
the inferred expression types, or when they appear in retained predicates.

Variance comes from constructor metadata in ENV. Unknown or opaque constructors
fall back to invariant, which is conservative."
  (declare (type tc:tyvar-list weak-tvars)
           (type tc:ty-list expr-tys)
           (type tc:ty-predicate-list retained-preds)
           (type tc:environment env)
           (values tc:tyvar-list &optional))
  (let* ((resolver (tc:make-env-variance-resolver env))
         (variance-table
           (tc:collect-tyvar-variances expr-tys resolver))
         (retained-tvars
           (tc:type-variables retained-preds)))
    (remove-duplicates
     (loop :for weak-var :in weak-tvars
           :for observed-variance := (tc:tyvar-variance variance-table weak-var)
           :when (or (find weak-var retained-tvars :test #'tc:ty=)
                     (not (tc:variance-covariant-p observed-variance)))
             :collect weak-var)
     :test #'tc:ty=)))

(defun error-non-generalizable-binding (binding scheme)
  "Signal a user-facing error for a top-level weak (non-generalizable) type.

At top level we reject unresolved weak variables in inferred schemes instead of
printing an implicit weak variable notation."
  (declare (type (or parser:toplevel-define parser:node-let-binding) binding)
           (type tc:ty-scheme scheme))
  (tc-error "Type is not generalizable"
            (tc-note (parser:binding-name binding)
                     "Inferred type ~A cannot be generalized because this binding is expansive."
                     (type-object-string scheme))
            (tc-note (parser:binding-name binding)
                     "Hint: move this allocation into a function body (eta-expand) for fresh state per call, or add an explicit type declaration if this binding should be monomorphic.")))

(defun infer-let-bindings (bindings declares subs env)
  (declare (type parser:node-let-binding-list bindings)
           (type parser:node-let-declare-list declares)
           (type tc:substitution-list subs)
           (type tc-env env)
           (values tc:ty-predicate-list accessor-list (or toplevel-define-list node-let-binding-list) tc:substitution-list &optional))

  (with-type-string-environment (env)
    (let ((def-table
            (make-hash-table :test #'eq))
          (dec-table
            (make-hash-table :test #'eq)))
    ;; Ensure that there are no duplicate definitions
    (loop :for binding :in bindings
          :for name := (parser:node-variable-name (parser:node-let-binding-name binding))

          :if (gethash name def-table)
            :do (tc-error "Duplicate binding in let"
                          (tc-note (parser:node-let-binding-name binding)
                                   "second definition here")
                          (tc-note (parser:node-let-binding-name
                                    (gethash name def-table))
                                   "first definition here"))
          :else
            :do (setf (gethash name def-table) binding))

    ;; Ensure that there are no duplicate declarations
    (loop :for declare :in declares
          :for name := (parser:node-variable-name (parser:node-let-declare-name declare))

          :if (gethash name dec-table)
            :do (tc-error "Duplicate declaration in let"
                          (tc-note (parser:node-let-declare-name declare)
                                   "second declaration here")
                          (tc-note (parser:node-let-declare-name
                                    (gethash name dec-table))
                                   "first declaration here"))
          :else
            :do (setf (gethash name dec-table) declare))

    ;; Ensure that each declaration has an associated definition
    (loop :for declare :in declares
          :for name := (parser:node-variable-name (parser:node-let-declare-name declare))

          :unless (gethash name def-table)
            :do (tc-error "Orphan declare in let"
                          (tc-note (parser:node-let-declare-name declare)
                                   "declaration does not have an associated definition")))

    (let ((dec-table
            (loop :with table := (make-hash-table :test #'eq)
                  :for declare :in declares
                  :for name := (parser:node-variable-name (parser:node-let-declare-name declare))
                  :do (setf (gethash name table) (parser:node-let-declare-type declare))
                  :finally (return table))))

      (infer-bindings-type bindings dec-table subs env)))))

(defun infer-sequential-let-bindings (bindings declares subs env)
  (declare (type parser:node-let-binding-list bindings)
           (type parser:node-let-declare-list declares)
           (type tc:substitution-list subs)
           (type tc-env env)
           (values tc:ty-predicate-list accessor-list node-let-binding-list tc:substitution-list &optional))

  (with-type-string-environment (env)
    (let ((def-table (make-hash-table :test #'eq))
          (dec-table (make-hash-table :test #'eq)))
      ;; Ensure that there are no duplicate definitions
      (loop :for binding :in bindings
            :for name := (parser:node-variable-name (parser:node-let-binding-name binding))
            :if (gethash name def-table)
              :do (tc-error "Duplicate binding in let"
                            (tc-note (parser:node-let-binding-name binding)
                                     "second definition here")
                            (tc-note (parser:node-let-binding-name
                                      (gethash name def-table))
                                     "first definition here"))
            :else
              :do (setf (gethash name def-table) binding))

      ;; Ensure that there are no duplicate declarations
      (loop :for declare :in declares
            :for name := (parser:node-variable-name (parser:node-let-declare-name declare))
            :if (gethash name dec-table)
              :do (tc-error "Duplicate declaration in let"
                            (tc-note (parser:node-let-declare-name declare)
                                     "second declaration here")
                            (tc-note (parser:node-let-declare-name
                                      (gethash name dec-table))
                                     "first declaration here"))
            :else
              :do (setf (gethash name dec-table) declare))

      ;; Ensure that each declaration has an associated definition
      (loop :for declare :in declares
            :for name := (parser:node-variable-name (parser:node-let-declare-name declare))
            :unless (gethash name def-table)
              :do (tc-error "Orphan declare in let"
                            (tc-note (parser:node-let-declare-name declare)
                                     "declaration does not have an associated definition")))

      (let ((declare-table
              (loop :with table := (make-hash-table :test #'eq)
                    :for declare :in declares
                    :for name := (parser:node-variable-name (parser:node-let-declare-name declare))
                    :do (setf (gethash name table) (parser:node-let-declare-type declare))
                    :finally (return table)))
            (preds nil)
            (accessors nil)
            (binding-nodes nil))
        (dolist (binding bindings)
          (let* ((name (parser:node-variable-name (parser:node-let-binding-name binding)))
                 (binding-declare-table (make-hash-table :test #'eq)))
            (when (gethash name declare-table)
              (setf (gethash name binding-declare-table)
                    (gethash name declare-table)))
            (multiple-value-bind (preds_ accessors_ nodes subs_)
                (infer-bindings-type (list binding) binding-declare-table subs env)
              (setf subs subs_)
              (setf preds (append preds preds_))
              (setf accessors (append accessors accessors_))
              (setf binding-nodes (append binding-nodes nodes)))))
        (values preds accessors binding-nodes subs)))))

(defun nest-sequential-let-bindings (bindings body ty location)
  (declare (type node-let-binding-list bindings)
           (type node-body body)
           (type tc:ty ty)
           (type source:location location)
           (values node-let &optional))
  (labels ((wrap (bindings)
             (if (endp bindings)
                 (make-node-let
                  :type (tc:qualify nil ty)
                  :location location
                  :bindings nil
                  :body body)
                 (make-node-let
                  :type (tc:qualify nil ty)
                  :location location
                  :bindings (list (first bindings))
                  :body (if (endp (rest bindings))
                            body
                            (make-node-body
                             :nodes nil
                             :last-node (wrap (rest bindings))))))))
    (wrap bindings)))


(defun binding-recursion-context (binding)
  "Return the recursion-check context for BINDING.

`:local` means BINDING is a `let` binding and recursive values may still be
accepted when they are constructor-built.
`:toplevel` means BINDING is a top-level definition.
`:instance` means BINDING is an instance method definition."
  (declare (type (or parser:toplevel-define
                     parser:node-let-binding
                     parser:instance-method-definition)
                 binding)
           (values (member :local :toplevel :instance) &optional))
  (etypecase binding
    (parser:node-let-binding ':local)
    (parser:toplevel-define ':toplevel)
    (parser:instance-method-definition ':instance)))

(defun recursive-binding-help (binding context)
  "Return a context-specific help note for an invalid recursive BINDING."
  (declare (type (or parser:toplevel-define
                     parser:node-let-binding
                     parser:instance-method-definition)
                 binding))
  (case context
    (:toplevel
     (source:help (source:location (parser:binding-name binding))
                  #'identity
                  "Top-level definitions are initialized in order. Turn one definition into a function, or build the cyclic value inside a local `let`."))
    (:instance
     (source:help (source:location (parser:binding-name binding))
                  #'identity
                  "Instance methods without parameters are values. Turn one definition into a function, or build the cyclic value inside a local `let`."))))

(defun typed-binding-function-p (binding)
  "Return true when typed BINDING should be treated as a recursive function."
  (declare (type (or toplevel-define
                     node-let-binding
                     instance-method-definition)
                 binding)
           (values boolean &optional))
  (and
   (or (typecase binding
         (node-let-binding
          (node-abstraction-p (node-let-binding-value binding)))
         (toplevel-define
          (or (toplevel-define-function-syntax-p binding)
              (and (toplevel-define-params binding) t)
              (and (null (node-body-nodes (toplevel-define-body binding)))
                   (node-abstraction-p (node-body-last-node (toplevel-define-body binding))))))
         (instance-method-definition
          (or (instance-method-definition-function-syntax-p binding)
              (and (instance-method-definition-params binding) t)
              (and (null (node-body-nodes (instance-method-definition-body binding)))
                   (node-abstraction-p
                    (node-body-last-node (instance-method-definition-body binding)))))))
       (consp (tc:qualified-ty-predicates
               (node-type
                (typecase binding
                  (node-let-binding
                   (node-let-binding-name binding))
                  (toplevel-define
                   (toplevel-define-name binding))
                  (instance-method-definition
                   (instance-method-definition-name binding)))))))
   t))

(defun sorted-bindings-by-location (bindings)
  (sort (copy-list bindings)
        #'source:location<
        :key (alexandria:compose #'source:location #'parser:binding-name)))

(defun binding-sccs (bindings)
  "Return the dependency SCCs for BINDINGS."
  (declare (type (or parser:toplevel-define-list
                     parser:node-let-binding-list
                     parser:instance-method-definition-list)
                 bindings)
           (values list &optional))
  (let* ((binding-table
           (loop :with table := (make-hash-table :test #'eq)
                 :for binding :in bindings
                 :for name := (parser:node-variable-name (parser:binding-name binding))
                 :do (setf (gethash name table) binding)
                 :finally (return table)))
         (binding-names (alexandria:hash-table-keys binding-table))
         (binding-deps
           (loop :for name :in binding-names
                 :for binding := (gethash name binding-table)
                 :for node := (parser:binding-value binding)
                 :for deps := (remove-duplicates
                               (intersection
                                (mapcar #'parser:node-variable-name
                                        (parser:collect-variables node))
                                binding-names
                                :test #'eq)
                               :test #'eq)
                 :collect (cons name deps))))
    (loop :for scc :in (algo:tarjan-scc binding-deps)
          :collect (loop :for name :in scc
                         :collect (gethash name binding-table)))))

(defun check-bindings-for-invalid-recursion (bindings env &key (binding-function-p #'parser:binding-function-p))
  "Signal an error if any SCC in BINDINGS is invalidly recursive.

BINDINGS is the set of bindings whose dependency graph should be checked.
ENV is used to validate recursive constructor-built values.
BINDING-FUNCTION-P must return true when a binding in BINDINGS should be treated
as a recursive function rather than a recursive value."
  (declare (type (or parser:toplevel-define-list
                     parser:node-let-binding-list
                     parser:instance-method-definition-list)
                 bindings)
           (type tc:environment env)
           (type function binding-function-p))
  (when (endp bindings)
    (return-from check-bindings-for-invalid-recursion))
  (dolist (binding-scc (binding-sccs bindings))
    (check-for-invalid-recursive-scc binding-scc env binding-function-p)))

(defun infer-bindings-type (bindings dec-table subs env)
  (declare (type list bindings)
           (type hash-table dec-table)
           (type tc:substitution-list subs)
           (type tc-env env)
           (values tc:ty-predicate-list accessor-list (or toplevel-define-list node-let-binding-list) tc:substitution-list))
  ;;
  ;; Binding type inference has several steps.
  ;; 1. Explicit types are parsed and added to the environment
  ;; 2. Implicit bindings are grouped by scc and then each scc is type checked
  ;; 3. Explicitly typed bindings are typechecked and compared against their declared types.
  ;;

  ;; Define explicit types to the environment
  (loop :for name :being :the :hash-keys :of dec-table
        :for unparsed-ty :being :the :hash-values :of dec-table

        :for scheme := (parse-ty-scheme unparsed-ty (tc-env-parser-env env))
        :do (tc-env-add-definition env name scheme))

  (when (and bindings
             (eq ':local (binding-recursion-context (first bindings))))
    (check-bindings-for-invalid-recursion bindings (tc-env-env env)))

  ;; Split apart explicit and implicit bindings
  (let* ((expl-bindings (loop :for binding :in bindings
                              :for name := (parser:node-variable-name (parser:binding-name binding))

                              :when (gethash name dec-table)
                                :collect binding))

         (impl-bindings (loop :with table := (make-hash-table :test #'eq)
                              :for binding :in bindings
                              :for name := (parser:node-variable-name (parser:binding-name binding))

                              :unless (gethash name dec-table)
                                :do (setf (gethash name table) binding)

                              :finally (return table)))

         (impl-bindings-names (alexandria:hash-table-keys impl-bindings))

         (impl-bindings-deps (loop :for name :in impl-bindings-names
                                   :for binding := (gethash name impl-bindings)
                                   :for node := (parser:binding-value binding)

                                   :for deps := (remove-duplicates
                                                 (intersection
                                                  (mapcar #'parser:node-variable-name
                                                          (parser:collect-variables node))
                                                  impl-bindings-names
                                                  :test #'eq)
                                                 :test #'eq)
                                   :collect (cons name deps)))

         (sccs (algo:tarjan-scc impl-bindings-deps))

         (preds nil)

         (impl-binding-nodes
           ;; Infer the types of implicit bindings on scc at a time
           (loop :for scc :in sccs
                 :for bindings
                   := (loop :for name :in scc
                            :collect (gethash name impl-bindings))
                 :append (multiple-value-bind (preds_ nodes subs_)
                             (infer-impls-binding-type bindings subs env)
                           (setf subs subs_)
                           (setf preds (append preds preds_))
                           nodes)))

         ;; Infer the type of each explicit bindings and check against the
         ;; declared type
         (expl-binding-nodes
           (loop :for binding :in expl-bindings

                 :for name := (parser:node-variable-name (parser:binding-name binding))
                 :for scheme := (gethash name (tc-env-ty-table env))
                 :for unparsed-ty := (gethash name dec-table)

                 :collect (multiple-value-bind (preds_ node_ subs_)
                              (infer-expl-binding-type binding
                                                       scheme
                                                       (source:location
                                                        (parser:binding-name binding))
                                                       subs
                                                       env)
                            (setf subs subs_)
                            (setf preds (append preds preds_))
                            node_))))
    (values preds
            nil
            (append impl-binding-nodes expl-binding-nodes)
            subs)))

(defun infer-expl-binding-type (binding declared-ty location subs env)
  "Infer the type of BINDING and then ensure it matches DECLARED-TY."
  (declare (type (or parser:toplevel-define parser:node-let-binding parser:instance-method-definition) binding)
           (type tc:ty-scheme declared-ty)
           (type source:location location)
           (type tc:substitution-list subs)
           (type tc-env env)
           (values tc:ty-predicate-list
                   (or toplevel-define node-let-binding instance-method-definition)
                   tc:substitution-list
                   &optional))
  
  (with-type-string-environment (env)
    ;; Top-level and instance bindings are validated with typed information so that
    ;; implicit dictionary arguments are treated as function parameters.
    (unless (parser:binding-toplevel-p binding)
      (check-for-invalid-recursive-scc (list binding) (tc-env-env env) #'parser:binding-function-p))

    (let* ((name (parser:node-variable-name (parser:binding-name binding)))

         (bound-variables (remove name (tc-env-bound-variables env) :test #'eq))

         (declared-instantiation-types (tc:ty-scheme-instantiation-types declared-ty))
         (declared-explicit-p (tc:ty-scheme-explicit-p declared-ty))
         (fresh-qual-type (tc:instantiate declared-instantiation-types
                                          (tc:ty-scheme-type declared-ty)))
         (fresh-type (tc:qualified-ty-type fresh-qual-type))
         (fresh-preds (tc:qualified-ty-predicates fresh-qual-type))
         (body-env
           (if declared-explicit-p
               (tc-env-extend-type-variable-scope
                env
                (remove-duplicates declared-instantiation-types :test #'tc:ty=))
               env)))

    (when (and declared-explicit-p
               (not (typep binding 'parser:instance-method-definition)))
      (setf body-env
            (tc-env-shadow-definition
             body-env
             name
             (tc:to-scheme fresh-qual-type))))

    (multiple-value-bind (preds accessors binding-node subs)
        (infer-binding-type
         binding
         fresh-type                     ; unify against declared type
         subs
         body-env)

      (tc:apply-substitution subs body-env)

      (setf accessors (tc:apply-substitution subs accessors))
      ;; Accessor disambiguation needs any substitutions implied by
      ;; functional dependencies before the accessor source type is concrete.
      (setf fresh-preds (tc:apply-substitution subs fresh-preds))
      (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) fresh-preds subs)))
      (setf preds (tc:apply-substitution subs preds))
      (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) preds subs)))
      (setf accessors (tc:apply-substitution subs accessors))

      (multiple-value-bind (accessors subs_)
          (solve-accessors accessors (tc-env-env env))
        (setf subs (tc:compose-substitution-lists subs subs_))

        (when accessors
          (tc-error "Ambiguous accessor"
                    (tc-note (first accessors)
                             "accessor is ambiguous")))

        ;; Generate additional substitutions from fundeps.
        ;; The purpose of this block is primarily to effect
        ;; the inheritance of functional dependencies.
        ;; For example, if we have
        ;;   (define-class (C :a :b (:a -> :b)))
        ;;   (define-class (C :a :b => D :a :b))
        ;; and a list of predicates ((D #T1 #T2) (C #T1 #T3)), then
        ;; the following lines will ensure that #T2 and #T3 are unified.
        (setf fresh-preds (tc:apply-substitution subs fresh-preds))
        (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) fresh-preds subs)))
        (setf preds (tc:apply-substitution subs preds))
        (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) preds subs)))
        (tc:apply-substitution subs body-env)

        (let* ((expr-type (tc:apply-substitution subs fresh-type))
               (expr-preds (tc:apply-substitution subs fresh-preds))

               ;; Generalization should see outer scoped type variables, but not the
               ;; explicit binders introduced by the declaration currently being checked.
               (env-tvars (tc-env-bindings-variables env bound-variables))
               (local-tvars (set-difference (remove-duplicates
                                             (append (tc:type-variables expr-type)
                                                     (tc:type-variables expr-preds))
                                             :test #'tc:ty=)
                                            env-tvars
                                            :test #'tc:ty=))
               (ordered-explicit-tvars
                 (mapcar (lambda (declared-tvar)
                           (tc:apply-substitution subs declared-tvar))
                         declared-instantiation-types))

               (output-qual-type (tc:qualify expr-preds expr-type))
               (output-scheme (if declared-explicit-p
                                  (tc:quantify-using-tvar-order
                                   (remove-if-not
                                    (lambda (declared-tvar)
                                      (find declared-tvar local-tvars :test #'tc:ty=))
                                    ordered-explicit-tvars)
                                   output-qual-type
                                   t)
                                  (tc:quantify local-tvars output-qual-type))))

          (let* ((expr-preds (tc:apply-substitution subs expr-preds))
                 (preds (tc:apply-substitution subs preds))
                 (subs subs)

                 ;; Known tvars for fundep-entail:
                 ;; 1. Tvars that appear in the binding's type signature
                 ;; 2. Tvars that have been fixed by the surrounding environment
                 (known-variables
                   (remove-if-not
                    #'tc:tyvar-p
                    (tc:apply-substitution
                     subs
                     (append
                      (remove-duplicates (tc:type-variables expr-type) :test #'tc:ty=)
                      env-tvars)))))

            ;; This loop repeats fundep-entail until no new substitutions are found
            ;; (or it hits the configured max-fundep depth). This allows the typechecker
            ;; to follow chained fundeps, where:
            ;;   (define-class (A :b :a (:b -> :a)))
            ;;   (define-class (B :c :b (:c -> :b)))
            ;;   ...
            ;; This is necessary for cases where dependent tvars only occur in the constraint:
            ;; (declare function ((B :c :b) (A :b :a) => (Void -> :c)))
            (loop :for i :below tc:+fundep-max-depth+
                  :for new-subs := (tc:fundep-entail (tc-env-env env)
                                                     expr-preds
                                                     preds
                                                     known-variables)
                  :do (when (endp new-subs)
                        (return))

                      (setf subs (tc:compose-substitution-lists new-subs subs))
                      (setf expr-preds (tc:apply-substitution new-subs expr-preds))
                      (setf preds (tc:apply-substitution new-subs preds))

                      ;; Promote newly-determined variables for downstream fundep-entail calls,
                      ;; so that tvars resulting from new substitutions become 'known' on the next pass.
                      (let ((range-tvars
                              (remove-duplicates
                               (alexandria:mappend #'tc:type-variables
                                                   (mapcar #'tc:substitution-to new-subs))
                               :test #'tc:ty=)))
                        (setf known-variables
                              (remove-duplicates
                               (append
                                (remove-if-not #'tc:tyvar-p
                                               (tc:apply-substitution new-subs known-variables))
                                range-tvars)
                               :test #'tc:ty=)))
                  :finally (util:coalton-bug "Fundep chain failed to converge"))

            (setf preds
                  (remove-if
                   (lambda (p) (tc:entail (tc-env-env env) expr-preds p))
                   preds))

            (setf local-tvars
                  (expand-local-tvars env-tvars
                                      local-tvars
                                      preds
                                      (tc-env-env env)))
            (setf env-tvars
                  (expand-local-tvars local-tvars
                                      (tc:type-variables
                                       (tc:apply-substitution subs env-tvars))
                                      preds
                                      (tc-env-env env)))

            ;; Split predicates into retained and deferred
            (multiple-value-bind (deferred-preds retained-preds)
                (tc:split-context (tc-env-env env) env-tvars preds subs)

              (let* (;; Calculate defaultable predicates
                     (defaultable-preds
                       (handler-case
                           (tc:default-preds
                            (tc-env-env env)
                            (append env-tvars local-tvars)
                            retained-preds)

                         (tc:ambiguous-constraint (e)
                           (error-ambiguous-pred (tc:ambiguous-constraint-pred e)))))

                     ;; Defaultable predicates are not retained
                     (retained-preds
                       (set-difference retained-preds defaultable-preds :test #'tc:type-predicate=)))

                ;; Apply defaulting to defaultable predicates
                (setf subs (tc:compose-substitution-lists
                            (tc:default-subs (tc-env-env env) nil defaultable-preds)
                            subs))

                ;; If the bindings is toplevel then attempt to default deferred-predicates
                (when (parser:binding-toplevel-p binding)
                  (setf subs (tc:compose-substitution-lists
                              (tc:default-subs (tc-env-env env) nil deferred-preds)
                              subs))
                  (setf deferred-preds (tc:reduce-context (tc-env-env env) deferred-preds subs)))

                ;; Toplevel bindings cannot defer predicates
                (when (and (parser:binding-toplevel-p binding) deferred-preds)
                  (error-unknown-pred (first deferred-preds)))

                ;; Check that the declared and inferred schemes match
                (let ((declared-output-scheme (tc:apply-substitution subs declared-ty)))
                  (setf output-scheme (tc:apply-substitution subs output-scheme))
                  (unless (tc:ty-scheme= declared-output-scheme output-scheme)
                    (tc-error "Declared type is too general"
                              (tc-location location
                                           "Declared type ~A is more general than inferred type ~A."
                                           (type-object-string declared-output-scheme)
                                           (type-object-string output-scheme)))))

                ;; Check for undeclared predicates
                (when (not (null retained-preds))
                  (tc-error "Explicit type is missing inferred predicate"
                            (tc-location location
                                         "Declared type ~A is missing inferred predicate ~A"
                                         (type-object-string output-qual-type)
                                         (type-object-string (first retained-preds))))))

                (values deferred-preds
                        (attach-explicit-binding-type
                         (tc:apply-substitution subs binding-node)
                         (tc:apply-substitution subs fresh-qual-type))
                        subs)))))))))

(defun infer-dynamic-binding-type (binding subs env)
  "Infer the initializer of dynamic BINDING against the existing scheme of its dynamic variable.

Dynamic rebinding is checked in the outer environment so initializer references
see the old value rather than a recursive self-binding. The resulting scheme
must match the variable's existing scheme after applying the same relaxed
value-restriction and variance logic used for implicit bindings."
  (declare (type parser:node-dynamic-binding binding)
           (type tc:substitution-list subs)
           (type tc-env env)
           (values tc:ty-predicate-list node-dynamic-binding tc:substitution-list &optional))
  (let* ((name-node (parser:node-dynamic-binding-name binding))
         (name (parser:node-variable-name name-node))
         (declared-ty
           (or (gethash name (tc-env-ty-table env))
               (tc:lookup-value-type (tc-env-env env) name :no-error t))))
    (unless declared-ty
      ;; Reuse normal unbound-variable diagnostics.
      (tc-env-lookup-value env name-node)
      (util:coalton-bug "Expected an error for unknown dynamic variable ~S" name))

    (let* ((synthetic-binding
             (parser:make-node-let-binding
              :name name-node
              :value (parser:node-dynamic-binding-value binding)
              :location (source:location binding)))
           ;; Ignore the rebound variable itself when deciding which type
           ;; variables come from the surrounding environment.
           (bound-variables (remove name (tc-env-bound-variables env) :test #'eq))
           (expr-ty (tc:qualified-ty-type (tc:fresh-inst declared-ty))))

      (multiple-value-bind (preds accessors binding-node subs_)
          (infer-binding-type synthetic-binding expr-ty subs env)
        (setf subs subs_)
        (tc:apply-substitution subs env)

        (setf accessors (tc:apply-substitution subs accessors))
        ;; Solve functional dependencies before accessors so field lookups can
        ;; see types fixed by class fundeps.
        (setf preds (tc:apply-substitution subs preds))
        (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) preds subs)))
        (setf accessors (tc:apply-substitution subs accessors))

        (multiple-value-bind (accessors subs_)
            (solve-accessors accessors (tc-env-env env))
          (setf subs (tc:compose-substitution-lists subs subs_))

          (when accessors
            (tc-error "Ambiguous accessor"
                      (tc-note (first accessors) "accessor is ambiguous")))

          (let* ((expr-ty (tc:apply-substitution subs expr-ty))
                 (env-tvars (tc-env-bindings-variables env bound-variables))
                 (expr-tvars (remove-duplicates (tc:type-variables expr-ty) :test #'eq))
                 (local-tvars (set-difference expr-tvars env-tvars :test #'eq))
                 (weak-tvars
                   (intersection
                    (weak-binding-type-variables (list synthetic-binding)
                                                 (list expr-ty)
                                                 (tc-env-env env))
                    local-tvars
                    :test #'tc:ty=)))

            (setf preds (tc:apply-substitution subs preds))

            ;; Keep dynamic rebinding aligned with ordinary implicit bindings by
            ;; reusing the same fundep, defaulting, and weak-variable handling.
            (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) preds subs)))
            (setf preds (tc:apply-substitution subs preds))
            (setf local-tvars
                  (expand-local-tvars env-tvars
                                      local-tvars
                                      preds
                                      (tc-env-env env)))
            (setf env-tvars
                  (expand-local-tvars local-tvars
                                      (tc:type-variables
                                       (tc:apply-substitution subs env-tvars))
                                      preds
                                      (tc-env-env env)))

            (multiple-value-bind (deferred-preds retained-preds)
                (tc:split-context (tc-env-env env) env-tvars preds subs)

              (let* ((defaultable-preds
                       (handler-case
                           (tc:default-preds (tc-env-env env)
                                             (append env-tvars local-tvars)
                                             retained-preds)
                         (tc:coalton-internal-type-error (e)
                           (error-ambiguous-pred (tc:ambiguous-constraint-pred e)))))
                     (retained-preds
                       (set-difference retained-preds defaultable-preds :test #'eq))
                     (restricted (not (parser:binding-function-p synthetic-binding))))

                (setf subs (tc:compose-substitution-lists
                            (tc:default-subs (tc-env-env env) nil defaultable-preds)
                            subs))

                (let ((coalton-impl/typechecker/context-reduction:*builder-class-cache* nil))
                  (setf subs (tc:compose-substitution-lists
                              (tc:default-builder-subs (tc-env-env env)
                                                       nil
                                                       (append deferred-preds retained-preds))
                              subs))
                  (setf subs (nth-value 1
                                        (tc:solve-fundeps (tc-env-env env)
                                                          (append deferred-preds retained-preds)
                                                          subs)))
                  (setf deferred-preds
                        (tc:expand-defaulted-builder-preds
                         (tc-env-env env)
                         (tc:apply-substitution subs deferred-preds)))
                  (setf retained-preds
                        (tc:expand-defaulted-builder-preds
                         (tc-env-env env)
                         (tc:apply-substitution subs retained-preds))))

                (setf deferred-preds (tc:reduce-context (tc-env-env env) deferred-preds nil))
                (setf retained-preds (tc:reduce-context (tc-env-env env) retained-preds nil))
                (setf expr-ty (tc:apply-substitution subs expr-ty))

                (let* ((generalizable-candidates
                         (remove-if-not
                          #'tc:tyvar-p
                          (tc:type-variables (tc:apply-substitution subs local-tvars))))
                       (blocked-weak-tvars
                         (intersection
                          (blocked-weak-type-variables
                           (remove-if-not #'tc:tyvar-p
                                          (tc:apply-substitution subs weak-tvars))
                           (list (tc:apply-substitution subs expr-ty))
                           (tc:apply-substitution subs retained-preds)
                           (tc-env-env env))
                          generalizable-candidates
                          :test #'tc:ty=))
                       (generalizable-tvars
                         (set-difference generalizable-candidates
                                         blocked-weak-tvars
                                         :test #'tc:ty=))
                       (output-qual-type
                         (if restricted
                             (tc:apply-substitution
                              subs
                              (tc:make-qualified-ty :predicates nil :type expr-ty))
                             (tc:apply-substitution
                              subs
                              (tc:make-qualified-ty :predicates retained-preds :type expr-ty))))
                       (output-scheme
                         (if restricted
                             (tc:quantify
                              (set-difference generalizable-tvars
                                              (tc:type-variables retained-preds)
                                              :test #'tc:ty=)
                              output-qual-type)
                             (tc:quantify generalizable-tvars output-qual-type)))
                       (deferred-preds
                         (if restricted
                             (append deferred-preds retained-preds)
                             deferred-preds))
                       (declared-output-scheme
                         (tc:apply-substitution subs declared-ty)))

                  (unless (tc:ty-scheme= declared-output-scheme output-scheme)
                    (tc-error "Dynamic binding does not preserve variable type"
                              (tc-note name-node
                                       "Dynamic variable ~S has type ~S, but this binding inferred ~S."
                                       name
                                       declared-output-scheme
                                       output-scheme)))

                  (values
                   deferred-preds
                   (make-node-dynamic-binding
                    :name (make-node-variable
                           :name name
                           :type output-qual-type
                           :location (source:location name-node))
                    :value (node-let-binding-value (tc:apply-substitution subs binding-node))
                    :location (source:location binding))
                   subs))))))))))

(defun check-for-invalid-recursive-scc (bindings env binding-function-p)
  "Validate one recursive SCC of bindings.

BINDINGS must be a non-empty strongly connected component from a binding
dependency graph.
ENV is used to validate recursive constructor-built values.
BINDING-FUNCTION-P must return true when a binding in BINDINGS should be treated
as a recursive function rather than a recursive value."
  (declare (type (or parser:toplevel-define-list
                     parser:node-let-binding-list
                     parser:instance-method-definition-list)
                 bindings)
           (type tc:environment env)
           (type function binding-function-p))

  (assert bindings)

  (let ((context (binding-recursion-context (first bindings))))

    ;; If all bindings are functions then the scc is valid
    (when (every binding-function-p bindings)
      (return-from check-for-invalid-recursive-scc))

    ;; If the SCC mixes functions and values then it is invalid.
    (when (and (some binding-function-p bindings)
               (notevery binding-function-p bindings))

      (case context
        (:local
         (let ((first-fn (find-if binding-function-p bindings)))
           (apply #'tc-error
                  "Invalid recursive bindings"
                  (tc-note (parser:binding-name first-fn)
                           "function can not be defined recursively with variables")
                  (loop :for binding :in (remove first-fn bindings :test #'eq)
                        :collect (tc-secondary-note (parser:binding-name binding)
                                                    "with definition")))))
        (t
         (let* ((ordered-bindings (sorted-bindings-by-location bindings))
                (first-value (find-if-not binding-function-p ordered-bindings))
                (help (and first-value
                           (recursive-binding-help first-value context))))
           (apply #'tc-error
                  "Invalid recursive bindings"
                  (append
                   (list
                    (tc-note (parser:binding-name first-value)
                             (ecase context
                               (:toplevel
                                "top-level recursive definitions must all be functions, but ~S is a value")
                               (:instance
                                "recursive instance methods must all be functions, but ~S is a value method"))
                             (parser:node-variable-name (parser:binding-name first-value))))
                   (loop :for binding :in (remove first-value ordered-bindings :test #'eq)
                         :collect (tc-secondary-note (parser:binding-name binding)
                                                     "recursive dependency here"))
                   (if help
                       (list help)
                       nil)))))))

    ;; If there is a single non-recursive binding then it is valid
    (when (and (= 1 (length bindings))
               (not (member (parser:node-variable-name (parser:binding-name (first bindings)))
                            (parser:collect-variables (parser:binding-value (first bindings)))
                            :key #'parser:node-variable-name
                            :test #'eq)))
      (return-from check-for-invalid-recursive-scc))

    ;; Toplevel bindings cannot be recursive values
    (when (parser:binding-toplevel-p (first bindings))
      (case context
        (:local
         (error "Unexpected local binding marked as top-level."))
        (t
         (let* ((ordered-bindings (sorted-bindings-by-location bindings))
                (first-binding (first ordered-bindings))
                (help (recursive-binding-help first-binding context)))
           (apply #'tc-error
                  "Invalid recursive bindings"
                  (append
                   (list
                    (tc-note (parser:binding-name first-binding)
                             (ecase context
                               (:toplevel "top-level values cannot be defined recursively")
                               (:instance "instance value methods cannot be defined recursively"))))
                   (loop :for binding :in (rest ordered-bindings)
                         :collect (tc-secondary-note (parser:binding-name binding)
                                                     "recursive dependency here"))
                   (if help
                       (list help)
                       nil)))))))

    (let ((binding-names (mapcar (alexandria:compose #'parser:node-variable-name
                                                     #'parser:binding-name)
                                 bindings)))

      (labels ((valid-recursive-constructor-call-p (node)
                 "Returns t if NODE is a valid constructor call in a recursive value binding group"
                 (typecase node
                   (parser:node-unsafe
                    (let ((body (parser:node-unsafe-body node)))
                      (and (null (parser:node-body-nodes body))
                           (valid-recursive-constructor-call-p
                            (parser:node-body-last-node body)))))
                   (parser:node-the
                    (valid-recursive-constructor-call-p (parser:node-the-expr node)))
                   (parser:node-application
                    (when (typep (parser:node-application-rator node) 'parser:node-variable)

                      (let* ((function-name (parser:node-variable-name (parser:node-application-rator node)))

                             (ctor (tc:lookup-constructor env function-name :no-error t)))

                        (when ctor
                          ;; The constructor must be fully applied
                          (unless (= (length (parser:node-application-rands node)) (tc:constructor-entry-arity ctor))
                            (return-from valid-recursive-constructor-call-p nil))

                          (let ((type (tc:lookup-type env (tc:constructor-entry-constructs ctor))))

                            ;; Recursive constructors are valid on types
                            ;; without reprs, types with repr lisp and
                            ;; the type "List"
                            (when (or (null (tc:type-entry-explicit-repr type))
                                      (eq :lisp (tc:type-entry-explicit-repr type))
                                      (eq 'coalton:List (tc:type-entry-name type)))
                              (return-from valid-recursive-constructor-call-p
                                (reduce
                                 (lambda (a b) (and a b))
                                 (parser:node-application-rands node)
                                 :key #'valid-recursive-value-p
                                 :initial-value t))))))))))

               (valid-recursive-value-p (node)
                 "Returns t if NODE is a valid subcomponent in a recursive value binding group"
                 ;; Variables are valid nodes
                 (when (typep node 'parser:node-variable)
                   (return-from valid-recursive-value-p t))

                 (when (valid-recursive-constructor-call-p node)
                   (return-from valid-recursive-value-p t))

                 ;; Nodes are valid if they do not reference variables in the current binding group
                 (not
                  (intersection
                   binding-names
                   (mapcar #'parser:node-variable-name
                           (parser:collect-variables node))
                   :test #'eq))))

        (when (every (alexandria:compose #'valid-recursive-constructor-call-p #'parser:binding-value) bindings)
          (return-from check-for-invalid-recursive-scc))

        (apply #'tc-error "Invalid recursive bindings"
               (tc-note (parser:binding-name (first bindings))
                        "invalid recursive variable bindings")
               (loop :for binding :in (rest bindings)
                     :collect (tc-note (parser:binding-name binding) "with definition")))))))

(defun infer-impls-binding-type (bindings subs env)
  "Infer the type's of BINDINGS and then qualify those types into schemes."
  (declare (type (or parser:toplevel-define-list parser:node-let-binding-list) bindings)
           (type tc:substitution-list subs)
           (type tc-env env)
           (values tc:ty-predicate-list (or toplevel-define-list node-let-binding-list) tc:substitution-list &optional))

  (with-type-string-environment (env)
    (check-for-invalid-recursive-scc bindings (tc-env-env env) #'parser:binding-function-p)

    (labels ((binding-inline-annotation-type (binding)
               (declare (type (or parser:toplevel-define parser:node-let-binding) binding)
                        (values (or null tc:ty) &optional))
               (let ((value (parser:binding-value binding)))
                 (when (typep value 'parser:node-the)
                   (tc:qualified-ty-type
                    (parse-the-type-annotation (parser:node-the-type value)
                                               env)))))
             (add-implicit-binding-type (name binding)
               (declare (type symbol name)
                        (type (or parser:toplevel-define parser:node-let-binding) binding)
                        (values tc:ty &optional))
               (let ((annotated-ty (binding-inline-annotation-type binding)))
                 (cond
                   (annotated-ty
                    ;; Recursive references must see the inline ascription, not a
                    ;; placeholder result variable.
                    (tc-env-add-definition env
                                           name
                                           (tc:make-ty-scheme
                                            :kinds nil
                                            :type (tc:qualify nil annotated-ty)))
                    annotated-ty)
                   (t
                    (tc-env-add-variable env name))))))

      (let* (;; track variables bound before typechecking
             (bound-variables (tc-env-bound-variables env))

             ;; Add all bindings to the environment
             (expr-tys
               (loop :for binding :in bindings
                     :for name := (parser:node-variable-name (parser:binding-name  binding))
                     :collect (add-implicit-binding-type name binding)))

             (preds nil)

             (accessors nil)

             ;; Derive the type of each binding
             (binding-nodes
               (loop :for binding :in bindings
                     :for ty :in expr-tys
                     :for node := (parser:binding-value binding)
                     :collect (multiple-value-bind (preds_ accessors_ node_ subs_)
                                  (infer-binding-type binding ty subs env)
                                (setf subs subs_)
                                (setf preds (append preds preds_))
                                (setf accessors (append accessors accessors_))
                                node_))))

        (tc:apply-substitution subs env)

        (setf accessors (tc:apply-substitution subs accessors))
        ;; Solve functional dependencies before accessors so field lookups can
        ;; see types fixed by class fundeps.
        (setf preds (tc:apply-substitution subs preds))
        (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) preds subs)))
        (setf accessors (tc:apply-substitution subs accessors))

        (multiple-value-bind (accessors subs_)
            (solve-accessors accessors (tc-env-env env))
          (setf subs (tc:compose-substitution-lists subs subs_))

          (when accessors
            (tc-error "Ambiguous accessor"
                      (tc-note (first accessors) "accessor is ambiguous")))

          (let* ((expr-tys
                   (tc:apply-substitution subs expr-tys))
                 (env-tvars
                   (tc-env-bindings-variables env bound-variables))
                 (expr-tvars
                   (remove-duplicates (tc:type-variables expr-tys) :test #'eq))
                 (local-tvars
                   (set-difference expr-tvars env-tvars :test #'eq))
                 (weak-tvars
                   (intersection
                    (weak-binding-type-variables bindings expr-tys (tc-env-env env))
                    local-tvars
                    :test #'tc:ty=)))

            (setf preds (tc:apply-substitution subs preds))

        ;; Generate additional substitutions from fundeps
        ;; This effects the inheritance of functional dependencies
        ;; from superclasses and reduces generality with respect to
        ;; instances defined in the environment.
        (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) preds subs)))

        (setf preds (tc:apply-substitution subs preds))
        (setf local-tvars
              (expand-local-tvars env-tvars
                                  local-tvars
                                  preds
                                  (tc-env-env env)))

        (setf env-tvars
              (expand-local-tvars local-tvars
                                  (tc:type-variables
                                   (tc:apply-substitution subs env-tvars))
                                  preds
                                  (tc-env-env env)))

        (multiple-value-bind (deferred-preds retained-preds)
            (tc:split-context (tc-env-env env) env-tvars preds subs)

          (let* ((defaultable-preds (handler-case
                                        (tc:default-preds (tc-env-env env) (append env-tvars local-tvars) retained-preds)
                                      (tc:coalton-internal-type-error (e)
                                        (error-ambiguous-pred (tc:ambiguous-constraint-pred e)))))

                 (retained-preds (set-difference retained-preds defaultable-preds :test #'eq))

                 ;; Check if the monomorphism restriction applies
                 (restricted (some (lambda (b)
                                     (not (parser:binding-function-p b)))
                                   bindings)))


            (setf subs (tc:compose-substitution-lists
                        (tc:default-subs (tc-env-env env) nil defaultable-preds)
                        subs))

            ;; Builder syntax should default its collection representation
            ;; even in unrestricted bindings, leaving element predicates alone.
            (let ((coalton-impl/typechecker/context-reduction:*builder-class-cache* nil))
              (setf subs (tc:compose-substitution-lists
                          (tc:default-builder-subs (tc-env-env env)
                                                   nil
                                                   (append deferred-preds retained-preds))
                          subs))

              ;; Once the collection/association type defaults, builder-state
              ;; variables become determined by the builder class functional
              ;; dependencies. Solve them before we expand the predicates
              ;; against concrete instances.
              (setf subs (nth-value 1
                                    (tc:solve-fundeps (tc-env-env env)
                                                      (append deferred-preds retained-preds)
                                                      subs)))

              (setf deferred-preds
                    (tc:expand-defaulted-builder-preds
                     (tc-env-env env)
                     (tc:apply-substitution subs deferred-preds)))
              (setf retained-preds
                    (tc:expand-defaulted-builder-preds
                     (tc-env-env env)
                     (tc:apply-substitution subs retained-preds))))

            (setf deferred-preds (tc:reduce-context (tc-env-env env) deferred-preds nil))
            (setf retained-preds (tc:reduce-context (tc-env-env env) retained-preds nil))
            (setf expr-tys (tc:apply-substitution subs expr-tys))

            (when (parser:binding-toplevel-p (first bindings))
              (if restricted
                  ;; Restricted bindings have all predicates defaulted
                  (setf subs (tc:compose-substitution-lists
                              (tc:default-subs (tc-env-env env) nil (append deferred-preds retained-preds))
                              subs))
                  ;; Unrestricted bindings have deferred predicates defaulted
                  (setf subs (tc:compose-substitution-lists
                              (tc:default-subs (tc-env-env env) nil deferred-preds)
                              subs)))

              (setf deferred-preds (tc:reduce-context (tc-env-env env) deferred-preds subs))
              (setf retained-preds (tc:reduce-context (tc-env-env env) retained-preds subs))
              (setf expr-tys (tc:apply-substitution subs expr-tys)))

            (let* ((generalizable-candidates
                     (remove-if-not
                      #'tc:tyvar-p
                      (tc:type-variables (tc:apply-substitution subs local-tvars))))
                   (blocked-weak-tvars
                     (intersection
                      (blocked-weak-type-variables
                       (remove-if-not #'tc:tyvar-p
                                      (tc:apply-substitution subs weak-tvars))
                       (tc:apply-substitution subs expr-tys)
                       (tc:apply-substitution subs retained-preds)
                       (tc-env-env env))
                      generalizable-candidates
                      :test #'tc:ty=))
                   (generalizable-tvars
                     (set-difference
                      generalizable-candidates
                      ;; Weak variables with non-covariant occurrences remain
                      ;; monomorphic placeholders until solved.
                      blocked-weak-tvars
                      :test #'tc:ty=)))

              (if restricted
                  (let* ((allowed-tvars (set-difference generalizable-tvars
                                                        (tc:type-variables retained-preds)
                                                        :test #'tc:ty=))

                         (output-qual-tys
                           (loop :for ty :in expr-tys
                                 :collect (tc:apply-substitution subs (tc:make-qualified-ty :predicates nil :type ty))))

                         (output-schemes
                           (loop :for ty :in output-qual-tys
                                 :collect (tc:quantify allowed-tvars ty)))

                         (deferred-preds (append deferred-preds retained-preds)))

                    (when (parser:binding-toplevel-p (first bindings))
                      (loop :for scheme :in output-schemes
                            :for binding :in bindings
                            :when (tc:type-variables scheme)
                              :do (error-non-generalizable-binding binding scheme)))

                    (loop :for scheme :in output-schemes
                          :for binding :in bindings

                          :for name := (parser:node-variable-name (parser:binding-name binding))

                          :do (tc-env-replace-type env name scheme))

                    (when (and (parser:binding-toplevel-p (first bindings)) deferred-preds)
                      (error-unknown-pred (first deferred-preds)))

                    (values
                     deferred-preds
                     (loop :for binding :in binding-nodes
                           :for ty :in output-qual-tys
                           :collect (tc:apply-substitution subs (attach-explicit-binding-type binding ty)))
                     subs))

                  (let* ((output-qual-tys
                           (loop :for ty :in expr-tys
                                 :collect (tc:make-qualified-ty :predicates retained-preds :type ty)))

                         (output-schemes
                           (loop :for ty :in output-qual-tys
                                 :collect (tc:quantify generalizable-tvars ty)))

                         (rewrite-table
                           (loop :with table := (make-hash-table :test #'eq)

                                 :for ty :in output-qual-tys
                                 :for binding :in bindings

                                 :for name := (parser:node-variable-name (parser:binding-name binding))
                                 :do (setf (gethash name table) ty)

                                 :finally (return table))))

                    (when (parser:binding-toplevel-p (first bindings))
                      (loop :for scheme :in output-schemes
                            :for binding :in bindings
                            :when (tc:type-variables scheme)
                              :do (error-non-generalizable-binding binding scheme)))

                    (loop :for scheme :in output-schemes
                          :for binding :in bindings

                          :for name := (parser:node-variable-name (parser:binding-name binding))

                          :do (tc-env-replace-type env name scheme))

                    (when (and (parser:binding-toplevel-p (first bindings)) deferred-preds)
                      (error-ambiguous-pred (first deferred-preds)))

                    (values
                     deferred-preds
                     (loop :for binding :in binding-nodes
                           :for ty :in output-qual-tys
                           :collect (rewrite-recursive-calls
                                     (tc:apply-substitution subs (attach-explicit-binding-type binding ty))
                                     rewrite-table))
                     subs))))))))))))

(defun infer-binding-type (binding expected-type subs env)
  "Infer the type of BINDING then unify against EXPECTED-TYPE. Adds BINDING's parameters to the environment."
  (declare (type (or parser:toplevel-define parser:node-let-binding parser:instance-method-definition) binding)
           (type tc:ty expected-type)
           (type tc:substitution-list subs)
           (values tc:ty-predicate-list accessor-list (or toplevel-define node-let-binding instance-method-definition) tc:substitution-list))

  (let ((params (parser:binding-parameters binding))
        (keyword-params (parser:binding-keyword-parameters binding)))
    (ensure-no-duplicate-function-parameters params keyword-params)
    (labels ((initform-abstraction-node (node)
               (typecase node
                 (parser:node-abstraction
                  node)
                 (parser:node-unsafe
                  (let ((body (parser:node-unsafe-body node)))
                    (and (null (parser:node-body-nodes body))
                         (initform-abstraction-node
                          (parser:node-body-last-node body)))))
                 (parser:node-the
                  (initform-abstraction-node (parser:node-the-expr node)))
                 (t
                  nil)))
             (binding-expression ()
               (etypecase binding
                 (parser:node-let-binding
                  (parser:binding-value binding))
                 ((or parser:toplevel-define parser:instance-method-definition)
                  (or (and (null params)
                           (null keyword-params)
                           (null (parser:node-body-nodes (parser:binding-value binding)))
                           (initform-abstraction-node
                            (parser:node-body-last-node (parser:binding-value binding))))
                      (parser:make-node-abstraction
                       :params params
                       :keyword-params keyword-params
                       :body (parser:binding-value binding)
                       :location (source:location binding)))))))
      (if (parser:binding-function-p binding)
          (multiple-value-bind (ty preds accessors value-node subs_)
              (infer-expression-type (binding-expression)
                                     expected-type
                                     subs
                                     env)
            (declare (ignore ty))
            (unless (or (typep value-node 'node-abstraction)
                        (and (null params)
                             (null keyword-params)))
              (util:coalton-bug "Expected abstraction node for binding ~S, got ~S"
                                binding
                                value-node))
            (let* ((type (if (typep value-node 'node-abstraction)
                             (tc:qualified-ty-type (node-type value-node))
                             (tc:apply-substitution subs_ expected-type)))
                   (name-node
                     (make-node-variable
                      :type (tc:qualify nil type)
                      :location (source:location (parser:binding-name binding))
                      :name (parser:node-variable-name (parser:binding-name binding))))
                   (body-node
                     (if (typep value-node 'node-abstraction)
                         (node-abstraction-body value-node)
                         (make-node-body
                          :nodes nil
                          :last-node value-node)))
                   (typed-params
                     (if (typep value-node 'node-abstraction)
                         (node-abstraction-params value-node)
                         nil))
                   (typed-keyword-params
                     (if (typep value-node 'node-abstraction)
                         (node-abstraction-keyword-params value-node)
                         nil))
                   (typed-binding
                     (if (typep binding 'parser:node-let-binding)
                         (build-typed-binding binding name-node value-node nil nil)
                         (build-typed-binding binding name-node body-node typed-params typed-keyword-params))))
              (values preds accessors typed-binding subs_)))

          (multiple-value-bind (value-ty preds accessors value-node subs_)
              (infer-expression-type (parser:binding-value binding)
                                     expected-type
                                     subs
                                     env)
            (let* ((type (tc:apply-substitution subs_ value-ty))
                   (name-node
                     (make-node-variable
                      :type (tc:qualify nil type)
                      :location (source:location (parser:binding-name binding))
                      :name (parser:node-variable-name (parser:binding-name binding))))
                   (typed-binding (build-typed-binding binding name-node value-node nil nil)))
              ;; A let-bound variable requires exactly one value.  Void
              ;; (zero values) and multi-value results cannot be bound
              ;; to a let variable -- use `let (values ...) = ...` instead.
              ;; Top-level defines are allowed to have Void type for
              ;; side-effect initialization.
              (when (and (typep type 'tc:result-ty)
                         (typep binding 'parser:node-let-binding))
                (tc-error "Cannot bind to variable"
                          (tc-note (parser:binding-name binding)
                                   "cannot bind ~A to a variable; use 'let (values ~A) = ...' for multiple values"
                                   (type-object-string type)
                                   (if (null (tc:result-ty-output-types type))
                                       "_"
                                       (format nil "~{~A~^ ~}"
                                               (loop :for _ :in (tc:result-ty-output-types type)
                                                     :collect "_"))))))
              (values preds accessors typed-binding subs_)))))))

;;;
;;; Helpers
;;;

(defgeneric build-typed-binding (binding name value params keyword-params)
  (:method ((binding parser:toplevel-define) name value params keyword-params)
    (declare (type node-variable name)
             (type node-body value)
             (type pattern-list params)
             (type keyword-param-list keyword-params)
             (values toplevel-define))

    (make-toplevel-define
     :name name
     :params params
     :keyword-params keyword-params
     :function-syntax-p (parser:toplevel-define-function-syntax-p binding)
     :body value
     :location (source:location binding)))

  (:method ((binding parser:node-let-binding) name value params keyword-params)
    (declare (type node-variable name)
             (type node value)
             (type pattern-list params)
             (type keyword-param-list keyword-params)
             (values node-let-binding))

    (assert (null params))
    (assert (null keyword-params))

    (make-node-let-binding
     :name name
     :value value
     :location (source:location binding)))

  (:method ((binding parser:instance-method-definition) name value params keyword-params)
    (declare (type node-variable name)
             (type node-body value)
             (type pattern-list params)
             (type keyword-param-list keyword-params)
             (values instance-method-definition))

    (make-instance-method-definition
     :name name
     :params params
     :keyword-params keyword-params
     :function-syntax-p (parser:instance-method-definition-function-syntax-p binding)
     :body value
     :location (source:location binding))))

(defgeneric attach-explicit-binding-type (binding explicit-type)
  (:method ((binding toplevel-define) explicit-type)
    (declare (type tc:qualified-ty explicit-type)
             (values toplevel-define))

   (make-toplevel-define
    :name (make-node-variable
           :name (node-variable-name (toplevel-define-name binding))
           :type explicit-type
           :location (source:location (toplevel-define-name binding)))
    :params (toplevel-define-params binding)
    :keyword-params (toplevel-define-keyword-params binding)
    :function-syntax-p (toplevel-define-function-syntax-p binding)
    :body (toplevel-define-body binding)
    :location (source:location binding)))

  (:method ((binding node-let-binding) explicit-type)
    (declare (type tc:qualified-ty explicit-type)
             (values node-let-binding))

   (make-node-let-binding
    :name (make-node-variable
           :name (node-variable-name (node-let-binding-name binding))
           :type explicit-type
           :location (source:location (node-let-binding-name binding)))
    :value (node-let-binding-value binding)
    :location (source:location binding)))

  (:method ((binding instance-method-definition) explicit-type)
    (declare (type tc:qualified-ty explicit-type)
             (values instance-method-definition))

    (make-instance-method-definition
     :name (make-node-variable
            :name (node-variable-name (instance-method-definition-name binding))
            :type explicit-type
            :location (source:location (instance-method-definition-name binding)))
     :params (instance-method-definition-params binding)
     :keyword-params (instance-method-definition-keyword-params binding)
     :function-syntax-p (instance-method-definition-function-syntax-p binding)
     :body (instance-method-definition-body binding)
     :location (source:location binding))))

;;; When inferring the types of bindings in a recursive binding group,
;;; references to those bindings will not yet have predicates. If
;;; these incorrectly typed references remain, the compiler will
;;; produce invalid code. `rewrite-recursive-calls' rewrites the type
;;; of recursive references once the predicates on each binding are
;;; known.

(defun rewrite-recursive-calls (binding table)
  (declare (type hash-table table))

  (labels ((rewrite-variable-ref (node)
             (declare (type node-variable node)
                      (values node-variable))

             (if (gethash (node-variable-name node) table)

                 (make-node-variable
                  :type (gethash (node-variable-name node) table)
                  :location (source:location node)
                  :name (node-variable-name node))

                 node)))

    (etypecase binding
      (toplevel-define
       (make-toplevel-define
        :name (toplevel-define-name binding)
        :params (toplevel-define-params binding)
        :keyword-params (toplevel-define-keyword-params binding)
        :function-syntax-p (toplevel-define-function-syntax-p binding)
        :body (traverse
               (toplevel-define-body binding)
               (make-traverse-block
                :variable #'rewrite-variable-ref))
        :location (source:location binding)))

      (node-let-binding
       (make-node-let-binding
        :name (node-let-binding-name binding)
        :value (traverse
                (node-let-binding-value binding)
                (make-traverse-block
                 :variable #'rewrite-variable-ref))
        :location (source:location binding)))

      (instance-method-definition
       (make-instance-method-definition
        :name (instance-method-definition-name binding)
        :params (instance-method-definition-params binding)
        :keyword-params (instance-method-definition-keyword-params binding)
        :function-syntax-p (instance-method-definition-function-syntax-p binding)
        :body (traverse
               (instance-method-definition-body binding)
               (make-traverse-block
                :variable #'rewrite-variable-ref))
        :location (source:location binding))))))

;;; When type checking bindings, Coalton computes the set of type
;;; variables that can be qualified over. Predicates that contain type
;;; variables not in this set are rejected as ambiguous.
;;;
;;;   An example rejected definition:
;;;   Into :a :b => :a -> Integer
;;;
;;; Without fundeps this set of type variables is simply the type
;;; variables in the inferred type, minus the type variables in the
;;; environment. With fundeps, type variables can appear in the
;;; predicates but not the type without being ambiguous.
;;;
;;;   A valid definition for "C :a :b (:a -> :b)"
;;;   C :a :b => :a -> Integer
;;;
;;; `expand-local-tvars' uses fundeps to compute an extended set of
;;; unambiguous type variables given a previous such list and the
;;; binding's predicates.

(defun expand-local-tvars (env-tvars local-tvars preds env)
  "Expand LOCAL-TVARS over the functional dependencies taken from PREDS
and return the set difference of the expansion and ENV-TVARS."
  (let* ((fundeps (tc:collect-fundep-vars env preds))
         (expansion (tc:generic-closure local-tvars fundeps :test #'tc:ty=))
         (expansion (remove-duplicates expansion :test #'tc:ty=)))
    (set-difference expansion env-tvars :test #'tc:ty=)))
