(defpackage #:coalton-impl/codegen/codegen-match
  (:use
   #:cl)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:rt #:coalton-impl/runtime)
   (#:ast #:coalton-impl/codegen/ast)
   (#:pattern #:coalton-impl/codegen/pattern)
   (#:codegen-pattern #:coalton-impl/codegen/codegen-pattern))
  (:export
   #:match-emit-jumptable-p             ; FUNCTION
   #:match-emit-fallback-p              ; FUNCTION
   #:match-emit-branchless-p            ; FUNCTION
   #:match-emit-if-p                    ; FUNCTION
   #:codegen-match-branch               ; FUNCTION
   #:codegen-match                      ; FUNCTION
   #:order-pattern-tests                ; FUNCTION
   #:lower-parameter-row                ; FUNCTION
   ))

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
the match is exhaustive and settings demand it."
  (declare (type ast:node-match match)
           (type tc:environment env)
           (values t &optional))

  (not (or
        (match-has-catch-all-p match)
        (and (settings:coalton-release-p)
             settings:*emit-type-annotations*
             (match-exhaustive-p match env)))))

(defun match-emit-branchless-p (match env)
  "Emit no conditional branching in cases where match is not used
for control flow."
  (declare (type ast:node-match match)
           (type tc:environment env)
           (values t &optional))

  (let ((one-branch-p (= 1 (length (ast:node-match-branches match)))))
    (and one-branch-p
         (or
          (match-has-catch-all-p match)
          (and (settings:coalton-release-p)
               (match-exhaustive-p match env))))))

(defun match-emit-if-p (match)
  "Emit match as `cl:if' in cases where the subexpression is a `Boolean'
and branches exhaustively pattern match with constructor branches.

When true, returns two `ast:node' objects representing then/else branches."
  (declare (type ast:node-match match)
           (values boolean (or null ast:node) (or null ast:node) &optional))

  (let ((branches (ast:node-match-branches match))
        (true-pattern
          (pattern:make-pattern-constructor :type tc:*boolean-type*
                                            :name 'coalton:True
                                            :patterns nil))
        (false-pattern
          (pattern:make-pattern-constructor :type tc:*boolean-type*
                                            :name 'coalton:False
                                            :patterns nil)))

    (cond
      ((not (and
             (equalp (ast:node-type (ast:node-match-expr match)) tc:*boolean-type*)
             (= 2 (length branches))))
       (values nil nil nil))

      ((and (equalp true-pattern (ast:match-branch-pattern (first branches)))
            (equalp false-pattern (ast:match-branch-pattern (second branches))))
       (values t
               (ast:match-branch-body (first branches))
               (ast:match-branch-body (second branches))))

      ((and (equalp true-pattern (ast:match-branch-pattern (second branches)))
            (equalp false-pattern (ast:match-branch-pattern (first branches))))
       (values t
               (ast:match-branch-body (second branches))
               (ast:match-branch-body (first branches))))

      (t
       (values nil nil nil)))))

;;;
;;; Decision Tree Pattern Matching Compilation
;;;

(defstruct (match-column (:copier nil))
  (var  (util:required 'var)  :type symbol :read-only t)
  (type (util:required 'type) :type tc:ty  :read-only t))

(defstruct (constructor-specialization (:copier nil))
  (fields nil :type list :read-only t)
  (accessors nil :type list :read-only t)
  (type-arguments nil :type tc:ty-list :read-only t)
  (substitutions nil :type tc:substitution-list :read-only t))

(defstruct (match-binding (:copier nil))
  (var       (util:required 'var)       :type symbol :read-only t)
  (source    (util:required 'source)    :type symbol :read-only t)
  (type      (util:required 'type)      :type tc:ty  :read-only t))

(defstruct (match-row (:copier nil))
  (patterns     (util:required 'patterns)     :type list   :read-only t)
  (bindings     (util:required 'bindings)     :type list   :read-only t)
  (openings nil :type tc:substitution-list :read-only t)
  (action-index (util:required 'action-index) :type fixnum :read-only t))

(defstruct (dtree (:constructor nil) (:copier nil)))

(defstruct (dtree-fail (:include dtree) (:copier nil)))

(defstruct (dtree-leaf (:include dtree) (:copier nil))
  (action-index (util:required 'action-index) :type fixnum :read-only t)
  (bindings     (util:required 'bindings)     :type list   :read-only t))

(defstruct (dtree-constructor-case (:copier nil))
  (ctor-name       (util:required 'ctor-name)       :type symbol               :read-only t)
  (ctor-entry      (util:required 'ctor-entry)      :type tc:constructor-entry :read-only t)
  (field-columns   (util:required 'field-columns)   :type list                 :read-only t)
  (field-accessors (util:required 'field-accessors) :type list                 :read-only t)
  (subtree         (util:required 'subtree)         :type dtree                :read-only t))

(defstruct (dtree-constructor-switch (:include dtree) (:copier nil))
  (column     (util:required 'column)     :type match-column  :read-only t)
  (type-entry (util:required 'type-entry) :type tc:type-entry :read-only t)
  (complete-p (util:required 'complete-p) :type boolean       :read-only t)
  (cases      (util:required 'cases)      :type list          :read-only t)
  (default    nil                         :type (or null dtree) :read-only t))

(defstruct (dtree-literal-if (:include dtree) (:copier nil))
  (column  (util:required 'column)  :type match-column            :read-only t)
  (pattern (util:required 'pattern) :type pattern:pattern-literal :read-only t)
  (then    (util:required 'then)    :type dtree                   :read-only t)
  (else    (util:required 'else)    :type dtree                   :read-only t))

(defun peel-pattern (pat col-var bindings)
  "Strip outer variable and binding patterns from PAT on COL-VAR,
accumulating variable bindings into BINDINGS.
Returns (VALUES CORE-PATTERN NEW-BINDINGS)."
  (declare (type pattern:pattern pat)
           (type symbol col-var)
           (type list bindings)
           (values pattern:pattern list &optional))
  (etypecase pat
    (pattern:pattern-wildcard
     (values pat bindings))
    (pattern:pattern-var
     (values (pattern:make-pattern-wildcard :type (pattern:pattern-type pat))
             (cons (make-match-binding :var (pattern:pattern-var-name pat)
                                       :source col-var
                                       :type (pattern:pattern-type pat))
                   bindings)))
    (pattern:pattern-binding
     (let ((var-name (pattern:pattern-var-name (pattern:pattern-binding-var pat)))
           (inner (pattern:pattern-binding-pattern pat)))
       (peel-pattern inner
                     col-var
                     (cons (make-match-binding :var var-name
                                               :source col-var
                                               :type (pattern:pattern-type pat))
                           bindings))))
    ((or pattern:pattern-constructor pattern:pattern-literal)
     (values pat bindings))))

(defun constructor-test-form (ctor-name ctor-entry type-entry col-var)
  (declare (type symbol ctor-name col-var)
           (type tc:constructor-entry ctor-entry)
           (type tc:type-entry type-entry))
  (cond
    ((tc:type-entry-enum-repr type-entry)
     `(eql ',(tc:constructor-entry-compressed-repr ctor-entry) ,col-var))
    ((eql ctor-name 'coalton:Cons)
     `(cl:consp ,col-var))
    ((eql ctor-name 'coalton:Nil)
     `(null ,col-var))
    ((eql ctor-name 'coalton:Some)
     `(rt:cl-some-p ,col-var))
    ((eql ctor-name 'coalton:None)
     `(rt:cl-none-p ,col-var))
    (t
     `(typep ,col-var ',(tc:constructor-entry-classname ctor-entry)))))

(defun column-type-name (column)
  (let ((head (tc:base-type (match-column-type column))))
    (and (tc:tycon-p head) (tc:tycon-name head))))

(defun specialize-constructor (column ctor-entry env table)
  "Instantiate the constructor once per incoming column, not per row. Its
result refines *all* columns on this edge, including for a nullary witness.
NIL means the constructor cannot inhabit these indices."
  (let* ((name (tc:constructor-entry-name ctor-entry))
         (key (cons column name)))
    (multiple-value-bind (cached foundp) (gethash key table)
      (when foundp (return-from specialize-constructor cached)))
    (assert (eq (column-type-name column) (tc:constructor-entry-constructs ctor-entry)))
    (let* ((scheme (tc:lookup-value-type env name))
           (binders (tc:ty-scheme-instantiation-types scheme))
           (ctor (tc:qualified-ty-type (tc:fresh-inst scheme binders))))
      (multiple-value-bind (subs possible)
          (tc:try-unify nil (tc:function-return-type ctor) (match-column-type column) :all)
        (setf (gethash key table)
              (when possible
                (make-constructor-specialization
                 :type-arguments (tc:apply-substitution subs binders)
                 :substitutions (let ((vars (tc:type-variables (match-column-type column))))
                                  (remove-if-not (lambda (s) (member (tc:substitution-from s) vars :test #'tc:ty=))
                                                 subs))
                 :fields (loop :for ft :in (tc:function-type-arguments ctor)
                               :collect (make-match-column :var (gensym "MATCH-FIELD-")
                                                           :type (tc:apply-substitution subs ft)))
                 :accessors
                 (let ((var (match-column-var column)))
                   (cond ((tc:type-entry-newtype (tc:lookup-type env (column-type-name column))) (list var))
                         ((eq name 'coalton:Cons) (list `(car ,var) `(cdr ,var)))
                         ((eq name 'coalton:Some) (list `(rt:unwrap-cl-some ,var)))
                         (t (loop :with class := (tc:constructor-entry-classname ctor-entry)
                                  :for i :below (tc:constructor-entry-arity ctor-entry)
                                  :collect `(,(alexandria:format-symbol (symbol-package class) "~A-_~D" class i)
                                             ,var))))))))))))

(defun open-pattern-binders (pat spec existing)
  "Give this row's existential binders the names of this column's opening.
Never use the row's refined child types as evidence about shared columns."
  (loop :with args := (constructor-specialization-type-arguments spec)
        :for (index . var) :in (pattern:pattern-constructor-openings pat)
        :collect (tc:make-substitution :from var :to (nth index args)) :into new
        :finally (return (append new existing))))

(defun row-evidence (row givens)
  (let ((openings (match-row-openings row)))
    (cond ((null openings) givens)
          ((null givens) openings)
          (t (tc:compose-substitution-lists givens openings)))))

(defun refine-columns (columns subs)
  (loop :for column :in columns
        :for type := (tc:apply-substitution subs (match-column-type column))
        :collect (if (tc:ty= type (match-column-type column))
                     column
                     (make-match-column :var (match-column-var column) :type type))))

(defun replace-nth-with-list (list index replacement)
  (declare (type list list replacement)
           (type fixnum index)
           (values list &optional))
  (append (subseq list 0 index)
          replacement
          (nthcdr (1+ index) list)))

(defun remove-nth (list index)
  (declare (type list list)
           (type fixnum index)
           (values list &optional))
  (append (subseq list 0 index)
          (nthcdr (1+ index) list)))

(defun prune-wildcard-columns (columns rows)
  "Remove columns where every row in ROWS has a wildcard pattern."
  (declare (type list columns rows)
           (values list list &optional))
  (let ((active-indices
          (loop :for col :in columns
                :for i :from 0
                :when (some (lambda (row)
                              (not (pattern:pattern-wildcard-p
                                    (nth i (match-row-patterns row)))))
                            rows)
                  :collect i)))
    (if (= (length active-indices) (length columns))
        (values columns rows)
        (values
         (loop :for i :in active-indices
               :collect (nth i columns))
         (loop :for row :in rows
               :for pats := (match-row-patterns row)
               :collect (make-match-row
                         :patterns (loop :for i :in active-indices
                                         :collect (nth i pats))
                         :bindings (match-row-bindings row)
                         :openings (match-row-openings row)
                         :action-index (match-row-action-index row)))))))

(defun specialize-rows-for-constructor (rows col-idx ctor-name spec)
  (declare (type list rows)
           (type fixnum col-idx)
           (type symbol ctor-name)
           (values list &optional))
  (let* ((field-columns (constructor-specialization-fields spec))
         (wildcard-fields
          (loop :for fc :in field-columns
                :collect (pattern:make-pattern-wildcard :type (match-column-type fc)))))
    (loop :for row :in rows
          :for pat := (nth col-idx (match-row-patterns row))
          :when (and (pattern:pattern-constructor-p pat)
                     (eq (pattern:pattern-constructor-name pat) ctor-name))
            :collect (let ((bindings (match-row-bindings row))
                           (peeled-subpats nil))
                       (loop :for subpat :in (pattern:pattern-constructor-patterns pat)
                             :for fc :in field-columns
                             :do (multiple-value-bind (core-pat new-bindings)
                                     (peel-pattern subpat
                                                   (match-column-var fc)
                                                   bindings)
                                   (push core-pat peeled-subpats)
                                   (setf bindings new-bindings)))
                       (make-match-row
                        :patterns (replace-nth-with-list (match-row-patterns row)
                                                         col-idx
                                                         (nreverse peeled-subpats))
                        :bindings bindings
                        :openings (open-pattern-binders pat spec (match-row-openings row))
                        :action-index (match-row-action-index row)))
          :when (pattern:pattern-wildcard-p pat)
            :collect (make-match-row
                      :patterns (replace-nth-with-list (match-row-patterns row)
                                                       col-idx
                                                       wildcard-fields)
                      :bindings (match-row-bindings row)
                      :openings (match-row-openings row)
                      :action-index (match-row-action-index row)))))

(defun default-rows-for-column (rows col-idx)
  (declare (type list rows)
           (type fixnum col-idx)
           (values list &optional))
  (loop :for row :in rows
        :for pat := (nth col-idx (match-row-patterns row))
        :when (pattern:pattern-wildcard-p pat)
          :collect (make-match-row
                    :patterns (remove-nth (match-row-patterns row) col-idx)
                    :bindings (match-row-bindings row)
                    :openings (match-row-openings row)
                    :action-index (match-row-action-index row))))

(defun matrix-memo-key (columns rows failure givens)
  (declare (type list columns rows))
  ;; A variable can occur under different proven types. Share whole typed
  ;; columns, not their names. Non-wildcard patterns come from the original
  ;; rows; comparing them by identity also preserves their literal tests.
  (list columns failure givens
        (loop :for row :in rows
              :collect (list (match-row-action-index row)
                             (match-row-openings row)
                             (loop :for pat :in (match-row-patterns row)
                                   :collect (unless (pattern:pattern-wildcard-p pat) pat))
                             (loop :for b :in (match-row-bindings row)
                                   :collect (cons (match-binding-var b)
                                                  (match-binding-source b)))))))

(defun constructor-family (pat env)
  (tc:constructor-entry-constructs (tc:lookup-constructor env (pattern:pattern-constructor-name pat))))

(defun match-test-state (pat column givens env dictionary-equalities)
  (etypecase pat
    (pattern:pattern-wildcard nil)
    (pattern:pattern-constructor
     (let ((actual (column-type-name column)))
       (cond ((null actual) nil)
             ((eq actual (constructor-family pat env)) :ready)
             (t :impossible))))
    (pattern:pattern-literal
     (let* ((expected (tc:apply-substitution givens (pattern:pattern-type pat)))
            (head (tc:base-type expected))
            (actual (column-type-name column)))
       (cond
         ((and actual (tc:tycon-p head) (not (eq actual (tc:tycon-name head))))
          :impossible)
         ((and (tc:ty= expected (match-column-type column))
               (loop :for (a b) :on (and dictionary-equalities (pattern:pattern-literal-test pat)
                                         (funcall dictionary-equalities (pattern:pattern-literal-test pat)))
                     :by #'cddr
                     :always (tc:ty= (tc:apply-substitution givens a) (tc:apply-substitution givens b))))
          :ready))))))

(defun next-match-column (patterns columns givens env &optional dictionary-equalities)
  "Select the first ready test. Defer only a test whose operand or dictionary
awaits evidence, not unrelated tests between it and its witness. A row's
constructor name is not evidence about its own operand."
  (let ((best nil) (best-order nil))
    (loop :for p :in patterns :for c :in columns :for i :from 0
          :for order := (or (pattern:pattern-test-order p) i)
          :when (or (null best-order) (< order best-order))
            :do (let ((state (match-test-state p c givens env dictionary-equalities)))
                  (when state
                    (setf best (and (eq state :ready) i)
                          best-order order))))
    (unless best-order
      (util:coalton-bug "No established test in row ~S for columns of types ~S"
                        patterns (mapcar #'match-column-type columns)))
    best))

(defun user-comparison-p (pat)
  (etypecase pat
    (pattern:pattern-literal (pattern:pattern-literal-test pat))
    (pattern:pattern-constructor (some #'user-comparison-p (pattern:pattern-constructor-patterns pat)))
    (pattern:pattern-binding (user-comparison-p (pattern:pattern-binding-pattern pat)))
    ((or pattern:pattern-wildcard pattern:pattern-var) nil)))

(defun specialized-columns (columns idx spec givens)
  (let ((new (constructor-specialization-substitutions spec)))
    (values (replace-nth-with-list (if new (refine-columns columns new) columns) idx
                                   (constructor-specialization-fields spec))
            (if new (tc:compose-substitution-lists new givens) givens))))

(defun walk-pattern-row (patterns columns env dictionary-equalities visit)
  "Visit one test at a time with the evidence available on entry to that
test. Return bindings and final evidence only on the success path."
  (let ((table (make-hash-table :test #'equal))
        (bindings nil)
        (givens nil))
    (flet ((peel (p col)
             (multiple-value-bind (core new) (peel-pattern p (match-column-var col) bindings)
               (setf bindings new)
               core)))
      (setf patterns (mapcar #'peel patterns columns))
      (loop :until (every #'pattern:pattern-wildcard-p patterns)
            :for idx := (next-match-column patterns columns givens env dictionary-equalities)
            :unless idx :do (return-from walk-pattern-row (values nil nil nil))
            :do (let ((p (nth idx patterns)) (col (nth idx columns)))
                  (etypecase p
                    (pattern:pattern-literal
                     (funcall visit p col nil givens)
                     (setf columns (remove-nth columns idx) patterns (remove-nth patterns idx)))
                    (pattern:pattern-constructor
                     (let ((spec (specialize-constructor
                                  col (tc:lookup-constructor env (pattern:pattern-constructor-name p)) env table)))
                       (unless spec (return-from walk-pattern-row (values nil nil nil)))
                       (funcall visit p col spec givens)
                       (setf patterns (replace-nth-with-list
                                       patterns idx
                                       (mapcar #'peel (pattern:pattern-constructor-patterns p)
                                               (constructor-specialization-fields spec))))
                       (multiple-value-setq (columns givens)
                         (specialized-columns columns idx spec (open-pattern-binders p spec givens))))))))
      (values bindings givens t))))

(defun order-pattern-tests (pat type env dictionary-equalities)
  "Fix observable test order before specializing or inlining a copy."
  (unless (and (user-comparison-p pat)
               (coalton-impl/codegen/transformations:pattern-contains-gadt-p pat env))
    (return-from order-pattern-tests pat))
  (let ((order (make-hash-table :test #'eq)))
    (walk-pattern-row (list pat) (list (make-match-column :var (gensym) :type type)) env dictionary-equalities
                      (lambda (p col spec givens)
                        (declare (ignore col spec givens))
                        (setf (gethash p order) (hash-table-count order))))
    (pattern:map-pattern pat :order-function (lambda (p) (gethash p order)))))

(defun lower-parameter-row (patterns vars types body env dictionary-equalities)
  "Lower correlated positional patterns without allocating a tuple. Emit
shallow matches so that even a nested field in another parameter can provide
the next witness. Their scrutinees have *incoming*, not branch-final, types."
  (let ((tests nil)
        (columns (mapcar (lambda (v ty) (make-match-column :var v :type ty)) vars types))
        (result-type (ast:node-type body)))
    (multiple-value-bind (bindings givens possible)
        (walk-pattern-row
         patterns columns env dictionary-equalities
         (lambda (p col spec givens)
           (let ((test (if spec
                           (pattern:make-pattern-constructor
                            :name (pattern:pattern-constructor-name p)
                            :type (match-column-type col)
                            :openings (loop :for (i . nil) :in (pattern:pattern-constructor-openings p)
                                            :for var := (nth i (constructor-specialization-type-arguments spec))
                                            :when (tc:tyvar-p var) :collect (cons i var))
                            :patterns (loop :for child :in (pattern:pattern-constructor-patterns p)
                                            :for fc :in (constructor-specialization-fields spec)
                                            :collect (if (pattern:pattern-wildcard-p child)
                                                         (pattern:make-pattern-wildcard :type (match-column-type fc))
                                                         (pattern:make-pattern-var :type (match-column-type fc)
                                                                                   :name (match-column-var fc)))))
                           (tc:apply-substitution givens p))))
             (push (cons (ast:make-node-local-variable :type (match-column-type col) :value (match-column-var col))
                         test)
                   tests))))
      (cond
        (possible
         (when givens (setf body (tc:apply-substitution givens body)))
         ;; An alias's branch-final declaration is justified only after every
         ;; required witness was checked, not when its source field was read.
         (dolist (b bindings)
           (setf body (ast:make-node-bind
                       :type result-type :name (match-binding-var b)
                       :expr (ast:make-node-local-variable :type (tc:apply-substitution givens (match-binding-type b))
                                                           :value (match-binding-source b))
                       :body body))))
        (t (setf body (ast:make-node-match :type result-type :branches nil
                                          :expr (ast:make-node-local-variable :type (first types) :value (first vars))))))
      (dolist (step tests body)
        (setf body (ast:make-node-match :type result-type :expr (car step)
                                        :branches (list (ast:make-match-branch :pattern (cdr step) :body body))))))))

(defun groupable-row-p (row head col-idx columns givens env)
  "Can this row participate in HEAD's disjoint switch? Never skip an
observable comparison in this row by factoring a different column first."
  (let* ((patterns (match-row-patterns row))
         (pat (nth col-idx patterns)))
    (and (or (not (some #'user-comparison-p patterns))
             (eql col-idx (next-match-column patterns columns (row-evidence row givens) env)))
         (or (pattern:pattern-wildcard-p pat)
             (etypecase head
               (pattern:pattern-constructor
                (and (pattern:pattern-constructor-p pat)
                     (eq (constructor-family head env) (constructor-family pat env))))
               (pattern:pattern-literal
                (and (pattern:pattern-literal-p pat)
                     (not (pattern:pattern-literal-test pat))
                     (tc:ty= (pattern:pattern-type head) (pattern:pattern-type pat)))))))))

(defun compile-pattern-matrix (columns rows env memo-table field-column-table failure &optional givens)
  "Compile the ordered ROWS on COLUMNS. FAILURE is the DAG to enter if none
matches, compiled with the evidence available before these rows were tried."
  (cond
    ((null rows)
     failure)

    ((every #'pattern:pattern-wildcard-p (match-row-patterns (first rows)))
     (make-dtree-leaf
      :action-index (match-row-action-index (first rows))
      :bindings (reverse (match-row-bindings (first rows)))))

    (t
     (multiple-value-bind (columns rows)
         (prune-wildcard-columns columns rows)
       (let ((key (matrix-memo-key columns rows failure givens)))
         (or (gethash key memo-table)
             (setf (gethash key memo-table)
                   (compile-matrix-test columns rows env memo-table field-column-table failure givens))))))))

(defun compile-matrix-test (columns rows env memo-table field-column-table failure givens)
  (flet ((build (columns rows fallback &optional (proof givens))
           (compile-pattern-matrix columns rows env memo-table field-column-table fallback proof)))
    (let* ((patterns (match-row-patterns (first rows)))
           (idx (next-match-column patterns columns (row-evidence (first rows) givens) env)))
      (unless idx
        (return-from compile-matrix-test (build columns (rest rows) failure)))
      (let* ((col (nth idx columns))
             (head (nth idx patterns))
             ;; User equality/fromInt is an opaque ordered test, not a
             ;; partition by literal value. A constructor or even another
             ;; integer literal can match the very same value.
             (suffix (if (and (pattern:pattern-literal-p head) (pattern:pattern-literal-test head))
                         (rest rows)
                         (member-if-not (lambda (row) (groupable-row-p row head idx columns givens env))
                                        (rest rows))))
             (prefix (if suffix (ldiff rows suffix) rows))
             (fallback (build columns suffix failure)))
        (etypecase head
          (pattern:pattern-constructor
           (let* ((type (tc:lookup-type env (column-type-name col)))
                  (names (remove-duplicates
                          (loop :for row :in prefix :for p := (nth idx (match-row-patterns row))
                                :when (pattern:pattern-constructor-p p)
                                  :collect (pattern:pattern-constructor-name p))
                          :from-end t))
                  (cases
                    (loop :for name :in names
                          :for ctor := (tc:lookup-constructor env name)
                          :for spec := (specialize-constructor col ctor env field-column-table)
                          :when spec
                            :collect (multiple-value-bind (next-cols next-givens)
                                         (specialized-columns columns idx spec givens)
                                       (let ((fields (constructor-specialization-fields spec)))
                                         (make-dtree-constructor-case
                                          :ctor-name name
                                          :ctor-entry ctor
                                          :field-columns fields
                                          :field-accessors (constructor-specialization-accessors spec)
                                          :subtree (build next-cols
                                                          (specialize-rows-for-constructor prefix idx name spec)
                                                          fallback next-givens))))))
                  (default (build (remove-nth columns idx) (default-rows-for-column prefix idx) fallback)))
             (if cases
                 (make-dtree-constructor-switch
                  :column col :type-entry type :cases cases :default default
                  :complete-p (subsetp (tc:type-entry-constructors type)
                                       (mapcar #'dtree-constructor-case-ctor-name cases)))
                 default)))
          (pattern:pattern-literal
           (flet ((same-value-p (p)
                    (and (pattern:pattern-literal-p p)
                         (util:literal-equal (pattern:pattern-literal-value p)
                                             (pattern:pattern-literal-value head)))))
             (let ((then (loop :for row :in prefix :for p := (nth idx (match-row-patterns row))
                               :when (or (pattern:pattern-wildcard-p p) (same-value-p p))
                                 :collect (make-match-row :patterns (remove-nth (match-row-patterns row) idx)
                                                          :bindings (match-row-bindings row)
                                                          :openings (match-row-openings row)
                                                          :action-index (match-row-action-index row))))
                   (else (remove-if (lambda (row) (same-value-p (nth idx (match-row-patterns row)))) prefix)))
               (make-dtree-literal-if :column col :pattern head
                                      :then (build (remove-nth columns idx) then fallback)
                                      :else (build columns else fallback))))))))))

(defun dtree-omit-default-p (node)
  (declare (type dtree-constructor-switch node)
           (values boolean &optional))
  (and (dtree-constructor-switch-complete-p node)
       (or (tc:type-entry-newtype (dtree-constructor-switch-type-entry node))
           (and (settings:coalton-release-p) settings:*emit-type-annotations*))))

(defun compute-free-column-vars (tree table)
  "Compute TREE's free variables from its successors, already visited in postorder."
  (declare (type (or null dtree) tree)
           (values list &optional))
  (etypecase tree
    (null nil)
    (dtree-fail nil)
    (dtree-leaf
     (remove-duplicates (mapcar #'match-binding-source (dtree-leaf-bindings tree))
                        :test #'eq))
    (dtree-constructor-switch
     (let ((free (list (match-column-var (dtree-constructor-switch-column tree)))))
       (dolist (c (dtree-constructor-switch-cases tree))
         (let ((sub-free (gethash (dtree-constructor-case-subtree c) table))
               (bound (mapcar #'match-column-var (dtree-constructor-case-field-columns c))))
           (setf free (union free (set-difference sub-free bound :test #'eq) :test #'eq))))
       (unless (dtree-omit-default-p tree)
         (setf free (union free (gethash (dtree-constructor-switch-default tree) table)
                           :test #'eq)))
       free))
    (dtree-literal-if
     (union (list (match-column-var (dtree-literal-if-column tree)))
            (union (gethash (dtree-literal-if-then tree) table)
                   (gethash (dtree-literal-if-else tree) table)
                   :test #'eq)
            :test #'eq))))

(defun analyze-dtree-dag (tree)
  "Count references, record free variables and find joins in one traversal.
In particular, don't expand a shared row-failure continuation at each edge."
  (declare (type dtree tree))
  (let ((action-counts (make-hash-table :test #'eql))
        (node-counts (make-hash-table :test #'eq))
        (free-vars (make-hash-table :test #'eq))
        (action-leaves (make-hash-table :test #'eql))
        (shared-postorder nil))
    (labels ((walk (node)
               (etypecase node
                 (null nil)
                 (dtree-fail nil)
                 (dtree-leaf
                  (let ((idx (dtree-leaf-action-index node)))
                    (incf (gethash idx action-counts 0))
                    (setf (gethash idx action-leaves) node
                          (gethash node free-vars) (compute-free-column-vars node free-vars))))
                 ((or dtree-constructor-switch dtree-literal-if)
                  (incf (gethash node node-counts 0))
                  (unless (nth-value 1 (gethash node free-vars))
                    (etypecase node
                      (dtree-constructor-switch
                       (dolist (c (dtree-constructor-switch-cases node))
                         (walk (dtree-constructor-case-subtree c)))
                       (unless (dtree-omit-default-p node)
                         (walk (dtree-constructor-switch-default node))))
                      (dtree-literal-if
                       (walk (dtree-literal-if-then node))
                       (walk (dtree-literal-if-else node))))
                    (setf (gethash node free-vars) (compute-free-column-vars node free-vars))
                    (push node shared-postorder))))))
      (walk tree)
      (values action-counts
              action-leaves
              free-vars
              (loop :for node :in (nreverse shared-postorder)
                    :when (> (gethash node node-counts 0) 1)
                      :collect node)))))

(defun collect-dtree-column-lisp-types (tree root-col env)
  "Build a hash-table mapping every column variable in TREE to its Lisp type."
  (declare (type dtree tree)
           (type match-column root-col)
           (type tc:environment env)
           (values hash-table &optional))
  (let ((table (make-hash-table :test #'eq))
        (visited (make-hash-table :test #'eq)))
    (setf (gethash (match-column-var root-col) table)
          (tc:lisp-type (match-column-type root-col) env))
    (labels ((walk (node)
               (when (and node (not (gethash node visited)))
                 (setf (gethash node visited) t)
                 (etypecase node
                   (dtree-fail nil)
                   (dtree-leaf
                    (dolist (b (dtree-leaf-bindings node))
                      (unless (gethash (match-binding-source b) table)
                        (setf (gethash (match-binding-source b) table)
                              (tc:lisp-type (match-binding-type b) env)))))
                   (dtree-constructor-switch
                    (dolist (c (dtree-constructor-switch-cases node))
                      (dolist (fc (dtree-constructor-case-field-columns c))
                        (setf (gethash (match-column-var fc) table)
                              (tc:lisp-type (match-column-type fc) env)))
                      (walk (dtree-constructor-case-subtree c)))
                    (walk (dtree-constructor-switch-default node)))
                   (dtree-literal-if
                    (walk (dtree-literal-if-then node))
                    (walk (dtree-literal-if-else node)))))))
      (walk tree)
      table)))

(defun trivial-branch-code-p (code)
  "Return true when CODE is small enough to inline at multiple decision-tree leaves."
  (or (symbolp code)
      (numberp code)
      (characterp code)
      (stringp code)
      (and (consp code)
           (eq (first code) 'quote))))

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
      ,(if (endp bindings)
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
      ((pattern:pattern-constructor-p pattern)
       (let* ((name (pattern:pattern-constructor-name pattern))
              (entry (tc:lookup-constructor env name)))
         `((,(tc:constructor-entry-compressed-repr entry))
           ,code)))

      (t
       `(otherwise
         (let ,bindings
           ,(codegen-binding-types bindings types)
           ,code))))))

(defun codegen-match-branch (code pattern match-var match-expr-type env emit-jumptable-p)
  "Generate code for a match branch."
  (declare (type t code)
           (type pattern:pattern pattern)
           (type symbol match-var)
           (type tc:ty match-expr-type)
           (type tc:environment env)
           (type boolean emit-jumptable-p)
           (values t &optional))

  (funcall (if emit-jumptable-p
               #'codegen-case-match-branch
               #'codegen-cond-match-branch)
           code
           pattern
           match-var
           match-expr-type
           env))

(defun make-last-branch-unconditional (branches catch-all-key)
  "Replace the test of the last of BRANCHES with CATCH-ALL-KEY.

This is only valid when the match is known to be exhaustive. Without
it, a `cl:cond' or `cl:case' lacking a fallback has an implicit NIL
return path, which (among other things) can prevent SBCL from
compiling self-calls in the other branches as tail calls."
  (declare (type list branches)
           (type symbol catch-all-key)
           (values list &optional))

  (if (endp branches)
      branches
      (append (butlast branches)
              (list (cons catch-all-key (rest (car (last branches))))))))

(defun literal-pattern-test-form (pat col-var env)
  (declare (type pattern:pattern-literal pat)
           (type symbol col-var)
           (type tc:environment env))
  (let ((val (pattern:pattern-literal-value pat)))
    (cond
      ((pattern:pattern-literal-test pat)
       (codegen-pattern:codegen-pattern-test (pattern:pattern-literal-test pat) col-var env))
      ((stringp val)
       `(string= ,val ,col-var))
      (t
       `(eql ,val ,col-var)))))

(defun emit-dtree (tree branch-patterns branch-codes arm-fn-table node-join-table free-vars env &key ignore-join-root)
  "Emit Common Lisp code for decision DAG TREE."
  (declare (type dtree tree)
           (type vector branch-patterns branch-codes)
           (type hash-table arm-fn-table node-join-table)
           (type tc:environment env))
  (alexandria:if-let ((join-entry (and (not (eq tree ignore-join-root))
                                       (gethash tree node-join-table))))
    `(,(car join-entry) ,@(cdr join-entry))
    (labels ((emit-constructor-case-body (c)
               (let* ((subtree (dtree-constructor-case-subtree c))
                      (used-vars (gethash subtree free-vars))
                      (let-bindings nil)
                      (let-types nil))
                 (loop :for fc :in (dtree-constructor-case-field-columns c)
                       :for acc :in (dtree-constructor-case-field-accessors c)
                       :when (member (match-column-var fc) used-vars :test #'eq)
                         :do (push (list (match-column-var fc) acc) let-bindings)
                             (push (tc:lisp-type (match-column-type fc) env) let-types))
                 (setf let-bindings (nreverse let-bindings)
                       let-types (nreverse let-types))
                 (let ((body-code (emit-dtree subtree branch-patterns branch-codes
                                              arm-fn-table node-join-table free-vars env)))
                   (if (null let-bindings)
                       body-code
                       `(let ,let-bindings
                          ,(codegen-binding-types let-bindings let-types)
                          ,body-code))))))
      (etypecase tree
        (dtree-fail
         `(error "Pattern match not exhaustive error."))

        (dtree-leaf
         (let* ((idx (dtree-leaf-action-index tree))
                (bindings (dtree-leaf-bindings tree))
                (arm-fn (gethash idx arm-fn-table)))
           (if arm-fn
               (let ((ordered-vars (pattern:pattern-variables (aref branch-patterns idx))))
                 `(,arm-fn
                   ,@(loop :for v :in ordered-vars
                           :for b := (find v bindings :key #'match-binding-var :test #'eq)
                           :collect (match-binding-source b))))
               (let ((code (aref branch-codes idx)))
                 (if (null bindings)
                     code
                     (let ((let-bindings
                             (loop :for b :in bindings
                                   :collect (list (match-binding-var b)
                                                  (match-binding-source b))))
                           (let-types
                             (loop :for b :in bindings :collect (tc:lisp-type (match-binding-type b) env))))
                       `(let ,let-bindings
                          ,(codegen-binding-types let-bindings let-types)
                          ,code)))))))

        (dtree-constructor-switch
         (let* ((col-var (match-column-var (dtree-constructor-switch-column tree)))
                (type-entry (dtree-constructor-switch-type-entry tree))
                (cases (dtree-constructor-switch-cases tree))
                (default (dtree-constructor-switch-default tree))
                (omit-default-p (dtree-omit-default-p tree)))
           (cond
             ;; Exhaustive single-constructor type in release mode: branchless!
             ((and omit-default-p (= 1 (length cases)))
              (emit-constructor-case-body (first cases)))

             ;; Enum type: emit `cl:case` jump table
             ((tc:type-entry-enum-repr type-entry)
              (let ((case-clauses
                      (loop :for c :in cases
                            :for entry := (dtree-constructor-case-ctor-entry c)
                            :collect `((,(tc:constructor-entry-compressed-repr entry))
                                       ,(emit-constructor-case-body c)))))
                (if omit-default-p
                    `(case ,col-var
                       ,@(make-last-branch-unconditional case-clauses 'otherwise))
                    `(case ,col-var
                       ,@case-clauses
                       (otherwise ,(emit-dtree default branch-patterns branch-codes
                                               arm-fn-table node-join-table free-vars env))))))

             ;; Non-enum ADT: emit `cl:cond`
             (t
              (let ((cond-clauses
                      (loop :for c :in cases
                            :collect `(,(constructor-test-form
                                         (dtree-constructor-case-ctor-name c)
                                         (dtree-constructor-case-ctor-entry c)
                                         type-entry
                                         col-var)
                                       ,(emit-constructor-case-body c)))))
                (if omit-default-p
                    `(cond
                       ,@(make-last-branch-unconditional cond-clauses 't))
                    `(cond
                       ,@cond-clauses
                       (t ,(emit-dtree default branch-patterns branch-codes
                                       arm-fn-table node-join-table free-vars env)))))))))

        (dtree-literal-if
         (let ((col-var (match-column-var (dtree-literal-if-column tree)))
               (pat (dtree-literal-if-pattern tree)))
           `(if ,(literal-pattern-test-form pat col-var env)
                ,(emit-dtree (dtree-literal-if-then tree) branch-patterns branch-codes
                             arm-fn-table node-join-table free-vars env)
                ,(emit-dtree (dtree-literal-if-else tree) branch-patterns branch-codes
                             arm-fn-table node-join-table free-vars env))))))))

(defun compile-match-decision-tree (match-var subexpr-type pattern-code-pairs env)
  "Compile PATTERN-CODE-PAIRS on MATCH-VAR of type SUBEXPR-TYPE into a Common Lisp form."
  (declare (type symbol match-var)
           (type tc:ty subexpr-type)
           (type list pattern-code-pairs)
           (type tc:environment env))
  (let* ((num-branches (length pattern-code-pairs))
         (branch-patterns (make-array num-branches))
         (branch-codes (make-array num-branches))
         (root-col (make-match-column :var match-var :type subexpr-type))
         (memo-table (make-hash-table :test #'equal))
         (field-column-table (make-hash-table :test #'equal))
         (rows
           (loop :for (pat . code) :in pattern-code-pairs
                 :for idx :from 0
                 :do (setf (aref branch-patterns idx) pat
                           (aref branch-codes idx) code)
                 :collect (multiple-value-bind (core-pat bindings)
                              (peel-pattern pat match-var nil)
                            (make-match-row
                             :patterns (list core-pat)
                             :bindings bindings
                             :action-index idx))))
         (tree (compile-pattern-matrix (list root-col) rows env memo-table field-column-table (make-dtree-fail)))
         (col-type-table (collect-dtree-column-lisp-types tree root-col env)))
    (multiple-value-bind (action-counts action-leaves free-table shared-nodes)
        (analyze-dtree-dag tree)
      (let ((arm-fn-table (make-hash-table :test #'eql))
            (node-join-table (make-hash-table :test #'eq))
            (labels-defs nil))
        ;; 1. Shared leaf branch bodies
        (loop :for idx :from 0 :below num-branches
              :for count := (gethash idx action-counts 0)
              :for pat := (aref branch-patterns idx)
              :for code := (aref branch-codes idx)
              :when (and (> count 1)
                         (not (trivial-branch-code-p code)))
                :do (let* ((fn-name (gensym (format nil "MATCH-ARM-~D-" idx)))
                           (vars (pattern:pattern-variables pat))
                           (sample-bindings (dtree-leaf-bindings (gethash idx action-leaves)))
                           (var-types
                             (loop :for v :in vars
                                   :for b := (find v sample-bindings :key #'match-binding-var :test #'eq)
                                   :collect (if b (tc:lisp-type (match-binding-type b) env) 't))))
                      (setf (gethash idx arm-fn-table) fn-name)
                      (push `(,fn-name ,vars
                               ,(codegen-binding-types
                                 (mapcar #'list vars vars)
                                 var-types)
                               ,code)
                            labels-defs)))
        ;; 2. Shared interior DAG decision nodes (in postorder so earlier definitions can be called by later ones)
        (loop :for shared-node :in shared-nodes
              :for idx :from 0
              :unless (eq shared-node tree)
                :do (let ((fn-name (gensym (format nil "MATCH-JOIN-~D-" idx)))
                          (free-vars (gethash shared-node free-table)))
                      (setf (gethash shared-node node-join-table)
                            (cons fn-name free-vars))))
        (loop :for shared-node :in shared-nodes
              :for join-entry := (gethash shared-node node-join-table)
              :when join-entry
                :do (let* ((fn-name (car join-entry))
                           (free-vars (cdr join-entry))
                           (free-types
                             (loop :for v :in free-vars
                                   :collect (gethash v col-type-table 't)))
                           (node-code
                             (emit-dtree shared-node branch-patterns branch-codes
                                         arm-fn-table node-join-table free-table env
                                         :ignore-join-root shared-node)))
                      (push `(,fn-name ,free-vars
                               ,(codegen-binding-types
                                 (mapcar #'list free-vars free-vars)
                                 free-types)
                               ,node-code)
                            labels-defs)))
        (let ((tree-code (emit-dtree tree branch-patterns branch-codes
                                     arm-fn-table node-join-table free-table env)))
          (if (null labels-defs)
              tree-code
              `(labels ,(nreverse labels-defs)
                 ,tree-code)))))))

(defun codegen-match (match-var subexpr-code subexpr-type pattern-code-pairs env)
  "Generate code for a match expression using a decision tree."
  (declare (type symbol match-var)
           (type t subexpr-code)
           (type tc:ty subexpr-type)
           (type list pattern-code-pairs)
           (type tc:environment env)
           (values t &optional))

  `(let ((,match-var ,subexpr-code))
     (declare ,@(list*
                 `(ignorable ,match-var)
                 (if (not settings:*emit-type-annotations*)
                     '()
                     (list `(type ,(tc:lisp-type subexpr-type env) ,match-var)))))

     (locally
         #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
         ,(compile-match-decision-tree match-var subexpr-type pattern-code-pairs env))))

