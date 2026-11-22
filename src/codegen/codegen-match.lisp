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

(defstruct (match-binding (:copier nil))
  (var       (util:required 'var)       :type symbol :read-only t)
  (source    (util:required 'source)    :type symbol :read-only t)
  (lisp-type (util:required 'lisp-type) :type t      :read-only t))

(defstruct (match-row (:copier nil))
  (patterns     (util:required 'patterns)     :type list   :read-only t)
  (bindings     (util:required 'bindings)     :type list   :read-only t)
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

(defun peel-pattern (pat col-var bindings env)
  "Strip outer variable, binding, and newtype wrappers from PAT on COL-VAR,
accumulating variable bindings into BINDINGS.
Returns (VALUES CORE-PATTERN NEW-BINDINGS)."
  (declare (type pattern:pattern pat)
           (type symbol col-var)
           (type list bindings)
           (type tc:environment env)
           (values pattern:pattern list &optional))
  (etypecase pat
    (pattern:pattern-wildcard
     (values pat bindings))
    (pattern:pattern-var
     (values (pattern:make-pattern-wildcard :type (pattern:pattern-type pat))
             (cons (make-match-binding :var (pattern:pattern-var-name pat)
                                       :source col-var
                                       :lisp-type (tc:lisp-type (pattern:pattern-type pat) env))
                   bindings)))
    (pattern:pattern-binding
     (let ((var-name (pattern:pattern-var-name (pattern:pattern-binding-var pat)))
           (inner (pattern:pattern-binding-pattern pat)))
       (peel-pattern inner
                     col-var
                     (cons (make-match-binding :var var-name
                                               :source col-var
                                               :lisp-type (tc:lisp-type (pattern:pattern-type pat) env))
                           bindings)
                     env)))
    (pattern:pattern-constructor
     (let* ((ctor-name (pattern:pattern-constructor-name pat))
            (ctor-entry (tc:lookup-constructor env ctor-name))
            (type-entry (tc:lookup-type env (tc:constructor-entry-constructs ctor-entry))))
       (if (tc:type-entry-newtype type-entry)
           (let ((inner (first (pattern:pattern-constructor-patterns pat))))
             (peel-pattern inner col-var bindings env))
           (values pat bindings))))
    (pattern:pattern-literal
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

(defun constructor-fields-info (ctor-name ctor-entry col-var col-type env)
  "Return (VALUES FIELD-TYPES FIELD-ACCESSOR-FORMS) for CTOR-NAME applied to COL-VAR."
  (declare (type symbol ctor-name col-var)
           (type tc:constructor-entry ctor-entry)
           (type tc:ty col-type)
           (type tc:environment env)
           (values list list &optional))
  (when (zerop (tc:constructor-entry-arity ctor-entry))
    (return-from constructor-fields-info (values nil nil)))
  (let* ((ctor-type (tc:qualified-ty-type (tc:fresh-inst (tc:lookup-value-type env ctor-name))))
         (result (tc:function-return-type ctor-type))
         ;; Rows share these projections. A peeled newtype or an abstract
         ;; index need not expose the constructor head in the incoming column.
         (subs (when (tc:ty= (tc:base-type result) (tc:base-type col-type))
                 (tc:unify-pattern-result nil result col-type :all)))
         (field-types (tc:apply-substitution subs (tc:function-type-arguments ctor-type))))
    (values field-types
            (cond ((eq ctor-name 'coalton:Cons) (list `(car ,col-var) `(cdr ,col-var)))
                  ((eq ctor-name 'coalton:Some) (list `(rt:unwrap-cl-some ,col-var)))
                  (t (loop :with classname := (tc:constructor-entry-classname ctor-entry)
                           :for i :below (length field-types)
                           :for accessor := (alexandria:format-symbol (symbol-package classname) "~A-_~D" classname i)
                           :collect `(,accessor ,col-var)))))))

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
                         :action-index (match-row-action-index row)))))))

(defun same-literal-pattern-p (p1 p2)
  (declare (type pattern:pattern-literal p1 p2)
           (values boolean &optional))
  (and (eq (null (pattern:pattern-literal-test p1))
           (null (pattern:pattern-literal-test p2)))
       (util:literal-equal (pattern:pattern-literal-value p1)
                           (pattern:pattern-literal-value p2))))

(defun specialize-rows-for-constructor (rows col-idx ctor-name field-columns env)
  (declare (type list rows field-columns)
           (type fixnum col-idx)
           (type symbol ctor-name)
           (type tc:environment env)
           (values list &optional))
  (let ((wildcard-fields
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
                                                   bindings
                                                   env)
                                   (push core-pat peeled-subpats)
                                   (setf bindings new-bindings)))
                       (make-match-row
                        :patterns (replace-nth-with-list (match-row-patterns row)
                                                         col-idx
                                                         (nreverse peeled-subpats))
                        :bindings bindings
                        :action-index (match-row-action-index row)))
          :when (pattern:pattern-wildcard-p pat)
            :collect (make-match-row
                      :patterns (replace-nth-with-list (match-row-patterns row)
                                                       col-idx
                                                       wildcard-fields)
                      :bindings (match-row-bindings row)
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
                    :action-index (match-row-action-index row))))

(defun pattern-signature (pat)
  "Return a structural key for PAT suitable for EQUALP hashing in matrix memoization."
  (declare (type pattern:pattern pat))
  (etypecase pat
    (pattern:pattern-wildcard
     ':wildcard)
    (pattern:pattern-var
     (list ':var (pattern:pattern-var-name pat)))
    (pattern:pattern-binding
     (list ':binding
           (pattern:pattern-var-name (pattern:pattern-binding-var pat))
           (pattern-signature (pattern:pattern-binding-pattern pat))))
    (pattern:pattern-literal
     (list ':literal
           (pattern:pattern-literal-value pat)
           (pattern:pattern-literal-test pat)))
    (pattern:pattern-constructor
     (list* ':ctor
            (pattern:pattern-constructor-name pat)
            (mapcar #'pattern-signature (pattern:pattern-constructor-patterns pat))))))

(defun matrix-memo-key (columns rows)
  (declare (type list columns rows))
  (list (mapcar #'match-column-var columns)
        (loop :for row :in rows
              :collect (list (match-row-action-index row)
                             (mapcar #'pattern-signature (match-row-patterns row))
                             (loop :for b :in (match-row-bindings row)
                                   :collect (cons (match-binding-var b)
                                                  (match-binding-source b)))))))

(defun canonical-field-columns (col-var ctor-name field-types field-column-table)
  "Return canonical match-column structures for the fields of CTOR-NAME on COL-VAR."
  (declare (type symbol col-var ctor-name)
           (type list field-types)
           (type hash-table field-column-table)
           (values list &optional))
  (let ((key (cons col-var ctor-name)))
    (or (gethash key field-column-table)
        (setf (gethash key field-column-table)
              (loop :for ft :in field-types
                    :collect (make-match-column
                              :var (gensym "MATCH-FIELD-")
                              :type ft))))))

(defun witness-column-index (patterns columns env)
  (let ((first-witness nil))
    (loop :for pat :in patterns :for col :in columns :for i :from 0
          :when (coalton-impl/codegen/transformations:pattern-contains-gadt-p pat env)
            :do (unless first-witness (setf first-witness i))
                (when (tc:tycon-p (tc:base-type (match-column-type col))) (return i))
          :finally (return first-witness))))

(defun compile-pattern-matrix (columns rows env memo-table field-column-table)
  "Compile pattern matrix (COLUMNS, ROWS) into a decision DAG."
  (declare (type list columns rows)
           (type tc:environment env)
           (type hash-table memo-table field-column-table)
           (values dtree &optional))
  (cond
    ((null rows)
     (make-dtree-fail))

    ((every #'pattern:pattern-wildcard-p (match-row-patterns (first rows)))
     (make-dtree-leaf
      :action-index (match-row-action-index (first rows))
      :bindings (reverse (match-row-bindings (first rows)))))

    (t
     (multiple-value-bind (columns rows)
         (prune-wildcard-columns columns rows)
       (let ((memo-key (matrix-memo-key columns rows)))
         (alexandria:if-let ((cached (gethash memo-key memo-table)))
           cached
           (let* ((patterns (match-row-patterns (first rows)))
                  ;; Establish tag evidence before running type-specific
                  ;; literal tests on a correlated field.
                  (col-idx (or (witness-column-index patterns columns env)
                               (position-if-not #'pattern:pattern-wildcard-p patterns)))
                  (col (nth col-idx columns))
                  (head-pat (nth col-idx (match-row-patterns (first rows))))
                  (result
                    (etypecase head-pat
                      (pattern:pattern-constructor
                       (let* ((first-ctor (tc:lookup-constructor env (pattern:pattern-constructor-name head-pat)))
                              (type-entry (tc:lookup-type env (tc:constructor-entry-constructs first-ctor)))
                              (ctor-names nil))
                         (dolist (row rows)
                           (let ((pat (nth col-idx (match-row-patterns row))))
                             (when (pattern:pattern-constructor-p pat)
                               (pushnew (pattern:pattern-constructor-name pat) ctor-names :test #'eq))))
                         (setf ctor-names (nreverse ctor-names))
                         (let* ((all-ctors (tc:type-entry-constructors type-entry))
                                (complete-p (null (set-difference all-ctors ctor-names :test #'eq)))
                                (cases
                                  (loop :for ctor-name :in ctor-names
                                        :for ctor-entry := (tc:lookup-constructor env ctor-name)
                                        :collect
                                        (multiple-value-bind (field-types field-accessors)
                                            (constructor-fields-info ctor-name
                                                                     ctor-entry
                                                                     (match-column-var col)
                                                                     (match-column-type col)
                                                                     env)
                                          (let* ((field-columns
                                                   (canonical-field-columns
                                                    (match-column-var col)
                                                    ctor-name
                                                    field-types
                                                    field-column-table))
                                                 (spec-columns
                                                   (replace-nth-with-list columns col-idx field-columns))
                                                 (spec-rows
                                                   (specialize-rows-for-constructor
                                                    rows col-idx ctor-name field-columns env)))
                                            (make-dtree-constructor-case
                                             :ctor-name ctor-name
                                             :ctor-entry ctor-entry
                                             :field-columns field-columns
                                             :field-accessors field-accessors
                                             :subtree (compile-pattern-matrix
                                                       spec-columns spec-rows env
                                                       memo-table field-column-table))))))
                                (non-ctor-rows
                                  (loop :for row :in rows
                                        :for pat := (nth col-idx (match-row-patterns row))
                                        :unless (pattern:pattern-constructor-p pat)
                                          :collect row))
                                (default-dtree
                                  (cond
                                    ((some (lambda (row)
                                             (not (pattern:pattern-wildcard-p
                                                   (nth col-idx (match-row-patterns row)))))
                                           non-ctor-rows)
                                     (compile-pattern-matrix columns non-ctor-rows env
                                                             memo-table field-column-table))
                                    (t
                                     (compile-pattern-matrix (remove-nth columns col-idx)
                                                             (default-rows-for-column rows col-idx)
                                                             env
                                                             memo-table field-column-table)))))
                           (make-dtree-constructor-switch
                            :column col
                            :type-entry type-entry
                            :complete-p complete-p
                            :cases cases
                            :default default-dtree))))

                      (pattern:pattern-literal
                       (let ((then-rows
                               (loop :for row :in rows
                                     :for pat := (nth col-idx (match-row-patterns row))
                                     :when (or (pattern:pattern-wildcard-p pat)
                                               (and (pattern:pattern-literal-p pat)
                                                    (same-literal-pattern-p pat head-pat)))
                                       :collect (make-match-row
                                                 :patterns (remove-nth (match-row-patterns row) col-idx)
                                                 :bindings (match-row-bindings row)
                                                 :action-index (match-row-action-index row))))
                             (else-rows
                               (loop :for row :in rows
                                     :for pat := (nth col-idx (match-row-patterns row))
                                     :unless (and (pattern:pattern-literal-p pat)
                                                  (same-literal-pattern-p pat head-pat))
                                       :collect row)))
                         (make-dtree-literal-if
                          :column col
                          :pattern head-pat
                          :then (compile-pattern-matrix (remove-nth columns col-idx) then-rows env
                                                        memo-table field-column-table)
                          :else (compile-pattern-matrix columns else-rows env
                                                        memo-table field-column-table)))))))
             (setf (gethash memo-key memo-table) result)
             result)))))))

(defun dtree-omit-default-p (node)
  (declare (type dtree-constructor-switch node)
           (values boolean &optional))
  (and (dtree-constructor-switch-complete-p node)
       (settings:coalton-release-p)
       settings:*emit-type-annotations*))

(defun dtree-free-column-vars (tree)
  "Return the list of column variable symbols referenced in TREE that are bound outside TREE."
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
         (let ((sub-free (dtree-free-column-vars (dtree-constructor-case-subtree c)))
               (bound (mapcar #'match-column-var (dtree-constructor-case-field-columns c))))
           (setf free (union free (set-difference sub-free bound :test #'eq) :test #'eq))))
       (unless (dtree-omit-default-p tree)
         (setf free (union free (dtree-free-column-vars (dtree-constructor-switch-default tree))
                           :test #'eq)))
       free))
    (dtree-literal-if
     (union (list (match-column-var (dtree-literal-if-column tree)))
            (union (dtree-free-column-vars (dtree-literal-if-then tree))
                   (dtree-free-column-vars (dtree-literal-if-else tree))
                   :test #'eq)
            :test #'eq))))

(defun analyze-dtree-dag (tree)
  "Traverse reachable DAG nodes of TREE, returning:
  (VALUES ACTION-COUNTS NODE-COUNTS SHARED-NODES-POSTORDER COLUMN-LISP-TYPES-Fn)"
  (declare (type dtree tree))
  (let ((action-counts (make-hash-table :test #'eql))
        (node-counts (make-hash-table :test #'eq))
        (visited (make-hash-table :test #'eq))
        (shared-postorder nil))
    (labels ((walk (node)
               (etypecase node
                 (null nil)
                 (dtree-fail nil)
                 (dtree-leaf
                  (incf (gethash (dtree-leaf-action-index node) action-counts 0)))
                 ((or dtree-constructor-switch dtree-literal-if)
                  (incf (gethash node node-counts 0))
                  (unless (gethash node visited)
                    (setf (gethash node visited) t)
                    (etypecase node
                      (dtree-constructor-switch
                       (dolist (c (dtree-constructor-switch-cases node))
                         (walk (dtree-constructor-case-subtree c)))
                       (unless (dtree-omit-default-p node)
                         (walk (dtree-constructor-switch-default node))))
                      (dtree-literal-if
                       (walk (dtree-literal-if-then node))
                       (walk (dtree-literal-if-else node))))
                    (push node shared-postorder))))))
      (walk tree)
      (values action-counts
              node-counts
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
                              (match-binding-lisp-type b)))))
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

(defun first-dtree-leaf-for-action (tree action-index)
  (declare (type dtree tree)
           (type fixnum action-index)
           (values (or null dtree-leaf) &optional))
  (labels ((walk (node)
             (etypecase node
               (null nil)
               (dtree-fail nil)
               (dtree-leaf
                (when (= action-index (dtree-leaf-action-index node))
                  node))
               (dtree-constructor-switch
                (or (some (lambda (c) (walk (dtree-constructor-case-subtree c)))
                          (dtree-constructor-switch-cases node))
                    (walk (dtree-constructor-switch-default node))))
               (dtree-literal-if
                (or (walk (dtree-literal-if-then node))
                    (walk (dtree-literal-if-else node)))))))
    (walk tree)))

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

(defun emit-dtree (tree branch-patterns branch-codes arm-fn-table node-join-table env &key ignore-join-root)
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
                      (used-vars (dtree-free-column-vars subtree))
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
                                              arm-fn-table node-join-table env)))
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
                             (mapcar #'match-binding-lisp-type bindings)))
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
                                               arm-fn-table node-join-table env))))))

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
                                       arm-fn-table node-join-table env)))))))))

        (dtree-literal-if
         (let ((col-var (match-column-var (dtree-literal-if-column tree)))
               (pat (dtree-literal-if-pattern tree)))
           `(if ,(literal-pattern-test-form pat col-var env)
                ,(emit-dtree (dtree-literal-if-then tree) branch-patterns branch-codes
                             arm-fn-table node-join-table env)
                ,(emit-dtree (dtree-literal-if-else tree) branch-patterns branch-codes
                             arm-fn-table node-join-table env))))))))

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
         (memo-table (make-hash-table :test #'equalp))
         (field-column-table (make-hash-table :test #'equal))
         (rows
           (loop :for (pat . code) :in pattern-code-pairs
                 :for idx :from 0
                 :do (setf (aref branch-patterns idx) pat
                           (aref branch-codes idx) code)
                 :collect (multiple-value-bind (core-pat bindings)
                              (peel-pattern pat match-var nil env)
                            (make-match-row
                             :patterns (list core-pat)
                             :bindings bindings
                             :action-index idx))))
         (tree (compile-pattern-matrix (list root-col) rows env memo-table field-column-table))
         (col-type-table (collect-dtree-column-lisp-types tree root-col env)))
    (multiple-value-bind (action-counts node-counts shared-nodes)
        (analyze-dtree-dag tree)
      (declare (ignore node-counts))
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
                           (sample-leaf (first-dtree-leaf-for-action tree idx))
                           (sample-bindings (and sample-leaf (dtree-leaf-bindings sample-leaf)))
                           (var-types
                             (loop :for v :in vars
                                   :for b := (find v sample-bindings :key #'match-binding-var :test #'eq)
                                   :collect (if b (match-binding-lisp-type b) 't))))
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
                          (free-vars (dtree-free-column-vars shared-node)))
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
                                         arm-fn-table node-join-table env
                                         :ignore-join-root shared-node)))
                      (push `(,fn-name ,free-vars
                               ,(codegen-binding-types
                                 (mapcar #'list free-vars free-vars)
                                 free-types)
                               ,node-code)
                            labels-defs)))
        (let ((tree-code (emit-dtree tree branch-patterns branch-codes
                                     arm-fn-table node-join-table env)))
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

