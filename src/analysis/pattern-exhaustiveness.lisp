(defpackage #:coalton-impl/analysis/pattern-exhaustiveness
  (:use
   #:cl)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:util #:coalton-impl/util))
  (:export
   #:non-exhaustive-match-warning
   #:exhaustive-patterns-p
   #:useful-pattern-p
   #:find-non-matching-value
   #:collapse-binding-patterns))

(in-package #:coalton-impl/analysis/pattern-exhaustiveness)

;;;
;;; This file provides an implementation of pattern matching
;;; exhaustiveness and usefulness analysis as described in
;;; Maranget 2007, http://moscova.inria.fr/~maranget/papers/warn/index.html
;;;

;;; Fundamental concepts for the reader: 
;;;
;;; Patterns
;;; ---------
;;;
;;;    The subtypes of COALTON-IMPL/TYPECHECKER:PATTERN
;;;
;;; Pattern Matrices & Row Matching
;;; -------------------------------
;;;
;;;   A pattern matrix is a data structure that facilitates a search
;;;   for a list of patterns that match a list of values on an
;;;   member-by-member basis.
;;;
;;;   Specifically, the pattern matrix P is a list of lists of
;;;   typed patterns of the form:
;;;
;;;      ((p₁₁ ... p₁ₙ)
;;;       ...
;;;       (pₘ₁ ... pₘₙ))
;;;
;;;   A list of values v = (v₁ ... vₙ) is said to match the iᵗʰ row of
;;;   P if each vⱼ matches each pᵢⱼ and i is the first row index for
;;;   which this true, counting from the top.
;;;
;;; Exhaustiveness
;;; --------------
;;;
;;;   A pattern matrix is exhaustive ⇔ for ∀v of type T=(t₁,...,tₙ),
;;;   ∃i such that v matches the ith row of P.
;;;
;;; Pattern "Usefulness"
;;; ---------------------
;;;
;;;   A row in a pattern matrix P is called useless, or redundant, if
;;;   no list of appropriately typed values will match it. This
;;;   "useless" designation includes the case where the row would
;;;   match a value v if only that row appeared before an earlier
;;;   matching row.
;;;
;;;   For example, consider the pattern matrix P:
;;;
;;;     (((Cons _ _) _)
;;;      ((Cons A B) (Nil))
;;;      ( _         _))
;;;
;;;   then the second row is useless b/c any value will match before
;;;   it is reached.
;;;
;;;   Finally, a pattern list q is USEFUL with respect to a pattern
;;;   matrix P, if there is a value list v such that v does not match
;;;   P but does match q (where v and q and P's rows all have the same
;;;   type).
;;;
;;;

(defvar *obligation-refutations* 0
  "Count of emptiness proofs in the current analysis that relied on
obligations. See \"Field obligations\" and SEARCH-COLUMN-CASES.")

(defun make-wildcard-for-type (type)
  (declare (type tc:ty type)
           (values tc:pattern-wildcard &optional))
  (tc:make-pattern-wildcard
   :type (tc:qualify nil type)))

(defun pattern-bare-type (pattern)
  (declare (type tc:pattern pattern)
           (values tc:ty))
  (tc:qualified-ty-type (tc:pattern-type pattern)))

(defun exhaustive-patterns-p (patterns env column-type)
  "Are PATTERNS exhaustive for COLUMN-TYPE? In practice, column type only matters
for GADTs, because ADTs have the same return type for every constructor."
  (declare (type tc:pattern-list patterns)
           (type tc:environment env)
           (type tc:ty column-type)
           (values boolean))
  (let ((*obligation-refutations* 0))
    (not
     (useful-pattern-clause-p
      (mapcar #'list patterns)
      (list (tc:make-pattern-wildcard
             :type (tc:qualify nil (tc:make-variable))))
      (list column-type)
      env))))

(defun useful-pattern-p (patterns pattern env column-type)
  "Is PATTERN useful within list PATTERNS for COLUMN-TYPE? PATTERN must EQ one element
of PATTERNS."
  (declare (type tc:pattern-list patterns)
           (type tc:pattern pattern)
           (type tc:environment env)
           (type tc:ty column-type)
           (values boolean))
  (let ((*obligation-refutations* 0))
    (useful-pattern-clause-p
     (loop :for p :in patterns
           :while (not (eq p pattern))
           :collect (list p))
     (list pattern)
     (list column-type)
     env)))

(defun pattern-matrix-p (x)
  (and (alexandria:proper-list-p x)
       (every #'tc:pattern-list-p x)
       (or (null x)
           (every
            (lambda (l)
              (= (length l) (length (first x))))
            (cdr x)))))

(deftype pattern-matrix ()
  '(satisfies pattern-matrix-p))

(defun type-entry-for-column-type (type env)
  "Return the TYPE-ENTRY for the head type constructor of TYPE, if any."
  (declare (type tc:ty type)
           (type tc:environment env)
           (values (or null tc:type-entry)))
  (let ((head (first (tc:flatten-type type))))
    (when (tc:tycon-p head)
      (tc:lookup-type env (tc:tycon-name head) :no-error t))))

(defun possible-constructor-pattern (constructor-name column-type env)
  "If CONSTRUCTOR-NAME can construct COLUMN-TYPE, return a synthetic
PATTERN-CONSTRUCTOR whose subpatterns are typed wildcards. Otherwise return NIL."
  (declare (type symbol constructor-name)
           (type tc:ty column-type)
           (type tc:environment env)
           (values (or null tc:pattern-constructor)))
  (let ((scheme (tc:lookup-value-type env constructor-name :no-error t)))
    (when scheme
      (let ((constructor-ty (tc:qualified-ty-type (tc:fresh-inst scheme))))
        ;; Unify, don't match. COLUMN-TYPE may itself contain type
        ;; variables, and a GADT constructor may unify them.
        (multiple-value-bind (subs possible-p)
            (tc:try-unify nil (tc:function-return-type constructor-ty) column-type :all)
          (when possible-p
            (let ((constructor-ty (tc:apply-substitution subs constructor-ty)))
              (tc:make-pattern-constructor :type (tc:qualify nil (tc:function-return-type constructor-ty))
                                           :name constructor-name
                                           :patterns (mapcar #'make-wildcard-for-type
                                                             (tc:function-type-arguments constructor-ty))))))))))

(defun type-entry-for-seen-constructor-patterns (patterns env)
  "Return the TYPE-ENTRY implied by constructor patterns in PATTERNS, if any.

This is needed when COLUMN-TYPE is still a tyvar. In that case the column
type has no type-constructor head, but the constructors already present in
the pattern matrix may sitll identify the full ADT signature."
  (declare (type tc:pattern-list patterns)
           (type tc:environment env)
           (values (or null tc:type-entry)))
  (loop :for pattern :in patterns
        :when (tc:pattern-constructor-p pattern)
          :do (let ((entry (type-entry-for-column-type
                            (pattern-bare-type pattern)
                            env)))
                (when (and entry
                           (member (tc:pattern-constructor-name pattern)
                                   (tc:type-entry-constructors entry)
                                   :test #'eq))
                  (return entry)))))

(defun possible-constructor-patterns (column-type env seen-patterns)
  "Return synthetic constructor patterns for every constructor that can produce COLUMN-TYPE.

If COLUMN-TYPE has no concrete type-constructor head, use SEEN-PATTERNS as a fallback to
recover the constructor signature from the matrix itself."
  (declare (type tc:ty column-type)
           (type tc:environment env)
           (type tc:pattern-list seen-patterns)
           (values tc:pattern-list))
  (let ((entry (or (type-entry-for-column-type column-type env)
                   (and (tc:tyvar-p column-type)
                        (type-entry-for-seen-constructor-patterns
                         seen-patterns
                         env)))))
    (if entry
        (loop :for constructor-name :in (tc:type-entry-constructors entry)
              :for pattern := (possible-constructor-pattern constructor-name
                                                            column-type
                                                            env)
              :when pattern
                :collect pattern)
        nil)))

;;; Field obligations
;;; -----------------
;;;
;;;   When a wildcard column is decided by a constructor C that no row
;;;   inspects, the search drops the column without adding columns for
;;;   C's fields, which could be recursive. The value it describes still
;;;   needs fields of those types, and they can share type variables with
;;;   later columns. With Permit :i inhabited only at Integer, the fields
;;;   of Metered :: Permit :i -> Door :i exist only when :i is Integer,
;;;   which excludes a later Lane :i value whose constructor fixes :i to
;;;   String. This holds whether Door is a GADT or an ordinary type.
;;;
;;;   Such field types are carried down the search as OBLIGATIONS. They
;;;   are never split, but every substitution applied to the columns is
;;;   applied to them as well, and every proof that the columns have no
;;;   value includes them. A missing case or a useful clause therefore
;;;   is found only for a tail that the chosen constructor's fields admit.
;;;
;;;   Obligations only remove values through EMPTY-INDEXED-COLUMN-P,
;;;   which refutes nothing it cannot prove: cycles, opaque types and an
;;;   exhausted budget leave them inhabited. Each such proof is counted
;;;   in *OBLIGATION-REFUTATIONS*, which tells SEARCH-COLUMN-CASES that
;;;   one constructor's failure no longer decides its siblings.

(defun columns-empty-p (column-types obligations env)
  "Can we prove that no values inhabit COLUMN-TYPES together with OBLIGATIONS?"
  (declare (type tc:ty-list column-types obligations)
           (type tc:environment env)
           (values boolean))
  (let ((empty (empty-indexed-column-p (append column-types obligations) env)))
    (when (and empty obligations)
      (incf *obligation-refutations*))
    empty))

(defun refine-column-types (column-types pattern obligations)
  "Unify the type of PATTERN with the first of COLUMN-TYPES.

Returns whether they unify, and then, refined by the unifier, the remaining
column types, the types of PATTERN's fields (none for a literal) and
OBLIGATIONS. Specializing the first column by PATTERN replaces it with the
fields. Choosing a constructor for a wildcard column drops it, keeping the
fields only as obligations, since the wildcard does not inspect them; see
\"Field obligations\". Refining the other columns and the obligations
preserves GADT equalities between nested fields."
  (declare (type tc:ty-list column-types obligations)
           (type (or tc:pattern-literal tc:pattern-constructor) pattern)
           (values boolean tc:ty-list tc:ty-list tc:ty-list))
  (multiple-value-bind (subs possible-p)
      (tc:try-unify nil (pattern-bare-type pattern) (first column-types) :all)
    (if possible-p
        (values t
                (tc:apply-substitution subs (rest column-types))
                (and (tc:pattern-constructor-p pattern)
                     (tc:apply-substitution
                      subs
                      (mapcar #'pattern-bare-type (tc:pattern-constructor-patterns pattern))))
                (tc:apply-substitution subs obligations))
        (values nil nil nil nil))))

(defun empty-indexed-column-p (columns env)
  "Can we prove that no list of values inhabits COLUMNS?

The columns are refuted together, not one at a time: they can share type
variables, so the constructor that inhabits one column may fix an index that
leaves no constructor for another."
  ;; Refute only what we can prove. A cycle, an opaque type or exhaustion
  ;; of the search budget is not evidence that a value cannot exist.
  ;;
  ;; An ordinary type with several constructors cannot fix an index by
  ;; itself, but it is empty when each of its alternatives has a field that
  ;; is. Expanding such sums can spend the whole budget on alternatives that
  ;; are irrelevant to a contradiction elsewhere, so they get a second search
  ;; with its own budget, run only when the first one, which leaves sums
  ;; unexpanded, fails to refute COLUMNS and skipped some sum.
  (let ((fuel 0)
        (expand-sums nil)
        (sums-skipped nil)
        (out-of-fuel (list 'out-of-fuel)))
    (labels ((expandable-entry (type indexed-only)
               (let* ((entry (type-entry-for-column-type type env))
                      (ctors (and entry (tc:type-entry-constructors entry))))
                 (and ctors
                      (or (not indexed-only)
                          (tc:type-entry-gadt-p entry)
                          (null (rest ctors)))
                      entry)))
             (inhabited-p (items)
               ;; Each item is (TYPE . SEEN), where SEEN lists the type entries
               ;; expanded on the path to TYPE. An item whose type does not yet
               ;; have a known head may get one from a sibling's constructor.
               (let ((item (or (find-if (lambda (item) (expandable-entry (car item) t)) items)
                               (let ((sum (find-if (lambda (item) (expandable-entry (car item) nil))
                                                   items)))
                                 (when sum
                                   (setf sums-skipped t))
                                 (and expand-sums sum)))))
                 (if (null item)
                     t
                     (let* ((type (car item))
                            (seen (cdr item))
                            (entry (expandable-entry type nil))
                            (others (remove item items :count 1 :test #'eq))
                            (shared-p (intersection (tc:type-variables type)
                                                    (tc:type-variables (mapcar #'car others))
                                                    :test #'tc:ty=)))
                       (cond
                         ((member entry seen)
                          (inhabited-p others))
                         ;; A nullary constructor of an ordinary type fits
                         ;; every instance of it without constraining others.
                         ((and (not (tc:type-entry-gadt-p entry))
                               (some (lambda (name)
                                       (let ((ctor (tc:lookup-constructor env name :no-error t)))
                                         (and ctor (zerop (tc:constructor-entry-arity ctor)))))
                                     (tc:type-entry-constructors entry)))
                          (inhabited-p others))
                         (shared-p
                          (constructed-p type entry seen others))
                         ;; Independent items can be refuted in either order.
                         ;; When sums are expanded, refute the siblings first,
                         ;; so that a sum inside this item cannot use up the
                         ;; budget before a contradiction among them is found.
                         (expand-sums
                          (and (inhabited-p others)
                               (constructed-p type entry seen nil)))
                         (t
                          (and (constructed-p type entry seen nil)
                               (inhabited-p others))))))))
             (constructed-p (type entry seen others)
               ;; Does a constructor of ENTRY build a TYPE whose fields are
               ;; inhabited together with OTHERS?
               (some (lambda (name)
                       (let ((scheme (tc:lookup-value-type env name :no-error t)))
                         (unless (and scheme (plusp fuel))
                           (throw out-of-fuel nil))
                         (decf fuel)
                         (let ((ctor-ty (tc:qualified-ty-type (tc:fresh-inst scheme))))
                           (multiple-value-bind (subs possible-p)
                               (tc:try-unify nil (tc:function-return-type ctor-ty) type :all)
                             (and possible-p
                                  (inhabited-p
                                   (append (loop :for field :in (tc:function-type-arguments
                                                                 (tc:apply-substitution subs ctor-ty))
                                                 :collect (cons field (cons entry seen)))
                                           (loop :for (other-type . other-seen) :in others
                                                 :collect (cons (tc:apply-substitution subs other-type)
                                                                other-seen)))))))))
                     (tc:type-entry-constructors entry))))
      (flet ((refuted-p (sums)
               (setf fuel 64
                     expand-sums sums)
               (catch out-of-fuel
                 (not (inhabited-p (mapcar #'list columns))))))
        (or (refuted-p nil)
            (and sums-skipped (refuted-p t)))))))

(defun search-column-cases (matrix column-types obligations env default specialize)
  "Search the cases of a wildcard column, returning the first non-NIL result.
Both usefulness and witness generation must propagate the same index equalities.

DEFAULT is called with a missing constructor (or NIL when the column's
constructors are unknown), the remaining column types and the obligations
for that choice. SPECIALIZE is called with a seen constructor, the
specialized column types and the obligations."
  (let* ((seen (loop :for row :in matrix
                     :when (tc:pattern-constructor-p (first row))
                       :collect (first row)))
         (candidates (possible-constructor-patterns (first column-types) env seen))
         ;; A constructor is a case only if its fields, the remaining columns
         ;; and the obligations, as refined by its result type, have a value
         ;; together. A field and a later column can each be inhabited while
         ;; sharing an index that no value of both can satisfy. Each case is
         ;; (CTOR TAIL-TYPES FIELDS OBLIGATIONS); see REFINE-COLUMN-TYPES.
         (cases (loop :for ctor :in candidates
                      :for (possible-p types fields outer)
                        := (multiple-value-list (refine-column-types column-types ctor obligations))
                      :when (and possible-p
                                 (not (columns-empty-p (append fields types) outer env)))
                        :collect (list ctor types fields outer)))
         (missing (remove-if (lambda (entry)
                               (find (tc:pattern-constructor-name (first entry)) seen
                                     :key #'tc:pattern-constructor-name))
                             cases))
         (tail (rest column-types))
         (refutations *obligation-refutations*))
    (unless cases
      (return-from search-column-cases
        (unless (or candidates (empty-indexed-column-p (list (first column-types)) env))
          (funcall default nil tail obligations))))
    ;; Without shared indices, every missing constructor leaves the same
    ;; remaining columns and default rows, so the first one decides them all
    ;; and any hole under a seen constructor is also one under it. A missing
    ;; constructor whose fields share type variables with the remaining
    ;; columns or obligations can still fail because its fields admit only
    ;; some remaining values. That shows up as a refutation using
    ;; obligations, and then the other constructors must be tried as well.
    ;; Constructors whose fields share nothing cannot change the answer, so
    ;; a refutation deeper in the search does not make them retry.
    (let ((correlated (and missing
                           (or tail obligations)
                           (tc:constructor-entry-gadt-p
                            (tc:lookup-constructor env (tc:pattern-constructor-name (first (first cases)))))
                           (intersection (tc:type-variables (first column-types))
                                         (tc:type-variables (append tail obligations))
                                         :test #'tc:ty=)))
          (fields-shared (loop :for (nil types fields outer) :in missing
                                 :thereis (and fields
                                               (intersection (tc:type-variables fields)
                                                             (tc:type-variables (append types outer))
                                                             :test #'tc:ty=)))))
      (flet ((refuted-p ()
               (and fields-shared (> *obligation-refutations* refutations))))
        ;; Missing constructors use only default rows; do not expand their fields
        ;; (which could be recursive), but keep them as obligations.
        (or (loop :for (ctor types fields outer) :in missing
                  :for first := t :then nil
                  :while (or first correlated (refuted-p))
                  :thereis (funcall default ctor types (append fields outer)))
            ;; Defaults can cover every missing constructor but still leave a hole
            ;; under a seen one if the other columns have different refinements.
            (and (or (null missing) correlated (refuted-p))
                 (loop :for (ctor types fields outer) :in cases
                       :thereis (and (not (member ctor missing :key #'first))
                                     (funcall specialize ctor (append fields types) outer)))))))))

(defun open-column-type-p (type)
  "Is the head of TYPE a type variable, leaving its constructors unknown?"
  (declare (type tc:ty type))
  (not (tc:tycon-p (first (tc:flatten-type type)))))

(defun split-column-index (pattern-matrix clause column-types)
  "Choose the column to split next.

Any column can be split without changing which row a value matches first, as
rows keep their order. With GADTs the choice still matters: a constructor
pattern in an open column (one whose type is a type variable) is justified by
an equation from a constructor in another column. That column's constructors
are known from its type, and splitting it substitutes its index into the open
column. So split refutable columns with a known head first. Inferring an open
column's constructors from its own patterns would omit every type that the
equation's alternatives allow."
  (declare (type pattern-matrix pattern-matrix)
           (type tc:pattern-list clause)
           (type tc:ty-list column-types)
           (values fixnum))
  (let ((rows (if clause (cons clause pattern-matrix) pattern-matrix))
        (open-index nil))
    (loop :for type :in column-types
          :for k :from 0
          :when (some (lambda (row)
                        (let ((pattern (nth k row)))
                          (or (tc:pattern-literal-p pattern)
                              (tc:pattern-constructor-p pattern))))
                      rows)
            :do (if (open-column-type-p type)
                    (unless open-index (setf open-index k))
                    (return k))
          :finally (return (or open-index 0)))))

(defun move-to-front (list k)
  (if (zerop k)
      list
      (cons (nth k list) (append (subseq list 0 k) (nthcdr (1+ k) list)))))

(defun move-from-front (list k)
  "Inverse of MOVE-TO-FRONT."
  (if (zerop k)
      list
      (append (subseq list 1 (1+ k)) (list (first list)) (nthcdr (1+ k) list))))

(defun wildcard-pattern-p (pattern)
  (or (tc:pattern-wildcard-p pattern)
      (tc:pattern-var-p pattern)))

(defun useful-pattern-clause-p (pattern-matrix clause column-types env &optional obligations)
  "Is CLAUSE useful with respect to PATTERN-MATRIX?

PATTERN-MATRIX is a list of lists representing a pattern matrix in row-major format.
CLAUSE is a list representing a row-vector of patterns. COLUMN-TYPES is the current
type of each matrix column. OBLIGATIONS are types that must be inhabited together
with the columns; see \"Field obligations\"."
  (declare (type pattern-matrix pattern-matrix)
           (type tc:pattern-list clause)
           (type tc:ty-list column-types obligations)
           (type tc:environment env)
           (values boolean &optional))

  ;; NOTE: It is assumed that all rows of PATTERN-MATRIX have the same number of columns,
  ;; and that COLUMN-TYPES has the same length as CLAUSE.
  (let* ((k (split-column-index pattern-matrix clause column-types))
         (pattern-matrix (mapcar (lambda (row) (move-to-front row k)) pattern-matrix))
         (clause (move-to-front clause k))
         (column-types (move-to-front column-types k)))
    (cond
      ;;
      ;; Check our base cases
      ;;

      ;; If there are no rows then a clause of wildcards is useful when a
      ;; value exists. A clause that still has constructor or literal
      ;; patterns is decomposed below: those patterns may admit no value
      ;; of an inhabited column type, such as (Metered _) at Door String.
      ((and (zerop (length pattern-matrix))
            (every #'wildcard-pattern-p clause))
       (not (columns-empty-p column-types obligations env)))
      ;; If both PATTERN-MATRIX and CLAUSE have no columns then the pattern is not useful.
      ((and pattern-matrix (zerop (length (first pattern-matrix))))
       nil)

      ;;
      ;; Now, we check based on the first member of CLAUSE.
      ;;

      ;; Sub-case 1a: The first member of CLAUSE is a literal that may equal
      ;; a value of a constructor pattern in the matrix. Which constructor
      ;; values equal the literal is decided by a user Eq instance, so
      ;; approximate the literal by every value of its type.
      ((and (tc:pattern-literal-p (first clause))
            (literal-constructor-overlap-p pattern-matrix (first clause)))
       (multiple-value-bind (subs possible-p)
           (tc:try-unify nil (pattern-bare-type (first clause)) (first column-types) :all)
         (when possible-p
           (useful-pattern-clause-p (literal-as-wildcard-matrix pattern-matrix (first clause))
                                    (cons (tc:make-pattern-wildcard
                                           :type (tc:pattern-type (first clause)))
                                          (rest clause))
                                    (tc:apply-substitution subs column-types)
                                    env
                                    (tc:apply-substitution subs obligations)))))

      ;; Sub-case 1b: The first member of CLAUSE is a constructor (or literal).
      ((or (tc:pattern-literal-p (first clause))
           (tc:pattern-constructor-p (first clause)))
       (multiple-value-bind (possible-p tail-types fields specialized-obligations)
           (refine-column-types column-types (first clause) obligations)
         (when possible-p
           (let ((specialized-matrix (specialize-matrix pattern-matrix (first clause)))
                 (specialized-clause (specialize-matrix (list clause) (first clause))))
             (and specialized-clause
                  (useful-pattern-clause-p specialized-matrix
                                           (first specialized-clause)
                                           (append fields tail-types)
                                           env
                                           specialized-obligations))))))

      ;; Sub-case 2: The first member of CLAUSE is a wildcard (or variable)
      ((wildcard-pattern-p (first clause))
       (search-column-cases
        pattern-matrix column-types obligations env
        (lambda (ctor types obligations)
          (declare (ignore ctor))
          (useful-pattern-clause-p (default-matrix pattern-matrix) (rest clause) types env obligations))
        (lambda (ctor types obligations)
          (useful-pattern-clause-p (specialize-matrix pattern-matrix ctor)
                                   (first (specialize-matrix (list clause) ctor))
                                   types env obligations))))

      (t
       (util:coalton-bug "Not reachable.")))))

;;; Literal patterns
;;; ----------------
;;;
;;;   A literal pattern L matches the values v with (== v L), so the
;;;   set of values it matches is decided by an Eq instance. Analysis
;;;   never runs that instance. For built-in types without
;;;   constructors this only concerns other literals; there, as before,
;;;   two occurrences of the same literal are the same test, and
;;;   distinct literals are treated as matching different values. The
;;;   latter can only make a later row seem useful or a match seem
;;;   non-exhaustive.
;;;
;;;   A column can also hold both literals and constructor patterns:
;;;
;;;   1. Rows typed under different GADT evidence, such as "" in an arm
;;;      that fixes the column to String and (Unit) in an arm that
;;;      fixes it to Unit. No value matches both, which shows up as
;;;      types that do not unify.
;;;
;;;   2. A Num literal of an ordinary data type, such as 5 and
;;;      (Amount x) for a type Amount with constructors. Whether a
;;;      constructor value equals the literal is unknown.
;;;
;;;   In both cases a literal row is never counted as covering a
;;;   constructor case, and vice versa; in case 1 this is exact. In
;;;   case 2 the values matching a literal clause are approximated by
;;;   all values of its type (see USEFUL-PATTERN-CLAUSE-P). A literal
;;;   case is reported useless only if the rows before it cover that
;;;   whole type, counting earlier occurrences of the same literal.
;;;   Any missing case reported for such a column may itself equal one
;;;   of the literals.

(defun patterns-may-share-value-p (pattern elem)
  "Can a single value be described by both PATTERN and ELEM?

The patterns may come from different arms, typed under different GADT
evidence. Types that cannot unify have no value in common."
  (declare (type tc:pattern pattern elem)
           (values boolean))
  (nth-value 1 (tc:try-unify nil (pattern-bare-type pattern) (pattern-bare-type elem) :all)))

(defun specialize-matrix (pattern-matrix pattern)
  "Specialize the given PATTERN-MATRIX to the constructor given in PATTERN."
  (declare (type pattern-matrix pattern-matrix)
           (type (or tc:pattern-literal tc:pattern-constructor) pattern)
           (values pattern-matrix))
  (loop :for row :in pattern-matrix
        ;; Only specialize on the first component of each row.
        :for elem := (first row)
        :append (etypecase elem
                  ;; A wildcard (or variable) matches every value, so emit
                  ;; wildcards for each field of the constructor (or none for
                  ;; a literal).
                  ((or tc:pattern-wildcard tc:pattern-var)
                   (etypecase pattern
                     (tc:pattern-literal
                      (list (rest row)))
                     (tc:pattern-constructor
                      (list (append (mapcar (lambda (pattern)
                                              (tc:make-pattern-wildcard
                                               :type (tc:pattern-type pattern)))
                                            (tc:pattern-constructor-patterns pattern))
                                    (rest row))))))
                  ;; The same literal of a compatible type is the same test.
                  ;; Anything else is not known to match a value matching
                  ;; PATTERN; see "Literal patterns" above.
                  (tc:pattern-literal
                   (and (tc:pattern-literal-p pattern)
                        (util:literal-equal (tc:pattern-literal-value pattern)
                                            (tc:pattern-literal-value elem))
                        (patterns-may-share-value-p pattern elem)
                        (list (rest row))))
                  ;; The same constructor expands its fields. A different
                  ;; constructor, or a constructor against a literal, emits
                  ;; nothing.
                  (tc:pattern-constructor
                   (and (tc:pattern-constructor-p pattern)
                        (eq (tc:pattern-constructor-name pattern)
                            (tc:pattern-constructor-name elem))
                        (list (append (tc:pattern-constructor-patterns elem)
                                      (rest row))))))))

(defun literal-constructor-overlap-p (pattern-matrix literal)
  "Does a row of PATTERN-MATRIX begin with a constructor pattern that may
match a value equal to LITERAL?"
  (declare (type pattern-matrix pattern-matrix)
           (type tc:pattern-literal literal)
           (values boolean))
  (loop :for row :in pattern-matrix
        :for elem := (first row)
          :thereis (and (tc:pattern-constructor-p elem)
                        (patterns-may-share-value-p literal elem))))

(defun literal-as-wildcard-matrix (pattern-matrix literal)
  "Rewrite the first column of PATTERN-MATRIX for a clause beginning with
LITERAL, where the clause is approximated by a wildcard over the literal's
type.

Earlier occurrences of the same literal match every value that LITERAL
matches, so they become wildcards. Other literals and patterns of
incompatible types are dropped. Constructor patterns and wildcards stay."
  (declare (type pattern-matrix pattern-matrix)
           (type tc:pattern-literal literal)
           (values pattern-matrix))
  (loop :for row :in pattern-matrix
        :for elem := (first row)
        :append (etypecase elem
                  ((or tc:pattern-wildcard tc:pattern-var)
                   (list row))
                  (tc:pattern-literal
                   (and (util:literal-equal (tc:pattern-literal-value literal)
                                            (tc:pattern-literal-value elem))
                        (patterns-may-share-value-p literal elem)
                        (list (cons (tc:make-pattern-wildcard :type (tc:pattern-type elem))
                                    (rest row)))))
                  (tc:pattern-constructor
                   (and (patterns-may-share-value-p literal elem)
                        (list row))))))

(defun default-matrix (pattern-matrix)
  "Default the given PATTERN-MATRIX."
  (declare (type pattern-matrix pattern-matrix)
           (values pattern-matrix))
  (loop :for row :in pattern-matrix
        ;; Only consider the first component of each row.
        :for elem := (first row)
        :append (cond
                  ;; If ELEM is a constructor (or literal) then don't emit a row.
                  ((or (tc:pattern-literal-p elem)
                       (tc:pattern-constructor-p elem))
                   nil)
                  ;; If ELEM is a wildcard (or variable) then remove ELEM.
                  ((or (tc:pattern-wildcard-p elem)
                       (tc:pattern-var-p elem))
                   (list (rest row)))
                  (t
                   (util:coalton-bug "Not reachable.")))))

(defun collapse-binding-patterns (pat)
  "For the purposes of exhaustiveness checking, a binding pattern like

        (@ VAR PAT) 

   can be collapsed to PAT."
  (declare (type tc:pattern pat) (values tc:pattern))
  (etypecase pat
    ((or tc:pattern-var tc:pattern-wildcard tc:pattern-literal)
     pat)
    (tc:pattern-constructor
     (tc:make-pattern-constructor
      :type (tc:pattern-type pat)
      :location (tc:pattern-location pat)
      :name (tc:pattern-constructor-name pat)
      :patterns (mapcar #'collapse-binding-patterns (tc:pattern-constructor-patterns pat))))
    (tc:pattern-binding
     (collapse-binding-patterns
      (tc:pattern-binding-pattern pat)))))

(defun find-non-matching-value (pattern-matrix column-types env)
  "Finds an example of a non-matching value for PATTERN-MATRIX or, if
   PATTERN-MATRIX is exhaustive returns T.

COLUMN-TYPES is the current type of each pattern-matrix column."
  (declare (type pattern-matrix pattern-matrix)
           (type tc:ty-list column-types)
           (type tc:environment env))
  (let ((*obligation-refutations* 0))
    (search-non-matching-value pattern-matrix column-types nil env)))

(defun search-non-matching-value (pattern-matrix column-types obligations env)
  "Like FIND-NON-MATCHING-VALUE. The value returned must also leave
OBLIGATIONS inhabited; see \"Field obligations\". A missing constructor in
the result stands for a value whose fields satisfy the obligations
together with the rest of the result."
  (declare (type pattern-matrix pattern-matrix)
           (type tc:ty-list column-types obligations)
           (type tc:environment env))
  (cond
    ;; An empty pattern matrix misses one wildcard value for each current column.
    ((zerop (length pattern-matrix))
     (if (columns-empty-p column-types obligations env)
         t
         (mapcar #'make-wildcard-for-type column-types)))

    ;; Zero columns with a non-empty matrix means the matrix is exhaustive.
    ((and (null column-types)
          (zerop (length (first pattern-matrix))))
     t)

    ((null column-types)
     (util:coalton-bug "Pattern matrix has columns but no column types."))

    (t
     (let ((k (split-column-index pattern-matrix nil column-types)))
       (if (plusp k)
           (let ((value (search-non-matching-value
                         (mapcar (lambda (row) (move-to-front row k)) pattern-matrix)
                         (move-to-front column-types k)
                         obligations
                         env)))
             (if (eq value t) t (move-from-front value k)))
           (or (search-column-cases
                pattern-matrix column-types obligations env
                (lambda (ctor types obligations)
                  (let ((tail (search-non-matching-value (default-matrix pattern-matrix) types obligations env)))
                    (unless (eq tail t)
                      (cons (or ctor (make-wildcard-for-type (first column-types))) tail))))
                (lambda (ctor types obligations)
                  (let ((value (search-non-matching-value (specialize-matrix pattern-matrix ctor)
                                                          types obligations env))
                        (arity (length (tc:pattern-constructor-patterns ctor))))
                    (unless (eq value t)
                      (cons (tc:make-pattern-constructor :type (tc:pattern-type ctor)
                                                         :name (tc:pattern-constructor-name ctor)
                                                         :patterns (subseq value 0 arity))
                            (nthcdr arity value))))))
               t))))))
