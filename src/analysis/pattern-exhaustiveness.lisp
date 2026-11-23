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
  (not
   (useful-pattern-clause-p
    (mapcar #'list patterns)
    (list (tc:make-pattern-wildcard
           :type (tc:qualify nil (tc:make-variable))))
    (list column-type)
    env)))

(defun useful-pattern-p (patterns pattern env column-type)
  "Is PATTERN useful within list PATTERNS for COLUMN-TYPE? PATTERN must EQ one element
of PATTERNS."
  (declare (type tc:pattern-list patterns)
           (type tc:pattern pattern)
           (type tc:environment env)
           (type tc:ty column-type)
           (values boolean))
  (useful-pattern-clause-p
   (loop :for p :in patterns
         :while (not (eq p pattern))
         :collect (list p))
   (list pattern)
   (list column-type)
   env))

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
      (let* ((fresh-qual-ty (tc:fresh-inst scheme))
             (constructor-ty (tc:qualified-ty-type fresh-qual-ty))
             (return-ty (tc:function-return-type constructor-ty)))
        (handler-case
            (let* (;; Unify, don't match. COLUMN-TYPE may itself contain
                   ;; type variables, and a GADT constructor may unify them.
                   (subs (tc:unify-pattern-result nil return-ty column-type :all))
                   (constructor-ty (tc:apply-substitution subs constructor-ty))
                   (return-ty (tc:apply-substitution subs return-ty))
                   (argument-tys (tc:function-type-arguments constructor-ty)))
              (tc:make-pattern-constructor :type (tc:qualify nil return-ty)
                                           :name constructor-name
                                           :patterns (loop :for arg-ty :in argument-tys
                                                           :collect (make-wildcard-for-type arg-ty))))
          (tc:coalton-internal-type-error ()
            nil))))))

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

(defun first-column-patterns (pattern-matrix)
  (declare (type pattern-matrix pattern-matrix)
           (values tc:pattern-list))
  (loop :for row :in pattern-matrix
        :for elem := (first row)
        :when (or (tc:pattern-literal-p elem)
                  (tc:pattern-constructor-p elem))
          :collect elem))

(defun constructor-pattern-names (patterns)
  (declare (type tc:pattern-list patterns)
           (values util:symbol-list))
  (loop :for pattern :in patterns
        :when (tc:pattern-constructor-p pattern)
          :collect (tc:pattern-constructor-name pattern)))

(defun specialize-column-types (column-types pattern)
  "Update COLUMN-TYPES after specializing the first column by PATTERN.

For a constructor C with fields T1 ... Tn, the first column is replaced with
T1 ... Tn. The unification substitution is also applied to the remaining
columns, which preserves GADT equalities between nested fields."
  (declare (type tc:ty-list column-types)
           (type (or tc:pattern-literal tc:pattern-constructor) pattern)
           (values tc:ty-list boolean))
  (let ((current-column-type (first column-types)))
    (handler-case
        (let* ((subs (tc:unify-pattern-result nil (pattern-bare-type pattern) current-column-type :all))
               (new-column-types (etypecase pattern
                                   (tc:pattern-literal (rest column-types))
                                   (tc:pattern-constructor
                                    (append (mapcar #'pattern-bare-type
                                                    (tc:pattern-constructor-patterns pattern))
                                            (rest column-types))))))
          (values (tc:apply-substitution subs new-column-types) t))
      (tc:coalton-internal-type-error ()
        (values nil nil)))))

(defun default-column-types-for-constructor (column-types constructor-pattern)
  "Drop the first column after choosing CONSTRUCTOR-PATTERN for a wildcard
column. Unlike SPECIALIZE-COLUMN-TYPES, this does not add constructor argument
types, because the wildcard does not inspect constructor fields.

It only applies the GADT refinement induced by the constructor return type to
the remaining columns."
  (declare (type tc:ty-list column-types)
           (type tc:pattern-constructor constructor-pattern)
           (values tc:ty-list boolean))
  (let ((current-column-type (first column-types)))
    (handler-case
        (let ((subs (tc:unify-pattern-result nil (pattern-bare-type constructor-pattern)
                                             current-column-type :all)))
          (values (tc:apply-substitution subs (rest column-types)) t))
      (tc:coalton-internal-type-error ()
        (values nil nil)))))

(defun missing-constructor-patterns (seen-patterns possible-ctors)
  "Return possible constructors for COLUMN-TYPE that are not named in SEEN-PATTERNS."
  (declare (type tc:pattern-list seen-patterns)
           (type tc:pattern-list possible-ctors)
           (values tc:pattern-list))
  (let ((seen-names (constructor-pattern-names seen-patterns)))
    (remove-if
     (lambda (constructor-pattern)
       (member (tc:pattern-constructor-name constructor-pattern)
               seen-names
               :test #'eq))
     possible-ctors)))

(defun empty-indexed-column-p (columns env)
  ;; Refute only what we can prove. A cycle, an opaque type or exhaustion
  ;; of the search budget is not evidence that a value cannot exist.
  (let ((fuel 32))
    (labels ((empty-p (type seen)
               (let* ((entry (type-entry-for-column-type type env))
                      (ctors (and entry (tc:type-entry-constructors entry))))
                 (and ctors
                      (or (tc:type-entry-gadt-p entry) (null (rest ctors)))
                      (not (member entry seen))
                      (every (lambda (name)
                               (when (and (plusp fuel) (tc:lookup-value-type env name :no-error t))
                                 (decf fuel)
                                 (let ((pat (possible-constructor-pattern name type env)))
                                   (or (null pat)
                                       (some (lambda (field)
                                               (empty-p (pattern-bare-type field) (cons entry seen)))
                                             (tc:pattern-constructor-patterns pat))))))
                             ctors)))))
      (some (lambda (type) (empty-p type nil)) columns))))

(defun search-column-cases (matrix column-types env default specialize)
  "Search the cases of a wildcard column, returning the first non-NIL result.
Both usefulness and witness generation must propagate the same index equalities."
  (let* ((seen (first-column-patterns matrix))
         (candidates (possible-constructor-patterns (first column-types) env seen))
         (possible (remove-if (lambda (ctor)
                                (empty-indexed-column-p (mapcar #'pattern-bare-type
                                                                (tc:pattern-constructor-patterns ctor))
                                                        env))
                              candidates))
         (missing (missing-constructor-patterns seen possible))
         (tail (rest column-types)))
    (unless possible
      (return-from search-column-cases
        (unless (or candidates (empty-indexed-column-p (list (first column-types)) env))
          (funcall default nil tail))))
    (let ((correlated (and missing tail
                           (tc:constructor-entry-gadt-p
                            (tc:lookup-constructor env (tc:pattern-constructor-name (first possible))))
                           (intersection (tc:type-variables (first column-types))
                                         (tc:type-variables tail) :test #'tc:ty=))))
      ;; Missing constructors use only default rows; do not expand their fields
      ;; (which could be recursive). Without shared indices, one suffices.
      (or (loop :for ctor :in (if correlated missing (and missing (list (first missing))))
                :thereis (multiple-value-bind (types possible-p)
                             (default-column-types-for-constructor column-types ctor)
                           (and possible-p (funcall default ctor types))))
          ;; Defaults can cover every missing constructor but still leave a hole
          ;; under a seen one if the other columns have different refinements.
          (and (or (null missing) correlated)
               (loop :for ctor :in possible
                     :thereis (and (not (member ctor missing))
                                   (multiple-value-bind (types possible-p)
                                       (specialize-column-types column-types ctor)
                                     (and possible-p (funcall specialize ctor types))))))))))

(defun useful-pattern-clause-p (pattern-matrix clause column-types env)
  "Is CLAUSE useful with respect to PATTERN-MATRIX?

PATTERN-MATRIX is a list of lists representing a pattern matrix in row-major format.
CLAUSE is a list representing a row-vector of patterns. COLUMN-TYPES is the current
type of each matrix column."
  (declare (type pattern-matrix pattern-matrix)
           (type tc:pattern-list clause)
           (type tc:ty-list column-types)
           (type tc:environment env)
           (values boolean &optional))

  ;; NOTE: It is assumed that all rows of PATTERN-MATRIX have the same number of columns,
  ;; and that COLUMN-TYPES has the same length as CLAUSE.
  (cond
    ;;
    ;; Check our base cases
    ;;

    ;; If there are no rows then the pattern is useful.
    ((zerop (length pattern-matrix))
     (not (empty-indexed-column-p column-types env)))
    ;; If both PATTERN-MATRIX and CLAUSE have no columns then the pattern is not useful.
    ((zerop (length (first pattern-matrix)))
     nil)

    ;;
    ;; Now, we check based on the first member of CLAUSE.
    ;;

    ;; Sub-case 1: The first member of CLAUSE is a constructor (or literal).
    ((or (tc:pattern-literal-p (first clause))
         (tc:pattern-constructor-p (first clause)))
     (multiple-value-bind (specialized-column-types possible-p)
         (specialize-column-types column-types (first clause))
       (when possible-p
         (let ((specialized-matrix (specialize-matrix pattern-matrix (first clause)))
               (specialized-clause (specialize-matrix (list clause) (first clause))))
           (and specialized-clause
                (useful-pattern-clause-p specialized-matrix
                                         (first specialized-clause)
                                         specialized-column-types
                                         env))))))

    ;; Sub-case 2: The first member of CLAUSE is a wildcard (or variable)
    ((or (tc:pattern-wildcard-p (first clause))
         (tc:pattern-var-p (first clause)))

     (search-column-cases
      pattern-matrix column-types env
      (lambda (ctor types)
        (declare (ignore ctor))
        (useful-pattern-clause-p (default-matrix pattern-matrix) (rest clause) types env))
      (lambda (ctor types)
        (useful-pattern-clause-p (specialize-matrix pattern-matrix ctor)
                                 (first (specialize-matrix (list clause) ctor))
                                 types env))))

    (t
     (util:coalton-bug "Not reachable."))))

(defun specialize-matrix (pattern-matrix pattern)
  "Specialize the given PATTERN-MATRIX to the constructor given in PATTERN."
  (declare (type pattern-matrix pattern-matrix)
           (type (or tc:pattern-literal tc:pattern-constructor) pattern)
           (values pattern-matrix))
  (loop :for row :in pattern-matrix
        ;; Only specialize on the first component of each row.
        :for elem := (first row)
        ;; NOTE: ELEM cannot be a literal when PATTERN is a
        ;;       constructor nor the other way around.
        :append (cond
                  ;; If ELEM is the same literal then remove this pattern.
                  ((and (tc:pattern-literal-p elem)
                        (util:literal-equal
                         (tc:pattern-literal-value pattern)
                         (tc:pattern-literal-value elem)))
                   (list (rest row)))
                  ;; If ELEM is not the same literal then emit nothing.
                  ((tc:pattern-literal-p elem)
                   nil)

                  ;; If ELEM is the same constructor then expand the inner patterns.
                  ((and (tc:pattern-constructor-p elem)
                        (eq (tc:pattern-constructor-name pattern)
                            (tc:pattern-constructor-name elem)))
                   (list (append (tc:pattern-constructor-patterns elem)
                                 (rest row))))
                  ;; If ELEM is not the same constructor then emit nothing.
                  ((tc:pattern-constructor-p elem)
                   nil)

                  ;; If ELEM is a wildcard (or variable) then emit
                  ;; wildcards for each pattern in the constructor (or
                  ;; literal).
                  ((or (tc:pattern-wildcard-p elem)
                       (tc:pattern-var-p elem))
                   (etypecase pattern
                     (tc:pattern-literal
                      (list (rest row)))
                     (tc:pattern-constructor
                      (list (append (mapcar (lambda (pattern)
                                              (tc:make-pattern-wildcard
                                               :type (tc:pattern-type pattern)))
                                            (tc:pattern-constructor-patterns pattern))
                                    (rest row))))))
                  (t
                   (util:coalton-bug "Not reachable.")))))

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
  (cond
    ;; An empty pattern matrix misses one wildcard value for each current column.
    ((zerop (length pattern-matrix))
     (if (empty-indexed-column-p column-types env)
         t
         (mapcar #'make-wildcard-for-type column-types)))

    ;; Zero columns with a non-empty matrix means the matrix is exhaustive.
    ((and (null column-types)
          (zerop (length (first pattern-matrix))))
     t)

    ((null column-types)
     (util:coalton-bug "Pattern matrix has columns but no column types."))

    (t
     (or (search-column-cases
          pattern-matrix column-types env
          (lambda (ctor types)
            (let ((tail (find-non-matching-value (default-matrix pattern-matrix) types env)))
              (unless (eq tail t)
                (cons (or ctor (make-wildcard-for-type (first column-types))) tail))))
          (lambda (ctor types)
            (let ((value (find-non-matching-value (specialize-matrix pattern-matrix ctor) types env))
                  (arity (length (tc:pattern-constructor-patterns ctor))))
              (unless (eq value t)
                (cons (tc:make-pattern-constructor :type (tc:pattern-type ctor)
                                                   :name (tc:pattern-constructor-name ctor)
                                                   :patterns (subseq value 0 arity))
                      (nthcdr arity value))))))
         t))))
