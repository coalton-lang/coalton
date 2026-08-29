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
                   (subs (tc:unify nil return-ty column-type))
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

(defun first-column-constructors (pattern-matrix)
  (declare (type pattern-matrix pattern-matrix)
           (values tc:pattern-list))
  (loop :for row :in pattern-matrix
        :for elem := (first row)
        :when (tc:pattern-constructor-p elem)
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
        (let* ((subs (tc:unify nil (pattern-bare-type pattern) current-column-type))
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
        (let ((subs (tc:unify nil
                              (pattern-bare-type constructor-pattern)
                              current-column-type)))
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
     t)
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

     (let* ((current-column-type (first column-types))
            (first-column-patterns (first-column-patterns pattern-matrix))
            (possible-constructors (possible-constructor-patterns current-column-type
                                                                  env
                                                                  first-column-patterns)))
       (cond
         ;; If this column has no known algebraic constructor signature, fall back
         ;; to the ordinary default matrix case.
         ((null possible-constructors)
          (useful-pattern-clause-p
           (default-matrix pattern-matrix)
           (rest clause)
           (rest column-types)
           env))

         ;; If the matrix has a complete constructor signature for this colmun,
         ;; specialize over every possible constructor. This is the normal
         ;; Maranget rule.
         ((complete-signature-p first-column-patterns possible-constructors)
          (loop :for constructor-pattern :in possible-constructors
                :thereis
                (multiple-value-bind (specialized-column-types possible-p)
                    (specialize-column-types column-types constructor-pattern)
                  (when possible-p
                    (let ((specialized-clause (specialize-matrix (list clause)
                                                                 constructor-pattern)))
                      (and specialized-clause
                           (useful-pattern-clause-p
                            (specialize-matrix pattern-matrix constructor-pattern)
                            (first specialized-clause)
                            specialized-column-types
                            env)))))))

         ;; If the signature is incomplete, the useful value can be chosen from
         ;; one of the missing constructors. Explicit constructor rows cannot match
         ;; that value, so only the default matrix matters.
         ;;
         ;; For GADTs, different missing constructors may refine the remaining
         ;; columns differently, so try each missing constructor. But do NOT expand
         ;; into the constructor's fields; the wildcard did not inspect them.
         (t
          (let ((defaulted-matrix
                  (default-matrix pattern-matrix))
                (defaulted-clause
                  (rest clause))
                (missing-constructors
                  (missing-constructor-patterns
                   first-column-patterns
                   possible-constructors)))
            (loop :for constructor-pattern :in missing-constructors
                  :thereis
                  (multiple-value-bind (defaulted-column-types possible-p)
                      (default-column-types-for-constructor column-types constructor-pattern)
                    (and possible-p
                         (useful-pattern-clause-p
                          defaulted-matrix
                          defaulted-clause
                          defaulted-column-types
                          env)))))))))

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

(defun complete-signature-p (patterns possible-ctors)
  "Do PATTERNS mention every constructor that can produce COLUMN-TYPE?"
  (declare (type tc:pattern-list patterns)
           (type tc:pattern-list possible-ctors)
           (values boolean))
  (cond
    ;; Zero constructors cannot form a complete signature.
    ((zerop (length patterns))
     nil)

    ;; Literals cannot have complete signatures.
    ;; NOTE: This will change when we allow number literals to take on finite types.
    ((some #'tc:pattern-literal-p patterns)
     nil)

    ;; Otherwise ensure that all constructors are accounted for in PATTERNS.
    (t
     (let ((constructor-names (constructor-pattern-names patterns))
           (possible-constructor-names (mapcar #'tc:pattern-constructor-name
                                               possible-ctors)))
       (and possible-constructor-names
            (null
             (set-difference possible-constructor-names
                             constructor-names
                             :test #'eq)))))))

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

(defun find-unnamed-constructor (patterns possible-ctors)
  "Find and create a pattern constructor for COLUMN-TYPE that is not named in PATTERNS."
  (declare (type tc:pattern-list patterns)
           (type tc:pattern-list possible-ctors)
           (values tc:pattern-constructor))
  (let* ((constructor-names (constructor-pattern-names patterns))
         (unnamed-constructor (find-if-not (lambda (constructor-pattern)
                                             (member (tc:pattern-constructor-name constructor-pattern)
                                                     constructor-names
                                                     :test #'eq))
                                           possible-ctors)))
    (unless unnamed-constructor
      (util:coalton-bug "Not reachable."))

    ;; NOTE: Here we _could_ reasonably return all missing
    ;; constructors, however that would require additional support in
    ;; the error generation. Instead we just select the first one.
    unnamed-constructor))

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
     (loop :for column-type :in column-types
           :collect (make-wildcard-for-type column-type)))

    ;; Zero columns with a non-empty matrix means the matrix is exhaustive.
    ((and (null column-types)
          (zerop (length (first pattern-matrix))))
     t)

    ((null column-types)
     (util:coalton-bug "Pattern matrix has columns but no column types."))

    (t
     (let* ((current-column-type (first column-types))
            (first-column-patterns (first-column-patterns pattern-matrix))
            (first-column-constructors (first-column-constructors pattern-matrix))
            (possible-constructors (possible-constructor-patterns current-column-type
                                                                  env
                                                                  first-column-patterns)))
       (cond
         ;; If the constructors in the PATTERN-MATRIX form a complete signature
         ;; for the current column type, specialize and return the first
         ;; non-maching sub value.
         ((complete-signature-p first-column-patterns possible-constructors)
          (loop :for constructor-pattern :in possible-constructors
                :for constructor-arity := (length (tc:pattern-constructor-patterns constructor-pattern))
                :for val := (multiple-value-bind (specialized-column-types possible-p)
                                (specialize-column-types column-types constructor-pattern)
                              (if possible-p
                                  (find-non-matching-value
                                   (specialize-matrix pattern-matrix constructor-pattern)
                                   specialized-column-types
                                   env)
                                  t))
                :unless (eq val t)
                  :do (return
                        (cons
                         (tc:make-pattern-constructor :type (tc:pattern-type constructor-pattern)
                                                      :name (tc:pattern-constructor-name constructor-pattern)
                                                      :patterns (subseq val 0 constructor-arity))
                         (subseq val constructor-arity)))
                :finally (return t)))

         ;; Otherwise, check the defaulted matrix.
         (t
          (let ((val (find-non-matching-value (default-matrix pattern-matrix)
                                              (rest column-types)
                                              env)))
            (cond
              ;; If this is exhaustive then PATTERN-MATRIX is exhaustive.
              ((eq val t)
               t)

              ;; If there are no possible constructors, emit a wildcard.
              ((null possible-constructors)
               (cons (make-wildcard-for-type current-column-type)
                     val))

              ;; Otherwise emit a possible constructor that was not named.
              (t
               (cons (find-unnamed-constructor first-column-constructors
                                               possible-constructors)
                     val))))))))))
