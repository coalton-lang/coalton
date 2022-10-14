(defpackage #:coalton-impl/analysis/pattern-exhaustiveness
  (:use #:cl)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:rt #:coalton-impl/runtime)
   (#:tc #:coalton-impl/typechecker)
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error))
  (:export
   #:non-exhaustive-match-warning
   #:useless-pattern-warning
   #:exhaustive-patterns-p
   #:useful-pattern-p
   #:find-non-matching-value))

(in-package #:coalton-impl/analysis/pattern-exhaustiveness)

;;;
;;; This file provides an implementation of pattern matching
;;; exhaustiveness and usefulness analysis as described in
;;; Maranget 2007, http://moscova.inria.fr/~maranget/papers/warn/index.html
;;;


(defun exhaustive-patterns-p (patterns env)
  "Are PATTERNS exhaustive?"
  (not
   (useful-pattern-clause-p
    (mapcar #'list patterns)
    (list (tc:make-pattern-wildcard
           :type (tc:qualify nil (tc:make-variable))
           :source (cons nil nil)))
    env)))

(defun useful-pattern-p (patterns pattern env)
  "Is PATTERN useful within list PATTERNS? PATTERN must EQ one element of PATTERNS."
  (useful-pattern-clause-p
   (loop :for p :in patterns
         :while (not (eq p pattern))
         :collect (list p))
   (list pattern)
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

(defun useful-pattern-clause-p (pattern-matrix clause env)
  "Is CLAUSE useful with respect to PATTERN-MATRIX?

PATTERN-MATRIX is a list of lists representing a pattern matrix in row-major format.
CLAUSE is a list representing a row-vector of patterns."
  (declare (type pattern-matrix pattern-matrix)
           (type tc:pattern-list clause)
           (type tc:environment env)
           (values boolean &optional))

  ;; NOTE: It is assumed that all rows of PATTERN-MATRIX have the same number of columns.
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
     (useful-pattern-clause-p
      (specialize-matrix pattern-matrix (first clause))
      (first (specialize-matrix (list clause) (first clause)))
      env))

    ;; Sub-case 2: The first member of CLAUSE is a wildcard (or variable)
    ((or (tc:pattern-wildcard-p (first clause))
         (tc:pattern-var-p (first clause)))

     (let ((first-column-constructors
             (loop :for row :in pattern-matrix
                   :for elem := (first row)
                   :when (or (tc:pattern-literal-p elem)
                             (tc:pattern-constructor-p elem))
                     :collect elem)))
       (cond
         ;; If the constructors form a complete signature then CLAUSE
         ;; is only useful if it is useful when specialized over the
         ;; constructor.
         ((complete-signature-p first-column-constructors env)
          (loop :for pattern :in first-column-constructors
                :when (useful-pattern-clause-p
                       (specialize-matrix pattern-matrix pattern)
                       (first (specialize-matrix (list clause) pattern))
                       env)
                  :do (return t)
                :finally (return nil)))
         ;; Otherwise, create a defaulted matrix and recurse, removing
         ;; the first member of CLAUSE.
         (t
          (useful-pattern-clause-p
           (default-matrix pattern-matrix)
           (rest clause)
           env)))))
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
                                               :type (tc:pattern-type pattern)
                                               :source (cons nil nil)))
                                            (tc:pattern-constructor-patterns pattern))
                                    (rest row))))))
                  (t
                   (util:coalton-bug "Not reachable.")))))

(defun complete-signature-p (patterns env)
  "Do the set of PATTERNS form a complete signature of the constructed type?"
  (declare (type tc:pattern-list patterns)
           (type tc:environment env)
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
     (let* ((constructor-names (mapcar #'tc:pattern-constructor-name patterns))
            (constructed-type (tc:constructor-entry-constructs (tc:lookup-constructor env (first constructor-names))))
            (type-constructors (tc:type-entry-constructors (tc:lookup-type env constructed-type))))
       (null (set-difference type-constructors constructor-names :test #'eq))))))

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

(defun find-non-matching-value (pattern-matrix n env)
  "Finds an example of a non-matching value for PATTERN-MATRIX or, if PATTERN-MATRIX is exhaustive returns T."
  (declare (type pattern-matrix pattern-matrix)
           (type (integer 0) n)
           (optimize (debug 3)))
  (cond
    ;; An empty pattern generates n wildcards.
    ((and (zerop (length pattern-matrix)))
     (loop :for i :below n
           :collect (tc:make-pattern-wildcard
                     :type (tc:qualify nil (tc:make-variable))
                     :source (cons nil nil))))
    ;; Zero wildcards with a pattern matrix that has zero columns indicates the matrix is exhaustive.
    ((and (zerop n)
          (zerop (length (first pattern-matrix))))
     t)
    (t
     (let ((first-column-constructors
             (loop :for row :in pattern-matrix
                   :for elem := (first row)
                   :when (tc:pattern-constructor-p elem)
                     :collect elem)))
       (cond
         ;; If the constructors in the PATTERN-MATRIX form a complete
         ;; signature then specialize and return the first
         ;; non-matching sub-value.
         ((complete-signature-p first-column-constructors env)
          (loop :for ctor :in first-column-constructors
                :for ctor-arity := (length (tc:pattern-constructor-patterns ctor))
                :for val := (find-non-matching-value
                             (specialize-matrix pattern-matrix ctor)
                             (+ ctor-arity n -1)
                             env)
                :unless (eq val t)
                  :do (return (cons (tc:make-pattern-constructor
                                     :type (tc:pattern-type ctor)
                                     :source (cons nil nil)
                                     :name (tc:pattern-constructor-name ctor)
                                     :patterns (subseq val 0 ctor-arity))
                                    (subseq val ctor-arity (+ ctor-arity n -1))))
                :finally (return t)))
         ;; Otherwise, check the defaulted matrix.
         (t
          (let ((val (find-non-matching-value
                      (default-matrix pattern-matrix)
                      (1- n)
                      env)))
            (cond
              ;; If this is exhaustive then PATTERN-MATRIX is exhaustive.
              ((eq val t)
               t)
              ;; If there are no constructors then emit a wildcard.
              ((null first-column-constructors)
               (cons (tc:make-pattern-wildcard
                      :type (tc:qualify nil (tc:make-variable))
                      :source (cons nil nil))
                     val))
              ;; Or emit a constructor which was not named in this pattern.
              (t
               (cons (find-unnamed-constructor first-column-constructors env)
                     val))))))))))

(defun find-unnamed-constructor (patterns env)
  "Find and create a pattern constructor with the type of, but not named in PATTERNS."
  (declare (type tc:pattern-list patterns)
           (type tc:environment env)
           (values tc:pattern))
  (let* ((constructor-names (mapcar #'tc:pattern-constructor-name patterns))
         (constructed-type (tc:constructor-entry-constructs (tc:lookup-constructor env (first constructor-names))))
         (type-constructors (tc:type-entry-constructors (tc:lookup-type env constructed-type)))
         (unnamed-constructor (first (set-difference type-constructors constructor-names :test #'eq)))
         (unnamed-constructor-entry (tc:lookup-constructor env unnamed-constructor)))
    (unless unnamed-constructor
      (util:coalton-bug "Not reachable."))

    ;; NOTE: Here we _could_ reasonably return all missing
    ;; constructors, however that would require additional support in
    ;; the error generation. Instead we just select the first one.
    (tc:make-pattern-constructor
     :type (tc:pattern-type (first patterns))
     :source (cons nil nil)
     :name unnamed-constructor
     :patterns (loop :for i :below (tc:constructor-entry-arity unnamed-constructor-entry)
                     :collect (tc:make-pattern-wildcard
                               :type (tc:qualify nil (tc:make-variable))
                               :source (cons nil nil))))))
