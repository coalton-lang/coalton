(defpackage #:coalton-impl/typechecker/parse-type
  (:use
   #:cl
   #:coalton-impl/ast
   #:coalton-impl/typechecker/kinds
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/type-errors
   #:coalton-impl/typechecker/substitutions
   #:coalton-impl/typechecker/predicate
   #:coalton-impl/typechecker/scheme
   #:coalton-impl/typechecker/context-reduction
   #:coalton-impl/typechecker/fundeps
   #:coalton-impl/typechecker/environment)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error))
  (:export
   #:parse-and-resolve-type             ; FUNCTION
   #:parse-type-expr                    ; FUNCTION
   #:collect-type-vars                  ; FUNCTION
   #:collect-types                      ; FUNCTION
   #:parse-qualified-type-expr          ; FUNCTION
   #:parse-type-predicate               ; FUNCTION
   #:coalton-arrow-p                    ; FUNCTION
   #:coalton-double-arrow-p             ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/parse-type)

;;;
;;; Type parsing
;;;

;;
;; A valid type signature is defined as follows:
;;
;; type-signature := <type-expr>                ; Type with no constraints
;;                 | (<pred> => <type-expr>)    ; Type with single constraint
;;                 | ((<pred>)+ => <type-expr>) ; Type with multiple constraints
;;
;; NOTE: either => or ⇒ can be used in type constraint expressions.
;;
;; pred := <class> <type-expr>+
;;
;; type-expr := <tyvar>                         ; Type variable
;;            | <0-arity tycon>                 ; Zero arity type constructor
;;            | (<tycon> <type-expr>+)          ; Applied type constructor
;;            | (<type-expr> [-> <type-expr>]+) ; Function type
;;
;; NOTE: either -> or → can be used in function type expressions.
;;
;; tyvar := symbol in the keyword package (e.g. :a, :b, etc.)
;;

(alexandria:define-constant +keyword-package+ (find-package "KEYWORD") :test #'equalp)

(defun parse-and-resolve-type (env expr)
  "Parse the type expression EXPR in environment ENV returning a TY-SCHEME"
  (declare (type environment env)
           (values ty-scheme &optional))

  ;; Parse out the expression and quantify over non-disallowed type variables
  (let ((type-vars
          (mapcar
           (lambda (type-var)
             (list type-var (make-variable (make-kvariable))))
           (collect-type-vars expr))))
    (multiple-value-bind (type ksubs)
        (parse-qualified-type-expr env expr type-vars nil)
      
      (let* ((kvars (kind-variables (apply-ksubstitution ksubs type)))

             (ksubs (kind-monomorphize-subs kvars ksubs))

             (type (apply-ksubstitution ksubs type)))

        (let* ((preds (qualified-ty-predicates type))
               (reduced-preds (reduce-context env preds nil :allow-deferred-predicates nil)))
          (unless (null (set-exclusive-or preds reduced-preds :test #'equalp))
            (with-pprint-variable-context ()
              (warn "Context for type ~S~%    can be reduced from ~{~S~^, ~} to ~{~S~^, ~}"
                    expr
                    preds
                    (or reduced-preds '("nothing"))))))

        (quantify (type-variables type) type)))))

(defun rewrite-type-expr (expr)
  (etypecase expr
    (symbol expr)
    (list
     (let ((arrow-index (position-if #'coalton-arrow-p expr)))
       (if arrow-index
           (list 'coalton:Arrow
                 (rewrite-type-expr (subseq expr 0 arrow-index))
                 (rewrite-type-expr (subseq expr (1+ arrow-index))))
           (mapcar #'rewrite-type-expr expr))))))

(defun parse-type-expr-inner (env expr type-vars ksubs)
  (declare (type environment env)
           (type t expr)
           (type list type-vars)
           (type ksubstitution-list ksubs)
           (values ty ksubstitution-list &optional))
  (labels
      ((find-tyvar-entry (name)
         (second (find name type-vars :key #'car))))

    (etypecase expr
      (symbol
       (let ((tyvar (find-tyvar-entry expr)))
         (values
          (if tyvar
              tyvar
              (type-entry-type (lookup-type env expr)))
          ksubs)))

      (list
       (let ((elems
               (loop :for elem :in expr
                     :collect (multiple-value-bind (type new-ksubs)
                                  (parse-type-expr-inner env elem type-vars ksubs)
                                (setf ksubs new-ksubs)
                                type))))
         (apply-type-argument-list
          (first elems)
          (rest elems)
          :ksubs ksubs))))))

(defun parse-type-expr (env expr type-vars ksubs)
  (multiple-value-bind (ty ksubs)
      (parse-type-expr-inner env (rewrite-type-expr expr) type-vars ksubs)
    (values
     (apply-ksubstitution ksubs ty)
     ksubs)))

(defun collect-type-vars (expr)
  (let ((type-vars nil))
    (labels ((inner (expr)
               (etypecase expr
                 (symbol
                  (when (equalp +keyword-package+ (symbol-package expr))
                    (push expr type-vars)))

                 (list
                  (loop :for elem :in expr
                        :do (inner elem))))))
      (inner expr)
      (remove-duplicates type-vars :test #'equalp))))

(defun collect-types (expr)
  (let ((types nil))
    (labels ((inner (expr)
               (etypecase expr
                 (symbol
                  (when (not (equalp +keyword-package+ (symbol-package expr)))
                    (push expr types)))

                 (list
                  (loop :for elem :in expr
                        :do (inner elem))))))
      (inner expr)
      (remove-duplicates types :test #'equalp))))

(defun parse-qualified-type-expr (env expr type-vars ksubs)
  "Parse qualified type expression EXPR in type environment ENV"
  (declare (type environment env)
           (type list type-vars)
           (values qualified-ty ksubstitution-list))

  (error:with-context ("qualified type expression ~S" expr)
    (let ((unparsed-type nil)
          (unparsed-preds nil))

      ;; If the expression is a list and contains => then it has constraints
      (if (and (listp expr) (some #'coalton-double-arrow-p expr))
          ;; Split the expression into parts before and after the arrow
          (let ((subseqs (split-sequence:split-sequence-if #'coalton-double-arrow-p expr)))
            (unless (= 2 (length subseqs))
              (error-parsing-type expr "Malformed constrained type"))

            (setf unparsed-type (second subseqs))

            ;; If the first member of the predicates is a list then we can assume there are multiple to parse.
            (if (listp (car (first subseqs)))
                (setf unparsed-preds (first subseqs))
                (setf unparsed-preds (list (first subseqs)))))

          (setf unparsed-type expr))

      ;; Check for duplicate predicates before parsing
      (labels ((check-for-duplicate-preds (preds)
                 (unless (null preds)
                   (let ((pred (car preds))
                         (rest (cdr preds)))
                     (if (find pred rest :test #'equalp)
                         (error-parsing pred "duplicate predicate")
                         (check-for-duplicate-preds rest))))))
        (check-for-duplicate-preds unparsed-preds))

      (let ((preds
              (loop :for unparsed-pred :in unparsed-preds
                    :collect (multiple-value-bind (pred new-ksubs)
                                 (parse-type-predicate env unparsed-pred type-vars ksubs)
                               (setf ksubs new-ksubs)
                               pred))))

        (multiple-value-bind (type new-ksubs)
            (parse-type-expr env unparsed-type type-vars ksubs)
          (setf ksubs new-ksubs)

          (check-for-ambigious-variables (apply-ksubstitution ksubs preds) (apply-ksubstitution ksubs type) env)

          (values
           (apply-ksubstitution ksubs (qualify preds type))
           ksubs))))))

(defun parse-type-predicate (env expr type-vars ksubs)
  "Parse type predicate EXPR in type environment ENV"
  (declare (type environment env)
           (type list type-vars)
           (type ksubstitution-list ksubs)
           (values ty-predicate ksubstitution-list))
  (error:with-context ("type predicate ~S" expr)
    (unless (and (listp expr)
                 (>= (length expr) 2)
                 (symbolp (first expr)))
      (error-parsing-type expr "Malformed type predicate"))

    (let* ((pred-class (first expr))

           (class-entry (lookup-class env pred-class))

           (pred-types
             ;; We need two loops so that we can do with or without the kinds
             (if class-entry
                 (loop :for pred-expr :in (cdr expr)
                       :for pred-kind :in (mapcar #'kind-of (ty-predicate-types (ty-class-predicate class-entry)))
                       :collect (multiple-value-bind (pred-type new-ksubs)
                                    (parse-type-expr env pred-expr type-vars ksubs)
                                  (setf ksubs new-ksubs)
                                  (setf ksubs (kunify pred-kind (kind-of pred-type) ksubs))
                                  pred-type))
                 (loop :for pred-expr :in (cdr expr)
                       :collect (multiple-value-bind (pred-type new-ksubs)
                                    (parse-type-expr env pred-expr type-vars ksubs)
                                  (setf ksubs new-ksubs)
                                  pred-type)))))
      (when class-entry
        (unless (= (length (ty-predicate-types (ty-class-predicate class-entry))) (length (cdr expr)))
          (error-parsing-type
           expr
           (format nil "Unexpected number of type variables: Expected ~A Got ~A"
                   (length (ty-predicate-types (ty-class-predicate class-entry)))
                   (length (cdr expr))))))

      (values
       (make-ty-predicate
        :class pred-class
        :types pred-types)
       ksubs))))

(defun check-for-ambigious-variables (preds type env)
  (declare (type ty-predicate-list preds)
           (type ty type)
           (type environment env))

  (let* ((old-unambigious-vars (type-variables type))
         (unambigious-vars old-unambigious-vars)) 

    (block fundep-fixpoint
      (loop :for i :below +fundep-max-depth+
            :do (setf old-unambigious-vars unambigious-vars)
            :do (loop :for pred :in preds
                      :for pred-tys := (ty-predicate-types pred)
                      :for class-name := (ty-predicate-class pred)
                      :for class := (lookup-class env class-name)
                      :for map := (ty-class-class-variable-map class)
                      :when (ty-class-fundeps class) :do
                        (loop :for fundep :in (ty-class-fundeps class)
                              :for from-vars := (util:project-map (fundep-from fundep) map pred-tys)
                              :do (when (subsetp from-vars unambigious-vars :test #'equalp)
                                    (let ((to-vars (util:project-map (fundep-to fundep) map pred-tys)))
                                      (setf unambigious-vars
                                            (remove-duplicates (append to-vars unambigious-vars) :test #'equalp))))))

            :when (equalp unambigious-vars old-unambigious-vars) ; Exit when progress stops
              :do (return-from fundep-fixpoint)

            :finally (util:coalton-bug "Fundep solving failed to fixpoint")))

    (unless (subsetp (type-variables preds) unambigious-vars :test #'equalp)
      (error 'ambigious-constraint-variables
             :type (qualify preds type)
             :variables (set-difference (type-variables preds) unambigious-vars :test #'equalp)))))

;;;
;;; Arrows
;;;

(alexandria:define-constant +coalton-arrows+ '(coalton:-> coalton:→) :test #'equalp
  :documentation "Allowed arrows in coalton function type expressions.")

(alexandria:define-constant +coalton-double-arrows+ '(coalton:=> coalton:⇒) :test #'equalp
  :documentation "Allowed double arrows in coalton type constraint expressions.")

(defun arrow-p (symbol valid-symbols)
  (or (and (member symbol valid-symbols) t)
      (and (symbolp symbol)
           ;; Throw an error if a non-coalton arrow is used.
           ;; NOTE: This disallows users to declare types with the same name as coalton arrows.
           (dolist (sym valid-symbols)
             (when (string= (symbol-name sym) (symbol-name symbol))
               (error-parsing-type sym "Invalid coalton arrow ~S. Did you mean ~S" sym symbol)))
           nil)))

(defun coalton-arrow-p (symbol)
  (arrow-p symbol +coalton-arrows+))

(defun coalton-double-arrow-p (symbol)
  (arrow-p symbol +coalton-double-arrows+))
