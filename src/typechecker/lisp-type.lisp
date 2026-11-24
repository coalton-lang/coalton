(defpackage #:coalton-impl/typechecker/lisp-type
  (:use
   #:cl
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/predicate
   #:coalton-impl/typechecker/scheme
   #:coalton-impl/typechecker/environment)
  (:local-nicknames
   (#:subs #:coalton-impl/typechecker/substitutions)
   (#:unify #:coalton-impl/typechecker/unify)
   (#:util #:coalton-impl/util))
  (:export
   #:lisp-type))

(in-package #:coalton-impl/typechecker/lisp-type)

;; These standard library types do not exist when the compiler is built.
(defun complex-tycon-p (name)
  (and (find-package "COALTON/MATH/COMPLEX")
       (eq name (find-symbol "COMPLEX" "COALTON/MATH/COMPLEX"))))

(defun lisparray-tycon-p (name)
  (and (find-package "COALTON/LISPARRAY")
       (eq name (find-symbol "LISPARRAY" "COALTON/LISPARRAY"))))

(defun application-size (ty)
  (if (tapp-p ty)
      (+ 1 (application-size (tapp-from ty)) (application-size (tapp-to ty)))
      1))

(defun lisp-type (ty env &optional incomplete-types)
  "Return a Lisp type containing every representation of TY. INCOMPLETE-TYPES
are nominal definitions in an SCC whose representations are not installed yet."
  (labels
      ((representation (ty seen)
         ;; The second value says whether the first is also a known storage
         ;; specifier. Arrays are invariant in their upgraded element type:
         ;; (SIMPLE-ARRAY T (*)) does not contain specialized float arrays.
         (typecase ty
           ((or tyvar tgen) (values 't nil))
           ((or tycon tapp)
            (destructuring-bind (head &rest args) (flatten-type ty)
              (let ((name (and (tycon-p head) (tycon-name head))))
                (cond
                  ((or (null name) (member name incomplete-types))
                   (values 't nil))
                  ((eq head *keyword-frame-type*)
                   (values 'list t))
                  ((function-type-p ty)
                   (values 'function t))
                  ((and args (complex-tycon-p name))
                   (cond
                     ((ty= (first args) *single-float-type*)
                      (values '(complex single-float) t))
                     ((ty= (first args) *double-float-type*)
                      (values '(complex double-float) t))
                     (t
                      (values `(or number ,name) nil))))
                  ((and args (lisparray-tycon-p name))
                   (multiple-value-bind (element known-p) (representation (first args) seen)
                     (values `(simple-array ,(if known-p element '*) (*)) known-p)))
                  (t
                   (let ((entry (lookup-type env name :no-error t)))
                     (cond
                       ((null entry)
                        ;; Compiler-generated nominal Lisp types.
                        (values name t))
                       ((type-entry-newtype entry)
                        (transparent-representation ty entry seen))
                       (t
                        (values (type-entry-runtime-type entry) t)))))))))
           (function-ty (values 'function t))
           (forall-ty
            (if (forall-ty-predicates ty)
                (values 'function t)
                (representation (forall-ty-body ty) seen)))
           (result-ty
            (let ((outputs (result-ty-output-types ty)))
              (cond
                ((null outputs) (values 'null t))
                ((null (rest outputs)) (representation (first outputs) seen))
                (t
                 (util:coalton-bug "Unable to produce a single lisp type for multiple-value result ~S"
                                   ty)))))
           (ty-scheme (representation (fresh-inst ty) seen))
           (qualified-ty
            (if (qualified-ty-predicates ty)
                (values 'function t)
                (representation (qualified-ty-type ty) seen)))
           (t (util:coalton-bug "Unable to produce lisp type for ~S" ty))))

       (transparent-representation (ty entry seen)
         ;; Permit e.g. (Id (Id F64)), but stop direct, mutual and growing
         ;; recursive expansions, even when the indices change at every step.
         (let* ((name (type-entry-name entry))
                (size (application-size ty))
                (previous (assoc name seen)))
           (when (and previous (>= size (cdr previous)))
             (return-from transparent-representation (values 't nil)))
           (let* ((constructor (first (type-entry-constructors entry)))
                  (signature (qualified-ty-type (fresh-inst (lookup-value-type env constructor))))
                  (field (function-type-from signature)))
             (multiple-value-bind (subs possible-p)
                 (unify:try-unify nil (function-return-type signature) ty :all)
               (multiple-value-bind (bound known-p)
                   (representation (if possible-p (subs:apply-substitution subs field) field)
                                   (acons name size seen))
                 ;; The RuntimeRepr instance used by array allocation is
                 ;; compiled once for the whole newtype, not per application.
                 (values bound (and known-p (equal bound (type-entry-runtime-type entry))))))))))
    (values (representation ty nil))))
