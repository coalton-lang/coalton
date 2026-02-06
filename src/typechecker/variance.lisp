(defpackage #:coalton-impl/typechecker/variance
  (:use
   #:cl
   #:coalton-impl/typechecker/kinds
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/environment)
  (:export
   #:variance                            ; TYPE
   #:variance-list                       ; TYPE
   #:variance-p                          ; FUNCTION
   #:variance-covariant-p                ; FUNCTION
   #:variance-flip                       ; FUNCTION
   #:variance-compose                    ; FUNCTION
   #:variance-join                       ; FUNCTION
   #:collect-tyvar-variances             ; FUNCTION
   #:tyvar-variance                      ; FUNCTION
   #:make-env-variance-resolver          ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/variance)

;;;
;;; Variance
;;;
;;; This module computes type-variable variance observations used by the
;;; relaxed value restriction in DEFINE.LISP.
;;;
;;; References:
;;;   - Jacques Garrigue, "Relaxing the Value Restriction" (2004)
;;;   - OCaml manual section "Polymorphism and its limitations"
;;;
;;; Design note:
;;;   We prefer conservative answers when information is missing. Unknown,
;;;   underspecified, or opaque type constructors are treated as invariant.
;;;   This may reject some programs but avoids unsound generalization.
;;;

(deftype variance ()
  '(member :covariant :contravariant :invariant))

(defun variance-p (x)
  (typep x 'variance))

(defun variance-covariant-p (variance)
  (declare (type (or variance (eql :absent)) variance)
           (values boolean &optional))
  (eq variance ':covariant))

(defun variance-flip (variance)
  (declare (type variance variance)
           (values variance &optional))
  (ecase variance
    (:covariant ':contravariant)
    (:contravariant ':covariant)
    (:invariant ':invariant)))

(defun variance-compose (outer inner)
  "Compose variances through nested type positions.

OUTER is the variance of the current position; INNER is the variance of the
child position relative to the current node."
  (declare (type variance outer inner)
           (values variance &optional))
  (ecase outer
    (:invariant ':invariant)
    (:covariant inner)
    (:contravariant (variance-flip inner))))

(defun variance-join (left right)
  "Join two variance observations for the same type variable.

`:absent` means there was no observation in that branch. Conflicting
covariant/contravariant observations collapse to invariant."
  (declare (type (or variance (eql :absent)) left right)
           (values (or variance (eql :absent)) &optional))
  (cond
    ((eq left ':absent) right)
    ((eq right ':absent) left)
    ((or (eq left ':invariant)
         (eq right ':invariant))
     ':invariant)
    ((eq left right) left)
    (t ':invariant)))

(defun make-env-variance-resolver (env)
  "Return a resolver for constructor variances in ENV.

The returned function accepts a type constructor name and the observed arity in
the current type application.

Missing, underspecified, or opaque entries are treated as invariant."
  (declare (type environment env)
           (values function &optional))
  (lambda (tycon-name arity)
    (declare (type symbol tycon-name)
             (type alexandria:non-negative-fixnum arity)
             (values variance-list &optional))
    (let ((fallback (make-list arity :initial-element ':invariant)))
      (alexandria:if-let (entry (lookup-type env tycon-name :no-error t))
        (let ((variances (type-entry-variances entry)))
          (if (>= (length variances) arity)
              (subseq variances 0 arity)
              (append variances
                      (make-list (- arity (length variances))
                                 :initial-element ':invariant))))
        fallback))))

(defun tyvar-variance (table tyvar)
  "Return TYVAR's variance in TABLE, or `:absent` if unseen."
  (declare (type hash-table table)
           (type tyvar tyvar)
           (values (or variance (eql :absent)) &optional))
  (or (gethash (tyvar-id tyvar) table)
      ':absent))

(defun collect-tyvar-variances (type resolver &key (position ':covariant) (table (make-hash-table :test #'eql)))
  "Collect type-variable variances for TYPE into TABLE.

TYPE may be a type or a list of types. RESOLVER must return a list of
variances for a type constructor name and application arity.

The walk composes variance through nested type constructors. Unknown
applications (for example higher-kinded heads without a resolved TYCON) are
treated as invariant to preserve soundness."
  (declare (type (or ty list) type)
           (type function resolver)
           (type variance position)
           (type hash-table table)
           (values hash-table &optional))
  (labels ((record (tyvar variance)
             (declare (type tyvar tyvar)
                      (type variance variance))
             (let* ((id (tyvar-id tyvar))
                    (old (or (gethash id table) ':absent))
                    (new (variance-join old variance)))
               (unless (eq old new)
                 (setf (gethash id table) new))))
           (walk (node variance)
             (declare (type t node)
                      (type variance variance))
             (typecase node
               (null nil)
               (list
                 (dolist (entry node)
                   (walk entry variance)))
               (tyvar
                 (record node variance))
               (tapp
                 (let* ((flattened (flatten-type node))
                        (head (first flattened))
                        (args (rest flattened)))
                   (if (typep head 'tycon)
                       (let ((arg-variances
                               (funcall resolver
                                        (tycon-name head)
                                        (length args))))
                         (loop :for arg :in args
                               :for i :from 0
                               :for arg-variance := (or (nth i arg-variances)
                                                        ':invariant)
                               :do (walk arg (variance-compose variance arg-variance))))
                       ;; Conservative fallback for higher-kinded applications:
                       ;; unknown applications are treated as invariant.
                       (progn
                         (walk (tapp-from node) (variance-compose variance ':invariant))
                         (walk (tapp-to node) (variance-compose variance ':invariant))))))
               ((or tycon tgen ty)
                nil))))
    (walk type position)
    table))
