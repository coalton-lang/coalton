(defpackage #:coalton-impl/typechecker/equality
  (:use
   #:cl
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/substitutions
   #:coalton-impl/typechecker/predicate
   #:coalton-impl/typechecker/scheme)
  (:export
   #:type=
   #:qualified-type=
   #:type-scheme=
   #:type-predicate=))

(in-package #:coalton-impl/typechecker/equality)

;;;
;;; Type equality
;;;

(defun type= (type1 type2)
  (declare (type ty type1 type2)
           (values boolean list))
  (let ((var-table nil))
    (labels ((%type= (type1 type2)
      (cond
        ;; Type variables
        ((and (tyvar-p type1)  (tyvar-p type2))
         (let* ((pair1 (find type1 var-table :key #'car :test #'equalp))
                (pair2 (find type2 var-table :key #'car :test #'equalp)))
           (cond
             ((and (null pair1) (null pair2))
                 (pushnew (cons type1 type2) var-table :key #'car :test #'equalp)
                 (pushnew (cons type2 type1) var-table :key #'car :test #'equalp)
                 t)
             ((or (null pair1) (null pair2))
              nil)
             (t
              (and (eql (car pair1) (cdr pair2))
                   (eql (cdr pair1) (car pair2)))))))


        ;; Type constants
        ((and (tycon-p type1) (tycon-p type2)) (equalp type1 type2))

        ;; Type application
        ((and (tapp-p type1) (tapp-p type2))
         (and (%type= (tapp-from type1) (tapp-from type2))
              (%type= (tapp-to type1) (tapp-to type2))))

        (t nil))))
      (let ((ret (%type= type1 type2)))
        (values ret var-table)))))

(defun qualified-type= (qual-type1 qual-type2)
  ;; Check that the types are equal
  (multiple-value-bind (types-equal-p var-table)
      (type= (qualified-ty-type qual-type1)
             (qualified-ty-type qual-type2))
    (unless types-equal-p
      (return-from qualified-type= nil))

    ;; Create a substitution list from the variables
    (let ((subs-list (mapcar (lambda (s)
                               (make-substitution :from (car s) :to (cdr s)))
                             var-table)))

      ;; Now check that all constraints in type1 exist in type2, mapping type variables
      (loop :for pred :in (qualified-ty-predicates qual-type1) :do
        (unless (member (apply-substitution subs-list pred)
                        (qualified-ty-predicates qual-type2)
                        :test #'equalp)
          (return-from qualified-type= nil)))
      ;; And do the same for constraints in type2
      (loop :for pred :in (qualified-ty-predicates qual-type2) :do
        (unless (member (apply-substitution subs-list pred)
                        (qualified-ty-predicates qual-type1)
                        :test #'equalp)
          (return-from qualified-type= nil)))))

  t)

(defun type-scheme= (scheme1 scheme2)
  (qualified-type=
   (fresh-inst scheme1)
   (fresh-inst scheme2)))

(defun type-predicate= (pred1 pred2)
  (and (eql (ty-predicate-class pred1)
            (ty-predicate-class pred2))
       (every (lambda (x y)
                (type= x y))
              (ty-predicate-types pred1)
              (ty-predicate-types pred2))))
