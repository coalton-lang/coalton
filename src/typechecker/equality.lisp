(in-package #:coalton-impl/typechecker)

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
        ((and (tvar-p type1)  (tvar-p type2))
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
        ((and (tcon-p type1) (tcon-p type2)) (equalp type1 type2))

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
                               (%make-substitution (tvar-tyvar (car s)) (cdr s)))
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

(defun normalize-kind (kind &optional (n 0) kpoly-subs)
  "Aligns all of the kpoly's within KIND into a normal form, numbered based off
where they occur within the kind signature."
  (declare (type kind kind)
           (type integer n)
           (type list kpoly-subs)
           (values kind integer list &optional))
  (etypecase kind
    (kfun
     (multiple-value-bind
           (k1 i l1) (normalize-kind (kfun-from kind) n kpoly-subs)
       (multiple-value-bind
             (k2 j l2) (normalize-kind (kfun-to kind) i l1)
         (values (kfun k1 k2) j l2))))
    (kpoly
     (let ((next-id
             (or (cdr (assoc (kpoly-name kind) kpoly-subs)) (incf n))))
       (values (kpoly nil next-id) n
               (cons (cons (kpoly-name kind) next-id) kpoly-subs))))
    (t (values kind n kpoly-subs))))

(defun kind= (k1 k2)
  "Checks if two kinds K1 and K2 are equal expressions."
  (declare (type kind k1) (type kind k2)
           (values boolean))
  (labels
      ((%kind-rec= (k1 k2 &optional kpoly-subs)
         (cond
           ((equalp k1 k2) (values t kpoly-subs))
           ((kpoly-p k1)
            (alexandria:if-let (k (assoc (kpoly-id k1) kpoly-subs))
              (%kind-rec= (cdr k) k2 kpoly-subs)
              (values t (cons (cons (kpoly-id k1) k2) kpoly-subs))))
           ((kpoly-p k2)
            (%kind-rec= k2 k1 kpoly-subs))
           ((and (kfun-p k1) (kfun-p k2))
            (multiple-value-bind
                  (a l1) (%kind-rec= (kfun-from k1) (kfun-from k2) kpoly-subs)
              (if a
                  (multiple-value-bind
                        (b l2) (%kind-rec= (kfun-to k1) (kfun-to k2) l1)
                    (values b l2))
                  (values nil kpoly-subs))))
           (t (values nil kpoly-subs)))))
    (values (%kind-rec= (normalize-kind k1) (normalize-kind k2)))))
