(in-package #:coalton-impl/typechecker)

;;;
;;; Kinds
;;;

(defstruct (kind (:constructor nil)))

(defstruct (kstar (:include kind)))

(defmethod make-load-form ((self kstar) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names nil
   :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type kstar))

(alexandria:define-constant kstar (make-instance 'kstar) :test #'equalp)

(defstruct
    (kfun
     (:include kind)
     (:constructor kfun (from to)))
  (from (required 'from) :type kind :read-only t)
  (to   (required 'to)   :type kind :read-only t))

(defmethod make-load-form ((self kfun) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names '(from to)
   :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type kfun))


(defstruct (kyvar (:constructor %make-kyvar))
  (id (required 'id) :type fixnum :read-only t))

(defmethod make-load-form ((self kyvar) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names '(id)
   :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type kyvar))

(defun kyvar-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'kyvar-p x)))

(deftype kyvar-list ()
  '(satisfies kyvar-list-p))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type kyvar-list))

(defstruct (kvar (:include kind)
                 (:constructor %make-kvar (kyvar)))
  (kyvar (required 'kyvar) :type kyvar :read-only t))

(defmethod make-load-form ((self kvar) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names '(kyvar)
   :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type kvar))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type kind))

(defvar *next-kvar-id* 0)

#+sbcl
(declaim (sb-ext:always-bound *next-kvar-id*))

(declaim (inline make-kvariable))
(defun make-kvariable ()
  (prog1 (%make-kvar (%make-kyvar :id *next-kvar-id*))
    (incf *next-kvar-id*)))

(defstruct (ksubstitution (:constructor %make-ksubstitution (from to)))
  (from (required 'from) :type kyvar :read-only t)
  (to   (required 'to)   :type kind  :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type ksubstitution))

(defun ksubstitution-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ksubstitution-p x)))

(deftype ksubstitution-list ()
  '(satisfies ksubstitution-list-p))

(defun compose-ksubstution-lists (s1 s2)
  (declare (type ksubstitution-list s1 s2)
           (values ksubstitution-list))
  (append
   (mapcar
    (lambda (s)
      (%make-ksubstitution
       (ksubstitution-from s)
       (apply-ksubstitution s1 (ksubstitution-to s))))
    s2)
   s1))


(defgeneric apply-ksubstitution (subs kind)
  (:method (subs (kind kstar))
    (declare (type ksubstitution-list subs)
             (ignore subs)
             (values kind &optional))
    kind)

  (:method (subs (kind kfun))
    (declare (type ksubstitution-list subs)
             (values kind &optional))
    (kfun
     (apply-ksubstitution subs (kfun-from kind))
     (apply-ksubstitution subs (kfun-to kind))))

  (:method (subs (kind kvar))
    (declare (type ksubstitution-list subs)
             (values kind &optional))
    (let ((elem (find (kvar-kyvar kind) subs :key #'ksubstitution-from :test #'equalp)))
      (if elem
          (ksubstitution-to elem)
          kind)))

  (:method (subs (kind list))
    (mapcar
     (lambda (kind)
       (apply-ksubstitution subs kind))
     kind)))

(defun kunify (kind1 kind2 subs)
  (declare (type kind kind1 kind2)
           (type ksubstitution-list subs)
           (values ksubstitution-list &optional))
  (let ((new-subs (kmgu (apply-ksubstitution subs kind1)
                       (apply-ksubstitution subs kind2))))
    (compose-ksubstution-lists new-subs subs)))

(defgeneric kmgu (kind1 kind2)
  (:method ((kind1 kstar) (kind2 kstar))
    (declare (values ksubstitution-list &optional))
    nil)

  (:method ((kind1 kvar) (kind2 kind))
    (declare (values ksubstitution-list &optional))
    (list
     (%make-ksubstitution
      (kvar-kyvar kind1)
      kind2)))

  (:method ((kind1 kind) (kind2 kvar))
    (declare (values ksubstitution-list &optional))
    (list
     (%make-ksubstitution
      (kvar-kyvar kind2)
      kind1)))

  (:method ((kind1 kfun) (kind2 kfun))
    (declare (values ksubstitution-list &optional))
    (append
     (kmgu (kfun-from kind1) (kfun-from kind2))
     (kmgu (kfun-to kind1) (kfun-to kind2))))

  (:method (kind1 kind2)
    (error 'kunify-error
           :kind1 kind1
           :kind2 kind2)))

(defun kind-return-type (kind)
  (if (kfun-p kind)
      (kind-return-type (kfun-to kind))
      kind))

(defgeneric kind-variables (kind)
  (:method ((kind kstar))
    nil)

  (:method ((kind kvar))
    (list (kvar-kyvar kind)))

  (:method ((kind kfun))
    (append
     (kind-variables (kfun-from kind))
     (kind-variables (kfun-to kind)))))

(defun kind-monomorphise-subs (kvars ksubs)
  (declare (type kyvar-list kvars)
           (values ksubstitution-list &optional))
  (compose-ksubstution-lists
   (loop :for kvar :in kvars
         :collect (%make-ksubstitution kvar kstar))
   ksubs))


(defun simple-kind-p (kind)
  "Whether KIND is a simple kind (either * or a function from many * to *)"
  (declare (type kind kind)
           (values boolean &optional))
  (etypecase kind
    (kstar t)
    (kfun (and (kstar-p (kfun-from kind))
               (simple-kind-p (kfun-to kind))))))

(defun kind-arity (kind)
  "The arity of the simple kind KIND (number of type arguments)"
  (declare (type kind kind)
           (values fixnum))
  (loop :while (kfun-p kind)
        :sum 1
        :do (setf kind (kfun-to kind))))

(defun make-kind-of-arity (arity)
  "Make a simple kind of arity ARITY"
  (declare (type fixnum arity)
           (values kind))
  (assert (<= 0 arity))
  (let ((kind kStar))
    (loop :for i :below arity
          :do (setf kind (kFun kStar kind)))
    kind))

(defun make-kind-function* (from to)
  (if (null from)
      to
      (kfun (car from) (make-kind-function* (cdr from) to))))

;;;
;;; Pretty printing
;;;

(defun pprint-kind (stream kind &optional colon-p at-sign-p)
  (declare (type stream stream)
           (type kind kind)
           (ignore colon-p)
           (ignore at-sign-p)
           (values kind))
  (etypecase kind
    (kstar
     (format stream "*"))
    (kfun
     (let ((from (kfun-from kind))
           (to (kfun-to kind)))
       (when (kfun-p from)
         (format stream "("))
       (pprint-kind stream from)
       (when (kfun-p from)
         (format stream ")"))

       (format stream " -> ")

       (when (kfun-p to)
         (format stream "("))
       (pprint-kind stream to)
       (when (kfun-p to)
         (format stream ")"))))
    (kvar
     (format stream "#K~A" (kyvar-id (kvar-kyvar kind)))))
  kind)

(set-pprint-dispatch 'kind 'pprint-kind)
