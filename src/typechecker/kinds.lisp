(defpackage #:coalton-impl/typechecker/kinds
  (:use
   #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:settings #:coalton-impl/settings)
   (#:error #:coalton-impl/error))
  (:export
   #:kind                               ; STRUCT
   #:kind-list                          ; TYPE
   #:kstar                              ; STRUCT
   #:+kstar+                            ; CONSTANT
   #:kfun                               ; STRUCT
   #:make-kfun                          ; CONSTRUCTOR
   #:kfun-from                          ; ACCESSOR
   #:kfun-to                            ; ACCESSOR
   #:kyvar                              ; STRUCT
   #:make-kyvar                         ; CONSTRUCTOR
   #:kyvar-id                           ; ACCESSOR
   #:kyvar-list                         ; TYPE
   #:make-kvariable                     ; FUNCTION
   #:ksubstitution                      ; STRUCT
   #:make-ksubstution                   ; CONSTRUCTOR
   #:ksubstitution-from                 ; ACCESSOR
   #:ksubstitution-to                   ; ACCESSOR
   #:ksubstitution-list                 ; TYPE
   #:compose-ksubstitution-list         ; FUNCTION
   #:apply-ksubstitution                ; FUNCTION
   #:kunify                             ; FUNCTION
   #:kmgu                               ; FUNCTION
   #:kind-return-type                   ; FUNCTION
   #:kind-variables                     ; FUNCTION
   #:kind-variables-generic%                   ; METHOD
   #:kind-monomorphize-subs             ; FUNCTION
   #:simple-kind-p                      ; FUNCTION
   #:kind-arity                         ; FUNCTION
   #:make-kind-function*                ; FUNCTION
   #:kfun-p                             ; FUNCTION
   #:kstar-p                            ; FUNCTION
   #:kunify-error                       ; CONDITION
   ))

(in-package #:coalton-impl/typechecker/kinds)

;;;
;;; Kinds
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (kind (:constructor nil)
                   (:copier nil)))

  (defmethod make-load-form ((self kind) &optional env)
    (make-load-form-saving-slots self :environment env)))

(defun kind-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'kind-p x)))

(deftype kind-list ()
  '(satisfies kind-list-p))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (kstar (:include kind)
                    (:copier nil))))

(alexandria:define-constant +kstar+ (make-kstar) :test #'equalp)

(defstruct (kfun (:include kind))
  (from (util:required 'from) :type kind :read-only t)
  (to   (util:required 'to)   :type kind :read-only t))

(defstruct (kyvar (:include kind)) 
  (id (util:required 'id) :type fixnum :read-only t))

(defun kyvar-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'kyvar-p x)))

(deftype kyvar-list ()
  '(satisfies kyvar-list-p))


;;;
;;; Kind Variables
;;;

(defparameter *next-kvar-id* 0)

#+sbcl
(declaim (sb-ext:always-bound *next-kvar-id*))

(declaim (inline make-kvariable))
(defun make-kvariable ()
  (prog1 (make-kyvar :id *next-kvar-id*)
    (incf *next-kvar-id*)))


;;;
;;; Kind Substitutions
;;;

(defstruct ksubstitution 
  (from (util:required 'from) :type kyvar :read-only t)
  (to   (util:required 'to)   :type kind  :read-only t))

(defun ksubstitution-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ksubstitution-p x)))

(deftype ksubstitution-list ()
  '(satisfies ksubstitution-list-p))

(defgeneric apply-ksubstitution (subs kind)
  (:method (subs (kind kstar))
    (declare (type ksubstitution-list subs)
             (ignore subs)
             (values kind &optional))
    kind)

  (:method (subs (kind kfun))
    (declare (type ksubstitution-list subs)
             (values kind &optional))
    (make-kfun
     :from (apply-ksubstitution subs (kfun-from kind))
     :to (apply-ksubstitution subs (kfun-to kind))))

  (:method (subs (kind kyvar))
    (declare (type ksubstitution-list subs)
             (values kind &optional))
    (let ((elem (find kind subs :key #'ksubstitution-from :test #'equalp)))
      (if elem
          (ksubstitution-to elem)
          kind)))

  (:method (subs (kind list))
    (mapcar
     (lambda (kind)
       (apply-ksubstitution subs kind))
     kind)))

(defun compose-ksubstitution-lists (s1 s2)
  "Returns the composition of S1 and S2.

  (apply-ksubstitution s2 (apply-ksubstitution s1 k)) == (apply-ksubstitution (compose-ksubstitutions s1 s2) k)"
  (declare (type ksubstitution-list s1)
           (type ksubstitution-list s2)
           (values ksubstitution-list &optional))
  (append
   (mapcar
    (lambda (s)
      (make-ksubstitution
       :from (ksubstitution-from s)
       :to (apply-ksubstitution s1 (ksubstitution-to s))))
    s2)
   s1))

;;;
;;; Kind Unification
;;;

(defun kunify (kind1 kind2 subs)
  (declare (type kind kind1 kind2)
           (type ksubstitution-list subs)
           (values ksubstitution-list &optional))
  (let ((new-subs (kmgu (apply-ksubstitution subs kind1)
                       (apply-ksubstitution subs kind2))))
    (compose-ksubstitution-lists new-subs subs)))

(defgeneric kmgu (kind1 kind2)
  (:method ((kind1 kstar) (kind2 kstar))
    (declare (values ksubstitution-list &optional))
    nil)

  (:method ((kind1 kyvar) (kind2 kind))
    (declare (values ksubstitution-list &optional))
    (list
     (make-ksubstitution
      :from kind1
      :to kind2)))

  (:method ((kind1 kind) (kind2 kyvar))
    (declare (values ksubstitution-list &optional))
    (list
     (make-ksubstitution
      :from kind2
      :to kind1)))

  (:method ((kind1 kfun) (kind2 kfun))
    (declare (values ksubstitution-list &optional))
    (nconc
     (kmgu (kfun-from kind1) (kfun-from kind2))
     (kmgu (kfun-to kind1) (kfun-to kind2))))

  (:method ((kind1 kind) (kind2 kind))
    (error 'kunify-error
           :kind1 kind1
           :kind2 kind2)))

;;;
;;; Operations on Kinds
;;;

(defun kind-return-type (kind)
  (declare (type kind kind)
           (values kind &optional))
  (if (kfun-p kind)
      (kind-return-type (kfun-to kind))
      kind))

(defun kind-variables (x)
  (declare (type t x)
           (values kyvar-list &optional))
  (remove-duplicates (kind-variables-generic% x) :test #'eq))

(defgeneric kind-variables-generic% (kind)
  (:method ((kind kstar))
    nil)

  (:method ((kind kyvar))
    (list kind))

  (:method ((kind kfun))
    (nconc
     (kind-variables (kfun-from kind))
     (kind-variables (kfun-to kind))))

  (:method ((list list))
    (mapcan #'kind-variables list)))

(defun kind-monomorphize-subs (kvars ksubs)
  (declare (type kyvar-list kvars)
           (values ksubstitution-list &optional))
  (compose-ksubstitution-lists
   (loop :for kvar :in kvars
         :collect (make-ksubstitution :from kvar :to +kstar+))
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

(defun make-kind-function* (from to)
  (declare (type kind-list from)
           (type kind to))
  (if (null from)
      to
      (make-kfun
       :from (car from)
       :to (make-kind-function* (cdr from) to))))

;;;
;;; Pretty printing
;;;

(defun pprint-kind (stream kind)
  (declare (type stream stream)
           (type kind kind))
  (etypecase kind
    (kstar
     (write-char #\* stream))
    (kfun
     (let ((from (kfun-from kind))
           (to (kfun-to kind)))
       (when (kfun-p from)
         (write-char #\( stream))
       (pprint-kind stream from)
       (when (kfun-p from)
         (write-char #\) stream))

       (write-string (if settings:*coalton-print-unicode*
                         " → "
                         " -> ")
                     stream)

       (when (kfun-p to)
         (write-char #\( stream))
       (pprint-kind stream to)
       (when (kfun-p to)
         (write-char #\) stream))))
    (kyvar
     (write-string "#K" stream)
     (write (kyvar-id kind) :stream stream)))

  nil)

(defmethod print-object ((kind kind) stream)
  (if *print-readably*
      (call-next-method)
      (pprint-kind stream kind)))


;;;
;;; Conditions
;;;

(define-condition kunify-error (error:coalton-internal-type-error)
  ((kind1 :initarg :kind1
          :reader kunify-errror-kind1
          :type kind)
   (kind2 :initarg :kind2
          :reader kunify-error-kind2
          :type kind))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Unable to unify kinds ~A and ~A"
               (kunify-errror-kind1 c)
               (kunify-error-kind2 c))))))
