(defpackage #:coalton-impl/typechecker/kinds
  (:use
   #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error))
  (:export
   #:kind                               ; STRUCT
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
   #:kind-monomorphize-subs             ; FUNCTION
   #:simple-kind-p                      ; FUNCTION
   #:kind-arity                         ; FUNCTION
   #:make-kind-function*                ; FUNCTION
   #:kfun-p                             ; FUNCTION
   #:kstar-p                            ; FUNCTION
   #:*coalton-print-unicode*            ; VARIABLE
   #:kunify-error                       ; CONDITION
   ))

(in-package #:coalton-impl/typechecker/kinds)

;;;
;;; Kinds
;;;

(defstruct (kind (:constructor nil)))

(defstruct (kstar (:include kind)))

(defmethod make-load-form ((self kstar) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type kstar))

(alexandria:define-constant +kstar+ (make-instance 'kstar) :test #'equalp)

(defstruct (kfun (:include kind))
  (from (util:required 'from) :type kind :read-only t)
  (to   (util:required 'to)   :type kind :read-only t))

(defmethod make-load-form ((self kfun) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type kfun))

(defstruct (kyvar (:include kind)) 
  (id (util:required 'id) :type fixnum :read-only t))

(defmethod make-load-form ((self kyvar) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type kyvar))

(defun kyvar-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'kyvar-p x)))

(deftype kyvar-list ()
  '(satisfies kyvar-list-p))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type kind))

(defvar *next-kvar-id* 0)

#+sbcl
(declaim (sb-ext:always-bound *next-kvar-id*))

(declaim (inline make-kvariable))
(defun make-kvariable ()
  (prog1 (make-kyvar :id *next-kvar-id*)
    (incf *next-kvar-id*)))

(defstruct ksubstitution 
  (from (util:required 'from) :type kyvar :read-only t)
  (to   (util:required 'to)   :type kind  :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type ksubstitution))

(defun ksubstitution-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ksubstitution-p x)))

(deftype ksubstitution-list ()
  '(satisfies ksubstitution-list-p))

(defun compose-ksubstitution-lists (s1 s2)
  (declare (type ksubstitution-list s1 s2)
           (values ksubstitution-list))
  (append
   (mapcar
    (lambda (s)
      (make-ksubstitution
       :from (ksubstitution-from s)
       :to (apply-ksubstitution s1 (ksubstitution-to s))))
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

  (:method ((kind kyvar))
    (list kind))

  (:method ((kind kfun))
    (append
     (kind-variables (kfun-from kind))
     (kind-variables (kfun-to kind)))))

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
  (if (null from)
      to
      (make-kfun
       :from (car from)
       :to (make-kind-function* (cdr from) to))))

;;;
;;; Pretty printing
;;;

(defvar *coalton-print-unicode* t
  "Whether to print coalton info using unicode symbols")

(defun pprint-kind (stream kind &optional colon-p at-sign-p)
  (declare (type stream stream)
           (type kind kind)
           (ignore colon-p)
           (ignore at-sign-p)
           (values kind))
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

       (write-string (if *coalton-print-unicode*
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
  kind)

(set-pprint-dispatch 'kind 'pprint-kind)


;;;
;;; Conditions
;;;

(define-condition kunify-error (error:coalton-type-error)
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
