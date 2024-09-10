(defpackage #:coalton-impl/parser/types
  (:use
   #:cl
   #:coalton-impl/source
   #:coalton-impl/parser/base)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:se #:source-error)
   (#:util #:coalton-impl/util))
  (:export
   #:ty                                 ; STRUCT
   #:ty-location                        ; ACCESSOR
   #:ty-list                            ; TYPE
   #:tyvar                              ; STRUCT
   #:make-tyvar                         ; CONSTRUCTOR
   #:tyvar-p                            ; FUNCTION
   #:tyvar-name                         ; ACCESSOR
   #:tyvar-list                         ; TYPE
   #:tycon                              ; STRUCT
   #:make-tycon                         ; CONSTRUCTOR
   #:tycon-p                            ; FUNCTION
   #:tycon-name                         ; ACCESSOR
   #:tycon-list                         ; TYPE
   #:tapp                               ; STRUCT
   #:make-tapp                          ; CONSTRUCTOR
   #:tapp-p                             ; FUNCTION
   #:tapp-from                          ; ACCESSOR
   #:tapp-to                            ; ACCESSOR
   #:ty-predicate                       ; STRUCT
   #:make-ty-predicate                  ; CONSTRUCTOR
   #:ty-predicate-class                 ; ACCESSOR
   #:ty-predicate-types                 ; ACCESSOR
   #:ty-predicate-location              ; ACCESSOR
   #:ty-predicate-list                  ; TYPE
   #:qualified-ty                       ; STRUCT
   #:make-qualified-ty                  ; CONSTRUCTOR
   #:qualified-ty-predicates            ; ACCESSOR
   #:qualified-ty-type                  ; ACCESSOR
   #:qualified-ty-location              ; ACCESSOR
   #:qualified-ty-list                  ; TYPE
   #:parse-qualified-type               ; FUNCTION
   #:parse-type                         ; FUNCTION
   #:parse-predicate                    ; FUNCTION
   ))

(in-package #:coalton-impl/parser/types)

;;;; # Type Parsing
;;;;
;;;; tyvar := <a keyword symbol>
;;;;
;;;; tycon := <a lisp symbol>
;;;;
;;;; class := <a lisp symbol>
;;;;
;;;; type-list := ty ty+
;;;;            | ty+ "->" type-list
;;;;
;;;; ty := tyvar
;;;;     | tycon
;;;;     | "(" type-list ")"
;;;;
;;;; ty-predicate := class ty+ 
;;;;
;;;; qualified-ty := ty
;;;;               | "(" ty-predicate "=>" type-list ")"
;;;;               | "(" ( "(" ty-predicate ")" )+ "=>" type-list ")"

(defstruct (ty (:constructor nil)
               (:copier nil))
  (location (util:required 'location) :type location :read-only t))

(defmethod location ((self ty))
  (ty-location self))

(defun ty-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-p x)))

(deftype ty-list ()
  '(satisfies ty-list-p))

(defstruct (tyvar (:include ty)
                  (:copier nil))
  (name (util:required 'name) :type keyword :read-only t))

(defun tyvar-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'tyvar-p x)))

(deftype tyvar-list ()
  '(satisfies tyvar-list-p))

(defstruct (tycon (:include ty)
                  (:copier nil))
  (name (util:required 'name) :type identifier :read-only t))

(defun tycon-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'tycon-p x)))

(deftype tycon-list ()
  '(satisfies tycon-list-p))

(defstruct (tapp (:include ty)
                 (:copier nil))
  ;; The type being applied to
  (from (util:required 'from) :type ty :read-only t)
  ;; The type argument
  (to   (util:required 'to)   :type ty :read-only t))

(defstruct (ty-predicate
            (:copier nil))
  (class    (util:required 'class)    :type identifier-src  :read-only t)
  (types    (util:required 'types)    :type ty-list         :read-only t)
  (location (util:required 'location) :type location        :read-only t))

(defmethod location ((self ty-predicate))
  (ty-predicate-location self))

(defun ty-predicate-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-predicate-p x)))

(deftype ty-predicate-list ()
  '(satisfies ty-predicate-list-p))

(defstruct (qualified-ty
            (:predicate nil)
            (:copier nil))
  (predicates (util:required 'predicates) :type ty-predicate-list :read-only t)
  (type       (util:required 'type)       :type ty                :read-only t)
  (location   (util:required 'location)   :type location          :read-only t))

(defmethod location ((self qualified-ty))
  (qualified-ty-location self))

(defun parse-qualified-type (form source)
  (declare (type cst:cst form))

  (if (cst:atom form)

      (make-qualified-ty
       :predicates nil
       :type (parse-type form source)
       :location (make-location source form))

      (multiple-value-bind (left right)
          (util:take-until (lambda (cst)
                             (and (cst:atom cst)
                                  (eq (cst:raw cst) 'coalton:=>)))
                           (cst:listify form))
        (cond
          ;; no predicates
          ((null right)
           (make-qualified-ty
            :predicates nil
            :type (parse-type-list left (make-location source form))
            :location (make-location source form)))

          ;; (=> T -> T)
          ((and (null left) right)
           (error 'parse-error
                  :err (se:source-error
                        :span (cst:source (cst:first form))
                        :source source
                        :message "Malformed type"
                        :primary-note "unnecessary `=>`"
                        :help-notes
                        (cond
                          ;; If this is the only thing in the list then don't suggest anything
                          ((cst:atom (cst:rest form))
                           nil)
                          ;; If there is nothing to the right of C then emit without list
                          ((cst:atom (cst:rest (cst:rest form)))
                           (list
                            (se:make-source-error-help
                             :span (cst:source form)
                             :replacement
                             (lambda (existing)
                               (subseq existing 4 (1- (length existing))))
                             :message "remove `=>`")))
                          (t
                           (list
                            (se:make-source-error-help
                             :span (cst:source form)
                             :replacement
                             (lambda (existing)
                               (concatenate 'string
                                            (subseq existing 0 1)
                                            (subseq existing 4)))
                             :message "remove `=>`")))))))

          ;; (... =>)
          ((null (rest right))
           (error 'parse-error
                  :err (se:source-error
                        :span (cst:source (cst:second form))
                        :source source
                        :message "Malformed type"
                        :primary-note "missing type after `=>`")))

          (t
           (let (predicates)
             (if (cst:atom (first left))
                 (setf predicates (list (parse-predicate
                                         left
                                         (make-location source
                                                        (cons (car (cst:source (first left)))
                                                              (cdr (cst:source (car (last left)))))))))

                 (loop :for pred :in left
                       :unless (cst:consp pred)
                         :do (error 'parse-error
                                    :err (se:source-error
                                          :span (cst:source (cst:second form))
                                          :source source
                                          :message "Malformed type predicate"
                                          :primary-note "expected predicate"))
                       :do (push (parse-predicate (cst:listify pred)
                                                  (make-location source
                                                                 (cst:source form)))
                                 predicates)))

             (make-qualified-ty
              :predicates (reverse predicates)
              :type (parse-type-list
                     (cdr right)
                     (make-location source
                                    (cons (car (cst:source (second right)))
                                          (cdr (cst:source (car (last right)))))))
              :location (make-location source form))))))))

(defun parse-predicate (forms location)
  (declare (type util:cst-list forms)
           (type location location)
           (values ty-predicate))

  (assert forms)
  (let ((source (location-source location)))
    (cond
      ;; (T) ... => ....
      ((not (cst:atom (first forms)))
       (error 'parse-error
              :err (se:source-error
                    :span (cst:source (first forms))
                    :source source
                    :message "Malformed type predicate"
                    :primary-note "expected class name"
                    :help-notes
                    (list
                     (se:make-source-error-help
                      :span (cst:source (first forms))
                      :replacement
                      (lambda (existing)
                        (subseq existing 1 (1- (length existing))))
                      :message "remove parentheses")))))

      ;; "T" ... => ...
      ((not (identifierp (cst:raw (first forms))))
       (error 'parse-error
              :err (se:source-error
                    :span (cst:source (first forms))
                    :source source
                    :message "Malformed type predicate"
                    :primary-note "expected identifier")))

      (t
       (let ((name (cst:raw (first forms))))
         (when (= 1 (length forms))
           (error 'parse-error
                  :err (se:source-error
                        :span (cst:source (first forms))
                        :source source
                        :message "Malformed type predicate"
                        :primary-note "expected predicate")))

         (make-ty-predicate
          :class (make-identifier-src
                  :name name
                  :location (make-location source (first forms)))
          :types (loop :for form :in (cdr forms)
                       :collect (parse-type form source))
          :location location))))))

(defun parse-type (form source)
  (declare (type cst:cst form)
           (values ty &optional))

  (cond
    ((and (cst:atom form)
          (symbolp (cst:raw form))
          (cst:raw form))

     (if (equalp (symbol-package (cst:raw form)) util:+keyword-package+)
         (make-tyvar :name (cst:raw form) :location (make-location source form))
         (make-tycon :name (cst:raw form) :location (make-location source form))))

    ((cst:atom form)
     (error 'parse-error
            :err (se:source-error
                  :span (cst:source form)
                  :source source
                  :message "Malformed type"
                  :primary-note "expected identifier")))

    ;; (T)
    ((cst:atom (cst:rest form))
     (error 'parse-error
            :err (se:source-error
                  :span (cst:source form)
                  :source source
                  :message "Malformed type"
                  :primary-note "unexpected nullary type")))

    (t
     (parse-type-list (cst:listify form) (make-location source form)))))

(defun parse-type-list (forms location)
  (declare (type util:cst-list forms)
           (type location location)
           (values ty &optional))

  (assert forms)

  (if (= 1 (length forms))
      (parse-type (first forms) (location-source location))
      (multiple-value-bind (left right)
          (util:take-until (lambda (cst)
                             (and (cst:atom cst)
                                  (eq (cst:raw cst) 'coalton:->)))
                           forms)

        ;; (T ... ->)
        (cond
          ((and right (null (rest right)))
           (error 'parse-error
                  :err (se:source-error
                        :span (cst:source (car right))
                        :source (location-source location)
                        :message "Malformed function type"
                        :primary-note "missing return type")))

          ;; (-> ...)
          ((and (null left) right)
           (error 'parse-error
                  :err (se:source-error
                        :span (cst:source (car right))
                        :source (location-source location)
                        :message "Malformed function type"
                        :primary-note "invalid function syntax")))

          (t
           (let ((ty (parse-type (car left) (location-source location))))
             (loop :for form_ :in (cdr left)
                   :for ty_ := (parse-type form_ (location-source location))
                   :do (setf ty (make-tapp :from ty
                                           :to ty_
                                           :location location)))

             (if (null right)
                 ty

                 (make-tapp
                  :from (make-tapp
                         :from (make-tycon
                                :name 'coalton:Arrow
                                :location (make-location (location-source location)
                                                         (first right)))
                         :to ty
                         :location (make-location (location-source location)
                                                  (cons (car (location-span (ty-location ty)))
                                                        (cdr (cst:source (first right))))))
                  :to (parse-type-list
                       (cdr right)
                       (make-location (location-source location)
                                      (cons (car (cst:source (first right)))
                                            (cdr (cst:source (car (last right)))))))
                  :location location))))))))
