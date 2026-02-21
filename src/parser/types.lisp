(defpackage #:coalton-impl/parser/types
  (:use
   #:cl
   #:coalton-impl/parser/base)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util))
  (:export
   #:ty                                 ; STRUCT
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
   #:keyword-ty-entry                   ; STRUCT
   #:make-keyword-ty-entry              ; CONSTRUCTOR
   #:keyword-ty-entry-keyword           ; ACCESSOR
   #:keyword-ty-entry-type              ; ACCESSOR
   #:keyword-ty-entry-list              ; TYPE
   #:keyword-stage-ty                   ; STRUCT
   #:make-keyword-stage-ty              ; CONSTRUCTOR
   #:keyword-stage-ty-entries           ; ACCESSOR
   #:keyword-stage-ty-to                ; ACCESSOR
   #:ty-predicate                       ; STRUCT
   #:make-ty-predicate                  ; CONSTRUCTOR
   #:ty-predicate-class                 ; ACCESSOR
   #:ty-predicate-types                 ; ACCESSOR
   #:ty-predicate-list                  ; TYPE
   #:qualified-ty                       ; STRUCT
   #:make-qualified-ty                  ; CONSTRUCTOR
   #:qualified-ty-predicates            ; ACCESSOR
   #:qualified-ty-type                  ; ACCESSOR
   #:qualified-ty-list                  ; TYPE
   #:flatten-type                       ; FUNCTION
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
;;;; keyword-ty-entry := keyword ty
;;;;                  | "(" keyword ty ")"
;;;;
;;;; keyword-stage-ty := "(" "&key" keyword-ty-entry+ ")"
;;;;
;;;; ty := tyvar
;;;;     | tycon
;;;;     | keyword-stage-ty
;;;;     | "(" type-list ")"
;;;;
;;;; ty-predicate := class ty+
;;;;
;;;; qualified-ty := ty
;;;;               | "(" ty-predicate "=>" type-list ")"
;;;;               | "(" ( "(" ty-predicate ")" )+ "=>" type-list ")"

(defstruct (ty (:constructor nil)
               (:copier nil))
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self ty))
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

(defstruct (keyword-ty-entry
            (:copier nil))
  "A single keyword/type pair inside an `&key` type stage."
  (keyword  (util:required 'keyword)  :type keyword-src     :read-only t)
  (type     (util:required 'type)     :type ty              :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self keyword-ty-entry))
  (keyword-ty-entry-location self))

(defun keyword-ty-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'keyword-ty-entry-p x)))

(deftype keyword-ty-entry-list ()
  '(satisfies keyword-ty-entry-list-p))

(defstruct (keyword-stage-ty (:include ty)
                             (:copier nil))
  "A keyword argument stage: `(&key ...)` with an optional continuation type."
  (entries (util:required 'entries) :type keyword-ty-entry-list :read-only t)
  (to      nil                      :type (or null ty)           :read-only t))

(defun keyword-marker-p (form)
  (and (cst:atom form)
       (symbolp (cst:raw form))
       (string= (symbol-name (cst:raw form)) "&KEY")))

(defstruct (ty-predicate
            (:copier nil))
  (class    (util:required 'class)    :type identifier-src  :read-only t)
  (types    (util:required 'types)    :type ty-list         :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self ty-predicate))
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
  (location   (util:required 'location)   :type source:location   :read-only t))

(defmethod source:location ((self qualified-ty))
  (qualified-ty-location self))

(defun flatten-type (type)
  "If TYPE is a TAPP of the form ((((T1 T2) T3) T4) ...), then return
the list (T1 T2 T3 T4 ...). Otherwise, return (LIST TYPE)."
  (declare (type ty type)
           (values ty-list &optional))
  (let ((flattened-type nil))
    (loop :for from := type :then (tapp-from from)
          :while (typep from 'tapp)
          :do (push (tapp-to from) flattened-type)
          :finally (push from flattened-type))
    flattened-type))

(defun parse-qualified-type (form source)
  (declare (type cst:cst form))

  (if (cst:atom form)

      (make-qualified-ty
       :predicates nil
       :type (parse-type form source)
       :location (form-location source form))

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
            :type (parse-type-list left (form-location source form))
            :location (form-location source form)))

          ;; (=> T -> T)
          ((and (null left) right)
           (apply #'parse-error "Malformed type"
                  (cons (note source (cst:first form) "unnecessary `=>`")
                        (cond
                          ;; If this is the only thing in the list then don't suggest anything
                          ((cst:atom (cst:rest form))
                           nil)
                          ;; If there is nothing to the right of C then emit without list
                          ((cst:atom (cst:rest (cst:rest form)))
                           (list (help source form
                                       (lambda (existing)
                                         (subseq existing 4 (1- (length existing))))
                                       "remove `=>`")))
                          (t
                           (list (help source form
                                       (lambda (existing)
                                         (concatenate 'string
                                                      (subseq existing 0 1)
                                                      (subseq existing 4)))
                                       "remove `=>`")))))))

          ;; (... =>)
          ((null (rest right))
           (parse-error "Malformed type"
                        (note source (cst:source (cst:second form))
                              "missing type after `=>`")))

          (t
           (let (predicates)
             (if (cst:atom (first left))
                 (setf predicates (list (parse-predicate
                                         left
                                         (source:make-location source
                                                               (cons (car (cst:source (first left)))
                                                                     (cdr (cst:source (car (last left)))))))))

                 (loop :for pred :in left
                       :unless (cst:consp pred)
                         :do (parse-error "Malformed type predicate"
                                          (note source (cst:second form)
                                                "expected predicate"))
                       :do (push (parse-predicate (cst:listify pred)
                                                  (form-location source form))
                                 predicates)))

             (make-qualified-ty
              :predicates (reverse predicates)
              :type (parse-type-list
                     (cdr right)
                     (source:make-location source
                                           (cons (car (cst:source (second right)))
                                                 (cdr (cst:source (car (last right)))))))
              :location (form-location source form))))))))

(defun parse-predicate (forms location)
  (declare (type util:cst-list forms)
           (type source:location location)
           (values ty-predicate))

  (assert forms)
  (let ((source (source:location-source location)))
    (cond
      ;; (T) ... => ....
      ((not (cst:atom (first forms)))
       (parse-error "Malformed type predicate"
                    (note source (first forms)
                          "expected class name")
                    (help source (first forms)
                          (lambda (existing)
                            (subseq existing 1 (1- (length existing))))
                          "remove parentheses")))

      ;; "T" ... => ...
      ((not (identifierp (cst:raw (first forms))))
       (parse-error "Malformed type predicate"
                    (note source (first forms) "expected identifier")))

      (t
       (let ((name (cst:raw (first forms))))
         (when (= 1 (length forms))
           (parse-error "Malformed type predicate"
                        (note source (first forms)
                              "expected predicate")))

         (make-ty-predicate
          :class (make-identifier-src
                  :name name
                  :source-name (source:extract-source-text source (cst:source (first forms)))
                  :location (form-location source (first forms)))
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
         (make-tyvar :name (cst:raw form) :location (form-location source form))
         (make-tycon :name (cst:raw form) :location (form-location source form))))

    ((cst:atom form)
     (parse-error "Malformed type"
                  (note source form "expected identifier")))

    ;; (T)
    ((cst:atom (cst:rest form))
     (parse-error "Malformed type"
                  (note source form "unexpected nullary type")))

    ;; (&key ...)
    ((keyword-marker-p (cst:first form))
     (let ((entries nil)
           (seen (make-hash-table :test #'eq))
           (entries-forms (cst:rest form)))
       (unless (cst:consp (cst:rest form))
         (parse-error "Malformed keyword type"
                      (note source form "expected keyword entries")))

       (labels ((check-and-record-keyword-name (keyword-form)
                  (let* ((keyword-name (cst:raw keyword-form))
                         (prev (gethash keyword-name seen)))
                    (when prev
                      (parse-error "Duplicate keyword type entry"
                                   (secondary-note source prev "first entry here")
                                   (note source keyword-form "second entry here")))
                    (setf (gethash keyword-name seen) keyword-form)
                    keyword-name))
                (push-entry (keyword-form type-form location)
                  (let ((keyword-name (check-and-record-keyword-name keyword-form)))
                    (push
                     (make-keyword-ty-entry
                      :keyword (make-keyword-src
                                :name keyword-name
                                :location (form-location source keyword-form))
                      :type (parse-type type-form source)
                      :location location)
                     entries))))
         (loop :while (cst:consp entries-forms)
               :for entry := (cst:first entries-forms)
               :do
                  (cond
                    ;; Flat syntax: (&key :timeout Integer ...)
                    ((and (cst:atom entry)
                          (keywordp (cst:raw entry)))
                     (unless (cst:consp (cst:rest entries-forms))
                       (parse-error "Malformed keyword type entry"
                                    (note source entry "missing type after keyword")))
                     (let* ((keyword-form entry)
                            (type-form (cst:second entries-forms))
                            (entry-location
                              (source:make-location source
                                                    (cons (car (cst:source keyword-form))
                                                          (cdr (cst:source type-form))))))
                       (push-entry keyword-form type-form entry-location)
                       (setf entries-forms (cst:rest (cst:rest entries-forms)))))

                    ;; Legacy syntax: (&key (:timeout Integer) ...)
                    (t
                     (unless (and (cst:consp entry)
                                  (cst:proper-list-p entry))
                       (parse-error "Malformed keyword type entry"
                                    (note source entry "expected `:keyword Type` or `(:keyword Type)`")))
                     (unless (cst:consp (cst:rest entry))
                       (parse-error "Malformed keyword type entry"
                                    (note source entry "expected key and type")))
                     (when (cst:consp (cst:rest (cst:rest entry)))
                       (parse-error "Malformed keyword type entry"
                                    (note source (cst:first (cst:rest (cst:rest entry)))
                                          "unexpected trailing form")))
                     (unless (and (cst:atom (cst:first entry))
                                  (keywordp (cst:raw (cst:first entry))))
                       (parse-error "Malformed keyword type entry"
                                    (note source (cst:first entry) "expected keyword")))
                     (push-entry (cst:first entry)
                                 (cst:second entry)
                                 (form-location source entry))
                     (setf entries-forms (cst:rest entries-forms))))))

       (unless (cst:null entries-forms)
         (parse-error "Malformed keyword type"
                      (note source entries-forms "unexpected dotted list")))

       (make-keyword-stage-ty
        :entries (reverse entries)
        :location (form-location source form))))

    (t
     (parse-type-list (cst:listify form) (form-location source form)))))

(defun parse-type-list (forms location)
  (declare (type util:cst-list forms)
           (type source:location location)
           (values ty &optional))

  (assert forms)

  (if (= 1 (length forms))
      (let ((ty (parse-type (first forms) (source:location-source location))))
        (when (typep ty 'keyword-stage-ty)
          (parse-error "Malformed function type"
                       (note (source:location-source location)
                             (first forms)
                             "keyword stage must be followed by `->` and a return type")))
        ty)
      (multiple-value-bind (left right)
          (util:take-until (lambda (cst)
                             (and (cst:atom cst)
                                  (eq (cst:raw cst) 'coalton:->)))
                           forms)

        ;; (T ... ->)
        (cond
          ((and right (null (rest right)))
           (parse-error "Malformed function type"
                        (note (source:location-source location) (car right)
                              "missing return type")))

          ;; (-> ...)
          ((and (null left) right)
           (parse-error "Malformed function type"
                        (note (source:location-source location) (car right)
                              "invalid function syntax")))

          (t
           (let ((left-ty (parse-type (car left) (source:location-source location))))
             (loop :for form_ :in (cdr left)
                   :for ty_ := (parse-type form_ (source:location-source location))
                   :do
                      (when (typep left-ty 'keyword-stage-ty)
                        (parse-error "Malformed function type"
                                     (note (source:location-source location) form_
                                           "keyword stage cannot be used in type application")))
                      (setf left-ty (make-tapp :from left-ty
                                               :to ty_
                                               :location location)))

             (if (null right)
                 left-ty
                 (let ((to-ty
                         (parse-type-list
                          (cdr right)
                          (source:make-location (source:location-source location)
                                                (cons (car (cst:source (first right)))
                                                      (cdr (cst:source (car (last right)))))))))
                   (if (typep left-ty 'keyword-stage-ty)
                       (make-keyword-stage-ty
                        :entries (keyword-stage-ty-entries left-ty)
                        :to to-ty
                        :location (source:location left-ty))
                       (make-tapp
                        :from (make-tapp
                               :from (make-tycon
                                      :name 'coalton:Arrow
                                      :location (form-location (source:location-source location)
                                                               (first right)))
                               :to left-ty
                               :location (source:make-location (source:location-source location)
                                                               (cons (car (source:location-span (ty-location left-ty)))
                                                                     (cdr (cst:source (first right))))))
                        :to to-ty
                        :location location))))))))))
