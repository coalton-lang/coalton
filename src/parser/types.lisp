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
   #:tyvar-source-name                  ; ACCESSOR
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
   #:function-ty                        ; STRUCT
   #:make-function-ty                   ; CONSTRUCTOR
   #:function-ty-positional-input-types ; ACCESSOR
   #:function-ty-keyword-input-types    ; ACCESSOR
   #:function-ty-output-types           ; ACCESSOR
   #:result-ty                          ; STRUCT
   #:make-result-ty                     ; CONSTRUCTOR
   #:result-ty-output-types             ; ACCESSOR
   #:ty-predicate                       ; STRUCT
   #:make-ty-predicate                  ; CONSTRUCTOR
   #:ty-predicate-class                 ; ACCESSOR
   #:ty-predicate-types                 ; ACCESSOR
   #:ty-predicate-list                  ; TYPE
   #:qualified-ty                       ; STRUCT
   #:make-qualified-ty                  ; CONSTRUCTOR
   #:qualified-ty-explicit-p           ; ACCESSOR
   #:qualified-ty-explicit-variables   ; ACCESSOR
   #:qualified-ty-predicates            ; ACCESSOR
   #:qualified-ty-type                  ; ACCESSOR
   #:qualified-ty-list                  ; TYPE
   #:flatten-type                       ; FUNCTION
   #:parse-qualified-type               ; FUNCTION
   #:parse-type                         ; FUNCTION
   #:parse-lisp-return-type             ; FUNCTION
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
;;;; keyword-ty-entry := "(" keyword ty ")"
;;;;
;;;; type-list := ty ty+
;;;;            | function-input-spec "->" function-output-spec
;;;;
;;;; function-input-spec := "Void" | input-elements
;;;; input-elements := input-element | input-element "*" input-elements
;;;; input-element := ty
;;;;               | "&key" keyword-ty-entry+
;;;;
;;;; function-output-spec := "Void" | output-elements
;;;; output-elements := type-list | ty | ty "*" output-elements
;;;;
;;;; ty := tyvar
;;;;     | tycon
;;;;     | type-list-with-stars ; multi-output pack
;;;;     | "(" type-list ")"
;;;;
;;;; ty-predicate := class ty+
;;;;
;;;; qualified-ty := ty
;;;;               | "(" ty-predicate "=>" type-list ")"
;;;;               | "(" ( "(" ty-predicate ")" )+ "=>" type-list ")"
;;;;               | "(" ("forall" | "∀") "(" tyvar* ")" qualified-ty ")"
;;;;
;;;; Within a forall, the quantified body consumes the rest of the enclosing
;;;; list, so both `(forall (:a) (:a -> :a))` and `(forall (:a) :a -> :a)` are
;;;; accepted.

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
  (name        (util:required 'name)      :type keyword           :read-only t)
  ;; The original source spelling survives parser renaming and is reused for
  ;; later printing of programmer-written type variables.
  (source-name nil                        :type (or null keyword) :read-only t))

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

(defstruct (function-ty (:include ty)
                        (:copier nil))
  (positional-input-types (util:required 'positional-input-types) :type ty-list               :read-only t)
  (keyword-input-types    (util:required 'keyword-input-types)    :type keyword-ty-entry-list :read-only t)
  (output-types           (util:required 'output-types)           :type (or null ty-list)     :read-only t))

(defstruct (result-ty (:include ty)
                      (:copier nil))
  (output-types (util:required 'output-types) :type (or null ty-list) :read-only t))

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
  ;; True when the source type used an explicit FORALL binder list.
  (explicit-p         nil                 :type boolean           :read-only t)
  ;; The explicit FORALL binders in source order, including nested FORALLs
  ;; after flattening, with original source spellings preserved.
  (explicit-variables nil                 :type keyword-src-list  :read-only t)
  (predicates         (util:required 'predicates) :type ty-predicate-list :read-only t)
  (type               (util:required 'type)       :type ty                :read-only t)
  (location           (util:required 'location)   :type source:location   :read-only t))

(defmethod source:location ((self qualified-ty))
  (qualified-ty-location self))

(defun parse-forall-type-variable (form source)
  (declare (type cst:cst form)
           (values keyword-src &optional))

  (when (cst:consp form)
    (parse-error "Invalid type variable"
                 (note source form "expected keyword symbol")))

  (unless (keywordp (cst:raw form))
    (parse-error "Invalid type variable"
                 (note source form "expected keyword symbol")
                 (help source form
                       (lambda (existing)
                         (concatenate 'string ":" existing))
                       "add `:` to symbol")))

  (make-keyword-src
   :name (cst:raw form)
   :source-name (cst:raw form)
   :location (form-location source form)))

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

(defun keyword-marker-p (form)
  (and (cst:atom form)
       (eq 'coalton:&key (cst:raw form))))

(defun qualified-type-marker-p (form)
  (and (cst:atom form)
       (symbolp (cst:raw form))
       (string= (symbol-name (cst:raw form)) "=>")))

(defun star-marker-p (form)
  (and (cst:atom form)
       (symbolp (cst:raw form))
       (string= (symbol-name (cst:raw form)) "*")))

(defun empty-input-set-form-p (form)
  (or (cst:null form)
      (and (cst:atom form)
           (null (cst:raw form)))
      (and (cst:proper-list-p form)
           (null (cst:listify form)))))

(defun void-type-marker-p (form)
  (and (cst:atom form)
       (symbolp (cst:raw form))
       (string= (symbol-name (cst:raw form)) "VOID")))

(defun arrow-marker-p (form)
  (and (cst:atom form)
       (symbolp (cst:raw form))
       (string= (symbol-name (cst:raw form)) "->")))

(defun star-separated-type-form-p (form)
  (and (cst:proper-list-p form)
       (let ((forms (cst:listify form)))
         (and (find-if #'star-marker-p forms)
              (not (find-if #'arrow-marker-p forms))))))

(defun convention-wrapper-name (form)
  (and (cst:consp form)
       (cst:atom (cst:first form))
       (identifierp (cst:raw (cst:first form)))
       (symbol-name (cst:raw (cst:first form)))))

(defun unwrap-input-convention-wrapper (forms source)
  (declare (type util:cst-list forms)
           (values util:cst-list &optional))
  (if (and (= 1 (length forms))
           (cst:consp (first forms)))
      (let* ((wrapper (first forms))
             (wrapper-name (convention-wrapper-name wrapper)))
        (if wrapper-name
            (cond
              ((string= wrapper-name "COALTONFUNCTIONINPUTS")
               (cst:listify (cst:rest wrapper)))
              ((string= wrapper-name "COALTONFUNCTIONOUTPUTS")
               (parse-error "Malformed function type"
                            (note source wrapper
                                  "output convention wrapper is not valid on function input side")))
              (t forms))
            forms))
      forms))

(defun unwrap-output-convention-wrapper (forms source)
  (declare (type util:cst-list forms)
           (values util:cst-list &optional))
  (if (and (= 1 (length forms))
           (cst:consp (first forms)))
      (let* ((wrapper (first forms))
             (wrapper-name (convention-wrapper-name wrapper)))
        (if wrapper-name
            (cond
              ((string= wrapper-name "COALTONFUNCTIONOUTPUTS")
               (cst:listify (cst:rest wrapper)))
              ((string= wrapper-name "COALTONFUNCTIONINPUTS")
               (parse-error "Malformed function type"
                            (note source wrapper
                                  "input convention wrapper is not valid on function output side")))
              (t forms))
            forms))
      forms))

(defun parse-star-separated-type-list (forms source context allow-empty)
  (declare (type util:cst-list forms)
           (values ty-list &optional))
  (when (and (null forms) (not allow-empty))
    (parse-error (format nil "Malformed function type: expected one or more types in ~A" context)))
  (when (null forms)
    (return-from parse-star-separated-type-list nil))
  (labels ((segment-location (segment)
             (source:make-location source
                                   (cons (car (cst:source (car segment)))
                                         (cdr (cst:source (car (last segment))))))))
    (let ((segments nil)
          (current nil))
      (loop :for rest := forms :then (cdr rest)
            :while rest
            :for form := (car rest)
            :do
               (if (star-marker-p form)
                   (if current
                       (progn
                         (push (nreverse current) segments)
                         (setf current nil))
                       (parse-error "Malformed function type"
                                    (note source form "unexpected `*` in ~A" context)))
                   (push form current)))
      (unless current
        (parse-error "Malformed function type"
                     (note-end source (car (last forms))
                               "missing type after `*` in ~A" context)))
      (push (nreverse current) segments)
      (setf segments (nreverse segments))
      (let ((types
              (loop :for segment :in segments
                    :collect
                       (if (= 1 (length segment))
                           (parse-type (car segment) source)
                           (parse-type-list segment (segment-location segment))))))
        ;; Each component of a multi-value type must be a single value.
        ;; Void and nested multi-value packs are not valid components.
        (loop :for ty :in types
              :for segment :in segments
              :when (result-ty-p ty)
                :do (parse-error "Malformed type"
                                 (note source (car segment)
                                       "Void and multi-value types cannot appear as components of a multi-value type")))
        types))))

(defun parse-keyword-type-entry-list (forms source)
  (declare (type util:cst-list forms)
           (values keyword-ty-entry-list &optional))
  (let ((entries nil)
        (seen (make-hash-table :test #'eq)))
    (labels ((record-keyword (keyword-form)
               (let* ((keyword-name (cst:raw keyword-form))
                      (prev (gethash keyword-name seen)))
                 (when prev
                   (parse-error "Duplicate keyword type entry"
                                (secondary-note source prev "first entry here")
                                (note source keyword-form "second entry here")))
                 (setf (gethash keyword-name seen) keyword-form)
                 keyword-name))
             (emit-entry (keyword-form type-form location)
               (push (make-keyword-ty-entry
                      :keyword (make-keyword-src
                                :name (record-keyword keyword-form)
                                :location (form-location source keyword-form))
                      :type (parse-type type-form source)
                      :location location)
                     entries)))
      (loop :for rest := forms :then (cdr rest)
            :while rest
            :for entry := (car rest)
            :do
               (progn
                 (unless (and (cst:consp entry)
                              (cst:proper-list-p entry))
                   (parse-error "Malformed keyword type entry"
                                (note source entry "expected `(:keyword Type)`")))
                 (unless (cst:consp (cst:rest entry))
                   (parse-error "Malformed keyword type entry"
                                (note source entry "expected key and type")))
                 (when (cst:consp (cst:rest (cst:rest entry)))
                   (parse-error "Malformed keyword type entry"
                                (note source (cst:third entry) "unexpected trailing form")))
                 (unless (and (cst:atom (cst:first entry))
                              (keywordp (cst:raw (cst:first entry))))
                   (parse-error "Malformed keyword type entry"
                      (note source (cst:first entry) "expected keyword")))
                 (emit-entry (cst:first entry)
                             (cst:second entry)
                             (form-location source entry)))))
    (sort (nreverse entries)
          #'string<
          :key (lambda (entry)
                 (symbol-name (keyword-src-name
                               (keyword-ty-entry-keyword entry)))))))

(defun split-input-forms-keyword-tail (forms source)
  (declare (type util:cst-list forms)
           (values util:cst-list util:cst-list &optional))
  (let ((split nil)
        (tail nil)
        (saw-keyword-marker nil))
    (loop :for rest := forms :then (cdr rest)
          :while rest
          :for current := (car rest)
          :do
             (cond
               ((keyword-marker-p current)
                (when saw-keyword-marker
                  (parse-error "Malformed function type"
                               (note source current "invalid `&key` placement")))
                (setf saw-keyword-marker t)
                (setf split (nreverse split))
                (setf tail (cdr rest))
                (return))
               (t
                (push current split))))
    (unless saw-keyword-marker
      (setf split (nreverse split)))
    (values split (or tail nil))))

(defun parse-function-output-spec (output-forms-raw source location context)
  (declare (type util:cst-list output-forms-raw)
           (type source:location location)
           (type string context)
           (values (or null ty-list) &optional))
  (declare (ignore location))
  ;; `A -> B -> C` is right-associative in output position and means a function
  ;; returning a function, not additional positional arguments for the current function.
  (if (find-if #'arrow-marker-p output-forms-raw)
      (let* ((recursive-location
               (source:make-location source
                                     (cons (car (cst:source (car output-forms-raw)))
                                           (cdr (cst:source (car (last output-forms-raw)))))))
             (output-type (parse-type-list output-forms-raw recursive-location)))
        (list output-type))
      (let* ((output-forms (unwrap-output-convention-wrapper output-forms-raw source))
             (empty-output-syntax-p
               (and (= 1 (length output-forms))
                    (or (void-type-marker-p (first output-forms))
                        (empty-input-set-form-p (first output-forms)))))
             (output-types
               (if empty-output-syntax-p
                   (progn
                     (when (empty-input-set-form-p (first output-forms))
                       (parse-error "Malformed function type"
                                    (note source
                                          (first output-forms)
                                          "zero-value outputs must be written as `Void`")))
                     nil)
                   (parse-star-separated-type-list output-forms source context nil))))
        output-types)))

(defun parse-lisp-return-type (form source)
  (declare (type cst:cst form)
           (values (or null ty-list) &optional))
  (cond
    ((and (cst:proper-list-p form)
          (cst:consp form)
          (arrow-marker-p (cst:first form)))
     (let ((output-forms-raw (cst:listify (cst:rest form))))
       (when (null output-forms-raw)
         (parse-error "Malformed lisp return type"
                      (note source (cst:first form)
                            "missing return type")))
       (parse-function-output-spec output-forms-raw
                                   source
                                   (form-location source form)
                                   "lisp return type")))
    ((empty-input-set-form-p form)
     (parse-error "Malformed lisp return type"
                  (note source form
                        "zero-value `lisp` return types must be written as `(-> Void)`")))
    ((star-separated-type-form-p form)
     (parse-error "Malformed lisp return type"
                  (note source form
                        "use `(-> a * b)` for multi-value `lisp` return types")))
    ((and (cst:proper-list-p form)
          (find-if #'arrow-marker-p (cst:listify form)))
     (parse-error "Malformed lisp return type"
                  (note source (find-if #'arrow-marker-p (cst:listify form))
                        "nothing may appear to the left of `->` in a `lisp` return type")))
    (t
     (parse-error "Malformed lisp return type"
                  (note source form
                        "use `(-> ...)` return type syntax in `lisp` forms")))))

(defun build-fixed-arity-function-type (positional keyword-entries outputs location)
  (declare (type ty-list positional)
           (type keyword-ty-entry-list keyword-entries)
           (type (or null ty-list) outputs)
           (type source:location location)
           (values function-ty &optional))
  (make-function-ty
   :positional-input-types positional
   :keyword-input-types (sort (copy-list keyword-entries)
                              #'string<
                              :key (lambda (entry)
                                     (symbol-name (keyword-src-name
                                                   (keyword-ty-entry-keyword entry)))))
   :output-types outputs
   :location location))

(defun parse-qualified-type (form source)
  (declare (type cst:cst form))

  (when (and (cst:consp form)
             (cst:atom (cst:first form))
             (member (cst:raw (cst:first form))
                     '(coalton:forall coalton:∀)
                     :test #'eq))
    (unless (cst:consp (cst:rest form))
      (parse-error "Malformed forall type"
                   (note source form "expected quantified variables")))

    (unless (cst:consp (cst:rest (cst:rest form)))
      (parse-error "Malformed forall type"
                   (note source form "expected quantified type")))

    (unless (cst:proper-list-p (cst:second form))
      (parse-error "Malformed forall type"
                   (note source (cst:second form)
                         "expected list of quantified variables")))

    (let* ((body-form
             (if (cst:null (cst:rest (cst:nthrest 2 form)))
                 (cst:third form)
                 (make-instance 'cst:cons-cst
                                :raw (nthcdr 2 (cst:raw form))
                                :first (cst:third form)
                                :rest (cst:nthrest 3 form)
                                :source (cst:source form))))
           (body (parse-qualified-type
                  body-form
                  source))
           (explicit-variables
             (append
              (parse-list #'parse-forall-type-variable (cst:second form) source)
              (qualified-ty-explicit-variables body))))
      (return-from parse-qualified-type
        (make-qualified-ty
         :explicit-p t
         :explicit-variables explicit-variables
         :predicates (qualified-ty-predicates body)
         :type (qualified-ty-type body)
         :location (form-location source form)))))

  (if (cst:atom form)

      (make-qualified-ty
       :explicit-p nil
       :explicit-variables nil
       :predicates nil
       :type (parse-type form source)
       :location (form-location source form))

      (multiple-value-bind (left right)
          (util:take-until (lambda (cst)
                             (and (cst:atom cst)
                                  (qualified-type-marker-p cst)))
                           (cst:listify form))
        (cond
          ;; no predicates
          ((null right)
           (make-qualified-ty
            :explicit-p nil
            :explicit-variables nil
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
              :explicit-p nil
              :explicit-variables nil
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
    ((empty-input-set-form-p form)
     (parse-error "Malformed type"
                  (note source form
                        "zero-value type syntax must be written as `Void`")))

    ((void-type-marker-p form)
     (make-result-ty :output-types nil :location (form-location source form)))

    ((and (cst:atom form)
          (symbolp (cst:raw form))
          (cst:raw form))

     (if (equalp (symbol-package (cst:raw form)) util:+keyword-package+)
         (make-tyvar :name (cst:raw form)
                     :source-name (cst:raw form)
                     :location (form-location source form))
         (make-tycon :name (cst:raw form) :location (form-location source form))))

    ((cst:atom form)
     (parse-error "Malformed type"
                  (note source form "expected identifier")))

    ;; (T)
    ((cst:atom (cst:rest form))
     (parse-error "Malformed type"
                  (note source form "unexpected nullary type")))

    ;; Bare keyword tails are only valid as part of a function signature. If the
    ;; form itself contains an arrow, let PARSE-TYPE-LIST handle it as a nested
    ;; function type such as `(&key (:x Integer) -> Integer)`.
    ((and (keyword-marker-p (cst:first form))
          (not (find-if #'arrow-marker-p (cst:listify form))))
     (parse-error "Malformed function type"
                  (note source form
                        "keyword type tail must appear inside function input before `->`")))

    (t
     (parse-type-list (cst:listify form) (form-location source form)))))

(defun parse-type-list (forms location)
  (declare (type util:cst-list forms)
           (type source:location location)
           (values ty &optional))

  (assert forms)

  (let* ((source (source:location-source location))
         (arrow-forms (remove-if-not #'arrow-marker-p forms)))
    (cond
      ;; Non-function type application
      ((null arrow-forms)
       (cond
         ((find-if #'star-marker-p forms)
          (make-result-ty
           :output-types (parse-star-separated-type-list forms
                                                        source
                                                        "output pack"
                                                        nil)
           :location location))
         ((find-if #'keyword-marker-p forms)
          (parse-error "Malformed function type"
                       (note source
                             (find-if #'keyword-marker-p forms)
                             "`&key` type syntax is only valid in function signatures")))
         ((= 1 (length forms))
          (parse-type (first forms) source))
         (t
          (let ((left-ty (parse-type (car forms) source)))
            (loop :for form_ :in (cdr forms)
                  :for ty_ := (parse-type form_ source)
                  :do (setf left-ty (make-tapp :from left-ty
                                               :to ty_
                                               :location location)))
            left-ty))))

      (t
       (multiple-value-bind (left right)
           (util:take-until #'arrow-marker-p forms)

         ;; (-> T)
         (when (null left)
           (parse-error "Malformed function type"
                        (note source (car right)
                              "invalid function syntax")))

         ;; (T ... ->)
         (when (or (null right) (null (cdr right)))
           (parse-error "Malformed function type"
                        (note source (or (car right) (car (last forms)))
                              "missing return type")))

         (multiple-value-bind (input-forms keyword-forms)
             (split-input-forms-keyword-tail (unwrap-input-convention-wrapper left source)
                                             source)
           (let* ((void-input-syntax-p
                    (and (= 1 (length input-forms))
                         (void-type-marker-p (first input-forms))))
                  (_checked-nullary-input-syntax
                    (progn
                      (when (and void-input-syntax-p keyword-forms)
                        (parse-error "Malformed function type"
                                     (note source (first input-forms)
                                           "nullary input syntax `Void` cannot be combined with `&key`")))
                      (when (find-if #'empty-input-set-form-p input-forms)
                        (parse-error "Malformed function type"
                                     (note source
                                           (find-if #'empty-input-set-form-p input-forms)
                                           "nullary function inputs must be written as `Void`")))
                      (when (and (not void-input-syntax-p)
                                 (find-if #'void-type-marker-p input-forms))
                        (parse-error "Malformed function type"
                                     (note source
                                           (find-if #'void-type-marker-p input-forms)
                                           "unexpected `Void` in function input")))))
                  (input-types (if void-input-syntax-p
                                   nil
                                   (parse-star-separated-type-list input-forms
                                                                   source
                                                                   "function input"
                                                                   t)))
                  (keyword-types (parse-keyword-type-entry-list keyword-forms source))
                  (output-forms-raw (cdr right)))
             (declare (ignore _checked-nullary-input-syntax))
             (let ((output-types
                     (parse-function-output-spec output-forms-raw
                                                 source
                                                 location
                                                 "function output")))
               (build-fixed-arity-function-type input-types
                                               keyword-types
                                               output-types
                                               location)))))))))
