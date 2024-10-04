(defpackage #:coalton-impl/parser/toplevel
  (:use
   #:cl
   #:coalton-impl/parser/base
   #:coalton-impl/parser/reader
   #:coalton-impl/parser/types
   #:coalton-impl/parser/pattern
   #:coalton-impl/parser/macro
   #:coalton-impl/parser/expression)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:cursor #:coalton-impl/parser/cursor)
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util))
  (:export
   #:attribute                                   ; TYPE
   #:attribute-monomorphize                      ; STRUCT
   #:make-attribute-monomorphize                 ; CONSTRUCTOR
   #:attribute-repr                              ; STRUCT
   #:make-attribute-repr                         ; CONSTRUCTOR
   #:attribute-repr-type                         ; ACCESSOR
   #:attribute-repr-arg                          ; ACCESSOR
   #:constructor                                 ; STRUCT
   #:make-constructor                            ; CONSTRUCTOR
   #:constructor-name                            ; ACCESSOR
   #:constructor-fields                          ; ACCESSOR
   #:constructor-list                            ; TYPE
   #:toplevel-define-type                        ; STRUCT
   #:make-toplevel-define-type                   ; CONSTRUCTOR
   #:toplevel-define-type-name                   ; ACCESSOR
   #:toplevel-define-type-vars                   ; ACCESSOR
   #:toplevel-define-type-ctors                  ; ACCESSOR
   #:toplevel-define-type-repr                   ; ACCESSOR
   #:toplevel-define-type-head-location          ; ACCESSOR
   #:toplevel-define-type-list                   ; TYPE
   #:toplevel-define-type-alias                  ; STRUCT
   #:make-toplevel-define-type-alias             ; CONSTRUCTOR
   #:toplevel-define-type-alias-name             ; ACCESSOR
   #:toplevel-define-type-alias-vars             ; ACCESSOR
   #:toplevel-define-type-alias-type             ; ACCESSOR
   #:toplevel-define-type-alias-head-location    ; ACCESSOR
   #:toplevel-define-type-alias-list             ; TYPE
   #:struct-field                                ; STRUCT
   #:make-struct-field                           ; CONSTRUCTOR
   #:struct-field-name                           ; ACCESSOR
   #:struct-field-type                           ; ACCESSOR
   #:struct-field-list                           ; TYPE
   #:toplevel-define-struct                      ; STRUCT
   #:make-toplevel-define-struct                 ; CONSTRUCTOR
   #:toplevel-define-struct-name                 ; ACCESSOR
   #:toplevel-define-struct-vars                 ; ACCESSOR
   #:toplevel-define-struct-fields               ; ACCESSOR
   #:toplevel-define-struct-repr                 ; ACCESSOR
   #:toplevel-define-struct-head-location        ; ACCESSOR
   #:toplevel-define-struct-list                 ; TYPE
   #:toplevel-declare                            ; STRUCT
   #:make-toplevel-declare                       ; CONSTRUCTOR
   #:toplevel-declare-name                       ; ACCESSOR
   #:toplevel-declare-type                       ; ACCESSOR
   #:toplevel-declare-list                       ; TYPE
   #:toplevel-declare-monomorphize               ; ACCESSOR
   #:toplevel-declare-inline                     ; ACCESSOR
   #:toplevel-define                             ; STRUCT
   #:make-toplevel-define                        ; CONSTRUCTOR
   #:toplevel-define-name                        ; ACCESSOR
   #:toplevel-define-params                      ; ACCESSOR
   #:toplevel-define-orig-params                 ; ACCESSOR
   #:toplevel-define-body                        ; ACCESSOR
   #:toplevel-define-monomorphize                ; ACCESSOR
   #:toplevel-define-list                        ; TYPE
   #:toplevel-define-inline                      ; ACCESSOR
   #:fundep                                      ; STRUCT
   #:make-fundep                                 ; CONSTRUCTOR
   #:fundep-left                                 ; ACCESSOR
   #:fundep-right                                ; ACCESSOR
   #:fundep-list                                 ; TYPE
   #:method-definition                           ; STRUCT
   #:make-method-definition                      ; STRUCT
   #:method-definition-name                      ; ACCESSOR
   #:method-definition-type                      ; ACCESSOR
   #:method-definition-list                      ; TYPE
   #:toplevel-define-class                       ; STRUCT
   #:make-toplevel-define-class                  ; CONSTRUCTOR
   #:toplevel-define-class-name                  ; ACCESSOR
   #:toplevel-define-class-vars                  ; ACCESSOR
   #:toplevel-define-class-preds                 ; ACCESSOR
   #:toplevel-define-class-fundeps               ; ACCESSOR
   #:toplevel-define-class-methods               ; ACCESSOR
   #:toplevel-define-class-head-location         ; ACCESSOR
   #:toplevel-define-class-list                  ; TYPE
   #:instance-method-definition                  ; STRUCT
   #:make-instance-method-definition             ; CONSTRUCTOR
   #:instance-method-definition-name             ; ACCESSOR
   #:instance-method-definition-params           ; ACCESSOR
   #:instance-method-definition-body             ; ACCESSOR
   #:instance-method-definition-inline           ; ACCESSOR
   #:instance-method-definition-list             ; TYPE
   #:toplevel-define-instance                    ; STRUCT
   #:make-toplevel-define-instance               ; CONSTRUCTOR
   #:toplevel-define-instance-context            ; ACCESSOR
   #:toplevel-define-instance-pred               ; ACCESSOR
   #:toplevel-define-instance-methods            ; ACCESSOR
   #:toplevel-define-instance-head-location      ; ACCESSOR
   #:toplevel-define-instance-compiler-generated ; ACCESSOR
   #:toplevel-define-instance-list               ; TYPE
   #:toplevel-package-name                       ; ACCESSOR
   #:toplevel-lisp-form                          ; STRUCT
   #:make-toplevel-lisp-form                     ; CONSTRUCTOR
   #:toplevel-lisp-form-body                     ; ACCESSOR
   #:toplevel-lisp-form-list                     ; TYPE
   #:toplevel-specialize                         ; STRUCT
   #:make-toplevel-specialize                    ; CONSTRUCTOR
   #:toplevel-specialize-from                    ; ACCESSOR
   #:toplevel-specialize-to                      ; ACCESSOR
   #:toplevel-specialize-type                    ; ACCESSOR
   #:toplevel-specialize-list                    ; TYPE
   #:program                                     ; STRUCT
   #:make-program                                ; CONSTRUCTOR
   #:program-package                             ; ACCESSOR
   #:program-lisp-forms                          ; ACCESSOR
   #:program-types                               ; ACCESSOR
   #:program-type-aliases                        ; ACCESSOR
   #:program-structs                             ; ACCESSOR
   #:program-declares                            ; ACCESSOR
   #:program-defines                             ; ACCESSOR
   #:program-classes                             ; ACCESSOR
   #:program-instances                           ; ACCESSOR
   #:program-specializations                     ; ACCESSOR
   #:program-lisp-package                        ; FUNCTION
   #:make-defpackage                             ; FUNCTION
   #:parse-toplevel-form                         ; FUNCTION
   #:read-program                                ; FUNCTION
   #:read-expression                             ; FUNCTION
   #:read-expressions                            ; FUNCTION
   ))

(in-package #:coalton-impl/parser/toplevel)

;;;; # Toplevel Form Parsing
;;;;
;;;; identifier := <a lisp symbol>
;;;;
;;;; keyword := <a lisp keyword symbol>
;;;;
;;;; ty := <defined in src/parser/types.lisp>
;;;;
;;;; ty-predicate := <defined in src/parser/types.lisp>
;;;;
;;;; qualified-ty := <defined in src/parser/types.lisp>
;;;;
;;;; node-body := <defined in src/parser/expression.lisp>
;;;;
;;;; lisp-form := <an arbitrary lisp form>
;;;;
;;;; docstring := <a lisp string>
;;;;
;;;; attribute-monomorphize := "(" "monomorphize" ")"
;;;;
;;;; attribute-inline := "(" "inline" ")"
;;;;
;;;; attribute-repr := "(" "repr" ( ":enum" | ":lisp" | ":transparent" ) ")"
;;;;                 | "(" "repr" ":native" lisp-form ")"
;;;;
;;;; toplevel := attribute-monomorphize? toplevel-declare
;;;;           | attribute-monomorphize? toplevel-define
;;;;           | attribute-repr? toplevel-define-type
;;;;           | toplevel-define-class
;;;;           | toplevel-define-instance
;;;;           | toplevel-specialize
;;;;
;;;; toplevel-declare := "(" "declare" identifier qualified-ty ")"
;;;;
;;;; toplevel-define := "(" "define" identifier node-body ")"
;;;;                  | "(" "define" "(" identifier pattern* ")" docstring? node-body ")"
;;;;
;;;; constructor := identifier docstring?
;;;;              | "(" identifier ")" docstring?
;;;;              | "(" identifier ty+ ")" docstring?
;;;;
;;;; toplevel-define-type := "(" "define-type" identifier docstring? constructor* ")"
;;;;                       | "(" "define-type" "(" identifier keyword+ ")" docstring? constructor* ")"
;;;;
;;;; toplevel-define-type-alias := "(" "define-type-alias" identifier ty docstring? ")"
;;;;                             | "(" "define-type-alias" "(" identifier keyword+ ")" ty docstring? ")"
;;;;
;;;; struct-field := "(" identifier docstring? type ")"
;;;;
;;;; toplevel-define-struct := "(" "define-struct" identifier docstring? struct-field* ")"
;;;;                         | "(" "define-struct" "(" identifier keyword+ ")" docstring? struct-field* ")"
;;;;
;;;; method-definition := "(" identifier docstring? qualified-ty ")"
;;;;
;;;; class-head := identifier keyword+
;;;;
;;;; toplevel-define-class := "(" "define-class" "(" class-head ")" docstring? method-definition* ")"
;;;;                        | "(" "define-class" "(" ty-predicate "=>" class-head ")" docstring? method-definition* ")"
;;;;                        | "(" "define-class" "(" ( "(" ty-predicate ")" )+ "=>" class-head ")" docstring? method-definition* ")"
;;;;
;;;; instance-method-definiton := "(" "define" identifier body ")"
;;;;                            | "(" "define" "(" identifier pattern* ")" body ")"
;;;;
;;;; toplevel-define-instance := "(" "define-instance" "(" ty-predicate ")" docstring? instance-method-definition* ")"
;;;;                           | "(" "define-instance" "(" ty-predicate "=>" ty-predicate ")" docstring? instance-method-definition ")"
;;;;                           | "(" "define-instance" "(" ( "(" ty-predicate ")" )+ "=>" ty-predicate ")" docstring? instance-method-definition+ ")"
;;;;
;;;; toplevel-lisp-form := "(" "lisp-toplevel" "(" ")" lisp-form* ")"
;;;;
;;;; toplevel-specialize := "(" identifier identifier ty ")"

;;
;; Attributes
;;

(defstruct (attribute
            (:constructor nil)
            (:copier nil))
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self attribute))
  (attribute-location self))

(defstruct (attribute-monomorphize
            (:include attribute)))

(defstruct (attribute-repr
            (:include attribute))
  (type (util:required 'type) :type keyword-src       :read-only t)
  (arg  (util:required 'arg)  :type (or null cst:cst) :read-only t))

(defstruct (attribute-inline
            (:include attribute)))

;;
;; Toplevel Structures
;;

(defstruct (constructor
            (:copier nil))
  (name      (util:required 'name)      :type identifier-src   :read-only t)
  (fields    (util:required 'fields)    :type ty-list          :read-only t)
  (docstring (util:required 'docstring) :type (or null string) :read-only t)
  (location  (util:required 'location)  :type source:location  :read-only t))

(defmethod source:location ((self constructor))
  (constructor-location self))

(defmethod source:docstring ((self constructor))
  (constructor-docstring self))

(defun constructor-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'constructor-p x)))

(deftype constructor-list ()
  '(satisfies constructor-list-p))

(defstruct (toplevel-definition
            (:constructor nil))
  (location  (util:required 'location)  :type source:location  :read-only t)
  (docstring (util:required 'docstring) :type (or null string) :read-only t))

(defmethod source:location ((self toplevel-definition))
  (toplevel-definition-location self))

(defmethod source:docstring ((self toplevel-definition))
  (toplevel-definition-docstring self))

(defstruct (toplevel-define-type
            (:include toplevel-definition)
            (:copier nil))
  (name          (util:required 'name)          :type identifier-src           :read-only t)
  (vars          (util:required 'vars)          :type keyword-src-list         :read-only t)
  (ctors         (util:required 'ctors)         :type constructor-list         :read-only t)
  (repr          (util:required 'repr)          :type (or null attribute-repr) :read-only nil)
  (head-location (util:required 'head-location) :type source:location          :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-define-type-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-define-type-p x))))

(deftype toplevel-define-type-list ()
  '(satisfies toplevel-define-type-list-p))

(defstruct (toplevel-define-type-alias
            (:include toplevel-definition)
            (:copier nil))
  (name          (util:required 'name)          :type identifier-src           :read-only t)
  (vars          (util:required 'vars)          :type keyword-src-list         :read-only t)
  (type          (util:required 'type)          :type ty                       :read-only t)
  (head-location (util:required 'head-location) :type source:location          :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-define-type-alias-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-define-type-alias-p x))))

(deftype toplevel-define-type-alias-list ()
  '(satisfies toplevel-define-type-alias-list-p))

(defstruct (struct-field
            (:include toplevel-definition)
            (:copier nil))
  (name (util:required 'name) :type string :read-only t)
  (type (util:required 'type) :type ty     :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun struct-field-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'struct-field-p x))))

(deftype struct-field-list ()
  '(satisfies struct-field-list-p))

(defstruct (toplevel-define-struct
            (:include toplevel-definition)
            (:copier nil))
  (name          (util:required 'name)          :type identifier-src           :read-only t)
  (vars          (util:required 'vars)          :type keyword-src-list         :read-only t)
  (fields        (util:required 'fields)        :type struct-field-list        :read-only t)
  (repr          (util:required 'repr)          :type (or null attribute-repr) :read-only nil)
  (head-location (util:required 'head-location) :type source:location          :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-define-struct-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-define-struct-p x))))

(deftype toplevel-define-struct-list ()
  '(satisfies toplevel-define-struct-list-p))

(defstruct (toplevel-declare
            (:copier nil))
  (name         (util:required 'name)         :type identifier-src                   :read-only t)
  (type         (util:required 'type)         :type qualified-ty                     :read-only t)
  (location     (util:required 'location)     :type source:location                  :read-only t)
  (monomorphize (util:required 'monomorphize) :type (or null attribute-monomorphize) :read-only nil)
  (inline       (util:required 'inline  )     :type (or null attribute-inline)       :read-only nil))

(defmethod source:location ((self toplevel-declare))
  (toplevel-declare-location self))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-declare-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-declare-p x))))

(deftype toplevel-declare-list ()
  '(satisfies toplevel-declare-list-p))

(defstruct (toplevel-define
            (:include toplevel-definition)
            (:copier nil))
  (name         (util:required 'name)         :type node-variable                    :read-only t)
  (params       (util:required 'params)       :type pattern-list                     :read-only t)
  (orig-params  (util:required 'orig-params)  :type pattern-list                     :read-only t)
  (body         (util:required 'body)         :type node-body                        :read-only t)
  (monomorphize (util:required 'monomorphize) :type (or null attribute-monomorphize) :read-only nil)
  (inline       (util:required 'inline  )     :type (or null attribute-inline)       :read-only nil))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-define-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-define-p x))))

(deftype toplevel-define-list ()
  '(satisfies toplevel-define-list-p))

(defstruct (fundep
            (:copier nil))
  (left     (util:required 'left)     :type keyword-src-list :read-only t)
  (right    (util:required 'right)    :type keyword-src-list :read-only t)
  (location (util:required 'location) :type source:location  :read-only t))

(defmethod source:location ((self fundep))
  (fundep-location self))

(defun fundep-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'fundep-p x)))

(deftype fundep-list ()
  '(satisfies fundep-list-p))

(defstruct (method-definition
            (:include toplevel-definition)
            (:copier nil))
  (name (util:required 'name) :type identifier-src :read-only t)
  (type (util:required 'type) :type qualified-ty   :read-only t))

(defmethod make-load-form ((self method-definition) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun method-definition-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'method-definition-p x)))

(deftype method-definition-list ()
  '(satisfies method-definition-list-p))

(defstruct (toplevel-define-class
            (:include toplevel-definition)
            (:copier nil))
  (name      (util:required 'name)      :type identifier-src         :read-only t)
  (vars      (util:required 'vars)      :type keyword-src-list       :read-only t)
  (preds     (util:required 'preds)     :type ty-predicate-list      :read-only t)
  (fundeps   (util:required 'fundeps)   :type fundep-list            :read-only t)
  (methods   (util:required 'methods)   :type method-definition-list :read-only t)
  ;; Source information for context, name, and vars
  (head-location (util:required 'head-location) :type source:location :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-define-class-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-define-class-p x))))

(deftype toplevel-define-class-list ()
  '(satisfies toplevel-define-class-list-p))

(defstruct (instance-method-definition
            (:copier nil))
  (name     (util:required 'name)     :type node-variable   :read-only t)
  (params   (util:required 'params)   :type pattern-list    :read-only t)
  (body     (util:required 'body)     :type node-body       :read-only t)
  (location (util:required 'location) :type source:location :read-only t)
  (inline   (util:required 'inline)   :type (or null attribute-inline) :read-only nil))

(defmethod source:location ((self instance-method-definition))
  (instance-method-definition-location self))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun instance-method-definition-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'instance-method-definition-p x))))

(deftype instance-method-definition-list ()
  '(satisfies instance-method-definition-list-p))

(defstruct (toplevel-define-instance
            (:include toplevel-definition)
            (:copier nil))
  (context            (util:required 'context)            :type ty-predicate-list               :read-only t)
  (pred               (util:required 'pred)               :type ty-predicate                    :read-only t)
  (methods            (util:required 'methods)            :type instance-method-definition-list :read-only t)
  ;; Source information for the context and the pred
  (head-location      (util:required 'head-location)      :type source:location                 :read-only t)
  (compiler-generated (util:required 'compiler-generated) :type boolean                         :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-define-instance-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-define-instance-p x))))

(deftype toplevel-define-instance-list ()
  '(satisfies toplevel-define-instance-list-p))

(defstruct (toplevel-lisp-form
            (:copier nil))
  (body   (util:required 'body)       :type cons            :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self toplevel-lisp-form))
  (toplevel-lisp-form-location self))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-lisp-form-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-lisp-form-p x))))

(deftype toplevel-lisp-form-list ()
  '(satisfies toplevel-lisp-form-list-p))

(defstruct (toplevel-specialize
            (:copier nil))
  (from   (util:required 'from)       :type node-variable   :read-only t)
  (to     (util:required 'to)         :type node-variable   :read-only t)
  (type   (util:required 'type)       :type ty              :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self toplevel-specialize))
  (toplevel-specialize-location self))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-specialize-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-specialize-p x))))

(deftype toplevel-specialize-list ()
  '(satisfies toplevel-specialize-list-p))

(defstruct (toplevel-package
            (:include toplevel-definition)
            (:copier nil))
  "A Coalton package definition, which can be used to generate either a DEFPACKAGE form or a package instance directly."
  (name        (util:required 'name)     :type string :read-only t)
  (import      nil                       :type list)
  (import-as   nil                       :type list)
  (import-from nil                       :type list)
  (shadow      nil                       :type list)
  (export      nil                       :type list))

(defstruct program
  (package         nil :type (or null toplevel-package)      :read-only t)
  (types           nil :type toplevel-define-type-list       :read-only nil)
  (type-aliases    nil :type toplevel-define-type-alias-list :read-only nil)
  (structs         nil :type toplevel-define-struct-list     :read-only nil)
  (declares        nil :type toplevel-declare-list           :read-only nil)
  (defines         nil :type toplevel-define-list            :read-only nil)
  (classes         nil :type toplevel-define-class-list      :read-only nil)
  (instances       nil :type toplevel-define-instance-list   :read-only nil)
  (lisp-forms      nil :type toplevel-lisp-form-list         :read-only nil)
  (specializations nil :type toplevel-specialize-list        :read-only nil))

(defun read-program (stream source &optional mode)
  "Read a PROGRAM from SOURCE (an instance of source-error:source).
MODE may be one of :file or :macro.

If MODE is :file, a package form is required.
If MODE is :macro, a package form is forbidden, and an explicit check is made for EOF."
  (declare (type (member :file :macro nil) mode)
           (values program))

  (let* ((eclector.readtable:*readtable*
           (eclector.readtable:copy-readtable eclector.readtable:*readtable*))

         ;; In mode :file, the value of package is a toplevel-package structure
         (package
           (when (eq mode ':file)
             (read-toplevel-package stream source)))
         (program
           (make-program :package package))
         (*package*
           (program-lisp-package program))
         (attributes
           (make-array 0 :adjustable t :fill-pointer t)))

    (loop :do
      (multiple-value-bind (form presentp eofp)
          (maybe-read-form stream source *coalton-eclector-client*)

        (when (and eofp (eq mode ':macro))
          (parse-error "Unexpected EOF"
                       (note source (cons (- (file-position stream) 2)
                                          (- (file-position stream) 1))
                             "missing close parenthesis")))

        (unless presentp
          (return))

        (when (and (parse-toplevel-form form program attributes source)
                   (plusp (length attributes)))
          (util:coalton-bug "parse-toplevel-form indicated that a form was parsed but did not consume all attributes"))))

    (unless (zerop (length attributes))
      (parse-error "Orphan attribute"
                   (source:note (aref attributes 0)
                                "attribute must be attached to another form")))

    (setf (program-types program) (nreverse (program-types program)))
    (setf (program-type-aliases program) (nreverse (program-type-aliases program)))
    (setf (program-structs program) (nreverse (program-structs program)))
    (setf (program-declares program) (nreverse (program-declares program)))
    (setf (program-defines program) (nreverse (program-defines program)))
    (setf (program-classes program) (nreverse (program-classes program)))
    (setf (program-instances program) (nreverse (program-instances program)))
    (setf (program-lisp-forms program) (nreverse (program-lisp-forms program)))
    (setf (program-specializations program) (nreverse (program-specializations program)))

    program))

(defun read-expression (stream source)
  (let* (;; Setup eclector readtable
         (eclector.readtable:*readtable*
           (eclector.readtable:copy-readtable eclector.readtable:*readtable*)))

    ;; Read the coalton form
    (multiple-value-bind (form presentp)
        (maybe-read-form stream source *coalton-eclector-client*)

      (unless presentp
        (parse-error "Malformed coalton expression"
                     (note source (cons (- (file-position stream) 2)
                                        (- (file-position stream) 1))
                           "missing expression")))

      ;; Ensure there is only one form
      (multiple-value-bind (form presentp)
          (maybe-read-form stream source *coalton-eclector-client*)

        (when presentp
          (parse-error "Malformed coalton expression"
                       (note source form "unexpected form"))))

      (parse-expression form source))))

(defun read-expressions (stream source)
  (let* (;; Setup eclector readtable
         (eclector.readtable:*readtable*
           (eclector.readtable:copy-readtable eclector.readtable:*readtable*)))

    ;; Read the coalton form
    (multiple-value-bind (form presentp)
        (maybe-read-form stream source *coalton-eclector-client*)

      (unless presentp
        (parse-error "Malformed coalton expression"
                     (note source (cons (- (file-position stream) 2)
                                        (- (file-position stream) 1))
                           "missing expression")))

      (let ((additional-forms nil))
        ;; Read multiple forms if present.
        (block collect-additional-forms
          (loop (multiple-value-bind (next-form presentp)
                    (maybe-read-form stream source *coalton-eclector-client*)
                  (if presentp
                      (push next-form additional-forms)
                      (return-from collect-additional-forms)))))
        (cond
          ((consp additional-forms)
           (parse-expressions (cons form (nreverse additional-forms)) source))
          (t
           (parse-expression form source)))))))

;;; Packages

(defun parse-import-statement (package cursor)
  (typecase (cst:raw (cursor:cursor-pointer cursor))
    (list (let ((name (cursor:next-symbol cursor
                                          "package name is missing"
                                          "package name must be a symbol")))
            (cursor:discard-symbol cursor 'as)
            (let ((nick (cursor:next-symbol cursor
                                            "missing package nickname"
                                            "package nickname msut be a symbol")))
              (when (not (cursor:empty-p cursor))
                (cursor:error cursor ':next "unexpected value"))
              (pushnew (list (symbol-name nick)
                             (symbol-name name))
                       (toplevel-package-import-as package)))))
    (symbol (pushnew (symbol-name (cst:raw (cursor:cursor-value cursor)))
                     (toplevel-package-import package)))
    (t (parse-error "Malformed package declaration"
                    (note (cursor:cursor-source cursor)
                          (cursor:cursor-value cursor)
                          "expected PACKAGE or (PACKAGE as NICK)")))))

(defun parse-import-from (package cursor)
  "Parse an IMPORT-FROM clause: a package designator followed by uninterned symbols."
  (push (cursor:collect-symbols cursor)
        (toplevel-package-import-from package)))

(defun parse-import (package cursor)
  (when (cursor:empty-p cursor)
    (cursor:error cursor ':after-last "empty IMPORT form"))
  (cursor:each cursor (alexandria:curry 'parse-import-statement package)))

(defun parse-export (package cursor)
  (setf (toplevel-package-export package)
        (append (toplevel-package-export package)
                (cursor:collect-symbols cursor))))

(defun parse-shadow (package cursor)
  (setf (toplevel-package-shadow package)
        (append (toplevel-package-shadow package)
                (cursor:collect-symbols cursor))))

(defvar *package-clauses*
  '(("IMPORT" . parse-import)
    ("IMPORT-FROM" . parse-import-from)
    ("EXPORT" . parse-export)
    ("SHADOW" . parse-shadow)))

(defun package-clause-parser (name)
  (cdr (assoc name *package-clauses* :test #'string-equal)))

(defun parse-package-clause (package cursor)
  "Parse a package clause form CURSOR and add it to PACKAGE."
  (unless (cst:consp (cursor:cursor-pointer cursor))
    (cursor:error cursor ':form "malformed package clause"))
  (let* ((clause-name (cursor:next cursor :unwrap nil))
         (clause-name-location (source:make-location (cursor:cursor-source cursor)
                                                     (cst:source clause-name))))
    (unless (symbolp (cst:raw clause-name))
      (parse-error (cursor:cursor-message cursor)
                   (source:note clause-name-location
                                "not a symbol")))
    (let ((parser (package-clause-parser (cst:raw clause-name))))
      (when (null parser)
        (parse-error (cursor:cursor-message cursor)
                     (source:note clause-name-location
                                  "Unknown package clause")
                     (source:help clause-name-location
                                  #'identity
                                  "Must be one of ~{~a~^, ~}"
                                  (mapcar #'car *package-clauses*))))
      (funcall parser package cursor))))


(defun parse-package (cursor)
  "Parse a coalton package declaration."
  (cursor:discard-symbol cursor 'package
                         "package declarations must start with `package`")
  (let* ((package-name
           (cursor:next-symbol cursor
                               "missing package name"
                               "package name must be a symbol"))
         (package-doc
           (unless (cursor:empty-p cursor)
             (cursor:next cursor :pred #'stringp)))
         (package
           (make-toplevel-package :name (symbol-name package-name)
                                  :docstring package-doc
                                  :location (form-location (cursor:cursor-source cursor)
                                                           (cursor:cursor-value cursor)))))
    (cursor:each cursor (alexandria:curry #'parse-package-clause package))
    package))

;; Empty package for reading (package) forms

(defpackage #:coalton-impl/parser/%defpackage
  (:documentation "The sole purpose of this package is to provide storage for symbols that are interned while Coalton package forms are read.")
  (:use))

(defmacro with-parser-package (&body body)
  `(let ((*package* (find-package 'coalton-impl/parser/%defpackage)))
     (unwind-protect
          (progn ,@body)
       (do-symbols (symbol)
         (unintern symbol)))))

(defun read-toplevel-package (stream source)
  "Read and parse a Coalton toplevel package form."
  (with-parser-package
    (multiple-value-bind (form presentp)
        (maybe-read-form stream source *coalton-eclector-client*)
      (unless presentp
        (parse-error "Malformed package declarations"
                     (source:note (source:make-location source
                                                        (cons (- (file-position stream) 2)
                                                              (- (file-position stream) 1)))
                                  "missing package form")))
      (parse-package (cursor:make-cursor form source "Malformed package declaration")))))

(defun make-defpackage (package)
  "Generate a Lisp defpackage form from a Caolton toplevel-package structure."
  `(defpackage ,(toplevel-package-name package)
     (:use "COALTON" ,@(toplevel-package-import package))
     ,@(mapcar (lambda (form)
                 `(:import-from ,@form))
               (toplevel-package-import-from package))
     (:local-nicknames ,@(toplevel-package-import-as package))
     (:export ,@(toplevel-package-export package))
     (:shadow ,@(toplevel-package-shadow package))))

;; The version of uiop:define-package that ships with SBCL doesn't
;; support package local nicknames. This could be remedied by
;; depending on trivial-package-nicknames, but evalling a defpackage
;; form is good enough to get predictable behavior from supported
;; platforms. This may be worth revisiting in the future if there trun
;; out to be places where package definition behavior should diverge
;; from that of common lisp.

(defun lisp-package (package)
  "Create a Lisp package by evaluating the defpackage representation of a PACKAGE form."
  (handler-case
      (eval (make-defpackage package))
    (error ()
      (parse-error "Malformed package declaration"
                   (source:note package
                                "unable to evaluate package definition")))))

(defun program-lisp-package (program)
  "Return the Lisp package associated with PROGRAM, or current *PACKAGE* if none was specified."
  (if (program-package program)
      (lisp-package (program-package program))
      *package*))

;; end package support

(defun eval-toplevel-p (form)
  "T if form is an instance of EVAL-WHEN containing a COMPILE-TOPLEVEL symbol."
  (and (eq 'cl:eval-when (car form))
       (some (lambda (k)
               (or (string-equal 'compile k)
                   (string-equal 'compile-toplevel k)))
             (cadr form))))

(defun maybe-def-p (symbol)
  "Return T if SYMBOL's name starts with 'DEF'."
  (when (symbolp symbol)
    (let ((name (symbol-name symbol)))
      (and (<= 3 (length name))
           (string= name "DEF" :end1 3)))))

(defun parse-lisp-toplevel-form (form program source)
  "Parse lisp forms that are to be literally included in compiler output.

If the outermost form matches (eval-when (compile-toplevel) ..), evaluate the enclosed forms."
  (let* ((options-node (cst:first (cst:rest form)))
         (options (cst:raw options-node)))
    (unless (null options)
      (cond ((maybe-def-p (car options))
             (parse-error "Invalid lisp-toplevel form"
                          (note source (cst:first (cst:rest form))
                                "saw 'def' form: in lisp-toplevel, code must be preceded by an empty options list")
                          (secondary-note source form
                                          "when parsing lisp-toplevel")))
            (t
             (parse-error "Invalid lisp-toplevel form"
                          (note source (cst:first (cst:rest form))
                                "lisp-toplevel must be followed by an empty options list")
                          (secondary-note source form
                                          "when parsing lisp-toplevel"))))))

  (loop :for form :in (cst:raw (cst:rest (cst:rest form)))
        :when (eval-toplevel-p form)
          :do (dolist (form (cddr form))
                (eval form)))
  (push (make-toplevel-lisp-form :body (cddr (cst:raw form))
                                 :location (form-location source form))
        (program-lisp-forms program)))


;;; Functions for working with attributes (repr, monomorphize)

(defun consume-repr (attributes toplevel-form message)
  "Return the unique repr attribute in ATTRIBUTES, or NIL.
If the attribute is not unique, or a monomorphize attribute is present, signal a parse error."
  (let (repr)
    (loop :for attribute :across attributes
          :do (etypecase attribute
                (attribute-repr
                 (when repr
                   (parse-error "Duplicate repr attribute"
                                (source:note attribute "repr attribute here")
                                (source:secondary-note repr "previous attribute here")
                                (source:secondary-note toplevel-form message)))
                 (setf repr attribute))
                (attribute-monomorphize
                 (parse-error "Invalid target for monomorphize attribute"
                              (source:note attribute "monomorphize must be attached to a define or declare form")
                              (source:secondary-note toplevel-form message)))))
    (setf (fill-pointer attributes) 0)
    repr))

(defun consume-optimize-attribute (attribute attributes toplevel-form message)
  "Return the unique monomorphize attribute in ATTRIBUTES, or NIL.
If the attribute is not unique, or a repr attribute is present, signal a parse error."
  (declare (type (member :monomorphize :inline) attribute))
  (let (monomorphize inline)
    (loop :for attribute :across attributes
          :do (etypecase attribute
                (attribute-repr
                 (parse-error "Invalid target for repr attribute"
                              (source:note attribute "repr must be attached to a define-type")
                              (source:secondary-note toplevel-form message)))
                (attribute-monomorphize
                 (when monomorphize
                   (parse-error "Duplicate monomorphize attribute"
                                (source:note attribute "monomorphize attribute here")
                                (source:secondary-note monomorphize "previous attribute here")
                                (source:secondary-note toplevel-form message)))
                 (setf monomorphize attribute))
                (attribute-inline
                 (when inline
                   (parse-error "Duplicate inline attribute"
                                (source:note attribute "inline attribute here")
                                (source:secondary-note inline "previous attribute here")
                                (source:secondary-note toplevel-form message)))
                 (setf inline attribute))))
    (ecase attribute
      (:monomorphize monomorphize)
      (:inline inline))))

(defun forbid-attributes (attributes form source)
  "If ATTRIBUTES is non-zero length, signal a parse error using FORM and SOURCE for location context."
  (unless (zerop (length attributes))
    (let ((toplevel-form-name (string-downcase (cst:raw (cst:first form)))))
      (parse-error (format nil "Invalid attribute for ~A" toplevel-form-name)
                   (source:note (aref attributes 0) "~A cannot have attributes" toplevel-form-name)
                   (secondary-note source form "when parsing ~A" toplevel-form-name)))))


;;; This is the parser for complete toplevel Coalton attributes,
;;; declarations and definitions. It selects a sub-parser by examining
;;; the first symbol in the form.

(defun parse-toplevel-form (form program attributes source)
  "Parse a toplevel Coalton form in FORM, recording source locations that refer to SOURCE.
If the parsed form is a program definition, add it to PROGRAM and return T.
If the parsed form is an attribute (e.g., repr or monomorphize), add it to to ATTRIBUTES and return NIL."
  (declare (type cst:cst form)
           (type program program)
           (type (vector attribute) attributes)
           (values boolean &optional))

  (when (cst:atom form)
    (parse-error "Malformed toplevel form"
                 (note source form "Unexpected atom")))

  ;; Toplevel forms must begin with an atom
  (when (cst:consp (cst:first form))
    (parse-error "Malformed toplevel form"
                 (note source (cst:first form) "unexpected list")))

  (case (cst:raw (cst:first form))
    ((coalton:monomorphize)
     (vector-push-extend (parse-monomorphize form source) attributes)
     nil)

    ((coalton:inline)
     (vector-push-extend (parse-inline form source) attributes)
     nil)

    ((coalton:repr)
     (vector-push-extend (parse-repr form source) attributes)
     nil)

    ((coalton:define)
     (let* ((define (parse-define form source))
            (monomorphize (consume-optimize-attribute :monomorphize attributes define "when parsing define"))
            (inline (consume-optimize-attribute :inline attributes define "when parsing define")))
       (setf (toplevel-define-monomorphize define) monomorphize)
       (setf (toplevel-define-inline define) inline)
       (setf (fill-pointer attributes) 0)
       (push define (program-defines program))
       t))

    ((coalton:declare)
     (let* ((declare (parse-declare form source))
            (monomorphize (consume-optimize-attribute :monomorphize attributes define "when parsing declare"))
            (inline (consume-optimize-attribute :inline attributes define "when parsing declare")))
       (setf (toplevel-declare-monomorphize declare) monomorphize)
       (setf (toplevel-declare-inline declare) inline)
       (setf (fill-pointer attributes) 0)
       (push declare (program-declares program))
       t))

    ((coalton:define-type)
     (let* ((type (parse-define-type form source))
            (repr (consume-repr attributes type "when parsing define-type")))
       (setf (toplevel-define-type-repr type) repr)
       (push type (program-types program))
       t))

    ((coalton:define-type-alias)
     (forbid-attributes attributes form source)
     (let ((alias (parse-define-type-alias form source)))
       (push alias (program-type-aliases program))
       t))

    ((coalton:define-struct)
     (let* ((struct (parse-define-struct form source))
            (repr (consume-repr attributes struct "when parsing define-struct")))
       (when (and repr
                  (not (eq :transparent (keyword-src-name (attribute-repr-type repr)))))
         (parse-error "Invalid repr attribute"
                      (source:note repr "structs can only be repr transparent")
                      (source:secondary-note struct "when parsing define-struct")))
       (setf (toplevel-define-struct-repr struct) repr)
       (push struct (program-structs program))
       t))

    ((coalton:define-class)
     (forbid-attributes attributes form source)
     (let ((class (parse-define-class form source)))
       (push class (program-classes program))
       t))

    ((coalton:define-instance)
     (forbid-attributes attributes form source)
     (let ((instance (parse-define-instance form source)))
       (push instance (program-instances program))
       t))

    ((coalton:lisp-toplevel)
     (unless (alexandria:featurep ':coalton-lisp-toplevel)
       (parse-error "Invalid lisp-toplevel form"
                    (note source form
                          "lisp-toplevel is only allowed in library source code. To enable elsewhere, (pushnew :coalton-lisp-toplevel *features*)")))
     (forbid-attributes attributes form source)
     (parse-lisp-toplevel-form form program source)
     t)

    ((coalton:specialize)
     (forbid-attributes attributes form source)
     (let ((spec (parse-specialize form source)))
       (push spec (program-specializations program))
       t))

    ((coalton:progn)
     (forbid-attributes attributes form source)
     (loop :for inner-form := (cst:rest form) :then (cst:rest inner-form)
           :while (not (cst:null inner-form)) :do
             (when (and (parse-toplevel-form (cst:first inner-form) program attributes source)
                        (plusp (length attributes)))
               (util:coalton-bug "parse-toplevel-form indicated that a form was parsed but did not
consume all attributes")))

     (unless (zerop (length attributes))
       (parse-error "Trailing attributes in progn"
                    (source:note (aref attributes 0) "progn cannot have trailing attributes")
                    (secondary-note source form "when parsing progn")))
     t)

    (t
     (cond
       ((and (cst:atom (cst:first form))
             (symbolp (cst:raw (cst:first form)))
             (macro-function (cst:raw (cst:first form))))
        (source:with-context
            (:macro "Error occurs within macro context. Source locations may be imprecise")
          (parse-toplevel-form (expand-macro form source) program attributes source)))

       ((parse-error "Invalid toplevel form"
                     (note source (cst:first form) "unknown toplevel form")))))))


(defun parse-define (form source)
  (declare (type cst:cst form)
           (values toplevel-define))

  (assert (cst:consp form))

  ;; (define)
  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed definition"
                 (note source form "expected define body")))

  ;; (define x)
  (unless (cst:consp (cst:rest (cst:rest form)))
    (parse-error "Malformed definition"
                 (note source form "expected value")))

  (multiple-value-bind (name params)
      (parse-argument-list (cst:second form) source)

    (multiple-value-bind (docstring body)
        (parse-definition-body (cst:rest (cst:rest form)) form source)

      (make-toplevel-define
       :name name
       :params params
       :orig-params params
       :docstring docstring
       :body body
       :monomorphize nil
       :location (form-location source form)
       :inline nil))))

(defun parse-declare (form source)
  (declare (type cst:cst form)
           (values toplevel-declare))

  (assert (cst:consp form))

  ;; (declare)
  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed declaration"
                 (note source form "expected body")))

  ;; (declare x)
  (unless (cst:consp (cst:rest (cst:rest form)))
    (parse-error "Malformed declaration"
                 (note source form "expected declared type")))

  ;; (declare x y z)
  (when (cst:consp (cst:rest (cst:rest (cst:rest form))))
    (parse-error "Malformed declaration"
                 (note source (cst:first (cst:rest (cst:rest (cst:rest form))))
                       "unexpected trailing form")))

  ;; (declare 0.5 x)
  (unless (identifierp (cst:raw (cst:second form)))
    (parse-error "Malformed declaration"
                 (note source (cst:second form)
                       "expected symbol")))

  (make-toplevel-declare
   :name (make-identifier-src
          :name (cst:raw (cst:second form))
          :location (form-location source (cst:second form)))
   :type (parse-qualified-type (cst:third form) source)
   :monomorphize nil
   :location (form-location source form)
   :inline nil))

(defun parse-define-type (form source)
  (declare (type cst:cst form)
           (values toplevel-define-type))

  (assert (cst:consp form))

  (let (name
        docstring
        variables)
    (declare (type (or null identifier-src) name)
             (type keyword-src-list variables))

    ;; (define-type)
    (unless (cst:consp (cst:rest form))
      (parse-error "Malformed type definition"
                   (note source form "expected body")))

    (cond
      ((cst:atom (cst:second form))
       (unless (identifierp (cst:raw (cst:second form)))
         (parse-error "Malformed type definition"
                      (note source (cst:second form) "expected symbol")))

       (setf name (make-identifier-src :name (cst:raw (cst:second form))
                                       :location (form-location source form))))

      (t                                ; (define-type (T ...) ...)
       ;; (define-type ((T) ...) ...)
       (unless (cst:atom (cst:first (cst:second form)))
         (parse-error "Malformed type definition"
                      (note source (cst:first (cst:second form))
                            "expected symbol")
                      (help source (cst:second form)
                            (lambda (existing)
                              (subseq existing 1 (1- (length existing))))
                            "remove parentheses")))

       ;; (define-type (1 ...) ...)
       (unless (identifierp (cst:raw (cst:first (cst:second form))))
         (parse-error "Malformed type definition"
                      (note source (cst:first (cst:second form))
                            "expected symbol")))

       (setf name (make-identifier-src :name (cst:raw (cst:first (cst:second form)))
                                       :location (form-location source
                                                                (cst:first (cst:second form)))))

       ;; (define-type (T) ...)
       (when (cst:atom (cst:rest (cst:second form)))
         (parse-error "Malformed type definition"
                      (note source (cst:second form)
                            "nullary types should not have parentheses")
                      (help source (cst:second form)
                            (lambda (existing)
                              (subseq existing 1 (1- (length existing))))
                            "remove unnecessary parentheses")))

       (loop :for vars := (cst:rest (cst:second form)) :then (cst:rest vars)
             :while (cst:consp vars)
             :do (push (parse-type-variable (cst:first vars) source) variables))))

    (when (and (cst:consp (cst:rest (cst:rest form)))
               (cst:atom (cst:third form))
               (stringp (cst:raw (cst:third form))))
      (setf docstring (cst:raw (cst:third form))))

    (when (and docstring
               (cst:consp (cst:nthrest 3 form))
               (stringp (cst:raw (cst:fourth form))))
      (parse-error "Malformed type definition"
                   (note source
                         (cst:source (cst:fourth form))
                         "only one docstring allowed.")
                   (help source
                         (cst:source (cst:fourth form))
                         (lambda (existing)
                           (subseq existing 1 (1- (length existing))))
                         "remove additional docstring")))

    (make-toplevel-define-type
     :name name
     :vars (reverse variables)
     :docstring docstring
     :ctors (loop :for constructors_
                    := (cst:nthrest (if docstring 3 2) form)
                      :then (cst:rest constructors_)
                  :with ctors := nil
                  :while (cst:consp constructors_)

                  ;; check for duplicate docstrings
                  :when (and (cst:atom (cst:first constructors_))
                             (stringp (cst:raw (cst:first constructors_)))
                             (not (cst:null (cst:rest constructors_)))
                             (cst:atom (cst:second constructors_))
                             (stringp (cst:raw (cst:second constructors_))))
                    :do (parse-error "Malformed type definition"
                                     (note source
                                           (cst:second constructors_)
                                           "only one docstring allowed per constructor"))

                        ;; collect constructors with docstrings if they follow
                  :do (let ((ctor-docstring (if (and (not (cst:null (cst:rest constructors_)))
                                                     (cst:atom (cst:second constructors_))
                                                     (stringp (cst:raw (cst:second constructors_))))
                                                (cst:raw (cst:second constructors_))
                                                nil)))


                        (unless (stringp (cst:raw (cst:first constructors_)))
                            (push (parse-constructor (cst:first constructors_) form ctor-docstring source) ctors)))
                  :finally (return ctors))
     :repr nil
     :location (form-location source form)
     :head-location (form-location source (cst:second form)))))

(defun parse-define-type-alias (form source)
  (declare (type cst:cst form)
           (values toplevel-define-type-alias))

  (assert (cst:consp form))

  (let (docstring
        name
        variables)

    ;; (define-type-alias)
    (unless (cst:consp (cst:rest form))
      (parse-error "Malformed type alias definition"
                   (note source form "expected body")))

    (cond
      ;; (define-type-alias _ ...)
      ((cst:atom (cst:second form))
       ;; (define-type-alias 0.5 ...)
       (unless (identifierp (cst:raw (cst:second form)))
         (parse-error "Malformed type alias definition"
                      (note source (cst:second form) "expected symbol")))

       ;; (define-type-alias name ...)
       (setf name (make-identifier-src :name (cst:raw (cst:second form))
                                       :location (form-location source form))))

      ;; (define-type-alias (_ ...) ...)
      (t
       ;; (define-type-alias((name) ...) ...)
       (unless (cst:atom (cst:first (cst:second form)))
         (parse-error "Malformed type alias definition"
                      (note source (cst:first (cst:second form))
                            "expected symbol")
                      (help source (cst:second form)
                            (lambda (existing)
                              (subseq existing 1 (1- (length existing))))
                            "remove parentheses")))

       ;; (define-type-alias (0.5 ...) ...)
       (unless (identifierp (cst:raw (cst:first (cst:second form))))
         (parse-error "Malformed type alias definition"
                      (note source (cst:first (cst:second form))
                            "expected symbol")))

       ;; (define-type-alias (name ...) ...)
       (setf name (make-identifier-src :name (cst:raw (cst:first (cst:second form)))
                                       :location (form-location source
                                                                (cst:first (cst:second form)))))

       ;; (define-type-alias (name) ...)
       (when (cst:atom (cst:rest (cst:second form)))
         (parse-error "Malformed type alias definition"
                      (note source (cst:second form)
                            "nullary type aliases should not have parentheses")
                      (help source (cst:second form)
                            (lambda (existing)
                              (subseq existing 1 (1- (length existing))))
                            "remove unnecessary parentheses")))

       ;; (define-type-alias (name type-variables+) ...)
       (loop :for vars := (cst:rest (cst:second form)) :then (cst:rest vars)
             :while (cst:consp vars)
             :do (push (parse-type-variable (cst:first vars) source) variables))))

    ;; (define-type-alias name)
    (unless (cst:consp (cst:rest (cst:rest form)))
      (parse-error "Malformed type alias definition"
                   (note source form "expected type")))

    ;; (define-type-alias name type docstring)
    (when (and (cst:consp (cst:nthrest 3 form))
               (cst:atom (cst:fourth form))
               (stringp (cst:raw (cst:fourth form))))
      (setf docstring (cst:raw (cst:fourth form))))

    ;; (define-type-alias name type docstring ...)
    (when (and docstring
               (cst:consp (cst:nthrest 4 form)))
      (parse-error "Malformed type alias definition"
                   (note source (cst:fifth form)
                         "unexpected trailing form")))

    (make-toplevel-define-type-alias
     :name name
     :vars (reverse variables)
     :type (parse-type (cst:third form) source)
     :docstring docstring
     :location (form-location source form)
     :head-location (form-location source (cst:second form)))))

(defun parse-define-struct (form source)
  (declare (type cst:cst form))

  (assert (cst:consp form))

  (let (unparsed-name
        unparsed-variables
        docstring)

    ;; (define-struct)
    (unless (cst:consp (cst:rest form))
      (parse-error "Malformed struct definition"
                   (note source form "expected body")))

    (if (cst:atom (cst:second form))
        ;; (define-struct S ...)
        (setf unparsed-name (cst:second form))

        ;; (define-struct (S ...) ...)
        (progn
          (setf unparsed-name (cst:first (cst:second form)))
          (setf unparsed-variables (cst:rest (cst:second form)))))

    ;; (define-struct S "docstring" ...)
    (when (and (cst:consp (cst:rest (cst:rest form)))
               (cst:atom (cst:third form))
               (stringp (cst:raw (cst:third form))))
      (setf docstring (cst:raw (cst:third form))))

    (make-toplevel-define-struct
     :name (parse-identifier unparsed-name source)
     :vars (when unparsed-variables
             (parse-list #'parse-type-variable unparsed-variables source))
     :docstring docstring
     :fields (parse-list
              #'parse-struct-field
              (cst:nthrest (if docstring 3 2) form)
              source)
     :location (form-location source form)
     :repr nil
     :head-location (form-location source (cst:second form)))))

(defun parse-define-class (form source)
  (declare (type cst:cst form)
           (values toplevel-define-class))

  (assert (cst:consp form))

  (let (unparsed-name
        unparsed-variables
        name
        variables
        fundeps
        predicates
        docstring
        methods)

    ;; (define-class)
    (unless (cst:consp (cst:rest form))
      (parse-error "Malformed class definition"
                   (note source form "expected body")))

    ;; (define-class C)
    (unless (cst:consp (cst:second form))
      (parse-error "Malformed class definition"
                   (note source (cst:second form)
                         "expected class type variable(s)")
                   (help source (cst:second form)
                         (lambda (existing)
                           (concatenate 'string "(" existing " :a)"))
                         "add class type variable `:a`")))

    (unless (cst:proper-list-p (cst:second form))
      (parse-error "Malformed class definition"
                   (note source (cst:second form) "unexpected dotted list")))

    (multiple-value-bind (left right)
        (util:take-until (lambda (cst)
                           (and (cst:atom cst)
                                (eq (cst:raw cst) 'coalton:=>)))
                         (cst:listify (cst:second form)))

      ;; (=> C ...)
      (when (and (null left) right)
        (apply #'parse-error "Malformed class definition"
               (cons (note source (cst:first (cst:second form))
                           "unnecessary `=>`")
                     (cond
                       ;; If this is the only thing in the list then don't suggest anything
                       ((cst:atom (cst:rest (cst:second form)))
                        nil)
                       ;; If there is nothing to the right of C then emit without list
                       ((cst:atom (cst:rest (cst:rest (cst:second form))))
                        (list (help source (cst:second form)
                                    (lambda (existing)
                                      (subseq existing 4 (1- (length existing))))
                                    "remove `=>`")))
                       (t
                        (list (help source (cst:second form)
                                    (lambda (existing)
                                      (concatenate 'string
                                                   (subseq existing 0 1)
                                                   (subseq existing 4)))
                                    "remove `=>`")))))))

      ;; (... =>)
      (when (and left right (null (cdr right)))
        (parse-error "Malformed class definition"
                     (note source (cst:second form) "missing class name")))

      (cond
        ;; No predicates
        ((null right)
         (setf unparsed-name (first left))
         (setf unparsed-variables (rest left)))

        ;; (... => (...) ...)
        ((and (cst:consp (second right))
              (consp (cdr (cdr right))))
         (parse-error "Malformed class definition"
                      (note source (third right) "unexpected form")))

        ;; (... => (...))
        ((cst:consp (second right))
         (setf unparsed-name (cst:first (second right)))
         (setf unparsed-variables (cst:listify (cst:rest (second right)))))

        ;; (... => C ...)
        (t
         (setf unparsed-name (second right))
         (setf unparsed-variables (nthcdr 2 right))))


      ;; (define-class ((C) ...))
      (unless (cst:atom unparsed-name)
        (parse-error "Malformed class definition"
                     (note source unparsed-name "unnecessary parentheses")
                     (help source unparsed-name
                           (lambda (existing)
                             (subseq existing 1 (1- (length existing))))
                           "remove unnecessary parentheses")))

      (unless (identifierp (cst:raw unparsed-name))
        (parse-error "Malformed class definition"
                     (note source unparsed-name "expected symbol")))

      (setf name (cst:raw unparsed-name))

      (when (null unparsed-variables)
        (parse-error "Malformed class definition"
                     (note source unparsed-name
                           "expected class type variable(s)")
                     (help source unparsed-name
                           (lambda (existing)
                             (if (cst:consp (cst:second form))
                                 (concatenate 'string existing " :a")
                                 (concatenate 'string "(" existing " :a)")))
                           "add class type variable `:a`")))


      (multiple-value-bind (left right)
          (util:take-until #'cst:consp unparsed-variables)

        (setf variables
              (loop :for var :in left
                    :collect (parse-type-variable var source)))

        (setf fundeps
              (loop :for fundep :in right
                    :collect (parse-fundep fundep source))))

      ;; (... => C ...)
      (when right
        (if (cst:atom (first left))
            ;; (C1 ... => C2 ...)
            (setf predicates
                  (list (parse-predicate left
                                         (source:make-location source
                                                               (util:cst-source-range left)))))

            ;; ((C1 ...) (C2 ...) ... => C3 ...)
            (setf predicates
                  (loop :for pred :in left
                        :collect (parse-predicate (cst:listify pred)
                                                  (form-location source pred))))))

      (when (and (cst:consp (cst:rest (cst:rest form)))
                 (cst:atom (cst:third form))
                 (stringp (cst:raw (cst:third form))))
        (setf docstring (cst:raw (cst:third form))))

      (setf methods
            (loop :for methods
                    := (cst:nthrest (if docstring 3 2) form)
                      :then (cst:rest methods)
                  :while (cst:consp methods)
                  :collect (parse-method (cst:first methods) form source)))

      (make-toplevel-define-class
       :name (make-identifier-src
              :name name
              :location (form-location source unparsed-name))
       :vars variables
       :preds predicates
       :fundeps fundeps
       :docstring docstring
       :methods methods
       :location (form-location source form)
       :head-location (form-location source (cst:second form))))))

(defun parse-define-instance (form source)
  (declare (type cst:cst form)
           (values toplevel-define-instance))

  (assert (cst:consp form))

  (let (unparsed-context
        context
        unparsed-predicate
        docstring)

    ;; (define-instance)
    (unless (cst:consp (cst:rest form))
      (parse-error "Malformed instance definition"
                   (note-end source (cst:first form) "expected an instance head")))

    ;; (define-instance 5)
    (unless (cst:consp (cst:second form))
      (parse-error "Malformed instance definition"
                   (note source (cst:second form) "expected a list")))

    (unless (cst:proper-list-p (cst:second form))
      (parse-error "Malformed instance definition"
                   (note source (cst:second form) "unexpected dotted list")))

    (multiple-value-bind (left right)
        (util:take-until
         (lambda (form)
           (and (cst:atom form)
                (eq (cst:raw form) 'coalton:=>)))
         (cst:listify (cst:second form)))

      (cond
        ;; No predicates
        ((null right)
         (setf unparsed-predicate left))

        ;; (... => (...) ...)
        ((and (second right)
              (cst:consp (second right))
              (consp (cdr (cdr right))))
         (parse-error "Malformed instance definition"
                      (note source (third right) "unexpected form")))

        ;; (.... => (...))
        ((and (second right)
              (cst:consp (second right)))
         (setf unparsed-predicate (cst:listify (second right)))
         (setf unparsed-context left))

        ;; (... => C ...)
        (t
         (setf unparsed-predicate (cdr right))
         (setf unparsed-context left)))

      ;; (... =>)
      (when (and left right (null (cdr right)))
        (parse-error "Malformed instance head"
                     (note source (first right)
                           "unexpected `=>`")
                     (help source (first right)
                           (lambda (existing)
                             (declare (ignore existing))
                             "")
                           "remove the `=>`")))

      ;; (=> ...)
      (when (and (null left) right)
        (parse-error "Malformed instance head"
                     (note source (first right)
                           "unexpected `=>`")
                     (help source (first right)
                           (lambda (existing)
                             (declare (ignore existing))
                             "")
                           "remove the `=>`")))

      (when unparsed-context
        (if (cst:atom (first unparsed-context))
            (setf context
                  (list (parse-predicate unparsed-context
                                         (source:make-location
                                          source
                                          (util:cst-source-range unparsed-context)))))

            (setf context
                  (loop :for unparsed :in unparsed-context
                        :collect (parse-predicate (cst:listify unparsed)
                                                  (form-location source unparsed))))))

      (when (and (cst:consp (cst:rest (cst:rest form)))
                 (cst:atom (cst:third form))
                 (stringp (cst:raw (cst:third form))))
        (setf docstring (cst:raw (cst:third form))))

      (let ((methods (loop :with forms := (cst:nthrest (if docstring 3 2) form)
                           :while (cst:raw forms)
                           :for inline := (let ((form (cst:first forms)))
                                              (when (and (cst:consp form)
                                                         (eq 'coalton:inline (cst:raw (cst:first form))))
                                                (if (null (cst:raw (cst:rest form)))
                                                    (progn
                                                      (setq forms (cst:rest forms))
                                                      (parse-inline form file))
                                                    (parse-error "Malformed inline form"
                                                                 (note source form "expected list")))))
                           :for method := (parse-instance-method-definition (cst:first forms) (cst:second form) source)
                           :collect
                           (progn
                             (when inline
                               (setf (instance-method-definition-inline method) inline))
                             (setq forms (cst:rest forms))
                             method))))

        (make-toplevel-define-instance
         :context context
         :pred (parse-predicate unparsed-predicate
                                (source:make-location source
                                                      (util:cst-source-range unparsed-predicate)))
         :docstring docstring
         :methods methods
         :location (form-location source form)
         :head-location (form-location source (cst:second form))
         :compiler-generated nil)))))

(defun parse-specialize (form source)
  (declare (type cst:cst form)
           (values toplevel-specialize))

  (assert (cst:consp form))

  ;; (specialize)
  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed specialize declaration"
                 (source:note (source:end-location (form-location source form))
                              "missing from name")))

  ;; (specialize f)
  (unless (cst:consp (cst:rest (cst:rest form)))
    (parse-error "Malformed specialize declaration"
                 (note-end source form "missing to name")))

  ;; (specialize f f2)
  (unless (cst:consp (cst:rest (cst:rest (cst:rest form))))
    (parse-error "Malformed specialize declaration"
                 (note-end source form "missing type")))

  ;; (specialize f f2 t ....)
  (when (cst:consp (cst:rest (cst:rest (cst:rest (cst:rest form)))))
    (parse-error "Malformed specialize declaration"
                 (note source (cst:first (cst:rest (cst:rest (cst:rest (cst:rest form)))))
                       "unexpected form")))

  (make-toplevel-specialize
   :from (parse-variable (cst:second form) source)
   :to (parse-variable (cst:third form) source)
   :type (parse-type (cst:fourth form) source)
   :location (form-location source form)))

(defun parse-method (method-form form source)
  (declare (type cst:cst method-form)
           (values method-definition))

  (let ((class-note (secondary-note source (cst:second form)
                                    "in this class definition")))
    ;; m or (m)
    (unless (and (cst:consp method-form)
                 (cst:consp (cst:rest method-form)))
      (parse-error "Malformed method definition"
                   (note source method-form "missing method type")
                   class-note))

    ;; (m d t ...)
    (unless (or (cst:null (cst:rest (cst:rest method-form)))
                (cst:null (cst:rest (cst:rest (cst:rest method-form)))))
      (parse-error "Malformed method definition"
                   (note source (cst:first (cst:rest (cst:rest (cst:rest method-form))))
                         "unexpected trailing form")
                   class-note))

    ;; (0.5 t ...)
    (unless (and (cst:atom (cst:first method-form))
                 (identifierp (cst:raw (cst:first method-form))))
      (parse-error "Malformed method definition"
                   (note source (cst:first method-form) "expected symbol")
                   class-note))

    ;; (m "docstring")
    (when (and (cst:atom (cst:second method-form))
               (stringp (cst:raw (cst:second method-form)))
               (cst:null (cst:rest (cst:rest method-form))))
      (parse-error "Malformed method definition"
                   (note source (cst:second method-form) "missing method type")
                   class-note))

    (let (docstring)
      (when (and (cst:atom (cst:second method-form))
                 (stringp (cst:raw (cst:second method-form))))
        (setf docstring (cst:raw (cst:second method-form))))

      ;; either list of length 2 or list of length 3 with docstring
      (unless (or (cst:null (cst:rest (cst:rest method-form)))
                  (and (cst:atom (cst:second method-form))
                       (stringp (cst:raw (cst:second method-form)))))
        (parse-error "Malformed method definition"
                     (note source (if docstring
                                      (cst:fourth method-form)
                                      (cst:third method-form))
                           "unexpected trailing form")
                     class-note))

      (make-method-definition
       :name (make-identifier-src
              :name (node-variable-name (parse-variable (cst:first method-form) source))
              :location (form-location source (cst:first method-form)))
       :docstring docstring
       :type (parse-qualified-type (if docstring
                                       (cst:third method-form)
                                       (cst:second method-form))
                                   source)
       :location (form-location source method-form)))))

(defun parse-type-variable (form source)
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
   :location (form-location source form)))

(defun parse-constructor (form enclosing-form docstring source)
  (declare (type cst:cst form enclosing-form)
           (values constructor))

  (let (unparsed-name
        unparsed-fields)

    (cond
      ((cst:atom form)
       (setf unparsed-name form))
      (t
       (progn
         (setf unparsed-name (cst:first form))
         (setf unparsed-fields (cst:listify (cst:rest form))))))

    (unless (cst:atom unparsed-name)
      (parse-error "Malformed constructor"
                   (note source unparsed-name "expected symbol")
                   (secondary-note source (cst:second enclosing-form)
                                   "in this type definition")))

    (unless (identifierp (cst:raw unparsed-name))
      (parse-error "Malformed constructor"
                   (note source unparsed-name "expected symbol")
                   (secondary-note source (cst:second enclosing-form)
                                   "in this type definition")))

    (make-constructor
     :name (make-identifier-src
            :name (cst:raw unparsed-name)
            :location (form-location source unparsed-name))
     :fields (loop :for field :in unparsed-fields
                   :collect (parse-type field source))
     :location (form-location source form)
     :docstring docstring)))


(defun parse-argument-list (form source)
  (declare (type cst:cst form)
           (values node-variable pattern-list))

  ;; (define x 1)
  (when (cst:atom form)
    (return-from parse-argument-list (values (parse-variable form source) nil)))

  ;; (define (0.5 x y) ...)
  (unless (identifierp (cst:raw (cst:first form)))
    (parse-error "Malformed function definition"
                 (note source (cst:first form) "expected symbol")))

  (values
   (parse-variable (cst:first form) source)
   (if (cst:null (cst:rest form))
       (list
        (make-pattern-wildcard
         :location (form-location source form)))
       (loop :for vars := (cst:rest form) :then (cst:rest vars)
             :while (cst:consp vars)
             :collect (parse-pattern (cst:first vars) source)))))

(defun parse-identifier (form source)
  (declare (type cst:cst form)
           (values identifier-src))

  (unless (cst:atom form)
    (parse-error "Unexpected list"
                 (note source form "expected an identifier")))

  (unless (identifierp (cst:raw form))
    (parse-error "Unexpected form"
                 (note source form "expected an identifier")))

  (when (string= "_" (cst:raw form))
    (parse-error "Invalid identifier"
                 (note source form "invalid identifier '_'")))

  (when (char= #\. (aref (symbol-name (cst:raw form)) 0))
    (parse-error "Invalid identifier"
                 (note source form "identifiers cannot start with '.'")))

  (make-identifier-src
   :name (cst:raw form)
   :location (form-location source form)))

(defun parse-definition-body (form enclosing-form source)
  (declare (type cst:cst form)
           (type cst:cst enclosing-form)
           (values (or null string) node-body))

  (let (docstring
        unparsed-body)

    (when (cst:atom (cst:rest form))
      (return-from parse-definition-body
        (values nil (parse-body form enclosing-form source))))

    (if (and (cst:atom (cst:first form))
             (stringp (cst:raw (cst:first form))))
        (progn
          (setf docstring (cst:raw (cst:first form)))
          (setf unparsed-body (cst:rest form)))

        (setf unparsed-body form))

    (values docstring (parse-body unparsed-body enclosing-form source))))

(defun parse-instance-method-definition (form parent-form source)
  (declare (type cst:cst form)
           (type cst:cst parent-form)
           (values instance-method-definition))

  (let ((context-note (secondary-note source parent-form "when parsing instance")))

    (unless (cst:consp form)
      (parse-error "Malformed method definition"
                   (note source form "expected list")
                   context-note))

    (unless (cst:proper-list-p form)
      (parse-error "Malformed method definition"
                   (note source form "unexpected dotted list")
                   context-note))

    (unless (and (cst:atom (cst:first form))
                 (eq (cst:raw (cst:first form)) 'coalton:define))
      (parse-error "Malformed method definition"
                   (note source (cst:first form) "expected method definition")
                   context-note))

    (unless (cst:consp (cst:rest form))
      (parse-error "Malformed method definition"
                   (note source form "expected definition name")
                   context-note))

    (multiple-value-bind (name params)
        (parse-argument-list (cst:second form) source)

      (make-instance-method-definition
       :name name
       :params params
       :body (parse-body (cst:rest (cst:rest form)) form source)
       :location (form-location source form)
       :inline nil))))

(defun parse-fundep (form source)
  "Parse a functional dependency in FORM, consisting of two lists of one or more type variables separated by `->`:

:a ... :n -> :a ... :n"
  (declare (type cst:cst form)
           (values fundep))
  (let ((cursor (cursor:make-cursor form source "Malformed functional dependency")))
    (labels ((parse-var (cst)
               (parse-type-variable cst source))
             (require-vars (vars)
               (when (endp vars)
                 (cursor:error cursor ':after-last "expected one or more type variables"))
               vars))
      (let ((left (require-vars (cursor:collect cursor :key #'parse-var
                                  :test (lambda (value)
                                          (not (eq value 'coalton:->)))))))
        (cursor:next cursor)       ; drop the arrow
        (let ((right (require-vars (cursor:collect cursor :key #'parse-var))))
          (make-fundep :left left
                       :right right
                       :location (form-location source form)))))))

(defun parse-monomorphize (form source)
  (declare (type cst:cst form)
           (values attribute-monomorphize))

  (assert (cst:consp form))

  (when (cst:consp (cst:rest form))
    (parse-error "Malformed monomophize attribute"
                 (note source form "unexpected form")))

  (make-attribute-monomorphize
   :location (form-location source form)))

(defun parse-inline (form source)
  (declare (type cst:cst form)
           (values attribute-inline))

  (assert (cst:consp form))

  (when (cst:consp (cst:rest form))
    (parse-error "Malformed inline attribute"
                 (note source form "unexpected form")))

  (make-attribute-inline
   :location (form-location source form)))

(defun parse-repr (form source)
  (declare (type cst:cst form)
           (values attribute-repr))

  (assert (cst:consp form))

  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed repr attribute"
                 (note source form "expected keyword symbol")))

  (let ((type (parse-type-variable (cst:second form) source)))
    (if (eq (keyword-src-name type) :native)

        (progn ;; :native reprs must have an argument
          (unless (cst:consp (cst:rest (cst:rest form)))
            (parse-error "Malformed repr :native attribute"
                         (note-end source (cst:second form) "expected a lisp type")))

          (when (cst:consp (cst:rest (cst:rest (cst:rest form))))
            (parse-error "Malformed repr :native attribute"
                         (note source (cst:first (cst:rest (cst:rest (cst:rest form))))
                               "unexpected form")))

          (make-attribute-repr
           :type type
           :arg (cst:third form)
           :location (form-location source form)))

        (progn ;; other reprs do not have an argument
          (when (cst:consp (cst:rest (cst:rest form)))
            (parse-error "Malformed repr attribute"
                         (note source (cst:first (cst:rest (cst:rest form)))
                               "unexpected form")))

          (case (keyword-src-name type)
            (:lisp nil)
            (:transparent nil)
            (:enum nil)
            (t
             (parse-error "Unknown repr attribute"
                          (note source (cst:second form)
                                "expected one of :lisp, :transparent, :enum, or :native"))))

          (make-attribute-repr
           :type type
           :arg nil
           :location (form-location source form))))))

(defun parse-struct-field (form source)
  (declare (type cst:cst form)
           (values struct-field))

  (let ((cursor (cursor:make-cursor form source "Malformed struct field")))
    (when (cursor:atom-p cursor)
      (cursor:error cursor ':form "unexpected form"))
    (let ((name (cursor:next-symbol cursor
                                    "missing field name"
                                    "invalid field name (must be a symbol)"))
          (docstring nil))
      (when (stringp (cursor:peek cursor))
        (setf docstring (cursor:next cursor)))
      (unless (cursor:peek cursor)
        (cursor:error cursor ':after-last "expected field type"))
      (let ((type (cursor:next cursor :unwrap nil)))
        (when (cursor:peek cursor :unwrap nil)
          (cursor:error cursor ':next "unexpected trailing form"))

        (make-struct-field :name (symbol-name name)
                           :type (parse-type type source)
                           :docstring docstring
                           :location (form-location source form))))))
