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
   (#:se #:source-error)
   (#:util #:coalton-impl/util))
  (:export
   #:attribute                                   ; TYPE
   #:attribute-source                            ; ACCESSOR
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
   #:constructor-source                          ; ACCESSOR
   #:constructor-list                            ; TYPE
   #:toplevel-define-type                        ; STRUCT
   #:make-toplevel-define-type                   ; CONSTRUCTOR
   #:toplevel-define-type-name                   ; ACCESSOR
   #:toplevel-define-type-vars                   ; ACCESSOR
   #:toplevel-define-type-docstring              ; ACCESSOR
   #:toplevel-define-type-ctors                  ; ACCESSOR
   #:toplevel-define-type-source                 ; ACCESSOR
   #:toplevel-define-type-repr                   ; ACCESSOR
   #:toplevel-define-type-head-src               ; ACCESSOR
   #:toplevel-define-type-list                   ; TYPE
   #:struct-field                                ; STRUCT
   #:make-struct-field                           ; CONSTRUCTOR
   #:struct-field-name                           ; ACCESSOR
   #:struct-field-type                           ; ACCESSOR
   #:struct-field-docstring                      ; ACCESSOR
   #:struct-field-source                         ; ACCESSOR
   #:struct-field-list                           ; TYPE
   #:toplevel-define-struct                      ; STRUCT
   #:make-toplevel-define-struct                 ; CONSTRUCTOR
   #:toplevel-define-struct-name                 ; ACCESSOR
   #:toplevel-define-struct-vars                 ; ACCESSOR
   #:toplevel-define-struct-docstring            ; ACCESSOR
   #:toplevel-define-struct-fields               ; ACCESSOR
   #:toplevel-define-struct-source               ; ACCESSOR
   #:toplevel-define-struct-repr                 ; ACCESSOR
   #:toplevel-define-struct-head-src             ; ACCESSOR
   #:toplevel-define-struct-list                 ; TYPE
   #:toplevel-declare                            ; STRUCT
   #:make-toplevel-declare                       ; CONSTRUCTOR
   #:toplevel-declare-name                       ; ACCESSOR
   #:toplevel-declare-type                       ; ACCESSOR
   #:toplevel-declare-source                     ; ACCESSOR
   #:toplevel-declare-list                       ; TYPE
   #:toplevel-declare-monomorphize               ; ACCESSOR
   #:toplevel-define                             ; STRUCT
   #:make-toplevel-define                        ; CONSTRUCTOR
   #:toplevel-define-name                        ; ACCESSOR
   #:toplevel-define-params                      ; ACCESSOR
   #:toplevel-define-orig-params                 ; ACCESSOR
   #:toplevel-define-docstring                   ; ACCESSOR
   #:toplevel-define-body                        ; ACCESSOR
   #:toplevel-define-source                      ; ACCESSOR
   #:toplevel-define-monomorphize                ; ACCESSOR
   #:toplevel-define-list                        ; TYPE
   #:fundep                                      ; STRUCT
   #:make-fundep                                 ; CONSTRUCTOR
   #:fundep-left                                 ; ACCESSOR
   #:fundep-right                                ; ACCESSOR
   #:fundep-source                               ; ACCESSOR
   #:fundep-list                                 ; TYPE
   #:method-definition                           ; STRUCT
   #:make-method-definition                      ; STRUCT
   #:method-definition-name                      ; ACCESSOR
   #:method-definition-type                      ; ACCESSOR
   #:method-definition-docstring                 ; ACCESSOR
   #:method-definition-source                    ; ACCESSOR
   #:method-definition-list                      ; TYPE
   #:toplevel-define-class                       ; STRUCT
   #:make-toplevel-define-class                  ; CONSTRUCTOR
   #:toplevel-define-class-name                  ; ACCESSOR
   #:toplevel-define-class-vars                  ; ACCESSOR
   #:toplevel-define-class-preds                 ; ACCESSOR
   #:toplevel-define-class-fundeps               ; ACCESSOR
   #:toplevel-define-class-docstring             ; ACCESSOR
   #:toplevel-define-class-methods               ; ACCESSOR
   #:toplevel-define-class-method-docstrings     ; ACCESSOR
   #:toplevel-define-class-source                ; ACCESSOR
   #:toplevel-define-class-head-src              ; ACCESSOR
   #:toplevel-define-class-list                  ; TYPE
   #:instance-method-definition                  ; STRUCT
   #:make-instance-method-definition             ; CONSTRUCTOR
   #:instance-method-definition-name             ; ACCESSOR
   #:instance-method-definition-params           ; ACCESSOR
   #:instance-method-definition-body             ; ACCESSOR
   #:instance-method-definition-source           ; ACCESSOR
   #:instance-method-definition-list             ; TYPE
   #:toplevel-define-instance                    ; STRUCT
   #:make-toplevel-define-instance               ; CONSTRUCTOR
   #:toplevel-define-instance-context            ; ACCESSOR
   #:toplevel-define-instance-pred               ; ACCESSOR
   #:toplevel-define-instance-methods            ; ACCESSOR
   #:toplevel-define-instance-source             ; ACCESSOR
   #:toplevel-define-instance-head-src           ; ACCESSOR
   #:toplevel-define-instance-docstring          ; ACCESSOR
   #:toplevel-define-instance-compiler-generated ; ACCESSOR
   #:toplevel-define-instance-list               ; TYPE
   #:toplevel-package-name                       ; ACCESSOR
   #:toplevel-specialize                         ; STRUCT
   #:make-toplevel-specialize                    ; CONSTRUCTOR
   #:toplevel-specialize-from                    ; ACCESSOR
   #:toplevel-specialize-to                      ; ACCESSOR
   #:toplevel-specialize-type                    ; ACCESSOR
   #:toplevel-specialize-source                  ; ACCESSOR
   #:toplevel-specialize-list                    ; TYPE
   #:program                                     ; STRUCT
   #:make-program                                ; CONSTRUCTOR
   #:program-package                             ; ACCESSOR
   #:program-types                               ; ACCESSOR
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
;;;; constructor := identifier
;;;;              | "(" identifier ty+ ")"
;;;;
;;;; toplevel-define-type := "(" "define-type" identifier docstring? constructor* ")"
;;;;                       | "(" "define-type" "(" identifier keyword+ ")" docstring? constructor* ")"
;;;;
;;;; struct-field := "(" identifier docstring? type)"
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
;;;; toplevel-specialize := "(" identifier identifier ty ")"

;;
;; Attributes
;;

(defstruct (attribute
            (:constructor nil)
            (:copier nil))
  (source (util:required 'source) :type source-location :read-only t))

(defstruct (attribute-monomorphize
            (:include attribute)))

(defstruct (attribute-repr
            (:include attribute))
  (type (util:required 'type) :type keyword-src       :read-only t)
  (arg  (util:required 'arg)  :type (or null cst:cst) :read-only t))

;;
;; Toplevel Structures
;;

(defstruct (constructor
            (:copier nil))
  (name   (util:required 'name)   :type identifier-src  :read-only t)
  (fields (util:required 'fields) :type ty-list         :read-only t)
  (source (util:required 'source) :type source-location :read-only t))

(defun constructor-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'constructor-p x)))

(deftype constructor-list ()
  '(satisfies constructor-list-p))

(defstruct (toplevel-define-type
            (:copier nil))
  (name      (util:required 'name)      :type identifier-src           :read-only t)
  (vars      (util:required 'vars)      :type keyword-src-list         :read-only t)
  (docstring (util:required 'docstring) :type (or null string)         :read-only t)
  (ctors     (util:required 'ctors)     :type constructor-list         :read-only t)
  (source    (util:required 'source)    :type source-location          :read-only t)
  (repr      (util:required 'repr)      :type (or null attribute-repr) :read-only nil)
  (head-src  (util:required 'head-src)  :type source-location                     :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-define-type-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-define-type-p x))))

(deftype toplevel-define-type-list ()
  '(satisfies toplevel-define-type-list-p))

(defstruct (struct-field
            (:copier nil))
  (name      (util:required 'name)      :type string           :read-only t)
  (type      (util:required 'type)      :type ty               :read-only t)
  (docstring (util:required 'docstring) :type (or null string) :read-only t)
  (source    (util:required 'source)    :type source-location             :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun struct-field-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'struct-field-p x))))

(deftype struct-field-list ()
  '(satisfies struct-field-list-p))

(defstruct (toplevel-define-struct
            (:copier nil))
  (name      (util:required 'name)      :type identifier-src           :read-only t)
  (vars      (util:required 'vars)      :type keyword-src-list         :read-only t)
  (docstring (util:required 'docstring) :type (or null string)         :read-only t)
  (fields    (util:required 'fields)    :type struct-field-list        :read-only t)
  (source    (util:required 'source)    :type source-location          :read-only t)
  (repr      (util:required 'repr)      :type (or null attribute-repr) :read-only nil)
  (head-src  (util:required 'head-src)  :type source-location          :read-only t))

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
  (source       (util:required 'source)       :type source-location                  :read-only t)
  (monomorphize (util:required 'monomorphize) :type (or null attribute-monomorphize) :read-only nil))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-declare-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-declare-p x))))

(deftype toplevel-declare-list ()
  '(satisfies toplevel-declare-list-p))

(defstruct (toplevel-define
            (:copier nil))
  (name         (util:required 'name)         :type node-variable                    :read-only t)
  (params       (util:required 'params)       :type pattern-list                     :read-only t)
  (orig-params  (util:required 'orig-params)  :type pattern-list                     :read-only t)
  (docstring    (util:required 'docstring)    :type (or null string)                 :read-only t)
  (body         (util:required 'body)         :type node-body                        :read-only t)
  (source       (util:required 'source)       :type source-location                  :read-only t)
  (monomorphize (util:required 'monomorphize) :type (or null attribute-monomorphize) :read-only nil))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-define-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-define-p x))))

(deftype toplevel-define-list ()
  '(satisfies toplevel-define-list-p))

(defstruct (fundep
            (:copier nil))
  (left   (util:required 'left)   :type keyword-src-list :read-only t)
  (right  (util:required 'right)  :type keyword-src-list :read-only t)
  (source (util:required 'source) :type source-location  :read-only t))

(defun fundep-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'fundep-p x)))

(deftype fundep-list ()
  '(satisfies fundep-list-p))

(defstruct (method-definition
            (:copier nil))
  (name      (util:required 'name)      :type identifier-src   :read-only t)
  (type      (util:required 'type)      :type qualified-ty     :read-only t)
  (docstring (util:required 'docstring) :type (or string null) :read-only t)
  (source    (util:required 'source)    :type source-location  :read-only t))

(defmethod make-load-form ((self method-definition) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun method-definition-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'method-definition-p x)))

(deftype method-definition-list ()
  '(satisfies method-definition-list-p))

(defstruct (toplevel-define-class
            (:copier nil))
  (name      (util:required 'name)      :type identifier-src         :read-only t)
  (vars      (util:required 'vars)      :type keyword-src-list       :read-only t)
  (preds     (util:required 'preds)     :type ty-predicate-list      :read-only t)
  (fundeps   (util:required 'fundeps)   :type fundep-list            :read-only t)
  (docstring (util:required 'docstring) :type (or null string)       :read-only t)
  (methods   (util:required 'methods)   :type method-definition-list :read-only t)
  (source    (util:required 'source)    :type source-location        :read-only t)
  ;; Source information for context, name, and vars
  (head-src  (util:required 'head-src) :type source-location         :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-define-class-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-define-class-p x))))

(deftype toplevel-define-class-list ()
  '(satisfies toplevel-define-class-list-p))

(defstruct (instance-method-definition
            (:copier nil))
  (name      (util:required 'name)      :type node-variable       :read-only t)
  (params    (util:required 'params)    :type pattern-list        :read-only t)
  (body      (util:required 'body)      :type node-body           :read-only t)
  (source    (util:required 'source)    :type source-location     :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun instance-method-definition-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'instance-method-definition-p x))))

(deftype instance-method-definition-list ()
  '(satisfies instance-method-definition-list-p))

(defstruct (toplevel-define-instance
            (:copier nil))
  (context            (util:required 'context)            :type ty-predicate-list               :read-only t)
  (pred               (util:required 'pred)               :type ty-predicate                    :read-only t)
  (docstring          (util:required 'docstring)          :type (or null string)                :read-only t)
  (methods            (util:required 'methods)            :type instance-method-definition-list :read-only t)
  (source             (util:required 'source)             :type source-location                 :read-only t)
  ;; Source information for the context and the pred
  (head-src           (util:required 'head-src)           :type source-location                 :read-only t)
  (compiler-generated (util:required 'compiler-generated) :type boolean                         :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-define-instance-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-define-instance-p x))))

(deftype toplevel-define-instance-list ()
  '(satisfies toplevel-define-instance-list-p))

(defstruct (toplevel-specialize
            (:copier nil))
  (from   (util:required 'from)   :type node-variable :read-only t)
  (to     (util:required 'to)     :type node-variable :read-only t)
  (type   (util:required 'type)   :type ty            :read-only t)
  (source (util:required 'source) :type source-location          :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-specialize-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-specialize-p x))))

(deftype toplevel-specialize-list ()
  '(satisfies toplevel-specialize-list-p))

(defstruct (toplevel-package
            (:copier nil))
  "A Coalton package definition, which can be used to generate either a DEFPACKAGE form or a package instance directly."
  (name        (util:required 'name) :type string           :read-only t)
  (docstring   nil                   :type (or null string) :read-only t)
  (import      nil                   :type list)
  (import-as   nil                   :type list)
  (import-from nil                   :type list)
  (export      nil                   :type list))

(defstruct program
  (package         nil :type (or null toplevel-package)    :read-only t)
  (types           nil :type toplevel-define-type-list     :read-only nil)
  (structs         nil :type toplevel-define-struct-list   :read-only nil)
  (declares        nil :type toplevel-declare-list         :read-only nil)
  (defines         nil :type toplevel-define-list          :read-only nil)
  (classes         nil :type toplevel-define-class-list    :read-only nil)
  (instances       nil :type toplevel-define-instance-list :read-only nil)
  (specializations nil :type toplevel-specialize-list      :read-only nil))

(defun read-program (stream file &optional mode)
  "Read a PROGRAM from FILE (an instance of source-error:file).
MODE may be one of :file or :macro.

If MODE is :file, a package form is required.
If MODE is :macro, a package form is forbidden, and an explicit check is made for EOF."
  (declare (type se:file file)
           (type (member :file :macro nil) mode)
           (values program))

  (let* ((eclector.readtable:*readtable*
           (eclector.readtable:copy-readtable eclector.readtable:*readtable*))

         ;; In mode :file, the value of package is a toplevel-package structure
         (package
           (when (eq mode ':file)
             (read-toplevel-package stream file)))
         (program
           (make-program :package package))
         (*package*
           (program-lisp-package program))
         (attributes
           (make-array 0 :adjustable t :fill-pointer t)))

    (loop :do
      (multiple-value-bind (form presentp eofp)
          (maybe-read-form stream file *coalton-eclector-client*)

        (when (and eofp (eq mode ':macro))
          (error 'parse-error
                 :err (se:source-error
                       :span (cons (- (file-position stream) 2)
                                   (- (file-position stream) 1))
                       :file file
                       :message "Unexpected EOF"
                       :primary-note "missing close parenthesis")))

        (unless presentp
          (return))

        (when (and (parse-toplevel-form form program attributes file)
                   (plusp (length attributes)))
          (util:coalton-bug "parse-toplevel-form indicated that a form was parsed but did not consume all attributes"))))

    (unless (zerop (length attributes))
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source (cdr (aref attributes 0)))
                   :file file
                   :message "Orphan attribute"
                   :primary-note "attribute must be attached to another form")))

    (setf (program-types program) (nreverse (program-types program)))
    (setf (program-structs program) (nreverse (program-structs program)))
    (setf (program-declares program) (nreverse (program-declares program)))
    (setf (program-defines program) (nreverse (program-defines program)))
    (setf (program-classes program) (nreverse (program-classes program)))
    (setf (program-specializations program) (nreverse (program-specializations program)))

    program))

(defun read-expression (stream file)
  (let* (;; Setup eclector readtable
         (eclector.readtable:*readtable*
           (eclector.readtable:copy-readtable eclector.readtable:*readtable*)))

    ;; Read the coalton form
    (multiple-value-bind (form presentp)
        (maybe-read-form stream file *coalton-eclector-client*)

      (unless presentp
        (error 'parse-error
               :err (se:source-error
                     :span (cons (- (file-position stream) 2)
                                 (- (file-position stream) 1))
                     :file file
                     :message "Malformed coalton expression"
                     :primary-note "missing expression")))

      ;; Ensure there is only one form
      (multiple-value-bind (form presentp)
          (maybe-read-form stream file *coalton-eclector-client*)

        (when presentp
          (error 'parse-error
                 :err (se:source-error
                       :span (cst:source form)
                       :file file
                       :message "Malformed coalton expression"
                       :primary-note "unexpected form"))))

      (parse-expression form file))))

;;; Packages

(defun parse-import-statement (package cursor)
  (typecase (cursor:peek cursor)
    (list (let ((name (cursor:next-symbol cursor
                                          :message "package name must be a symbol"
                                          :missing "package name is missing")))
            (cursor:next-symbol cursor :require "AS"
                                       :missing "missing AS")
            (let ((nick (cursor:next-symbol cursor
                                            :message "package nickname msut be a symbol"
                                            :missing "missing package nickname")))
              (when (not (cursor:empty-p cursor))
                (cursor:syntax-error cursor "unexpected value"))
              (pushnew (list (symbol-name nick)
                             (symbol-name name))
                       (toplevel-package-import-as package)))))
    (symbol (pushnew (symbol-name (cst:raw (cursor:cursor-value cursor)))
                     (toplevel-package-import package)))
    (t (cursor:syntax-error cursor "expected PACKAGE or (PACKAGE as NICK)"))))

(defun parse-import-from (package cursor)
  "Parse an IMPORT-FROM clause: a package designator followed by uninterned symbols."
  (push (cursor:collect-symbols cursor)
        (toplevel-package-import-from package)))

(defun parse-import (package cursor)
  (when (cursor:empty-p cursor)
    (cursor:syntax-error cursor "empty IMPORT form"))
  (cursor:do-every cursor (alexandria:curry 'parse-import-statement package)))

(defun parse-export (package cursor)
  (setf (toplevel-package-export package)
        (append (toplevel-package-export package)
                (cursor:collect-symbols cursor))))

(defvar *package-clauses*
  '(("IMPORT" . parse-import)
    ("IMPORT-FROM" . parse-import-from)
    ("EXPORT" . parse-export)))

(defun package-clause-parser (name)
  (cdr (assoc name *package-clauses* :test #'string-equal)))

(defun parse-package-clause (package cursor)
  "Parse a package clause form CURSOR and add it to PACKAGE."
  (unless (consp (cursor:peek cursor))
    (cursor:syntax-error cursor "malformed package clause"))
  (let ((parser (package-clause-parser (cursor:next-symbol cursor))))
    (when (null parser)
      (let ((form (cursor:cursor-value cursor)))
        (error 'cursor:syntax-error
               :notes (list (cursor:make-note :span (cst:source form)
                                              :text "Unknown package clause")
                            (cursor:make-note :span (cst:source (cst:first form))
                                              :text (format nil "Must be one of ~{~a~^, ~}"
                                                            (mapcar #'car *package-clauses*))
                                              :type ':help)))))
    (funcall parser package cursor)))

(defun parse-package (cursor)
  "Parse a coalton package declaration."
  (cursor:next-symbol cursor
                      :require "PACKAGE"
                      :message "package declarations must start with `package`"
                      :missing "missing `package`")
  (let* ((package-name
           (cursor:next-symbol cursor
                               :missing "missing package name"
                               :message "package name must be a symbol"))
         (package-doc
           (unless (cursor:empty-p cursor)
             (cursor:next cursor :pred #'stringp)))
         (package
           (make-toplevel-package :name (symbol-name package-name)
                                  :docstring package-doc)))
    (cursor:do-every cursor (alexandria:curry 'parse-package-clause package))
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

(defun read-toplevel-package (stream file)
  "Read and parse a Coalton toplevel package form."
  (with-parser-package
    (handler-bind ((cursor:syntax-error
                     (lambda (syntax-error)
                       (cursor:parse-error file
                                           "Malformed package declaration"
                                           syntax-error))))
      (multiple-value-bind (form presentp)
          (maybe-read-form stream file *coalton-eclector-client*)
        (unless presentp
          (cursor:span-error (cons (- (file-position stream) 2)
                                   (- (file-position stream) 1))
                      "missing package form"))
        (parse-package (cursor:make-cursor form))))))

(defun make-defpackage (package)
  "Generate a Lisp defpackage form from a Caolton toplevel-package structure."
  `(defpackage ,(toplevel-package-name package)
     (:use "COALTON" ,@(toplevel-package-import package))
     ,@(mapcar (lambda (form)
                 `(:import-from ,@form))
               (toplevel-package-import-from package))
     (:local-nicknames ,@(toplevel-package-import-as package))
     (:export ,@(toplevel-package-export package))))

;; The version of uiop:define-package that ships with SBCL doesn't
;; support package local nicknames. This could be remedied by
;; depending on trivial-package-nicknames, but evalling a defpackage
;; form is good enough to get predictable behavior from supported
;; platforms. This may be worth revisiting in the future if there trun
;; out to be places where package definition behavior should diverge
;; from that of common lisp.

(defun lisp-package (package)
  "Create a Lisp package by evaluating the defpackage representation of a PACKAGE form."
  (eval (make-defpackage package)))

(defun program-lisp-package (program)
  "Return the Lisp package associated with PROGRAM, or current *PACKAGE* if none was specified."
  (if (program-package program)
      (lisp-package (program-package program))
      *package*))

;; end package support

(defun parse-toplevel-form (form program attributes file)
  (declare (type cst:cst form)
           (type program program)
           (type (vector (cons attribute cst:cst)) attributes)
           (type se:file file)
           (values boolean &optional))

  (when (cst:atom form)
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Malformed toplevel form"
                 :primary-note "Unexpected atom")))

  ;; Toplevel forms must begin with an atom
  (when (cst:consp (cst:first form))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source (cst:first form))
                 :file file
                 :message "Malformed toplevel form"
                 :primary-note "unexpected list")))

  (case (cst:raw (cst:first form))
    ((coalton:monomorphize)
     (vector-push-extend
      (cons
       (parse-monomorphize form file)
       form)
      attributes)
     nil)

    ((coalton:repr)
     (vector-push-extend
      (cons
       (parse-repr form file)
       form)
      attributes)
     nil)

    ((coalton:define)
     (let ((define (parse-define form file))

           monomorphize
           monomorphize-form)
       (loop :for (attribute . attribute-form) :across attributes
             :do (etypecase attribute
                   (attribute-repr
                    (error 'parse-error
                           :err (se:source-error
                                 :span (cst:source attribute-form)
                                 :file file
                                 :message "Invalid target for repr attribute"
                                 :primary-note "repr must be attached to a define-type"
                                 :notes
                                 (list
                                  (se:make-source-error-note
                                   :type :secondary
                                   :span (source-location-span (node-source (toplevel-define-name define)))
                                   :message "when parsing define")))))

                   (attribute-monomorphize
                    (when monomorphize
                      (error 'parse-error
                             :err (se:source-error
                                   :span (cst:source attribute-form)
                                   :file file
                                   :message "Duplicate monomorphize attribute"
                                   :primary-note "monomorphize attribute here"
                                   :notes
                                   (list
                                    (se:make-source-error-note
                                     :type :secondary
                                     :span (cst:source monomorphize-form)
                                     :message "previous attribute here")
                                    (se:make-source-error-note
                                     :type :secondary
                                     :span (source-location-span (node-source (toplevel-define-name define)))
                                     :message "when parsing define")))))

                    (setf monomorphize attribute)
                    (setf monomorphize-form attribute-form))))

       (setf (fill-pointer attributes) 0)
       (setf (toplevel-define-monomorphize define) monomorphize)
       (push define (program-defines program))
       t))

    ((coalton:declare)
     (let ((declare (parse-declare form file))

           monomorphize
           monomorphize-form)

       (loop :for (attribute . attribute-form) :across attributes
             :do (etypecase attribute
                   (attribute-repr
                    (error 'parse-error
                           :err (se:source-error
                                 :span (cst:source attribute-form)
                                 :file file
                                 :message "Invalid target for repr attribute"
                                 :primary-note "repr must be attached to a define-type"
                                 :notes
                                 (list
                                  (se:make-source-error-note
                                   :type :secondary
                                   :span (cst:source form)
                                   :message "when parsing declare")))))

                   (attribute-monomorphize
                    (when monomorphize
                      (error 'parse-error
                             :err (se:source-error
                                   :span (cst:source attribute-form)
                                   :file file
                                   :message "Duplicate monomorphize attribute"
                                   :primary-note "monomorphize attribute here"
                                   :notes
                                   (list
                                    (se:make-source-error-note
                                     :type :secondary
                                     :span (cst:source monomorphize-form)
                                     :message "previous attribute here")
                                    (se:make-source-error-note
                                     :type :secondary
                                     :span (cst:source form)
                                     :message "when parsing declare")))))

                    (setf monomorphize attribute)
                    (setf monomorphize-form attribute-form))))

       (setf (fill-pointer attributes) 0)
       (setf (toplevel-declare-monomorphize declare) monomorphize)
       (push declare (program-declares program))
       t))

    ((coalton:define-type)
     (let* ((type (parse-define-type form file))

            repr
            repr-form)

       (loop :for (attribute . attribute-form) :across attributes
             :do (etypecase attribute
                   (attribute-repr
                    (when repr
                      (error 'parse-error
                             :err (se:source-error
                                   :span (cst:source attribute-form)
                                   :file file
                                   :message "Duplicate repr atttribute"
                                   :primary-note "repr attribute here"
                                   :notes
                                   (list
                                    (se:make-source-error-note
                                     :type :secondary
                                     :span (cst:source repr-form)
                                     :message "previous attribute here")
                                    (se:make-source-error-note
                                     :type :secondary
                                     :span (source-location-span (toplevel-define-type-head-src type))
                                     :message "when parsing define-type")))))

                    (setf repr attribute)
                    (setf repr-form attribute-form))

                   (attribute-monomorphize
                    (error 'parse-error
                           :err (se:source-error
                                 :span (cst:source attribute-form)
                                 :file file
                                 :message "Invalid target for monomorphize attribute"
                                 :primary-note "monomorphize must be attached to a define or declare form"
                                 :notes
                                 (list
                                  (se:make-source-error-note
                                   :type :secondary
                                   :span (source-location-span (toplevel-define-type-head-src type))
                                   :message "when parsing define-type")))))))

       (setf (fill-pointer attributes) 0)
       (setf (toplevel-define-type-repr type) repr)
       (push type (program-types program))
       t))

    ((coalton:define-struct)

     (let ((struct (parse-define-struct form file))
           repr
           repr-form)

       (loop :for (attribute . attribute-form) :across attributes
             :do (etypecase attribute
                   (attribute-repr
                    (when repr
                      (error 'parse-error
                             :err (se:source-error
                                   :span (cst:source attribute-form)
                                   :file file
                                   :message "Duplicate repr attribute"
                                   :primary-note "repr attribute here"
                                   :notes
                                   (list
                                    (se:make-source-error-note
                                     :type :secondary
                                     :span (cst:source repr-form)
                                     :message "previous attribute here")
                                    (se:make-source-error-note
                                     :type :secondary
                                     :span (source-location-span (toplevel-define-struct-head-src struct) )
                                     :message "when parsing define-struct")))))

                    (unless (eq :transparent (keyword-src-name (attribute-repr-type attribute)))
                      (error 'parse-error
                             :err (se:source-error
                                   :span (cst:source attribute-form)
                                   :file file
                                   :message "Invalid repr attribute"
                                   :primary-note "structs can only be repr transparent"
                                   :notes
                                   (list
                                    (se:make-source-error-note
                                     :type :secondary
                                     :span (source-location-span (toplevel-define-struct-head-src struct))
                                     :message "when parsing define-struct")))))

                    (setf repr attribute)
                    (setf repr-form attribute-form))

                   (attribute-monomorphize
                    (error 'parse-error
                           :err (se:source-error
                                 :span (cst:source attribute-form)
                                 :file file
                                 :message "Invalid target for monomorphize attribute"
                                 :primary-note "monomorphize must be attached to a define or declare form"
                                 :notes
                                 (list
                                  (se:make-source-error-note
                                   :type :secondary
                                   :span (source-location-span (identifier-src-source (toplevel-define-struct-name struct)))
                                   :message "when parsing define-type")))))))

       (setf (fill-pointer attributes) 0)
       (setf (toplevel-define-struct-repr struct) repr)
       (push struct (program-structs program))
       t))

    ((coalton:define-class)
     (let ((class (parse-define-class form file)))

       (unless (zerop (length attributes))
         (error 'parse-error
                :err (se:source-error
                      :span (cst:source (cdr (aref attributes 0)))
                      :file file
                      :message "Invalid attribute for define-class"
                      :primary-note "define-class cannot have attributes"
                      :notes
                      (list
                       (se:make-source-error-note
                        :type :secondary
                        :span (source-location-span (toplevel-define-class-head-src class))
                        :message "while parsing define-class")))))

       (push class (program-classes program))
       t))

    ((coalton:define-instance)
     (let ((instance (parse-define-instance form file)))

       (unless (zerop (length attributes))
         (error 'parse-error
                :err (se:source-error
                      :span (cst:source (cdr (aref attributes 0)))
                      :file file
                      :message "Invalid attribute for define-instance"
                      :primary-note "define-instance cannot have attributes"
                      :notes
                      (list
                       (se:make-source-error-note
                        :type :secondary
                        :span (source-location-span (toplevel-define-instance-head-src instance))
                        :message "while parsing define-instance")))))


       (push instance (program-instances program))
       t))

    ((coalton:specialize)
     (let ((spec (parse-specialize form file)))

       (unless (zerop (length attributes))
         (error 'parse-error
                :err (se:source-error
                      :span (cst:source (cdr (aref attributes 0)))
                      :file file
                      :message "Invalid attribute for specialize"
                      :primary-note "specialize cannot have attributes"
                      :notes
                      (list
                       (se:make-source-error-note
                        :type :secondary
                        :span (cst:source form)
                        :message "when parsing specialize")))))

       (push spec (program-specializations program))
       t))

    ((coalton:progn)
     (unless (zerop (length attributes))
       (error 'parse-error
              :err (se:source-error
                    :span (cst:source (cdr (aref attributes 0)))
                    :file file
                    :message "Invalid attribute for progn"
                    :primary-note "progn cannot have attributes"
                    :notes
                    (list
                     (se:make-source-error-note
                      :type :secondary
                      :span (cst:source form)
                      :message "when parsing progn")))))

     (loop :for inner-form := (cst:rest form) :then (cst:rest inner-form)
           :while (not (cst:null inner-form)) :do
             (when (and (parse-toplevel-form (cst:first inner-form) program attributes file)
                        (plusp (length attributes)))
               (util:coalton-bug "parse-toplevel-form indicated that a form was parsed but did not
consume all attributes")))

     (unless (zerop (length attributes))
       (error 'parse-error
              :err (se:source-error
                    :span (cst:source (cdr (aref attributes 0)))
                    :file file
                    :message "Trailing attributes in progn"
                    :primary-note "progn cannot have trailing attributes"
                    :notes
                    (list
                     (se:make-source-error-note
                      :type :secondary
                      :span (cst:source form)
                      :message "when parsing progn")))))
     t)

    (t
     (cond
       ((and (cst:atom (cst:first form))
             (symbolp (cst:raw (cst:first form)))
             (macro-function (cst:raw (cst:first form))))
        (let ((se:*source-error-context*
                (adjoin (se:make-source-error-context
                         :message "Error occurs within macro context. Source locations may be imprecise")
                        se:*source-error-context*
                        :test #'equalp)))
          (parse-toplevel-form (expand-macro form) program attributes file)))

       ((error 'parse-error
               :err (se:source-error
                     :span (cst:source (cst:first form))
                     :file file
                     :message "Invalid toplevel form"
                     :primary-note "unknown toplevel form")))))))


(defun parse-define (form file)
  (declare (type cst:cst form)
           (type se:file file)
           (values toplevel-define))

  (assert (cst:consp form))

  ;; (define)
  (unless (cst:consp (cst:rest form))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Malformed definition"
                 :primary-note "expected define body")))

  ;; (define x)
  (unless (cst:consp (cst:rest (cst:rest form)))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Malformed definition"
                 :primary-note "expected value")))

  (multiple-value-bind (name params)
      (parse-argument-list (cst:second form) file)

    (multiple-value-bind (docstring body)
        (parse-definition-body (cst:rest (cst:rest form)) form file)

      (make-toplevel-define
       :name name
       :params params
       :orig-params params
       :docstring docstring
       :body body
       :monomorphize nil
       :source (source-location form file)))))

(defun parse-declare (form file)
  (declare (type cst:cst form)
           (type se:file file)
           (values toplevel-declare))

  (assert (cst:consp form))

  ;; (declare)
  (unless (cst:consp (cst:rest form))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Malformed declaration"
                 :primary-note "expected body")))

  ;; (declare x)
  (unless (cst:consp (cst:rest (cst:rest form)))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Malformed declaration"
                 :primary-note "expected declared type")))

  ;; (declare x y z)
  (when (cst:consp (cst:rest (cst:rest (cst:rest form))))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source (cst:first (cst:rest (cst:rest (cst:rest form)))))
                 :file file
                 :message "Malformed declaration"
                 :primary-note "unexpected trailing form")))

  ;; (declare 0.5 x)
  (unless (identifierp (cst:raw (cst:second form)))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source (cst:second form))
                 :file file
                 :message "Malformed declaration"
                 :primary-note "expected symbol")))

  (make-toplevel-declare
   :name (make-identifier-src
          :name (cst:raw (cst:second form))
          :source (source-location (cst:second form) file))
   :type (parse-qualified-type (cst:third form) file)
   :monomorphize nil
   :source (source-location form file)))

(defun parse-define-type (form file)
  (declare (type cst:cst form)
           (type se:file file)
           (values toplevel-define-type))

  (assert (cst:consp form))

  (let (name
        docstring
        variables)
    (declare (type (or null identifier-src) name)
             (type keyword-src-list variables))

    ;; (define-type)
    (unless (cst:consp (cst:rest form))
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source form)
                   :file file
                   :message "Malformed type definition"
                   :primary-note "expected body")))

    (cond
      ((cst:atom (cst:second form))
       (unless (identifierp (cst:raw (cst:second form)))
         (error 'parse-error
                :err (se:source-error
                      :span (cst:source (cst:second form))
                      :file file
                      :message "Malformed type definition"
                      :primary-note "expected symbol")))

       (setf name (make-identifier-src :name (cst:raw (cst:second form))
                                       :source (source-location form file))))

      (t                                ; (define-type (T ...) ...)
       ;; (define-type ((T) ...) ...)
       (unless (cst:atom (cst:first (cst:second form)))
         (error 'parse-error
                :err (se:source-error
                      :span (cst:source (cst:first (cst:second form)))
                      :file file
                      :message "Malformed type definition"
                      :primary-note "expected symbol"
                      :help-notes
                      (list
                       (se:make-source-error-help
                        :span (cst:source (cst:second form))
                        :replacement
                        (lambda (existing)
                          (subseq existing 1 (1- (length existing))))
                        :message "remove parentheses")))))

       ;; (define-type (1 ...) ...)
       (unless (identifierp (cst:raw (cst:first (cst:second form))))
         (error 'parse-error
                :err (se:source-error
                      :span (cst:source (cst:first (cst:second form)))
                      :file file
                      :message "Malformed type definition"
                      :primary-note "expected symbol")))

       (setf name (make-identifier-src :name (cst:raw (cst:first (cst:second form)))
                                       :source (source-location (cst:first (cst:second form)) file)))

       ;; (define-type (T) ...)
       (when (cst:atom (cst:rest (cst:second form)))
         (error 'parse-error
                :err (se:source-error
                      :span (cst:source (cst:second form))
                      :file file
                      :message "Malformed type definition"
                      :primary-note "nullary types should not have parentheses"
                      :help-notes
                      (list
                       (se:make-source-error-help
                        :span (cst:source (cst:second form))
                        :replacement
                        (lambda (existing)
                          (subseq existing 1 (1- (length existing))))
                        :message "remove unnecessary parentheses")))))

       (loop :for vars := (cst:rest (cst:second form)) :then (cst:rest vars)
             :while (cst:consp vars)
             :do (push (parse-type-variable (cst:first vars) file) variables))))

    (when (and (cst:consp (cst:rest (cst:rest form)))
               (cst:atom (cst:third form))
               (stringp (cst:raw (cst:third form))))
      (setf docstring (cst:raw (cst:third form))))

    (make-toplevel-define-type
     :name name
     :vars (reverse variables)
     :docstring docstring
     :ctors (loop :for constructors_ := (cst:nthrest (if docstring 3 2) form) :then (cst:rest constructors_)
                  :while (cst:consp constructors_)
                  :collect (parse-constructor (cst:first constructors_) form file))
     :repr nil
     :source (source-location form file)
     :head-src (source-location (cst:second form) file))))

(defun parse-define-struct (form file)
  (declare (type cst:cst form)
           (type se:file file))

  (assert (cst:consp form))

  (let (unparsed-name
        unparsed-variables
        docstring)

    ;; (define-struct)
    (unless (cst:consp (cst:rest form))
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source form)
                   :file file
                   :message "Malformed struct definition"
                   :primary-note "expected body"
                   :highlight :end)))

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
     :name (parse-identifier unparsed-name file)
     :vars (when unparsed-variables
             (parse-list #'parse-type-variable unparsed-variables file)) 
     :docstring docstring
     :fields (parse-list
              #'parse-struct-field
              (cst:nthrest (if docstring 3 2) form)
              file)
     :source (source-location form file)
     :repr nil
     :head-src (source-location (cst:second form) file))))

(defun parse-define-class (form file)
  (declare (type cst:cst form)
           (type se:file file)
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
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source form)
                   :file file
                   :message "Malformed class definition"
                   :primary-note "expected body")))

    ;; (define-class C)
    (unless (cst:consp (cst:second form))
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source (cst:second form))
                   :file file
                   :message "Malformed class definition"
                   :primary-note "expected class type variable(s)"
                   :help-notes
                   (list
                    (se:make-source-error-help
                     :span (cst:source (cst:second form))
                     :replacement
                     (lambda (existing)
                       (concatenate 'string "(" existing " :a)"))
                     :message "add class type variable `:a`")))))

    (unless (cst:proper-list-p (cst:second form))
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source (cst:second form))
                   :file file
                   :message "Malformed class definition"
                   :primary-note "unexpected dotted list")))

    (multiple-value-bind (left right)
        (util:take-until (lambda (cst)
                           (and (cst:atom cst)
                                (eq (cst:raw cst) 'coalton:=>)))
                         (cst:listify (cst:second form)))

      ;; (=> C ...)
      (when (and (null left) right)
        (error 'parse-error
               :err (se:source-error
                     :span (cst:source (cst:first (cst:second form)))
                     :file file
                     :message "Malformed class definition"
                     :primary-note "unnecessary `=>`"
                     :help-notes
                     (cond
                       ;; If this is the only thing in the list then don't suggest anything
                       ((cst:atom (cst:rest (cst:second form)))
                        nil)
                       ;; If there is nothing to the right of C then emit without list
                       ((cst:atom (cst:rest (cst:rest (cst:second form))))
                        (list
                         (se:make-source-error-help
                          :span (cst:source (cst:second form))
                          :replacement
                          (lambda (existing)
                            (subseq existing 4 (1- (length existing))))
                          :message "remove `=>`")))
                       (t
                        (list
                         (se:make-source-error-help
                          :span (cst:source (cst:second form))
                          :replacement
                          (lambda (existing)
                            (concatenate 'string
                                         (subseq existing 0 1)
                                         (subseq existing 4)))
                          :message "remove `=>`")))))))

      ;; (... =>)
      (when (and left right (null (cdr right)))
        (error 'parse-error
               :err (se:source-error
                     :span (cst:source (cst:second form))
                     :file file
                     :message "Malformed class definition"
                     :primary-note "missing class name")))

      (cond
        ;; No predicates
        ((null right)
         (setf unparsed-name (first left))
         (setf unparsed-variables (rest left)))

        ;; (... => (...) ...)
        ((and (cst:consp (second right))
              (consp (cdr (cdr right))))
         (error 'parse-error
                :err (se:source-error
                      :span (cst:source (third right))
                      :file file
                      :message "Malformed class definition"
                      :primary-note "unexpected form")))

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
        (error 'parse-error
               :err (se:source-error
                     :span (cst:source unparsed-name)
                     :file file
                     :message "Malformed class definition"
                     :primary-note "unnecessary parentheses"
                     :help-notes
                     (list
                      (se:make-source-error-help
                       :span (cst:source unparsed-name)
                       :replacement
                       (lambda (existing)
                         (subseq existing 1 (1- (length existing))))
                       :message "remove unnecessary parentheses")))))

      (unless (identifierp (cst:raw unparsed-name))
        (error 'parse-error
               :err (se:source-error
                     :span (cst:source unparsed-name)
                     :file file
                     :message "Malformed class definition"
                     :primary-note "expected symbol")))

      (setf name (cst:raw unparsed-name))

      (when (null unparsed-variables)
        (error 'parse-error
               :err (se:source-error
                     :span (cst:source unparsed-name)
                     :file file
                     :message "Malformed class definition"
                     :primary-note "expected class type variable(s)"
                     :help-notes
                     (list
                      (se:make-source-error-help
                       :span (cst:source unparsed-name)
                       :replacement
                       (lambda (existing)
                         (if (cst:consp (cst:second form))
                             (concatenate 'string existing " :a")
                             (concatenate 'string "(" existing " :a)")))
                       :message "add class type variable `:a`")))))


      (multiple-value-bind (left right)
          (util:take-until #'cst:consp unparsed-variables)

        (setf variables
              (loop :for var :in left
                    :collect (parse-type-variable var file)))

        (setf fundeps
              (loop :for fundep :in right
                    :collect (parse-fundep fundep file))))

      ;; (... => C ...)
      (when right
        (if (cst:atom (first left))
            ;; (C1 ... => C2 ...)
            (setf predicates (list (parse-predicate left (make-source-location :file file
                                                                               :span (util:cst-source-range left))
                                                    file)))

            ;; ((C1 ...) (C2 ...) ... => C3 ...)
            (setf predicates
                  (loop :for pred :in left
                        :collect (parse-predicate (cst:listify pred) (source-location pred file) file)))))

      (when (and (cst:consp (cst:rest (cst:rest form)))
                 (cst:atom (cst:third form))
                 (stringp (cst:raw (cst:third form))))
        (setf docstring (cst:raw (cst:third form))))

      (setf methods
            (loop :for methods := (cst:nthrest (if docstring 3 2) form) :then (cst:rest methods)
                  :while (cst:consp methods)
                  :collect (parse-method (cst:first methods) form file)))

      (make-toplevel-define-class
       :name (make-identifier-src
              :name name
              :source (source-location unparsed-name file))
       :vars variables
       :preds predicates
       :fundeps fundeps
       :docstring docstring
       :methods methods
       :source (source-location form file)
       :head-src (source-location (cst:second form) file)))))

(defun parse-define-instance (form file)
  (declare (type cst:cst form)
           (type se:file file)
           (values toplevel-define-instance))

  (assert (cst:consp form))

  (let (unparsed-context
        context
        unparsed-predicate
        docstring)

    ;; (define-instance)
    (unless (cst:consp (cst:rest form))
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source form)
                   :file file
                   :highlight :end
                   :message "Malformed instance definition"
                   :primary-note "expected an instance head")))

    ;; (define-instance 5)
    (unless (cst:consp (cst:second form))
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source (cst:second form))
                   :file file
                   :message "Malformed instance definition"
                   :primary-note "expected a list")))

    (unless (cst:proper-list-p (cst:second form))
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source (cst:second form))
                   :file file
                   :message "Malformed instance definition"
                   :primary-note "unexpected dotted list")))

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
         (error 'parse-error
                :err (se:source-error
                      :span (cst:source (third right))
                      :file file
                      :message "Malformed instance definition"
                      :primary-note "unexpected form")))

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
        (error 'parse-error
               :err (se:source-error
                     :span (cst:source (first right))
                     :file file
                     :message "Malformed instance head"
                     :primary-note "unexpected `=>`"
                     :help-notes
                     (list
                      (se:make-source-error-help
                       :span (cst:source (first right))
                       :replacement
                       (lambda (existing)
                         (declare (ignore existing))
                         "")
                       :message "remove the `=>`")))))

      ;; (=> ...)
      (when (and (null left) right)
        (error 'parse-error
               :err (se:source-error
                     :span (cst:source (first right))
                     :file file
                     :message "Malformed instance head"
                     :primary-note "unexpected `=>`"
                     :help-notes
                     (list
                      (se:make-source-error-help
                       :span (cst:source (first right))
                       :replacement
                       (lambda (existing)
                         (declare (ignore existing))
                         "")
                       :message "remove the `=>`")))))

      (when unparsed-context
        (if (cst:atom (first unparsed-context))
            (setf context (list (parse-predicate unparsed-context
                                                 (make-source-location :file file
                                                                       :span (util:cst-source-range unparsed-context))
                                                 file)))

            (setf context
                  (loop :for unparsed :in unparsed-context
                        :collect (parse-predicate (cst:listify unparsed) (source-location unparsed file) file)))))

      (when (and (cst:consp (cst:rest (cst:rest form)))
                 (cst:atom (cst:third form))
                 (stringp (cst:raw (cst:third form))))
        (setf docstring (cst:raw (cst:third form))))

      (make-toplevel-define-instance
       :context context
       :pred (parse-predicate unparsed-predicate
                              (make-source-location :file file
                                                    :span (util:cst-source-range unparsed-predicate))
                              file)
       :docstring docstring
       :methods (loop :for methods := (cst:nthrest (if docstring 3 2) form) :then (cst:rest methods)
                      :while (cst:consp methods)
                      :for method := (cst:first methods)
                      :collect (parse-instance-method-definition method (cst:second form) file))
       :source (source-location form file)
       :head-src (source-location (cst:second form) file)
       :compiler-generated nil))))

(defun parse-specialize (form file)
  (declare (type cst:cst form)
           (type se:file file)
           (values toplevel-specialize))

  (assert (cst:consp form))

  ;; (specialize)
  (unless (cst:consp (cst:rest form))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :highlight :end
                 :message "Malformed specialize declaration"
                 :primary-note "missing from name")))

  ;; (specialize f)
  (unless (cst:consp (cst:rest (cst:rest form)))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :highlight :end
                 :message "Malformed specialize declaration"
                 :primary-note "missing to name")))

  ;; (specialize f f2)
  (unless (cst:consp (cst:rest (cst:rest (cst:rest form))))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :highlight :end
                 :message "Malformed specialize declaration"
                 :primary-note "missing type")))

  ;; (specialize f f2 t ....)
  (when (cst:consp (cst:rest (cst:rest (cst:rest (cst:rest form)))))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source (cst:first (cst:rest (cst:rest (cst:rest (cst:rest form))))))
                 :file file
                 :message "Malformed specialize declaration"
                 :primary-note "unexpected form")))

  (make-toplevel-specialize
   :from (parse-variable (cst:second form) file)
   :to (parse-variable (cst:third form) file)
   :type (parse-type (cst:fourth form) file)
   :source (source-location form file)))

(defun parse-method (method-form form file)
  (declare (type cst:cst method-form)
           (type se:file file)
           (values method-definition))

  ;; m or (m)
  (unless (and (cst:consp method-form)
               (cst:consp (cst:rest method-form)))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source method-form)
                 :file file
                 :message "Malformed method definition"
                 :primary-note "missing method type"
                 :notes
                 (list
                  (se:make-source-error-note
                   :type :secondary
                   :span (cst:source (cst:second form))
                   :message "in this class definition")))))

  ;; (m d t ...)
  (unless (or (cst:null (cst:rest (cst:rest method-form)))
              (cst:null (cst:rest (cst:rest (cst:rest method-form)))))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source (cst:first (cst:rest (cst:rest (cst:rest method-form)))))
                 :file file
                 :message "Malformed method definition"
                 :primary-note "unexpected trailing form"
                 :notes
                 (list
                  (se:make-source-error-note
                   :type :secondary
                   :span (cst:source (cst:second form))
                   :message "in this class definition")))))

  ;; (0.5 t ...)
  (unless (and (cst:atom (cst:first method-form))
               (identifierp (cst:raw (cst:first method-form))))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source (cst:first method-form))
                 :file file
                 :message "Malformed method definition"
                 :primary-note "expected symbol"
                 :notes
                 (list
                  (se:make-source-error-note
                   :type :secondary
                   :span (cst:source (cst:second form))
                   :message "in this class definition")))))

  ;; (m "docstring")
  (when (and (cst:atom (cst:second method-form))
             (stringp (cst:raw (cst:second method-form)))
             (cst:null (cst:rest (cst:rest method-form))))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source (cst:second method-form))
                 :file file
                 :message "Malformed method definition"
                 :primary-note "missing method type"
                 :notes
                 (list
                  (se:make-source-error-note
                   :type :secondary
                   :span (cst:source (cst:second form))
                   :message "in this class definition")))))

  (let (docstring)
    (when (and (cst:atom (cst:second method-form))
               (stringp (cst:raw (cst:second method-form))))
      (setf docstring (cst:raw (cst:second method-form))))

    ;; either list of length 2 or list of length 3 with docstring
    (unless (or (cst:null (cst:rest (cst:rest method-form)))
                (and (cst:atom (cst:second method-form))
                     (stringp (cst:raw (cst:second method-form)))))
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source (if docstring
                                         (cst:fourth method-form)
                                         (cst:third method-form)))
                   :file file
                   :message "Malformed method definition"
                   :primary-note "unexpected trailing form"
                   :notes
                   (list
                    (se:make-source-error-note
                     :type :secondary
                     :span (cst:source (cst:second form))
                     :message "in this class definition")))))

    (make-method-definition
     :name (make-identifier-src
            :name (node-variable-name (parse-variable (cst:first method-form) file))
            :source (source-location (cst:first method-form) file))
     :docstring docstring
     :type (parse-qualified-type (if docstring
                                     (cst:third method-form)
                                     (cst:second method-form))
                                 file)
     :source (source-location method-form file))))

(defun parse-type-variable (form file)
  (declare (type cst:cst form)
           (type se:file file)
           (values keyword-src &optional))

  (when (cst:consp form)
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Invalid type variable"
                 :primary-note "expected keyword symbol")))

  (unless (keywordp (cst:raw form))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Invalid type variable"
                 :primary-note "expected keyword symbol"
                 :help-notes
                 (list
                  (se:make-source-error-help
                   :span (cst:source form)
                   :replacement
                   (lambda (existing)
                     (concatenate 'string ":" existing))
                   :message "add `:` to symbol")))))

  (make-keyword-src
   :name (cst:raw form)
   :source (source-location form file)))

(defun parse-constructor (form enclosing-form file)
  (declare (type cst:cst form enclosing-form)
           (type se:file file)
           (values constructor))

  (let (unparsed-name
        unparsed-fields)

    (if (cst:atom form)
        (setf unparsed-name form)
        (progn
          (setf unparsed-name (cst:first form))
          (setf unparsed-fields (cst:listify (cst:rest form)))))

    (unless (cst:atom unparsed-name)
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source unparsed-name)
                   :file file
                   :message "Malformed constructor"
                   :primary-note "expected symbol"
                   :notes
                   (list
                    (se:make-source-error-note
                     :type :secondary
                     :span (cst:source (cst:second enclosing-form))
                     :message "in this type definition")))))

    (unless (identifierp (cst:raw unparsed-name))
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source unparsed-name)
                   :file file
                   :message "Malformed constructor"
                   :primary-note "expected symbol"
                   :notes
                   (list
                    (se:make-source-error-note
                     :type :secondary
                     :span (cst:source (cst:second enclosing-form))
                     :message "in this type definition")))))

    (make-constructor
     :name (make-identifier-src
            :name (cst:raw unparsed-name)
            :source (source-location unparsed-name file))
     :fields (loop :for field :in unparsed-fields
                   :collect (parse-type field file))
     :source (source-location form file))))

(defun parse-argument-list (form file)
  (declare (type cst:cst form)
           (type se:file file)
           (values node-variable pattern-list))

  ;; (define x 1)
  (when (cst:atom form)
    (return-from parse-argument-list (values (parse-variable form file) nil)))

  ;; (define (0.5 x y) ...)
  (unless (identifierp (cst:raw (cst:first form)))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source (cst:first form))
                 :file file
                 :message "Malformed function definition"
                 :primary-note "expected symbol")))

  (values
   (parse-variable (cst:first form) file)
   (if (cst:null (cst:rest form))
       (list
        (make-pattern-wildcard
         :source (source-location form file)))
       (loop :for vars := (cst:rest form) :then (cst:rest vars)
             :while (cst:consp vars)
             :collect (parse-pattern (cst:first vars) file)))))

(defun parse-identifier (form file)
  (declare (type cst:cst form)
           (type se:file file)
           (values identifier-src))

  (unless (cst:atom form)
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Unexpected list"
                 :primary-note "expected an identifier")))

  (unless (identifierp (cst:raw form))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Unexpected form"
                 :primary-note "expected an identifier")))

  (when (string= "_" (cst:raw form))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Invalid identifier"
                 :primary-note "invalid identifier '_'")))

  (when (char= #\. (aref (symbol-name (cst:raw form)) 0))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Invalid identifier"
                 :primary-note "identifiers cannot start with '.'")))

  (make-identifier-src
   :name (cst:raw form)
   :source (source-location form file)))

(defun parse-definition-body (form enclosing-form file)
  (declare (type cst:cst form)
           (type cst:cst enclosing-form)
           (type se:file file)
           (values (or null string) node-body))

  (let (docstring
        unparsed-body)

    ;; (define y 2)
    (when (cst:atom (cst:rest form))
      (return-from parse-definition-body (values nil (parse-body form enclosing-form file))))

    (if (and (cst:atom (cst:first form))
             (stringp (cst:raw (cst:first form))))
        (progn
          (setf docstring (cst:raw (cst:first form)))
          (setf unparsed-body (cst:rest form)))

        (setf unparsed-body form))

    (values docstring (parse-body unparsed-body enclosing-form file))))

(defun parse-instance-method-definition (form parent-form file)
  (declare (type cst:cst form)
           (type cst:cst parent-form)
           (type se:file file)
           (values instance-method-definition))

  (let ((context-note
          (se:make-source-error-note
           :type :secondary
           :span (cst:source parent-form)
           :message "when parsing instance")))

    (unless (cst:consp form)
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source form)
                   :file file
                   :message "Malformed method definition"
                   :primary-note "expected list"
                   :notes (list context-note))))

    (unless (cst:proper-list-p form)
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source form)
                   :file file
                   :message "Malformed method definition"
                   :primary-note "unexpected dotted list"
                   :notes (list context-note))))

    (unless (and (cst:atom (cst:first form))
                 (eq (cst:raw (cst:first form)) 'coalton:define))
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source (cst:first form))
                   :file file
                   :message "Malformed method definition"
                   :primary-note "expected method definition"
                   :notes (list context-note))))

    (unless (cst:consp (cst:rest form))
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source form)
                   :file file
                   :message "Malformed method definition"
                   :primary-note "expected definition name"
                   :notes (list context-note))))

    (multiple-value-bind (name params)
        (parse-argument-list (cst:second form) file)

      (make-instance-method-definition
       :name name
       :params params
       :body (parse-body (cst:rest (cst:rest form)) form file)
       :source (source-location form file)))))

(defun parse-fundep (form file)
  (declare (type cst:cst form)
           (type se:file file)
           (values fundep))

  (unless (cst:consp form)
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Malformed functional dependency"
                 :primary-note "expected a list")))

  (unless (cst:proper-list-p form)
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Malformed functional dependency"
                 :primary-note "unexpected dotted list")))

  (multiple-value-bind (left right)
      (util:take-until
       (lambda (cst)
         (and (cst:atom cst)
              (eq (cst:raw cst) 'coalton:->)))
       (cst:listify form))

    (unless left
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source form)
                   :file file
                   :message "Malformed functional dependency"
                   :primary-note "expected one or more type variables")))

    (unless (rest right)
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source form)
                   :file file
                   :highlight :end
                   :message "Malformed functional dependency"
                   :primary-note "expected one or more type variables")))

    (make-fundep
     :left (loop :for var :in left
                 :collect (parse-type-variable var file))
     :right (loop :for var :in (cdr right)
                  :collect (parse-type-variable var file))
     :source (source-location form file))))


(defun parse-monomorphize (form file)
  (declare (type cst:cst form)
           (type se:file file)
           (values attribute-monomorphize))

  (assert (cst:consp form))

  (when (cst:consp (cst:rest form))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Malformed monomophize attribute"
                 :primary-note "unexpected form")))

  (make-attribute-monomorphize
   :source (source-location form file)))

(defun parse-repr (form file)
  (declare (type cst:cst form)
           (type se:file file)
           (values attribute-repr))

  (assert (cst:consp form))

  (unless (cst:consp (cst:rest form))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :highlight :end
                 :message "Malformed repr attribute"
                 :primary-note "expected keyword symbol")))

  (let ((type (parse-type-variable (cst:second form) file)))
    (if (eq (keyword-src-name type) :native)

        (progn ;; :native reprs must have an argument
          (unless (cst:consp (cst:rest (cst:rest form)))
            (error 'parse-error
                   :err (se:source-error
                         :span (cst:source form)
                         :file file
                         :highlight :end
                         :message "Malformed repr :native attribute"
                         :primary-note "expected a lisp type")))

          (when (cst:consp (cst:rest (cst:rest (cst:rest form))))
            (error 'parse-error
                   :err (se:source-error
                         :span (cst:source (cst:first (cst:rest (cst:rest (cst:rest form)))))
                         :file file
                         :message "Malformed repr :native attribute"
                         :primary-note "unexpected form")))

          (make-attribute-repr
           :type type
           :arg (cst:third form)
           :source (source-location form file)))

        (progn ;; other reprs do not have an argument
          (when (cst:consp (cst:rest (cst:rest form)))
            (error 'parse-error
                   :err (se:source-error
                         :span (cst:source (cst:first (cst:rest (cst:rest form))))
                         :file file
                         :message "Malformed repr attribute"
                         :primary-note "unexpected form")))

          (case (keyword-src-name type)
            (:lisp nil)
            (:transparent nil)
            (:enum nil)
            (t
             (error 'parse-error
                    :err (se:source-error
                          :span (cst:source (cst:second form))
                          :file file
                          :message "Unknown repr attribute"
                          :primary-note "expected one of :lisp, :transparent, :enum, or :native"))))

          (make-attribute-repr
           :type type
           :arg nil
           :source (source-location form file))))))

(defun parse-struct-field (form file)
  (declare (type cst:cst form)
           (type se:file file)
           (values struct-field))

  ;; 5
  (unless (cst:consp form)
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Malformed struct field"
                 :primary-note "unexpected form")))

  ;; (5 ...)
  (unless (and (cst:atom (cst:first form))
               (symbolp (cst:raw (cst:first form))))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Malformed struct field"
                 :primary-note "invalid field name (must be a symbol)"
                 :highlight :end)))

  ;; (name)
  (unless (cst:consp (cst:rest form))
    (error 'parse-error
           :err (se:source-error
                 :span (cst:source form)
                 :file file
                 :message "Malformed struct field"
                 :primary-note "expected field type")))

  (multiple-value-bind (docstring rest-field)
      (if (stringp (cst:raw (cst:second form)))
          (values (and (cst:raw (cst:first (cst:rest form)))) (cst:rest (cst:rest form)))
          (values nil (cst:rest form)))

    ;; (name docstring)
    (when (cst:null rest-field)
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source form)
                   :file file
                   :message "Malformed struct field"
                   :primary-note "expected field type"
                   :highlight :end)))

    ;; (name ty ...) or (name "docstring" ty ...)
    (unless (cst:null (cst:rest rest-field))
      (error 'parse-error
             :err (se:source-error
                   :span (cst:source form)
                   :file file
                   :message "Malformed struct field"
                   :primary-note "unexpected trailing form"
                   :highlight :end)))

    (make-struct-field
     :name (symbol-name (cst:raw (cst:first form)))
     :type (parse-type (cst:first rest-field)
                       file)
     :docstring docstring
     :source (source-location form file))))
