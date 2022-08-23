(defpackage #:coalton-impl
  (:documentation "Implementation and runtime for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/algorithm
   #:coalton-impl/ast
   #:coalton-impl/typechecker)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:tc #:coalton-impl/typechecker))

  (:export
   #:coalton-parse-error                ; CONDITION
   #:coalton-parse-error-form           ; READER
   #:coalton-parse-error-reason-control ; READER
   #:coalton-parse-error-reason-args    ; READER
   #:coalton-type-error                 ; CONDITION
   #:unification-error                  ; CONDITION
   #:unification-error-first-type       ; READER
   #:unification-error-second-type      ; READER
   #:type-mismatch                      ; CONDITION
   #:type-mismatch-types                ; READER
   #:arity-mismatch                     ; CONDITION
   #:arity-mismatch-arities             ; READER
   #:non-terminating-unification-error  ; CONDITION
   #:non-terminating-unification-error-contained-type
                                        ; READER
   #:non-terminating-unification-error-containing-type
                                        ; READER
   )
  (:export
   #:print-value-db
   #:print-type-db
   #:print-class-db
   #:print-instance-db
   #:print-specializations
   ))
