(in-package :cl-user)
(defpackage :practical-coalton.simple-database
  (:use
    #:coalton
    #:coalton-prelude)
  (:import-from
   #:alexandria
   :with-gensyms)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:lst #:coalton-library/list)
   (#:cel #:coalton-library/cell)
   (#:str #:coalton-library/string)))
(in-package :practical-coalton.simple-database)

(named-readtables:in-readtable coalton:coalton)

;;; Port of Chapter 3 from Practical Common Lisp, by Peter Seibel,
;;; to Coalton.
;;;
;;; In PCL, the database is stored in a global variable. The functions
;;; modifying the database write to that global. This is possible in
;;; Coalton (by defining a CELL global variable), and we take that approach
;;; in the port of chapter 23. In this chapter, we define a mutable Database
;;; type, and each of the functions that read or write to a Database take
;;; it as an argument.
;;;
;;; Note that the CL macros generating Coalton functions work very well
;;; with the type system. Even though the macro code itself is not type
;;; checked, the code it generates is. This way the type system will catch
;;; any type errors caused by incorrect macro code or incorrect usage of
;;; a macro.

;;;
;;; Define the types required to create a mutable database and basic
;;; functions to work with them.
;;;

(coalton-toplevel
  (define-struct CD
    (title String)
    (artist String)
    (rating Integer)
    (ripped Boolean))

  (repr :transparent)
  (define-type Database
    (Database (Cell (List CD))))

  (declare cd-data (Database -> (Cell (List CD))))
  (define (cd-data (Database cd-cell))
    cd-cell)

  (declare from-cds ((List CD) -> Database))
  (define (from-cds cd-list)
    (Database (cel:new cd-list)))

  (declare cds (Database -> (List CD)))
  (define (cds (Database cds-data))
    (cel:read cds-data))

  (declare new-database (Unit -> Database))
  (define (new-database)
    (Database (cel:new Nil)))

  (declare add-record (Database -> CD -> Database))
  (define (add-record db cd)
    (cel:push! (cd-data db) cd)
    db)

  (declare dump-cd (CD -> Unit))
  (define (dump-cd cd)
    (traceobject "title" (.title cd))
    (traceobject "artist" (.artist cd))
    (traceobject "rating" (.rating cd))
    (traceobject "ripped" (.ripped cd)))

  (declare dump-db (Database -> Unit))
  (define (dump-db (Database cd-data))
    (for cd in (cel:read cd-data)
        (dump-cd cd)
        (trace "--"))
    Unit))

;;;
;;; Query DSL implemented as Common Lisp macros that generate Coalton code.
;;;

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defun keyword-to-struct-accessor (keyword)
    "Converts a keyword to a Coalton struct accessor.

  This is useful for converting keywords to Coalton struct accessors in the
  query DSL, like:

  (where :rating 8), in which :rating will eventually be transformed into
  (.rating cd) in the generated Coalton code.

  Example:

  (keyword-to-struct-accessor :title) => .TITLE"
    (cl:intern (cl:concatenate 'cl:string (cl:string '#:.) (cl:string keyword))))

  (cl:defun comparison-clause (attr-keyword value var-sym)
    "Generates a comparison clause for the query DSL.

  Example:

  (comparison-clause :rating 5 'cd) => `(== 5 (.rating cd))"
      `(== ,value (,(keyword-to-struct-accessor attr-keyword) ,var-sym)))

  (cl:defun comparison-clauses (var-sym clauses)
    "Generates a list of comparison clauses for the query DSL."
    (cl:loop while clauses
      collecting (comparison-clause (cl:pop clauses)
                                    (cl:pop clauses)
                                    var-sym))))

(cl:defmacro where (cl:&rest clauses)
  "Provides a query DSL to select records from a database.

Creates an anonymous function that takes a CD and returns True if the
CD matches the specified criteria.

Example:

(where :rating 5 :artist ''Peter Siebel'')
  => (fn (cd) (and (== 5 (.rating cd)) (== ''Peter Siebel'' (.artist cd))))"
  (with-gensyms (cd-sym)
    `(fn (,cd-sym)
       (and ,@(comparison-clauses cd-sym clauses)))))

(cl:defmacro to (cl:&key title artist rating (ripped nil ripped-p))
  "Provides a query DSL to produce updated records for the database.

Creates an anonymyous function that takes a CD and returns a new CD with
the specified attributes updated. The attributes that are not specified
are copied from the original CD.

Example:

(to :ripped True :rating 1)
  => (fn (cd) (CD (.title cd) (.artist cd) 1 True))"
  (with-gensyms (cd-sym)
    `(fn (,cd-sym)
      (CD
        ,(cl:if title title `(.title ,cd-sym))
        ,(cl:if artist artist `(.artist ,cd-sym))
        ,(cl:if rating rating `(.rating ,cd-sym))
        ,(cl:if ripped-p ripped `(.ripped ,cd-sym))))))

;;;
;;; Functions to ask the user for database input, and to save/load the
;;; database to/from a file.
;;;

(coalton-toplevel
  (declare prompt-read (String -> String))
  (define (prompt-read prompt)
    (lisp String (prompt)
      (cl:format cl:*query-io* "~a: " prompt)
      (cl:force-output cl:*query-io*)
      (cl:read-line cl:*query-io*)))

  (declare prompt-y-n (String -> Boolean))
  (define (prompt-y-n prompt)
    (lisp Boolean (prompt)
      (cl:y-or-n-p (cl:format cl:nil "~a [y/n]: " prompt))))

  (declare prompt-for-cd (Unit -> CD))
  (define (prompt-for-cd)
    (CD
      (prompt-read "Title")
      (prompt-read "Artist")
      (with-default 0 (str:parse-int (prompt-read "Rating")))
      (prompt-y-n "Ripped")))

  (declare add-cds (Database -> Database))
  (define (add-cds db)
    (add-record db (prompt-for-cd))
    (if (prompt-y-n "Another?")
          (add-cds db)
        db))

  (declare save-db (Database -> String -> Database))
  (define (save-db db filename)
    (let ((cd-list (cds db)))
      (lisp :a (cd-list filename)
        (cl:with-open-file (out filename
                                :direction :output
                                :if-exists :supersede)
          (cl:with-standard-io-syntax
            (cl:print cd-list out)))))
    db)

  (declare load-db (String -> Database))
  (define (load-db filename)
    (from-cds
      (lisp (List CD) (filename)
        (cl:with-open-file (in filename)
          (cl:with-standard-io-syntax
            (cl:read in)))))))

;;;
;;; Functions to query the database using the DSL.
;;;

(coalton-toplevel
  (declare select (Database -> (CD -> Boolean) -> (List CD)))
  (define (select db selector-fn)
    (lst:filter selector-fn (cds db)))

  (declare delete! (Database -> (CD -> Boolean) -> Database))
  (define (delete! db selector-fn)
    (cel:write! (cd-data db) (lst:remove-if selector-fn (cds db)))
    db)

  (declare update! (Database -> (CD -> Boolean) -> (CD -> CD) -> Database))
  (define (update! db selector-fn update-fn)
    (cel:write! (cd-data db)
      (map (fn (cd) (if (selector-fn cd)
                      (update-fn cd)
                      cd))
           (cds db)))
    db))

;;;
;;; Example code. These are Common Lisp functions that call Coalton code,
;;; so they can be easily called from the REPL.
;;;

(cl:defun save-prompted-db ()
  (coalton
    (traceobject "Saved database to file:"
      (save-db (add-cds (new-database)) "cds.db"))))

(cl:defun load-and-update-snoop ()
  (coalton
    (traceobject "Database:"
      (update!
        (load-db "cds.db")
        (where :artist "Snoop Dogg")
        (to :ripped True :rating 5)))))

(cl:defun prompt-and-select ()
  (coalton
    (traceobject "Ripped tracks with a perfect rating:"
      (select
        (add-cds (new-database))
        (where :rating 10 :ripped True)))))
