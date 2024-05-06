(coalton-library/utils:defstdlib-package #:coalton-library/hashtable
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/hash)
  (:local-nicknames
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator)
   (#:shim #:coalton/hashtable-shim))
  (:export
   #:Hashtable
   #:new
   #:with-capacity
   #:get
   #:set!
   #:remove!
   #:count
   #:foreach
   #:entries
   #:keys
   #:values
   #:extend!
   #:make))

(in-package #:coalton-library/hashtable)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;;
  ;; Hashtable
  ;;

  (repr :native shim:hash-table-type)
  (define-type (Hashtable :key :value)
    "A mutable hash table.")

  (declare %get-hash-test-funcs (Hash :key => Unit -> (Tuple (:key -> :key -> Boolean) (:key -> Hash))))
  (define (%get-hash-test-funcs _)
    "Construct closures over the `Hash' instance for KEY"
    (Tuple == hash))

  (declare %make-hashtable ((Hash :key) =>
                            (:key -> :key -> Boolean)
                            -> (:key -> Hash)
                            -> Integer
                            -> Hashtable :key :value))
  (define (%make-hashtable test hash_ cap)
    "Inner function: allocate a hash table using the COALTON/HASHTABLE-SHIM interface"
    (lisp (Hashtable :key :value) (cap test hash_)
      (cl:flet ((coalton-hashtable-test (a b)
                  (call-coalton-function test a b))
                (coalton-hashtable-hash (key)
                  (call-coalton-function hash_ key)))
        (shim:make-custom-hash-table
         cap
         #'coalton-hashtable-hash
         #'coalton-hashtable-test))))

  (declare default-hash-table-capacity UFix)
  (define default-hash-table-capacity
    ;; same as SBCL's
    17)

  (declare with-capacity (Hash :key => Integer -> Hashtable :key :value))
  (define (with-capacity capacity)
    "Crate a new empty hashtable with a given capacity"
    (match (%get-hash-test-funcs)
      ((Tuple test hash)
       (%make-hashtable test hash capacity))))

  (declare new (Hash :key => Unit -> Hashtable :key :value))
  (define (new)
    "Create a new empty hashtable"
    ;; default size is the same as SBCL's
    (with-capacity (into default-hash-table-capacity)))

  (declare get (Hash :key => Hashtable :key :value -> :key -> Optional :value))
  (define (get table key)
    "Lookup KEY in TABLE"
    (lisp (Optional :a) (key table)
      (cl:multiple-value-bind (elem exists?)
          (shim:custom-hash-table-get table key)
        (cl:if exists?
               (Some elem)
               None))))

  (declare set! (Hash :key => Hashtable :key :value -> :key -> :value -> Unit))
  (define (set! table key value)
    "Set KEY to VALUE in TABLE"
    (lisp Unit (table key value)
      (cl:progn
        (shim:custom-hash-table-set table key value)
        Unit)))

  (declare remove! (Hash :key => Hashtable :key :value -> :key -> Unit))
  (define (remove! table key)
    "Remove the entry at KEY from TABLE"
    (lisp Unit (table key)
      (cl:progn
        (shim:custom-hash-table-remove table key)
        Unit)))

  (declare count (Hashtable :key :value -> Integer))
  (define (count table)
    "Returns the number of entries in TABLE"
    (lisp Integer (table)
      (shim:custom-hash-table-count table)))

  (declare entries (Hashtable :key :value -> iter:Iterator (Tuple :key :value)))
  (define (entries table)
    "Returns the key-values pairs as a list."
    (let lisp-iter = (lisp :a (table) (shim:custom-hash-table-iter table)))
    (iter:with-size
        (fn ()
          (lisp (Optional (Tuple :key :value)) (lisp-iter)
            (cl:multiple-value-bind (presentp key value)
                (cl:funcall lisp-iter)
              (cl:if presentp
                     (Some (Tuple key value))
                     None))))
      (with-default 0 (the (Result String UFix) (tryinto (count table))))))

  (declare keys (Hashtable :key :value -> iter:Iterator :key))
  (define (keys table)
    "Returns the keys in TABLE as a list"
    (let lisp-iter = (lisp :a (table) (shim:custom-hash-table-iter table)))
    (iter:with-size
        (fn ()
          (lisp (Optional :key) (lisp-iter)
            (cl:multiple-value-bind (presentp key value)
                (cl:funcall lisp-iter)
              (cl:declare (cl:ignore value))
              (cl:if presentp
                     (Some key)
                     None))))
      (with-default 0 (the (Result String Ufix) (tryinto (count table))))))

  (declare values (Hashtable :key :value -> iter:Iterator :value))
  (define (values table)
    "Returns the values in TABLE as a list"
    (let lisp-iter = (lisp :a (table) (shim:custom-hash-table-iter table)))
    (iter:with-size
        (fn ()
          (lisp (Optional :value) (lisp-iter)
            (cl:multiple-value-bind (presentp key value)
                (cl:funcall lisp-iter)
              (cl:declare (cl:ignore key))
              (cl:if presentp
                     (Some value)
                     None))))
      (with-default 0 (the (Result String UFix) (tryinto (count table)))))) 

  (declare extend! ((Hash :key) (iter:IntoIterator :container (Tuple :key :value))
                   => Hashtable :key :value -> :container -> Unit))
  (define (extend! table iter)
    "Insert all of the key value pairs from ITER into TABLE, overwriting duplicate keys."
    (let iter = (iter:into-iter iter))
    (iter:for-each!
     (fn ((Tuple key value))
       (set! table key value))
     iter)
    Unit)

  ;;
  ;; Instances
  ;;

  (define-instance ((Hash :key) (Eq :value) => (Eq (Hashtable :key :value)))
    (define (== ht1 ht2)
      (unless (== (count ht1) (count ht2))
        (return False))
      
      (iter:every!
       (fn (key)
         (== (get ht1 key) (get ht2 key)))
       (keys ht1))))

  (define-instance (iter:IntoIterator (Hashtable :key :value) (Tuple :key :value))
    (define (iter:into-iter table)
      (entries table)))

  (define-instance (Hash :key => iter:FromIterator (Hashtable :key :value) (Tuple :key :value))
    (define (iter:collect! iter)
      (let capacity = (with-default 0 (iter:size-hint iter)))
      (let table = (with-capacity (into capacity)))
      (iter:for-each!
       (fn ((Tuple key value))
         (set! table key value))
       iter)
      table))

  (define-instance (Hash :key => Default (Hashtable :key :value))
    (define default new)))

(cl:define-condition make-hash-table-static-duplicate-keys (cl:error)
  ((offending-key :initarg :offending-key
                  :reader duplicate-offending-key)
   (entry-a :initarg :entry-a
            :reader duplicate-entry-a)
   (entry-b :initarg :entry-b
            :reader duplicate-entry-b))
  (:report (cl:lambda (c s)
             (cl:format s "Statically-detected duplicate keys in `make-hash-table': key ~S applies to both ~S and ~S"
                        (duplicate-offending-key c)
                        (duplicate-entry-a c)
                        (duplicate-entry-b c)))))

(cl:defun find-duplicate-entry (elements cl:&key (test #'cl:equal) (key #'cl:first))
  (cl:declare (cl:type cl:function test key))
  (cl:let* ((ht (cl:make-hash-table :test test)))
    (cl:dolist (entry elements (cl:values cl:nil cl:nil cl:nil))
      (cl:let* ((keyed (cl:funcall key entry)))
        (cl:multiple-value-bind (old-entry presentp)
            (cl:gethash keyed ht)
          (cl:if presentp (cl:return-from find-duplicate-entry
                            (cl:values cl:t old-entry entry))
                 (cl:setf (cl:gethash keyed ht) entry)))))))

(cl:defmacro make (cl:&rest pairs)
  "Construct a `HashTable' containing the PAIRS as initial entries.

Each of the PAIRS should be a two-element list of the form (KEY VALUE). The resulting table will map KEY to
VALUE.

Listing duplicate keys is disallowed. This macro will attempt to detect duplicate keys at compile-time, and
signal an error if duplicate keys are found. If the macro doesn't detect a pair of duplicate keys, the
resulting table will map the KEY to exactly one of the listed VALUEs, but which VALUE is chosen is undefined.

Examples:

(make (\"zero\" 0)
      (\"one\" 1)
      (\"two\" 2))

(let ((zero \"zero\")
      (one \"one\"))
  (make (zero 0)
        (one 1)
        (\"two\" 2)))"
  (cl:let* ((keys (cl:mapcar #'cl:first pairs))
            (values (cl:mapcar #'cl:second pairs))
            (ht (cl:gensym "HASH-TABLE-")))
    (cl:multiple-value-bind (duplicatep dup-a dup-b)
        (find-duplicate-entry pairs)
      (cl:if duplicatep
             (cl:error 'make-hash-table-static-duplicate-keys
                       :offending-key (cl:first dup-a)
                       :entry-a dup-a
                       :entry-b dup-b)
             `(progn
                (let ,ht = (with-capacity ,(cl:length keys)))
                ,@(cl:mapcar (cl:lambda (key val)
                               `(set! ,ht ,key ,val))
                             keys values)
                ,ht)))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/HASHTABLE")

(in-package #:coalton-library/iterator)

(coalton-toplevel
  (declare remove-duplicates! (Hash :elt => Iterator :elt -> Iterator :elt))
  (define (remove-duplicates! iter)
    "Yield unique elements from ITER in order of first appearance."
    (let ((already-seen (coalton-library/hashtable:new))
          (unique? (fn (elt)
                     (match (coalton-library/hashtable:get already-seen elt)
                       ((Some _) False)
                       ((None)
                        (coalton-library/hashtable:set! already-seen elt Unit)
                        True)))))
      (filter! unique? iter))))
