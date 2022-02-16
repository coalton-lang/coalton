(coalton-library/utils:defstdlib-package #:coalton-library/hashtable
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:local-nicknames
   (#:cell #:coalton-library/cell))
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
   #:values))

(cl:in-package #:coalton-library/hashtable)

(coalton-toplevel
  ;;
  ;; Hashtable
  ;;

  (define-type (Hashtable :key :value)
    (%Hashtable Lisp-Object))

  (declare %get-hash-test-funcs
           ((Hash :key) => Unit -> (Tuple (:key -> :key -> Boolean)
                                          (:key -> UFix))))
  (define (%get-hash-test-funcs _)
    "Construct closures over the `Hash' instance for KEY"
    (Tuple == hash))

  (declare %make-hashtable
           ((Hash :key) =>
            (:key -> :key -> Boolean)
            -> (:key -> UFix)
            -> Integer
            -> (Hashtable :key :value)))
  (define (%make-hashtable test hash_ cap)
    "Inner function: allocate a hash table using the COALTON/HASHTABLE-SHIM interface"
    (%Hashtable
     (lisp Lisp-Object (cap test hash_)
       (cl:flet ((coalton-hashtable-test (a b)
                   (coalton-impl/codegen:a2 test a b))
                 (coalton-hashtable-hash (key)
                   (coalton-impl/codegen:a1 hash_ key)))
         (coalton/hashtable-shim:make-custom-hash-table cap
                                                        #'coalton-hashtable-hash
                                                        #'coalton-hashtable-test)))))

  (declare with-capacity ((Hash :key) => Integer -> (Hashtable :key :value)))
  (define (with-capacity capacity)
    "Crate a new empty hashtable with a given capacity"
    (match (%get-hash-test-funcs)
      ((Tuple test hash) (%make-hashtable test hash capacity))))

  (declare new ((Hash :key) => Unit -> (Hashtable :key :value)))
  (define (new _)
    "Create a new empty hashtable"
    ;; default size is the same as SBCL's
    (with-capacity 17))

  (declare get ((Hash :key) => (Hashtable :key :value) -> :key -> (Optional :value)))
  (define (get table key)
    "Lookup KEY in TABLE"
    (match table
      ((%Hashtable table)
       (lisp (Optional :a) (key table)
         (cl:multiple-value-bind (elem exists?)
             (coalton/hashtable-shim:custom-hash-table-get table key)
           (cl:if exists?
                  (Some elem)
                  None))))))

  (declare set! ((Hash :key) => (Hashtable :key :value) -> :key -> :value -> Unit))
  (define (set! table key value)
    "Set KEY to VALUE in TABLE"
    (match table
      ((%Hashtable inner)
       (lisp Unit (key value inner)
         (cl:progn
           (coalton/hashtable-shim:custom-hash-table-set inner key value)
           Unit)))))

  (declare remove! ((Hash :key) => (Hashtable :key :value) -> :key -> Unit))
  (define (remove! table key)
    "Remove the entry at KEY from TABLE"
    (match table
      ((%Hashtable inner)
       (lisp Unit (inner key)
         (cl:progn
           (coalton/hashtable-shim:custom-hash-table-remove inner key)
           Unit)))))

  (declare count ((Hashtable :key :value) -> Integer))
  (define (count table)
    "Returns the number of entries in TABLE"
    (match table
      ((%Hashtable table)
       (lisp Integer (table)
         (coalton/hashtable-shim:custom-hash-table-count table)))))

  (declare foreach ((:key -> :value -> :a) -> (Hashtable :key :value) -> Unit))
  (define (foreach f table)
    "Call F once for each key value pair in TABLE"
    (match table
      ((%Hashtable table)
       (lisp Unit (f table)
         (cl:progn 
           (coalton/hashtable-shim:custom-hash-table-foreach
            table
            (cl:lambda (key value)
              (coalton-impl/codegen::A2 f key value)))
           Unit)))))

  (declare entries ((Hashtable :key :value) -> (List (Tuple :key :value))))
  (define (entries table)
    (progn
      (let lst = (cell:new Nil))
      (foreach (fn (key val)
                           (cell:push! lst (Tuple key val)))
                         table)
      (cell:read lst)))

  (declare keys ((Hashtable :key :value) -> (List :key)))
  (define (keys table)
    "Returns the keys in TABLE as a list"
    (progn
      (let lst = (cell:new Nil))
      (foreach (fn (key _)
                           (cell:push! lst key))
                         table)
      (cell:read lst)))

  (declare values ((Hashtable :key :value) -> (List :value)))
  (define (values table)
    "Returns the values in TABLE as a list"
    (progn
      (let lst = (cell:new Nil))
      (foreach (fn (_ val)
                           (cell:push! lst val))
                         table)
      (cell:read lst))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/HASHTABLE")
