(cl:in-package #:coalton-library)

(coalton-toplevel
  ;;
  ;; Hashtable
  ;;

  (define-type (Hashtable :key :value)
    (%Hashtable Lisp-Object))

  (declare %make-hashtable-capacity
           ((Hash :key) => Integer -> (Tuple3 (Hashtable :key :value)
                                              (:key -> :key -> Boolean)
                                              (:key -> UFix))))
  (define (%make-hashtable-capacity cap)
    (let ((hash-fn hash)
          (test-fn ==))
      (Tuple3 (%Hashtable (lisp Lisp-Object (cap test-fn hash-fn)
                            (cl:flet ((coalton-hashtable-test (a b)
                                        (coalton-impl/codegen:a2 test-fn a b))
                                      (coalton-hashtable-hash (key)
                                        (coalton-impl/codegen:a1 hash-fn key)))
                              (coalton/hashtable-shim:make-custom-hash-table cap
                                                                             #'coalton-hashtable-hash
                                                                             #'coalton-hashtable-test))))
              test-fn
              hash-fn)))

  (declare make-hashtable-capacity ((Hash :key) => Integer -> (Hashtable :key :value)))
  (define (make-hashtable-capacity capacity)
    "Crate a new empty hashtable with a given capacity"
    (match (%make-hashtable-capacity capacity)
      ((Tuple3 tbl _ _) tbl)))

  (declare make-hashtable ((Hash :key) => Unit -> (Hashtable :key :value)))
  (define (make-hashtable _)
    "Create a new empty hashtable"
    (make-hashtable-capacity 17))

  (declare hashtable-get ((Hash :key) => (Hashtable :key :value) -> :key -> (Optional :value)))
  (define (hashtable-get table key)
    "Lookup KEY in TABLE"
    (match table
      ((%Hashtable table)
       (lisp (Optional :a) (key table)
         (cl:multiple-value-bind (elem exists?)
             (coalton/hashtable-shim:custom-hash-table-get key table)
           (cl:if exists?
                  (Some elem)
                  None))))))

  (declare hashtable-set! ((Hash :key) => (Hashtable :key :value) -> :key -> :value -> Unit))
  (define (hashtable-set! table key value)
    "Set KEY to VALUE in TABLE"
    (match table
      ((%Hashtable inner)
       (lisp Unit (key value inner)
         (cl:progn
           (coalton/hashtable-shim:custom-hash-table-set inner key value)
           Unit)))))

  (declare hashtable-remove! ((Hash :key) => (Hashtable :key :value) -> :key -> Unit))
  (define (hashtable-remove! table key)
    "Remove the entry at KEY from TABLE"
    (match table
      ((%Hashtable inner)
       (lisp Unit (inner key)
         (cl:progn
           (coalton/hashtable-shim:custom-hash-table-remove inner key)
           Unit)))))

  (declare hashtable-count ((Hashtable :key :value) -> Integer))
  (define (hashtable-count table)
    "Returns the number of entries in TABLE"
    (match table
      ((%Hashtable table)
       (lisp Integer (table)
         (coalton/hashtable-shim:custom-hash-table-count table)))))

  (declare hashtable-foreach ((:key -> :value -> :a) -> (Hashtable :key :value) -> Unit))
  (define (hashtable-foreach f table)
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

  (declare hashtable-entries ((Hashtable :key :value) -> (List (Tuple :key :value))))
  (define (hashtable-entries table)
    (progn
      (let lst = (make-cell Nil))
      (hashtable-foreach (fn (key val)
                           (cell-push! lst (Tuple key val)))
                         table)
      (cell-read lst)))

  (declare hashtable-keys ((Hashtable :key :value) -> (List :key)))
  (define (hashtable-keys table)
    "Returns the keys in TABLE as a list"
    (progn
      (let lst = (make-cell Nil))
      (hashtable-foreach (fn (key _)
                           (cell-push! lst key))
                         table)
      (cell-read lst)))

  (declare hashtable-values ((Hashtable :key :value) -> (List :value)))
  (define (hashtable-values table)
    "Returns the values in TABLE as a list"
    (progn
      (let lst = (make-cell Nil))
      (hashtable-foreach (fn (_ val)
                           (cell-push! lst val))
                         table)
      (cell-read lst))))
