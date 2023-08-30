(defpackage #:coalton/hashtable-shim/hash-table
  (:shadow #:hash-table
           #:make-hash-table
           #:hash-table-size
           #:hash-table-p
           #:hash-table-count)
  (:use #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:hash-table                         ; STRUCT
   #:make-hash-table                    ; FUNCTION
   #:hash-table-insert                  ; FUNCTION
   #:hash-table-get                     ; FUNCTION
   #:hash-table-delete                  ; FUNCTION
   #:hash-table-count                   ; FUNCTION
   #:hash-table-iter                    ; FUNCTION
   ))

(in-package #:coalton/hashtable-shim/hash-table)

;;;; An imlementation of a hashtable using open addressing and linear
;;;; probing. Loosely based on swiss tables.
;;;;
;;;; Each hash table stores three equal length arrays. Array elements
;;;; are referred to as associated with elements of other arrays at
;;;; them same index. The term "slot" refers to a set of array
;;;; elements across all three arrays.
;;;;
;;;; +----+-----+----+
;;;; | v1 | ... | vn |
;;;; +----+-----+----+
;;;;
;;;; +----+-----+----+
;;;; | k1 | ... | kn |
;;;; +----+-----+----+
;;;;
;;;; +----+-----+----+
;;;; | m1 | ... | mn |
;;;; +----+-----+----+
;;;;
;;;; The keys and values arrays store keys and values respectively. The
;;;; metadata array stores signed bytes. If the value of an element in
;;;; the metadata array is positive (0b0xxxxxxx) then it is the lower
;;;; 7 bits of the associated keys hash. If the values is -128
;;;; (0b10000000) then the slot is empty. If the values is -127
;;;; (0b10000001) then the slot has been deleted. Other negative
;;;; values are unused.
;;;;
;;;; Collisions are resolved by linear probing; the metadata array is
;;;; scanned starting from hash(key) % capacity to find the first empty
;;;; element. When searching for a value deleted elements are skipped
;;;; over, but finding an empty element terminates the search.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *hash-table-optimize* '(optimize (speed 3) (safety 1) (debug 0))))

(declaim (type fixnum +default-capacity+))
(defconstant +default-capacity+ 7)

(declaim (type (signed-byte 8) +slot-empty+))
(defconstant +slot-empty+ -128)

(declaim (type (signed-byte 8) +slot-deleted+))
(defconstant +slot-deleted+ -127)

;; Return type of sxhash
(deftype hash ()
  '(unsigned-byte 62))

(defstruct (hash-table
            (:constructor %make-hash-table)
            (:copier nil))
  ;; Vector of values
  (values (util:required 'values)     :type (simple-array t (*)))
  ;; Vector of keys
  (keys     (util:required 'keys)     :type (simple-array t (*)))
  ;; Vector of hash-table slot metadata
  (metadata (util:required 'metadata) :type (simple-array (signed-byte 8) (*)))
  ;; Number of used slots in the table including deleted elements
  (size     (util:required 'size)     :type fixnum)

  (hash-function (util:required 'hash-function) :type (function (t) hash) :read-only t)
  (eq-function  (util:required 'eq-function) :type (function (t t) boolean) :read-only t))

#+sbcl
(declaim (sb-ext:freeze-type hash-table))

(declaim (inline hash-table-size))
(defun hash-table-capacity (table)
  (declare (type hash-table table)
           (values fixnum)
           #.*hash-table-optimize*)
  (length (hash-table-values table)))

(declaim (inline make-hash-table))
(defun make-hash-table (capacity hash-function eq-function)
  (declare (type fixnum capacity)
           (type (function (t) hash) hash-function)
           (type (function (t t) boolean) eq-function)
           #.*hash-table-optimize*)

  (let ((capacity (max capacity +default-capacity+)))

        (%make-hash-table
          :values (make-array capacity)
          :keys (make-array capacity :element-type 't)
          :metadata (make-array capacity :element-type '(signed-byte 8) :initial-element +slot-empty+)
          :size 0
          :hash-function hash-function
          :eq-function eq-function)))

(declaim (inline hash-matches))
(defun hash-matches% (metadata keys key short-hash index eq-function)
  "Returns t if SHORT-HASH and HASH are matches in the arrays METADATA and KEYS at INDEX."
  (declare (type (simple-array (signed-byte 8) (*)) metadata)
           (type (simple-array t (*)) keys)
           (type t key)
           (type (signed-byte 8) short-hash)
           (type fixnum index)
           (type (function (t t) boolean) eq-function)
           (values boolean)
           #.*hash-table-optimize*)
  (and (= short-hash (aref metadata index))
       (funcall eq-function key (aref keys index))))

(defmacro across-hash-table ((index-var start capacity-var) &body body)
  `(progn
     (loop :for ,index-var fixnum :from ,start :below ,capacity-var
           :do (progn ,@body))

     (loop :for ,index-var fixnum :from 0 :below ,start
           :do (progn ,@body))))

(declaim (inline hash-table-insert%))
(defun hash-table-insert% (metadata keys values capacity key element hash-function eq-function)
  "Returns t if a new slot was used."
  (declare (type (simple-array (signed-byte 8) (*)) metadata)
           (type (simple-array t (*)) keys)
           (type (simple-array t (*)) values)
           (type fixnum capacity)
           (type t key)
           (type t element)
           (type (function (t) hash) hash-function)
           (type (function (t t) boolean) eq-function)
           (values boolean &optional)
           #.*hash-table-optimize*)
  (let* ((hash (funcall hash-function key))

         (short-hash (ldb (byte 7 0) hash))

         (idx (rem hash capacity)))

    (across-hash-table (i idx capacity)
      ;; When encountering an empty slot, take it updating the slots
      ;; metadata and key in the process. Also increment the tables
      ;; size
      (when (= +slot-empty+ (aref metadata i))
        (setf (aref metadata i) short-hash)
        (setf (aref keys i) key)
        (setf (aref values i) element)
        (return-from hash-table-insert% t))

      ;; If the slot has the same hash then only update the element

      (when (hash-matches% metadata keys key short-hash i eq-function)
        (setf (aref values i) element)
        (return-from hash-table-insert% nil)))

    (util:coalton-bug "hash table full")))

(declaim (inline hash-table-insert))
(defun hash-table-insert (table key element)
  "Insert or update the values stored at KEY in TABLE."
  (declare (type hash-table table)
           (type t key)
           (type t element)
           (values null)
           #.*hash-table-optimize*)

  ;; Resize when the table is over 70% full
  (when (>= (the fixnum (* (hash-table-size table) 10))
            (the fixnum (* (the fixnum (hash-table-capacity table)) 7))) 
    (hash-table-resize% table))

  (when (hash-table-insert%
         (hash-table-metadata table)
         (hash-table-keys table)
         (hash-table-values table)
         (hash-table-capacity table)
         key
         element
         (hash-table-hash-function table)
         (hash-table-eq-function table))
    (incf (hash-table-size table)))

  nil)

(defun hash-table-resize% (table)
  "Resize TABLE to be twice as large. Also removes all slots marking deleted values."
  (declare (type hash-table table)
           (values null)
           #.*hash-table-optimize*)
  (let* ((values (hash-table-values table))

         (keys (hash-table-keys table))

         (metadata (hash-table-metadata table))

         (old-capacity (the fixnum (hash-table-capacity table)))

         ;; Resize to double the previous capacity
         (new-capacity (* 2 old-capacity)))

    (setf (hash-table-values table) (make-array new-capacity :element-type 't))
    (setf (hash-table-keys table) (make-array new-capacity :element-type 't))
    (setf (hash-table-metadata table) (make-array new-capacity :element-type '(signed-byte 8) :initial-element +slot-empty+))

    (loop :for i fixnum :from 0 :below old-capacity
          :when (>= (aref metadata i) 0)
            :do (hash-table-insert%
                 (hash-table-metadata table)
                 (hash-table-keys table)
                 (hash-table-values table)
                 new-capacity
                 (aref keys i)
                 (aref (the (simple-array t (*)) values) i)
                 (hash-table-hash-function table)
                 (hash-table-eq-function table)))

    nil))

(declaim (inline hash-table-get))
(defun hash-table-get (table key)
  "Get the element stored at KEY in TABLE. Returns (VALUES ELEMENT FOUNDP)."
  (declare (type hash-table table)
           (type t key)
           (values t boolean)
           #.*hash-table-optimize*)

  (let* ((hash (funcall (hash-table-hash-function table) key))

         (capacity (hash-table-capacity table))

         (short-hash (ldb (byte 7 0) hash))

         (idx (rem hash capacity)))

    (across-hash-table (i idx capacity)
      (when (= +slot-empty+ (aref (hash-table-metadata table) i))
        (return-from hash-table-get (values nil nil)))

      (when (hash-matches%
             (hash-table-metadata table)
             (hash-table-keys table)
             key
             short-hash
             i
             (hash-table-eq-function table))
        (return-from hash-table-get
          (values
           (aref (hash-table-values table) i)
           t))))

    (values nil nil)))

(declaim (inline hash-table-delete))
(defun hash-table-delete (table key)
  "Attempt to delete the element stored at KEY in TABLE. Returns t if an element was deleted."
  (declare (type hash-table table)
           (type t key)
           (values boolean)
           #.*hash-table-optimize*)

  (let* ((hash (funcall (hash-table-hash-function table) key))

         (capacity (hash-table-capacity table))

         (short-hash (ldb (byte 7 0) hash))

         (idx (rem hash capacity)))

    (across-hash-table (i idx capacity)
      ;; If an empty slot is found then key is not in the table
      (when (= +slot-empty+ (aref (hash-table-metadata table) i))
        (return-from hash-table-delete nil))

      (when (hash-matches%
             (hash-table-metadata table)
             (hash-table-keys table)
             key
             short-hash
             i
             (hash-table-eq-function table))
        ;; Mark slot as deleted
        (setf (aref (hash-table-metadata table) i) +slot-deleted+)

        ;; Clear key and element slots to avoid pinning objects
        (setf (aref (hash-table-keys table) i) nil)
        (setf (aref (hash-table-values table) i) nil)

        (return-from hash-table-delete t)))

    nil))

(declaim (inline hash-table-count))
(defun hash-table-count (table)
  "Returns the number of undeleted key values pairs in TABLE."
  (declare (type hash-table table)
           (values fixnum)
           #.*hash-table-optimize*)
    (loop :with count fixnum := 0
          :for m :across (hash-table-metadata table)
          :when (>= m 0)
            :do (incf count)
          :finally (return count)))

(declaim (inline hash-table-foreach))
(defun hash-table-foreach (table f)
  "Call the function F once with every key value pair in TABLE."
  (declare (type hash-table table)
           (type (function (t t)) f)
           #.*hash-table-optimize*)
  (loop :for m :across (hash-table-metadata table)
        :for i fixnum :from 0
        :when (>= m 0)
          :do (funcall f (aref (hash-table-keys table) i) (aref (hash-table-values table) i)))
  nil)

(declaim (inline hash-table-iter))
(defun hash-table-iter (table)
  (declare (type hash-table table)
           #.*hash-table-optimize*)
  (let ((i 0))
    (lambda ()
      (block iter
        (loop :while (< i (length (hash-table-metadata table)))
              :for idx := i
              :for m := (aref (hash-table-metadata table) idx)

              :do (incf i)

              :when (>= m 0)
                :do (return-from iter
                      (values
                       t
                       (aref (hash-table-keys table) idx)
                       (aref (hash-table-values table) idx))))))))
