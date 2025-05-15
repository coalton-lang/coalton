;;; Benchmarks for various mapping data structures.

(cl:in-package #:benchmark-mapping/native)

(coalton-toplevel
  (define data-size 1000000)

  ;; NB: See tests/hamt-tests.lisp.  Will be replaced with data generator
  ;; library.
  (define mapping-bench/rand64
    (let ((s (cell:new 42))
          (a 2862933555777941757)
          (b 3037000493)
          (m (coalton-library/bits:shift 64 1)))
      ;; Simple 64bit congruential generator.
      ;; https://nuclear.llnl.gov/CNP/rng/rngman/node4.html
      ;; CL's random is cumbersome to use with portable fixed seed value.
      (fn ()
        (let ((ss (mod (+ (* a (cell:read s)) b) m)))
          (cell:write! s ss)
          ss))))

  ;; NB: This can be simplifed once we have unfold
  (define mapping-data
    (rec %loop ((n data-size)
                (r Nil))
      (if (== n 0)
          (reverse r)
          (%loop (1- n) (Cons (Tuple (mapping-bench/rand64)
                                     (mapping-bench/rand64))
                              r)))))

  ;; Building
  (define (build-hamt)
    (the (hamt:Hamt Integer Integer)
         (iter:collect! (iter:into-iter mapping-data))))

  (define (build-map)
    (the (map:Map Integer Integer)
         (iter:collect! (iter:into-iter mapping-data))))

  (define (build-hashtable)
    (the (hashtable:Hashtable Integer Integer)
         (iter:collect! (iter:into-iter mapping-data))))

  ;; Looking up
  (define (lookup-hamt ht)
    (l:dolist ((Tuple k _) mapping-data)
      (hamt:get ht k)))

  (define (lookup-map m)
    (l:dolist ((Tuple k _) mapping-data)
      (map:lookup m k)))

  (define (lookup-hashtable tab)
    (l:dolist ((Tuple k _) mapping-data)
      (hashtable:get tab k)))

  ;; Deletion
  (define (remove-hamt ht)
    (l:dolist ((Tuple k _) mapping-data)
      (hamt:remove ht k)))

  (define (remove-map m)
    (l:dolist ((Tuple k _) mapping-data)
      (map:remove m k)))

  (define (remove-hashtable tab)
    (l:dolist ((Tuple k _) mapping-data)
      (hashtable:remove! tab k)))
  )
(cl:in-package #:benchmark-mapping)

(defmacro mapping-benchmark (type builder looker remover)
  (let ((obj (gensym)))
    `(let (obj)
       (let ((build-timer (make-timer))
             (lookup-timer (make-timer))
             (delete-timer (make-timer)))
         (format t "Buliding ~A~%" ',type)
         (with-sampling (build-timer)
           (setf obj (coalton:coalton (,builder))))
         (report build-timer)
         (format t "Looking up keys in ~A~%" ',type)
         (with-sampling (lookup-timer)
           (coalton:coalton (,looker (coalton:lisp ,type () obj))))
         (report lookup-timer)
         (format t "Removing from ~A~%" ',type)
         (with-sampling (delete-timer)
           (coalton:coalton (,remover (coalton:lisp ,type () obj))))
         (report delete-timer)
         ))))

(define-benchmark build-hamt ()
  (declare (optimize speed))
  (mapping-benchmark (hamt:Hamt coalton:Integer coalton:Integer)
                     benchmark-mapping/native::build-hamt
                     benchmark-mapping/native::lookup-hamt
                     benchmark-mapping/native::remove-hamt))

(define-benchmark build-map ()
  (declare (optimize speed))
  (mapping-benchmark (map:Map coalton:Integer coalton:Integer)
                     benchmark-mapping/native::build-map
                     benchmark-mapping/native::lookup-map
                     benchmark-mapping/native::remove-map))

(define-benchmark build-hashtable ()
  (declare (optimize speed))
  (mapping-benchmark (hashtable:HashTable coalton:Integer coalton:Integer)
                     benchmark-mapping/native::build-hashtable
                     benchmark-mapping/native::lookup-hashtable
                     benchmark-mapping/native::remove-hashtable))
