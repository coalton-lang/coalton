(in-package #:coalton-native-tests)

(coalton-toplevel
  (declare list->hashmap (Hash :k => List (Tuple :k :v) -> hashmap:HashMap :k :v))
  (define (list->hashmap xs)
    (iter:collect!
     (iter:into-iter
      (the (List (Tuple :a :b)) xs))))

  (declare hashmap->list (hashmap:HashMap :k :v -> List (Tuple :k :v)))
  (define (hashmap->list m) (iter:collect! (iter:into-iter m)))
  )

(define-test hashmap-basic-test ()
  (let a = (the (hashmap:HashMap String Integer) hashmap:empty))
  (is (hashmap:empty? a))
  (is (== (hashmap:count a) 0))

  (let a = (hashmap:insert a "alpha" 1))
  (is (not (hashmap:empty? a)))
  (is (== (hashmap:count a) 1))

  (let a = (hashmap:replace a "beta" 5))
  (is (== (hashmap:lookup a "beta") None))

  (let a = (hashmap:insert a "beta" 9))
  (is (not (hashmap:empty? a)))
  (is (== (hashmap:count a) 2))

  (let a = (hashmap:replace a "beta" 2))
  (let a = (hashmap:insert a "gamma" 3))
  (let a = (hashmap:insert a "delta" 4))

  (is (== (Some 1) (hashmap:lookup a "alpha")))
  (is (== (Some 2) (hashmap:lookup a "beta")))
  (is (== (Some 3) (hashmap:lookup a "gamma")))
  (is (== (Some 4) (hashmap:lookup a "delta")))
  (is (== None (hashmap:lookup a "epsilon")))

  (let a = (hashmap:adjoin a "alpha" 3))
  (is (== (Some 1) (hashmap:lookup a "alpha")))

  (let a = (hashmap:replace a "alpha" 5))
  (is (== (Some 5) (hashmap:lookup a "alpha")))

  (let a = (hashmap:remove a "beta"))
  (is (== (Some 5) (hashmap:lookup a "alpha")))
  (is (== None (hashmap:lookup a "beta")))
  (is (== (Some 3) (hashmap:lookup a "gamma")))
  (is (== (Some 4) (hashmap:lookup a "delta")))

  (let a = (hashmap:remove a "epsilon"))
  (is (== (Some 5) (hashmap:lookup a "alpha")))
  (is (== (Some 3) (hashmap:lookup a "gamma")))
  (is (== (Some 4) (hashmap:lookup a "delta")))

  (let a = (hashmap:insert a "alpha" 5))
  (let a = (hashmap:insert a "gamma" 3))
  (let a = (hashmap:insert a "delta" 4))
  (is (== (Some 5) (hashmap:lookup a "alpha")))
  (is (== (Some 3) (hashmap:lookup a "gamma")))
  (is (== (Some 4) (hashmap:lookup a "delta")))

  (let z = (the (List (Tuple String Integer)) (hashmap->list a)))
  (is (== (into (list:length z)) (hashmap:count a)))
  (is (iter:every! (fn ((Tuple k v))
                     (match (hashmap:lookup a k)
                       ((None) False)
                       ((Some vv) (== v vv))))
                   (iter:into-iter z)))

  (let a = (hashmap:remove (hashmap:remove (hashmap:remove a "alpha") "gamma") "delta"))
  (is (hashmap:empty? a))
  )

(define-test hashmap-update-test ()
  ;; Update protocol
  ;;  In the following tests, the insertion of keys are chosen so that
  ;;  internal tree takes specific shapes.  If we ever change hash function,
  ;;  the keys must be adjusted accordingly.


  ;; utility.  delete k, returns prev value
  (let t-delete = (fn (m k)
                    (hashmap:update m k
                                    (fn (v)
                                      (match v
                                        ((None) (Tuple None "huh?"))
                                        ((Some x) (Tuple None x)))))))
  ;; utility.  replace k, returns prev value
  (let t-replace = (fn (m k v)
                     (hashmap:update m k
                                     (fn (v0)
                                       (match v0
                                         ((None) (Tuple None "huh?"))
                                         ((Some x) (Tuple (Some v) x)))))))
  ;; utility.  insert k, returns "yeah" on success
  (let t-insert = (fn (m k v)
                    (hashmap:update m k
                                    (fn (v0)
                                      (match v0
                                        ((None) (Tuple (Some v) "yeah"))
                                        ((Some _) (Tuple v0 "huh?")))))))


  ;; One 'Leaf' node
  (let m_leaf = (hashmap:insert hashmap:empty 1 "ichi"))

  (match (t-replace m_leaf 1 "hitotsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) (Some "hitotsu")))
     (is (== aux "ichi"))))

  (match (t-insert m_leaf 2 "ni")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== aux "yeah"))))

  (match (t-delete m_leaf 1)
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) None))
     (is (hashmap:empty? m))
     (is (== aux "ichi"))))

  ;; One 'Bud' node
  (let m_bud = (hashmap:insert m_leaf 2 "ni"))

  (match (t-replace m_bud 2 "futatsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 2) (Some "futatsu")))
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== aux "ni"))))

  (match (t-insert m_bud 3 "mittsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== (hashmap:lookup m 3) (Some "mittsu")))
     (is (== aux "yeah"))))

  (match (t-delete m_bud 2)
    ((Tuple m aux)
     (is (== (hashmap:lookup m 2) None))
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== aux "ni"))
     (match (t-delete m 2)
       ((Tuple _ aux)
        (is (== aux "huh?"))))
     (match (t-delete m 1)
       ((Tuple m aux)
        (is (hashmap:empty? m))
        (is (== aux "ichi"))))))

  ;; One 'Tree' node, with 3 leaves
  (let m_tree_leaf = (hashmap:insert m_bud 3 "san"))

  (match (t-replace m_tree_leaf 3 "mittsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 3) (Some "mittsu")))
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== aux "san"))))

  (match (t-insert m_tree_leaf 4 "yottsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 4) (Some "yottsu")))
     (is (== (hashmap:lookup m 3) (Some "san")))
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== aux "yeah"))))

  (match (t-delete m_tree_leaf 1)
    ((Tuple m aux)
     (is (== (hashmap:lookup m 3) (Some "san")))
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== (hashmap:lookup m 1) None))
     (is (== aux "ichi"))
     ;; ensure the tree shrinks as supposed
     (match (t-delete m 2)
       ((Tuple m aux)
        (is (== aux "ni"))
        (match (t-delete m 3)
          ((Tuple m aux)
           (is (== aux "san"))
           (is (hashmap:empty? m))))))))

  ;; this creates a bud with 1 and 32 under the toplevel tree
  (let m_tree_bud = (hashmap:insert m_tree_leaf 32 "sanjuuni"))

  (match (t-replace m_tree_bud 1 "hitotsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) (Some "hitotsu")))
     (is (== (hashmap:lookup m 32) (Some "sanjuuni")))
     (is (== aux "ichi"))))

  (match (t-insert m_tree_bud 4 "yottsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 4) (Some "yottsu")))
     (is (== (hashmap:lookup m 32) (Some "sanjuuni")))
     (is (== (hashmap:lookup m 3) (Some "san")))
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== aux "yeah"))))

  (match (t-delete m_tree_bud 1)
    ((Tuple m aux)
     (is (== (hashmap:lookup m 32) (Some "sanjuuni")))
     (is (== (hashmap:lookup m 3) (Some "san")))
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== (hashmap:lookup m 1) None))
     (is (== aux "ichi"))
     (match (t-delete m 32)
       ((Tuple m aux)
        (is (== aux "sanjuuni"))
        (match (t-delete m 2)
          ((Tuple m aux)
           (is (== aux "ni"))
           (match (t-delete m 3)
             ((Tuple m aux)
              (is (== aux "san"))
              (match (t-delete m 1)
                ((Tuple m aux)
                 (is (== aux "huh?"))
                 (is (hashmap:empty? m))))))))))))

  ;; this creats a tree for 1, 32, 65 under the toplevel tree
  (let m_tree_tree = (hashmap:insert m_tree_bud 65 "rokujuugo"))

  (match (t-replace m_tree_tree 1 "hitotsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) (Some "hitotsu")))
     (is (== (hashmap:lookup m 32) (Some "sanjuuni")))
     (is (== (hashmap:lookup m 65) (Some "rokujuugo")))
     (is (== aux "ichi"))))

  (match (t-insert m_tree_tree 4 "yottsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== (hashmap:lookup m 3) (Some "san")))
     (is (== (hashmap:lookup m 4) (Some "yottsu")))
     (is (== (hashmap:lookup m 32) (Some "sanjuuni")))
     (is (== (hashmap:lookup m 65) (Some "rokujuugo")))
     (is (== aux "yeah"))))

  (match (t-delete m_tree_tree 2)
    ((Tuple m aux)
     (is (== (hashmap:lookup m 2) None))
     (is (== aux "ni"))
     (match (t-delete m 3)
       ((Tuple m aux)
        (is (== (hashmap:lookup m 3) None))
        (is (== aux "san"))
        (match (t-delete m 32)
          ((Tuple m aux)
           (is (== (hashmap:lookup m 32) None))
           (is (== aux "sanjuuni"))
           (match (t-delete m 65)
             ((Tuple m aux)
              (is (== (hashmap:lookup m 65) None))
              (is (== aux "rokujuugo"))
              (match (t-delete m 1)
                ((Tuple m aux)
                 (is (== (hashmap:lookup m 1) None))
                 (is (== aux "ichi"))
                 (is (hashmap:empty? m))))))))))))

  ;; this creats a tree for a bud with 1, 1056 under the 2nd level subtree
  (let m_tree_tree_bud = (hashmap:insert m_tree_tree 1056 "sengojuuroku"))

  (match (t-replace m_tree_tree_bud 1056 "hitomarugoroku")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== (hashmap:lookup m 32) (Some "sanjuuni")))
     (is (== (hashmap:lookup m 65) (Some "rokujuugo")))
     (is (== (hashmap:lookup m 1056) (Some "hitomarugoroku")))
     (is (== aux "sengojuuroku"))))

  (match (t-insert m_tree_tree_bud 4 "yottsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 4) (Some "yottsu")))
     (is (== aux "yeah"))))

  (match (t-delete m_tree_tree_bud 1)
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) None))
     (is (== (hashmap:lookup m 1056) (Some "sengojuuroku")))
     (is (== aux "ichi"))
     (match (t-insert m 1 "hitotsu")
       ((Tuple m aux)
        (is (== (hashmap:lookup m 1) (Some "hitotsu")))
        (is (== (hashmap:lookup m 1056) (Some "sengojuuroku")))
        (is (== aux "yeah"))
        (match (t-delete m 1056)
          ((Tuple m aux)
           (is (== (hashmap:lookup m 1) (Some "hitotsu")))
           (is (== (hashmap:lookup m 1056) None))
           (is (== aux "sengojuuroku"))))))
     (match (t-delete m 2)
       ((Tuple m aux)
        (is (== aux "ni"))
        (match (t-delete m 3)
          ((Tuple m aux)
           (is (== aux "san"))
           (match (t-delete m 1)
             ((Tuple m aux)
              (is (== aux "huh?"))))
           (match (t-delete m 32)
             ((Tuple m aux)
              (is (== aux "sanjuuni"))
              (match (t-delete m 65)
                ((Tuple m aux)
                 (is (== aux "rokujuugo"))
                 (match (t-delete m 1056)
                   ((Tuple m aux)
                    (is (== aux "sengojuuroku"))
                    (is (hashmap:empty? m))))))))))))))
  )

;; NB: Eventually we want a library of random data generators, similar to
;; Haskell's Test.QuickCheck.Gen or Scheme's SRFI-252 Property Testing.
;; This is a temporary stuff.
(coalton-toplevel
  (define hashmap-test/rand64
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
  )

(define-test hashmap-set-test ()
  (let a = (the (hashmap:HashMap Integer String)
                (list->hashmap
                 (make-list (Tuple 1 "Ichi")
                            (Tuple 4 "Yon")
                            (Tuple 5 "Go")
                            (Tuple 9 "Kyuu")
                            (Tuple 10 "Juu")
                            (Tuple 14 "Juuyon")))))
  (let b = (the (hashmap:HashMap Integer String)
                (list->hashmap
                 (make-list (Tuple 2 "Dul")
                            (Tuple 3 "Set")
                            (Tuple 5 "Daseot")
                            (Tuple 7 "Ilgop")
                            (Tuple 9 "Ahop")
                            (Tuple 13 "Yeolset")))))

  (is (== (hashmap:union a b)
          (list->hashmap
           (make-list (Tuple 1 "Ichi")
                      (Tuple 2 "Dul")
                      (Tuple 3 "Set")
                      (Tuple 4 "Yon")
                      (Tuple 5 "Go")
                      (Tuple 7 "Ilgop")
                      (Tuple 9 "Kyuu")
                      (Tuple 10 "Juu")
                      (Tuple 13 "Yeolset")
                      (Tuple 14 "Juuyon")))))
  (is (== (hashmap:union b a)
          (list->hashmap
           (make-list (Tuple 1 "Ichi")
                      (Tuple 2 "Dul")
                      (Tuple 3 "Set")
                      (Tuple 4 "Yon")
                      (Tuple 5 "Daseot")
                      (Tuple 7 "Ilgop")
                      (Tuple 9 "Ahop")
                      (Tuple 10 "Juu")
                      (Tuple 13 "Yeolset")
                      (Tuple 14 "Juuyon")))))

  (is (== (hashmap:intersection a b)
          (list->hashmap (make-list (Tuple 5 "Go") (Tuple 9 "Kyuu")))))
  (is (== (hashmap:intersection b a)
          (list->hashmap (make-list (Tuple 5 "Daseot") (Tuple 9 "Ahop")))))

  (is (== (hashmap:difference a b)
          (list->hashmap
           (make-list (Tuple 1 "Ichi")
                      (Tuple 4 "Yon")
                      (Tuple 10 "Juu")
                      (Tuple 14 "Juuyon")))))

  (is (== (hashmap:xor b a)
          (list->hashmap
           (make-list (Tuple 1 "Ichi")
                      (Tuple 2 "Dul")
                      (Tuple 3 "Set")
                      (Tuple 4 "Yon")
                      (Tuple 7 "Ilgop")
                      (Tuple 10 "Juu")
                      (Tuple 13 "Yeolset")
                      (Tuple 14 "Juuyon")))))
  )

(define-test hashmap-instance-test ()
  (let a = (the (hashmap:HashMap Integer String)
                (list->hashmap
                 (make-list (Tuple 0 "Ling")
                            (Tuple 1 "Yi")
                            (Tuple 2 "Er")
                            (Tuple 3 "San")
                            (Tuple 4 "Si")))))

  (is (== (hash a) (hash a)))

  (is (== (map string:reverse a)
          (list->hashmap
           (the (List (Tuple Integer String))
                (make-list (Tuple 0 "gniL")
                           (Tuple 1 "iY")
                           (Tuple 2 "rE")
                           (Tuple 3 "naS")
                           (Tuple 4 "iS"))))))
  )

(define-test hashmap-heavy-test ()
  (let data-size = 500000)
  (let tab = (hashtable:new))
  (let ht = (rec gen ((ht hashmap:empty)
                      (i data-size))
              (if (== i 0)
                  ht
                  (let ((k (hashmap-test/rand64))
                        (v (hashmap-test/rand64)))
                    (hashtable:set! tab k v)
                    (gen (hashmap:insert ht k v) (1- i))))))
  (is (== (hashmap:count ht) (hashtable:count tab)))

  (is (iter:every! (fn ((Tuple key val))
                     (match (hashtable:get tab key)
                       ((None) False)
                       ((Some value) (== value val))))
                   (iter:into-iter ht)))

  (let ht2 = (iter:fold! (fn (ht key) (hashmap:remove ht key))
                         ht
                         (hashtable:keys tab)))

  (is (hashmap:empty? ht2))
  )
