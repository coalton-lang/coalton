(in-package #:coalton-native-tests)

(define-test simple-hashmap ()
  (let a = (the (hashmap:HashMap String Integer) hashmap:empty))
  (is (hashmap:empty? a))
  (is (== (hashmap:count a) 0))

  (let b = (hashmap:insert a "alpha" 1))
  (is (not (hashmap:empty? b)))
  (is (== (hashmap:count b) 1))

  (let c = (hashmap:insert b "beta" 2))
  (is (not (hashmap:empty? c)))
  (is (== (hashmap:count c) 2))

  (let d = (hashmap:insert c "gamma" 3))
  (let e = (hashmap:insert d "delta" 4))

  (is (== (Some 1) (hashmap:lookup e "alpha")))
  (is (== (Some 2) (hashmap:lookup e "beta")))
  (is (== (Some 3) (hashmap:lookup e "gamma")))
  (is (== (Some 4) (hashmap:lookup e "delta")))
  (is (== None (hashmap:lookup e "epsilon")))

  (let f = (hashmap:insert e "alpha" 5))
  (is (== (Some 5) (hashmap:lookup f "alpha")))

  (let g = (hashmap:remove f "beta"))
  (is (== (Some 5) (hashmap:lookup g "alpha")))
  (is (== None (hashmap:lookup g "beta")))
  (is (== (Some 3) (hashmap:lookup g "gamma")))
  (is (== (Some 4) (hashmap:lookup g "delta")))

  (let h = (hashmap:remove g "epsilon"))
  (is (== (Some 5) (hashmap:lookup g "alpha")))
  (is (== (Some 3) (hashmap:lookup g "gamma")))
  (is (== (Some 4) (hashmap:lookup g "delta")))

  (let i = (hashmap:insert h "alpha" 5))
  (let j = (hashmap:insert i "gamma" 3))
  (let k = (hashmap:insert j "delta" 4))
  (is (== (Some 5) (hashmap:lookup g "alpha")))
  (is (== (Some 3) (hashmap:lookup g "gamma")))
  (is (== (Some 4) (hashmap:lookup g "delta")))

  (let z = (the (List (Tuple String Integer))
                (iter:collect! (iter:into-iter g))))
  (is (== (into (list:length z)) (hashmap:count g)))
  (is (iter:every! (fn ((Tuple k v))
                     (match (hashmap:lookup g k)
                       ((None) False)
                       ((Some vv) (== v vv))))
                   (iter:into-iter z)))

  (let i = (hashmap:remove (hashmap:remove (hashmap:remove h "alpha") "gamma") "delta"))
  (is (hashmap:empty? i))
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

(define-test heavy-hashmap ()
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
