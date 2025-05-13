(in-package #:coalton-native-tests)

(define-test simple-hamt ()
  (let a = (the (hamt:Hamt String Integer) (hamt:new)))
  (is (hamt:empty? a))
  (is (== (hamt:count a) 0))

  (let b = (hamt:insert a "alpha" 1))
  (is (not (hamt:empty? b)))
  (is (== (hamt:count b) 1))

  (let c = (hamt:insert b "beta" 2))
  (is (not (hamt:empty? c)))
  (is (== (hamt:count c) 2))

  (let d = (hamt:insert c "gamma" 3))
  (let e = (hamt:insert d "delta" 4))

  (is (== (Some 1) (hamt:get e "alpha")))
  (is (== (Some 2) (hamt:get e "beta")))
  (is (== (Some 3) (hamt:get e "gamma")))
  (is (== (Some 4) (hamt:get e "delta")))
  (is (== None (hamt:get e "epsilon")))

  (let f = (hamt:insert e "alpha" 5))
  (is (== (Some 5) (hamt:get f "alpha")))

  (let g = (hamt:remove f "beta"))
  (is (== (Some 5) (hamt:get g "alpha")))
  (is (== None (hamt:get g "beta")))
  (is (== (Some 3) (hamt:get g "gamma")))
  (is (== (Some 4) (hamt:get g "delta")))

  (let h = (hamt:remove g "epsilon"))
  (is (== (Some 5) (hamt:get g "alpha")))
  (is (== (Some 3) (hamt:get g "gamma")))
  (is (== (Some 4) (hamt:get g "delta")))

  (let i = (hamt:insert h "alpha" 5))
  (let j = (hamt:insert i "gamma" 3))
  (let k = (hamt:insert j "delta" 4))
  (is (== (Some 5) (hamt:get g "alpha")))
  (is (== (Some 3) (hamt:get g "gamma")))
  (is (== (Some 4) (hamt:get g "delta")))

  (let z = (the (List (Tuple String Integer))
                (iter:collect! (iter:into-iter g))))
  (is (== (into (list:length z)) (hamt:count g)))
  (is (iter:every! (fn ((Tuple k v))
                     (match (hamt:get g k)
                       ((None) False)
                       ((Some vv) (== v vv))))
                   (iter:into-iter z)))

  (let i = (hamt:remove (hamt:remove (hamt:remove h "alpha") "gamma") "delta"))
  (is (hamt:empty? i))
  )

;; NB: Eventually we want a library of random data generators, similar to
;; Haskell's Test.QuickCheck.Gen or Scheme's SRFI-252 Property Testing.
;; This is a temporary stuff.
(coalton-toplevel
  (define hamt-test/rand64
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

(define-test heavy-hamt ()
  (let data-size = 500000)
  (let tab = (hashtable:new))
  (let ht = (rec gen ((ht (hamt:new))
                      (i data-size))
              (if (== i 0)
                  ht
                  (let ((k (hamt-test/rand64))
                        (v (hamt-test/rand64)))
                    (hashtable:set! tab k v)
                    (gen (hamt:insert ht k v) (1- i))))))
  (is (== (hamt:count ht) (hashtable:count tab)))

  (is (iter:every! (fn ((Tuple key val))
                     (match (hashtable:get tab key)
                       ((None) False)
                       ((Some value) (== value val))))
                   (iter:into-iter ht)))

  (let ht2 = (iter:fold! (fn (ht key) (hamt:remove ht key))
                         ht
                         (hashtable:keys tab)))
  (is (hamt:empty? ht2))
  )
