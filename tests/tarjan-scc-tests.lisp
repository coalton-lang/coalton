(in-package #:coalton-tests)

(defun sccs-equalp (sccs1 sccs2)
  (every #'set-equalp sccs1 sccs2))

(deftest test-tarjan-scc ()
  ;; Can we detect a basic scc
  (is (sccs-equalp
       (coalton-impl/algorithm::tarjan-scc
        '((a b)
          (b c)
          (c a)))
       '((a b c))))

  ;; Can we detect an isolated scc
  (is (sccs-equalp
       (coalton-impl/algorithm::tarjan-scc
        '((a a)
          (b c)
          (c b)))
       '((b c)
         (a))))

  ;; Can we produce a topological sorting of sccs
  (is (sccs-equalp
       (coalton-impl/algorithm::tarjan-scc
        '((a b)
          (b c e)
          (c a d)
          (d e)
          (e d)))
       '((a b c)
         (d e))))

  ;; Can we out wiki the pedia (https://commons.wikimedia.org/wiki/File:Scc-1.svg)
  (is (sccs-equalp
       (coalton-impl/algorithm::tarjan-scc
        '((a b)
          (b c e f)
          (c d g)
          (d c h)
          (e a f)
          (f g)
          (g f)
          (h d g)))
       '((a b e)
         (c d h)
         (f g))))

  ;; Wikipedia example 2 (https://commons.wikimedia.org/wiki/File:Graph_Condensation.svg)
  (is (sccs-equalp
       (coalton-impl/algorithm::tarjan-scc
        '((a c)
          (b a f)
          (c b e)
          (d c m)
          (e d f n)
          (f g i k)
          (g h)
          (h i j)
          (i g j)
          (j)
          (k j l)
          (l k)
          (m n)
          (n o p)
          (o m)
          (p o k)))
       '((a b c d e)
         (m n o p)
         (f)
         (l k)
         (g h i)
         (j)))))
