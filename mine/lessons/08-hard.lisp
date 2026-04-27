(
 "Ready for a real challenge? These require some creative thinking. Remember, you have all four structural commands plus regular typing and deleting at your disposal.
"

 ("a b c d" . "(a b) (c d)")
 ("(a b) (c d) (e f)" . "(a b c d e f)")
 ("((a b c d))" . "(a) (b) (c) (d)")
 ("(a b c d)" . "(a (b c) d)")
 ("(a b c d)" . "((a b) (c d))")
 ("define (f) y (* x) y" . "(define (f x y) (* x y))"))
