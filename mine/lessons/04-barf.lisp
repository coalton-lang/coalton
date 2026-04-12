(
 "If you were thinking there must be an opposite to slurping, you're right, it's barfing. Barfing means to eject the last item in a parenthetical expression. Again, doesn't matter what it is: a single symbol or another parenthetical. It doesn't matter where your cursor is in a particular expression, all that matters is what nesting depth it's at. (Same for slurping too, by the way.)

Motion: (a b| c d) --> press Alt+Left --> (a b| c) d"

 ("(a b c d)" . "(a b c) d")
 ("(a b c (d))" . "(a b c) (d)")
 ("(a b c d)" . "() a b c d")
 ("(a (b c) d)" . "(a (b) c d)")
)
