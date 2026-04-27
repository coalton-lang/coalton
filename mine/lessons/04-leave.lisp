(
 "If you were thinking there must be an opposite to join, you're right, it's \"leave\". With leave, the last item leaves from a parenthetical expression. Again, doesn't matter what it is: a single symbol or another parenthetical. It doesn't matter where your cursor is in a particular expression, all that matters is what nesting depth it's at. (Same for join too, by the way.)

Motion: (a b| c d) --> press Alt+← --> (a b| c) d

The above has an item leave in the forward direction. We can also have an item leave in the backward direction:

Motion: a (b c|) d --> press Alt+Shift+→ --> a b (c|) d
"

 ("(a b c d)" . "(a b c) d")
 ("(a b c (d))" . "(a b c) (d)")
 ("(a b c d)" . "() a b c d")
 ("(a (b c) d)" . "(a (b) c d)")
)
