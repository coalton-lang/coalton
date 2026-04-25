(
 "Now we get to our first real structural command. This is when your cursor is inside of parentheses, and you \"grab\" the next thing after the parentheses. It doesn't matter what it is: a number, a symbol, another parenthesis thing. It takes whatever is there, and puts it inside of the existing pair. For some reason, Lisp programmers like to call this \"slurping\".

Motion: (a b|) c d --> press Alt+→ --> (a b| c) d

The above slurps in the forward direction. We can also slurp in the backward direction:

Motion: a (b| c) d --> press Alt+Shift+← --> (a b| c) d

For some of these, you might need to press multiple keys!"
 
 ("(a b) c d" . "(a b c) d")
 ("(a b) c d" . "(a b c d)")
 ("(a b) (c d)" . "(a b (c d))")
 ("(a b c d)" . "((a b c d))")
 ("a b c d" . "(a) (b) (c) (d)"))
