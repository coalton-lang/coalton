(
 "Now we get to our first real structural command. This is when your cursor is inside of parentheses, and you \"grab\" the next thing after the parentheses. It doesn't matter what it is: a number, a symbol, another parenthesis thing. It takes whatever is there, and puts it inside of the existing pair. For this reason, we call this operation \"join\".

Motion: (a b|) c d --> press Alt+→ --> (a b| c) d

The above has items join in the forward direction. We can also have items join in the backward direction:

Motion: a (b| c) d --> press Alt+Shift+← --> (a b| c) d

For some of these, you might need to press multiple keys!
"
 
 ("(a b) c d" . "(a b c) d")
 ("(a b) c d" . "(a b c d)")
 ("(a b) (c d)" . "(a b (c d))")
 ("(a b c d)" . "((a b c d))")
 ("a b c d" . "(a) (b) (c) (d)"))
