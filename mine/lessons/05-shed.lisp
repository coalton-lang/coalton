(
 "We have some left and right motion, does that mean there's an up and down? Yeah, sort of! Our \"up\" motion is called \"shed\". Think of it as a pair of parentheses lifting up and disappearing into the distance. More seriously though, if your cursor is inside of a parenthetical expression, it just sheds those parentheses away.

Motion: (a b| c d) --> press Alt+↑ --> a b c d
"

 ("(a b c)" . "a b c")
 ("((a b c))" . "(a b c)")
 ("(a (b c) d)" . "(a b c d)")
 ("(((a)))" . "a")
 ("(a (b (c) d) e)" . "(a (b c d) e)"))
