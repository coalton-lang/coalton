(
 "Last one! Split is the \"down\" motion. If shed makes parens disappear by pulling them up, split sort of makes new ones by splitting them apart: It splits one pair of parentheses into two pairs, right at your cursor position. Everything to your left stays in the first group, everything to your right goes into the second.

Motion: (a b| c d) --> press Alt+↓ --> (a b)| (c d)
"

 ("(a b c d)" . "(a b) (c d)")
 ("(a b c d)" . "(a) (b c d)")
 ("(a b c d)" . "(a b c) (d)")
 ("(a b c d e f)" . "(a b) (c d) (e f)")
 ("((a b c d))" . "((a b) (c d))")
 ("((a b c d))" . "((a b)) ((c d))"))
