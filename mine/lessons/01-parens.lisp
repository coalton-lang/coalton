(
 "Where do we start? Oh, inputting parentheses! Just press the ( key to insert a pair. Once you insert a pair, you can press anything you want in and around them.

Motion: | --> press ( --> (|)

Then what does the close paren do? Well, it doesn't insert a close paren, instead, it just skips to the closing paren of the parenthetical expression you're in.

Motion: (|) --> press ) --> ()|
Motion: (a| b c) --> press ) --> (a b c)|
"

 ("" . "()")
 ("" . "(())")
 ("" . "()()")
 ("" . "(1 2 3)")
 ("()" . "(1 (2) 3)"))
