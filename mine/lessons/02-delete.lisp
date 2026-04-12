(
 "Okay, we can enter parentheses. How can we delete them? Well, if you try to delete a close parenthesis, you'll notice the cursor just goes inside. Deleting an open parenthesis? Not a chance! So the only way we can delete a paren-pair is to empty it of its contents, and when you're left with just (), place your cursor inside and press backspace.

Motion: ()| --> press Backspace --> (|) --> press Backspace --> |
"
 
 ("()" . "")
 ("(())" . "")
 ("(a b c)" . "")
 ("(a (b) c)" . "")
)
