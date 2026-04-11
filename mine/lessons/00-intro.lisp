(
 "Welcome to the Structural Editing tutorial! In this tutorial you will learn about how to edit Lisp-like languages in a\"structural\" way. With structural editing, parentheses, brackets, and string delimiters are manipulated using special commands that always keep them balanced. In this tutorial, we'll teach you them all. You only need to learn four intuitive commands:

Slurp:  (a b) c d  ==>  (a b c)
Barf:   (a b) c d  ==>  (a) b c
Splice: (a b c) d  ==>  a b c
Cleave  (a b c d)  ==>  (a b)(c d)

We will use the character '|' to indicate the position of your cursor. If your cursor is a box or an underline, then this character represents the left edge of it.

Let's get started!
"
)
