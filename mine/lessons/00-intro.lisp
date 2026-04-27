(
 "Welcome to the Structural Editing tutorial! In this tutorial you will learn about how to edit Lisp-like languages in a \"structural\" way. With structural editing, parentheses, brackets, and string delimiters are manipulated using special commands that always keep them balanced. In this tutorial, we'll teach you how. You only need to learn four intuitive commands:

Join:   (a b) c d  =>  (a b c) d
Leave:  (a b) c d  =>  (a) b c d
Shed:   (a b c) d  =>  a b c d
Split:  (a b c d)  =>  (a b) (c d)

If these make no sense, don't worry! We will learn them.

In these lessons, we will use the character '|' to indicate the position of your cursor. If your cursor is a box or an underline, then this character represents the left edge of it.

Let's get started!
"
)
