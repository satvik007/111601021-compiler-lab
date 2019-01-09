# Extending the reverse polish language

In the repository we have a simple example of a compiler and a
machine. We will not go over the motions of extending it to support
with more features.


[Parenthesising expressions]
:  Currently there is no way to enter the input (2 + 3) * 4 as the
   expression language does not support the bracket. Modify the code in
   the reverse polish source tree to start supporting bracketting of
   expression.

[Adding the division operator]
:  Add an operator for doing division.



In each of this case it would be good if you start with answering the
following questions.

1. Which changes result in a change in the abstract syntax and which of
   them changes the concrete syntax ?

2. What are the files that will have to be changed when the
   abstract/concrete syntax changes ?

3. When will you have to modify `translate.sml` ?

4. When will you have to modify the machine `rp` ?
