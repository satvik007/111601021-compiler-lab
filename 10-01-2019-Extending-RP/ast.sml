(* The abstract syntax tree for expression *)
structure Ast = struct

(*

Every language has a syntax or a grammar which defines what are valid
constructs of the language. In our case we have the expression
language. Languages have two grammars

1. The _concrete syntax_ which define what are the strings that are
   valid programs.

2. The _abstract syntax_ which captures the essence of the syntax.

The concrete syntax has stuff that is only required to disambiguation
the strings. For example, we use parenthesis in the string `(2 + 3) *
4` only to distinguish it from `2 + (3 * 4)`. If we are allowed to use
trees, the above two expressions can be represented as trees as
show below.

```

+--------------------------------------------------------+
|      	   (2 + 3) * 4       |      2 + (3 * 4)    	 |
|----------------------------|---------------------------|
|      	       	       	     | 	       	       	       	 |
|              *             |        +                  |
|      	      / \            |       / \                 |
|            +   4           |      2   *                |
|    	    / \              |         / \               |
|          2   3             |        3   4              |
|                            |                           |
+--------------------------------------------------------+

```

Notice that here the tree structure completely specifies what the
expression and no parenthesis is required for disambiguation. Inside
the compiler an input program is therefore represented using the
abstract syntax in which case it is some times called the _abstract
syntax tree_ or AST for short and hence the name of the module.

The advantage of Standard ML for writing compilers starts showing
itself right away: It almost seems that algebraic data types are
tailor made to represent abstract syntax trees of programming
languages. Here is the one for our rather humble expression language.

*)

datatype Expr  = Const of int
	       | Op    of Expr * BinOp * Expr

     and BinOp = Plus
	       | Minus
	       | Mul

(*

The process of converting the input program, which is a string, to the
abstract syntax guided by the concrete syntax is called
_parsing_. This module does not have the code for parsing our
expression language. We use the mlyacc and mllex programs to write our
expression parsers. You can have a look at the expr.grm and expr.lex
files in this repository but it would be clear only after we have seen
some bit of parsing theory. So you can skip it for time being.

The rest of the module can be skipped for the first reading. Instead
start reading the machine.sml where we describe the reverse polish
machine.


*)


(** ** Meanings of expression

We give the "meaning" of an expression by converting it into ML
values. Our target is to give the meaning of expressions as
integers. For this purpose we define the meaning of an operator
(binary) as a bivariate function on ints.

 *)

fun binOpDenote Plus  x y = x + y
  | binOpDenote Minus x y = x - y
  | binOpDenote Mul   x y = x * y;

fun exprDenote (Const x)       = x
  | exprDenote (Op (x,oper,y)) = binOpDenote oper (exprDenote x) (exprDenote y);

(* Conversion to strings *)

fun binOpToString Plus  = "+"
  | binOpToString Minus = "-"
  | binOpToString Mul   = "*"

(* Some helper functions *)
fun plus  a b = Op (a, Plus, b)
fun minus a b = Op (a, Minus, b)
fun mul   a b = Op (a, Mul, b)


end
