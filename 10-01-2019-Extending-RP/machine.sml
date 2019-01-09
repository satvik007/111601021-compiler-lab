(** * The reverse polish machine. *)

structure Machine =
struct

(*

The reverse polish machine is a machine with a stack of integers. It
supports the following operations.

1. Push an integer on to the stack

2. Execute a binary operator using the top two arguments of the stack
   and push the result on to the stack.

3. Clearing the stack

4. Printing the top and printing the entire stack contents.

We capture this "instruction set" as a ML data type (what else). You
can see this as the abstract syntax tree for the "assembly language"
for the reverse polish machine.

An "assembly language" program in rpn is just a list of such
instructions.

*)

datatype Inst
  = Exec of Ast.BinOp
  | Push of int
  | ClearStack
  | PrintTop
  | PrintStack


type Program = Inst list

(*

You can now skip the rest of the module for the first reading and move
over to translate where the actual compiler exists. The rest of the
section is a simulator for the reverse polish machine.

*)

(** ** Simulator for the machine

The reverse polish machine consists of a stack. We also have an
exception that is raised where we encounter a stack underflow during
the operations of the machine.

*)

type Stack   = int list
exception StackUnderflow of Stack

(* Some helper functions for printing the stack *)

fun printstack stack = let val conts = String.concatWith ", " (List.map Int.toString stack)
		       in print ("[" ^ conts ^ "]\n")
		       end
fun printtop (x::xs) = print (Int.toString x ^ "\n")
  | printtop _       = raise StackUnderflow []

(* This function performs a single instruction of the stack machine *)

fun step (Push x)     stack           = x :: stack
  | step PrintStack   stack           = (printstack stack; stack)
  | step PrintTop     stack           = (printtop stack; stack)
  | step ClearStack   _               = []
  | step (Exec oper) (a :: b :: rest) = Ast.binOpDenote oper a b :: rest
  | step _           stack            = raise StackUnderflow stack

(* And finally this runs a program. *)


val run = List.foldl (fn (inst,stack) => step inst stack) []


(* Conversion of machine instructions to strings *)


fun instToString (Exec oper) = Ast.binOpToString oper
  | instToString (Push x   ) = Int.toString x
  | instToString ClearStack  = "c"
  | instToString PrintTop    = "p"
  | instToString PrintStack  = "s"

val programToString = String.concatWith " " o List.map instToString

end
