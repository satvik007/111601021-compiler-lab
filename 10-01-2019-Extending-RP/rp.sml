(*

This is a reverse polish calculator. This is meant to be compiled into
a standalone executable using mlton. The reverse polish calculator
described here usually takes multi-line integer expressions in reverse
polish notation and converts it into.

Besides the operators +, *, and -, the calculator understands the
following command.

p - print the top of the stack
s - print the entire stack
c - clear the stack.
# - starts a line comment. Ends when the line ends.

*)

structure RP =
struct


fun load lexer = case lexer () of
		     SOME i => i :: load lexer
		   | NONE   => []

(*

Lexer suitable for interactive usage. No buffering and hence
slow. However, instant feed back is available and hence should be used
on interactive sessions

*)

val interactive = RPLex.makeLexer (fn _ => TextIO.inputN (TextIO.stdIn,1))

fun lexfile file = let val strm = TextIO.openIn file
		   in RPLex.makeLexer (fn n => TextIO.inputN(strm,n))
		   end

(*

TODO: For standard input which is not from the terminal, we would like
to use a more efficient lexer than interactive. This will be relevent
when we want to pipe the output to rp.

*)


(* Running with a lexer *)
fun runWithLexer lexer = let fun loop stack = case lexer () of
						  NONE      => ()
					       |  SOME inst => loop (Machine.step inst stack)
			 in loop []
			 end


val _ =  ( case CommandLine.arguments() of
	       [] => runWithLexer interactive
	    |  xs => (List.map (runWithLexer o lexfile) xs; ())
	 )
	 handle Machine.StackUnderflow s =>
		( print "error: Stack underflow: " ;
		  Machine.printstack s;
		  OS.Process.exit OS.Process.failure
		)

end
