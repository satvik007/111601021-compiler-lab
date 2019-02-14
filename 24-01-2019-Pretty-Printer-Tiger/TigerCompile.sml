structure  TigerCompile =
struct

(* This three structure definitions are what the lexer and parser *)

structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token) (* Generate the LR values structure *)
structure TigerLex    = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure TigerParser = Join( structure ParserData = TigerLrVals.ParserData
			     structure Lex        = TigerLex
			     structure LrParser   = LrParser
			   )

(* Build Lexers *)
fun makeTigerLexer strm = TigerParser.makeLexer (fn n => TextIO.inputN(strm,n))
val makeFileLexer      = makeTigerLexer o TextIO.openIn

(* Parse command line and set a suitable lexer *)
(* If takes the file name as the input and parses that file
 if no file name is mentioned as the input then it reads the input from the terminal
 Otherwise it will give the error*)

val thisLexer = case CommandLine.arguments() of
		    []  => makeTigerLexer TextIO.stdIn
		 |  [x] => makeFileLexer x
		 |  _   => (TextIO.output(TextIO.stdErr, "usage: ec file"); OS.Process.exit OS.Process.failure)



fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr,
					    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

(* The portion of the code that does the actual compiling *)

val (program,_) = TigerParser.parse (0,thisLexer,print_error,())
val executable  = Translate.compile program

end
