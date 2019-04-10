structure  TigerCompile =
struct

val (program,_) = Parse.parse (0,thisLexer,print_error,())
val executable  = Code.print , program

end
