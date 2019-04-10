CM.make "sources.cm";
val program = Parse.parse "test/test2.tig";
val outs = TextIO.openOut "final.py";
val executable = Code.print (outs, program);
val ast = TextIO.openOut "ast.txt";
val _ = PrintAbsyn.print (ast, program);
OS.Process.exit(OS.Process.success)


