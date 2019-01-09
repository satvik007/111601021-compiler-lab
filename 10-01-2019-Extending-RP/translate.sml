structure Translate =
struct

fun compileExpr (Ast.Const x)         = [Machine.Push x]
  | compileExpr (Ast.Op (x, oper, y)) = compileExpr y @ compileExpr x @ [Machine.Exec oper];


fun compile []        = []
  | compile (x :: xs) = compileExpr x @ Machine.PrintTop :: compile xs

end
