type lexresult             = Machine.Inst option
fun eof ()                 = NONE
fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt  = toSigned o String.explode

%%
%structure RPLex
whitespace=[\ \r\t\n];
digit=[0-9];
%%
{whitespace}+     => (lex()  (* White spaces are ignored *) );
"#".*\n           => (lex()  (* A line comment *)           );
[+-~]?{digit}+    => (SOME (Machine.Push (toInt yytext)));
"+"               => (SOME (Machine.Exec Ast.Plus      ));
"-"               => (SOME (Machine.Exec Ast.Minus     ));
"*"               => (SOME (Machine.Exec Ast.Mul       ));
"p"               => (SOME (Machine.PrintTop));
"s"               => (SOME (Machine.PrintStack));
"c"               => (SOME (Machine.ClearStack));
