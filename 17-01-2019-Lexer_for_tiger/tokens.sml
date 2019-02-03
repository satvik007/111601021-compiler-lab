structure Tokens : Tiger_TOKENS =
struct
  (* A "scaffold" structure for debugging lexers. *)

type linenum = int
type token = string

(* Syntax highlighting *)

val reset_color = "\027[0m";        (*DEFAULT SETTINGS*)
val kw_color = "\027[0;31m";        (*RED*)
val op_color = "\027[0;33m";        (*YELLOW*)
val str_color = "\027[0;32m";       (*GREEN*)
val id_color = "\027[0;34m";        (*BLUE*)
val int_color = "\027[4;36m";       (*UNDERLINED CYAN*)

fun TYPE(i,j) = kw_color ^ "type" ^ reset_color
fun VAR(i,j) = kw_color ^ "var" ^ reset_color
fun FUNCTION(i,j) = kw_color ^ "function" ^ reset_color
fun BREAK(i,j) = kw_color ^ "break" ^ reset_color
fun OF(i,j) = kw_color ^ "of" ^ reset_color
fun END(i,j) = kw_color ^ "end" ^ reset_color
fun IN(i,j) = kw_color ^ "in" ^ reset_color
fun NIL(i,j) = kw_color ^ "nil" ^ reset_color
fun LET(i,j) = kw_color ^ "let" ^ reset_color
fun DO(i,j) = kw_color ^ "do" ^ reset_color
fun TO(i,j) = kw_color ^ "to" ^ reset_color
fun FOR(i,j) = kw_color ^ "for" ^ reset_color
fun WHILE(i,j) = kw_color ^ "while" ^ reset_color
fun ELSE(i,j) = kw_color ^ "else" ^ reset_color
fun THEN(i,j) = kw_color ^ "then" ^ reset_color
fun IF(i,j) = kw_color ^ "if" ^ reset_color
fun ARRAY(i,j) = kw_color ^ "array" ^ reset_color
fun ASSIGN(i,j) = op_color ^ ":=" ^ reset_color
fun OR(i,j) = op_color ^ "|" ^ reset_color
fun AND(i,j) = op_color ^ "&" ^ reset_color
fun GE(i,j) = op_color ^ ">=" ^ reset_color
fun GT(i,j) = op_color ^ ">" ^ reset_color
fun LE(i,j) = op_color ^ "<=" ^ reset_color
fun LT(i,j) = op_color ^ "<" ^ reset_color
fun NEQ(i,j) = op_color ^ "<>" ^ reset_color
fun EQ(i,j) = op_color ^ "=" ^ reset_color
fun DIVIDE(i,j) = op_color ^ "/" ^ reset_color
fun TIMES(i,j) = op_color ^ "*" ^ reset_color
fun MINUS(i,j) = op_color ^ "-" ^ reset_color
fun PLUS(i,j) = op_color ^ "+" ^ reset_color
fun DOT(i,j) = op_color ^ "." ^ reset_color
fun RBRACE(i,j) = "}" ^ reset_color
fun LBRACE(i,j) = "{" ^ reset_color
fun RBRACK(i,j) = "]" ^ reset_color
fun LBRACK(i,j) = "[" ^ reset_color
fun RPAREN(i,j) = ")" ^ reset_color
fun LPAREN(i,j) = "(" ^ reset_color
fun SEMICOLON(i,j) = op_color ^ ";" ^ reset_color
fun COLON(i,j) = op_color ^ ":" ^ reset_color
fun COMMA(i,j) = op_color ^ "," ^ reset_color
fun STRING(s,i,j) = str_color ^  s  ^ reset_color
fun INT(c,i,j) = int_color ^ Int.toString(c) ^ reset_color
fun ID(s,i,j) = id_color ^ s ^ reset_color
fun EOF(i,j) = "EOF " ^ reset_color
end
