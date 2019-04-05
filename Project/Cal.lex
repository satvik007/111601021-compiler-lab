type pos = int

type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token

fun eof() = Tokens.EOF(0,0)
fun IntFromString str = let
                            val x = Int.fromString str
                        in
                            case x of
                                    SOME n => n
                                |   NONE => 0
                        end

fun toStrConst str = String.substring (str , 1 , (String.size str) - 2)

%%
%header (functor CalLexFun (structure Tokens : Cal_TOKENS));
%s STR;
digit = [0-9] ;
eol = ("\n\r"|"\r\n"|"\r"|"\n") ;
whitespace = (" "|\t)  ;
letter = [a-zA-Z]   ;
letdig = {digit}|{letter} ;
esc = ("\a"|"\b"|"\f"|"\n"|"\r"|"\t"|"\v");
%%

<INITIAL> {eol} => (continue());

<INITIAL> {whitespace}+ => (continue());

<INITIAL> ":="        => (Tokens.ASSIGN (yypos, yypos+2));
<INITIAL> "&"         => (Tokens.AND (yypos, yypos+1));
<INITIAL> "|"         => (Tokens.OR (yypos, yypos+1));
<INITIAL> ">="        => (Tokens.GE (yypos, yypos+2));
<INITIAL> ">"         => (Tokens.GT (yypos, yypos+1));
<INITIAL> "<="        => (Tokens.LE (yypos, yypos+2));
<INITIAL> "<"         => (Tokens.LT (yypos, yypos+1));
<INITIAL> "!="        => (Tokens.NEQ (yypos, yypos+2));
<INITIAL> "="         => (Tokens.EQ (yypos, yypos+1));
<INITIAL> "/"         => (Tokens.DIVIDE (yypos, yypos+1));
<INITIAL> "*"         => (Tokens.TIMES (yypos, yypos+1));
<INITIAL> "-"         => (Tokens.MINUS (yypos, yypos+1));
<INITIAL> "+"         => (Tokens.PLUS (yypos, yypos+1));
<INITIAL> "."         => (Tokens.DOT (yypos, yypos+1));
<INITIAL> "}"         => (Tokens.RBRACE (yypos, yypos+1));
<INITIAL> "{"         => (Tokens.LBRACE (yypos, yypos+1));
<INITIAL> "]"         => (Tokens.RBRACK (yypos, yypos+1));
<INITIAL> "["         => (Tokens.LBRACK (yypos, yypos+1));
<INITIAL> ")"         => (Tokens.RPAREN (yypos, yypos+1));
<INITIAL> "("         => (Tokens.LPAREN (yypos, yypos+1));
<INITIAL> ";"         => (Tokens.SEMICOLON (yypos, yypos+1));
<INITIAL> ":"         => (Tokens.COLON (yypos, yypos+1));
<INITIAL> ","	      => (Tokens.COMMA (yypos, yypos+1));
<INITIAL> "%"	      => (Tokens.MOD (yypos, yypos+1));
<INITIAL> int         => (Tokens.INT (yypos, yypos+3));
<INITIAL> string      => (Tokens.STRING (yypos, yypos+6));
<INITIAL> bool        => (Tokens.BOOL (yypos , yypos+4));
<INITIAL> true        => (Tokens.BOOLCONST (true, yypos , yypos+4));
<INITIAL> false        => (Tokens.BOOLCONST (false, yypos , yypos+4));
<INITIAL> {digit}+    => (Tokens.NUMCONST(IntFromString yytext, 
                            yypos, yypos + size yytext));

<INITIAL> \"(\\.|[^"\\])*\"
                        => (Tokens.STRINGCONST(toStrConst yytext,
                                            yypos, yypos + size yytext));

(* In C- id is very restrictive but here we also allow _*)
<INITIAL> ({letter}|"_")(({letter}|{digit}|"_")*)
                        =>  (Tokens.ID(yytext, yypos, 
                            yypos + size yytext));

.           => (print("error"); print(yytext); continue());