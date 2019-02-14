type pos = int
(* type lexresult = Tiger.token *)

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


%%
%header (functor TigerLexFun (structure Tokens : Tiger_TOKENS));
%s COMMENT;
digit = [0-9] ;
eol = ("\n\r"|"\r\n"|"\r"|"\n") ;
whitespace = (" "|\t)  ;
letter = [a-zA-Z]   ;
esc = ("\a"|"\b"|"\f"|"\n"|"\r"|"\t"|"\v");
%%

{eol} => (continue());
{whitespace} => (continue());
"/"         => (Tokens.DIVIDE (yypos, yypos+1));
"*"         => (Tokens.TIMES (yypos, yypos+1));
"-"         => (Tokens.MINUS (yypos, yypos+1));
"+"         => (Tokens.PLUS (yypos, yypos+1));
";"         => (Tokens.SEMICOLON (yypos, yypos+1));
{digit}+    => (Tokens.INT(IntFromString yytext,
                    yypos, yypos + size yytext));
.           =>  (continue());
