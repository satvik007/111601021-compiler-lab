type pos = int
(* type lexresult = Tiger.token *)

type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token

val nested_commenting = ref 0;

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

<INITIAL> "/*"    => (
            nested_commenting := !nested_commenting + 1;
            YYBEGIN COMMENT; continue());

<COMMENT> "/*"    => (
                      nested_commenting := !nested_commenting + 1;
                      continue());

<COMMENT> "*/"    => (
                      nested_commenting := !nested_commenting - 1;
                      if (!nested_commenting = 0) then (YYBEGIN INITIAL; continue())
                      else continue());
                
<COMMENT> . | [\n]       => (continue());

<INITIAL> {eol} =>            (continue());

<INITIAL> {whitespace} => (continue());
<INITIAL> "/"         => (Tokens.DIVIDE (yypos, yypos+1));
<INITIAL> "*"         => (Tokens.TIMES (yypos, yypos+1));
<INITIAL> "-"         => (Tokens.MINUS (yypos, yypos+1));
<INITIAL> "+"         => (Tokens.PLUS (yypos, yypos+1));
<INITIAL> ";"         => (Tokens.SEMICOLON (yypos, yypos+1));
<INITIAL> ":="        => (Tokens.ASSIGN (yypos, yypos + 2));
<INITIAL> ":"         => (Tokens.COLON(yypos, yypos + 1));
<INITIAL> ","         => (Tokens.COMMA(yypos, yypos + 1));
<INITIAL> "("         => (Tokens.LPAREN(yypos, yypos + 1));
<INITIAL> ")"         => (Tokens.RPAREN(yypos, yypos + 1));

<INITIAL> "let"       => (Tokens.LET(yypos, yypos + 3));
<INITIAL> "in"        => (Tokens.IN(yypos, yypos + 2));
<INITIAL> "end"       => (Tokens.END(yypos, yypos + 3));
<INITIAL> "var"       => (Tokens.VAR(yypos, yypos + 3));
<INITIAL> "if"        => (Tokens.IF(yypos, yypos + 2));
<INITIAL> "then"      => (Tokens.THEN(yypos, yypos + 4));
<INITIAL> "else"      => (Tokens.ELSE(yypos, yypos + 4));
<INITIAL> "function"  => (Tokens.FUNCTION(yypos, yypos + 8));

<INITIAL> ({letter}({letter}|{digit}|"_")*) | ("_main")
            =>  (Tokens.ID(yytext, yypos,
                    yypos + size yytext));

<INITIAL> {digit}+    => (Tokens.INT(IntFromString yytext,
                    yypos, yypos + size yytext));
.           =>  (continue());
