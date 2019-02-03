type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum 
val linePos = ErrorMsg.linePos 
fun err(p1, p2) = ErrorMsg.error p1

val comment_color = "\027[3m\027[4;35m"
val reset_color = "\027[0m"
fun printComment (str) = print (comment_color ^ str ^ reset_color)

fun eof() = let val pos = hd (!linePos) in Tokens.EOF(pos, pos) end
fun IntFromString str = let val SOME x = Int.fromString str in x end
val nested_commenting = ref 0;

%%
%s COMMENT;
digit = [0-9];
eol = ("\n\r"|"\r\n"|"\r"|"\n") ;
whitespace = [\ \t]+;
letter = [a-zA-Z];
esc = [\a\b\f\n\r\t\v];

%%

<INITIAL> "/*"    => (printComment(yytext);
            nested_commenting := !nested_commenting + 1;
            YYBEGIN COMMENT; continue());

<COMMENT> "/*"    => (printComment(yytext);
                      nested_commenting := !nested_commenting + 1;
                      continue());

<COMMENT> "*/"    => (printComment(yytext);
                      nested_commenting := !nested_commenting - 1;
                      if (!nested_commenting = 0) then (YYBEGIN INITIAL; continue())
                      else continue());
                
<COMMENT> . | [\n]       => (printComment(yytext); continue());

<INITIAL> {eol}   => (lineNum := !lineNum + 1;
                      linePos := yypos :: !linePos;
                      print(yytext);
                      continue());

<INITIAL> {whitespace} => (print(yytext); continue());
<INITIAL> ":="        => (Tokens.ASSIGN (yypos, yypos+2));
<INITIAL> "&"         => (Tokens.AND (yypos, yypos+1));
<INITIAL> "|"         => (Tokens.OR (yypos, yypos+1));
<INITIAL> ">="        => (Tokens.GE (yypos, yypos+2));
<INITIAL> ">"         => (Tokens.GT (yypos, yypos+1));
<INITIAL> "<="        => (Tokens.LE (yypos, yypos+2));
<INITIAL> "<"         => (Tokens.LT (yypos, yypos+1));
<INITIAL> "<>"        => (Tokens.NEQ (yypos, yypos+2));
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
<INITIAL> ","	        => (Tokens.COMMA (yypos, yypos+1));

<INITIAL> var  	    => (Tokens.VAR (yypos, yypos+3));
<INITIAL> function    => (Tokens.FUNCTION (yypos, yypos+8));
<INITIAL> break       => (Tokens.BREAK (yypos, yypos+5));
<INITIAL> of          => (Tokens.OF (yypos, yypos+2));
<INITIAL> end         => (Tokens.END (yypos, yypos+3));
<INITIAL> in          => (Tokens.IN (yypos, yypos+2));
<INITIAL> nil         => (Tokens.NIL (yypos, yypos+3));
<INITIAL> let         => (Tokens.LET (yypos, yypos+3));
<INITIAL> do          => (Tokens.DO (yypos, yypos+3));
<INITIAL> to          => (Tokens.TO (yypos, yypos+2));
<INITIAL> for         => (Tokens.FOR (yypos, yypos+3));
<INITIAL> while       => (Tokens.WHILE (yypos, yypos+5));
<INITIAL> else        => (Tokens.ELSE (yypos, yypos+4));
<INITIAL> then        => (Tokens.THEN (yypos, yypos+4));
<INITIAL> if          => (Tokens.IF (yypos, yypos+2));
<INITIAL> array       => (Tokens.ARRAY (yypos, yypos+5));

<INITIAL> {digit}+    => (Tokens.INT(IntFromString yytext, 
                    yypos, yypos + size yytext));

<INITIAL> ({letter}({letter}|{digit}|"_")*) | ("_main")
            =>  (Tokens.ID(yytext, yypos, 
                    yypos + size yytext));
<INITIAL> \"([^\n\\\"]|\\.)*\"
            => (Tokens.STRING(yytext, yypos, 
                    yypos + size yytext));


.           => (print(yytext); continue());



