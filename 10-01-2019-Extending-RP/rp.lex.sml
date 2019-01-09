structure RPLex=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "rp.lex"*)type lexresult             = Machine.Inst option
fun eof ()                 = NONE
fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt  = toSigned o String.explode

(*#line 17.1 "rp.lex.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\000\000\000\000\000\000\000\000\000\014\014\000\000\014\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\014\000\000\012\000\000\000\000\000\000\011\010\003\009\003\003\
\\008\008\008\008\008\008\008\008\008\008\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\007\003\003\003\003\003\003\003\003\003\003\003\003\
\\006\003\003\005\003\003\003\003\003\003\003\003\003\003\003\000\
\\000"
),
 (3, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (12, 
"\012\012\012\012\012\012\012\012\012\012\013\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\014\014\000\000\014\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\014\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = map f (rev (tl (rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [], trans = 3},
{fin = [(N 10)], trans = 3},
{fin = [(N 20)], trans = 3},
{fin = [(N 18)], trans = 3},
{fin = [(N 22)], trans = 3},
{fin = [(N 10)], trans = 3},
{fin = [(N 14)], trans = 3},
{fin = [(N 12)], trans = 3},
{fin = [(N 16)], trans = 0},
{fin = [], trans = 12},
{fin = [(N 6)], trans = 0},
{fin = [(N 2)], trans = 14}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  10 => let val yytext=yymktext() in (*#line 20.23 "rp.lex"*)SOME (Machine.Push (toInt yytext))(*#line 148.1 "rp.lex.sml"*)
 end
| 12 => ((*#line 21.23 "rp.lex"*)SOME (Machine.Exec Ast.Plus      )(*#line 150.1 "rp.lex.sml"*)
)
| 14 => ((*#line 22.23 "rp.lex"*)SOME (Machine.Exec Ast.Minus     )(*#line 152.1 "rp.lex.sml"*)
)
| 16 => ((*#line 23.23 "rp.lex"*)SOME (Machine.Exec Ast.Mul       )(*#line 154.1 "rp.lex.sml"*)
)
| 18 => ((*#line 24.23 "rp.lex"*)SOME (Machine.PrintTop)(*#line 156.1 "rp.lex.sml"*)
)
| 2 => ((*#line 18.23 "rp.lex"*)lex()  (* White spaces are ignored *) (*#line 158.1 "rp.lex.sml"*)
)
| 20 => ((*#line 25.23 "rp.lex"*)SOME (Machine.PrintStack)(*#line 160.1 "rp.lex.sml"*)
)
| 22 => ((*#line 26.23 "rp.lex"*)SOME (Machine.ClearStack)(*#line 162.1 "rp.lex.sml"*)
)
| 6 => ((*#line 19.23 "rp.lex"*)lex()  (* A line comment *)           (*#line 164.1 "rp.lex.sml"*)
)
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
