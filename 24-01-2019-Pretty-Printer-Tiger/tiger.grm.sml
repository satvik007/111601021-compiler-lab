functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "tiger.grm"*)
(*#line 12.1 "tiger.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\014\000\004\000\013\000\005\000\012\000\006\000\011\000\
\\007\000\010\000\008\000\009\000\000\000\
\\001\000\003\000\008\000\010\000\007\000\011\000\006\000\020\000\005\000\000\000\
\\001\000\004\000\013\000\005\000\012\000\006\000\011\000\007\000\010\000\
\\008\000\009\000\021\000\027\000\000\000\
\\001\000\009\000\037\000\000\000\
\\001\000\010\000\030\000\000\000\
\\001\000\012\000\029\000\000\000\
\\001\000\013\000\041\000\000\000\
\\001\000\019\000\038\000\000\000\
\\048\000\000\000\
\\049\000\000\000\
\\050\000\003\000\008\000\010\000\007\000\011\000\006\000\020\000\005\000\000\000\
\\051\000\000\000\
\\052\000\018\000\020\000\000\000\
\\053\000\008\000\009\000\000\000\
\\054\000\008\000\009\000\000\000\
\\055\000\004\000\013\000\005\000\012\000\008\000\009\000\000\000\
\\056\000\004\000\013\000\005\000\012\000\008\000\009\000\000\000\
\\057\000\008\000\009\000\000\000\
\\058\000\000\000\
\\059\000\000\000\
\\060\000\004\000\013\000\005\000\012\000\006\000\011\000\007\000\010\000\
\\008\000\009\000\022\000\040\000\000\000\
\\061\000\004\000\013\000\005\000\012\000\006\000\011\000\007\000\010\000\
\\008\000\009\000\000\000\
\\062\000\000\000\
\\063\000\004\000\013\000\005\000\012\000\006\000\011\000\007\000\010\000\
\\008\000\009\000\017\000\039\000\000\000\
\\064\000\003\000\008\000\010\000\007\000\011\000\006\000\020\000\005\000\000\000\
\\065\000\000\000\
\\066\000\002\000\042\000\004\000\013\000\005\000\012\000\006\000\011\000\
\\007\000\010\000\008\000\009\000\000\000\
\\067\000\003\000\008\000\010\000\007\000\011\000\006\000\020\000\005\000\000\000\
\\068\000\000\000\
\\069\000\002\000\028\000\000\000\
\\070\000\014\000\019\000\000\000\
\\071\000\000\000\
\\072\000\004\000\013\000\005\000\012\000\006\000\011\000\007\000\010\000\
\\008\000\009\000\000\000\
\"
val actionRowNumbers =
"\011\000\001\000\009\000\002\000\
\\031\000\013\000\012\000\002\000\
\\002\000\002\000\002\000\002\000\
\\011\000\003\000\030\000\006\000\
\\032\000\005\000\025\000\018\000\
\\017\000\016\000\015\000\014\000\
\\010\000\002\000\031\000\028\000\
\\004\000\008\000\024\000\021\000\
\\029\000\007\000\027\000\002\000\
\\020\000\025\000\002\000\019\000\
\\028\000\033\000\023\000\022\000\
\\026\000\000\000"
val gotoT =
"\
\\001\000\045\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\003\000\013\000\000\000\
\\005\000\016\000\006\000\015\000\007\000\014\000\000\000\
\\000\000\
\\000\000\
\\003\000\019\000\000\000\
\\003\000\020\000\000\000\
\\003\000\021\000\000\000\
\\003\000\022\000\000\000\
\\003\000\023\000\000\000\
\\002\000\024\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\030\000\008\000\029\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\031\000\000\000\
\\005\000\016\000\006\000\032\000\007\000\014\000\000\000\
\\003\000\034\000\004\000\033\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\041\000\000\000\
\\000\000\
\\003\000\030\000\008\000\042\000\000\000\
\\003\000\043\000\000\000\
\\000\000\
\\003\000\034\000\004\000\044\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 46
val numrules = 25
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit | ID of unit ->  (string) | INT of unit ->  (int) | expcomm of unit ->  (Ast.exp list) | dec of unit ->  (Ast.dec) | decs of unit ->  (Ast.dec list) | vardec of unit ->  (Ast.dec) | exps of unit ->  (Ast.exp list) | exp of unit ->  (Ast.exp) | program of unit ->  (Ast.exp list) | init of unit ->  (Ast.exp list)
end
type svalue = MlyValue.svalue
type result = Ast.exp list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "SEMICOLON"
  | (T 2) => "INT"
  | (T 3) => "PLUS"
  | (T 4) => "MINUS"
  | (T 5) => "TIMES"
  | (T 6) => "DIVIDE"
  | (T 7) => "EQ"
  | (T 8) => "ASSIGN"
  | (T 9) => "ID"
  | (T 10) => "LET"
  | (T 11) => "IN"
  | (T 12) => "END"
  | (T 13) => "VAR"
  | (T 14) => "FUNCTION"
  | (T 15) => "COLON"
  | (T 16) => "COMMA"
  | (T 17) => "LPAREN"
  | (T 18) => "RPAREN"
  | (T 19) => "IF"
  | (T 20) => "THEN"
  | (T 21) => "ELSE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.program program1, program1left, program1right)) :: rest671)) => let val  result = MlyValue.init (fn _ => let val  (program as program1) = program1 ()
 in ((*#line 40.22 "tiger.grm"*)program(*#line 240.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, program1left, program1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.program program1, _, program1right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.program (fn _ => let val  (exp as exp1) = exp1 ()
 val  (program as program1) = program1 ()
 in ((*#line 42.34 "tiger.grm"*)exp::program(*#line 246.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 1, ( result, exp1left, program1right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.program (fn _ => ((*#line 43.12 "tiger.grm"*)[](*#line 253.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = INT1 ()
 in ((*#line 45.38 "tiger.grm"*)Ast.INT(INT)(*#line 257.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, INT1left, INT1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 in ((*#line 46.16 "tiger.grm"*)Ast.ID(ID)(*#line 263.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 47.38 "tiger.grm"*)Ast.BINOP(exp1,Ast.PLUS,exp2)(*#line 269.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 48.22 "tiger.grm"*)Ast.BINOP(exp1,Ast.MINUS,exp2)(*#line 276.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 49.22 "tiger.grm"*)Ast.BINOP(exp1,Ast.TIMES,exp2)(*#line 283.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 50.23 "tiger.grm"*)Ast.BINOP(exp1,Ast.DIVIDE,exp2)(*#line 290.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 51.22 "tiger.grm"*)Ast.BINOP(exp1,Ast.EQUALS,exp2)(*#line 297.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 10, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.exps exps1, _, _)) :: _ :: ( _, ( MlyValue.decs decs1, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (decs as decs1) = decs1 ()
 val  (exps as exps1) = exps1 ()
 in ((*#line 52.28 "tiger.grm"*)Ast.LET(decs, exps)(*#line 304.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, LET1left, END1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expcomm expcomm1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 val  (expcomm as expcomm1) = expcomm1 ()
 in ((*#line 53.32 "tiger.grm"*)Ast.FUNC(ID, expcomm)(*#line 311.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 54.25 "tiger.grm"*)Ast.IF(exp1, exp2)(*#line 318.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, IF1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ((*#line 55.32 "tiger.grm"*)Ast.IFELSE(exp1, exp2, exp3)(*#line 325.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, IF1left, exp3right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.expcomm expcomm1, _, expcomm1right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.expcomm (fn _ => let val  (exp as exp1) = exp1 ()
 val  (expcomm as expcomm1) = expcomm1 ()
 in ((*#line 57.33 "tiger.grm"*)exp :: expcomm(*#line 333.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 7, ( result, exp1left, expcomm1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)) => let val  result = MlyValue.expcomm (fn _ => let val  (exp as exp1) = exp1 ()
 in ((*#line 58.16 "tiger.grm"*)exp :: [](*#line 340.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 7, ( result, exp1left, exp1right), rest671)
end
|  ( 16, ( rest671)) => let val  result = MlyValue.expcomm (fn _ => ((*#line 59.13 "tiger.grm"*)[](*#line 346.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exps exps1, _, exps1right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exps (fn _ => let val  (exp as exp1) = exp1 ()
 val  (exps as exps1) = exps1 ()
 in ((*#line 61.31 "tiger.grm"*)exp :: exps(*#line 350.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, exp1left, exps1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)) => let val  result = MlyValue.exps (fn _ => let val  (exp as exp1) = exp1 ()
 in ((*#line 62.16 "tiger.grm"*)exp :: [](*#line 357.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, exp1left, exp1right), rest671)
end
|  ( 19, ( rest671)) => let val  result = MlyValue.exps (fn _ => ((*#line 63.13 "tiger.grm"*)[](*#line 363.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 20, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: _ :: ( _, ( MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result = MlyValue.decs (fn _ => let val  (dec as dec1) = dec1 ()
 val  (decs as decs1) = decs1 ()
 in ((*#line 65.31 "tiger.grm"*)dec :: decs(*#line 367.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 5, ( result, dec1left, decs1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.dec dec1, dec1left, dec1right)) :: rest671)) => let val  result = MlyValue.decs (fn _ => let val  (dec as dec1) = dec1 ()
 in ((*#line 66.16 "tiger.grm"*)dec :: [](*#line 374.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 5, ( result, dec1left, dec1right), rest671)
end
|  ( 22, ( rest671)) => let val  result = MlyValue.decs (fn _ => ((*#line 67.14 "tiger.grm"*)[](*#line 380.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 23, ( ( _, ( MlyValue.vardec vardec1, vardec1left, vardec1right)) :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (vardec as vardec1) = vardec1 ()
 in ((*#line 69.22 "tiger.grm"*)vardec(*#line 384.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 6, ( result, vardec1left, vardec1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.vardec (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 71.33 "tiger.grm"*)Ast.VARDEC(ID, exp)(*#line 390.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, VAR1left, exp1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.init x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.INT (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.ID (fn () => i),p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
end
end
