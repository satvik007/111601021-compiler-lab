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
structure A = Absyn

fun addToTypeDec(newType, (A.TypeDec a)::l) = A.TypeDec(newType::a)::l
  | addToTypeDec(newType, a::l) = A.TypeDec([])::[]
  | addToTypeDec(newType, []) = A.TypeDec([newType])::[];

fun addToFunctionDec(newType, (A.FunctionDec a)::l) = A.FunctionDec(newType::a)::l
  | addToFunctionDec(newType, a::l) = A.FunctionDec([])::[]
  | addToFunctionDec(newType, []) = A.FunctionDec([newType])::[];
    

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\169\000\005\000\169\000\007\000\169\000\009\000\169\000\
\\011\000\169\000\013\000\169\000\015\000\030\000\016\000\029\000\
\\018\000\028\000\019\000\027\000\026\000\169\000\027\000\169\000\
\\031\000\169\000\032\000\169\000\035\000\169\000\036\000\169\000\
\\038\000\169\000\039\000\169\000\043\000\169\000\044\000\169\000\
\\045\000\169\000\000\000\
\\001\000\001\000\170\000\005\000\170\000\007\000\170\000\009\000\170\000\
\\011\000\170\000\013\000\170\000\015\000\030\000\016\000\029\000\
\\018\000\028\000\019\000\027\000\026\000\170\000\027\000\170\000\
\\031\000\170\000\032\000\170\000\035\000\170\000\036\000\170\000\
\\038\000\170\000\039\000\170\000\043\000\170\000\044\000\170\000\
\\045\000\170\000\000\000\
\\001\000\001\000\171\000\005\000\171\000\007\000\171\000\009\000\171\000\
\\011\000\171\000\013\000\171\000\015\000\030\000\016\000\029\000\
\\018\000\028\000\019\000\027\000\026\000\171\000\027\000\171\000\
\\031\000\171\000\032\000\171\000\035\000\171\000\036\000\171\000\
\\038\000\171\000\039\000\171\000\043\000\171\000\044\000\171\000\
\\045\000\171\000\000\000\
\\001\000\001\000\172\000\005\000\172\000\007\000\172\000\009\000\172\000\
\\011\000\172\000\013\000\172\000\015\000\030\000\016\000\029\000\
\\018\000\028\000\019\000\027\000\026\000\172\000\027\000\172\000\
\\031\000\172\000\032\000\172\000\035\000\172\000\036\000\172\000\
\\038\000\172\000\039\000\172\000\043\000\172\000\044\000\172\000\
\\045\000\172\000\000\000\
\\001\000\001\000\173\000\005\000\173\000\007\000\173\000\009\000\173\000\
\\011\000\173\000\013\000\173\000\015\000\030\000\016\000\029\000\
\\018\000\028\000\019\000\027\000\026\000\173\000\027\000\173\000\
\\031\000\173\000\032\000\173\000\035\000\173\000\036\000\173\000\
\\038\000\173\000\039\000\173\000\043\000\173\000\044\000\173\000\
\\045\000\173\000\000\000\
\\001\000\001\000\174\000\005\000\174\000\007\000\174\000\009\000\174\000\
\\011\000\174\000\013\000\174\000\015\000\030\000\016\000\029\000\
\\018\000\028\000\019\000\027\000\026\000\174\000\027\000\174\000\
\\031\000\174\000\032\000\174\000\035\000\174\000\036\000\174\000\
\\038\000\174\000\039\000\174\000\043\000\174\000\044\000\174\000\
\\045\000\174\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\009\000\043\000\016\000\011\000\030\000\010\000\033\000\009\000\
\\034\000\008\000\037\000\007\000\041\000\006\000\042\000\005\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\016\000\011\000\030\000\010\000\033\000\009\000\034\000\008\000\
\\037\000\007\000\041\000\006\000\042\000\005\000\000\000\
\\001\000\002\000\038\000\000\000\
\\001\000\002\000\048\000\000\000\
\\001\000\002\000\070\000\000\000\
\\001\000\002\000\071\000\000\000\
\\001\000\002\000\072\000\000\000\
\\001\000\002\000\110\000\012\000\109\000\029\000\108\000\000\000\
\\001\000\002\000\112\000\000\000\
\\001\000\002\000\136\000\000\000\
\\001\000\002\000\141\000\000\000\
\\001\000\002\000\142\000\000\000\
\\001\000\002\000\146\000\000\000\
\\001\000\002\000\151\000\000\000\
\\001\000\005\000\127\000\009\000\126\000\000\000\
\\001\000\005\000\127\000\013\000\137\000\000\000\
\\001\000\006\000\092\000\028\000\091\000\000\000\
\\001\000\006\000\128\000\000\000\
\\001\000\006\000\140\000\020\000\139\000\000\000\
\\001\000\006\000\147\000\000\000\
\\001\000\007\000\077\000\009\000\076\000\015\000\030\000\016\000\029\000\
\\018\000\028\000\019\000\027\000\020\000\026\000\021\000\025\000\
\\022\000\024\000\023\000\023\000\024\000\022\000\025\000\021\000\
\\026\000\020\000\027\000\019\000\000\000\
\\001\000\008\000\093\000\000\000\
\\001\000\009\000\101\000\000\000\
\\001\000\009\000\131\000\000\000\
\\001\000\011\000\083\000\015\000\030\000\016\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\027\000\019\000\000\000\
\\001\000\011\000\100\000\015\000\030\000\016\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\027\000\019\000\000\000\
\\001\000\013\000\098\000\000\000\
\\001\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\
\\031\000\075\000\000\000\
\\001\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\
\\035\000\115\000\000\000\
\\001\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\
\\036\000\074\000\000\000\
\\001\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\
\\036\000\143\000\000\000\
\\001\000\020\000\090\000\000\000\
\\001\000\020\000\099\000\000\000\
\\001\000\020\000\150\000\000\000\
\\001\000\028\000\073\000\000\000\
\\001\000\028\000\125\000\000\000\
\\001\000\038\000\069\000\000\000\
\\001\000\039\000\104\000\000\000\
\\001\000\040\000\123\000\000\000\
\\155\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\156\000\010\000\018\000\014\000\017\000\028\000\016\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\159\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\018\000\028\000\019\000\027\000\000\000\
\\166\000\018\000\028\000\019\000\027\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\175\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\000\000\
\\176\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\177\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\178\000\000\000\
\\179\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\180\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\181\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\
\\032\000\116\000\000\000\
\\182\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\183\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\005\000\132\000\015\000\030\000\016\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\027\000\019\000\000\000\
\\187\000\000\000\
\\188\000\002\000\079\000\000\000\
\\189\000\000\000\
\\190\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\016\000\011\000\030\000\010\000\033\000\009\000\034\000\008\000\
\\037\000\007\000\041\000\006\000\042\000\005\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\043\000\037\000\044\000\036\000\045\000\035\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\043\000\037\000\044\000\036\000\000\000\
\\198\000\000\000\
\\199\000\000\000\
\\200\000\044\000\036\000\045\000\035\000\000\000\
\\201\000\045\000\035\000\000\000\
\\202\000\000\000\
\\203\000\000\000\
\\204\000\000\000\
\\205\000\000\000\
\\206\000\002\000\114\000\000\000\
\\207\000\000\000\
\\208\000\000\000\
\\209\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\210\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\211\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\
\\043\000\037\000\000\000\
\\212\000\015\000\030\000\016\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\021\000\025\000\022\000\024\000\023\000\023\000\
\\024\000\022\000\025\000\021\000\026\000\020\000\027\000\019\000\
\\043\000\037\000\000\000\
\\213\000\000\000\
\\214\000\000\000\
\\215\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\016\000\011\000\030\000\010\000\033\000\009\000\034\000\008\000\
\\037\000\007\000\041\000\006\000\042\000\005\000\000\000\
\\216\000\000\000\
\\217\000\000\000\
\\218\000\005\000\103\000\015\000\030\000\016\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\027\000\019\000\000\000\
\\219\000\000\000\
\\220\000\007\000\106\000\015\000\030\000\016\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\027\000\019\000\000\000\
\\221\000\008\000\046\000\010\000\045\000\012\000\044\000\000\000\
\\222\000\000\000\
\\223\000\040\000\119\000\000\000\
\\224\000\000\000\
\"
val actionRowNumbers =
"\008\000\047\000\046\000\048\000\
\\069\000\079\000\009\000\008\000\
\\008\000\008\000\007\000\053\000\
\\052\000\106\000\008\000\010\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\085\000\079\000\082\000\
\\043\000\011\000\012\000\013\000\
\\041\000\036\000\034\000\055\000\
\\027\000\050\000\073\000\008\000\
\\100\000\062\000\107\000\031\000\
\\061\000\060\000\006\000\005\000\
\\004\000\003\000\002\000\001\000\
\\059\000\058\000\057\000\056\000\
\\078\000\079\000\082\000\077\000\
\\076\000\085\000\079\000\075\000\
\\038\000\023\000\028\000\008\000\
\\008\000\008\000\070\000\008\000\
\\033\000\039\000\032\000\029\000\
\\103\000\109\000\083\000\084\000\
\\081\000\080\000\044\000\105\000\
\\014\000\008\000\015\000\091\000\
\\035\000\067\000\066\000\105\000\
\\063\000\008\000\108\000\054\000\
\\101\000\008\000\051\000\074\000\
\\008\000\086\000\045\000\091\000\
\\088\000\094\000\042\000\021\000\
\\024\000\008\000\008\000\030\000\
\\071\000\008\000\103\000\105\000\
\\087\000\016\000\022\000\008\000\
\\025\000\017\000\018\000\037\000\
\\065\000\049\000\073\000\064\000\
\\102\000\104\000\090\000\089\000\
\\095\000\008\000\019\000\026\000\
\\093\000\008\000\072\000\096\000\
\\040\000\020\000\068\000\098\000\
\\008\000\092\000\097\000\099\000\
\\000\000"
val gotoT =
"\
\\001\000\152\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\032\000\009\000\031\000\012\000\030\000\013\000\029\000\000\000\
\\000\000\
\\002\000\037\000\003\000\001\000\000\000\
\\002\000\038\000\003\000\001\000\000\000\
\\002\000\039\000\003\000\001\000\000\000\
\\002\000\040\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\045\000\003\000\001\000\000\000\
\\000\000\
\\002\000\047\000\003\000\001\000\000\000\
\\002\000\048\000\003\000\001\000\000\000\
\\002\000\049\000\003\000\001\000\000\000\
\\002\000\050\000\003\000\001\000\000\000\
\\002\000\051\000\003\000\001\000\000\000\
\\002\000\052\000\003\000\001\000\000\000\
\\002\000\053\000\003\000\001\000\000\000\
\\002\000\054\000\003\000\001\000\000\000\
\\002\000\055\000\003\000\001\000\000\000\
\\002\000\056\000\003\000\001\000\000\000\
\\002\000\057\000\003\000\001\000\000\000\
\\002\000\058\000\003\000\001\000\000\000\
\\002\000\059\000\003\000\001\000\000\000\
\\009\000\062\000\012\000\061\000\018\000\060\000\000\000\
\\007\000\063\000\009\000\031\000\012\000\030\000\013\000\029\000\000\000\
\\012\000\066\000\013\000\065\000\017\000\064\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\076\000\000\000\
\\002\000\078\000\003\000\001\000\000\000\
\\002\000\080\000\003\000\001\000\005\000\079\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\082\000\009\000\031\000\012\000\030\000\013\000\029\000\000\000\
\\012\000\066\000\013\000\065\000\017\000\083\000\000\000\
\\000\000\
\\000\000\
\\009\000\062\000\012\000\061\000\018\000\084\000\000\000\
\\007\000\085\000\009\000\031\000\012\000\030\000\013\000\029\000\000\000\
\\002\000\087\000\003\000\001\000\014\000\086\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\092\000\003\000\001\000\000\000\
\\002\000\093\000\003\000\001\000\000\000\
\\002\000\094\000\003\000\001\000\000\000\
\\000\000\
\\002\000\095\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\100\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\103\000\000\000\
\\010\000\105\000\000\000\
\\002\000\109\000\003\000\001\000\000\000\
\\000\000\
\\011\000\111\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\115\000\000\000\
\\000\000\
\\002\000\116\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\118\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\119\000\003\000\001\000\000\000\
\\009\000\120\000\000\000\
\\000\000\
\\011\000\122\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\127\000\003\000\001\000\000\000\
\\002\000\128\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\131\000\003\000\001\000\000\000\
\\006\000\132\000\000\000\
\\004\000\133\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\136\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\142\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\143\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\146\000\003\000\001\000\000\000\
\\000\000\
\\013\000\147\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\150\000\003\000\001\000\000\000\
\\000\000\
\\013\000\151\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 153
val numrules = 70
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
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
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
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string) | nonfundec of unit ->  (A.dec list)
 | nontydec of unit ->  (A.dec list)
 | recordtail of unit ->  ( ( A.symbol * A.exp * pos )  list)
 | recordbody of unit ->  ( ( A.symbol * A.exp * pos )  list)
 | letbody of unit ->  (A.exp) | fundec of unit ->  (A.dec list)
 | vardec of unit ->  (A.dec) | tyfields of unit ->  (A.field list)
 | ty of unit ->  (A.ty) | tydec of unit ->  (A.dec list)
 | dec of unit ->  (A.dec) | decs of unit ->  (A.dec list)
 | funargtail of unit ->  (A.exp list)
 | funarg of unit ->  (A.exp list)
 | exptail of unit ->  ( ( A.exp * pos )  list)
 | lvalue of unit ->  (A.var) | exp of unit ->  (A.exp)
 | program of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 32) => true | (T 33) => true | (T 34) => true | (T 40) => true
 | (T 36) => true | (T 37) => true | (T 38) => true | (T 42) => true
 | (T 43) => true | (T 44) => true | (T 28) => true | (T 29) => true
 | (T 30) => true | (T 31) => true | (T 35) => true | (T 39) => true
 | (T 41) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 31))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "UMINUS"
  | (T 17) => "TIMES"
  | (T 18) => "DIVIDE"
  | (T 19) => "EQ"
  | (T 20) => "NEQ"
  | (T 21) => "LT"
  | (T 22) => "LE"
  | (T 23) => "GT"
  | (T 24) => "GE"
  | (T 25) => "AND"
  | (T 26) => "OR"
  | (T 27) => "ASSIGN"
  | (T 28) => "ARRAY"
  | (T 29) => "IF"
  | (T 30) => "THEN"
  | (T 31) => "ELSE"
  | (T 32) => "WHILE"
  | (T 33) => "FOR"
  | (T 34) => "TO"
  | (T 35) => "DO"
  | (T 36) => "LET"
  | (T 37) => "IN"
  | (T 38) => "END"
  | (T 39) => "OF"
  | (T 40) => "BREAK"
  | (T 41) => "NIL"
  | (T 42) => "FUNCTION"
  | (T 43) => "VAR"
  | (T 44) => "TYPE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.program (fn _ => let val  (exp as exp1
) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.lvalue lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
lvalue as lvalue1) = lvalue1 ()
 in (A.VarExp(lvalue))
end)
 in ( LrTable.NT 1, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 2, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (A.NilExp))
 in ( LrTable.NT 1, ( result, NIL1left, NIL1right), rest671)
end
|  ( 3, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exptail 
exptail1, _, _)) :: ( _, ( MlyValue.exp exp2, exp2left, _)) :: _ :: (
 _, ( MlyValue.exp exp1, exp1left, _)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  exp2 = exp2 ()
 val  (exptail as exptail1) = exptail1 ()
 in (A.SeqExp((exp1, exp1left)::(exp2, exp2left)::exptail))
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 4, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _)) ::
 rest671)) => let val  result = MlyValue.exp (fn _ => (A.SeqExp([])))
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 5, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.letbody letbody1
, _, _)) :: _ :: ( _, ( MlyValue.decs decs1, _, _)) :: ( _, ( _, (
LETleft as LET1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (decs as decs1) = decs1 ()
 val  (letbody as letbody1) = letbody1 ()
 in (A.LetExp({decs=decs, body=letbody, pos=LETleft}))
end)
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (A.IntExp(INT))
end)
 in ( LrTable.NT 1, ( result, INT1left, INT1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left),
 STRING1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp(STRING, STRINGleft))
end)
 in ( LrTable.NT 1, ( result, STRING1left, STRING1right), rest671)
end
|  ( 8, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.funarg 
funarg1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _
)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
ID as ID1) = ID1 ()
 val  (funarg as funarg1) = funarg1 ()
 in (A.CallExp({func=Symbol.symbol ID, args=funarg, pos=IDleft}))
end)
 in ( LrTable.NT 1, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, (
MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (
A.OpExp({left=A.IntExp(0), oper=A.MinusOp, right=exp, pos=MINUSleft}))

end)
 in ( LrTable.NT 1, ( result, MINUS1left, exp1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.PlusOp, right=exp2, pos=exp1left}))

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.MinusOp, right=exp2, pos=exp1left}))

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.TimesOp, right=exp2, pos=exp1left}))

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.DivideOp, right=exp2, pos=exp1left}))

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.EqOp, right=exp2, pos=exp1left}))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.NeqOp, right=exp2, pos=exp1left}))
end
)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.LtOp, right=exp2, pos=exp1left}))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.LeOp, right=exp2, pos=exp1left}))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.GtOp, right=exp2, pos=exp1left}))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.GeOp, right=exp2, pos=exp1left}))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp({test=exp1, then'=exp2, else'=SOME(A.IntExp(0)), pos=exp1left})
)
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp({test=exp1, then'=A.IntExp(1), else'=SOME exp2, pos=exp1left})
)
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.lvalue lvalue1, (lvalueleft as lvalue1left), _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  (lvalue as 
lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in (A.AssignExp({var=lvalue, exp=exp, pos=lvalueleft}))
end)
 in ( LrTable.NT 1, ( result, lvalue1left, exp1right), rest671)
end
|  ( 23, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.recordbody 
recordbody1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left
), _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (recordbody as recordbody1) = recordbody1 ()
 in (
A.RecordExp({fields=recordbody, typ=Symbol.symbol ID, pos=IDleft}))

end)
 in ( LrTable.NT 1, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: _ :: ( _, 
( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft
 as ID1left), _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.ArrayExp({typ=Symbol.symbol ID, size=exp1, init=exp2, pos=IDleft}))

end)
 in ( LrTable.NT 1, ( result, ID1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (A.IfExp({test=exp1, then'=exp2, else'=SOME exp3, pos=IFleft}))

end)
 in ( LrTable.NT 1, ( result, IF1left, exp3right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = 
exp1 ()
 val  exp2 = exp2 ()
 in (A.IfExp({test=exp1, then'=exp2, else'=NONE, pos=IFleft}))
end)
 in ( LrTable.NT 1, ( result, IF1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  exp2 = exp2 ()
 in (A.WhileExp({test=exp1, body=exp2, pos=WHILEleft}))
end)
 in ( LrTable.NT 1, ( result, WHILE1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as FOR1left
), _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.ForExp({var=Symbol.symbol ID, escape=(ref true), lo=exp1, hi=exp2, body=exp3, pos=FORleft})
)
end)
 in ( LrTable.NT 1, ( result, FOR1left, exp3right), rest671)
end
|  ( 29, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => (
A.BreakExp(BREAKleft)))
 in ( LrTable.NT 1, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.recordbody (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ([(Symbol.symbol ID, exp, defaultPos)])
end)
 in ( LrTable.NT 14, ( result, ID1left, exp1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.recordbody recordbody1, _, recordbody1right
)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID
 ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.recordbody (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (recordbody as recordbody1) = recordbody1 ()
 in ((Symbol.symbol ID, exp, defaultPos) :: recordbody)
end)
 in ( LrTable.NT 14, ( result, ID1left, recordbody1right), rest671)

end
|  ( 33, ( rest671)) => let val  result = MlyValue.recordbody (fn _ =>
 ([]))
 in ( LrTable.NT 14, ( result, defaultPos, defaultPos), rest671)
end
|  ( 34, ( ( _, ( MlyValue.exptail exptail1, _, exptail1right)) :: ( _
, ( MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let
 val  result = MlyValue.letbody (fn _ => let val  (exp as exp1) = exp1
 ()
 val  (exptail as exptail1) = exptail1 ()
 in (A.SeqExp((exp, expleft)::exptail))
end)
 in ( LrTable.NT 13, ( result, exp1left, exptail1right), rest671)
end
|  ( 35, ( rest671)) => let val  result = MlyValue.letbody (fn _ => (
A.NilExp))
 in ( LrTable.NT 13, ( result, defaultPos, defaultPos), rest671)
end
|  ( 36, ( ( _, ( MlyValue.nontydec nontydec1, _, nontydec1right)) :: 
( _, ( MlyValue.tydec tydec1, tydec1left, _)) :: rest671)) => let val 
 result = MlyValue.decs (fn _ => let val  (tydec as tydec1) = tydec1
 ()
 val  (nontydec as nontydec1) = nontydec1 ()
 in (tydec @ nontydec)
end)
 in ( LrTable.NT 6, ( result, tydec1left, nontydec1right), rest671)

end
|  ( 37, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) => let val  
result = MlyValue.decs (fn _ => let val  (vardec as vardec1) = vardec1
 ()
 val  (decs as decs1) = decs1 ()
 in (vardec :: decs)
end)
 in ( LrTable.NT 6, ( result, vardec1left, decs1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.nonfundec nonfundec1, _, nonfundec1right))
 :: ( _, ( MlyValue.fundec fundec1, fundec1left, _)) :: rest671)) =>
 let val  result = MlyValue.decs (fn _ => let val  (fundec as fundec1)
 = fundec1 ()
 val  (nonfundec as nonfundec1) = nonfundec1 ()
 in (fundec @ nonfundec)
end)
 in ( LrTable.NT 6, ( result, fundec1left, nonfundec1right), rest671)

end
|  ( 39, ( rest671)) => let val  result = MlyValue.decs (fn _ => ([]))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 40, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) => let val  
result = MlyValue.nontydec (fn _ => let val  (vardec as vardec1) = 
vardec1 ()
 val  (decs as decs1) = decs1 ()
 in (vardec :: decs)
end)
 in ( LrTable.NT 16, ( result, vardec1left, decs1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.nonfundec nonfundec1, _, nonfundec1right))
 :: ( _, ( MlyValue.fundec fundec1, fundec1left, _)) :: rest671)) =>
 let val  result = MlyValue.nontydec (fn _ => let val  (fundec as 
fundec1) = fundec1 ()
 val  (nonfundec as nonfundec1) = nonfundec1 ()
 in (fundec @ nonfundec)
end)
 in ( LrTable.NT 16, ( result, fundec1left, nonfundec1right), rest671)

end
|  ( 42, ( rest671)) => let val  result = MlyValue.nontydec (fn _ => (
[]))
 in ( LrTable.NT 16, ( result, defaultPos, defaultPos), rest671)
end
|  ( 43, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) => let val  
result = MlyValue.nonfundec (fn _ => let val  (vardec as vardec1) = 
vardec1 ()
 val  (decs as decs1) = decs1 ()
 in (vardec :: decs)
end)
 in ( LrTable.NT 17, ( result, vardec1left, decs1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.nontydec nontydec1, _, nontydec1right)) :: 
( _, ( MlyValue.tydec tydec1, tydec1left, _)) :: rest671)) => let val 
 result = MlyValue.nonfundec (fn _ => let val  (tydec as tydec1) = 
tydec1 ()
 val  (nontydec as nontydec1) = nontydec1 ()
 in (tydec @ nontydec)
end)
 in ( LrTable.NT 17, ( result, tydec1left, nontydec1right), rest671)

end
|  ( 45, ( rest671)) => let val  result = MlyValue.nonfundec (fn _ =>
 ([]))
 in ( LrTable.NT 17, ( result, defaultPos, defaultPos), rest671)
end
|  ( 46, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (TYPEleft as TYPE1left), _)) :: 
rest671)) => let val  result = MlyValue.tydec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 in (A.TypeDec([{name=Symbol.symbol ID, ty=ty, pos=TYPEleft}])::[])

end)
 in ( LrTable.NT 8, ( result, TYPE1left, ty1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.tydec tydec1, _, tydec1right)) :: ( _, ( 
MlyValue.ty ty1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _,
 ( _, (TYPEleft as TYPE1left), _)) :: rest671)) => let val  result = 
MlyValue.tydec (fn _ => let val  (ID as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 val  (tydec as tydec1) = tydec1 ()
 in (addToTypeDec({name=Symbol.symbol ID, ty=ty, pos=TYPEleft}, tydec)
)
end)
 in ( LrTable.NT 8, ( result, TYPE1left, tydec1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.ty (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (A.NameTy(Symbol.symbol ID, IDleft))
end)
 in ( LrTable.NT 9, ( result, ID1left, ID1right), rest671)
end
|  ( 49, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ty (fn _ => let val  (tyfields as tyfields1) =
 tyfields1 ()
 in (A.RecordTy(tyfields))
end)
 in ( LrTable.NT 9, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, (
ARRAYleft as ARRAY1left), _)) :: rest671)) => let val  result = 
MlyValue.ty (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.ArrayTy(Symbol.symbol ID, ARRAYleft))
end)
 in ( LrTable.NT 9, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 51, ( rest671)) => let val  result = MlyValue.tyfields (fn _ => (
[]))
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: _ :: ( _, ( MlyValue.tyfields 
tyfields1, tyfields1left, _)) :: rest671)) => let val  result = 
MlyValue.tyfields (fn _ => let val  (tyfields as tyfields1) = 
tyfields1 ()
 val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in (
tyfields @ [{name=Symbol.symbol ID1, escape=(ref true), typ=Symbol.symbol ID2, pos=ID1left}]
)
end)
 in ( LrTable.NT 10, ( result, tyfields1left, ID2right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.tyfields (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in (
[{name=Symbol.symbol ID1, escape=(ref true), typ=Symbol.symbol ID2, pos=ID1left}]
)
end)
 in ( LrTable.NT 10, ( result, ID1left, ID2right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (VARleft as VAR1left), _)) :: 
rest671)) => let val  result = MlyValue.vardec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec({name=Symbol.symbol ID, escape=(ref true), typ=NONE, init=exp, pos=VARleft})
)
end)
 in ( LrTable.NT 11, ( result, VAR1left, exp1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( _, (VARleft as VAR1left), _)) :: rest671)) => let val  
result = MlyValue.vardec (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec({name=Symbol.symbol ID1, escape=(ref true), typ=SOME (Symbol.symbol ID2, ID2left), init=exp, pos=VARleft})
)
end)
 in ( LrTable.NT 11, ( result, VAR1left, exp1right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: _ :: ( _, 
( MlyValue.tyfields tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1,
 _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left), _)) :: rest671))
 => let val  result = MlyValue.fundec (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.FunctionDec([
                                                                        {name=Symbol.symbol ID, params=tyfields,
                                                                        result=NONE, body=exp, pos=FUNCTIONleft}])::[]
)
end)
 in ( LrTable.NT 12, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: _ :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _,
 (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  result
 = MlyValue.fundec (fn _ => let val  ID1 = ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
A.FunctionDec([
                                                                        {name=Symbol.symbol ID1, params=tyfields,
                                                                        result=SOME(Symbol.symbol ID2, ID2left), body=exp,
                                                                        pos=FUNCTIONleft}])::[]
)
end)
 in ( LrTable.NT 12, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.fundec fundec1, _, fundec1right)) :: ( _, (
 MlyValue.exp exp1, _, _)) :: _ :: _ :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _,
 (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  result
 = MlyValue.fundec (fn _ => let val  (ID as ID1) = ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  (exp as exp1) = exp1 ()
 val  (fundec as fundec1) = fundec1 ()
 in (
addToFunctionDec({name=Symbol.symbol ID, params=tyfields,
                                                                        result=NONE, body=exp, pos=FUNCTIONleft}, fundec)
)
end)
 in ( LrTable.NT 12, ( result, FUNCTION1left, fundec1right), rest671)

end
|  ( 59, ( ( _, ( MlyValue.fundec fundec1, _, fundec1right)) :: ( _, (
 MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, ID2left, _)
) :: _ :: _ :: ( _, ( MlyValue.tyfields tyfields1, _, _)) :: _ :: ( _,
 ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left
), _)) :: rest671)) => let val  result = MlyValue.fundec (fn _ => let
 val  ID1 = ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 val  (fundec as fundec1) = fundec1 ()
 in (
addToFunctionDec({name=Symbol.symbol ID1, params=tyfields,
                                                                        result=SOME(Symbol.symbol ID2, ID2left), body=exp,
                                                                        pos=FUNCTIONleft}, fundec)
)
end)
 in ( LrTable.NT 12, ( result, FUNCTION1left, fundec1right), rest671)

end
|  ( 60, ( rest671)) => let val  result = MlyValue.funarg (fn _ => ([]
))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 61, ( ( _, ( MlyValue.funargtail funargtail1, _, funargtail1right
)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val 
 result = MlyValue.funarg (fn _ => let val  (exp as exp1) = exp1 ()
 val  (funargtail as funargtail1) = funargtail1 ()
 in (exp::funargtail)
end)
 in ( LrTable.NT 4, ( result, exp1left, funargtail1right), rest671)

end
|  ( 62, ( ( _, ( MlyValue.funargtail funargtail1, _, funargtail1right
)) :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, COMMA1left, _)) ::
 rest671)) => let val  result = MlyValue.funargtail (fn _ => let val 
 (exp as exp1) = exp1 ()
 val  (funargtail as funargtail1) = funargtail1 ()
 in (exp::funargtail)
end)
 in ( LrTable.NT 5, ( result, COMMA1left, funargtail1right), rest671)

end
|  ( 63, ( rest671)) => let val  result = MlyValue.funargtail (fn _ =>
 ([]))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 64, ( ( _, ( MlyValue.exptail exptail1, _, exptail1right)) :: ( _
, ( MlyValue.exp exp1, expleft, _)) :: ( _, ( _, SEMICOLON1left, _))
 :: rest671)) => let val  result = MlyValue.exptail (fn _ => let val 
 (exp as exp1) = exp1 ()
 val  (exptail as exptail1) = exptail1 ()
 in ((exp, expleft)::exptail)
end)
 in ( LrTable.NT 3, ( result, SEMICOLON1left, exptail1right), rest671)

end
|  ( 65, ( rest671)) => let val  result = MlyValue.exptail (fn _ => (
[]))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 66, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (ID
 as ID1) = ID1 ()
 in (A.SimpleVar(Symbol.symbol ID, IDleft))
end)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 67, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.lvalue lvalue1, (lvalueleft as lvalue1left), _)) :: rest671))
 => let val  result = MlyValue.lvalue (fn _ => let val  (lvalue as 
lvalue1) = lvalue1 ()
 val  (ID as ID1) = ID1 ()
 in (A.FieldVar(lvalue, Symbol.symbol ID, lvalueleft))
end)
 in ( LrTable.NT 2, ( result, lvalue1left, ID1right), rest671)
end
|  ( 68, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: 
rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.SubscriptVar(A.SimpleVar(Symbol.symbol ID, IDleft), exp, IDleft))

end)
 in ( LrTable.NT 2, ( result, ID1left, RBRACK1right), rest671)
end
|  ( 69, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.lvalue lvalue1, (lvalueleft as 
lvalue1left), _)) :: rest671)) => let val  result = MlyValue.lvalue
 (fn _ => let val  (lvalue as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in (A.SubscriptVar(lvalue, exp, lvalueleft))
end)
 in ( LrTable.NT 2, ( result, lvalue1left, RBRACK1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
