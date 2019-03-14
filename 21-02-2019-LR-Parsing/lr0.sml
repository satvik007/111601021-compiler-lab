use "first.sml";

type Item = { lhs    : Atom.atom
            , before : Atom.atom list
	        , after : Atom.atom list
		    };

(* val aItem = { lhs       = atom "A"
              before = List.map atom ["A", "a"]
              after  = List.map atom ["b", "B"]
            } *)


structure ITEM_KEY : ORD_KEY = struct
	(* complete this *)
    type ord_key = Item 
	fun comp () = GREATER
    fun compare (a : ord_key, b : ord_key) = 
		RHS_KEY.compare ((#lhs a :: #before a) @ (Atom.atom "." :: #after a), 
						(#lhs b :: #before b) @ (Atom.atom "." :: #after b))
end;

structure STATE_KEY : ORD_KEY = struct 
	type ord_key = int 
	val compare = Int.compare
end;

structure LR0_TBL_KEY : ORD_KEY = struct 
	type ord_key = int * Atom.atom 
	fun compare (a : ord_key, b : ord_key) = 
		case Int.compare (#1 a, #1 b) of 
			EQUAL	=> Atom.lexCompare (#2 a, #2 b)
		|	GREATER => GREATER
		|	LESS 	=> LESS 
end;

structure ITEM_SET = RedBlackSetFn (ITEM_KEY);
structure STATE_MAP = RedBlackMapFn (STATE_KEY);
structure LR0_TBL_MAP = RedBlackMapFn (LR0_TBL_KEY);
structure STATE_TR_MAP = RedBlackMapFn (LR0_TBL_KEY);

type item_set = ITEM_SET.set;
type state_map = item_set STATE_MAP.map;
type lr0_table = Productions LR0_TBL_MAP.map;
type tr_map = int STATE_TR_MAP.map;

val sm : state_map ref = ref STATE_MAP.empty;
val lpt : lr0_table ref = ref LR0_TBL_MAP.empty ;
val tr : tr_map ref = ref STATE_TR_MAP.empty ;

fun closure (state_no) = 
	(
		let 
			val old_state = ref STATE_MAP.find (!sm, state_no)
		in 
			
		end 
	);








