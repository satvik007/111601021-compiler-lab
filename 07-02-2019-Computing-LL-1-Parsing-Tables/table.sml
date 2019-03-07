use "first.sml";

structure LLONE_KEY : ORD_KEY = struct
    type ord_key = (Atom.atom * Atom.atom)
    fun compare ((a : ord_key , b : ord_key))   = case Atom.lexCompare ((#1 a), (#1 b)) of 
                                                EQUAL => ( Atom.lexCompare ((#2 a), (#2 b)) )
                                            |   LESS => LESS 
                                            |   GREATER => GREATER
end;

structure LLONE_TBL_MAP = RedBlackMapFn (LLONE_KEY);

type lloneParsingTable = Productions LLONE_TBL_MAP.map;

val lpt : lloneParsingTable ref = ref LLONE_TBL_MAP.empty;

fun insert_empty_lpt c_sym c_tok = 
    (
        print (Atom.toString (c_sym) ^ Atom.toString (c_tok) ^ "\n")
    );

fun traverse_tok function c_sym = 
    (
        let 
            val tok = ref (AtomSet.listItems(#tokens Grm))
        in 
            while (List.null (!tok) = false) do (
                let 
                    val x = hd(!tok)
                in 
                    function c_sym x;
                    tok := tl(!tok)
                end
            )
        end
    );

fun init_lpt () = 
    (
        traverse_sym (traverse_tok (insert_empty_lpt))
    );

fun add_production (token :: token_list, x, rhs_set) = 
    (
        let 
            val el = ref (LLONE_TBL_MAP.lookup (!lpt, (x, token)) handle NotFound => (print ("Error in add_production\n"); RHSSet.empty))
        in 
            add_production (token_list, x, rhs_set)
        end
    )
|   add_production ([], x, rhs_set) = 
    (

    );

fun add_to_table (token_set, x, rhs) = 
    (
        add_production (AtomSet.listItems(token_set), x, AtomSet.fromList(rhs))
    );

fun calculate_table x rhs = 
    (
        let 
            val i = ref 0
            val still_nullable = ref true
            val k = List.length(!rhs)
        in 
            while (!i < k andalso !still_nullable) do (
                let 
                    val yi = List.nth(!rhs, !i)
                in
                    if (AtomSet.member (!sym, yi)) then (
                        add_to_table (AtomMap.lookup (!FIRST, yi), x, !rhs)
                    ) else ();
                    still_nullable := is_null(yi)
                end;
                i := !i + 1
            );
            if (is_nullable (!rhs)) then (
                add_to_table (AtomMap.lookup (!FOLLOW, x), x, !rhs)
            ) else ()
        end
    );

calculate calculate_table
