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
        (* print (Atom.toString (c_sym) ^ Atom.toString (c_tok) ^ "\n") *)
        lpt := LLONE_TBL_MAP.insert (!lpt, (c_sym, c_tok), RHSSet.empty)
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

fun print_prod_helper c_sym c_tok = 
    (
        let 
            val prods = ref (RHSSet.listItems ( LLONE_TBL_MAP.lookup(!lpt, (c_sym, c_tok)) handle NotFound => RHSSet.empty ))
        in
            print (Atom.toString (c_tok) ^ ": "); 
            while (List.null(!prods) = false) do (
                let
                    val rhs = ref (List.hd(!prods))
                in
                    if (null (!rhs)) then (
                        print ("ε ")
                    ) else (
                        printAtomList (!rhs)
                    )
                end;
                prods := tl (!prods);
                if (null (!prods)) then (
                        
                ) else (
                    print ("| ")
                )
            );
            print ("\n")
        end
    );

fun traverse_tok_print function c_sym = 
    (
        let 
            val tok = ref (AtomSet.listItems(#tokens Grm))
        in 
            print (Atom.toString c_sym ^ ": \n");
            while (List.null (!tok) = false) do (
                let 
                    val x = hd(!tok)
                in 
                    function c_sym x;
                    tok := tl(!tok)
                end
            );
            print ("\n\n")
        end
    );

fun init_lpt () = 
    (
        traverse_sym (traverse_tok (insert_empty_lpt))
    );

fun add_production (token :: token_list, x, rhs_list) = 
    (
        let 
            val c = LLONE_TBL_MAP.remove (!lpt, (x, token)) handle NotFound => (print ("Error in add_production\n"); (!lpt, RHSSet.empty))
            val (mp, el) = (ref (#1 c), ref (#2 c))
        in 
            lpt := !mp;
            if (RHSSet.member (!el, rhs_list)) then (

            ) else (
                el := RHSSet.add (!el, rhs_list)
            );
            lpt := LLONE_TBL_MAP.insert (!lpt, (x, token), !el);
            add_production (token_list, x, rhs_list)
        end
    )
|   add_production ([], x, rhs_list) = 
    (

    );

fun add_to_table (token_set, x, rhs) = 
    (
        add_production (AtomSet.listItems(token_set), x, rhs)
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
                    ) else (
                        add_to_table (AtomSet.singleton (yi), x, !rhs)
                    );
                    still_nullable := is_null(yi)
                end;
                i := !i + 1
            );
            if (is_nullable (!rhs)) then (
                add_to_table (AtomMap.lookup (!FOLLOW, x), x, !rhs)
            ) else ()
        end
    );

fun print_table () = 
    (
        print ("\n=== Printing LLONE TABLE ===\n");
        traverse_sym (traverse_tok_print (print_prod_helper));
        print ("=== ======== ===\n")
    );

init_lpt ();

calculate calculate_table;

print_table ()

