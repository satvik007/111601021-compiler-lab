type RHS = Atom.atom list  (* The RHS γ of a rule A -> γ *)

structure RHS_KEY : ORD_KEY = struct
	(* complete this *)
    type ord_key = RHS 
    fun compare ([], []) = EQUAL
        | compare ([], _) = LESS
        | compare (_, []) = GREATER
        | compare (x :: xs, y :: ys) = 
            let 
                val temp = Atom.lexCompare(x, y)
            in
                case temp of
                    EQUAL   => compare(xs , ys)
                |   GREATER => GREATER
                |   LESS    => LESS
            end
end

structure RHSSet = RedBlackSetFn (RHS_KEY)

type Productions = RHSSet.set

type Rules = Productions AtomMap.map


type Grammar    = { symbols : AtomSet.set, tokens : AtomSet.set, rules : Rules }

val sym = ref AtomSet.empty ;
sym := AtomSet.add (!sym , Atom.atom "X") ;
sym := AtomSet.add (!sym , Atom.atom "Y") ;
sym := AtomSet.add (!sym , Atom.atom "Z") ;

val tok = ref AtomSet.empty ;
tok := AtomSet.add (!tok , Atom.atom "a") ;
tok := AtomSet.add (!tok , Atom.atom "c") ;
tok := AtomSet.add (!tok , Atom.atom "d") ;

val X_prod : Productions = RHSSet.fromList ([
        [Atom.atom "Y"],
        [Atom.atom "a"]
    ])

val Y_prod : Productions = RHSSet.fromList ([
        [],
        [Atom.atom "c"]
    ])

val Z_prod : Productions = RHSSet.fromList ([
        [Atom.atom "d"],
        [Atom.atom "X" , Atom.atom "Y", Atom.atom "Z"]
    ])

val rul : Rules ref = ref AtomMap.empty ;
rul := AtomMap.insert (!rul , Atom.atom "X" , X_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "Y" , Y_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "Z" , Z_prod) ;

val Grm : Grammar = {
    symbols = !sym,
    tokens = !tok,
    rules = !rul
}

val FIRST : AtomSet.set AtomMap.map ref = ref AtomMap.empty;
val FOLLOW : AtomSet.set AtomMap.map ref = ref AtomMap.empty;
val NULLABLE : bool AtomMap.map ref = ref AtomMap.empty;

fun printAtomList (x :: xs) = 
    (
        print (Atom.toString (x));
        printAtomList (xs)
    )
| printAtomList ([]) = (print(" "));

val change = ref true;
let 
    val sym = ref (AtomMap.listKeys (#rules Grm))
in
    while (List.null(!sym) = false) do (
        let 
            val x = hd(!sym)
            val prods = ref (RHSSet.listItems ( AtomMap.lookup((#rules Grm) , x ) handle NotFound => RHSSet.empty ))
        in
            print (Atom.toString (x) ^ ": "); 
            while (List.null(!prods) = false) do (
                let
                    val rhs = ref (List.hd(!prods))
                in
                    printAtomList (!rhs)
                end;
                prods := tl (!prods)
            )
        end;
        sym := tl(!sym);
        print("\n")
    )
end;


(* 
while !change = true do (
    change := false
    let 
        val sym = ref (AtomMap.listKeys (#rules Grm))
    in
        while (List.null(!sym) = false) do (
            let 
                val x = hd(!sym)
                val prods = ref (RHSSet.listItems ( AtomMap.lookup((#rules Grm) , x ) handle NotFound => RHSSet.empty ))
            in
                print (Atom.toString (x) ^ ": "); 
                while (List.null(!prods) = false) do (
                    let
                        val rhs = ref (List.hd(!prods))
                    in
                        printAtomList (!rhs)
                    end;
                    prods := tl (!prods)
                )
            end;
            sym := tl(!sym);
            print("\n")
        )
    end;
) *)
