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

val OLD_FIRST : AtomSet.set AtomMap.map ref = ref AtomMap.empty;
val OLD_FOLLOW : AtomSet.set AtomMap.map ref = ref AtomMap.empty;
val OLD_NULLABLE : bool AtomMap.map ref = ref AtomMap.empty;


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

fun init () = 
    (
    let 
        val sym = ref (AtomMap.listKeys (#rules Grm))
    in
        while (List.null(!sym) = false) do (
            let 
                val x = hd(!sym)
                val prods = ref (RHSSet.listItems ( AtomMap.lookup((#rules Grm) , x ) handle NotFound => RHSSet.empty ))
            in
                NULLABLE := AtomMap.insert (!NULLABLE, x, false);
                FOLLOW := AtomMap.insert (!FOLLOW, x, AtomSet.empty);
                FIRST := AtomMap.insert (!FIRST, x, AtomSet.empty)
            end;
            sym := tl(!sym)
        )
    end
    );

fun printAtomSet at_set = (printAtomList(AtomSet.listItems(at_set)))

fun printFirstFollowHelper [] =  (print "=== ======== ===\n")
|   printFirstFollowHelper (x::xs) = (let
                                        val (k , v) = x
                                    in
                                        (print ((Atom.toString k) ^ " : " );
                                        printAtomSet(v);
                                        printFirstFollowHelper xs)
                                    end);

fun printNullableHelper [] = (print "=== ======== ===\n")
|   printNullableHelper (x::xs) = (let
                                    val (k , v) = x
                                in
                                    (print ((Atom.toString k) ^ " : " ^ (Bool.toString v) ^ "\n");
                                    printNullableHelper xs)
                                end);

fun printNullable () =  (let
                            val nullable_lst = AtomMap.listItemsi (!NULLABLE)
                        in
                            (print ("\n=== NULLABLE ===\n");
                            printNullableHelper nullable_lst)
                        end);

fun printFollow () = (let
                            val follow_lst = AtomMap.listItemsi (!FOLLOW)
                        in
                            (print ("\n=== FOLLOW ===\n");
                            printFirstFollowHelper follow_lst)
                        end);

fun printFirst () = (let
                            val first_lst = AtomMap.listItemsi (!FIRST)
                        in
                            (print ("\n=== FIRST ===\n");
                            printFirstFollowHelper first_lst)
                        end);
                    
init();

fun is_null (s) = 
    (
        AtomMap.lookup (!NULLABLE, s) handle NotFound => false
    );

fun is_nullable (x :: xs) = 
    (
        is_null (x) andalso is_nullable (xs)
    )
| is_nullable ([]) = 
    (true);

(* is_nullable (List.hd(ref RHSSet.listItems(Y_prod)));    *)

while !change = true do (
    change := false;
    OLD_NULLABLE := !NULLABLE;
    OLD_FIRST := !FIRST;
    OLD_FOLLOW := !FOLLOW;
    let 
        val sym = ref (AtomMap.listKeys (#rules Grm))
    in
        while (List.null(!sym) = false) do (
            let 
                val x = hd(!sym)
                val prods = ref (RHSSet.listItems ( AtomMap.lookup((#rules Grm) , x ) handle NotFound => RHSSet.empty ))
            in
                while (List.null(!prods) = false) do (
                    let
                        val rhs = ref (List.hd(!prods))
                        val i = ref 0
                        val j = ref 0 
                        val k = List.length(!rhs)
                        val ff = ref true
                        val ss = ref true 
                        val fs = ref true
                    in
                        if (is_nullable(!rhs) andalso not (is_null(x))) then (
                            NULLABLE := #1 (AtomMap.remove (!NULLABLE, x));
                            NULLABLE := AtomMap.insert (!NULLABLE, x, true)
                        ) else ();
                        i := 0;
                        while (!i < k) do (
                            if (!ff) then (
                                let 
                                    val v = List.nth(!rhs , !i)
                                    val c = ((AtomMap.remove (!FIRST , x)) handle LibBase.NotFound => (!FIRST, AtomSet.empty));
                                    val (mp , el) = (ref (#1 c) , ref (#2 c))
                                in
                                    FIRST := !mp;
                                    if (AtomMap.lookup (!NULLABLE, v) handle NotFound => false) then (
                                        el := AtomSet.union(!el , AtomMap.lookup(!FIRST, v) handle NotFound => AtomSet.empty)
                                    ) else ();
                                    FIRST := AtomMap.insert (!FIRST , x, !el);
                                    ff := (!ff andalso is_null(v))
                                end
                            ) else ();
                            j := !i + 1;
                            fs := true;
                            
                            while (!j < k) do (
                                fs := (!fs andalso is_null(List.nth(!rhs, !j)));
                                j := !j + 1
                            );
                            i := !i + 1
                        )
                    end;
                    prods := tl (!prods)
                )
            end;
            sym := tl(!sym)
        )
    end;
    if (AtomMap.listItems(!NULLABLE) = AtomMap.listItems(!OLD_NULLABLE)) then (
    ) else (
        OLD_NULLABLE := !NULLABLE;
        change := true 
    );
    if (AtomMap.listItemsi(!FIRST) = AtomMap.listItemsi(!OLD_FIRST)) then (
    ) else (
        OLD_FIRST := !FIRST;
        change := true 
    );
    if (AtomMap.listItemsi(!FOLLOW) = AtomMap.listItemsi(!OLD_FOLLOW)) then (
    ) else (
        OLD_FOLLOW := !FOLLOW;
        change := true 
    )
);


printNullable();
printFirst();
printFollow()
