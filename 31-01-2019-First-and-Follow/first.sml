val FIRST : AtomSet.set AtomMap.map ref = ref AtomMap.empty;
val FOLLOW : AtomSet.set AtomMap.map ref = ref AtomMap.empty;
val NULLABLE : bool AtomMap.map ref = ref AtomMap.empty;

val OLD_FIRST : AtomSet.set AtomMap.map ref = ref AtomMap.empty;
val OLD_FOLLOW : AtomSet.set AtomMap.map ref = ref AtomMap.empty;
val OLD_NULLABLE : bool AtomMap.map ref = ref AtomMap.empty;

val change = ref true;

fun is_null (s) = 
    (
        AtomMap.lookup (!NULLABLE, s) handle NotFound => false
    );

fun is_nullable (x :: xs) = 
    (
        is_null (x) andalso is_nullable (xs)
    )
| is_nullable ([]) = 
    (
        true
    );

fun printAtomList (x :: xs) = 
    (
        print (Atom.toString (x));
        printAtomList (xs)
    )
| printAtomList ([]) = 
    (
        print(" ")
    );

fun init x = 
    (
        let 
            val prods = ref (RHSSet.listItems ( AtomMap.lookup((#rules Grm) , x ) handle NotFound => RHSSet.empty ))
        in
            NULLABLE := AtomMap.insert (!NULLABLE, x, false);
            FOLLOW := AtomMap.insert (!FOLLOW, x, AtomSet.empty);
            FIRST := AtomMap.insert (!FIRST, x, AtomSet.empty)
        end
           
    );

fun print_prods x = 
    (
        let 
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
            );
            print ("\n")
        end
    );

fun calculate_nullable x rhs = 
    (
        if (is_nullable(!rhs) andalso not (is_null(x))) then (
                change := true;
                NULLABLE := #1 (AtomMap.remove (!NULLABLE, x));
                NULLABLE := AtomMap.insert (!NULLABLE, x, true)
            ) else ()
    );

fun calculate_first x rhs = 
    (
        let 
            val i = ref 0
            val still_nullable = ref true
            val k = List.length(!rhs)
            val c = ((AtomMap.remove (!FIRST , x)) handle LibBase.NotFound => (!FIRST, AtomSet.empty))
            val (mp , el) = (ref (#1 c) , ref (#2 c))
        in 
            FIRST := !mp;
            while (!i < k andalso !still_nullable) do (
                el :=  AtomSet.union(!el , AtomMap.lookup(!FIRST, yi) handle NotFound => AtomSet.empty);
                i := !i + 1
            )
        end
    );

fun calculate_follow x rhs = 
    (
        
    )

fun traverse_prods function x = 
    (
        let 
            val prods = ref (RHSSet.listItems ( AtomMap.lookup((#rules Grm) , x ) handle NotFound => RHSSet.empty ))
        in
            while (List.null(!prods) = false) do (
                let
                    val rhs = ref (List.hd(!prods))
                in
                    function x rhs
                end;
                prods := tl (!prods)
            )
        end
    );

fun traverse_sym function = 
    (
        let 
            val sym = ref (AtomMap.listKeys (#rules Grm))
        in 
            while (List.null (!sym) = false) do (
                let 
                    val x = hd(!sym)
                in 
                    function x;
                    sym := tl(!sym)
                end
            )
        end
    );

fun print_all_productions () = 
    (
        print ("\n=== All productions ===\n");
        traverse_sym print_prods;
        print ("=== ======== ===\n")
    );

fun printAtomSet at_set = 
    (
        printAtomList(AtomSet.listItems(at_set))
    );

fun printFirstFollowHelper [] =  
    (
        print "=== ======== ===\n"
    )
|   printFirstFollowHelper (x::xs) = 
    (
        let
            val (k , v) = x
        in
            (print ((Atom.toString k) ^ " : " );
            printAtomSet(v);
            printFirstFollowHelper xs)
        end
    );

fun printNullableHelper [] = 
    (
        print "=== ======== ===\n"
    )
|   printNullableHelper (x::xs) = 
    (
        let
            val (k , v) = x
        in
            (print ((Atom.toString k) ^ " : " ^ (Bool.toString v) ^ "\n");
            printNullableHelper xs)
        end
    );

fun printNullable () =  
    (
        let
            val nullable_lst = AtomMap.listItemsi (!NULLABLE)
        in
            (print ("\n=== NULLABLE ===\n");
            printNullableHelper nullable_lst)
        end
    );

fun printFollow () = 
    (
        let
            val follow_lst = AtomMap.listItemsi (!FOLLOW)
        in
            (print ("\n=== FOLLOW ===\n");
            printFirstFollowHelper follow_lst)
        end
    );

fun printFirst () = 
    (
        let
            val first_lst = AtomMap.listItemsi (!FIRST)
        in
            (print ("\n=== FIRST ===\n");
            printFirstFollowHelper first_lst)
        end
    );
                    

print_all_productions();
traverse_sym init;

fun calculate function = 
    (
        change := true;
        while (!change = true) do (
            change := false;
            traverse_sym (traverse_prods (function))
        )
    );

calculate calculate_nullable;
calculate calculate_first;
calculate calculate_follow;

printNullable();
printFirst();
printFollow()
