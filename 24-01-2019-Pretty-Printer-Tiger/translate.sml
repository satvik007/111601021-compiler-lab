structure Translate =
struct

val indent = ref 0

val bktab = "\b\b\b\b";

fun get_tabs n = if n = 0 then "" else "    " ^ get_tabs (n-1)

fun new_line n = ("\n" ^ get_tabs(n))

fun print_expression (Ast.INT x) = print (Int.toString (x))
|   print_expression (Ast.BINOP (x , bop , y)) =
            (   print_expression x ;
                print (" " ^ (Ast.binopDenote bop) ^ " ") ;
                print_expression y
                )

and

print_exps (x::exp_lst)   =
    (   print_expression (x);
        print (";");
        print (new_line(!indent));
        print_exps(exp_lst))
|   print_exps []   = (print (bktab))

fun compile []        = ()
  | compile (x :: xs) = (print_expression x ; print (";" ^ new_line (!indent)) ; compile xs)

end
