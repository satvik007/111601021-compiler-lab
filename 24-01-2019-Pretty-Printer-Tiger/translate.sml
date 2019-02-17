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
| 	print_expression (Ast.LET (x, y)) =
			(	indent := !indent + 1;	
				print ("let" ^ new_line(!indent));
				print_decs (x);
				print ("in" ^ new_line (!indent));	
				print_exps (y);
				indent := !indent - 1;
				print ("end")

			)
| 	print_expression (Ast.ID x) =
		(print (x)
		)
and

print_exps (x::exp_lst)   =
    (   print_expression (x);
        print (";");
        print (new_line(!indent));
        print_exps(exp_lst))
|   print_exps []   = (print (bktab))

and 

print_decs (x :: y)		=
	(	print_dec(x);
		if (null y) then print ("") else print (";");
		print (new_line(!indent));
		print_decs(y)
	)
| print_decs [] =	(print (bktab))

and 

print_dec (Ast.VARDEC(x, y)) = 
	(	print ("var " ^ x ^ " := ");
	 	print_expression(y)
	)
| print_dec (Ast.FUNCDEC(a, b, c)) =
	(
		print(a);
		print ("(");
		print_param(b);
		print(") = ");
		print_expression(c);
		print(new_line(!indent))
	)

and 

print_param ((a, b) :: xs) = 
	( 
		print (a ^ ":" ^ b);
		if (null xs) then () else print(", ");
		print_param(xs)
	)
| print_param ([]) = ()


fun compile []        = ()
  | compile (x :: xs) = (print_expression x ; print (";" ^ new_line (!indent)) ; compile xs)

end
