structure Code : 
  sig val print : TextIO.outstream * Absyn.exp -> unit end =
struct

structure A = Absyn

val temp_let : int ref = ref 0;

fun print (outstream, e0) =
	let 
		fun say s = 
			TextIO.output(outstream,s)


  	fun sayln s = 
		(
			say s; 
			say "\n"
		) 


		fun indent 0 = ()
    | indent i = 
    (
      say "\t"; 
      indent(i - 1)
    )


		fun opname A.PlusOp = "+"
    | opname A.MinusOp = "-"
    | opname A.TimesOp = "*"
    | opname A.DivideOp = "/"
    | opname A.EqOp = "=="
    | opname A.NeqOp = "!="
    | opname A.LtOp = "<"
    | opname A.LeOp = "<="
    | opname A.GtOp = ">"
    | opname A.GeOp = ">="
		| opname A.AndOp = "and"
		| opname A.OrOp = "or"


		fun dolist d f [a] = 
    (
      sayln "";
      f(a, d)
    )
    | dolist d f (a :: r) = 
    (
      f(a, d); 
      dolist d f r
    )
    | dolist d f nil = ()


		fun dolist_fn d f [a] = 
    (
      sayln "";
			indent d;
			say "return ";	
      f(a, d)
    )
    | dolist_fn d f (a :: r) = 
    (
      f(a, d); 
      dolist d f r
    )
    | dolist_fn d f nil = ()


    fun dolist_no d f [a] = 
    (
      f(a, d)
    )
    | dolist_no d f (a :: r) = 
    (
      f(a, d); 
      dolist d f r
    )
    | dolist_no d f nil = ()


		fun var_name (A.SubscriptVar(v, e, p), d) = 
		(
			var_name(v, 0)
		)
		| var_name (A.SimpleVar(s, p), d) = 
		(
			indent d;
			say (Symbol.name s)
		) 


		fun var(A.SimpleVar(s, p), d) = 
		(
			indent d;
			say(Symbol.name s)
		)
		| var(A.FieldVar(v, s, p), d) = 
		(
			(* indent d; 
			sayln "FieldVar(";
			var(v, d + 1); 
			sayln ",";
			indent(d + 1);
			say(Symbol.name s); 
			say ")\n" *)
		)
		| var(A.SubscriptVar(v, e, p), d) = 
		(
			indent d; 
			var(v, 0);
			say "[";
			exp(e, 0); 
			say "]"
		)
  
		and 
		
		exp(A.VarExp v, d) = 
		(
			indent d; 
			var(v, 0)
		)
		| exp(A.NilExp, d) = 
		(
      indent d;
      say "None"
		)
		| exp(A.IntExp i, d) = 
		(
			indent d; 
			say(Int.toString i)
		)
		| exp(A.StringExp(s, p), d) = 
		(
			indent d; 
			say "\"";
      say s;
      say "\"" 
		)
		| exp(A.CallExp{func, args, pos}, d) =
		(
			indent d;
			say(Symbol.name func);
			say "_";
      say "(";
			dolist_no 0 exp args;
      say ")"
		)
		| exp(A.OpExp{left, oper, right, pos}, d) =
		(
			indent d;  
      say "(";
			exp(left, 0);
			say " ";
      say(opname oper); 
			say " ";
			exp(right, 0);
      say ")"
		)
		| exp(A.RecordExp{fields, typ, pos}, d) =
		(
			(* let 
				fun f((name, e, pos), d) = 
				(
					indent d; 
					say "("; 
					say(Symbol.name name);
					sayln ","; exp(e, d + 1);
					say ")"
				)
			in 
				indent d; 
				say(Symbol.name typ); 
				sayln ","; 
				dolist d f fields; 
				say "])" 
			end *)
		)
		| exp(A.SeqExp l, d) = 
		(
			(* indent d; *)
			(* say "SeqExp[";  *)
			dolist d exp (map #1 l)
			(* say "]" *)
		)
		| exp(A.AssignExp{var = v, exp = e, pos}, d) = 
		(
			indent d;
			(* say "nonlocal ";
			var_name (v, 0);
			sayln "";
			indent d; *)
			var(v, 0);
      say " = "; 
			exp(e, 0);
			sayln ""
		)
		| exp(A.IfExp{test, then', else', pos}, d) =
		(
			sayln "";
			indent d; 
      say "if ";
			exp(test, 0); 
			say ":\n";
			exp(then', d + 1);
      (* say "\n"; *)
			case else' of NONE => ()
				| SOME e => (
											sayln "";
											indent d;
                      say "else:\n";
											exp(e, d + 1)
                      (* say "\n" *)
										)
		)
		| exp(A.WhileExp{test, body, pos}, d) =
		(
			indent d; 
			say "while "; 
			exp(test, 0);
			sayln ":\n";
			exp(body, d + 1);
      sayln ""
		)
		| exp(A.ForExp{var = v, escape = b, lo, hi, body, pos}, d) =
		(
			indent d;
			say "for ";
			say(Symbol.name v); 
			say " in range (";
			(* say(Bool.toString (!b)); 
			sayln ","; *)
			exp(lo, 0);
			say ", "; 
			exp(hi, 0); 
			say " + 1):\n";
			exp(body, d + 1)
		)
		| exp(A.BreakExp p, d) = 
		(
			indent d; 
			sayln "break"
		)
		| exp(A.LetExp{decs, body, pos}, d) =
		(
			let 
				val here = !temp_let
			in 
				indent d;
				say "def ";
				say "LetH";
				say (Int.toString (!temp_let));
				sayln "():";
				temp_let := !temp_let + 1;
				dolist_fn (d + 1) dec decs;
				exp(body, d + 1);
				sayln "";
				indent d;
				say "LetH";
				say (Int.toString here);
				say "()"
			end
		)
		| exp(A.ArrayExp{typ, size, init, pos}, d) =
		(
			indent d;
			(* say(Symbol.name typ);  *)
			say "[";
			exp(init, 0);
			say "] * (";
			exp(size, 0); 
			say ")"
		)

  	and 
		
		dec(A.FunctionDec l, d) = 
	    let 
				fun field({name, escape, typ, pos}, d) = 
				(
					
					say(Symbol.name name)
					(* indent d; 
					
					say(Bool.toString(!escape));
					say(Symbol.name typ);  *)
				)
				fun f({name, params, result, body, pos}, d) =
		   	(
					indent d; 
					(* say "(";  *)
					say "def ";
					say (Symbol.name name); 
					say "_";
					say "(";
		    	dolist_no d field params;
					say "):\n";
					exp(body, d + 1);
					sayln ""
					(* indent (d + 1); 
					say "return ("; 
					say ")" *)
				)
	    in 
			 	(* indent d; *)
				dolist d f l
	    end
    | dec(A.VarDec{name, escape, typ, init, pos}, d) =
	  (
			indent d; 
			(* say "nonlocal "; *)
			say(Symbol.name name); 
			say (" = ");
	    (* say(Bool.toString (!escape));  *)
	    (* case typ of 
				NONE => ()
		    | SOME(s,p) => 
				(
					 say "SOME(";  
					say(Symbol.name s)
					 say ")" 
				); *)
			exp(init, 0);
			sayln ""
		)
    | dec(A.TypeDec l, d) = 
		(
			(* let 
				fun tdec({name, ty = t, pos}, d) = 
				(
					indent d; 
					say"("; 
					say(Symbol.name name); 
					sayln ",";
					ty(t,d+1); 
					say ")"
				)
			in 
				indent d; 
				say "TypeDec["; 
				dolist d tdec l; 
				say "]"
			end *)
		)
			
   
  	and 
	
		ty(A.NameTy(s,p), d) = 
		()
    | ty(A.RecordTy l, d) =  
		()
    | ty(A.ArrayTy(s,p),d) = 
		()

 	in  
	 	sayln "from __future__ import print_function\n";
		sayln "def print_(s):\n\tprint(s, end=\"\")\n\n";
	 	exp(e0, 0);
		sayln "\n";
		TextIO.flushOut outstream
		
	end

end

