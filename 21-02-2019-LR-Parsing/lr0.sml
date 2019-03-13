use "first.sml";

type Item = { lhs    : atom      (* the left hand side *)
            , before : atom list (* the symbols/tokens before the dot
	                              in the rhs in reverse order
							      *)
	        , after : atom list  (* The symbols/tokens after the dot *)
		    }

(* val aItem = { lhs       = atom "A"
              before = List.map atom ["A", "a"]
              after  = List.map atom ["b", "B"]
            } *)

signature PROXY = sig
   type proxy
   type actual

   val proxy  : actual -> proxy
   val actual : proxy -> actual
end

functor Proxy(structure A : ORD_KEY) : PROXY = struct
	type proxy = int
	type sofar = ref 0
	type proxyMap = ref (map from A.ord_key to int)
	type reverseMap = ref (map from int -> A.ord_key)

	val proxy :
	val actual : proxy -> A.ord_key

	(* ... *)
end


