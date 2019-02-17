structure Ast = struct
    datatype binop = PLUS | MINUS | TIMES | DIVIDE

    type ID = string
    type TYFIELD = (ID * ID)

    datatype exp    = INT of int
                    | ID of string
                    | BINOP of (exp * binop * exp)
                    | LET of (dec list * exp list)

        and 

        dec         = VARDEC of (ID * exp)
                    | FUNCDEC of (ID * TYFIELD list * exp)
        
    datatype program = EXPS of (exp list)

    fun binopDenote x = case x of
                            PLUS    =>  "+"
                        |   MINUS   =>  "-"
                        |   TIMES   =>  "*"
                        |   DIVIDE  =>  "/"

end
