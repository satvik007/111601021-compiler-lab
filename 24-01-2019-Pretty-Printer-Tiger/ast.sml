structure Ast = struct
    datatype binop = PLUS | MINUS | TIMES | DIVIDE | EQUALS

    type ID = string

    datatype exp    = INT of int
                    | ID of string
                    | BINOP of (exp * binop * exp)
                    | LET of (dec list * exp list)
                    | FUNC of (ID * exp list)
                    | IF of (exp * exp)
                    | IFELSE of (exp * exp * exp)
        and 

        dec         = VARDEC of (ID * exp)
        
    datatype program = EXPS of (exp list)

    fun binopDenote x = case x of
                            PLUS    =>  "+"
                        |   MINUS   =>  "-"
                        |   TIMES   =>  "*"
                        |   DIVIDE  =>  "/"
                        |   EQUALS  =>  "="

end
