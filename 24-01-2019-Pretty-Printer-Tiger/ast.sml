structure Ast = struct
    datatype binop = PLUS | MINUS | TIMES | DIVIDE

    type ID = string

    datatype exp    = INT of int
                    | BINOP of (exp * binop * exp)

    datatype program = EXPS of (exp list)

    fun binopDenote x = case x of
                            PLUS    =>  "+"
                        |   MINUS   =>  "-"
                        |   TIMES   =>  "*"
                        |   DIVIDE  =>  "/"
end
