structure Ast = struct 

    type ID = string

    type NUMCONST = int

    type STRINGCONST = string

    type BOOLCONST = bool

    datatype binop = PLUS | MINUS | TIMES | DIVIDE | MOD

    datatype relop = GT | GE | LT | LE | EQ | NEQ

    datatype unaryop = UMINUS | NOT

    datatype boolop = AND | OR | XOR

    datatype declaration = VARDEC of (typeSpecifier * varDeclId list)
                         | FUNDEC of funDeclaration

    and varDeclId = VARDEC_IDEN of ID
                  | VARDEC_ARR_IDEN of (ID * NUMCONST)
                  | VARDEC_INIT of (ID * simpleExpression)

    and funDeclaration = RETFUNC of (typeSpecifier * ID * param list * declaration)

    and param = PARAM of ( typeSpecifier * paramId list )

    and paramId = PARAM_IDEN of ID 
                | PARAM_ARR_IDEN of ID

    (* and statement   = EXPR_STMT of (expression option)
                    | SEL_STMT of selectionStmt
                    | RET_STMT of returnStmt *)

    (* and expression  = SIMP_EXP of simpleExpression
                    | ASSIGNMENT of (mutable * expression) *)
    
    and simpleExpression = IMMUTABLE of (immutable)
                        | OPERATION of (simpleExpression * binop * simpleExpression)
                        (*| RELATION of (expression * relop * expression)
                        | BOOLEAN of (expression * boolop * expression)
                        | UNARY of (unaryop * expression)   *)
    
    and immutable = NUM_CONST of NUMCONST 
                |   STRING_CONST of STRINGCONST
                |   BOOL_CONST of BOOLCONST

    (* and mutable = MUT_IDEN of ID
                | MUT_ARR of (mutable * expression)

    and selectionStmt   = IF of (simpleExpression * statement)
                        | IF_ELSE of (simpleExpression * statement * statement)

    and returnStmt = RETNOTHING
                    | RETEXPR of (expression) *)
    
    and typeSpecifier   = INT
                        | BOOL 
                        | STRING


    type program = declaration list

end