open Ast

type scope = {
    parent_scope    : string;
    locals          : bind list;
}

type sem_expr = 
    SLiteral of int
    | SMyStringLit of string
    | BoolLit of bool

type sem_typ = UnderConstruction

type sem_stmt = UnderConstruction

type sem_fdecl = {
    return_typ  : typ;
    fname       : string;
    formals     : bind list;
    f_scope     : scope;
    body        : sem_stmt list;
}

type sem_prgm = var_init list * stmt list * sem_fdecl list * pipe_decl list * struct_decl list

let sem_fdecl_conv fdecl = 
    {
        return_typ  = fdecl.typ;
        fname       = fdecl.fname;
        formals     = fdecl.formals;
        f_scope     = { parent_scope = "globals"; locals = [] };
        body        = [];
    }

