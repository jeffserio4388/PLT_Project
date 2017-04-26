open Ast

type sexpr = 
    SLiteral of int
    | SMyStringLit of string
    | BoolLit of bool
