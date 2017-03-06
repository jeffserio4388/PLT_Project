type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or 

type uop = Neg | Not | Deref | Ref

type typ = Int | Float | Bool | Void | MyString | StructType of string | Voidstar | PointerType of typ

type bind = typ * string

type expr =
    Literal of int
  | FloatLiteral of float
  | BoolLit of bool
  | MyStringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Dotop of expr * string 
  | Castop of typ * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type struct_decl = {
    sname: string;
    sformals: bind list;

}

type program = bind list * func_decl list * struct_decl list

