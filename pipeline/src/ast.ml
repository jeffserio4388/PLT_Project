(* Abstract Syntax Tree *)


type typ = Undefined | Bool | Int | Float | Void | String | Tuple of string | Table of string | Array of typ


type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not


type tupitem = typ * string

type tup = string * tupitem list 

type obj = (* lhs *)
    Id of string
  | Brac of obj * expr * bool (* a[0] a[i] a[i+1] *)
  | Brac2 of obj * expr * expr (* a[0:2] *)
  | Attr of obj * string (* a$b *)
  | AttrInx of obj * expr (* a$(0) *)
and
 expr =
    Literal of int
  | BoolLit of bool
  | FloatLit of float
  | StrLit of string
  | Obj of obj
  | Binop of expr * op * expr(* as on lhs *)
  | Unop of uop * expr
  | Assign of obj * expr
  | Call of string * expr list
  | TupInst of string (* tuple instantiation *)
  | TabInst of string (* table instantiation e.g. Foo[] *)
  | Arr of expr list (* arrays e.g. [1,2,3] *)
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of string * expr * stmt (* for i in a *)
  | While of expr * stmt
  | Break
  | Continue

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }


type program_decl =
    Func of func_decl
  | Tup of tup

type program = stmt list * program_decl list

type includ = 
  FileIncl of string

type program_with_headers = includ list * program
