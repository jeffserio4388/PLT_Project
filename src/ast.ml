(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Dot

type uop = Neg | Not

type typ = Int | Bool | Void | MyString | Float | List_t of typ
        | File | Struct of string

type bind = typ * string  

type literal = Literal of int | MyStringLit of string | BoolLit of bool | FloatLit of float


type expr =
    Literal of int
    | FloatLit of float
    | MyStringLit of string
    | BoolLit of bool
    | Id of string
    | Binop of expr * op * expr
    | Unop of uop * expr
    | Assign of string * expr
    | Call of string * expr list
    | Access of string * int
    | Addleft of string * expr
    | Addright of string * expr
    | Popleft of string
    | Popright of string
    | StructAccess of expr * expr
    | Concat of expr * expr 
    | Noexpr

type stmt =
    Block of stmt list
    | Expr of expr
    | Return of expr
    | If of expr * stmt * stmt
    | Http_put of expr * expr
    | Http_get of expr * expr
    | Http_post of expr * expr
    | Http_delete of expr * expr
    | For of expr * expr * expr * stmt
    | While of expr * stmt
    | Local of typ * string * expr
    | List of typ * string

type var_init = typ * string * expr

type listen = {
    arg1    : string;
    arg2    : int;
}


type pipe_decl = {
    pname   :   string;
    listen  :   listen list;
    body:   stmt list;
}

type func_decl = {
    typ     :   typ;
    fname   :   string;
    formals :   bind list;
    body    :   stmt list;
}

type struct_decl = {
    sname   :   string;
    vars    :   var_init list;
}

type program = var_init list * stmt list * func_decl list  * pipe_decl list * struct_decl list

let string_of_op = function
    Add -> "+"
    | Sub -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Equal -> "=="
    | Neq -> "!="
    | Less -> "<"
    | Leq -> "<="
    | Greater -> ">"
    | Geq -> ">="
    | And -> "&&"
    | Or -> "||"
    | Dot -> "."


let string_of_uop = function
    Neg -> "-"
    | Not -> "!"

let rec string_of_typ = function
    Int -> "int"
    | Float -> "float"
    | Bool -> "bool"
    | Void -> "void"
    | MyString -> "string"
    | File -> "file"
    | Struct(s) -> "struct " ^ s
    | List_t(s) -> string_of_typ s

let rec string_of_expr = function
      Literal(l) ->         string_of_int l
    | MyStringLit(s) ->     s
    | FloatLit(l) ->        string_of_float l
    | BoolLit(true) ->      "true"
    | BoolLit(false) ->     "false"
    | Id(s) ->              s
    | Binop(e1, o, e2) ->   string_of_expr e1 ^ "" ^ string_of_op o ^ "" ^ string_of_expr e2
    | Unop(o, e) ->         string_of_uop o ^ string_of_expr e
    | Concat(e1,e2) ->      string_of_expr e1 ^" $ "^ string_of_expr e2
    | Assign(v, e) ->       v ^ " = " ^ string_of_expr e
    | Call("print_int",e) ->"print_int(" ^ String.concat ","  (List.map string_of_expr e)^")"
    | Call("print_str",e)-> "print_str("^  String.concat ","  (List.map string_of_expr e)^")"
    | Call("print_float",e)->"print_float(" ^ String.concat ","  (List.map string_of_expr e)^")"
    | Call("print_bool",e) -> "print_bool(" ^ String.concat "," (List.map string_of_expr e) ^ "? \"true\\n\":\"false\\n\")"
    | Call("len",e)        -> "len(" ^ String.concat ","(List.map string_of_expr e) ^ ")"
    | Call("cmp",e)        -> "cmp(" ^ String.concat ","(List.map string_of_expr e) ^ ")? 0:1"
    | Call(f, el) ->        f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    | Access(ln,n) ->       ln ^"[" ^ string_of_int n ^"]"
    | Addleft(n,e) ->      "addleft("^ n ^ ", "  ^string_of_expr e ^")"
    | Addright(n,e) ->      "addright("^ n ^ ", "  ^string_of_expr e ^ ")"
    | Popleft(n) ->        "popeft("^n^")"
    | Popright(n) ->       "popright("^n^")"
    | Noexpr ->             ""
    | StructAccess(s, e) -> string_of_expr s ^ "." ^ string_of_expr e



let rec string_of_stmt = function
    Block(stmts) ->         "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) ->           string_of_expr expr ^ ";\n";
  | Return(expr) ->         "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) ->  "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->        "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->   "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) ->          "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Http_put (e1, e2) -> "put"
  | Http_get (e1, e2) -> "get"
  | Http_post (e1, e2) -> "post"
  | Http_delete (e1, e2) -> "delete"
  | Local (t,n,Noexpr) -> string_of_typ t ^ " " ^  n ^";\n"
  | Local(t,n,e) -> string_of_typ t ^" " ^ n ^" = " ^string_of_expr e^";\n"
  | List(t,n) -> "struct "^String.sub (string_of_typ t) 0 1^ "_list " ^ n ^ ";\n" ^ "initList(&"^ n ^ ".list);\n" ^ string_of_typ t ^" " ^"ARRAY_FOR_LIST_"^ n ^ "[100000];\n"
                 ^ string_of_typ t ^"* " ^ "PTR_ARRAY_FOR_LIST_"^ n ^ "=" ^ "&ARRAY_FOR_LIST_" ^ n ^ "[0];\n" ^n^".cast = "^String.sub (string_of_typ t) 0 1^"_cast;"
(*  | Struct(sid, vid) -> "struct " ^sid^" "^ vid^";" *)
