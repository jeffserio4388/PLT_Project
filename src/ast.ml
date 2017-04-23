(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Dot

type uop = Neg | Not

type typ = Int | Bool | Void | MyString | Float | Double 
           

type bind = typ * string

type expr =
    Literal of int
    | MyStringLit of string
    | BoolLit of bool
    | Id of string
    | Binop of expr * op * expr
    | Unop of uop * expr
    | Assign of string * expr
    | Call of string * expr list
    | Noexpr

type var_init = typ * string * expr
(* type bind = Dec of typ * string | Dec_init of typ * string * expr *)

type stmt =
    Block of stmt list
    | Expr of expr
    | Return of expr
    | If of expr * stmt * stmt
    | For of expr * expr * expr * stmt
    | While of expr * stmt
    | Local of typ * string * expr

type pipe_decl = {
    pname: string;
    body : stmt list;
}

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
}

type struct_decl = {
  sname : string;
  vars : stmt list;
}

type program = var_init list * stmt list * func_decl list  * pipe_decl list * struct_decl list


(* Pretty-printing functions *)

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

let string_of_typ = function
    Int -> "int"
    | Bool -> "bool"
    | Void -> "void"
    | MyString -> "string"
    | Float -> "float"
    | Double -> "double"


let string_of_uop = function
    Neg -> "-"
    | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
    | MyStringLit(s) -> s
    | BoolLit(true) -> "true"
    | BoolLit(false) -> "false"
    | Id(s) -> s
    | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
    | Unop(o, e) -> string_of_uop o ^ string_of_expr e
    | Assign(v, e) -> v ^ " = " ^ string_of_expr e
    | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Local(t, id, e) -> if e = Noexpr then 
        string_of_typ t ^ " " ^ id ^ ";\n" else
        string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e ^";\n"

(* let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n" *)
let string_of_globals (t, id, e) = if e = Noexpr then
    string_of_typ t ^ " " ^ id ^ ";\n" else
    string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e ^";\n"
 
let string_of_pdecl pdecl = 
    "void work_" ^ pdecl.pname ^
    "(uv_work_t *req) {    " ^ 
    (* String.concat "\n    " (List.map string_of_vdecl pdecl.locals) ^ "\n    " ^ *)
    String.concat "\n    " (List.map string_of_stmt pdecl.body)^
    "\n}"

let string_of_pdecl_main pdecl = 
	"    int data_" ^ pdecl.pname ^ ";\n" ^
    "    uv_work_t req_" ^ pdecl.pname ^ ";\n" ^
    "    req_" ^ pdecl.pname ^ ".data = (void * ) &data_" ^ pdecl.pname ^ ";\n" ^
    "    uv_queue_work(uv_default_loop(), &req_" ^ pdecl.pname ^ ", work_" ^ pdecl.pname ^ ", after);\n"

let string_of_fdecl fdecl =
    string_of_typ fdecl.typ ^ " " ^
    fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
    ") {\n" ^
(*    String.concat "    " (List.map string_of_vdecl fdecl.locals) ^ "    " ^ *)
    String.concat "    " (List.map string_of_stmt fdecl.body) ^
    "}\n"

let string_of_sdecl sdecl =
    sdecl.sname ^ "{"^
    String.concat "\n    " (List.map string_of_stmt sdecl.vars)^
    "};"

let string_of_program (globals, stmts, funcs, pipes, structs) =
 
    "#include <stdio.h>\n#include <unistd.h>\n#include <uv.h>\n#include <stdlib.h>\n"^ 
    String.concat "\n" (List.map string_of_globals globals) ^ "\n" ^
    
    String.concat "\n" (List.map string_of_sdecl structs) ^ "\n" ^

  	String.concat "\n\n" (List.map string_of_fdecl funcs) ^ "\n" ^
 
	"void after(uv_work_t *req, int status) { }\n\n" ^
  
  	String.concat "\n\n" (List.map string_of_pdecl pipes) ^ "\n\n" ^

  	"int main() {\n    " ^

  	String.concat "\n    " (List.rev (List.map string_of_stmt stmts)) ^ "\n" ^
   
  	String.concat "\n" (List.map string_of_pdecl_main pipes) ^ "\n" ^

   	"    return uv_run(uv_default_loop(), UV_RUN_DEFAULT);\n}\n"
