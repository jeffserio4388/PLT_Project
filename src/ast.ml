(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = Int | Bool | Void | MyString

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

type stmt =
    Block of stmt list
    | Expr of expr
    | Return of expr
    | If of expr * stmt * stmt
    | For of expr * expr * expr * stmt
    | While of expr * stmt

type pipe_decl = {
    pname: string;
    locals : bind list;
    body : stmt list;
}

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
}

type program = bind list * stmt list * func_decl list  * pipe_decl list

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

let string_of_typ = function
    Int -> "int"
    | Bool -> "bool"
    | Void -> "void"
    | MyString -> "string"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"
 
let string_of_pdecl pdecl = 
    "void idle_" ^ pdecl.pname ^
    " (uv_idle_t* handle) {    " ^ 
    String.concat "\n    " (List.map string_of_vdecl pdecl.locals) ^ "\n    " ^
    String.concat "\n    " (List.map string_of_stmt pdecl.body)^
    "    uv_idle_stop(handle);\n}"

let string_of_pdecl_main pdecl = 
	"    uv_idle_t idler_" ^ pdecl.pname ^ ";\n" ^
    "    uv_idle_init(uv_default_loop(), &idler_" ^ pdecl.pname ^ ");\n" ^
    "    uv_idle_start(&idler_" ^ pdecl.pname ^ ", idle_" ^ pdecl.pname ^ ");\n"

let string_of_fdecl fdecl =
    string_of_typ fdecl.typ ^ " " ^
    fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
    ") {\n" ^
    String.concat "    " (List.map string_of_vdecl fdecl.locals) ^ "    " ^ 
    String.concat "    " (List.map string_of_stmt fdecl.body) ^
    "}\n"

let string_of_program (vars, stmts, funcs, pipes) =
 
  "#include <stdio.h>\n#include <unistd.h>\n#include <uv.h>\n#include <stdlib.h>\n"^ 
    String.concat "\n" (List.map string_of_vdecl vars) ^ "\n" ^
 
  String.concat "\n\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  
  String.concat "\n\n" (List.map string_of_pdecl pipes) ^ "\n\n" ^

  "int main() {\n    " ^

  String.concat "\n    " (List.rev (List.map string_of_stmt stmts)) ^ "\n" ^
   
  String.concat "\n" (List.rev (List.map string_of_pdecl_main pipes)) ^ "\n" ^

   "    uv_run(uv_default_loop(), UV_RUN_DEFAULT);\n"^
   "    uv_loop_close(uv_default_loop());\n\n"^
   "    return 0;\n}\n"

(*
let string_of_program (vars, funcs) =
    "#include <stdio.h>\n#include <unistd.h>\n#include <uv.h>\n#include <stdlib.h>\n int main(){" ^
    String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
    String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
    "}"
*)


(*
let string_of_pdecl pdecl =
    pdecl.pname ^ "\n" ^
    "uv_idle_t idler\n;" ^
    "uv_idle_init(uv_default_loop(), &idler);\n" ^
    "uv_idle_start(&idler, idle);\n"

let string_of_pdecl_second pdecl =
    "void idle(uv_idle_t* handle) {\n" ^
    pdecl.pname ^ 
    String.concat "" (List.map string_of_vdecl pdecl.locals) ^ "\n" ^
    String.concat "" (List.map string_of_stmt pdecl.body) ^ "\n" ^
    "uv_idle_stop(handle);\n" ^
    "}\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_pdecl fdecl.pipes) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_fdecl_second fdecl =
  String.concat "" (List.map string_of_pdecl_second fdecl.pipes)

let string_of_program_first (vars, funcs) =
    "#include <stdio.h>\n#include <unistd.h>\n#include <uv.h>\n#include <stdlib.h>\n" ^
    String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
    String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n"

let string_of_program_second (vars, funcs) =
    String.concat "\n" (List.map string_of_fdecl_second funcs) ^ "\n"

let string_of_program (vars, funcs) =
    string_of_program_first (vars, funcs) ^ "\n" ^ 
    string_of_program_second (vars, funcs) ^ "\n"
*)
