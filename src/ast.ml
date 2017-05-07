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
    | Add_left of expr * expr
    | Add_right of expr * expr
    | Find_node of expr * expr * expr
    | Http of string * string * expr
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
  (*| Int_list_decl(listid, intlist) ->   "struct List *" ^ listid ^ " = initialize((int[]) {" ^ (String.concat ", " (List.map string_of_int intlist)) ^ "}, " 
                                        ^ (string_of_int (List.length intlist)) ^ ", 1);"
  | Str_list_decl(listid, strlist) ->   "struct List *" ^ listid ^ " = initialize((char*[]) {" ^ (String.concat ", " strlist) ^ "}, " 
                                        ^ (string_of_int (List.length strlist))  ^ ", 0);"*)
  | Add_left(e1, e2) -> "void *a7858585765 = (void * )" ^string_of_expr e2^ "; \n addLeft(" ^ string_of_expr e1 ^" ,"^ "a7858585765"^ ");"
  | Add_right(e1, e2) -> "void *a782345765 = (void * )" ^string_of_expr e2^ "; \n addRight(" ^ string_of_expr e1 ^" ,"^ "a782345765"^ ");"
  | Find_node(e1, e2, e3) -> "void *a7b45765 = (void * )" ^string_of_expr e2^ "; \n findNode(" ^ string_of_expr e1 ^" ,"^ "a7b45765, "^ string_of_expr e3 ^");"
  | Http (e1, e2, e3) -> e1 ^ " " ^ e2
  | Local (t,n,Noexpr) -> string_of_typ t ^ " " ^  n ^";\n"
  | Local(t,n,e) -> string_of_typ t ^" " ^ n ^" = " ^string_of_expr e^";\n"
  | List(t,n) -> "struct "^String.sub (string_of_typ t) 0 1^ "_list " ^ n ^ ";\n" ^ "initList(&"^ n ^ ".list);\n" ^ string_of_typ t ^" " ^"ARRAY_FOR_LIST_"^ n ^ "[100000];\n"
                 ^ string_of_typ t ^"* " ^ "PTR_ARRAY_FOR_LIST_"^ n ^ "=" ^ "&ARRAY_FOR_LIST_" ^ n ^ "[0];\n" ^n^".cast = "^String.sub (string_of_typ t) 0 1^"_cast;"
(*  | Struct(sid, vid) -> "struct " ^sid^" "^ vid^";" *)

let string_of_typ = function
    Int -> "int"
    | Bool -> "bool"
    | Void -> "void"
    | MyString -> "string"
    | Float -> "float"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_pdecl_listen pdecl = 
"uv_tcp_t tcp_" ^ pdecl.pname ^ ";\n" ^
"struct sockaddr_in addr_" ^ pdecl.pname ^ ";\n" ^
"uv_work_t req_listen_" ^ pdecl.pname ^ ";\n" ^

"void post_listen_" ^ pdecl.pname ^ "(uv_work_t *req) {
    // fprintf(stderr, \"%s\", req->data);
    " ^ String.concat "\n    " (List.map string_of_stmt pdecl.body) ^ "
}

void onread_"^ pdecl.pname ^"(uv_stream_t *client, ssize_t nread, const uv_buf_t *buf) {
    if (nread > 0) {
        req_listen_" ^ pdecl.pname ^ ".data = (void * ) buf->base;
        uv_queue_work(loop, &req_listen_" ^ pdecl.pname ^ ", post_listen_" ^ pdecl.pname ^ ", after);
        return;
    }
    if (nread < 0) {
        if (nread != UV_EOF)
            fprintf(stderr, \"Read error %s\", uv_err_name(nread));
        uv_close((uv_handle_t* ) client, NULL);
    }

    // free(buf->base);
}

void on_new_connection_" ^ pdecl.pname ^ "(uv_stream_t *server, int status) {
    if (status < 0) {
        fprintf(stderr, \"New connection error %s\", uv_strerror(status));
        // error!
        return;
    }

    uv_tcp_t *client = (uv_tcp_t* ) malloc(sizeof(uv_tcp_t));
    uv_tcp_init(loop, client);
    if (uv_accept(server, (uv_stream_t* ) client) == 0) {
        uv_read_start((uv_stream_t* ) client, alloc_buffer, onread_" ^ pdecl.pname ^ ");
    }
    else {
        uv_close((uv_handle_t* ) client, NULL);
    }
}


void listen_" ^ pdecl.pname ^ "(char *ip_addr, int port) {
    uv_tcp_init(loop, &tcp_" ^ pdecl.pname ^ ");

    uv_ip4_addr(ip_addr, port, &addr_" ^ pdecl.pname ^ ");

    uv_tcp_bind(&tcp_" ^ pdecl.pname ^ ", (const struct sockaddr* ) &addr_" ^ pdecl.pname ^ ", 0);
    int r = uv_listen((uv_stream_t* ) &tcp_" ^ pdecl.pname ^ ", DEFAULT_BACKLOG, on_new_connection_" ^ pdecl.pname ^ ");
    if (r) {
        fprintf(stderr, \"Listen error %s\", uv_strerror(r));
    }
}\n"

let string_of_pdecl_no_listen pdecl = 
    "int 3918723981723912_" ^ pdecl.pname ^ ";\n" ^ 
      String.concat "\n    " (List.map string_of_stmt pdecl.body)

 
let string_of_pdecl pdecl = 
    (if ((List.length pdecl.listen) != 0) then (string_of_pdecl_listen pdecl) else "\n" ) ^ "\n" ^
    "void work_" ^ pdecl.pname ^
    "(uv_work_t *req) {    " ^ 
    (if ((List.length pdecl.listen) == 0) then (string_of_pdecl_no_listen pdecl) else "listen_" ^ pdecl.pname ^ "(" ^ (List.hd pdecl.listen).arg1 ^ ", " ^ string_of_int (List.hd pdecl.listen).arg2 ^ ");" ) ^ "\n" ^
    "\n}"


let string_of_pdecl_main pdecl = 
    "    int data_" ^ pdecl.pname ^ ";\n" ^
    "    uv_work_t req_" ^ pdecl.pname ^ ";\n" ^
    "    req_" ^ pdecl.pname ^ ".data = (void * ) &data_" ^ pdecl.pname ^ ";\n" ^
    "    uv_queue_work(loop, &req_" ^ pdecl.pname ^ ", work_" ^ pdecl.pname ^ ", after);\n"

let string_of_fdecl fdecl =
    string_of_typ fdecl.typ ^ " " ^
    fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
    ") {\n" ^
    String.concat "    " (List.map string_of_stmt fdecl.body) ^
    "}\n"

let string_of_sdecl sdecl =
    "struct " ^ sdecl.sname ^ " {\n    " ^
    String.concat "    " (List.map string_of_vdecl sdecl.vars) ^
    "};\n"

let string_of_program (vars, stmts, funcs, pipes, structs) =
 
    "#include <stdio.h>\n#include <unistd.h>\n#include <uv.h>\n#include <stdlib.h>\n#include \"stdlib/mylist.h\"\n"^ 

"#define DEFAULT_PORT 7000
#define DEFAULT_BACKLOG 128

uv_loop_t *loop;

void alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
    buf->base = (char* ) malloc(suggested_size);
    buf->len = suggested_size;
}" ^

    String.concat "\n" (List.map string_of_vdecl vars) ^ "\n" ^
    
  	String.concat "\n\n" (List.map string_of_fdecl funcs) ^ "\n" ^
	
    String.concat "\n" (List.map string_of_sdecl structs) ^ "\n" ^
 
	"void after(uv_work_t *req, int status) { }\n\n" ^
  
  	String.concat "\n\n" (List.map string_of_pdecl pipes) ^ "\n\n" ^

  	"int main() {\n    " ^
    "    loop = uv_default_loop();" ^

  	String.concat "\n    " (List.rev (List.map string_of_stmt stmts)) ^ "\n" ^
   
  	String.concat "\n" (List.map string_of_pdecl_main pipes) ^ "\n" ^

   	"    return uv_run(loop, UV_RUN_DEFAULT);\n}\n"

