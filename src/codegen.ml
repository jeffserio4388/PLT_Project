(*codegen for Pipeline*)




open Ast

module S = Semant

module StringMap = Map.Make(String)


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
    | Mod -> "%"


let string_of_uop = function
    Neg -> "-"
    | Not -> "!"

let rec string_of_typ = function
    Int -> "int"
    | Float -> "float"
    | Bool -> "int"
    | Void -> "void"
    | MyString -> "char *"
    | File -> "File"
    | Struct(s) -> "struct " ^ s
    | List_t(s) -> string_of_typ s

let rec string_of_expr = function
      Literal(l) ->         string_of_int l
    | MyStringLit(s) ->     s
    | FloatLit(l) ->        string_of_float l
    | BoolLit(true) ->      "1"
    | BoolLit(false) ->     "0"
    | Id(s) ->              s
    | Binop(e1, o, e2) ->   string_of_expr e1 ^ "" ^ string_of_op o ^ "" ^ string_of_expr e2
    | Unop(o, e) ->         string_of_uop o ^ string_of_expr e
    | Concat(e1,e2) ->      "stringcat(" ^string_of_expr e1 ^","^ string_of_expr e2 ^")"
    | Assign(v, e) ->       v ^ " = " ^ string_of_expr e
    | Call("print_int",e) ->"fprintf(stdout, \"%d\"," ^ String.concat ","  (List.map string_of_expr e)^"); fflush(stdout)"
    | Call("print_str",e)-> "fprintf(stdout, \"%s\","^  String.concat ","  (List.map string_of_expr e)^"); fflush(stdout)"
    | Call("print_float",e) ->"fprintf(stdout, \"%f\"," ^ String.concat ","  (List.map string_of_expr e)^"); fflush(stdout)"
    | Call("print_bool",e) -> "fprintf(stdout, " ^ String.concat "," (List.map string_of_expr e) ^ "? \"true\\n\":\"false\\n\"); fflush(stdout)"
    | Call("init_file_obj", e) -> let string_of_actuals = List.map string_of_expr e 
                              in
                              let check_mode m = match m with
                                  "\"r\""   -> ()
                                 |"\"w\""   -> ()
                                 |"\"a\""   -> ()
                                 |"\"r+\""  -> ()
                                 |"\"w+\""  -> ()
                                 |"\"a+\""  -> ()
                                 |"\"rb\""  -> ()
                                 |"\"wb\""  -> ()
                                 |"\"ab\""  -> ()
                                 |"\"rb+\"" -> ()
                                 |"\"wb+\"" -> ()
                                 |"\"ab+\"" -> ()
                                 | _    -> raise (Failure("illegal file mode "^ 
                                                  m ^ "given as argument in open file" ^
                                                  " perhaps you added an extra space."))
                              in  
                              let expr_string =
                              function
                              [file_obj; filename; filemode] ->
                                            check_mode filemode; 
                                            "init_file_obj(&" ^ file_obj ^ ");\n" ^
                                            file_obj ^ ".fp = fopen(" ^ filename ^
                                            ", " ^ filemode ^ ");\n"
                              | _ -> raise (Failure("codegen error with open_file(" ^
                                                    String.concat "," string_of_actuals ^
                                                    ") illegal argumets passed"))
                             in expr_string string_of_actuals

    | Call("fread_line", e) -> let file_obj = 
                                    let temp = List.map string_of_expr e 
                                    in List.hd temp
                               in
                               file_obj ^ ".readln((void * ) &" ^ 
                               file_obj ^ ");\n"
    | Call("freadn", e)    ->  let string_of_actuals = List.map string_of_expr e
                               in
                               let expr_string = function
                                   [file_obj; n] -> file_obj ^ ".readn((void * ) &" ^ 
                                                              file_obj ^ "," ^ n ^
                                                                ");\n"
                                   | _ -> raise (Failure("codegen error with freadn(" ^
                                                        String.concat "," string_of_actuals ^
                                                        ") illegal argumets passed"))
                               in expr_string string_of_actuals

    | Call("fwrite_str", e) -> let string_of_actuals = List.map string_of_expr e
                               in
                               let expr_string = 
                                   function
                                       [str; file_obj] -> file_obj ^ ".writestr(" ^
                                                          str ^ ", " ^ file_obj ^
                                                          ".fp);\n"
                                   | _ -> raise (Failure("codegen error with fwrite_str(" ^
                                                        String.concat "," string_of_actuals ^ 
                                                        ") illegal argumets passed"))
                               in expr_string string_of_actuals

    | Call("close_file", e) -> let file_obj = 
                                    let temp = List.map string_of_expr e 
                                    in List.hd temp
                               in
                               file_obj ^ ".close((void * ) &" ^ 
                               file_obj ^ ");\n"

    | Call("len",e)        -> "strlen(" ^ String.concat ","(List.map string_of_expr e) ^ ")"
    | Call("cmp",e)        -> "strcmp(" ^ String.concat ","(List.map string_of_expr e) ^ ")? 0:1"
    | Call("sleep",e)       -> "sleep(" ^ String.concat ","(List.map string_of_expr e) ^  ")"
    | Call("print_error",e) -> "perror(" ^ String.concat ","(List.map string_of_expr e) ^ ")"
    | Call("exit",e)        ->"exit(" ^ String.concat "," (List.map string_of_expr e)   ^ ")"
    | Call("free_list",e)   -> "removeAllNodes(" ^ String.concat "," (List.map string_of_expr e) ^")"
    | Call("rt_string",e)   -> "strdup(" ^ String.concat "," (List.map string_of_expr e) ^")"
    | Call(f, el) ->        f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    | Access(ln,n) ->       ln ^".cast("^ "accessL(&"^ ln ^ ".list," ^ string_of_int n ^"))"
    | Addleft(n,e) ->      "*PTR_ARRAY_FOR_LIST_"^ n ^ "="  ^string_of_expr e ^";\n" ^ "addLeft(&" ^ n ^".list,(void *)PTR_ARRAY_FOR_LIST_"^ n ^");\n"
                            ^"PTR_ARRAY_FOR_LIST_" ^ n ^ "++"
    | Addright(n,e) ->      "*PTR_ARRAY_FOR_LIST_"^ n ^ "="  ^string_of_expr e ^";\n" ^ "addRight(&" ^ n ^".list,(void *)PTR_ARRAY_FOR_LIST_"^ n ^");\n"
                            ^"PTR_ARRAY_FOR_LIST_" ^ n ^ "++"
    | Popleft(n) ->        "removeLeft(&"^n^".list)"
(*    | Popright(n) ->       "removeRight(&"^n^".list)"*)
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
  | Local (t,n,Noexpr) -> string_of_typ t ^ " " ^  n ^";\n"
  | Local(t,n,e) -> string_of_typ t ^" " ^ n ^" = " ^string_of_expr e^";\n"
  | List(t,n) -> "struct "^String.sub (string_of_typ t) 0 1^ "_list " ^ n ^ ";\n" ^ "initList(&"^ n ^ ".list);\n" ^ string_of_typ t ^" " ^"ARRAY_FOR_LIST_"^ n ^ "[100000];\n"
                 ^ string_of_typ t ^"* " ^ "PTR_ARRAY_FOR_LIST_"^ n ^ "=" ^ "&ARRAY_FOR_LIST_" ^ n ^ "[0];\n" ^n^".cast = "^String.sub (string_of_typ t) 0 1^"_cast;"
let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_formal (t,id) = string_of_typ t ^ " " ^ id

let string_of_global (t , id, e) = if e = Noexpr then
   string_of_typ t ^ " " ^ id ^";\n" else
   string_of_typ t ^ " " ^ id ^ "= "^ string_of_expr e ^ ";\n"


let string_of_http http = "if (!strcmp(" ^ http.httpArg1^ http.httpArg2 ^", userVariable)) { userResult = "^ String.sub http.httpArg3 1 (String.length(http.httpArg3)-2) ^ "();} else"


let construct_routing http_list = 
      String.concat "\n    " (List.map string_of_http http_list)


let string_of_pdecl_listen pdecl = 
"uv_tcp_t tcp_" ^ pdecl.pname ^ ";\n" ^
"struct sockaddr_in addr_" ^ pdecl.pname ^ ";\n" ^
"uv_work_t req_listen_" ^ pdecl.pname ^ ";\n" ^
"void post_listen_" ^ pdecl.pname ^ "(uv_work_t *req){
    char *token;
    char *method;
    char *route;
    char *protocol;

    token = strtok(((struct Backpack* ) (req->data))->data, \"\\n\");
    fprintf(stderr, \"%s\\n\", token);

    method = strtok(token, \" \");
    fprintf(stderr, \"method: %s\\n\", method);

    route = strtok(NULL, \" \");
    fprintf(stderr, \"route: %s\\n\", route);

    protocol = strtok(NULL, \" \");
    fprintf(stderr, \"protocol: %s\\n\", protocol);\n
    int methodLength = strlen(method);
        int routeLength = strlen(route);
            char userVariable[routeLength + methodLength + 1];
                strcpy(userVariable, method);
                    strcat(userVariable, route);
                        fprintf(stderr, \"userVariable: %s\\n\", userVariable); 
char* userResult = \"\";"^

(construct_routing (List.hd pdecl.listen).arg3)  ^ 
" { fprintf(stderr,\"no match\"); } \n 
	write_req_t *req_send = (write_req_t* ) malloc(sizeof(write_req_t));
    char *result = makeHTTP(userResult);
	req_send->buf = uv_buf_init(result, strlen(result));
    req_send->req.data = ((struct Backpack* ) (req->data))->client;
	uv_write((uv_write_t* ) &req_send->req, ((struct Backpack* ) (req->data))->client, &req_send->buf, 1, echo_write);
}


void onread_"^ pdecl.pname ^"(uv_stream_t *client, ssize_t nread, const uv_buf_t *buf) {
    if (nread > 0) {
        struct Backpack *backpack = (struct Backpack* ) malloc(sizeof(struct Backpack));
        backpack->data = buf->base;
        backpack->client = client;

        req_listen_"^pdecl.pname^".data = (void * ) backpack;
        uv_queue_work(loop, &req_listen_"^pdecl.pname^", post_listen_"^pdecl.pname^", after);
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

    uv_tcp_t *client = (uv_tcp_t * ) malloc(sizeof(uv_tcp_t));
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
    ignore(pdecl.pname);
    String.concat "\n   " (List.map string_of_stmt pdecl.body)

 
let string_of_pdecl pdecl = 
    (if ((List.length pdecl.listen) != 0) 
    then (string_of_pdecl_listen pdecl) 
    else "\n" ) ^ "\n" ^
    "void work_" ^ pdecl.pname ^
    "(uv_work_t *req) {    " ^ 
        (if ((List.length pdecl.listen) == 0) 
        then 
            (string_of_pdecl_no_listen pdecl) 
        else 
            "listen_" ^ 
            pdecl.pname ^ 
            "(" ^ 
            (List.hd pdecl.listen).arg1 ^ 
            ", " ^ 
            string_of_int (List.hd pdecl.listen).arg2 ^ 
            ");"  ^ 
            "\n" ^
            String.concat "\n    " (List.map string_of_stmt pdecl.body)) ^ 
    "\n}"
 

    let string_of_pdecl_main pdecl = 
        "    int data_" ^ pdecl.pname ^ ";\n" ^ 
        "    uv_work_t req_" ^ pdecl.pname ^ ";\n" ^
        "    req_" ^ pdecl.pname ^ ".data = (void * ) &data_" ^ pdecl.pname ^ ";\n" ^
        "    uv_queue_work(loop, &req_" ^ pdecl.pname ^ ", work_" ^ pdecl.pname ^ ", after);\n"

    let string_of_fdecl fdecl =
        string_of_typ fdecl.typ ^ " " ^
        fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_formal fdecl.formals) ^
        ") {\n" ^
        String.concat "    " (List.map string_of_stmt fdecl.body) ^
        "}\n"

    let string_of_sdecl sdecl =
        let svars = 
            let temp_vars = List.fold_left 
            (fun l (t, id, _) -> (t, id) :: l) [] sdecl.vars 
            in List.rev temp_vars
        in
        "struct " ^ sdecl.sname ^ " {\n    " ^
        String.concat "    " (List.map string_of_vdecl svars) ^
        "};\n"

    let translate (globals, stmts, funcs, pipes, structs) =
     
        "#include <stdio.h>\n#include <unistd.h>\n#include <uv.h>\n#include <stdlib.h>\n
        #include <string.h>\n#include <string.h>\n#include \"../stdlib/mylist.h\"\n#include \"../stdlib/strop.h\"\n"^ 
   "#define DEFAULT_PORT 7000
    #define DEFAULT_BACKLOG 128
    uv_loop_t *loop;
    int TEMP_FOR_ADD_LEFT;
    int TEMP_FOR_ADD_RIGHT;" ^

        String.concat "\n" (List.map string_of_global globals) ^ "\n" ^
        
        String.concat "\n" (List.map string_of_sdecl structs) ^ "\n" ^

"\nint i_cast(void* data){
    return *(int * )data;
}

float f_cast(void *data){
    return *(float * )data;
}

char * c_cast(void* data){
    return *(char ** )data;
}

char *header =
\"HTTP/1.0 200 OK\\n\"
\"Date: Fri, 31 Dec 1999 23:59:59 GMT\\n\"
\"\\n\";

char *makeHTTP(char *body) {
    int headerLength = strlen(header);
        int bodyLength = strlen(body);
    char *httpResult = (char* ) malloc(headerLength + bodyLength + 1);
    strcpy(httpResult, header);
    strcat(httpResult, body);

    return httpResult;
}


struct Backpack {
    uv_stream_t *client;
    char *data;
};

typedef struct {
    uv_write_t req;
    uv_buf_t buf;
} write_req_t;

void echo_write(uv_write_t *req, int status) {
    fprintf(stderr, \"I did print\\n\");
    uv_close((uv_handle_t * ) req->data, NULL);
    if (status) {
        fprintf(stderr, \"Write error %s\\n\", uv_strerror(status));
    }
    // free_write_req(req);
}


uv_loop_t *loop;

void alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
    buf->base = (char * ) malloc(suggested_size);
    buf->len = suggested_size;
}\n" ^ 

    
  	String.concat "\n\n" (List.map string_of_fdecl funcs) ^ "\n" ^
 
	"void after(uv_work_t *req, int status) { }\n\n" ^
  
  	String.concat "\n\n" (List.map string_of_pdecl pipes) ^ "\n\n" ^

  	"int main(int argc, char **argv) {\n    " ^
    "    loop = uv_default_loop();\n" ^

  	String.concat "\n    " (List.rev (List.map string_of_stmt stmts)) ^ "\n" ^
   
  	String.concat "\n" (List.map string_of_pdecl_main pipes) ^ "\n" ^
    "    Free_strs();\n" ^
   	"    return uv_run(loop, UV_RUN_DEFAULT);\n}\n"
