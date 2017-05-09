(* Semantic checker for the Pipeline language *)

open Ast

module StringMap = Map.Make(String)

module StringSet = Set.Make(String)

type struct_info = 
    {
        info_sdecl       : struct_decl;
        info_vars        : typ StringMap.t;
    }

type env = 
    {
        env_name        : string;
        env_funcs       : func_decl StringMap.t;
        env_structs     : struct_info StringMap.t;
        env_pipes       : pipe_decl StringMap.t;
        env_locals      : typ StringMap.t;
        env_parameters  : typ StringMap.t;
        env_globals     : typ StringMap.t;
        env_in_block    : bool;
        env_check_strct : typ StringMap.t; 
        env_reserved    : func_decl StringMap.t;
    }

(* Makes a list of fdecls for all reserved functions *)
let reserved_funcs = 
    StringMap.add "print_str" {
        typ = Void;
        fname = "print_str";
        formals = [(MyString, "x")];
        body = [];
        }(
    StringMap.add "print_int" {
            typ     = Void;
            fname   = "print_int";
            formals = [(Int, "x")];
            body    = [];
        }(
    StringMap.add "print_float" {
            typ     = Void;
            fname   = "print_float";
            formals = [(Float, "x")];
            body    = [];
        }(
    StringMap.add "print_bool" {
            typ     = Void;
            fname   = "print_bool";
            formals = [(Bool, "x")];
            body    = [];
        }(
    StringMap.add "len" {
            typ     = Int;
            fname   = "len";
            formals = [(MyString, "x")];
            body    = [];
        }(
    StringMap.add "cmp" {
            typ     = Bool;
            fname   = "cmp";
            formals = [(MyString, "x"); (MyString, "y")];
            body    = [];
        }(
    StringMap.add "fread_line" {
            typ     = MyString;
            fname   = "fread_line";
            formals = [(File, "file_obj")];
            body    = [];
        }(
    StringMap.add "freadn" {
            typ     = MyString;
            fname   = "freadn";
            formals = [(File, "file_obj"); (Int, "n")];
            body    = [];
        }(
    StringMap.add "fwrite_str" {
            typ     = Void;
            fname   = "fwrite_str";
            formals = [(MyString, "s"); (File, "file_obj")];
            body    = [];
        }(
    StringMap.add "close_file" {
            typ     = Void;
            fname   = "close_file";
            formals = [(File, "file_obj")];
            body    = [];
        }(
    StringMap.add "sleep" {
            typ     = Void;
            fname   = "sleep";
            formals = [(Int, "x")];
            body    = [];
        }(
    StringMap.add "print_error" {
            typ     = Void;
            fname   = "print_error";
            formals = [(MyString, "x")];
            body    = [];
        }(
    StringMap.add "exit" {
            typ     = Void;
            fname   = "exit";
            formals = [(Int, "x")];
            body    = [];
        }(
    StringMap.singleton "init_file_obj" {
            typ     = Void;
            fname   = "init_file_obj";
            formals = [(File, "x"); (MyString, "file_name"); (MyString, "mode")];
            body = [];
        }
    )))))))))))))

let init_struct_info map sdecl = 
    let st_info = 
        let var_map =  
            List.fold_left (fun map (t, id, _) -> StringMap.add id t map) 
                                                StringMap.empty sdecl.vars
        in
        {
            info_sdecl           = sdecl;
            info_vars            = var_map;
        }
    in StringMap.add sdecl.sname st_info map

let update_env_structs env struct_info struct_name  = 
    {
        env_name        = env.env_name;
        env_funcs       = env.env_funcs;
        env_structs     = StringMap.add struct_name struct_info env.env_structs;
        env_pipes       = env.env_pipes;
        env_locals      = env.env_locals;
        env_parameters  = env.env_parameters;
        env_globals     = env.env_globals;
        env_in_block    = env.env_in_block;
        env_check_strct = env.env_check_strct; 
        env_reserved    = env.env_reserved;
    }

let update_env_name env new_name = 
    {
        env_name        = new_name;
        env_funcs       = env.env_funcs;
        env_structs     = env.env_structs;
        env_pipes       = env.env_pipes;
        env_locals      = env.env_locals;
        env_parameters  = env.env_parameters;
        env_globals     = env.env_globals;
        env_in_block    = env.env_in_block;
        env_check_strct = env.env_check_strct;
        env_reserved    = env.env_reserved;
    }

let update_call_stack env in_block = 
    {
        env_name        = env.env_name;
        env_funcs       = env.env_funcs;
        env_structs     = env.env_structs;
        env_pipes       = env.env_pipes;
        env_locals      = env.env_locals;
        env_parameters  = env.env_parameters;
        env_globals     = env.env_globals;
        env_in_block    = in_block;
        env_check_strct = env.env_check_strct;
        env_reserved    = env.env_reserved;
    }

let update_globals env global_map = 
    {
        env_name        = env.env_name;
        env_funcs       = env.env_funcs;
        env_structs     = env.env_structs;
        env_pipes       = env.env_pipes;
        env_locals      = env.env_locals;
        env_parameters  = env.env_parameters;
        env_globals     = env.env_globals;
        env_in_block    = env.env_in_block;
        env_check_strct = env.env_check_strct; 
        env_reserved    = env.env_reserved;
    }

let update_locals env typ_t id_t = 
    {
        env_name        = env.env_name;
        env_funcs       = env.env_funcs;
        env_structs     = env.env_structs;
        env_pipes       = env.env_pipes;
        env_locals      = StringMap.add id_t typ_t env.env_locals;
        env_parameters  = env.env_parameters;
        env_globals     = env.env_globals;
        env_in_block    = env.env_in_block;
        env_check_strct = env.env_check_strct; 
        env_reserved    = env.env_reserved;
    }

let update_formals env formals = 
    let formal_map = List.fold_left 
                         (fun map (t, id) -> StringMap.add id t map)
                             StringMap.empty formals
    in
    {
        env_name        = env.env_name;
        env_funcs       = env.env_funcs;
        env_structs     = env.env_structs;
        env_pipes       = env.env_pipes;
        env_locals      = env.env_locals;
        env_parameters  = formal_map;
        env_globals     = env.env_globals;
        env_in_block    = env.env_in_block;
        env_check_strct = env.env_check_strct; 
        env_reserved    = env.env_reserved;
    }

let update_struct_check env var_map = 
    {
        env_name        = env.env_name;
        env_funcs       = env.env_funcs;
        env_structs     = env.env_structs;
        env_pipes       = env.env_pipes;
        env_locals      = env.env_locals;
        env_parameters  = env.env_parameters;
        env_globals     = env.env_globals;
        env_in_block    = env.env_in_block;
        env_check_strct = var_map; 
        env_reserved    = env.env_reserved;
    }


       
let find_var env id =
    if StringMap.mem id env.env_parameters 
    then StringMap.find id env.env_parameters
    else if StringMap.mem id env.env_locals
    then StringMap.find id env.env_locals
    else if StringMap.mem id env.env_globals
    then StringMap.find id env.env_globals
    else if StringMap.mem id env.env_check_strct
    then StringMap.find id env.env_check_strct
    else raise Not_found

let check (globals, stmts, functions, pipes, structs) =
  let report_duplicate exceptf list =
    let rec helper = function
	n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void_global exceptf = function
      (Void, n, _) -> raise (Failure (exceptf n))
    | _ -> ()
  in

  let check_not_void exceptf = function
     (Void,n)-> raise (Failure (exceptf n))
    | _ -> () 
  in
  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise err
  in
   
  (**** Checking Global Variables ****)

  List.iter (check_not_void_global (fun n -> "illegal void global " ^ n)) globals;
   
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map (fun (a,b,c) -> b ) globals);


  (**** Checking Functions ****)

  
(********** need to fix this so it checks for builtins ***************)
(*  let builtins_and_funcs = 
      let func_list = List.map (fun fd -> fd.fname) functions 
      in builtins_list @ func_list *)
  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

let fdecls = List.fold_left 
                    (fun m fd -> StringMap.add fd.fname fd m) 
                        StringMap.empty functions
in
let global_map = List.fold_left
                 (fun map (t, id, _) -> StringMap.add id t map)
                    StringMap.empty globals
in
let struct_map = 
    List.fold_left 
                 (fun map sd -> init_struct_info map sd)
                      StringMap.empty structs
in
    let init_env func = if StringMap.mem func.fname reserved_funcs 
                        then raise (Failure("duplicate"))
                        else print_string "no...\n";
        let formals = List.fold_left (fun map (t, id) -> print_string id; StringMap.add id t map)
                                     StringMap.empty func.formals 
        in
        {
            env_name            = func.fname;
            env_funcs           = fdecls;
            env_structs         = struct_map;
            env_pipes           = StringMap.empty;
            env_locals          = formals;
            env_parameters      = formals;
            env_globals         = global_map;
            env_in_block        = false;
            env_check_strct     = StringMap.empty;
            env_reserved        = reserved_funcs;
        }
    in
  let find_fdecl env s = try StringMap.find s env.env_funcs
       with Not_found -> try StringMap.find s env.env_reserved
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let check_function env func =
    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    let get_ID_typ env s =
      try find_var env s
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let is_defined env s =
        if StringMap.mem s env.env_parameters then true
        else if StringMap.mem s env.env_locals then true
        else if StringMap.mem s env.env_globals then true
        else false
    in
    let rec expr env = 
        let check_list_typ operation s = 
            let list_typ = get_ID_typ env s 
            in
            match list_typ with
            List_t(t) -> t
            | _ -> raise (Failure ("illegal argument passed in "^ operation ^
                                   ", " ^ s ^" should be of type List " ^
                                   "but instead instead is of type "^
                                   string_of_typ list_typ ^ "."))
        in

        let list_op_test op_name list_id e =
            let list_typ = get_ID_typ env list_id in
            let t2 = expr env e in
            let t1 = 
                match list_typ with
                List_t(t) -> t
                | _ -> raise (Failure ("illegal argument in " ^ op_name ^ "(" ^
                                       list_id ^", " ^ string_of_expr e ^") "^
                                       list_id ^" has type " ^ 
                                       string_of_typ list_typ ^ 
                                       " but should be of type" ^
                                        string_of_typ t2 ^ 
                                       " List."))
                  in if t1 = t2 
                     then list_typ
                     else raise (Failure ("illegal argument in " ^ op_name ^ "(" ^
                                           list_id ^", " ^ string_of_expr e ^") "^
                                           string_of_expr e ^" has type " ^ 
                                           string_of_typ t2 ^ 
                                           " but should be of type" ^
                                           string_of_typ t1))
        in 
        function
	Literal _ -> Int
      | FloatLit _ -> Float
      | BoolLit _ -> Bool
      | MyStringLit _ -> MyString
      | Id s -> (*print_string "in ID";*) get_ID_typ env s
      | Binop(e1, op, e2) as e -> 
              let t1 = expr env e1 and t2 = expr env e2 in
                let match_helper (t1,t2) = 
                    match (t1,t2) with
                    (Int, Int) -> Int
                    | (Float, Float) -> Float
                    | (Int, Float) -> Float
                    | (Float, Int) -> Float
                    | (_ , _) -> raise ((Failure ("illegal binary operator " ^
                              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                              string_of_typ t2 ^ " in " ^ string_of_expr e)))
                in
    	(match op with
        | Add ->     
                match_helper (t1,t2) 
        | Sub -> match_helper (t1,t2)
        | Mult -> match_helper (t1,t2)
        | Div -> match_helper (t1,t2)
    	| Equal | Neq when t1 = t2 -> Bool
	    | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
    	| And | Or when t1 = Bool && t2 = Bool -> Bool
        | _ -> raise (Failure ("illegal binary operator " ^
                      string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                      string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Unop(op, e) as ex -> let t = expr env e in
	 (match op with
       Neg -> if t = Int || t = Float 
              then t 
              else raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex))
	 | Not when t = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = get_ID_typ env var (*type_of_identifier var*)
                                and rt = expr env e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				     " = " ^ string_of_typ rt ^ " in " ^ 
				     string_of_expr ex))
      | Call(fname, actuals) as call -> 
              let fd = find_fdecl env fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 (fun (ft, _) e -> let et = expr env e in
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
             fd.formals actuals;
           fd.typ
               (*need to check the list type*)  
       (*| Struct(s) -> print_string "in Struct(s) expr"; print_string s; print_string "\n";
                      if StringMap.mem s env.env_structs
                      then Struct(s)
                      else raise Not_found *)
      (****** list_op_test op_name list_id e  ***************************)
       | Addleft(s, e) -> list_op_test "addleft" s e
       | Addright(s, e) -> list_op_test "addleft" s e
       | Popleft(s) -> let operation = "popleft(" ^ s ^")" in 
                       check_list_typ operation s 
       | Popright(s) -> let operation = "popright(" ^ s ^")" in
                        check_list_typ operation s 
       | Access(list_id, number) -> let operation = list_id ^ "[" 
                                        ^ string_of_int number ^ "]"
                                    in check_list_typ operation list_id
                      (* let list_typ = get_ID_typ env s in
                           match list_typ with
                           List_t(t) -> t
                         | _ -> raise (Failure ("illegal argument passed in popleft(" ^
                                          s ^ "), " ^ s ^" should be of type list " ^
                                          "but instead instead is of type "^
                                          string_of_typ list_typ ^ ".")) *)
                                           
       | StructAccess(struct_name, var_name) -> 
               (*print_string "in StructAccess\n";
               print_string "after raise_error\n";*)
               let struct_info = 
                   let struct_t = expr env struct_name in
               match struct_t with
               Struct(s) -> (*print_string "in Struct(s) "; print_string s; print_string "\n";*)
                            if StringMap.mem s env.env_structs
                            then StringMap.find s env.env_structs
                            else raise Not_found
               | _ -> raise (Failure("illegal argument found " ^
                             string_of_typ struct_t ^ " must be a struct " ^
                             "type for argument in expr " ^ 
                             string_of_expr struct_name ^
                             "." ^ string_of_expr var_name))
               in (*print_string(string_of_expr(var_name)); print_string "\n";*)
             (*  let var_t = expr env var_name 
               in *)
               let senv = update_struct_check env struct_info.info_vars in
               expr senv var_name
        | Concat(s1, s2) -> let t1 = expr env s1 and t2 = expr env s2 in
                            match (t1, t2) with
                            (MyString, MyString) -> MyString
                            | (_ , _) -> raise ((Failure ("illegal concatenation operator " ^
                                                 string_of_typ t1 ^ " $  " ^
                                                 string_of_typ t2 ^ " in " ^ 
                                                 string_of_expr s1 ^ " $ " ^
                                                 string_of_expr s2)))

    in
    let check_bool_expr env e = if expr env e != Bool
     then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
     else () in
    (*
    let duplicate_env env = 
    {
        env_name        = env.env_name;
        env_funcs       = env.env_funcs;
        env_structs     = env.env_structs;
        env_pipes       = env.env_pipes;
        env_locals      = env.env_locals;
        env_parameters  = env.env_parameters;
        env_globals     = env.env_globals;
        env_in_block    = env.env_in_block;
        env_check_strct = env.env_check_strct;
        env_reserved    = env.env_reserved;
    }
    in
    *)
    let curr_env = ref env
    in
(*    let check_dup_var env s = if StringMap.find *)
(*
    let rec check_struct env struct_info var = 
        match var with
        StructAccess(s, e)  -> if StringMap.mem s struct_info.vars
                               then 
                                   let curr_info = StringMap.find s struct_info.vars in

                                   check_struct env curr_info e
                               else raise Not_found
(*        | Call(fname, actuals)-> if StringMap.mem fname struct_info.vars
                               then  StringMap.find fname struct_info.vars in
                               if StringMap.mem real_fname *)
       | ID s -> if StringMap.mem s struct_info.vars 
                 then StringMap.find s struct_info.vars
                 else raise Not_found 
    in*)
    (* Verify a statement or throw an exception *)
    let rec stmt env =        
        function
    	Block sl -> let rec check_block block_env old_env=
            function
               [Return _ as s] -> stmt block_env s
             | Return _ :: _ -> raise (Failure "nothing may follow a return")
             | Block sl::ss -> check_block block_env old_env (sl @ ss)
             | s :: ss -> stmt block_env s; 
                          check_block block_env old_env ss
             | [] -> ignore(curr_env := old_env)
        in 
        let block_env = update_call_stack !curr_env true in
        check_block block_env !curr_env sl

      | Expr e -> (*print_string "Expr_stmt\n"; 
                  if StringMap.mem "i" !curr_env.env_locals 
                  then print_string "true" else print_string "false";*)
                  ignore(expr !curr_env e) 
      | Return e -> let t = expr !curr_env e in if t = func.typ then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
           
      | If(p, b1, b2) -> let block_env = update_call_stack !curr_env true in
                        check_bool_expr block_env p; 
                        stmt block_env b1; 
                        stmt block_env b2
      | For(e1, e2, e3, st) -> let block_env = update_call_stack env true in
                               ignore (expr !curr_env e1); 
                               check_bool_expr !curr_env e2;
                               ignore (expr !curr_env e3); stmt block_env st
      | While(p, s) -> let block_env = update_call_stack !curr_env true in
                                       check_bool_expr !curr_env p;
                                       stmt block_env s
      | Local(t,id,Noexpr) -> if is_defined !curr_env id then raise 
                                    (Failure("variable "^ id ^ " is a duplicate in scope "
                                     ^ env.env_name ^ "."))
                         else
                             curr_env := update_locals !curr_env t id(*;
                             ignore(expr !curr_env e)*)
      | Local(t,id,e) -> let uninitializable = function
                            Struct(_) -> true
                            | File -> true
                            | _ -> false
                         in
                         if is_defined !curr_env id then raise 
                                    (Failure("variable "^ id ^ " is a duplicate in scope "
                                     ^ env.env_name ^ "."))
                         else if uninitializable t 
                         then raise (Failure("variable "^ id ^ " is of type " ^
                                             string_of_typ t ^ " which cannot be " ^
                                             "initialized and declared at the " ^
                                             "time."))
                         else
                             curr_env := update_locals !curr_env t id;
                             ignore(expr !curr_env e)
      (*| Add_left(e1, e2) -> ignore(expr e1); ignore(expr e2)
      | Add_right(e1, e2) -> ignore(expr e1); ignore(expr e2)
      | Find_node(e1, e2, e3) -> ignore(expr e1); ignore(expr e2); 
                                 ignore(expr e3)
      *)
     (* | Http(e1, e2, e3) -> ignore(expr !curr_env e1); ignore(expr !curr_env e2)
      | Http(e1, e2, e3) -> let t1 = expr !curr_env e1 
                            and t2 = expr !curr_env e2
                            and t3 = expr !curr_env e3
                            in
                (match (t1, t3, t3) with
                    (MyString, MyString, MyString) -> ()
     *)
                    
      (*| Int_list_decl(_,_) -> ()
      | Str_list_decl(_,_) -> ()*)
(*      | List(t,id) -> ignore(update_locals !curr_env t id)*)
      | List(t,id) -> if is_defined !curr_env id then raise 
                                    (Failure("variable "^ id ^ " is a duplicate in scope "
                                     ^ env.env_name ^ "."))
                         else
                             curr_env := update_locals !curr_env t id(*;
                             ignore(expr !curr_env e)*)
      (* listen takes a string and int -> check if positive and under max
       * http takes e1) string -> http method 'get' post' 'put' 'delete'
       *            e2) route -> string start with the "/"
       *            e3) string and translate to a func pointer -> name of a function
       * 
       * 
       * *)
    in
    stmt env (Block func.body)
  in
  let pdecl_to_fdecl p =
      {
          typ = Void;
          fname = p.pname;
          formals = [];
          body = p.body;
      }
  in
  let pipe_list = List.fold_left (fun l p -> pdecl_to_fdecl(p) :: l) [] pipes in
  (*
  let stmt_strings = List.fold_left (fun l s -> string_of_stmt s :: l) ["*********\n"] stmts in
  print_string "\n************\n";
  List.iter print_string stmt_strings;
  *)
(*  let printme = print_string "\n\n*****************\n\n" in*)
List.iter (fun f -> check_function (init_env f) f) (functions @ pipe_list);
let locals_map = 
    let helper s = 
        (*let rstmt_str = "rejected " ^ string_of_stmt s ^"\n" in
        let astmt_str = "found " ^ string_of_stmt s ^"\n" in*)
        match s with
        Local(_, _, _) -> (*print_string astmt_str;*)true
      | List(_,_) ->  (*print_string astmt_str;*)true
      | _ -> (*print_string rstmt_str ;*) false
    in
    let helper2 s =
        match s with
        Local(t, id, _) -> (id, t)
      | List(t, id) ->  (id, t)
      | _ -> ("", Void)
    in
    List.fold_left (fun m s -> if  helper s 
                               then let kv = helper2 s in 
                               (fun (id, t) -> StringMap.add id t m ) kv
                               else m) StringMap.empty stmts
in
let other_stmts = 
    let helper s = 
        (*let rstmt_str = "other rejected " ^ string_of_stmt s ^"\n" in
        let astmt_str = "other found " ^ string_of_stmt s ^"\n" in*)
        match s with
        Local(_, _, _) -> (*print_string rstmt_str;*)false
      | List(_,_) -> (* print_string rstmt_str;*)false
      | _ -> (*print_string astmt_str ;*) true
    in
    let temp = List.fold_left (fun l s -> if helper s then s :: l else l) [] stmts
    in List.rev temp
in
(*
let locals_stmts = 
    let helper s = let rstmt_str = "other rejected " ^ string_of_stmt s ^"\n" in
        let astmt_str = "other found " ^ string_of_stmt s ^"\n" in
        match s with
        Local(_, _, _) -> (*print_string rstmt_str;*)true
      | List(_,_) -> (* print_string rstmt_str;*)true
      | _ -> (*print_string astmt_str ;*) false
    in
    List.fold_left (fun l s -> if helper s then s :: l else l) [] stmts
in
let stmt_strings = List.fold_left (fun l s -> string_of_stmt s :: "\n" :: l) [] locals_stmts in
List.iter (fun s -> print_string s) stmt_strings;
*)
let main = 
    (*let stmt_strings =
        let temp_list =List.fold_left (fun l s -> string_of_stmt s :: l) [] stmts 
        in temp_list
    in*)
(*    List.iter print_string stmt_strings;*)
      {
          typ = Int;
          fname = "main";
          formals = [(Int, "argc"); (List_t(MyString), "argv")];
          body = other_stmts;
      }
in
let main_env = 
    {
        env_name            = "main";
        env_funcs           = fdecls;
        env_structs         = struct_map;
        env_pipes           = StringMap.empty;
        env_locals          = locals_map;
        env_parameters      = StringMap.empty;
        env_globals         = global_map;
        env_in_block        = false;
        env_check_strct     = StringMap.empty;
        env_reserved        = reserved_funcs;
    }
in
(*print_string "omg" *)
check_function main_env main

(*check_function (init_env main.fname) main;*)
(*let seed_env = init_env "main" in*)
(*** plan is to add the locals to the env while removing them from the stmts list **)
(*let init_main_env name = 
    let main_vars env stmt =
      | Local(t,id,e) -> if StringMap.mem  then raise 
                                    (Failure("variable "^ id ^ " is a duplicate in scope "
                                     ^ env.env_name ^ "."))
                         else
                             curr_env := update_locals !curr_env t id;
                             ignore(expr !curr_env e)
    {
        env_name            = name;
        env_funcs           = fdecls;
        env_structs         = struct_map;
        env_pipes           = StringMap.empty;
        env_locals          = StringMap.empty;
        env_parameters      = StringMap.empty;
        env_globals         = global_map;
        env_in_block        = false;
        env_check_strct     = StringMap.empty;
        env_reserved        = reserved_funcs;
    }
in
check_function (init_env "main") { typ = Void; fname = "main"; 
                                   formals = []; body = stmts;};
  (* List -> check the elements in a list *)*)
