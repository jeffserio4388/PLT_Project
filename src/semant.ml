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
    StringMap.add "print_unbuf" {
        typ = Void;
        fname = "print_unbuf";
        formals = [(MyString, "x")];
        body = [];
        }(
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
    ))))))))))))))

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
    if typ_t = Void then raise (Failure(id_t ^" is an illegal void type variable")) else ();
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
                        then raise (Failure("illegal function definition. " ^
                                             "function " ^ func.fname ^
                                             "is the name of a built_in function"))
                        else ();
        let formals = List.fold_left (fun map (t, id) ->  StringMap.add id t map)
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
      | Id s -> get_ID_typ env s
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
      | Assign(var, e) as ex -> let lt = get_ID_typ env var 
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
       | Addleft(s, e) -> list_op_test "addleft" s e
       | Addright(s, e) -> list_op_test "addleft" s e
       | Popleft(s) -> let operation = "popleft(" ^ s ^")" in 
                       check_list_typ operation s 
       | Popright(s) -> let operation = "popright(" ^ s ^")" in
                        check_list_typ operation s 
       | Access(list_id, number) -> let operation = list_id ^ "[" 
                                        ^ string_of_int number ^ "]"
                                    in check_list_typ operation list_id
       | StructAccess(struct_name, var_name) -> 
               let struct_info = 
                   let struct_t = expr env struct_name in
               match struct_t with
               Struct(s) -> 
                            if StringMap.mem s env.env_structs
                            then StringMap.find s env.env_structs
                            else raise Not_found
               | _ -> raise (Failure("illegal argument found " ^
                             string_of_typ struct_t ^ " must be a struct " ^
                             "type for argument in expr " ^ 
                             string_of_expr struct_name ^
                             "." ^ string_of_expr var_name))
               in 
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
    let curr_env = ref env
    in
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

      | Expr e -> ignore(expr !curr_env e) 
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
      | Local(t,id,Noexpr) -> 
                         if is_defined !curr_env id then raise 
                                    (Failure("variable "^ id ^ " is a duplicate in scope "
                                     ^ env.env_name ^ "."))
                         else
                             curr_env := update_locals !curr_env t id
      | Local(t,id,e) -> let uninitializable = function
                            Struct(_) -> true
                            | File -> true
                            | _ -> false
                         in
                         if is_defined !curr_env id || t = Void then raise 
                                    (Failure("variable "^ id ^ " is a duplicate in scope "
                                     ^ env.env_name ^ "."))
                         else if uninitializable t 
                         then raise (Failure("variable "^ id ^ " is of type " ^
                                             string_of_typ t ^ " which cannot be " ^
                                             "initialized and declared at the " ^
                                             "time."))
                         else
                             curr_env := update_locals !curr_env t id;
                             let t1 = expr !curr_env e in
                             if t == t1 then ()
                             else raise (Failure("illegal assignment in " ^
                                         string_of_typ t ^ " " ^ id ^ " = " ^
                                         string_of_expr e ^ " of type " ^ 
                                         string_of_typ t1))
      | List(t,id) -> if is_defined !curr_env id then raise 
                                    (Failure("variable "^ id ^ " is a duplicate in scope "
                                     ^ env.env_name ^ "."))
                         else
                             curr_env := update_locals !curr_env t id
    in
    stmt env (Block func.body)
  in
  let listen_check l = 
        let helper arg = 
            if StringMap.mem arg fdecls then ()
            else if StringMap.mem arg reserved_funcs then ()
            else raise (Failure("illegal http function argument " ^ arg ))
        in
        List.iter (fun h ->  helper h.httpArg3) l.arg3
  in               
  let pdecl_to_fdecl p =
      List.iter listen_check p.listen;
      {
          typ = Void;
          fname = p.pname;
          formals = [];
          body = p.body;
      }
  in
  let pipe_list = List.fold_left (fun l p -> pdecl_to_fdecl(p) :: l) [] pipes in
List.iter (fun f -> check_function (init_env f) f) (functions @ pipe_list);
let locals_map = 
    let helpervoid = function
        Void -> raise (Failure("illegal void type found"))
      | _ -> true
    in
    let helper s = 
        match s with
        Local(t, _, _) -> helpervoid t
      | List(_,_) -> true
      | _ -> false
    in
    let helper2 s =
        match s with
        Local(t, id, Noexpr) -> (id, t)
      | List(t, id) ->  (id, t)
      | _ -> ("", Void)
    in
    List.fold_left (fun m s -> if  helper s 
                               then let kv = helper2 s in 
                               (fun (id, t) -> StringMap.add id t m ) kv
                               else m) StringMap.empty stmts
in
let other_stmts = 
    let helpervoid = function
        Void -> raise (Failure("illegal void type found"))
      | _ -> true
    in
    let helper s = 
        match s with
        Local(_, _, Noexpr) -> false
      | List(_,_) -> false
      | Local(t, _, _) -> helpervoid t
      | _ -> true
    in
    let temp = List.fold_left (fun l s -> if helper s then s :: l else l) [] stmts
    in List.rev temp
in
let main = 
    
      let helper = function
          Block(_) -> ()
        | _ -> raise (Failure("illegal syntax, statement found outside of a "^
                              "block. All statements in main must be" ^
                              " either global, a function a pipe or in a block" ^
                               " inside of braces"))
      in
      List.iter helper stmts;
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
check_function main_env main
