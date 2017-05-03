(* Semantic checker for the Pipeline language *)
(* TODO:
 *      Struct checking
 *      List checking
 *      Figure how to check them as types in typ
 *      Listen, Http stuff
 * *)

open Ast

module StringMap = Map.Make(String)

module StringSet = Set.Make(String)

type env = 
    {
        env_name        : string;
        env_funcs       : func_decl StringMap.t;
        env_structs     : struct_decl StringMap.t;
        env_pipes       : pipe_decl StringMap.t;
        env_locals      : typ StringMap.t;
        env_parameters  : typ StringMap.t;
        env_globals     : typ StringMap.t;
        env_in_block    : bool;
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
    StringMap.add "open" {
            typ     = File;
            fname   = "open";
            formals = [(MyString, "file_name"); (MyString, "mode")];
            body = [];
        }(
    StringMap.add "open_file" {
            typ     = File;
            fname   = "fclose";
            formals = [(File , "file_object")];
            body = [];
        }( (*
    StringMap.add "writeln" {
            typ     = int;
            fname   = "close";
            formals = [(MyString, "input_string"), (File , "file_object")];
            body = [];
        }(
    StringMap.add "readln" {
            typ     = MyString;
            fname   = "fclose";
            formals = [(File , "file_object")];
            body = [];
        }(
    StringMap.add "write_str" {
            typ     = Void;
            fname   = "write_str";
            formals = [(File, "file_object"); (MyString,"string")];
            body    = [];
        }( *)
    StringMap.add "addLeft" {
            typ     = Void;
            fname   = "addLeft";
            formals = [(List, "x")];
            body = [];
        }(
    StringMap.add "addRight" {
            typ     = Void;
            fname   = "addRight";
            formals = [(List, "x")];
            body = [];
        }(
    StringMap.add "popLeft" {
            typ     = Void;
            fname   = "popLeft";
            formals = [(List, "x")];
            body = [];
        }(
    StringMap.singleton "popRight" {
            typ     = Void;
            fname   = "popRight";
            formals = [(List, "x")];
            body = [];
        }
    )))))))))

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
        env_reserved    = env.env_reserved;
    }

let find_var env id = 
    if StringMap.mem id env.env_parameters 
    then StringMap.find id env.env_parameters
    else if StringMap.mem id env.env_locals 
    then StringMap.find id env.env_locals
    else if StringMap.mem id env.env_globals
    then StringMap.find id env.env_globals
    else raise Not_found

let check (globals, stmts, functions, pipes, structs) = 
(*    if List.length stmts < 1 
    then raise (Failure ("You must have at least 1 statement."))
    else ();*)
  (* Raise an exception if the given list has a duplicate *)
  (* let stmt_strings = List.fold_left (fun l s -> string_of_stmt (s) :: l) ["*********\n"] stmts in
  print_string "\n************\n";
  List.iter print_string stmt_strings; *)
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

  
  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
    let built_in_decls =  StringMap.singleton "printf"
            { 
                typ = Void; 
                fname = "printf"; 
                formals = [(MyString, "x")]; 
                body = [] 
            }
   in
     
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in
    let main = 
          {
              typ = Int;
              fname = "main";
              formals = [(Int, "argc"); (List, "argv")];
              body = stmts;
          }
    in
   
    let init_env name = 
        let fdecls = List.fold_left 
                            (fun m fd -> StringMap.add fd.fname fd m) 
                                StringMap.empty functions
        in
        let global_map = List.fold_left
                         (fun map (t, id, _) -> StringMap.add id t map)
                            StringMap.empty globals
        in
        {
            env_name        = name;
            env_funcs       = fdecls;
            env_structs     = StringMap.empty;
            env_pipes       = StringMap.empty;
            env_locals      = StringMap.empty;
            env_parameters  = StringMap.empty;
            env_globals     = global_map;
            env_in_block    = false;
            env_reserved    = reserved_funcs;
        }
    in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  let find_fdecl env s = try StringMap.find s env.env_funcs
       with Not_found -> try StringMap.find s env.env_reserved
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  (*let _ = function_decl "main" in (* Ensure "main" is defined *)*)

  let check_function env func =
      let old_env = env in
      let cur_env = (fun e -> update_formals e func.formals) (update_call_stack env false) in

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    (*  
    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;
    *)

    (*
    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);
    *)
    (* Type of each variable (global, formal, or local) *)
    let symbols =  
        let global_pair  = List.map (fun (a,b,c) -> (a,b)) globals in 
        List.fold_left (fun m (t, n) -> StringMap.add n t m) 
	StringMap.empty (global_pair @ func.formals)
    in
    
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let get_ID_typ env s =
        (*print_string "in get_ID_typ\n"; 
        if StringMap.mem s env.env_locals then print_string "true\n" else print_string "false\n";
        if StringMap.mem s env.env_parameters then print_string "true" else print_string "false\n";
        if StringMap.mem s env.env_globals then print_string "true" else print_string "false\n"; *)
      try find_var env s
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let is_defined env s =
        if StringMap.mem s env.env_parameters then true
        else if StringMap.mem s env.env_locals then true
        else if StringMap.mem s env.env_globals then true
        else false
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr env = 
        let match_helper (t1,t2) = 
            match (t1,t2) with
            (Int, Int) -> Int
            | (Float, Float) -> Float
            | (Int, Float) -> Float
            | (Float, Int) -> Float
        in
        function
	Literal _ -> Int
      | FloatLit _ -> Float
      | BoolLit _ -> Bool
      | MyStringLit _ -> MyString
      | Id s -> (*print_string "in ID";*) get_ID_typ env s
      | Binop(e1, op, e2) as e -> 
              let t1 = expr env e1 and t2 = expr env e2 in
    	(match op with
(*          Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int*)
        | Add -> if t1 = MyString && t2 = MyString then MyString
                 else match_helper (t1,t2) 
                (*if t1 = Int && t2 = Int then Int
                 else if (t1 = Int || t1 = Float) && (t2 = Int || t2 = Float) then Float
                 else raise Not_found*)
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
	   Neg when t = Int -> Int
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
              let fd = find_fdecl env fname (*function_decl fname*) in
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
       | Access(list_name, number) -> Int(*need to check the list type*)  
    in
    let check_bool_expr env e = if expr env e != Bool
     then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
     else () in
    
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
        env_reserved    = env.env_reserved;
    }
    in 
    let curr_env = ref env
    in
(*    let check_dup_var env s = if StringMap.find *)
    
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
      | Local(t,id,e) -> if is_defined !curr_env id then raise 
                                    (Failure("variable "^ id ^ " is a duplicate in scope "
                                     ^ env.env_name ^ "."))
                         else
                            (*print_string "in local\n";*)
                             curr_env := update_locals !curr_env t id;
                             (*if StringMap.mem id !curr_env.env_locals
                             then print_string "true\n" else print_string "false\n";*)
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
      | List(t,id) -> ignore(update_locals !curr_env t id)
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
  List.iter (fun f -> check_function (init_env f.fname) f) (functions @ [main] @ pipe_list)
  (* List -> check the elements in a list *)
