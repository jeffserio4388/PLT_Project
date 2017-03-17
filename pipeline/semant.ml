(* Semantic checker for Pipeline *)

open Ast

module StringMap = Map.Make(String)

let check (globals, functions) = 
    let report_duplicate exceptf list =
        let rec helper = function
            n1::n2::_ when n1 = n2 -> raise (Failure (exceptf n1))
            | _ :: t -> helper t
            | [] -> ()
        in helper (List.sort compare list)
    in
    (* Not sure if this is desirable in our language *)
    let check_not_void exceptf = function
        (Void, n) -> raise (Failure (except n))
        | _ -> ()
    in
    let check_assign lvaluet rvaluet err =
        if lvaluet == rvaluet then lvaluet else raise err
    in
    (* Checking globals *)
    List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
    report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);
    (* Checking Functions *)
    if List.mem "print" (List.map (fun fd -> fd.fname) functions) (*not sure this is necessary*)
    then raise (Failure ("function print may not be defined")) else ();

    report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

    let built_in_decls = StringMap.add "print" (*think this is incorrect*)
        { typ = Void; fname = "print"; formals = [(MyString, "str")] 
          locals = []; body = [] }
    in
    let function_decls = List.fold_left 
        (fun m fd -> StringMap.add fd.fname fd m) built_in_decls functions
    in
    let _ = function_decl "main"
    in 
    let check_function func = 
        report_duplicate (fun n -> "duplicate formal "^ n ^" in " ^ func.fname)
          (List.map snd func.formals);
        List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
            " in " func.fname)) func.locals;
        report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
          (List.map snd func.locals);

    let symbols = List.fold_left (fun m (t, n) ->  StringMap.add n t m)
    StringMap.empty (globals @ func.formals @ func.locals )
    in
    let type_of_identifier s =
        try StringMap.find s symbols
        with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
    let rec expr = function
        Literal _ -> Int
        | FloatLiteral _ -> Float
        | MyStringLit _ -> MyString
        | Binop(e1, op, e2) as e-> let t1 = expr e1 and t2 = expr e2 in
        (match op with
            Plus | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
            | Equal | Neq when t1 = t2 -> Bool
            | Less | Leq | Greater | Geq when (t1 = Int || t1 = Float) &&
                                              (t2 = Int || t2 = Float) -> Bool
            | And | Or when t1 = Bool && t2 = Bool -> Bool
            (* Need to add other expressions here *)
            | _ -> raise (Failure ("illegal binary operator " ^
                   string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                   string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
            | Unop(op, e) as ex -> let t = expr e in
            (match op with
              Neg when t = Int -> Int
            | Not when t = Bool -> Bool
            | Ref when t of typ
            | Deref when (t = PointerType || Voidstar)
            | _ -> raise (Failure ("illegal unary operator " 
                                    ^ string_of_uop op ^ string_of_type ^ " in "
                                    ^ string_of_expr ex))
            )
            | Noexpr -> Void
            | Assign(var, e) as ex let lt = type_of_identifier var
                                   and rt = expr e in
              check_assign lt rt (Failure ("illegal assignment " ^
                                            string_of_typ lt ^ 
                                            " = "  ^ string_of_typ rt
                                            ^ " in " ^
                                            string_of expr ex))
            | Call(fname, actuals) as call -> let fd = function_decl fname in
              if List.length actuals != List.length fd.formals then
                  raise (Failure ("expecting " ^ string_of_int
                    (List.length fd.formals) ^ " arguments in " ^
                     string_of_expr call))
              else
                  List.iter2 (fun (ft, _) e -> let = et = expr e in
                    ignore (check_assign ft et
                      (Failure ("illegal actual argument found " 
                      ^ string_of_typ et ^ " expected "
                      ^ string_of_typ ft ^ " in " string_of_expr e))))
                    fd.formals actials;
                  fd.typ
        in

        let check_bool_expr e = 
            if expr e != Bool then raise 
            (Failure ("expected Boolean Boolean expre in " ^ string_of_expr r))
            else () in


