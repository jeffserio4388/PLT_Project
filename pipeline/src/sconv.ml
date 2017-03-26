open Ast
open Semt
open Stack
module StringMap = Map.Make(String);;
open Builtin
open Codegen

let param_separator = "__"

let fun_decls = ref StringMap.empty;;

let fun_abodies = ref StringMap.empty;;

let globals = ref (StringMap.singleton "args" (Array(String)) );;

let funs_to_reparse = ref [];;

let fun_parser_stack = (Stack.create () : string Stack.t);;

let loop_stack = (Stack.create () : int Stack.t);;

let tup_decls = ref StringMap.empty;;

let tup_attrs = ref StringMap.empty;;

let tup_inx = ref StringMap.empty;;

let tup_sizes = ref StringMap.empty;;

(* let tup_sizes = ref StringMap.empty;; *)

let rec string_of_param_typ = function
    Bool -> "bool"
  | Int -> "int"
  | Float -> "float"
  | String -> "str"
  | Void -> "void"
  | Array(x) -> "arr"^(string_of_param_typ x)
  | Tuple(s) -> "tup"^s
  | Table(s) -> "tab"^s
  | Undefined -> raise(Failure("Undefined param on string_of_param_typ"))

let rec string_of_typ = function
    Bool -> "Bool"
  | Int -> "Int"
  | Float -> "Float"
  | Void -> "Void"
  | String -> "String"
  | Array(x) -> "Array("^(string_of_typ x)^")"
  | Table(x) -> "Table("^x^")"
  | Tuple(x) -> "Tuple("^x^")"
  | Undefined -> "Undefined"
  

let get_built_in_name_folder map fun_prot =
  StringMap.add (get_built_in_name fun_prot) true map;;

let built_in_decls =
  List.fold_left get_built_in_name_folder StringMap.empty built_in_prototypes;;

let generate_fun_name (name : string) (typs: typ list) : string = 
  name^param_separator^(String.concat "_" (List.map string_of_param_typ typs))

let built_in_prot_map_folder map fun_prot =
  let build_formal ftyp = (ftyp,"x") in
  let build_formals typlist = List.map build_formal typlist in
  let rec build_prot_map rtyp name protlist mmap = match protlist with
      prot::l -> 
        let funname = generate_fun_name name prot in
        let semfdecl = {
          rtyp = rtyp; semfname = funname;
          originalname = name; semformals = build_formals prot;
          semlocals = ref StringMap.empty; sembody = [];
        } in StringMap.add funname semfdecl (build_prot_map rtyp name l mmap)
    | [] -> mmap
  in build_prot_map (get_built_in_rtyp fun_prot) (get_built_in_name fun_prot)
    (get_built_in_formals fun_prot) map;;


let parsed_funs = ref 
  (List.fold_left built_in_prot_map_folder StringMap.empty built_in_prototypes);;

let is_logical_op = function
  | And | Or -> true
  | _ -> false

let is_comp_op = function
  | Equal | Neq | Less | Leq | Greater | Geq -> true
  | _ -> false

let pv k v = print_string("\nvar: "^k);;

let get_id_typ_from_locals_or_globals id fname = 
  let semfdecl = StringMap.find fname !parsed_funs in
  if StringMap.mem id !(semfdecl.semlocals)
  then StringMap.find id !(semfdecl.semlocals)
  else if StringMap.mem id !globals
  then StringMap.find id !globals
  else raise(Failure("var "^id^" not found"))

let rec get_string_of_sem_obj semobj = match semobj with
    SId(id) -> id
  | SBrac(o,_,_,_) -> (get_string_of_sem_obj o)^"*"
  | SBrac2(o,_,_,_) -> (get_string_of_sem_obj o)
  | SAttr(_,_,o,attr,_) -> (get_string_of_sem_obj o)^"$"^attr^"$"
  | SAttrInx(_,_,o,_) -> (get_string_of_sem_obj o)^"$$"
  (* | _ -> raise(Failure("get_string_of_sem_obj case not implemented")) *)

let is_collection_access (obj : sem_obj) : bool =
  match obj with
    SBrac(_) -> true
  | _ -> false

let is_collection_range_access (obj : sem_obj) : bool =
  match obj with
    SBrac2(_) -> true
  | _ -> false

let is_array (typ : typ) : bool =
  match typ with
    Array(_) -> true
  | _ -> false

let is_tuple (typ : typ) : bool =
  match typ with
    Tuple(_) -> true
  | _ -> false

let get_tuple_name (typ : typ) : string =
  match typ with
    Tuple(t) -> t
  | _ -> ""

let rec is_array_of_undefined typ = match typ with
    Array(t) -> is_array_of_undefined t
  | Undefined -> true
  | _ -> false

let rec get_obj_typ (o : sem_obj) : typ = match o with
    SId(id) -> (* print_string(id); print_string(Stack.top fun_parser_stack); *)
      let fname = (Stack.top fun_parser_stack) in
      if fname = "_global_"
      then if StringMap.mem id !globals
        then StringMap.find id !globals
        else raise(Failure("global "^id^" not found"))
      else get_id_typ_from_locals_or_globals id fname
  | SBrac(o,e,i,_) ->
      let otyp = get_obj_typ o in
      ( match otyp with
        Table(t) -> Tuple(t)
      | Array(t) -> t
      | _ -> raise(Failure("cant get elem of non collection object"))
    )
  | SBrac2(o,_,_,_) ->
      let otyp = get_obj_typ o in
      ( match otyp with
        Table(x) -> Table(x)
      | Array(x) -> Array(x)
      | _ -> raise(Failure("cant get elem of non collection object"))
    )
  | SAttr(_,t,o,attr,_) -> t
  | SAttrInx(_,t,o,attr) -> t
(*
      let otyp = get_obj_typ o in (
      match otyp with
        Table(x) ->
          let attrId = x^"$"^attr in
          if StringMap.mem attrId !tup_decls
          then Array(StringMap.find attrId !tup_decls)
          else raise(Failure("tuple "^x^" has no attr "^attr))
      | Tuple(x) ->
          let attrId = x^"$"^attr in
          if StringMap.mem attrId !tup_decls
          then StringMap.find attrId !tup_decls
          else raise(Failure("tuple "^x^" has no attr "^attr))
      | _ -> raise(Failure("cant get attr of item that is not tuple or table"))
    )
  *)
  (* | _ -> raise(Failure("get_obj_typ case not implemented")) *)

let get_expr_typ (exp : sem_expr) : typ = match exp with (* TODO *)
    SLiteral(_) -> Int
  | SStrLit(_) -> String
  | SFloatLit(_) -> Float
  | SBoolLit(_) -> Bool
  | SObj(o) -> get_obj_typ o
  | SBinop(t,_,_,_) -> t
  | SUnop(t,_,_) -> t
  | SCall(name,_) -> (StringMap.find name !parsed_funs).rtyp
  | STupInst(tupname,_) -> Tuple(tupname)
  | STabInst(tupname) -> Table(tupname)
  | SArr(t,_) -> Array(t)
  | SSpecialCall(t,_,_) -> t
  | SNoexpr | _ -> Void
;; 

(* TODO: improve this function *)
let rec get_typ_from_fun_sembody name body = match body with
    SReturn(exp)::l ->
      let rettyp = get_expr_typ exp in
      if rettyp == Undefined then
      get_typ_from_fun_sembody name l
      else if rettyp == Void then
      raise(Failure("fun "^name^": cant return void"))
      else rettyp
  | _::l -> get_typ_from_fun_sembody name l
  | [] -> Void;;

let rec validate_param_typs formals typs = match formals, typs with
   f::fl, t::tl -> ( (fst f) == t) && validate_param_typs fl tl
  | [], _::_ -> false
  | _::_, [] -> false
  | [], [] -> true;;

let rec generate_new_formals_and_locals oldformals typs =
  match oldformals, typs with
    oldformal::llst, t::tl ->
      let recval = generate_new_formals_and_locals llst tl in
      ( (t,snd oldformal)::(fst recval), StringMap.add (snd oldformal) t (snd recval) )
  | [], [] -> ([], StringMap.empty)
  | _, _ -> raise(Failure ( "generate_new_formals_and_locals error" ))
;;

let update_formal_typs_in_semfdecl (newname: string) (old : sem_func_decl) (typs : typ list) : sem_func_decl =
  let newformals, newlocals = generate_new_formals_and_locals old.semformals typs in 
  { old with
    semfname = newname;
    semformals = newformals;
    semlocals = ref newlocals;
  }

let rec check_if_exists_in_stack stack str =
  if Stack.is_empty stack
  then false
  else
    if (Stack.pop stack) == str
    then true
    else check_if_exists_in_stack stack str;;

let rec add_id_to_map_backtrace mapref semobj expr_typ =
  (* print_string("backtrace adding "^(get_string_of_sem_obj semobj)^" "^(string_of_typ expr_typ)^"\n"); *)
  mapref := StringMap.add (get_string_of_sem_obj semobj) expr_typ !mapref;
  ( match semobj with
      SBrac(o,_,_,_)  -> (add_id_to_map_backtrace mapref o (Array(expr_typ)))
    | _ -> ()
  );
  ()

let rec add_id_to_map_helper mapref semobj expr_typ backtrace =
  (* print_string("adding "^(get_string_of_sem_obj semobj)^" "^(string_of_typ expr_typ)^"\n"); *)
  (if backtrace then ( match semobj with
      SBrac(o,_,_,_) -> (add_id_to_map_backtrace mapref o (Array(expr_typ)))
    | SBrac2(o,_,_,_) -> (add_id_to_map_backtrace mapref o (expr_typ))
    | _ -> ()
  ) else ());
  ( match expr_typ with
      Tuple(x) -> (
        let register_attr attr =
          ((mapref := StringMap.add
            ((get_string_of_sem_obj semobj)^"$"^(snd attr)^"$" ) (fst attr)
          !mapref);
          (* print_string("adding-attr "^((get_string_of_sem_obj semobj)^"$"^(snd attr)^"$" )^" "^(string_of_typ (fst attr))^"\n"); *)
          ())
           in
        List.iter register_attr (StringMap.find x !tup_attrs)
      )
    | Table(x) ->
      (add_id_to_map_helper mapref (SBrac(semobj,SNoexpr,false,(Tuple(x))) ) (Tuple(x)) false);
      (let register_attr attr =
          ((mapref := StringMap.add
            ((get_string_of_sem_obj semobj)^"$"^(snd attr)^"$" ) (Array(fst attr))
          !mapref);
          (* print_string("adding-attr "^((get_string_of_sem_obj semobj)^"$"^(snd attr)^"$" )^" "^(string_of_typ (Array(fst attr)))^"\n"); *)
          ())
           in
        List.iter register_attr (StringMap.find x !tup_attrs));
      ()
    | Array(x) -> (add_id_to_map_helper mapref (SBrac(semobj,SNoexpr,false,x)) x false); ()
    | _ -> ()
  );
  (mapref := StringMap.add (get_string_of_sem_obj semobj) expr_typ !mapref);
  (* (StringMap.iter pv !mapref); *)
  ()

let add_id_to_map mapref semobj expr_typ =
  add_id_to_map_helper mapref semobj expr_typ true

let rec parse_semfdecl newname semfdecl typs =
  let callername = Stack.top fun_parser_stack in
  if check_if_exists_in_stack (Stack.copy fun_parser_stack) semfdecl.semfname
  then (* function already being parsed, so reparse caller later *)
    (funs_to_reparse := callername :: (!funs_to_reparse) )
  else (
    Stack.push newname fun_parser_stack;
    Stack.push 0 loop_stack;
    let newsemfdecl = update_formal_typs_in_semfdecl newname semfdecl typs in
    (* the part below is copied to check_if_fun_needs_reparse *)
    (* so any changes made here must be also done there *)
    let abody = StringMap.find semfdecl.semfname !fun_abodies in
    parsed_funs := StringMap.add newsemfdecl.semfname newsemfdecl !parsed_funs;
    let newsembody = convert_stmts abody in 
    let newrtype = get_typ_from_fun_sembody newsemfdecl.semfname newsembody in
    if newrtype == Undefined then (
    ignore(Stack.pop fun_parser_stack);
    ignore(Stack.pop loop_stack);
    (funs_to_reparse := (newsemfdecl.semfname) :: (!funs_to_reparse) )
    ) else (
      let newnewsemfdecl = { newsemfdecl with
        sembody = newsembody;
        rtyp = newrtype;
      } in
      parsed_funs := StringMap.add newnewsemfdecl.semfname newnewsemfdecl !parsed_funs;
      ignore(Stack.pop fun_parser_stack);
      ()
    )
  )


and handle_scall (name : string) (typs : typ list) : string =
  let newname = generate_fun_name name typs in (
    if (StringMap.mem newname !parsed_funs)
    then ()
    else (
      if (StringMap.mem name built_in_decls) then
      raise(Failure("builtin fun "^name^" does not accept such params"))
      else
        if StringMap.mem name !fun_decls then (
          let semfdecl = StringMap.find name !fun_decls in
          if ( (List.length typs) == (List.length semfdecl.semformals) )
          then parse_semfdecl newname semfdecl typs
          else raise (Failure ("fun "^name^" call has wrong number of param"))
        ) else raise( Failure ("fun "^name^" used but never declared"))
    )
  ); newname

and convert_obj (o : obj) : sem_obj = 
  convert_obj_checking_side o false

and convert_obj_checking_side (o : obj) (is_lhs : bool ) : sem_obj = match o with
    Id(id) -> SId(id)
  | Brac(o,e,i) ->
      let semo = convert_obj o in
      let seme = convert_expr e in
      let otyp = get_obj_typ semo in
      let etyp = get_expr_typ seme in
      (* (print_string ((string_of_typ otyp)^" ")); *)
      (* (print_string ((get_string_of_sem_obj semo)^"\n")); *)
      if etyp = Int || (seme = SNoexpr && is_lhs) then
        match otyp with
          Table(t) -> SBrac(semo,seme,i,Tuple(t))
        | Array(t) -> SBrac(semo,seme,i,t)
        | _ -> raise(Failure("cant get elem of non collection object"))
      else if (seme = SNoexpr) then 
        raise(Failure("illegal empty index on rhs"))
      else raise(Failure("index must be integer"))
  | Brac2(o,e1,e2) ->
      let semo = convert_obj o in
      let seme1 = convert_expr e1 in
      let seme2 = convert_expr e2 in
      let otyp = get_obj_typ semo in
      let e1typ = get_expr_typ seme1 in
      let e2typ = get_expr_typ seme2 in
      if (e1typ = Int || seme1 = SNoexpr) && (e2typ = Int || seme2 = SNoexpr) then
        match otyp with
          Table(_) | Array(_) -> SBrac2(semo,seme1,seme2,otyp)
        | _ -> raise(Failure("cant get elem of non collection object"))
      else raise(Failure("index must be integer"))
  | Attr(o,attr) ->
      let semo = convert_obj o in
      let otyp = get_obj_typ semo in (
      match otyp with
        Table(x) ->
          let t =
            if StringMap.mem (x^"$"^attr) !tup_decls
            then StringMap.find (x^"$"^attr) !tup_decls
            else raise(Failure(x^"$"^attr^" doesnt exist")) in
          let inx = (StringMap.find (x^"$"^attr) !tup_inx)
          in SAttr(Table(x),Array(t),semo,attr,inx)
      | Tuple(x) ->
          let t =
            if StringMap.mem (x^"$"^attr) !tup_decls
            then StringMap.find (x^"$"^attr) !tup_decls
            else raise(Failure(x^"$"^attr^" doesnt exist")) in
          let inx = StringMap.find (x^"$"^attr) !tup_inx
          in SAttr(Tuple(x),t,semo,attr,inx)
      | _ -> raise(Failure("cant get attr of item that is not tuple or table"))
    ) 
  | AttrInx(o,expr) ->
      let semexpr = convert_expr expr in
      let etyp = get_expr_typ semexpr in
      let semo = convert_obj o in
      let otyp = get_obj_typ semo in 
      if etyp = Int
      then (
        match otyp with
          Table(x) -> SAttrInx(Table(x),Array(String),semo,semexpr)
        | Tuple(x) -> SAttrInx(Tuple(x),String,semo,semexpr)
        | _ -> raise(Failure("cant get attr of item that is not tuple or table"))
      ) else raise(Failure("attribute index must be integer")) 
  (* | _ -> raise(Failure("convert_obj case not implemented")) *)

(*
and str_of_obj = function
Id(s) -> s
*)

and convert_expr (exp : expr) : sem_expr = match exp with
    Literal(i) -> SLiteral(i)
  | StrLit(s) ->  SStrLit(s)
  | FloatLit(f) -> SFloatLit(f)
  | BoolLit(b) -> SBoolLit(b)
  | Assign(o,e) -> convert_assign o e
  | Binop(e1,op,e2) -> 
    let seme1 = convert_expr e1 in let seme2 = convert_expr e2 in
    let e1typ = get_expr_typ seme1 in let e2typ = get_expr_typ seme2 in
    if (is_array e1typ) || (is_array e2typ) then
    ( if op = Add then
        if e1typ = e2typ then
        SBinop(e1typ,seme1,op,seme2)
        else raise(Failure("array concat operands must have same type"))
      else raise(Failure("invalid array operation"))
    )
    else if is_logical_op op then
    ( if ((e1typ = Bool || e1typ = Int) && (e2typ = Bool || e2typ = Int))
      then SBinop(Bool,seme1,op,seme2)
      else raise(Failure("invalid type on logical operation"))
    )
    else if is_comp_op op then
    ( if (e1typ = e2typ) || ((e1typ = Float || e1typ = Int) && (e2typ = Float || e2typ = Int))
      then SBinop(Bool,seme1,op,seme2)
      else if (e1typ = Float && e2typ = String)
      then SBinop(Bool,seme1,op,SCall("float__str",[seme2]))
      else if (e1typ = Int && e2typ = String)
      then SBinop(Bool,seme1,op,SCall("int__str",[seme2]))
      else if (e1typ = String && e2typ = Float)
      then SBinop(Bool,SCall("float__str",[seme1]),op,seme2)
      else if (e1typ = String && e2typ = Int)
      then SBinop(Bool,SCall("float__str",[seme1]),op,seme2)
      else raise(Failure("invalid type on comparision operation"))
    )
    else 
      if e1typ = String then 
        if e2typ = String && op = Add
        then SBinop(String,seme1,op,seme2)
        else raise(Failure("Invalid string operation"))
      else if e2typ = String then 
        if e1typ = String && op = Add
        then SBinop(String,seme1,op,seme2)
        else raise(Failure("Invalid string operation"))
      else if e1typ = Bool || e2typ = Bool
        then raise(Failure("logical expression inside arithmetic expression"))
      else
        let resulttyp = if e1typ = Float || e2typ = Float
          then Float else Int in
        SBinop(resulttyp,seme1,op,seme2)
  | Unop(op,expr) ->
    let semexpr = convert_expr expr in
    let exprtyp = get_expr_typ semexpr in
    if op = Neg
      then if (exprtyp = Int || exprtyp = Bool)
        then SUnop(Bool,op,semexpr)
        else raise(Failure("neg unop on invalid type"))
    else
      if (exprtyp = Int || exprtyp = Float)
        then SUnop(exprtyp,op,semexpr)
        else raise(Failure("minus unop on invalid type"))
  | Call(s,lst) -> ( (* (print_string("_"^s^"_in "^(Stack.top fun_parser_stack)^"\n"); *)
          (* verifies if it is a special function *)
          let sbexpr = handle_special_function s lst in
          if sbexpr = SNoexpr then
            let exprs = convert_exprs lst in 
            let typs = (List.map get_expr_typ exprs) in
            let funname = (handle_scall s typs ) in
            SCall(funname,exprs) 
          else sbexpr )
  | Obj(o) -> SObj(convert_obj o)
  | TupInst(tup) -> (
      if StringMap.mem tup !tup_decls
      then STupInst(tup, (StringMap.find tup !tup_sizes))
      else raise(Failure("tuple"^tup^" never declared"))
    )
  | TabInst(tup) -> (
      if StringMap.mem tup !tup_decls
      then STabInst(tup)
      else raise(Failure("tuple"^tup^" never declared"))
    )
  | Arr(exps) ->
    let semexps = List.map convert_expr exps in
    let exptyps = List.map get_expr_typ semexps in
    let rec checktyps l = ( match l with
      [a] -> a
    | a::ll -> if (a = checktyps ll)
      then a else raise(Failure("type mismatch on array init"))
    | [] -> Undefined )
    in SArr(checktyps exptyps, semexps)    
  | Noexpr -> SNoexpr
  

and convert_exprs exps = (List.map convert_expr exps)


and convert_stmt stmt = match stmt with
    Block(lst) -> SBlock(convert_stmts lst)
  | Expr(exp) -> (* print_string("E_in "^(Stack.top fun_parser_stack)^"\n"); *)
      SExpr(convert_expr exp)
  | Return(exp) -> (* print_string("R_in "^(Stack.top fun_parser_stack)^"\n"); *)
      if (Stack.top fun_parser_stack) <> "_global_" then
        SReturn(convert_expr exp)
      else raise(Failure("Illegal return outside function"))
  | If(exp,s1,s2) ->
      let semexpr = convert_expr exp in
      let typ = get_expr_typ semexpr in
      if typ = Bool || typ = Int then
        SIf(semexpr, convert_stmt s1, convert_stmt s2)
      else raise(Failure("invalid if condition"))
  | While(exp,stmt) -> 
      (let loopcount = (Stack.pop loop_stack) in Stack.push (loopcount+1) loop_stack);
      let semexpr = convert_expr exp in
      let typ = get_expr_typ semexpr in
      if typ = Bool || typ = Int then (
        let ret = SWhile(semexpr, convert_stmt stmt) in
        (let loopcount = (Stack.pop loop_stack) in (Stack.push (loopcount-1) loop_stack));
        ret )
      else raise(Failure("invalid while condition"))
  | For(id,expr,stmt) -> (
      (let loopcount = (Stack.pop loop_stack) in Stack.push (loopcount+1) loop_stack);
      let semexpr = convert_expr expr in
      let typ = get_expr_typ semexpr in
      let value = match typ with
          Array(t) -> (
            if t = Undefined then raise(Failure("cant loop on array of undefined type"))
            else  
              let mapref = if (Stack.top fun_parser_stack) = "_global_"
              then globals else (StringMap.find (Stack.top fun_parser_stack) !parsed_funs).semlocals in
              let return = (mapref := (StringMap.add id t !mapref)); SFor(id,t,semexpr, convert_stmt stmt) in
              (mapref := (StringMap.remove id !mapref)); return )
        | Table(t) -> (
            let mapref = if (Stack.top fun_parser_stack) = "_global_"
            then globals else (StringMap.find (Stack.top fun_parser_stack) !parsed_funs).semlocals in
            let return = (mapref := (StringMap.add id (Tuple(t)) !mapref)); SFor(id,(Tuple(t)),semexpr, convert_stmt stmt) in
            (mapref := (StringMap.remove id !mapref)); return )
        | _ -> raise(Failure("cant make for on non iterable"))
       in (let loopcount = (Stack.pop loop_stack) in (Stack.push (loopcount-1) loop_stack)); value )
  | Break -> if (Stack.top loop_stack) > 0
    then SBreak else raise(Failure("cannot use break out of loop"))
  | Continue -> if (Stack.top loop_stack) > 0
    then SContinue else raise(Failure("cannot use continue out of loop"))
  (* | _ ->  raise( Failure ("convert_stmt case not implemented")) *)

and convert_stmts stmts = (List.map convert_stmt stmts)




and convert_assign o e = (
  (* print_string("converting "^(get_string_of_sem_obj (convert_obj o) )^"\n"); *)
  let semexpr = convert_expr e in
  let semobj = convert_obj_checking_side o true in
  let expr_typ = (get_expr_typ semexpr) in
  if (Stack.top fun_parser_stack) = "_global_"
  then (
    if StringMap.mem (get_string_of_sem_obj semobj) !globals
    (* outside function, global environment *)
    then (
      (* print_string((get_string_of_sem_obj semobj)^" "^(string_of_param_typ(get_obj_typ semobj))); *)
      (* print_string(" "^(string_of_param_typ(get_expr_typ semexpr))^"\n"); *)
      let expectedtyp = StringMap.find (get_string_of_sem_obj semobj) !globals in
      if expectedtyp = get_expr_typ semexpr
        || (expectedtyp = Float && (get_expr_typ semexpr) = Int)
        || ( ( is_array expectedtyp ) && ( expr_typ = (Array(Undefined)) ) )
        then SAssign(expectedtyp,semobj, semexpr)
      else if ( (not (is_collection_access(semobj))) && expectedtyp = Int && expr_typ = Float)
        then ( add_id_to_map globals semobj expr_typ;
        SAssign(Float,semobj, semexpr) )
      else if(is_collection_access(semobj) && expectedtyp = Undefined )
        then ( add_id_to_map globals semobj expr_typ;
        SAssign(expr_typ,semobj, semexpr) )
      else if(is_collection_range_access(semobj) && expectedtyp = (Array(Undefined)) && (is_array expr_typ))
        then ( add_id_to_map globals semobj expr_typ;
        SAssign(expr_typ,semobj, semexpr) )
      else if ( is_tuple expectedtyp && expr_typ = (Array(String)) ) then
        let tupname = (get_tuple_name expectedtyp) in
        let tupsize = StringMap.find tupname !tup_sizes in 
        let params = [semexpr;SLiteral(tupsize);SString("map"^tupname)] in
        let newexp = (SSpecialCall(Tuple(tupname),"dampl_tup_convert",params)) in
        SAssign(Tuple(tupname),semobj,newexp)
      else raise(Failure("cant change global "^(get_string_of_sem_obj semobj)^" type"))
    ) else ( add_id_to_map globals semobj expr_typ; 
    SAssign(expr_typ,semobj, semexpr)
    )
  ) else (
    (* inside function, local environment *)
    let semfdecl = StringMap.find (Stack.top fun_parser_stack) !parsed_funs in
    if StringMap.mem (get_string_of_sem_obj semobj) !(semfdecl.semlocals)
    (* check if it is a local variable *)
    then (
      let expectedtyp = StringMap.find (get_string_of_sem_obj semobj) !(semfdecl.semlocals) in
      if expectedtyp = get_expr_typ semexpr
        || (expectedtyp = Float && (get_expr_typ semexpr) = Int)
        || ( ( is_array expectedtyp ) && ( expr_typ = (Array(Undefined)) ) )
        then SAssign(expectedtyp,semobj, semexpr)
      else if ( (not (is_collection_access(semobj))) && expectedtyp = Int && expr_typ = Float)
        then ( (add_id_to_map (semfdecl.semlocals) semobj expr_typ);
        SAssign(Float,semobj, semexpr) )
      else if(is_collection_access(semobj) && expectedtyp = Undefined )
        then ( add_id_to_map (semfdecl.semlocals) semobj expr_typ;
        SAssign(expr_typ,semobj, semexpr) )
      else if(is_collection_range_access(semobj) && expectedtyp = (Array(Undefined)) && (is_array expr_typ))
        then ( add_id_to_map (semfdecl.semlocals) semobj expr_typ;
        SAssign(expr_typ,semobj, semexpr) )
      else if ( is_tuple expectedtyp && expr_typ = (Array(String)) ) then
        let tupname = (get_tuple_name expectedtyp) in
        let tupsize = StringMap.find tupname !tup_sizes in 
        let params = [semexpr;SLiteral(tupsize);SString("map"^tupname)] in
        let newexp = (SSpecialCall(Tuple(tupname),"dampl_tup_convert",params)) in
        SAssign(Tuple(tupname),semobj,newexp)
      else raise(Failure("cant change var "^(get_string_of_sem_obj semobj)^" type"))
    ) else (
      (* check if it is a global *)
      if StringMap.mem (get_string_of_sem_obj semobj) !globals
      then (
        let expectedtyp = StringMap.find (get_string_of_sem_obj semobj) !globals in
       if expectedtyp = get_expr_typ semexpr
        || (expectedtyp = Float && (get_expr_typ semexpr) = Int)
        || ( ( is_array expectedtyp ) && ( expr_typ = (Array(Undefined)) ) )
          then SAssign(expectedtyp,semobj, semexpr)
        else if ( (not (is_collection_access(semobj))) && expectedtyp = Int && expr_typ = Float)
          then (add_id_to_map globals semobj expr_typ;
          SAssign(Float,semobj, semexpr) )
        else if(is_collection_access(semobj) && expectedtyp = Undefined )
          then ( add_id_to_map globals semobj expr_typ;
          SAssign(expr_typ,semobj, semexpr) )
        else if(is_collection_range_access(semobj) && expectedtyp = (Array(Undefined)) && (is_array expr_typ))
          then ( add_id_to_map globals semobj expr_typ;
          SAssign(expr_typ,semobj, semexpr) )
        else if ( is_tuple expectedtyp && expr_typ = (Array(String)) ) then
          let tupname = (get_tuple_name expectedtyp) in
          let tupsize = StringMap.find tupname !tup_sizes in 
          let params = [semexpr;SLiteral(tupsize);SString("map"^tupname)] in
          let newexp = (SSpecialCall(Tuple(tupname),"dampl_tup_convert",params)) in
          SAssign(Tuple(tupname),semobj,newexp)
        else raise(Failure("cant change global "^(get_string_of_sem_obj semobj)^" type"))
        (* if not local or global, create new local var *)
      ) else ( add_id_to_map (semfdecl.semlocals) semobj expr_typ;
      SAssign(expr_typ,semobj, semexpr)
      )
    )
  )
)

and handle_special_function (name : string) (exprs : expr list) : sem_expr =
  match name with
  "len" -> ( match exprs with (* len call for arrays or tables *)
    [Obj(o)] -> ( match (get_obj_typ (convert_obj_checking_side o true)) with
          Table(_) -> SSpecialCall(Int,"dampl_arr_len",(convert_exprs exprs))
        | Array(_) -> SSpecialCall(Int,"dampl_arr_len",(convert_exprs exprs))
        | Tuple(name) -> (SLiteral(StringMap.find name !tup_sizes))
        | _ -> SNoexpr
      )
    | _ -> SNoexpr
    )
  | "print" -> (
      let rec get_array_dimension_and_type arr n = ( match arr with
          Array(t) -> get_array_dimension_and_type t (n+1)
        | t -> (n,t)
        ) in
      let semexprs = convert_exprs exprs in
      let typs = List.map get_expr_typ semexprs in
      match typs with
        [Array(t)] -> let dim,typ = get_array_dimension_and_type (Array(t)) 0 in
          let tstr = Codegen.simple_string_of_typ typ in
          SSpecialCall(Void,"dampl_print_arr__"^tstr,semexprs@[(SLiteral(dim))])
      | [Table(tname)] ->
          SSpecialCall(Void,"dampl_print_arr__tup",semexprs@[(SLiteral(1))])
      | [Tuple(name)] -> 
          SSpecialCall(Void,"dampl_print__tup",semexprs)
      | _ -> SNoexpr
  )
  | "str" -> (
      let rec get_array_dimension_and_type arr n = ( match arr with
          Array(t) -> get_array_dimension_and_type t (n+1)
        | t -> (n,t)
        ) in
      let semexprs = convert_exprs exprs in
      let typs = List.map get_expr_typ semexprs in
      match typs with
        [Array(t)] -> let dim,typ = get_array_dimension_and_type (Array(t)) 0 in
          let tstr = Codegen.simple_string_of_typ typ in
          SSpecialCall(String,"dampl_str_arr__"^tstr,semexprs@[(SLiteral(dim))])
      | [Table(tname)] ->
          SSpecialCall(Void,"dampl_str_arr__tup",semexprs@[(SLiteral(1))])
      | [Tuple(name)] -> 
          SSpecialCall(String,"dampl_str__tup",semexprs)
      | _ -> SNoexpr
  )

  | _ -> SNoexpr (* No special behavior *)

(*
and convert_stmt_ignoring_unparsed_call stmt = match stmt with
    Call(name,_) ->
      let semfdecl = StringMap.find name !fun_decls in
      if semfdecl.parsed
      then convert_stmt stmt
      else SIgnoredCall
  | _ -> convert_stmt stmt

  and convert_stmts_ignoring_unparsed_calls stmts = match stmts with
    s :: l -> (convert_stmt_ignoring_unparsed_call s) :: (convert_stmts_ignoring_unparsed_calls l)
  | [] -> [];;
*)

let convert_id_to_undef_typed_id (id : string) : typed_id = (Undefined, id);;

let rec convert_ids_to_undef_typed_ids (ids : string list) : typed_id list = match ids with
    id :: l -> (convert_id_to_undef_typed_id id) :: (convert_ids_to_undef_typed_ids l)
  | [] -> [];;

let create_fun_decl fd = 
  (* let body = convert_stmts fd.body in
  let rettyp = (get_typ_from_fun_sembody fd.fname body) in *)
  fun_abodies := StringMap.add fd.fname fd.body !fun_abodies;
  let semfdecl = {
   rtyp = Undefined;
   semfname = fd.fname;
   originalname = fd.fname;
   semformals = (convert_ids_to_undef_typed_ids fd.formals);
   semlocals = ref StringMap.empty;
   sembody = []; } in
  fun_decls := StringMap.add fd.fname semfdecl !fun_decls;
  ();;

let rec create_funs_from_decls decls = match decls with
    Func(f_decl)::l -> create_fun_decl f_decl; create_funs_from_decls l
  | Tup(_)::l -> create_funs_from_decls l
  | [] -> ();;

let rec remove_formals_from_locals formals map = match formals with
  | f::lst -> StringMap.remove (snd f) (remove_formals_from_locals lst map)
  | [] -> map;;

let fun_map_to_list_fold_helper k v l =
  (* print_string ("function: "^(string_of_typ v.rtyp)^" "^v.semfname^"\n" ); *)
  v.semlocals := remove_formals_from_locals v.semformals !(v.semlocals);
  v.semlocals := StringMap.filter
    (fun k -> (fun v -> (k.[(String.length k)-1] <> '*' && k.[(String.length k)-1] <> '$')))
    !(v.semlocals);
  if ( StringMap.mem v.originalname built_in_decls) then l else v::l;;

let globals_map_to_list_fold_helper k v l = 
  if (k.[(String.length k)-1] = '*' || k.[(String.length k)-1] = '$')
    then l else (v,k)::l

(*
let reparse_fun2 name =
  Stack.push name fun_parser_stack;
  let semfdecl = StringMap.find name !fun_decls in
  let abody = StringMap.find name !fun_abodies in
  let newsembody = convert_stmts abody in
  let newrtype = get_typ_from_fun_sembody semfdecl.semfname newsembody in
  if newrtype == Undefined then
  raise(Failure("could not determine fun "^name^" return type"))
  else (
    let newnewsemfdecl = { semfdecl with
      sembody = newsembody;
      rtyp = newrtype;
    } in
    fun_decls := StringMap.add newnewsemfdecl.semfname newnewsemfdecl !fun_decls;
    ignore(Stack.pop fun_parser_stack);
    ()
  )
*)


let reparse_fun name =
  Stack.push name fun_parser_stack;
  let semfdecl = StringMap.find name !parsed_funs in
  let abody = StringMap.find semfdecl.originalname !fun_abodies in
  let newsembody = convert_stmts abody in 
  let newrtype = get_typ_from_fun_sembody semfdecl.originalname newsembody in
  if newrtype == Undefined then (
  ignore(Stack.pop fun_parser_stack);
  raise(Failure("could not determine fun "^name^" return type"))
  ) else (
    let newnewsemfdecl = { semfdecl with
      sembody = newsembody;
      rtyp = newrtype;
    } in
    parsed_funs := StringMap.add newnewsemfdecl.semfname newnewsemfdecl !parsed_funs;
    ignore(Stack.pop fun_parser_stack);
    ()
  )

let create_tup_decl tdecl =
  let tsize = List.length (snd tdecl) in
  let create_tup_item inx item = 
    (* print_string((string_of_int inx)^"declaring "^((fst tdecl)^"$"^(snd item))^"\n"); *)
    
    (tup_decls := StringMap.add ((fst tdecl)^"$"^(snd item)) (fst item) !tup_decls);
    (tup_decls := StringMap.add ((fst tdecl)^"$"^(string_of_int inx)) (fst item) !tup_decls);
    (tup_inx := StringMap.add ((fst tdecl)^"$"^(snd item)) inx !tup_inx);
    () 
  in 
  (tup_sizes := StringMap.add (fst tdecl) tsize !tup_sizes);
  (tup_decls := StringMap.add (fst tdecl) (Tuple(fst tdecl)) !tup_decls);
  (tup_attrs := StringMap.add (fst tdecl) (snd tdecl) !tup_attrs);
  List.iteri create_tup_item (snd tdecl) ;;

let rec create_tups_from_decls decls = match decls with
    Func(_)::l -> create_tups_from_decls l
  | Tup(t)::l -> create_tup_decl t; t::(create_tups_from_decls l)
  | [] -> [];;


let convert (stmts, decls) =
  let tups = create_tups_from_decls decls in
  (Stack.push "_global_" fun_parser_stack);
  (Stack.push 0 loop_stack);
  create_funs_from_decls decls;
  let semstmts = convert_stmts stmts in
  (* (List.iter print_string !funs_to_reparse); *)
  (List.iter reparse_fun !funs_to_reparse);
  (
    (StringMap.fold globals_map_to_list_fold_helper !globals []),
    semstmts,
    (StringMap.fold fun_map_to_list_fold_helper !parsed_funs []),
    tups
  );;


