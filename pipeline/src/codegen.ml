(* Code generation: translate takes a semantically checked Semantic ST and
produces C

*)

open Ast

open Semt

module StringMap = Map.Make(String)

let simple_string_of_typ = function
    Bool -> "int"
  | Int -> "int"
  | Float -> "float"
  | Void -> "void"
  | String -> "str"
  | Array(_) -> "arr"
  | Tuple(_) -> "tup"
  | _ -> raise(Failure("simple_string_of_typ failure"))

let string_of_typ = function
    Bool -> "int"
  | Int -> "int"
  | Float -> "float"
  | Void -> "void"
  | String -> "String"
  | Array(_) -> "Array"
  | Table(_) -> "Array"
  | Tuple(_) -> "Tuple"
  | Undefined -> raise(Failure("Undefined type on string_of_typ"))


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

let set_or_insert t =
  if t then "insert" else "set"

let empty_inx = SString("INT_MIN")

let mapping_of_tup (t : tup) : string =
  let string_of_attr_type t = ( match fst t with
      String -> "text"
    | Float -> "real"
    | Int -> "integer"
    | _ -> raise(Failure("tuple mapping failure")) ) in
  "type_map map"^fst t^"[] = {"
    ^(String.concat ", " (List.map string_of_attr_type (snd t)))^"};\n"

let rec string_of_obj = function
    SId(s) -> "dampl_" ^ s
  | SBrac(o,e,_,t) -> "dampl_arr_get__" ^ simple_string_of_typ t ^ "("
      ^ string_of_obj o ^ "," ^ string_of_expr e ^ ")"
  | SBrac2(o,e1,e2,typ) ->
      let e1 = if e1 = SNoexpr then empty_inx else e1 in
      let e2 = if e2 = SNoexpr then empty_inx else e2 in
      let arrtyp = (match typ with
              Array(t) -> t
            | Table(tname) -> Tuple(tname)
            | _ -> raise(Failure("attr of table failure"))
          ) in
        "dampl_arr_get_range__"^simple_string_of_typ arrtyp^"(" ^ string_of_obj o ^ ","
      ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")" 
  | SAttr(otyp,atyp,o,name,inx) -> (match otyp with
        Tuple(tname) -> "dampl_tup_get__"^simple_string_of_typ atyp^"("
          ^string_of_obj o^","^string_of_int inx^")"
      | Table(tname) -> let typ = (match atyp with
            Array(t) -> simple_string_of_typ t
          | _ -> raise(Failure("attribute extraction failure"))
        ) in "dampl_arr_extract_attr__"^typ^"("^string_of_obj o^","
          ^string_of_int inx^")"
      | _ -> raise(Failure("$attribute failure"))
    )
  | SAttrInx(otyp,atyp,o,inx_expr) -> (match otyp with
        Tuple(tname) -> "dampl_tup_get__str("
          ^string_of_obj o^","^string_of_expr inx_expr^")"
      | Table(tname) -> "dampl_arr_extract_attr__str("^string_of_obj o^","
          ^string_of_expr inx_expr^")"
      | _ -> raise(Failure("$attribute failure"))
    )
  (*
  | SBrac of string * sem_expr (* a[0] a[i] a[i+1] *)
  | SBrac2 of string * sem_expr * sem_expr (* a[0:2] *)
  | SAttr of string * string (* a$b *)
  *)

and string_of_expr = function
    SLiteral(l) -> string_of_int l
  | SBoolLit(true) -> "1"
  | SBoolLit(false) -> "0"
  | SFloatLit(f) -> string_of_float f
  | SStrLit(s) -> s
  | SObj(o) -> string_of_obj o
  | SBinop(t, e1, o, e2) -> ( match t with
        Array(t2) -> "( dampl_arr_concat__"^simple_string_of_typ t2^"("^ string_of_expr e1
          ^ "," ^ string_of_expr e2 ^ ") )"
      | String -> ( match o with
            Add -> "( dampl_str_concat("^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ") )"
          | Equal | Neq | Less | Leq | Greater | Geq ->
              "( strcmp("^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ") "^string_of_op o^" 0 )"
          | _ -> raise(Failure("String operation translation failure"))
      )
      | _ -> "(" ^ string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2 ^ ")"
    )
  | SUnop(t, o, e) -> "(" ^ string_of_uop o ^ string_of_expr e ^ ")"
  | SAssign(t, o, e) -> ( match o with
        SBrac(o2,e2,is_ins,_) -> ( if e2 = SNoexpr
          then ( "dampl_arr_append__" ^ simple_string_of_typ t
         ^ "(" ^ string_of_obj o2 ^ "," ^ string_of_expr e ^ ")")
         else ( "dampl_arr_" ^ set_or_insert is_ins ^ "__" ^ simple_string_of_typ t
         ^ "(" ^ string_of_obj o2 ^ "," ^ string_of_expr e2 ^ "," ^ string_of_expr e ^ ")" )
      )
      | SBrac2(o2,e21,e22,typ) ->
        let e21 = if e21 = SNoexpr then empty_inx else e21 in
        let e22 = if e22 = SNoexpr then empty_inx else e22 in
        let arrtyp = (match typ with
              Array(t) -> t
            | Table(tname) -> Tuple(tname)
            | _ -> raise(Failure("attr of table failure"))
          ) in
        "dampl_arr_set_range__"^simple_string_of_typ arrtyp^"(" ^ string_of_obj o2 ^ ","
        ^ string_of_expr e21 ^ "," ^ string_of_expr e22 ^ "," ^ string_of_expr e ^ ")"
      | SAttr(otyp,atyp,o2,name,inx) -> (match otyp with
          Tuple(tname) -> "dampl_tup_set__"^simple_string_of_typ t^"("
            ^string_of_obj o2^","^string_of_int inx^","^string_of_expr e^")"
        | Table(tname) -> let attrtyp = (match atyp with
              Array(t) -> t
            | _ -> raise(Failure("attr of table failure"))
          ) in
          "dampl_arr_set_attr__"^simple_string_of_typ attrtyp^"("
          ^string_of_obj o2^","^string_of_int inx^","^string_of_expr e^")"
        | _ -> raise(Failure("$attribute failure"))
      )
      | SAttrInx(otyp,atyp,o2,inx_expr) -> (match otyp with
          Tuple(tname) -> "dampl_tup_set__"^simple_string_of_typ t^"("
            ^string_of_obj o2^","^string_of_expr inx_expr^","^string_of_expr e^")"
        | Table(tname) -> "dampl_arr_set_attr__"^simple_string_of_typ t^"("
          ^string_of_obj o^","^string_of_expr inx_expr^","^string_of_expr e^")"
        | _ -> raise(Failure("$attribute failure"))
      )
      | _ -> string_of_obj o ^ " = " ^ string_of_expr e
  )
  | SCall(f, el) ->
      "dampl_" ^ f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | SSpecialCall(_,f,el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | SArr(t,exprs) -> if (List.length exprs) = 0
      then "dampl_arr_new()"
      else (
        let create_append expr =
            "dampl_arr_append__"^simple_string_of_typ t^"(a,"^string_of_expr expr^");\n"
        in "({\nArray a=dampl_arr_new();\n"
          ^(String.concat "" (List.map create_append exprs))
          ^"a;})"
      )
  | STabInst(_) -> "dampl_arr_new()"
  | STupInst(name,n) -> "dampl_tup_new("^string_of_int n^",map"^name^")"
  (*
  | STupInst of string (* tuple instantiation *)
  | STabInst of string (* table instantiation e.g. Foo[] *)
  | STupInit of string * expr list (* tuple init e.g. Foo{1,2,"abc"} *)
  | SArr of expr list (* arrays e.g. [1,2,3] *)
  | SDict of expr list * expr list (* dicts *)
  *)
  | SNoexpr -> ""
  | SString(s) -> s


and string_of_stmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_expr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  
  | SFor(str,t, e, s) -> "{\n"
      ^ string_of_typ t ^ " dampl_"^str^" = dampl_arr_get__"^simple_string_of_typ t^"("^ string_of_expr e ^",0);\n"
      ^ "int i_"^str ^ " = 0;\n"
      ^ "while(i_"^str^" < dampl_arr_len(" ^ string_of_expr e ^ ") ) {\n" 
        ^"dampl_"^str^" = dampl_arr_get__"^simple_string_of_typ t^"("^ string_of_expr e ^",i_"^str ^ ");\n"
      ^string_of_stmts ( match s with SBlock(sl) -> sl | stmt -> [stmt] )
      ^"i_"^str^"++;\n}\n}\n"
  
     (* "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
    *)
  | SWhile(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | SBreak -> "break;\n"
  | SContinue -> "continue;\n"

and string_of_stmts stmts =
  String.concat "" (List.map string_of_stmt stmts)


let string_of_vdecl (id, t) = string_of_typ t ^ " dampl_" ^ id ^ ";\n"

let string_of_formal f = (string_of_typ (fst f))^" dampl_"^(snd f)

let string_of_global global =
  string_of_typ (fst global) ^ " " ^ " dampl_" ^ (snd global) ^";\n";;

let fdecl_prototype fdecl =
  string_of_typ fdecl.rtyp ^ " " ^ " dampl_" ^ fdecl.semfname ^
  "(" ^ String.concat ", " (List.map string_of_formal fdecl.semformals) ^
  ");\n";;

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^ " dampl_" ^ fdecl.semfname ^
  "(" ^ String.concat ", " (List.map string_of_formal fdecl.semformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl (StringMap.bindings !(fdecl.semlocals) ) ) ^
  String.concat "" (List.map string_of_stmt fdecl.sembody) ^
  "}\n\n"



let string_of_program (globals, statements, functions, tuples) = 
  "#include <stdio.h>\n" ^ "#include <stdlib.h>\n" ^
  "#include \"dampllib.h\"\n\n" ^
  String.concat "" (List.map mapping_of_tup tuples) ^ "\n" ^
  String.concat "" (List.map string_of_global globals) ^ "\n" ^
  String.concat "" (List.map fdecl_prototype functions) ^ "\n" ^
  String.concat "" (List.map string_of_fdecl functions) ^
  "int main(int argc,char** argv){\n" ^
  "dampl_args=build_args_array(argv);\n" ^
  "dampl_file_constructor();\n" ^
  String.concat "" (List.map string_of_stmt statements) ^ 
  "return 0;\n}\n"
