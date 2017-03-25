open Ast
open Semant

let convert prog =
  
  let rec create_expr = function
      | Ast.Literal(l) -> (string_of_int l)
      | Ast.MyStringLit(l) -> l 
      | Ast.FloatLiteral(l) -> (string_of_float l) ^ "0"
      | Ast.BoolLit(x) -> if x then "1" else "0"
      (*
      | Ast.Literal_List(l)-> "[" ^ (String.concat "," (List.map  create_expr l) ) ^ "]"
      *)
      | Ast.Id(s) -> "(" ^ s ^ ")"
      | Ast.CharLit(l) -> (Char.escaped l)
      | Ast.Unop(o,e1) -> 
          (match o with
            Neg -> "-" 
          | Not -> "!"  
          ) ^ " " ^ create_expr e1 ^ ")"
      | Ast.Binop(e1, o, e2) -> 
      			"(" ^ create_expr e1 ^ " " ^
      			(match o with
		        Add -> "+" 
          | Sub -> "-" 
          | Mult -> "*" 
          | Div -> "/"
      		| Equal -> "==" 
          | Neq -> "!="
      		| Mod -> "%" 
      		| And -> "&&" 
          | Or -> "||"
      		| Less -> "<" 
          | Leq -> "<=" 
      		| Greater -> ">" 
          | Geq -> ">="
      		) ^ " " ^ create_expr e2 ^ ")"
      
         	   
   
   in
   
   let rec create_stmt = function
   	   | Ast.Expr(expr) -> create_expr expr

   	(*   | Ast.Var_Decl(tp, id) -> 
            (match tp with
                  "num" -> "float" ^ " " ^ id ^ ";\n"
                | "string" -> "string" ^ " " ^ id ^ ";\n"
                | "point" -> "float" ^ " " ^ id ^ "[2];\n"
                | _ -> "bool" ^ " " ^ id ^ ";\n"
            ) 
    *)
    (*
   	   | Ast.Assign(v, e) -> printAssign v e
   	*)
       | Ast.Print(e) -> "printf( " ^ create_expr e ^ ");\n"
    (*   
       | Ast.For(s1, e1, s2, body) -> "for (" ^ create_stmt s1 ^ " " ^ create_expr e1 ^ " ; "
                                      ^ ( remSemColon (create_stmt s2 )) ^ " ) { \n" 
                                      ^ String.concat "" (List.map create_stmt body) ^ "\n } \n"
       
       | Ast.While(e, body) -> "while " ^ create_expr e ^ " { \n" ^ String.concat "" (List.map create_stmt body) ^ "}\n"
      (*
       | Ast.Ifelse(e, s1, s2) -> "if " ^ create_expr e ^ " { \n" ^ String.concat "" (List.map create_stmt s1)
            ^ "} else { \n" ^ String.concat "" (List.map create_stmt s2) ^ "}\n"
      *)
   	   | Ast.Return(expr) -> "return " ^ create_expr expr ^ ";\n"
   	   | Ast.Noexpr       -> ""
   	   | Ast.Fdecl(f)     -> string_of_fdecl f and
              string_of_fdecl fdecl =
                  "void " ^ fdecl.fname ^ "(" ^ 
                    ( String.concat ", " (List.map (fun s -> printFunArgs s) fdecl.args) ) ^
                     "){\n" ^
                  ( String.concat "" (List.map create_stmt fdecl.body) ) ^
                  "\n}\n"
      *)
   in
   
    "#include <stdio.h>\n#include <stdlib.h>\n#include <unistd.h>\n" ^

    "//All user & library functions goes here\n" ^
    
    String.concat "" (List.map create_stmt prog) ^
    
    "//Main prog starts\n"^
    
    "int main() {\n"^
    (* change the name to be the filename.svg based on the file which is ran *)
    
   String.concat "" (List.map create_stmt prog) ^

    "return 0;\n}\n"
