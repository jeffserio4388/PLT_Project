(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) =

  let ctype_of_typ = function
      A.Int     -> "int"
    | A.Bool    -> "int"
    | A.Float   -> "float"
    | A.Char    -> "char"
    | A.Void    -> "void" in

  let global_vars = 
    let global_var global_map (typ, name) =
      let init = StringMap.add n t global_map in 
      List.fold_left global_var StringMap.empty globals in

    let function_decls = 
      let function_decl func_map fdecl = 
        let name = fdecl.A.fname
        and formal_types = Array.of_list (List.map (fun(t, _) -> ctype_of_typ t) decl.A.formal) 
        in StringMap.add name (fdecl.A.typ, formal_types, fdecl) func_map in 
        List.fold_left function_decl StringMap.empty functions in 

        let build_function_body fdecl =
          let add_local m (t, n) =
            StringMap.add n t m in 

            let formals = List.fold_left add_local StringMap.empty fdecl.A.







