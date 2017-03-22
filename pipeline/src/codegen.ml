module A = Ast
module StringMap = Map.Make(String)

open Printf

(* file print related *)

let file = "output.c"
let message = "Hello!"
  
let () =
  (* Write message to file *)
  let oc = open_out file in    (* create or truncate file, return channel *)
  fprintf oc "%s\n" message;   (* write something *)   
  close_out oc;

(* string map for storing variables *)


(* specific grammars specified here *)

let ltype_of_typ = function
    A.Int     -> "int"
    | A.Bool    -> "int"
    | A.Float   -> "float"
    | A.Char    -> "char"
    | A.Void    -> "void" in


    (**)

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in
