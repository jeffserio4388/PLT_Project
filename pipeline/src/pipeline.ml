(* Pipeline, adapted from MicroC by Stephen Edwards Columbia University *)
(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

type action = Ast | C_code | Compile
open Printf

let file = "out.c"

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);	(* Print the AST only *)
			      ("-tc", C_code);  (* Generate C code, don't check *)
			      ("-c", Compile) ] (* Generate, check LLVM IR *)
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  Semant.check ast;
      
    let save_file s = 
        let oc = open_out file in
        fprintf oc "%s" s;
        close_out oc in 
 
  match action with
    Ast -> (*print_string (Ast.string_of_program ast)*) ignore
  | C_code ->  (*save_file (Codegen.translate ast)*) ignore
  | Compile -> Unix.execv "gcc" [|"out.c"|]
