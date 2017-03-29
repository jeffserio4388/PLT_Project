(* Top-level of the Pipeline compiler: scan & parse the input,
   check the resulting AST and generate output C file *)

type action = Ast | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);	(* Print the AST only *)
			      ("-c", Compile) ] (* Generate output c file *)
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  (* Semant.check ast; *)
  match action with
    Ast -> print_string (Ast.string_of_program ast)
  | Compile -> Printf.fprintf (open_out "out.c") "%s" (Codegen.translate ast)
  (* | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
  | Compile -> let m = Codegen.translate ast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m) *)


