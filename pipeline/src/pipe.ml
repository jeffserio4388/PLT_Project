(* Pipeline, adapted from MicroC by Stephen Edwards Columbia University *)
(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)


type action = Ast | C_out (* | Tast | Cppsast *)

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
                ("-c", C_out); (* ("-t", Tast) *) ]
  else C_out in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
    Ast -> print_string (Ast.string_of_program (program))
  | C_out -> print_string (Codegen.convert (program))
  (*| Tast -> Cppsast.convert_to_cppast (List.rev program)*)
