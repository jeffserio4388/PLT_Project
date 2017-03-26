open Ast
open Parser
open Semt

let already_included = ref []

let basepath = ref ""

let remove_quotes str =
  String.sub str 1 ((String.length str)-2)

let get_file_from_include incl = (!basepath)^(match incl with
  FileIncl(f) -> remove_quotes f
)

let rec join_ast_list l = match l with
    a::tl -> let rec_call = join_ast_list tl in
      ((fst a)@(fst rec_call),(snd a)@(snd rec_call))
  | [] -> ([],[])

let rec get_program_from_include incl = 
  let filename = get_file_from_include incl in
  if not (List.mem filename !already_included) then (
    already_included := filename::(!already_included);
    let file = Pervasives.open_in filename in
    let lexbuf = Lexing.from_channel file in
    let includes,ast = Parser.program Scanner.token lexbuf in
    join_ast_list ((List.map get_program_from_include includes)@[ast])
  ) else ([],[])
  
let _ =
  let filename =
    if (Array.length Sys.argv) = 2 then Sys.argv.(1)
    else raise (Invalid_argument("Usage: ./dampl <input_filename>")) in
  already_included := filename::(!already_included);
  basepath := if String.contains filename '/' then
    String.sub filename 0 ((String.rindex filename '/')+1) else "";
  let file = Pervasives.open_in filename in
  let lexbuf = Lexing.from_channel file in
  let includes,ast = Parser.program Scanner.token lexbuf in
  let complete_ast = join_ast_list ((List.map get_program_from_include includes)@[ast]) in
  let semt = Sconv.convert complete_ast in
  print_string (Codegen.string_of_program semt);;

