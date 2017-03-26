open Ast
open Parser


(*
let varArray = Array.make 26 0;;

let rec eval = function 
    Lit(x) -> x
  | Var(x) -> varArray.(x)
  | Binop(e1, op, e2) ->
      (let v1 = eval e1 and v2 = eval e2 in
      match op with
	     Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2 )
  | Asn(v,e) ->
    varArray.(v) <- eval e;
    varArray.(v)
  | Seq(e1,e2) ->
    ignore (eval e1);
    eval e2
*)




let string_of_token tk = match tk with
   SEMI -> "SEMI"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIVIDE -> "DIVIDE"
  | ASSIGN -> "ASSIGN"
  | NOT -> "NOT"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | LEQ -> "LEQ"
  | GT -> "GT"
  | GEQ -> "GEQ"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | AND -> "AND"
  | OR -> "OR"
  | RETURN -> "RETURN"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | FOR -> "FOR"
  | WHILE -> "WHILE"
  | INCLUDE -> "INCLUDE"
  | TUPLE -> "TUPLE"
  | DOLLAR -> "DOLLAR"
  | BREAK -> "BREAK"
  | CONTINUE -> "CONTINUE"
  | FUN -> "FUN"
  | IN -> "IN"
  | LITERAL(_) -> "LITERAL"
  | ID(_) -> "ID"
  | TID(_) -> "TID"
  | FLOAT(_) -> "FLOAT"
  | STRING(_) -> "STRING"
  | AT -> "AT"
  | EOF -> "EOF";;

let rec string_of_tokens tks = match tks with
  a::l -> (string_of_token a) ^ " " ^ (string_of_tokens l)
  | [] -> "";;

(*
let aa =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Scanner.token lexbuf in
  let out = string_of_token expr in
  print_endline (out);;
*)

(* let a = *)
 (* 
let paraporra = ref false;;

let lexbuf = Lexing.from_channel stdin in
while not (!paraporra) do
  let result = Scanner.token lexbuf in
    let out = string_of_token result in
    (ignore(paraporra := (out = "EOF" ));
    print_endline (out))
done

*)


  let lexbuf = Lexing.from_channel stdin in
  let rec loop acc =  function
      | EOF   ->  string_of_token EOF :: acc |> List.rev
      | x     ->  loop (string_of_token x :: acc) (Scanner.token lexbuf)
  in
      loop [] (Scanner.token lexbuf) 
      |> String.concat " " 
      |> print_endline
