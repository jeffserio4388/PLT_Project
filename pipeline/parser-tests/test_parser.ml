open OUnit2;;

let token_list_of_string s =
  let lexbuf = Lexing.from_string s in
  let rec helper l =
    let t = Scanner.token lexbuf in
    if t = Parser.EOF then List.rev l else helper (t::l)
  in 
  helper []
;;

let testcases = [
  ("true", [Parser.TRUE]);
  ("false", [Parser.FALSE])
  ];;

let suite =
  "Tests">:::
  (List.map
     (fun (name,res) ->
        name >::
        (fun test_ctxt -> 
           let token = token_list_of_string name in
           assert_equal token res))
     testcases)
;;

let () =
  run_test_tt_main suite
;;

