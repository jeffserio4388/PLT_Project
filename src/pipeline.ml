(* Top-level of the Pipeline compiler: scan & parse the input,
   check the resulting AST and generate output C file *)

type action = Ast | Translate | Compile | Run | Compare

let read_process command =
  	let buffer_size = 2048 in
  	let buffer = Buffer.create buffer_size in
  	let string = Bytes.create buffer_size in
  	let in_channel = Unix.open_process_in command in
  	let chars_read = ref 1 in
  	while !chars_read <> 0 do
    	chars_read := input in_channel string 0 buffer_size;
    	Buffer.add_substring buffer string 0 !chars_read
  	done;
  	ignore (Unix.close_process_in in_channel);
    Buffer.contents buffer

let _ =
    let action = 
        if 
            Array.length Sys.argv > 1 
        then
            List.assoc Sys.argv.(1) [   ("-a", Ast);	    (* Print the AST *)
                                        ("-t", Translate);  (* Translate the program to output c file *)
                                        ("-c", Compile);    (* Compile the output c file *)
                                        ("-r", Run);        (* Run the program *)
                                        ("-d", Compare) ]   (* Compare output and expected output *)
        else 
            Compile in
            let splitFileExt s = Str.split (Str.regexp "\\.p") s in
            let fullName = Sys.argv.(2) in
            let fullNameStub = List.hd (splitFileExt fullName) in
            let program = open_in fullName in 
            let lexbuf = Lexing.from_channel program in
            let ast = Parser.program Scanner.token lexbuf in
            Semant.check ast;
            match action with
                Ast -> print_string (Codegen.translate ast);
                | Translate | Compile | Run | Compare -> ();
            match action with
                Ast -> ();
                | Translate | Compile | Run | Compare -> 
                        let new_file_name = fullNameStub ^ ".c" in
                        let oc = open_out new_file_name in 
                        Printf.fprintf oc "%s" (Codegen.translate ast); 
                        close_out oc;
            match action with
                Ast | Translate -> ();
                | Compile | Run -> 
                        let new_file_name = fullNameStub ^ ".c"in
                        let gen_o_file = 
                            "gcc -g -Wno-unused-variable -Wno-unused-but-set-variable -c " ^ new_file_name in
                        let gen_exec = 
                            "gcc -g -Wno-unused-variable -I" ^ 
                            "../stdlib/ -g -L../stdlib/ -o" ^ fullNameStub ^ " " ^
                            new_file_name ^ " -lmylist -luv"
                        in
                        ignore(read_process gen_o_file);
                        ignore(read_process gen_exec);
                | Compare->
                        let new_file_name = fullNameStub ^ ".c"in
                        let gen_o_file = 
                            "gcc -g -Wall -Wno-unused-variable -c " ^ 
                            new_file_name ^ " > " ^ fullNameStub ^ ".out 2>&1" in
                        let gen_exec = 
                            "gcc -g -Wall -Wno-unused-variable -I" ^ 
                            "../stdlib/ -g -L../stdlib/ -o" ^ fullNameStub ^ " " ^
                            new_file_name ^ " -lmylist -luv" 
                        in
                        ignore(read_process gen_o_file);
                        ignore(read_process gen_exec);
            match action with
                Ast | Translate | Compile -> ();
                | Run | Compare -> 
                 let executable = "./" ^ fullNameStub ^ " > " in
                 ignore(read_process (executable ^ fullNameStub ^ ".out")); 
            match action with
                Ast | Translate | Compile | Run -> ();
                | Compare -> let a = "diff " ^ fullNameStub ^ ".out 2>&1 " ^ fullNameStub ^ ".expected > " ^ fullNameStub ^ ".diff" in 
                    ignore(read_process a);



