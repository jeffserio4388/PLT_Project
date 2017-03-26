(* Ocamllex scanner for DaMPL *)

{ open Parser }


let digit = ['0'-'9']
let exponent = ['e' 'E']['+' '-']?digit+
let decimal = (digit+"."digit*)|(digit*"."digit+)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACK }
| ']'      { RBRACK }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ':'	   { COLON }
| ';'      { SEMI }
| ','      { COMMA }
| '@'	   { AT }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }

| "pipe"   { PIPELINE }

| "true"   { TRUE }
| "false"  { FALSE }


| "include"  { INCLUDE }
| "tuple"    { TUPLE }
| "$"        { DOLLAR }
| "break"    { BREAK }
| "continue" { CONTINUE }
| "function"      { FUN }
| "in"       { IN }

| "real"	 { REAL }
| "integer"  { INTEGER }
| "text"	 { TEXT }

| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| (decimal exponent?)|(digit+exponent) as lxm { FLOAT(float_of_string lxm) }
| '"'([^'"']|("\\\""))*'"' as lxm { STRING(lxm) }
| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { TID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
