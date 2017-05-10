(* Ocamllex scanner for MicroC *)

{
    open Parser
    let lineno = ref 1
}


let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let string_t = '"' ( (ascii | escape)*) '"'


rule token = parse
  [' ' '\t' '\r']       { token lexbuf }             (* Whitespace *)
| '\n'                  { incr lineno; token lexbuf }
| "/*"                  { comment lexbuf }           (* Comments *)
| '('                   { LPAREN }
| ')'                   { RPAREN }
| '{'                   { LBRACE }
| '}'                   { RBRACE }
| '['                   { LSBRACE }
| ']'                   { RSBRACE }
| ';'                   { SEMI }
| ','                   { COMMA }
| '+'                   { PLUS }
| '-'                   { MINUS }
| '*'                   { TIMES }
| '/'                   { DIVIDE }
| '$'                   { CONCAT}
| '='                   { ASSIGN }
| '.'                   { DOT }
| '%'                   { MOD }
| "=="                  { EQ }
| "!="                  { NEQ }
| '<'                   { LT }
| "<="                  { LEQ }
| ">"                   { GT }
| ">="                  { GEQ }
| "&&"                  { AND }
| "||"                  { OR }
| "!"                   { NOT }
| "if"                  { IF }
| "else"                { ELSE }
| "for"                 { FOR }
| "while"               { WHILE }
| "return"              { RETURN }
| "int"                 { INT }
| "float"               { FLOAT }
| "bool"                { BOOL }
| "void"                { VOID }
| "true"                { TRUE }
| "false"               { FALSE }
| "addleft"             {ADDLEFT}
| "addright"            {ADDRIGHT}
| "popleft"             {POPLEFT}
| "popright"            {POPRIGHT}
| "string"		        { STRING }
| "struct"              { STRUCT }
| "pipe"                { PIPE }
| "function"            { FUNCTION }
| "listen"		        { LISTEN }
| "http" 		    { HTTP }
| "global"              { GLOBAL }
| "File"                { FILE }
| "struct"              { STRUCT }
| string_t as str       { STR_LIT(str) }
| ['0'-'9']+ as lxm     { LITERAL(int_of_string lxm) }
| ['0'-'9']+ ['.']['0'-'9']+ as lxm {FLOAT_LIT(float_of_string lxm)}
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
