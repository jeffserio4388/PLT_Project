{ 
	open Parser
	open Lexing
        let lineno = ref 1
}

let whitespace = [' ' '\t' '\r']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let alpha = ['a'-'z' 'A'-'Z']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = (['0'-'9'])
let id = alpha (alpha | digit | '_')*
let string_t = '"' ( (ascii | escape)* as s) '"'
let char_t = ''' (ascii | digit) '''
let float_t = (digit+)? ['.'] digit+
let int_t = digit+

rule token = parse
  whitespace    { token lexbuf }
| '\n'          { incr lineno; token lexbuf }
| "/*"          { multiLineComment lexbuf }

(*punctuation*)
| '('   { LPAREN }
| ')'   { RPAREN }
| '{'   { LBRACE }
| '}'   { RBRACE }
| ','   { COMMA }
| ';'   { SEMI }

(*operators*)
| '+'   { PLUS } 
| '-'   { MINUS }
| '*'   { STAR }
| '/'   { DIVIDE }
| '%'   { MOD }
| ">="  { GEQ }
| "<="  { LEQ }
| "=="  { EQ }
| "!="  { NEQ }
| '>'   { GT }
| '<'   { LT }
| '['   { LBRACK }
| ']'   { RBRACK }    
| '='   { ASSIGN }
| "and" { AND }
| "or"  { OR }
| "not" { NOT }

(*types*)
| "int"     { INT }
| "float"   { FLOAT }
(*| "char"    { CHAR } *)
| "string"  { STRING }
| "void"    { VOID }

(*declarations*)
| "fun"         { FUNCTION }
| "pipe"        { PIPELINE }
| "struct"      { STRUCT }

(*control sequence*)
| "if"          { IF }
| "else"        { ELSE }
| "for"         { FOR }
| "while"       { WHILE }
(*
| "break"       { BREAK }
| "continue"    { CONTINUE }
| "catch"       { CATCH } 
*)
| "return"      { RETURN }

| float_t as lxm      { FLOAT_LIT(float_of_string lxm) }
| int_t as lxm        { INT_LIT(int_of_string lxm) }
| string_t as str     { STR_LIT(str) }
(*| char_t as ch        { CHAR_LIT(ch) }*)
(*| escape_char as es { ESC_CHAR(es) }*)
| id as i           { ID(i) }
| eof               { EOF }

and multiLineComment = parse
|"*/"   { token lexbuf }
| _     { multiLineComment lexbuf }
