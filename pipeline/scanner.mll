{ 
	open Err
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
let string = '"' ( (ascii | escape)* as s) '"'
let char = ''' (ascii | digit) '''
let float = (digit+)? ['.'] digit+
let int = digit+

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
| '*'   { TIMES }
| '/'   { DIVIDE }
| '%'   { MOD }
| ">="  { GEQ }
| "<="  { LEQ }
| "=="  { EQUAL }
| "!="  { NEQ }
| '>'   { GREATER }
| '<'   { LESS }
| '['   { LBRACK }
| ']'   { RBRACK }
| '.'   { NAMESPACE }    
| '='   { ASSIGN }
| ">>"  { RSHIFT }
| "<<"  { LSHIFT }
| "and" { AND }
| "or"  { OR }
| "not" { NOT }
| "@"   { POINTER }
| "&"   { REFERENCE }

(*types*)
| "int"     { INT }
| "float"   { FLOAT }
| "char"    { CHAR }
| "string"  { STRING }
| "void"    { VOID }

(*declarations*)
| "fun"         { FUNCTION }
| "pipe"        { PIPELINE }
| "struct"      { STRUCT }

(*control sequence*)
| "if"          { IF }
| "else"        { ELSE }
| "elif"        { ELIF }
| "end"         { END } (* what is this used for? *)
| "for"         { FOR }
| "while"       { WHILE }
| "break"       { BREAK }
| "continue"    { CONTINUE }
| "catch"       { CATCH }
| "return"      { RETURN }

| float as lxm      { FLOAT_LIT(float_of_string lxm) }
| int as lxm        { INT_LIT(int_of_string lxm) }
| string as str     { STR_LIT(str) }
| char as ch        { CHAR_LIT(ch) }
| escape_char as es { ESC_CHAR(es) }
| id as i           { ID(i) }
| eof               { EOF }

and multiLineComment = parse
|"*/"   { token lexbuf }
| _     { multiLineComment lexbuf }
