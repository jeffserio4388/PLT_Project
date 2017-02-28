{ 
	open Err
	open Parser
	open Lexing
}

rule token = parse
  [' ' '\t' '\r'] { token lexbuf } (*whitespace*)
(*punctuations*)
| '\n' { EOL }
| "*/" { token lexbuf }
| '+' { PLUS } (*operators*)
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '%' { MOD }
| ">=" { GEQ }
| "<=" { LEQ }
| "==" { EQUAL }
| "!=" { NEQ }
(*| "**" { SQUARE }*)
| '>' { GREATER }
| '<' { LESS }
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACK }
| ']' { RBRACK }
| '{' { LBRACE }
| '}'' { RBRACE }
| ',' { COMMA }
| '.' { NAMESPACE }    
| '=' { ASSIGN }
| ">>" { RSHIFT }
| "<<" { LSHIFT }
| "and" { AND }
| "or" { OR }
| "not" { NOT }
| ';'  { SEMI }
| ':'  { COLON }
| "string" { STRING }
| "point" { POINT }
| "if" { IF } (*controlling sequence*)
| "else" { ELSE }
| "elif" {ELIF}
| "end" { END }
| "for" { FOR }
| "while" { WHILE }
| "break" { BREAK }
| "continue" { CONTINUE }
| "print" { PRINT }
| "for" { FOR }
| "return" { RETURN }
| "int" { INT }
| "float" { FLOAT }
| "char" { CHAR }
| "pipe" {PIPE}
| "@" { POINTER }
| "&" { AMPERSAND }
| "fun" { FUNCTION }
| "pipe" {PIPELINE}
| "void" { VOID }
| "struct" { STRUCT }
| "string" { STRING }
| "coupling" {COUPLING}
| "catch" {CATCH}
| "fun" { FN }
| "return" { RETURN }
| '-'?(['0'-'9']+('.'['0'-'9']*)?) as lxm { LIT_NUM(float_of_string lxm) } (*Change to add negative*)
| ['"'][^'"']*['"'] as str { LIT_STR(str) }
| ['A'-'Z' 'a'-'z']+['A'-'Z' 'a'-'z' '0'-'9']* as i { ID(i) }
| "/*"  { multiLineComment lexbuf }
| _ as c { raise (Failure("Illegal character : " ^ Char.escaped c)) }

and multiLineComment = parse
|"*/"   { token lexbuf }
| _     { multiLineComment lexbuf }