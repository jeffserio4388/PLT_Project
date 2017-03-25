type token =
  | COLON
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | COMMA
  | PLUS
  | MINUS
  | STAR
  | DIVIDE
  | MOD
  | ASSIGN
  | NOT
  | DOT
  | DEREF
  | REF
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | SASSIGN
  | POINTER
  | LET
  | RETURN
  | IF
  | ELSE
  | FOR
  | INT
  | FLOAT
  | BOOL
  | CHAR
  | VOID
  | STRING
  | FUNCTION
  | STRUCT
  | CAST
  | TO
  | SET
  | PIPELINE
  | WHILE
  | STR_LIT of (string)
  | FLOAT_LIT of (float)
  | INT_LIT of (int)
  | CHAR_LIT of (char)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
