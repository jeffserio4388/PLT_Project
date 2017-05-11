type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | LSBRACE
  | RSBRACE
  | DOT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | MOD
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
  | CONCAT
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | VOID
  | STRING
  | STRUCT
  | GLOBAL
  | FLOAT
  | FILE
  | PIPE
  | FUNCTION
  | LISTEN
  | HTTP
  | ADDLEFT
  | ADDRIGHT
  | POPLEFT
  | POPRIGHT
  | LITERAL of (int)
  | FLOAT_LIT of (float)
  | STR_LIT of (string)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
