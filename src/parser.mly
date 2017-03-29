/* Ocamlyacc parser for MicroC */

%{
open Ast;;

let first (a,_,_,_) = a;;
let second (_,b,_,_) = b;;
let third (_,_,c,_) = c;;
let fourth (_,_,_,d) = d;;
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL VOID STRING
%token PIPE
%token <int> LITERAL
%token <string> STR_LIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ 
   { [], [], [], [] }
 | decls vdecl { ($2 :: first $1), second $1, third $1, fourth $1 }
 | decls stmt { first $1, ($2 :: second $1), third $1, fourth $1 }
 | decls fdecl { first $1, second $1, ($2 :: third $1), fourth $1 }
 | decls pdecl { first $1, second $1, third $1, ($2::fourth $1) }

pdecl:
    PIPE ID LBRACE vdecl_list stmt_list RBRACE
    { { 
        pname = $2;
        locals = List.rev $4;
        body = List.rev $5 
    } }

pdecl_list:
    /* nothing */    { [] }
  | pdecl_list pdecl { $2 :: $1 }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT { Int }
    | BOOL { Bool }
    | VOID { Void }
    | STRING { MyString }


vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  /*| LBRACE stmt_list RBRACE { Block($2) }*/
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL                         { Literal($1) }
    | TRUE                          { BoolLit(true) }
    | FALSE                         { BoolLit(false) }
    | ID                            { Id($1) }
    | STR_LIT                       { MyStringLit($1) }
    | expr PLUS   expr              { Binop($1, Add,   $3) }
    | expr MINUS  expr              { Binop($1, Sub,   $3) }
    | expr TIMES  expr              { Binop($1, Mult,  $3) }
    | expr DIVIDE expr              { Binop($1, Div,   $3) }
    | expr EQ     expr              { Binop($1, Equal, $3) }
    | expr NEQ    expr              { Binop($1, Neq,   $3) }
    | expr LT     expr              { Binop($1, Less,  $3) }
    | expr LEQ    expr              { Binop($1, Leq,   $3) }
    | expr GT     expr              { Binop($1, Greater, $3) }
    | expr GEQ    expr              { Binop($1, Geq,   $3) }
    | expr AND    expr              { Binop($1, And,   $3) }
    | expr OR     expr              { Binop($1, Or,    $3) }
    | MINUS expr %prec NEG          { Unop(Neg, $2) }
    | NOT expr                      { Unop(Not, $2) }
    | ID ASSIGN expr                { Assign($1, $3) }
    | ID LPAREN actuals_opt RPAREN  { Call($1, $3) }
    | LPAREN expr RPAREN            { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }