/* Ocamlyacc parser for DaMPL */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA COLON AT
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE
%token INCLUDE TUPLE DOLLAR BREAK CONTINUE FUN IN PIPELINE
%token REAL INTEGER TEXT
%token <int> LITERAL
%token <float> FLOAT
%token <string> STRING
%token <string> ID
%token <string> TID
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
%type <Ast.program_with_headers> program

%%

program:
  program_with_headers EOF { $1 }

program_with_headers:
    includs decls { $1, $2 }
  | decls { [], ($1) }

includs:
    includs includ  { $2 :: $1 }
  | includ { [$1] }

includ:
  | INCLUDE STRING SEMI { FileIncl($2) }

decls:
   /* nothing */ { [], [] }
 | stmt decls { ($1 :: fst $2), snd $2 }
 | tdecl_or_fdecl decls { fst $2, ($1 :: snd $2) }
 
tdecl_or_fdecl:
    fdecl { $1 }
  | tdecl { $1 }

fdecl:
  FUN ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { Func({ fname = $2;
	 formals = $4;
	 locals = [];
	 body = List.rev $7 }) }

tdecl:
  TUPLE TID LBRACE tup_item_list RBRACE { Tup($2, List.rev $4) }

tup_item_list:
    tup_item                   { [$1] }
  | tup_item_list COMMA tup_item { $3 :: $1 }

tup_item:
    ID      { (String, $1) }
  | ID COLON tup_typ { ($3, $1) }

tup_typ:
    INTEGER   { Int }
  | REAL      { Float }
  | TEXT      { String }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR ID IN expr LBRACE stmt_list RBRACE { For($2, $4, Block($6)) }
  | FOR ID IN expr COLON stmt { For($2, $4, $6) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | BREAK SEMI { Break }
  | CONTINUE SEMI { Continue }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }


obj:
    ID               { Id($1) }
  | obj LBRACK expr RBRACK { Brac($1,$3,false) }

appended_obj:
    obj               { $1 }
  | obj DOLLAR ID     { Attr($1,$3) }
  | obj DOLLAR LPAREN expr RPAREN     { AttrInx($1,$4) }
  | obj LBRACK expr_opt COLON expr_opt RBRACK { Brac2($1,$3,$5) }
  | obj LBRACK RBRACK { Brac($1,Noexpr,false) }

lhs_appended_obj:
    appended_obj      { $1 }
  | AT obj LBRACK expr RBRACK { Brac($2,$4,true) }


/* | obj DOLLAR LPAREN LITERAL RPAREN     { AttrInx($1, Literal($4) ) } */
  

expr_opt:
    /* Nothing */ { Noexpr }
  | expr          { $1 }


expr:
    LITERAL          { Literal($1) }
  | FLOAT            { FloatLit($1) }
  | STRING           { StrLit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | appended_obj     { Obj($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | lhs_appended_obj ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | TID { TupInst($1) }
  | TID LBRACK RBRACK { TabInst($1) }
  | LBRACK actuals_opt RBRACK { Arr($2) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
