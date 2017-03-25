%{
open Ast;;

let first (a,_,_) = a;;
let second (_,b,_) = b;;
let third (_,_,c) = c;;
%}

%token COLON SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA 
%token PLUS MINUS STAR DIVIDE MOD ASSIGN NOT DOT DEREF REF
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR SASSIGN POINTER
%token LET RETURN IF ELSE FOR INT FLOAT BOOL CHAR VOID STRING FUNCTION STRUCT CAST TO SET PIPELINE WHILE
%token <string> STR_LIT
%token <float> FLOAT_LIT
%token <int> INT_LIT
%token <char> CHAR_LIT
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
%left STAR DIVIDE MOD
%right NOT NEG DEREF REF
%left DOT

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [], [] }
 | decls vdecl { ($2 :: first $1), second $1, third $1 }
 | decls fun_decl { first $1, ($2 :: second $1), third $1 }
 | decls struct_decl { first $1, second $1, ($2 :: third $1) }
/* add 
  | decls pipe_decl
  | decls array_decl     
*/


fun_decl:
   /*Add typ_opt as per grammar in wiki */
   /* statement before declaring anything fails because of*/
   FUNCTION ID LPAREN formals_opt RPAREN typ LBRACE vdecl_list stmt_list RBRACE
     { { typ = $6;

	 fname = $2;
	 formals = $4;
	 locals = List.rev $8;
	 body = List.rev $9 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT { Int }
  | FLOAT { Float }
  | CHAR {Char}
  | BOOL { Bool }
  | VOID { Void }
  | STRING { MyString }
  | STRUCT ID { StructType ($2) }
  | STAR typ %prec POINTER{ PointerType ($2) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   LET ID typ SEMI { ($3, $2) }

struct_decl:
    STRUCT ID LBRACE vdecl_list RBRACE
      { { sname = $2;
      sformals = $4;
      } }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | typ ID ASSIGN expr SEMI {SAssign($1, $2, $4)}

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INT_LIT          { Literal($1) }
  | FLOAT_LIT     { FloatLiteral($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | STR_LIT	     { MyStringLit($1) }
  /* | CHAR_LIT            { CharLit($1) } */
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr STAR  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr MOD expr { Binop($1, Mod,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | expr DOT    ID   { Dotop($1, $3) }
/*  | expr DOT    ID ASSIGN expr { SAssign($1, $3, $5) } */
  | CAST expr TO typ { Castop($4, $2) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | STAR expr %prec DEREF { Unop(Deref, $2) }
  | REF expr { Unop(Ref, $2) }
  | NOT expr         { Unop(Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
