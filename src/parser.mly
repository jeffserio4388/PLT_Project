/* Ocamlyacc parser for Pipeline */

%{
	open Ast;;

	let first   (a,_,_,_,_) = a;;
	let second  (_,b,_,_,_) = b;;
	let third   (_,_,c,_,_) = c;;
	let fourth  (_,_,_,d,_) = d;;
	let fifth   (_,_,_,_,e) = e;;
    let pipe_name = ref 0;;
%}

	%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LSBRACE RSBRACE DOT
	%token PLUS MINUS TIMES DIVIDE ASSIGN NOT 
	%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR CONCAT
	%token RETURN IF ELSE FOR WHILE INT BOOL VOID STRING STRUCT GLOBAL FLOAT FILE
	%token PIPE FUNCTION LISTEN HTTPGET HTTPPUT HTTPDELETE HTTPPOST
    %token ADDLEFT ADDRIGHT POPLEFT POPRIGHT
	%token <int> LITERAL
	%token <float> FLOAT_LIT
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
	%left PLUS MINUS CONCAT
	%left TIMES DIVIDE
	%right NOT NEG
    %left DOT

	%start program
	%type <Ast.program> program

	%%

	program:
	decls EOF           { $1 }

decls:
/* nothing */       { [], [], [], [], [] }
| decls global     { ($2 :: first $1), second $1, third $1, fourth $1, fifth $1 }
| decls stmt        { first $1, ($2 :: second $1), third $1, fourth $1, fifth $1 }
| decls fdecl       { first $1, second $1, ($2 :: third $1), fourth $1, fifth $1 }
| decls pdecl       { first $1, second $1, third $1, ($2 :: fourth $1), fifth $1 }
| decls sdecl       { first $1, second $1, third $1, fourth $1, ($2 :: fifth $1) }

sdecl:
STRUCT ID LBRACE vdecl_list RBRACE SEMI
{ {
	sname = $2;
	vars = List.rev $4;
} }


listen:
LISTEN LPAREN STR_LIT COMMA LITERAL RPAREN SEMI
{{
	arg1 = $3;
	arg2 = $5;
}}

listen_opt:
    /* nothing */   { [] }
    | listen   { [$1] }


pdecl:
PIPE LBRACE listen_opt stmt_list RBRACE
{ 
    pipe_name := !pipe_name + 1;
    { 
    	pname = "pipe_" ^ string_of_int !pipe_name;
    	listen = $3;
    	body = List.rev $4;
    }
}

vdecl:
    typ ID SEMI {($1,$2, Noexpr)}




fdecl:
FUNCTION typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
{ { 
	typ = $2;
	fname = $3;
	formals = $5;
	body = List.rev $8 
} }

formals_opt:
/* nothing */ { [] }
| formal_list   { List.rev $1 }

formal_list:
typ ID                   { [($1,$2)] }
| formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
INT         { Int }
| BOOL      { Bool }
| VOID      { Void }
| STRING    { MyString }
| FLOAT     { Float }
| FILE      { File }
| STRUCT ID { Struct($2) }

vdecl_list:
/* nothing */    { [] }
| vdecl_list vdecl { $2 :: $1 }

global:
  GLOBAL typ ID SEMI { ($2, $3, Noexpr) }
| GLOBAL typ ID ASSIGN expr SEMI { ($2,$3,$5) }


stmt_list:
/* nothing */  { [] }
| stmt_list stmt { $2 :: $1 }


stmt:
expr SEMI                                                     { Expr $1 }
| RETURN SEMI                                                 { Return Noexpr }
| RETURN expr SEMI                                            { Return $2 }
| LBRACE stmt_list RBRACE                                     { Block(List.rev $2) }
/*| LBRACE stmt_list RBRACE                                   { Block($2) }*/
| IF LPAREN expr RPAREN stmt %prec NOELSE                     { If($3, $5, Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt                        { If($3, $5, $7) }
| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt     { For($3, $5, $7, $9) }
| WHILE LPAREN expr RPAREN stmt                               { While($3, $5) }
/*| LIST ID ASSIGN STRING LPAREN stringlit_list RPAREN SEMI     { Str_list_decl($2, $6) }*/
/*| LIST ID ASSIGN INT LPAREN literal_list RPAREN SEMI          { Int_list_decl($2, $6) }*/ 
/*| ADDLEFT LPAREN expr COMMA expr RPAREN SEMI                  { Add_left($3, $5) }
| ADDRIGHT LPAREN expr COMMA expr RPAREN SEMI                 { Add_left($3, $5) }
| FINDNODE LPAREN expr COMMA expr COMMA expr RPAREN SEMI      { Add_left($3, $5) }
| HTTPGET LPAREN expr COMMA expr RPAREN SEMI 		      { Http_get($3, $5) }
| HTTPPUT LPAREN expr COMMA expr RPAREN SEMI		      { Http_put($3, $5) }
| HTTPDELETE LPAREN expr COMMA expr RPAREN SEMI		      { Http_delete($3, $5) }
| HTTPPOST LPAREN expr COMMA expr RPAREN SEMI		      { Http_post($3, $5) }*/
| typ ID SEMI                                             {Local($1,$2, Noexpr)}
| typ ID ASSIGN expr SEMI                                 {Local($1,$2,$4)}
| typ ID LSBRACE RSBRACE SEMI                             {List(List_t($1),$2)}
/*| STRUCT ID ID SEMI                                       {Struct($2,$3)} */

expr_opt:
/* nothing */ { Noexpr }
| expr          { $1 }

expr:
LITERAL                         { Literal($1) }
| TRUE                          { BoolLit(true) }
| FALSE                         { BoolLit(false) }
| ID                            { Id($1) }
| FLOAT_LIT                     { FloatLit($1) }
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
| expr DOT    expr              { StructAccess($1, $3) }
| expr CONCAT expr              { Concat($1,$3) }
| MINUS expr %prec NEG          { Unop(Neg, $2) }
| NOT expr                      { Unop(Not, $2) }
| ID ASSIGN expr                { Assign($1, $3) }
| ID LPAREN actuals_opt RPAREN  { Call($1, $3) }
| LPAREN expr RPAREN            { $2 }
| ID LSBRACE LITERAL RSBRACE    { Access($1,$3) }
| ADDLEFT LPAREN ID COMMA expr RPAREN  {Addleft($3,$5)}
| ADDRIGHT LPAREN ID COMMA expr RPAREN {Addright($3,$5)}
| POPLEFT LPAREN ID   RPAREN {Popleft($3)}
| POPRIGHT LPAREN ID  RPAREN {Popright($3)}


actuals_opt:
/* nothing */ { [] }
| actuals_list  { List.rev $1 }

actuals_list:
expr                    { [$1] }
| actuals_list COMMA expr { $3 :: $1 }
