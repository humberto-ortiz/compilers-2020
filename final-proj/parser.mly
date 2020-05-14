/* Tokens for add-sub */
%token <string> ID
%token <int64> INT64
%token <bool> BOOL
%token IF ELSE
%token PLUS MINUS TIMES AND OR LESS LESSEQ GREATER GREATEREQ EQUAL NOTEQ NOT
%token LET IN 
%token DEF
%token LPAREN RPAREN COMMA COLON
%token ADD1 SUB1
%token ASSIGN
%token EOF
%token EOL
%nonassoc UMINUS        /* highest precedence */

%start <Syntax.prog> main
%{ open Syntax %}
%%

main:
|p = prog EOF  { p }
;

prog:
| d_ls = decl_ls e = expr  { Decls (d_ls, e)  }    
| e = expr     { Expr e }

decl_ls:
| 
    { [] }
| d = decl
    { [d] }
| d = decl COMMA ds = decl_ls
    { d :: ds }

expr:
| i = INT64 
    { Num i }
| id = ID 
    { Id id }
| b = BOOL
    {Bool b}
| LPAREN e = expr RPAREN 
    { e }
| e1 = expr PLUS e2 = expr
    { BinOp (e1, Plus, e2) }
| e1 = expr MINUS e2 = expr
    { BinOp (e1, Minus, e2) }
| e1 = expr TIMES e2 = expr
    { BinOp (e1, Mult, e2) }
| e1 = expr AND e2 = expr
    { BinOp (e1, And, e2) }
| e1 = expr OR e2 = expr
    { BinOp (e1, Or, e2) }
| e1 = expr LESS e2 = expr
    { BinOp (e1, Less, e2) }
| e1 = expr LESSEQ e2 = expr
    { BinOp (e1, LessEq, e2) }
| e1 = expr GREATER e2 = expr
    { BinOp (e1, Greater, e2) }
| e1 = expr GREATEREQ e2 = expr
    { BinOp (e1, GreaterEq, e2) }
| e1 = expr EQUAL e2 = expr
    { BinOp (e1, Eq, e2) }
| e1 = expr NOTEQ e2 = expr
    { BinOp (e1, Ne, e2) }
| MINUS e = expr %prec UMINUS
    { BinOp (e, Mult, (Num (-1L))) }
| ADD1 e = expr 
    { Add1 e }
| SUB1 e = expr 
    { Sub1 e }
| LET id = ID ASSIGN e1 = expr IN e2 = expr 
    { Let (id,e1,e2) }
| IF e1 = expr COLON e2 = expr ELSE COLON e3 = expr
    { If (e1,e2,e3) }
| NOT e1 = expr
    { Not e1 }
| id = ID LPAREN p = params RPAREN
    { FuncCall (id,p) }
;

params:
|
    { [] }
| e = expr
    { [e] }
| e = expr COMMA es = params
    { e :: es }
;

/* def sum3(x, y, z):
  a + b + cdef sum3(x, y, z):
  a + b + c */

decl: 
 | DEF id = ID LPAREN vars = args RPAREN COLON e = expr
    { Func (id,vars,e) }
;

args:
 | { [] }
 | id = ID { [id] }
 | id = ID COMMA ars = args { id :: ars}
;
