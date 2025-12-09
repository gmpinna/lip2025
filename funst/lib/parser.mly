%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token PLUS
%token MINUS
%token MUL
%token EQ
%token LEQ
%token LESS
%token <string> ID
%token <string> CONST
%token LPAREN
%token RPAREN
%token EOF
%token IF
%token THEN
%token ELSE
%token LET
%token TAKES
%token IN
%token FUN
%token REC
%token APPL
%token COMMA

%nonassoc ELSE IN
%left OR
%left AND
%nonassoc EQ LEQ LESS
%left PLUS MINUS
%left MUL
%nonassoc NOT

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Eint (int_of_string n) }
  | TRUE { Ebool true }
  | FALSE { Ebool false }
  | NOT; e=expr { Not e }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | e1=expr; PLUS; e2=expr { Add(e1,e2) }
  | e1=expr; MINUS; e2=expr { Sub(e1,e2) }
  | e1=expr; MUL; e2=expr { Mul(e1,e2) }
  | e1=expr; EQ; e2=expr { Eq(e1,e2) }
  | e1=expr; LEQ; e2=expr { Leq(e1,e2) }
  | e1=expr; LESS; e2=expr { Less(e1,e2) }
  | LPAREN; e = expr; RPAREN { e }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | LET; x = ID; TAKES; e1 = expr; IN; e2 = expr; { Let(x,e1,e2) } 
  | FUN; LPAREN; x = ID; COMMA; e = expr; RPAREN { Fun(x,e) } 
  | REC; LPAREN; x = ID; COMMA; e = expr; RPAREN  { Rec(x,e) } 
  | APPL; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN { Appl(e1,e2) } 
  | x = ID { Var(x) }
;

