%{
open Utils
%}

%token LET
%token <int> NUM
%token <string> VAR
%token EQUALS
%token IN
%token EOF
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token LT
%token LTE
%token GT
%token GTE
%token EQ
%token NEQ
%token AND
%token OR
%token LPAREN
%token RPAREN
%token IF
%token THEN 
%token ELSE
%token ARROW
%token FUN



%left LT LTE GT GTE EQ NEQ
%right OR AND 
%left ADD SUB MUL DIV MOD

%
%start <Utils.prog> prog

%%

prog:
  | e = expr; EOF {e}

expr: 
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr
    {If (e1, e2, e3)}
  | LET; x= VAR; EQUALS; e1 = expr; IN; e2 = expr 
    {Let (x, e1, e2)}
  | FUN; x = VAR; ARROW; e1 = expr;
    {Fun (x, e1)}
  | e = expr2 {e}


%inline bop:
  | ADD {Add}
  | SUB {Sub}
  | MUL {Mul}
  | DIV {Div}
  | MOD {Mod}
  | LT {Lt}
  | LTE {Lte}
  | GT {Gt}
  | GTE {Gte}
  | EQ {Eq}
  | NEQ {Neq}
  | AND {And}
  | OR {Or}


expr2:
  | e1 = expr2; op = bop; e2 = expr2; 
    {Bop (op, e1, e2)}
  | #more here


expr3:
  |LPAREN;RPAREN; 
  #more here


  | n = NUM {Num n}; x = VAR {Var x}
  | LPAREN; e = expr; RPAREN {e}