%{
open Utils

let rec mk_app e es =
  match es with
  | [] -> e
  | x :: es -> mk_app (App (e, x)) es
%}

%token <int> NUM
%token <string> VAR
%token EOF
%token LET 
%token COLON 
%token EQ
%token REC
%token LPAREN
%token RPAREN
%token INT
%token BOOL 
%token TYUNIT
%token ARROW
%token IN
%token IF 
%token THEN 
%token ELSE
%token FUN 
%token ASSERT
%token UNIT
%token TRUE
%token FALSE  


%right OR 
%right AND 
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD


%start <Utils.prog> prog

%%

prog:
    | x = (e = toplet; e1 = toplet* {mk_app e e1}); EOF{x}

toplet: 
    | LET; x = VAR; a = (a1 = arg; a2 = arg* {mk_app a1 a2}); COLON; t = ty; EQ; e = expr
        {toplet (false; x; a; t; e)} 
    | LET; REC; x = VAR; a = arg; a1 = (a2 = arg; a3 = arg* {mk_app a2 a3}); 
        COLON; t = ty; EQ; e = expr 
        {toplet(true; x; a; a1; t; e)}

arg:
    | LPAREN; x = VAR; COLON; t = ty; RPAREN 
        {arg( x; t)}

ty: 
    | INT {ty IntTy}
    | BOOL {ty BoolTy}
    | TYUNIT {ty UnitTy}
    | t1 = ty; ARROW; t2 = ty {ty (FunTy t1; t2)}
    | LPAREN; t = ty; RPAREN {ty (FunTy t; t)}

expr:
    |LET; x = VAR; a = (a1 = arg; a2 = arg* {mk_app a1 a2}); COLON; t = ty;
        EQ; e = expr; IN; e1 = expr
        {Let (false; x ; a; t; e; e1)}
    |LET; REC x = VAR; a = arg; a1 = (a2 = arg; a3 = arg* {mk_app a2 a3}); 
        COLON; t = ty; COLON; e = expr; IN; e1 = expr 
        {Let (true; x; a; a1; t; e; e1)}
    |IF; e = expr; THEN; e1 = expr; ELSE; e2 = expr 
        {If (e, e1, e2)}
    |FUN; a = arg; a1 = (a2 = arg; a3 = arg* {mk_app a2 a3}); ARROW;
        e = expr 
        {a; a1; expr}
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
    | e = expr2; op = bop; e1 = expr2
        {Bop (op, e, e1)}
    | ASSERT; e = expr3 
        {Assert (e)}
    | e = expr3; e1 = expr3* {mk_app e e1}
    
expr3:
    | UNIT {Unit}
    | TRUE {True}
    | FALSE {False}
    | n = NUM {Num n}
    | x = VAR {Var x}
    | LPAREN; e = expr; RPAREN {e}