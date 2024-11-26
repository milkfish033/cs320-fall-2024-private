%{
open Utils


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
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token LT
%token LTE
%token GT
%token GTE
%token NEQ
%token AND
%token OR
%token ASSERT
%token UNIT
%token TRUE
%token FALSE  

%right ARROW
%right OR 
%right AND 
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD


%start <Utils.prog> prog

%%

prog:
    | ls = toplet* EOF{ls}

toplet: 
    | LET; x = VAR; a = arg*; COLON; t = ty; EQ; e = expr
        { {is_rec = false; name = x; args = a; ty = t; value = e}}
    | LET; REC; x = VAR; a = arg; a1 =  arg*; COLON; t = ty; EQ; e = expr 
        { {is_rec = true; name = x; args = (a ::a1); ty = t; value = e}}

arg:
    | LPAREN; x = VAR; COLON; t = ty; RPAREN 
        {x, t} 

ty: 
    | INT {IntTy}
    | BOOL {BoolTy}
    | TYUNIT {UnitTy}
    | t1 = ty; ARROW; t2 = ty {FunTy (t1, t2)}
    | LPAREN; t  = ty; RPAREN {t}


expr:
    |LET; x = VAR; a = arg*; COLON; t = ty;EQ; e = expr; IN; e1 = expr
        {SLet {is_rec = false; name = x; args = a; ty =t; value = e; body = e1}}
    |LET; REC x = VAR; a = arg; a1 = arg* ; COLON; t = ty; EQ; e = expr; IN; e1 = expr 
        {SLet {is_rec = true; name = x; args =  a :: a1; ty = t; value = e; body = e1}}
    |IF; e = expr; THEN; e1 = expr; ELSE; e2 = expr 
        {SIf (e, e1, e2)}
    |FUN; a = arg; a1 = arg*; ARROW;e = expr 
        {SFun {arg = a; args = a1; body =  e} }
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
        {SBop (op, e, e1)}
    | ASSERT; e = expr3
        {SAssert (e)}
    | e = expr3; e1 = expr3* {List.fold_left (fun e1 e2 -> SApp (e1, e2)) e e1}
    
expr3:
    | UNIT {SUnit}
    | TRUE {STrue}
    | FALSE {SFalse}
    | n = NUM {SNum n}
    | x = VAR {SVar x}
    | LPAREN; e = expr; RPAREN {e}