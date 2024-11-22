

{open Par}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | eof {EOF}
  | "let" {LET}
  | ":" {COLON}
  | "=" {EQ}
  | "rec" {REC}
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "int" {INT}
  | "bool" {BOOL}
  | "unit" {TYUNIT}
  | "->" {ARROW}
  | "in" {IN}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "fun" {FUN}
  | "assert" {ASSERT}
  | "()" {UNIT}
  | "true" {TRUE}
  | "false" {FALSE}
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "||" {OR}
  | "&&" {AND}
  | "<" {LT}
  | "<=" {LTE}
  | ">" {GT}
  | ">=" {GTE}
  | "<>" {NEQ}
  | "mod" {MOD}
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
