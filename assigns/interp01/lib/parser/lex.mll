{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | "()" {UNIT}
  | "let" { LET }
  | "=" { EQ}
  | "in" { IN }
  | "+" { ADD }
  | "->" {ARROW}
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "||" {OR}
  | "&&" {AND}
  | "<" {LT}
  | "<=" {LTE}
  | ">" {GT}
  | ">=" {GTE}
  | "<>" {NEQ}
  | "mod" {MOD}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "fun" {FUN}
  | "true" {TRUE}
  | "false" {FALSE}
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }
