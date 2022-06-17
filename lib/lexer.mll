{
  open Parser
}

let blank = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"

let digit = ['0'-'9']
let number = digit+

let ulph = ['_' 'a'-'z' 'A'-'Z']
let ulnum = ulph | digit
let var = ulph ulnum*

rule token = parse
  | '#' { HASH }
  | "lang" blank* (number as n) { LANG (int_of_string n) }
  | blank { token lexbuf }
  | newline { token lexbuf }
  | number as n { NUM (int_of_string n) }
  | "[]" { HOLE }
  | '(' { LPAR }
  | ')' { RPAR }
  | ',' { COMMA }
  | ".1" { FST }
  | ".2" { SND }
  | '+' { PLUS }
  | '-' { MINUS }
  | "case" { CASE }
  | "if" { IF }
  | "let" { LET }
  | var as s { VAR s }
  | eof { EOF }
