{
open Parser
}

let white = [' ' '\n' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read =
  parse
  | white { read lexbuf }

  (* multi-character and keyword tokens first *)
  | "<-"       { TAKES }
  | "<="       { LEQ }
  | "<"        { LESS }
  | "="        { EQ }

  | "true"     { TRUE }
  | "false"    { FALSE }
  | "not"      { NOT }
  | "and"      { AND }
  | "or"       { OR }
  | "if"       { IF }
  | "then"     { THEN }
  | "else"     { ELSE }
  | "let"      { LET }
  | "in"       { IN }
  | "fun"      { FUN }
  | "rec"      { REC }
  | "apply"    { APPL }

  | "("        { LPAREN }
  | ")"        { RPAREN }
  | "+"        { PLUS }
  | "-"        { MINUS }
  | "*"        { MUL }  
  | ","        { COMMA }

  | num   { CONST (Lexing.lexeme lexbuf) }  

  (* identifier rule last, after keywords *)
  | id { ID (Lexing.lexeme lexbuf) }

  | eof        { EOF }

