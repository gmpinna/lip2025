
(* The type of tokens. *)

type token = 
  | TRUE
  | THEN
  | TAKES
  | RPAREN
  | REC
  | PLUS
  | OR
  | NOT
  | MUL
  | MINUS
  | LPAREN
  | LET
  | LESS
  | LEQ
  | IN
  | IF
  | ID of (string)
  | FUN
  | FALSE
  | EQ
  | EOF
  | ELSE
  | CONST of (string)
  | COMMA
  | APPL
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
