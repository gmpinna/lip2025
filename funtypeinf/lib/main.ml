open Ast
open Inferenza



let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast




let inftype (s:string) = tipo (parse s)


 