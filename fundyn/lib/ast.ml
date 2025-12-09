type ide = string
  
type expr =
  | Ebool of bool
  | Var of ide
  | Eint of int
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Less of expr * expr
  | If of expr * expr * expr
  | Let of ide * expr * expr  
  | Fun of ide * expr
  | Appl of expr * expr    
