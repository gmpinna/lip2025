open Ast


type envs = ide -> evals
and 
evals = Unbound 
      | Int of int 
      | Bool of bool 
      | Funval of expr * envs
;;


let emptyenvs = function _ -> Unbound;;

let applyenv(x,y) = x y;;
let bind((r: envs) , (l:ide),  (e:evals)) =
    function lu -> if lu = l then e else applyenv(r,lu);;


let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(*

(******************************************************************************)
(*                      Dynamic type-checking of expressions                   *)
(******************************************************************************)

*)


(* Operations on Eval(s/d) *)

let typecheck (x, y) =
match x with
   | "int" -> (match y with 
	            | Int(_) -> true
	            | _ -> false)
   | "bool" ->  (match y with 
	 |  Bool(_) -> true
	 | _ -> false)
   | _ -> failwith ("not a valid type")
;;

  	
let equ (x,y) = if typecheck("int",x) && typecheck("int",y) 
   then (match (x,y) with
	| (Int(u), Int(w)) -> Bool(u = w)
  | _ -> failwith ("type error"))
   else failwith ("type error")

let leq (x,y) = if typecheck("int",x) && typecheck("int",y) 
   then (match (x,y) with
	| (Int(u), Int(w)) -> Bool(u <= w)
  | _ -> failwith ("type error"))
   else failwith ("type error")
   
let less (x,y) = if typecheck("int",x) && typecheck("int",y) 
   then (match (x,y) with
	| (Int(u), Int(w)) -> Bool(u < w)
  | _ -> failwith ("type error"))
   else failwith ("type error")   

let plus (x,y) = if typecheck("int",x) && typecheck("int",y) 
  then (match (x,y) with
	| (Int(u), Int(w)) -> Int(u+w)
  | _ -> failwith ("type error"))
   else failwith ("type error")

let diff (x,y) = if typecheck("int",x) && typecheck("int",y) 
   then (match (x,y) with
	| (Int(u), Int(w)) -> Int(u-w)
  | _ -> failwith ("type error"))
   else failwith ("type error")

let mult (x,y) = if typecheck("int",x) && typecheck("int",y) 
   then (match (x,y) with
	| (Int(u), Int(w)) -> Int(u*w)
  | _ -> failwith ("type error"))
   else failwith ("type error")

let et (x,y) = if typecheck("bool",x) && typecheck("bool",y) 
   then (match (x,y) with
	| (Bool(u), Bool(w)) -> Bool(u && w)
  | _ -> failwith ("type error"))
   else failwith ("type error")

let vel (x,y) = if typecheck("bool",x) && typecheck("bool",y) 
   then 	(match (x,y) with
	| (Bool(u), Bool(w)) -> Bool(u || w)
  | _ -> failwith ("type error"))
   else failwith ("type error")

let non x = if typecheck("bool",x) 
   then 	(match x with
  	| Bool(y) -> Bool((not y))
    | _ -> failwith ("type error"))
   else failwith ("type error")


   
(*

(******************************************************************************)
(*                      Big-step semantics of expressions                   *)
(******************************************************************************)

*)


(*SEMANTIC EVALUATION FUNCTIONS  *)
let rec sem (e:expr) (r:envs) =
      match e with
      | Eint(n) -> Int(n)
      | Ebool(b) -> Bool(b)
      | Var(i) -> applyenv(r,i)
      | Eq(a,b) -> equ((sem a r) ,(sem b r) )
      | Mul(a,b) ->  mult ( (sem a r), (sem b r))
      | Add(a,b) ->  plus ( (sem a r), (sem b r))
      | Sub(a,b)  ->  diff ( (sem a r), (sem b r))
      | And(a,b) ->  et ( (sem a r), (sem b r))
      | Or(a,b) ->  vel ( (sem a r), (sem b r))
      | Leq(a,b) ->  leq ( (sem a r), (sem b r))
      | Less(a,b) ->  less ( (sem a r), (sem b r))
      | Not(a) -> non( (sem a r))
      | If(a,b,c) -> 
            let g = sem a r in
            if typecheck("bool",g) then
               (if g = Bool(true) 
               then sem b r
               else sem c r)
            else failwith ("nonboolean guard") 
      | Let(i,e1,e2) -> sem e2 (bind (r, i, sem e1 r))          
      | Fun(i,a) ->  makefun(Fun(i,a), r)
      | Appl(a,b) -> applyfun(sem a r, sem b r)
	    | Rec(f,e) -> makefunrec (f,e,r)

and makefun ((a:expr),(x:envs)) =
      (match a with
      |	Fun(_,_) -> Funval(a,x)
      |	_ -> failwith ("Non-functional object")) 
    
and applyfun ((ev1:evals),(ev2:evals)) =
      ( match ev1 with
      | Funval(Fun(ii,aa),r) -> sem aa (bind(r,ii,ev2))
      | _ -> failwith ("attempt to apply a non-functional object"))

and makefunrec (i, e1, (r:envs)) = 
    let functional (rr: envs)  = 
	     bind(r, i, makefun(e1,rr)) in 
       let rec rfix =  function x -> functional rfix x 
          in makefun(e1,rfix) 



let semstatic (s:string) = sem (parse s) emptyenvs
