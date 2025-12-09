open Ast
 
type etype = 
    TBool 
  | TInt 
  | TPair of etype * etype
  | TVar of string 
  | TFun of etype * etype
    
(* 
let rec ctotl l = match l with
    [] -> []
   |(c1,c2)::l1 -> (c1, c2)::(ctotl l1)
;;

let rec etotl l = match l with
    [] -> []
   |(c1,c2)::l1 -> (c1, c2)::(etotl l1)
;;

let rec cll l = match l with
    [] -> []
   |(i,x)::l1 -> (i,x)::(cll l1)
;;

let rec llc l = match l with
    [] -> []
   |(i,x)::l1 -> (i,x)::(llc l1)
;;

let ctot (c,l) = (c, ctotl l)
;;  
*)  

let nextsym = ref (-1);;
let gentide = fun () -> nextsym := !nextsym + 1; TVar ("?T" ^ string_of_int (!nextsym));;

(* let newtypenv = cll (List.tl ((Ide "x",TInt)::[]));; *)

let emptyenv = [];;

let rec applyenv tr x = match tr with
   (y,e)::tr1 -> if x = y then e else (applyenv tr1 x)
 | _ -> failwith("la variabile non compare nell'ambiente");; 
 
let rec applytypenv (tr:(ide * etype) list) (x:ide)  = match tr with
   (y,e)::tr1 -> if x = y then e else (applytypenv tr1 x)
 | _ -> failwith("la variabile non compare nell'ambiente");; 
 
let rec bind tr x e = match tr with 
      [] -> (x,e)::tr
    |(y,e1)::tr1 -> if x = y then (x,e)::tr1 else (y,e1)::(bind tr1 x e);;

let rec bindtyp (tr:(ide * etype) list) (x:ide) (e:etype) = match tr with 
      [] -> (x,e)::tr
    |(y,e1)::tr1 -> if x = y then (x,e)::tr1 else (y,e1)::(bindtyp tr1 x e);;

let rec cconstraints e tr = match e with
  Eint _ -> (TInt,[])
| Var x ->  (applyenv tr x,[])
| Add (e1,e2)
| Sub (e1,e2)
| Mul (e1,e2)  ->
    let (t1,c1) = cconstraints e1 tr in
    let (t2,c2) = cconstraints e2 tr in
    let c = [(t1,TInt); (t2,TInt)] in
    (TInt, c @ c1 @ c2)
| Ebool _ -> (TBool,[])
| Eq (e1,e2) ->
    let (t1,c1) = cconstraints e1 tr in
    let (t2,c2) = cconstraints e2 tr in
    let c = [(t1,t2)] in
    (TBool, c @ c1 @ c2)
| Leq (e1,e2) ->
    let (t1,c1) = cconstraints e1 tr in
    let (t2,c2) = cconstraints e2 tr in
    let c = [(t1,TInt); (t2,TInt)] in
    (TBool, c @ c1 @ c2)
| Less (e1,e2) ->
    let (t1,c1) = cconstraints e1 tr in
    let (t2,c2) = cconstraints e2 tr in
    let c = [(t1,TInt); (t2,TInt)] in
    (TBool, c @ c1 @ c2)
    | Not e1 ->
    let (t1,c1) = cconstraints e1 tr in
    let c = [(t1,TBool)] in
    (TBool, c @ c1)
| And (e1,e2)
| Or (e1,e2) ->
    let (t1,c1) = cconstraints e1 tr in
    let (t2,c2) = cconstraints e2 tr in
    let c = [(t1,TBool); (t2,TBool)] in
    (TBool, c @ c1 @ c2)
| Pair (e1,e2) ->
    let (t1,c1) = cconstraints e1 tr in
    let (t2,c2) = cconstraints e2 tr in
    let c = [] in
    (TPair(t1,t2), c @ c1 @ c2)
| Fst e1 -> 
    let tx1 = gentide() in
    let tx2 = gentide() in
    let (t1,c1) = cconstraints e1 tr in
    let c = [(t1,TPair(tx1,tx2))] in
    (tx1, c @ c1)
| Snd e1 -> 
    let tx1 = gentide() in
    let tx2 = gentide() in
    let (t1,c1) = cconstraints e1 tr in
    let c = [(t1,TPair(tx1,tx2))] in
    (tx2, c @ c1)    
| If(e0,e1,e2) ->
    let (t0,c0) = cconstraints e0 tr in
    let (t1,c1) = cconstraints e1 tr in
    let (t2,c2) = cconstraints e2 tr in
    let c = [(t0,TBool); (t1,t2)] in
    (t1, c @ c0 @ c1 @ c2)
| Let (x,e1,e2) ->
    let (t1,c1) = cconstraints e1 tr in
    let (t2,c2) = cconstraints e2 (bind tr x t1) in
    (t2, c1 @ c2)
| Fun (x,e1) ->
    let tx = gentide() in
    let (t1,c1) = cconstraints e1 (bind tr x tx) in
    (TFun (tx,t1), c1)
| Appl (e1,e2) ->
    let tx = gentide() in
    let (t1,c1) = cconstraints e1 tr in
    let (t2,c2) = cconstraints e2 tr in
    let c = [(t1,TFun(t2,tx))] in
    (tx, c @ c1 @ c2)
;;

let tconstraints e tr = (cconstraints e tr)
;; 

let rec applysubst1 t0 x t = match t0 with
    TInt -> TInt
  | TBool -> TBool
  | TVar y -> if y=x then t else TVar y
  | TFun (t1,t2) -> TFun (applysubst1 t1 x t, applysubst1 t2 x t)
  | TPair (t1,t2) -> TPair (applysubst1 t1 x t, applysubst1 t2 x t)
;;
 
let rec applysubst l x t = match l with
    [] -> []
  | (t1,t2)::l' -> (applysubst1 t1 x t, applysubst1 t2 x t)::(applysubst l' x t)
;;
 
let rec occurs x t = match t with
  TInt
| TBool -> false
| TVar y -> x=y
| TFun (t1,t2) -> (occurs x t1) || (occurs x t2)
| TPair (t1,t2) -> (occurs x t1) || (occurs x t2)
;;
 
let rec unify  l = match l with
  [] -> []
| (TInt,TInt)::l' -> unify l'
| (TBool,TBool)::l' -> unify l'
| (TVar x, t)::l' ->
    if occurs x t then failwith "Occurs check"
    else (TVar x, t)::(unify (applysubst l' x t))
| (t, TVar x)::l' ->
    if occurs x t then failwith "Occurs check"
    else (TVar x, t)::(unify (applysubst l' x t))
| (TFun(t1,t2),TFun(t1',t2'))::l' ->
    unify ((t1,t1') :: (t2,t2') :: l')
| (TPair(t1,t2),TPair(t1',t2'))::l' ->
    unify ((t1,t1') :: (t2,t2') :: l')
| _ -> failwith "Unsolvable constraints"
;;

let tipo e =
  let rec resolve t s = (match s with
    [] -> t
  | (TVar x, t')::s' -> resolve (applysubst1 t x t') s'
  | _ -> failwith ("Ill-formed substitution")) in
  let (t,c) = cconstraints e emptyenv in
  resolve t (unify c)
;;

let typeinf e = (tipo e)
;;




