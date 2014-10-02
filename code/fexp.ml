type fexp =
  | Int of int
  | Bool of bool
  | Var of string
  | Pred of fexp
  | Succ of fexp
  | Plus of fexp * fexp
  | Mult of fexp * fexp
  | Div of fexp * fexp
  | If of fexp * fexp * fexp
  (* new stuff *)
  | Fun of string * fexp
  | App of fexp * fexp

type ans = 
  | Val of value
  | Err of string
and value =
  | VInt of int
  | VBool of bool
  (* new stuff *)
  | VFun of (value -> ans)

type 'a env = (string * 'a) list

(* eval, ignoring errors for now. *)

let rec eval (e : fexp) (r : value env) : ans =
  match e with
    | Int i -> Val (VInt i)
    | Bool b -> Val (VBool b)
    | Var x -> Val (List.assoc x r)
    | Pred e -> let Val (VInt i) = eval e r in
	Val (VInt (i-1))
    | Succ e -> let Val (VInt i) = eval e r in
	Val (VInt (i+1))
    | Plus (e1, e2) ->
	(match (eval e1 r, eval e2 r) with
	   | (Val (VInt i), Val (VInt j)) -> Val (VInt (i+j)))
    | Mult (e1, e2) ->
	(match (eval e1 r, eval e2 r) with
	   | (Val (VInt i), Val (VInt j)) -> Val (VInt (i*j)))
    | Div (e1, e2) ->
	(match (eval e1 r, eval e2 r) with
	   | (Val (VInt i), Val (VInt j)) -> Val (VInt (i/j)))
    | If (e1, e2, e3) ->
	(match eval e1 r with
	   | Val (VBool b) -> if b then eval e2 r else eval e3 r)
    (* new stuff *)
    | App (e1, e2) -> 
	let (a1,a2) = (eval e1 r, eval e2 r) in
	let Val (VFun f) = a1 in
	let Val v2 = a2 in
	  f v2	  
    | Fun (x, e) -> 
	Val (VFun (fun v -> eval e ((x,v)::r)))
