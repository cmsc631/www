type _B_ =
  | Int of int
  | Pred of _B_
  | Succ of _B_
  | Plus of _B_ * _B_
  | Mult of _B_ * _B_
  (* new stuff *)
  | Var of string
  | Bool of bool
  | If of _B_ * _B_ * _B_
  | Div of (_B_ * _B_)

type value =
  | VInt of int 
  | VBool of bool
  | VError of string

type env = (string * value) list

let rec lookup (r : env) (x : string) : value =
  match r with
    | (y,v)::r' -> if y=x then v else lookup r' x

let rec eval (e : _B_) (r : env) : value =
  match e with
    | Int i -> VInt i
    | Bool b -> VBool b
    | Pred e -> 
	(match eval e r with
	  | VBool b -> VError "expected a bool"
	  | VInt i -> VInt (i - 1))
    | Succ e -> 
	let VInt i = eval e r in
	  VInt (i + 1)
    | Plus (e1, e2) ->
	let VInt i = eval e1 r in
	let VInt j = eval e2 r in
	  VInt (i+j)
    | Mult (e1, e2) ->
	let VInt i = eval e1 r in
	let VInt j = eval e2 r in
	  VInt (i*j)
    | If (e1, e2, e3) ->
	let VBool b = eval e1 r in
	  if b=true then eval e2 r else eval e3 r
    | Div (e1, e2) ->
	let VInt i = eval e1 r in
	let VInt j = eval e2 r in
	  VInt (i/j)
    | Var x -> lookup r x
	
	    

let rec compile (e : _B_) : (env -> value) =
  match e with
    | Int i -> (fun _ -> VInt i)
    | Bool b -> (fun _ -> VBool b)
    | Pred e ->
	let c = compile e in
	(fun env ->
	   let VInt i = c env in
	     VInt (i-1))
    | Succ e ->
	let c = compile e in
	  (fun env -> 
	     let VInt i = c env in
	       VInt (i+1))
    | Mult (e1, e2) ->
	let c1 = compile e1 in
	let c2 = compile e2 in
	  (fun env ->
	     let VInt i = (c1 env) in
	     let VInt j = (c2 env) in
	       VInt (i*j))
    | Plus (e1, e2) ->
	let c1 = compile e1 in
	let c2 = compile e2 in
	  (fun env ->
	     let VInt i = (c1 env) in
	     let VInt j = (c2 env) in
	       VInt (i+j))
    | Div (e1, e2) ->
	let c1 = compile e1 in
	let c2 = compile e2 in
	  (fun env ->
	     let VInt i = (c1 env) in
	     let VInt j = (c2 env) in
	       VInt (i/j))
    | If (e1, e2, e3) ->
	let c1 = compile e1 in
	let c2 = compile e2 in
	let c3 = compile e3 in
	  (fun env ->
	     let VBool b = c1 env in
	       if b=true then c2 env else c3 env)
    | Var x ->
	(fun env -> lookup env x)
