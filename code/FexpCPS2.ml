type fexp =
  | Int of int
  | Bool of bool
  | Var of var
  | Pred of fexp
  | Succ of fexp
  | Plus of fexp * fexp
  | Mult of fexp * fexp
  | Div of fexp * fexp
  | If of fexp * fexp * fexp
  (* new stuff *)
  | Fun of string * fexp
  | App of fexp * fexp

and var = string

type ans = 
  | Val of value
  | Err of string

and value =
  | VInt of int
  | VBool of bool
  (* new stuff *)
  | VFun of funval

and funval = (* (value -> ans) *)
  | Clos of var * fexp * value env

and 'a env = (string * 'a) list

type cont = 
  | K0 (* identity function *)
  | K1 of cont
  | K2 of cont
  | K3 of fexp * value env * cont (* Plus *)
  | K4 of cont * int
  | K5 of fexp * value env * cont (* Mult *)
  | K6 of cont * int
  | K7 of fexp * value env * cont (* Div *)
  | K8 of cont * int
  | K9 of fexp * fexp * value env * cont (* If *)
  | K10 of fexp * value env * cont (* App *)
  | K11 of funval * cont

let rec continue (k : cont) (v : value) : ans =
  match k with
  | K0 -> (Val v)
  | K1 k -> let VInt i = v in continue k (VInt (i-1))
  | K2 k -> let VInt i = v in continue k (VInt (i+1))
  | K3 (e2, r, k) -> 
      let VInt i = v in
	eval e2 r (K4 (k, i))
  | K4 (k, i) ->
      let VInt j = v in
	continue k (VInt (i+j))
  | K5 (e2, r, k) -> 
      let VInt i = v in
	eval e2 r (K6 (k, i))
  | K6 (k, i) ->
      let VInt j = v in
	continue k (VInt (i*j))
  | K7 (e2, r, k) -> 
      let VInt i = v in
	eval e2 r (K8 (k, i))
  | K8 (k, i) ->
      let VInt j = v in
	if j=0
	then Err "Divide by Zero"
	else continue k (VInt (i/j))
  | K9 (e2, e3, r, k) ->
      let VBool b = v in
	if b then eval e2 r k else eval e3 r k
  | K10 (e2, r, k) ->
      let VFun f = v in
	eval e2 r (K11 (f, k))
  | K11 (f, k) ->
      apply f v k

and eval (e : fexp) (r : value env) (k : cont) : ans =
  match e with
    | Int i -> continue k (VInt i)
    | Bool b -> continue k (VBool b)
    | Var x -> continue k (List.assoc x r)
    | Pred e -> 
	eval e r (K1 k)
    | Succ e -> 
	eval e r (K2 k)
    | Plus (e1, e2) ->
	eval e1 r (K3 (e2, r, k))
    | Mult (e1, e2) ->
	eval e1 r (K5 (e2, r, k))
    | Div (e1, e2) ->
	eval e1 r (K7 (e2, r, k))
    | If (e1, e2, e3) ->
	eval e1 r (K9 (e2, e3, r, k))
    (* new stuff *)
    | App (e1, e2) -> 
	eval e1 r (K10 (e2, r, k))
    | Fun (x, e) -> 
	continue k (VFun (Clos (x, e, r))) (* fun v -> eval e ((x,v)::r) *)

and apply (f : funval) (v : value) (k : cont) : ans =
    match f with
      | Clos (x, e, r) -> eval e ((x, v)::r) k

let interp (e : fexp) (r : value env) : ans =
  eval e r K0
