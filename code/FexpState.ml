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
  | Fun of string * fexp
  | App of fexp * fexp
  | Let of var * fexp * fexp
  (* new stuff *)
  | Set of var * fexp
  | Seq of fexp * fexp

and var = string
and loc = int

type ans = 
  | Val of value
  | Err of string

and value =
  | VInt of int
  | VBool of bool
  (* new stuff *)
  | VFun of funval

and funval = (* (value -> ans) *)
  | Clos of var * fexp * loc env

and 'a env = (string * 'a) list
and ('a, 'b) map = ('a * 'b) list

type cont = 
  | K0 (* identity function *)
  | K1 of cont
  | K2 of cont
  | K3 of fexp * loc env * cont (* Plus *)
  | K4 of cont * int
  | K5 of fexp * loc env * cont (* Mult *)
  | K6 of cont * int
  | K7 of fexp * loc env * cont (* Div *)
  | K8 of cont * int
  | K9 of fexp * fexp * loc env * cont (* If *)
  | K10 of fexp * loc env * cont (* App *)
  | K11 of funval * cont
  | K12 of var * loc env * cont (* Set *)
  | K13 of fexp * loc env * cont (* Seq *)

let rec continue (k : cont) (v : value) (s : (loc, value) map) : ans =
  match k with
  | K0 -> (Val v)
  | K1 k -> let VInt i = v in continue k (VInt (i-1)) s
  | K2 k -> let VInt i = v in continue k (VInt (i+1)) s
  | K3 (e2, r, k) -> 
      let VInt i = v in
	eval e2 r s (K4 (k, i))
  | K4 (k, i) ->
      let VInt j = v in
	continue k (VInt (i+j)) s
  | K5 (e2, r, k) -> 
      let VInt i = v in
	eval e2 r s (K6 (k, i))
  | K6 (k, i) ->
      let VInt j = v in
	continue k (VInt (i*j)) s
  | K7 (e2, r, k) -> 
      let VInt i = v in
	eval e2 r s (K8 (k, i))
  | K8 (k, i) ->
      let VInt j = v in
	if j=0
	then Err "Divide by Zero"
	else continue k (VInt (i/j)) s
  | K9 (e2, e3, r, k) ->
      let VBool b = v in
	if b then eval e2 r s k else eval e3 r s k
  | K10 (e2, r, k) ->
      let VFun f = v in
	eval e2 r s (K11 (f, k))
  | K11 (f, k) ->
      apply f v k s
  | K12 (x, r, k) -> continue k (List.assoc (List.assoc x r) s) (update s (List.assoc x r) v)
  | K13 (e, r, k) -> eval e r s k

and update (s : (loc, value) map) (l : loc) (v : value) : (loc, value) map =
  match s with
    | [] -> failwith "wat?"
    | (l',v')::s ->
	if l=l' then (l,v)::s else (l',v')::(update s l v)

and eval (e : fexp) (r : loc env) (s : (loc, value) map)  (k : cont) : ans =
  match e with
      (* Syntactic sugar *)
    | Let (x, e1, e2) -> eval (App (Fun (x, e2), e1)) r s k
	(* "Real" stuff *)
    | Int i -> continue k (VInt i) s
    | Bool b -> continue k (VBool b) s
    | Var x -> continue k (List.assoc (List.assoc x r) s) s
    | Pred e -> 
	eval e r s (K1 k)
    | Succ e -> 
	eval e r s (K2 k)
    | Plus (e1, e2) ->
	eval e1 r s (K3 (e2, r, k))
    | Mult (e1, e2) ->
	eval e1 r s (K5 (e2, r, k))
    | Div (e1, e2) ->
	eval e1 r s (K7 (e2, r, k))
    | If (e1, e2, e3) ->
	eval e1 r s (K9 (e2, e3, r, k))
    | App (e1, e2) -> 
	eval e1 r s (K10 (e2, r, k))
    | Fun (x, e) -> 
	continue k (VFun (Clos (x, e, r))) s
    | Set (x, e) -> eval e r s (K12 (x, r, k))
    | Seq (e1, e2) -> eval e1 r s (K13 (e2, r, k))

and apply (f : funval) (v : value) (k : cont) (s : (loc, value) map) : ans =
    match f with
      | Clos (x, e, r) -> 
	  let a = alloc s in
	    eval e ((x, a)::r) ((a, v)::s) k

and alloc s =
  match s with
    | [] -> 0
    | (l,_)::s ->
	1 + l


let interp (e : fexp) (r : loc env) (s : (loc, value) map) : ans =
  eval e r s K0

let run (e : fexp) : ans =
  interp e [] []
