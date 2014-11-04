(* Simplifying abstract machine down to core language *)

type fexp =
  | Var of var
  | Fun of string * fexp
  | App of fexp * fexp
  | Let of var * fexp * fexp

and var = string
and loc = int

type ans = 
  | Val of value
  | Err of string

and value =
  | VFun of funval

and funval = (* (value -> ans) *)
  | Clos of var * fexp * loc env

and 'a env = (string * 'a) list
and ('a, 'b) map = ('a * 'b) list

type cont = 
  | K0 (* identity function *)
  | K10 of fexp * loc env * cont (* App *)
  | K11 of funval * cont

let rec continue (k : cont) (v : value) (s : (loc, value) map) : ans =
  match k with
  | K0 -> (Val v)
  | K10 (e2, r, k) ->
      let VFun f = v in
	eval e2 r s (K11 (f, k))
  | K11 (f, k) ->
      apply f v k s

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
    | Var x -> continue k (List.assoc (List.assoc x r) s) s
    | App (e1, e2) -> 
	eval e1 r s (K10 (e2, r, k))
    | Fun (x, e) -> 
	continue k (VFun (Clos (x, e, r))) s

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
