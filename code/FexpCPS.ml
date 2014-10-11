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
(* and funval = (value -> ans) *)
and funval =
  | Clos of var * fexp * value env
and 'a env = (string * 'a) list

let rec eval_cps (e : fexp) (r : value env) (k : ans -> ans) : ans =
  match e with
    | Int i -> k (Val (VInt i))
    | Bool b -> k (Val (VBool b))
    | Var x -> k (Val (List.assoc x r))
    | Pred e -> 
	eval_cps e r (fun (Val (VInt i)) -> k (Val (VInt (i-1))))
    | Succ e -> 
	eval_cps e r (fun (Val (VInt i)) -> k (Val (VInt (i+1))))
    | Plus (e1, e2) ->
	eval_cps e1 r
	  (fun (Val (VInt i)) ->
	     eval_cps e2 r
	       (fun (Val (VInt j)) -> k (Val (VInt (i+j)))))
    | Mult (e1, e2) ->
	eval_cps e1 r
	  (fun (Val (VInt i)) ->
	     eval_cps e2 r
	       (fun (Val (VInt j)) -> k (Val (VInt (i*j)))))
    | Div (e1, e2) ->
	eval_cps e1 r
	  (fun (Val (VInt i)) ->
	     eval_cps e2 r
	       (fun (Val (VInt j)) -> k (Val (VInt (i/j)))))
    | If (e1, e2, e3) ->
	eval_cps e1 r
	  (fun (Val (VBool b)) -> if b then eval_cps e2 r k else eval_cps e3 r k)
    (* new stuff *)
    | App (e1, e2) -> 
	eval_cps e1 r
	  (fun (Val (VFun f)) ->
	     eval_cps e2 r 
	       (fun (Val v) -> apply f v k))
    | Fun (x, e) -> 
	k (Val (VFun (Clos (x, e, r)))) (* fun v -> eval e ((x,v)::r) *)
and apply (f : funval) (v : value) (k : (ans -> ans)) : ans =
    match f with
      | Clos (x, e, r) -> eval_cps e ((x, v)::r) k
