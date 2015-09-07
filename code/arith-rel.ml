type arith =
  | Num of int
  | Pred of arith
  | Succ of arith
  | Plus of arith * arith
  | Mult of arith * arith

(*  | Amb of arith * arith *)

let cartesian (l : 'a list) (l' : 'b list) : ('a * 'b) list =
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l) 

let rec eval (e : arith) : int list =
  match e with
    Num i -> [i]
  | Pred e -> 
      List.map (fun v -> v-1) (eval e)
  | Succ e ->
      List.map (fun v -> v+1) (eval e)
  | Plus (e1, e2) -> 
      List.map (fun (v1,v2) -> v1+v2)
	(cartesian (eval e1) (eval e2))
  | Mult (e1, e2) ->
      List.map (fun (v1,v2) -> v1*v2)
	(cartesian (eval e1) (eval e2))
  | Amb (e1, e2) -> eval e1 @ eval e2


let a (e : arith) : arith list =
  match e with
    | Pred (Num i) -> [Num (i-1)]
    | Succ (Num i) -> [Num (i+1)]
    | Plus (Num i, Num j) -> [Num (i+j)]
    | Mult (Num i, Num j) -> [Num (i*j)]
(*  | Amb (Num i, Num j) -> [Num i; Num j] *)
    | _ -> []
;;

(* let rec a_step (e : arith) : arith list = *)

type ectx =
  | Hole
  | EPred of ectx
  | ESucc of ectx
  | EPlusL of ectx * arith
  | EPlusR of int * ectx
  | EMultL of ectx * arith
  | EMultR of int * ectx

let rec decompose (e : arith) : (ectx * arith) option =
  match e with
    | Num i -> None
    | Pred (Num i) -> Some (Hole, e)
    | Succ (Num i) -> Some (Hole, e)
    | Plus (Num i, Num j) -> Some (Hole, e)
    | Mult (Num i, Num j) -> Some (Hole, e)
    | Pred e' ->
	let Some (c, e'') = decompose e' in
	  Some (EPred c, e'')
    | Succ e' ->
	let Some (c, e'') = decompose e' in
	  Some (ESucc c, e'')
    | Plus (Num i, e2) ->
	let Some (c, e'') = decompose e2 in
	  Some (EPlusR (i, c), e'')
    | Plus (e1, e2) ->
	let Some (c, e'') = decompose e1 in
	  Some (EPlusL (c, e2) ,e'')
    | Mult (Num i, e2) ->
	let Some (c, e'') = decompose e2 in
	  Some (EMultR (i, c), e'')
    | Mult (e1, e2) ->
	let Some (c, e'') = decompose e1 in
	  Some (EMultL (c, e2) ,e'')
	

let rec plug (c : ectx) (e : arith) : arith =
  match c with
    | Hole -> e
    | EPred c' -> Pred (plug c' e)
    | ESucc c' -> Succ (plug c' e)
    | EPlusL (c', e2) ->
	Plus (plug c' e, e2)
    | EPlusR (i, c') ->
	Plus (Num i, plug c' e)
    | EMultL (c', e2) ->
	Mult (plug c' e, e2)
    | EMultR (i, c') ->
	Mult (Num i, plug c' e)

(* reduction-based evaluation function *)
let rec eval_r (e : arith) : int =
  match decompose e with
    | None -> let Num i = e in i
    | Some (c, e') ->
	let [e''] = a e' in
	  eval_r (plug c e'')
