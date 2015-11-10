type exp = Var of string
	 | App of exp * exp
	 | Fun of string * exp

type env = (string * cls) list
 and cls = Close of exp * env
	 | App of cls * cls

type c = CHole
       | CAppL of c * cls
       | CAppR of cls * c

type k = KHole
       | KAppL of k * cls
       | KAppR of cls * k

let rec plug_c (cxt : c) (x : cls) : cls =
  match cxt with
  | CHole -> x
  | CAppL (cxt, cls) ->
     App (plug_c cxt x, cls)
  | CAppR (cls, cxt) ->
     App (cls, plug_c cxt x)

let rec id (xs : int list) : int list =
  match xs with
  | [] -> []
  | x :: xs -> x :: (id xs)

let rec rev_acc (xs : int list) (acc : int list) : int list =
  match xs with
  | [] -> acc
  | x :: xs -> rev_acc xs (x :: acc)

let rec rev (xs : int list) : int list =
  rev_acc xs []
  
let rec decompose_c (x : cls) : (c * cls) option =
  match x with
  | App (Close (Fun (z, e), r), Close (Fun (y, e'), r')) ->
     Some (CHole, x)
  | Close (Var y, r) ->
     Some (CHole, x)
  | Close (App (e1, e2), r) ->
     Some (CHole, x)

  | App (c1, c2) ->
     (match decompose_c c1 with
      | None -> (match decompose_c c2 with
		 | None -> None
		 | Some (cxt, cls) ->
		    Some (CAppR (c1, cxt), cls))
      | Some (cxt, cls) ->
	 Some (CAppL (cxt, c2), cls))

  | Close (Fun (y, e), r) ->
     None

let rec decompose_k (x : cls) : (k * cls) option =
  decompose_k_acc x KHole

and decompose_k_acc (x : cls) (acc : k) : (k * cls) option =
  match x with
  | App (Close (Fun (z, e), r), Close (Fun (y, e'), r')) ->
     Some (acc, x)
  | Close (Var y, r) ->
     Some (acc, x)
  | Close (App (e1, e2), r) ->
     Some (acc, x)
  
  | App (c1, c2) ->
     (match decompose_k_acc c1 (KAppL (acc, c2)) with
      | None -> (match decompose_k_acc c2 (KAppR (c1, acc)) with
		 | None -> None
		 | Some (cont, cls) as r -> r)
      | Some (cont, cls) as r -> r)		    
  | Close (Fun (y, e), r) ->
     None
	       
let rec plug_k (cont : k) (x : cls) : cls =
  match cont with
  | KHole -> x
  | KAppL (cont, cls) ->
     plug_k cont (App (x, cls))
  | KAppR (cls, cont) ->
     plug_k cont (App (cls, x))

let ext (r : env) (x : string) (v : cls) : env =
  (x,v)::r
	

let is_val (c : cls) : bool =
  match c with
  | Close (Fun (_, _), _) -> true
  | _ -> false

let step (c : cls) (k : k) : (cls * k) option =
  match c with
  (* 1: Reduction axiom transitions *)
  | Close (Var x, r) -> Some (List.assoc x r, k)
  | Close (App (e1, e2), r) ->
     Some (App (Close (e1, r), Close (e2, r)), k)
  | App (Close (Fun (x, e), r), v)
       when is_val v ->	 
     Some (Close (e, ext r x v), k)
  (* 2: Eval transitions *)
  | App (c1, c2) when is_val c1 ->
     Some (c2, KAppR (c1, k))
  | App (c1, c2) ->
     Some (c1, KAppL (k, c2))
  (* 3: Continue transitions *)
  | v -> 
     (match k with
      | KHole -> None
      | KAppL (k', c) -> Some (App (v, c), k')
      | KAppR (c, k') -> Some (App (c, v), k'))








