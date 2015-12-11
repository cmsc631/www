type exp = Var of string
	 | App of exp * exp
	 | Fun of string * exp
	 | Set of string * exp

type loc = int
type env = (string * loc) list

type cls = Close of exp * env
	 | App of cls * cls
	 | Set of string * env * cls

type sto = (loc * cls) list

type k = KHole
       | KAppL of k * cls
       | KAppR of cls * k
       | KSet of string * env * k

let is_val (c : cls) : bool =
  match c with
  | Close (Fun (_, _), _) -> true
  | _ -> false

let alloc_ian =
  let i = ref 0 in
  fun s -> (let j = !i in i := j + 1; j)

let alloc (s : sto) : loc =
  (List.length s) + 1

let step (c : cls) (k : k) (s : sto) : (cls * k * sto) option =
  match c with
  (* 1: Reduction axiom transitions *)
  | Close (Var x, r) -> 
     Some (List.assoc (List.assoc x r) s, k, s)
  | Close (App (e1, e2), r) ->
     Some (App (Close (e1, r), Close (e2, r)), k, s)
  | App (Close (Fun (x, e), r), v)
       when is_val v ->
     let l = alloc s in
     Some (Close (e, (x,l)::r), k, (l,v)::s)

  | Set (x, r, v)
       when is_val v ->
     Some (Close (Fun ("id", Var "id"), []), k,
	   ((List.assoc x r),v)::s)
     

  (* 2: Eval transitions *)
  | App (c1, c2) when is_val c1 ->
     Some (c2, KAppR (c1, k), s)
  | App (c1, c2) ->
     Some (c1, KAppL (k, c2), s)

  | Set (x, r, c) -> Some (c, KSet (x, r, k), s)

  (* 3: Continue transitions *)
  | v -> 
     (match k with
      | KHole -> None
      | KAppL (k', c) -> Some (App (v, c), k', s)
      | KAppR (c, k') -> Some (App (c, v), k', s)
      | KSet (x, r, k') -> Some (Set (x, r, v), k', s))
