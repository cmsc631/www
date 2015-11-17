#lang scribble/manual

@(define (gh p) (link (format "https://github.com/cmsc631/~a" p) (format "~a" p)))

@title{Blog}

@bold{Tue Nov 17 13:43:48 EST 2015}

I've received several requests to extend the deadline for
@secref{PS4}.  I've pushed it back one week.

@bold{Fri Nov 13 12:36:52 EST 2015}

Please read the first three sections of the following document to get
another perspective on the machine design we've been talking about in
class.  This development is done in Redex instead of OCaml.

@url{https://dvanhorn.github.io/redex-aam-tutorial/}

@bold{Fri Nov 13 12:31:15 EST 2015}

Here is the code from yesterday's class.  I had to add a few cases
because we forgot to add a new continuation form for set-expressions
and missed the stack-pushing case for set.

@verbatim|{
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

}|

@bold{Tue Nov 10 16:53:52 EST 2015}

Here's the code from today:

@verbatim|{
type exp = Var of string
	 | App of exp * exp
	 | Fun of string * exp

type env = (string * cls) list
 and cls = Close of exp * env
	 | App of cls * cls

type k = KHole
       | KAppL of k * cls
       | KAppR of cls * k

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
}|

@bold{Tue Nov 10 15:16:52 EST 2015}

Here's the code from last time:

@verbatim|{
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
}|

@bold{Tue Nov  3 15:08:57 EST 2015}

I've provided some initial feedback (as github issues) on all of the
research proposals I've received so far.  Please see me if you want
talk more about the projects, have questions on what I said, or didn't
get feedback.

Please have your revised and expanded proposal @secref{RP2} turned in
by the end of the week.

It's important that you make progress on your research project now;
it's not something you can do overnight or even in a week.  The only
way to be successful is to use your time wisely between now and the
end of the semester.


@bold{Tue Oct 20 14:48:07 EDT 2015}

I need to cancel class today; see you Thursday.

@bold{Fri Oct 16 19:22:19 EDT 2015}

Here are the project team assignments for those I've heard from:

@verbatim|{

proj19: aconz2 (andrewconsroe@g), mckaz (milod@u)
proj20: iobender (matthewbender77@g), xmaumd (xinyuanma521@g)
proj21: KonstantinosX (kostasx@cs), bbengfort (benjamin@b)
proj22: Umar-Farooqui (umar.a.farooqui@g), ashbondu (ashwin.lg341@g)
proj23: meethu24 (meethu24@g), angel6392 (aplane@u)
proj24: Isweet (isweet@t), wir963 (wir963@g)
proj25: rstevens70 (stevens.rock@g), osuciu (osuciu@cs)
}|

@bold{Thu Oct 15 17:10:19 EDT 2015}

Here is the complete constraint-based type inference code we developed
in class:

@verbatim|{
#lang racket
(require redex)
(require "types.rkt")

(define-extended-language H F
  (T ::= .... t)
  (t ::= variable-not-otherwise-mentioned)

  (Γ ::= ((x t) ...))
  (C ::= ((T = T) ...)))

(define-metafunction H
  ∧ : C ... -> C
  [(∧ ((T_0 = T_1) ...) ...)
   ((T_0 = T_1) ... ...)])

(define-metafunction/extension ext H
  ext′ : Γ x T -> Γ)
(define-metafunction/extension lookup H
  lookup′ : Γ x -> T)

(define-judgment-form H
  #:mode (typeof I I I I O O)
  #:contract (typeof Γ ⊢ e : T C)

  [(where t (lookup′ Γ x))
   -------------
   (typeof Γ ⊢ x : t ())]

  [-------------------
   (typeof Γ ⊢ i : Int ())]

  [-------------------
   (typeof Γ ⊢ b : Bool ())]

  [(typeof Γ ⊢ e : T C)
   -----------
   (typeof Γ ⊢ (Succ e) : Int (∧ C ((T = Int))))]

  [(typeof Γ ⊢ e_0 : T_0 C_0)
   (typeof Γ ⊢ e_1 : T_1 C_1)
   (typeof Γ ⊢ e_2 : T_2 C_2)
   --------------------------
   (typeof Γ ⊢ (If e_0 e_1 e_2) : T_1
           (∧ C_0 C_1 C_2 ((T_0 = Bool) (T_1 = T_2))))]
  
  [(typeof Γ ⊢ e_1 : T_1 C_1)
   (typeof Γ ⊢ e_2 : T_2 C_2)
   -----------
   (typeof Γ ⊢ (Plus e_1 e_2) : Int
           (∧ C_1 C_2 ((T_1 = Int) (T_2 = Int))))]

  [(where T_1 ,(variable-not-in (term Γ) 'x))
   (typeof (ext′ Γ x T_1) ⊢ e : T_2 C)
   --------------------------------
   (typeof Γ ⊢ (Fun x e) : (T_1 → T_2) C)]
  
  [(typeof Γ ⊢ e_1 : T_1 C_1)
   (typeof Γ ⊢ e_2 : T_2 C_2)
   (where T_3 ,(variable-not-in (term (Γ T_1 T_2)) 'ta))
   -----------------------
   (typeof Γ ⊢ (App e_1 e_2) : T_3
           (∧ C_1 C_2 ((T_1 = (T_2 → T_3)))))])

(judgment-holds (typeof () ⊢ (If True 1 False) : T C) (T C))
(judgment-holds (typeof () ⊢ (Fun x x) : T C) (T C))
(judgment-holds
 (typeof () ⊢ (Fun x (Plus x x)) : T C) (T C))

(judgment-holds
 (typeof () ⊢ (Fun x (App x x)) : T C) (T C))

(define-metafunction H
  unify : C -> C or #f
  [(unify ()) ()]
  [(unify ((T = T) any ...))
   (unify (any ...))]
  [(unify (((T_1 → T_2) = (T_3 → T_4)) any ...))
   (unify ((T_1 = T_3) (T_2 = T_4) any ...))]
  [(unify ((Bool = Int) _ ...)) #f]
  [(unify ((Int = Bool) _ ...)) #f]
  [(unify ((Bool = (_ → _)) _ ...)) #f]
  [(unify ((Int = (_ → _)) _ ...)) #f]
  [(unify (((_ → _) = Bool) _ ...)) #f]
  [(unify (((_ → _) = Int) _ ...)) #f]
  [(unify ((T = t) any ...))
   (unify ((t = T) any ...))]
  [(unify ((t = T) any ...))
   (∧ ((t = T)) C)
   (where C (unify (subst T t (any ...))))
   ;; occurs check
   (where #t (not-in t (vars T)))]
  [(unify C) #f])

(define-metafunction H
  vars : T -> (t ...)
  [(vars t) (t)]
  [(vars Bool) ()]
  [(vars Int) ()]
  [(vars (T_1 → T_2))
   (t_1 ... t_2 ...)
   (where (t_1 ...) (vars T_1))
   (where (t_2 ...) (vars T_2))])

(define-metafunction H
  not-in : t (t ...) -> #t or #f
  [(not-in t ()) #t]
  [(not-in t (t t_0 ...)) #f]
  [(not-in t (t_0 t_1 ...))
   (not-in t (t_1 ...))])

(define-metafunction H
  subst : T t C -> C
  [(subst T t ((T_0 = T_1) ...))
   (((subst-t T t T_0) = (subst-t T t T_1)) ...)])

(define-metafunction H
  subst-t : T t T -> T
  [(subst-t T t Int) Int]
  [(subst-t T t Bool) Bool]
  [(subst-t T t (T_1 → T_2))
   ((subst-t T t T_1) → (subst-t T t T_2))]
  [(subst-t T t t) T]
  [(subst-t T t t_0) t_0])

(define-metafunction H
  infer : Γ ⊢ e -> T or #f
  [(infer Γ ⊢ e)
   T
   (judgment-holds (typeof Γ ⊢ e : T_0 C_0))
   (where C (unify C_0))
   (where T (apply-soln C T_0))]
  [(infer Γ ⊢ e) #f])

(define-metafunction H
  apply-soln : C T -> T
  [(apply-soln () T) T]
  [(apply-soln ((t = T) any ...) T_0)
   (subst-t T t (apply-soln (any ...) T_0))])
}|


@bold{Thu Oct 15 15:29:55 EDT 2015}

Here's the code from last time (two parts).

@verbatim|{
#lang racket
(require redex)
(provide F lookup ext)

(define-language F
  (e ::= i b x
     (Succ e)
     (Plus e e)
     (If e e e)
     (Fun x e)
     (App e e))

  (x ::= variable-not-otherwise-mentioned)
  (i ::= integer)
  (b ::= True False)

  ;; Types
  (T ::= Int Bool (T → T))
  ;; Type environment
  (Γ ::= ((x T) ...)))

(define-metafunction F
  lookup : Γ x -> T
  [(lookup ((x T) _ ...) x) T]
  [(lookup (_ (x_0 T_0) ...) x)
   (lookup ((x_0 T_0) ...) x)])

(define-metafunction F
  ext : Γ x T -> Γ
  [(ext ((x_0 T_0) ...) x T) ((x T) (x_0 T_0) ...)])

(define-judgment-form F
  #:mode (typeof I I I I O)
  #:contract (typeof Γ ⊢ e : T)

  [--------------------
   (typeof Γ ⊢ i : Int)]

  [--------------------
   (typeof Γ ⊢ b : Bool)]
  
  [(where T (lookup Γ x))
    ---------------------
   (typeof Γ ⊢ x : T)]

  [(typeof Γ ⊢ e : Int)
   --------------------
   (typeof Γ ⊢ (Succ e) : Int)]

  [(typeof Γ ⊢ e_0 : Int)
   (typeof Γ ⊢ e_1 : Int)
   --------------------
   (typeof Γ ⊢ (Plus e_0 e_1) : Int)]

  [(typeof Γ ⊢ e_0 : Bool)
   (typeof Γ ⊢ e_1 : T)
   (typeof Γ ⊢ e_2 : T)
   -----------------------
   (typeof Γ ⊢ (If e_0 e_1 e_2) : T)]

  ;; This doesn't work
  #;
  [(typeof (ext Γ x T_1) ⊢ e : T_2)
   --------------------------------
   (typeof Γ ⊢ (Fun x e) : (T_1 → T_2))])
  

(judgment-holds (typeof ((x Int)) ⊢ x : Int))
(judgment-holds (typeof ((x Int) (x Bool)) ⊢ x : Int))
(judgment-holds (typeof ((x Int) (x Bool)) ⊢ x : Bool))
}|

Here is the rest:

@verbatim|{
#lang racket
(require redex)
(require "types.rkt")

(define-extended-language G F
  (e ::= .... (Fun x T e)))

(define-judgment-form F
  #:mode (typeof I I I I O)
  #:contract (typeof Γ ⊢ e : T)

  ;; This works because of explicit type annotations.
  [(typeof (ext Γ x T_1) ⊢ e : T_2)
   --------------------------------
   (typeof Γ ⊢ (Fun x T_1 e) : (T_1 → T_2))]

  [(typeof Γ ⊢ e_1 : (T_1 → T_2))
   (typeof Γ ⊢ e_2 : T_1)
   -----------------------
   (typeof Γ ⊢ (App e_1 e_2) : T_2)])

}|


@bold{Thu Oct  8 15:22:30 EDT 2015}

I've extended the deadline for @secref{PS3} by one week and for
@secref{RP1} by 2 days.

I've added several potential project ideas.

@bold{Thu Oct  8 15:00:21 EDT 2015}

Here's the code from Tuesday:

@verbatim|{
type exp = 
  | Int of int
  | Bool of bool
  | Var of var
  | Plus of exp * exp
  | Mult of exp * exp
  | If of exp * exp * exp
  | Let of var * exp * exp
  (* added *)
  | App of exp * exp
  | Fun of var * exp
 and var = string

type value =
  | VInt of int
  | VBool of bool
  (* added *)
  | VFun of f
 and f =
   | F1 of var * env * exp (* lexical scope *)
(* | F1 of var * exp *)    (* dynamic scope *)

type env = (var * value) list


let rec eval (r : env) (e : exp) : value =
  match e with
  | Int i -> VInt i
  | Bool b -> VBool b
  | Var x -> List.assoc x r
  | Plus (e1, e2) -> 
     let VInt v1 = eval r e1 in
     let VInt v2 = eval r e2 in
     VInt (v1 + v2)
  | Mult (e1, e2) ->
     let VInt v1 = eval r e1 in
     let VInt v2 = eval r e2 in
     VInt (v1 * v2)
  | If (e1, e2, e3) ->
     let VBool b = eval r e1 in
     if b then eval r e2 else eval r e3
  | App (e1, e2) ->
     let VFun f = eval r e1 in
     let v = eval r e2 in
     (match f with
      (* lexical scope *)
      | F1 (x, r, e) -> 
	 eval ((x,v)::r) e)
      (* dynamic scope *)
(*    | F1 (x, e) ->
	 eval ((x,v)::r) e)
*)

  | Fun (x, e) ->
      (* functional representation *)
      (* (fun v -> eval ((x,v)::r) e) *)

      (* closure representation, lexical *)
      VFun (F1 (x, r, e))
      (* dynamic *)
      (* VFun (F1 (x, e)) *)

  | Let (x, e1, e2) ->
     let v = eval r e1 in
     eval ((x,v)::r) e2
}|

@bold{Thu Sep 24 15:25:40 EDT 2015}

I am removing problem 0 from @secref{PS2}; it will be added to @secref{PS3}.

@bold{Tue Sep 22 23:00:54 EDT 2015}

New pair:

@verbatim|{
pair31: ashbondu (ashwin.lg341@g), xmaumd (xinyuanma521@g)
}|


Code from today:

@verbatim|{
#lang racket
(require redex)

(define-language A
  (e ::=
     i
     (Succ e)
     (Pred e)
     (Plus e e)
     (Mult e e)
     (Amb e e))
  (i j ::= integer)
  (E ::=
     hole
     (Succ E)
     (Pred E)
     (Plus e E)
     (Plus E i)
     (Mult E e)
     (Mult i E)
     (Amb E e)
     (Amb i E)))


(define-extended-language B A
  (e ::= .... 
     (If e e e)
     (Div e e)
     True
     False)
  (E ::= ....
     (If E e e)
     (Div E e)
     (Div i E))
  (v ::= i True False)

  (t :: Bool Int))
  
  ;; (t ::= True False Zero Pos Neg One
  ;;        Two Three NegOne ... (∪ t t))


(define r
  (reduction-relation
   A
   #:domain e
   (--> (Succ i) ,(+ (term i) 1))
   (--> (Pred i) ,(- (term i) 1))
   (--> (Plus i_1 i_2) ,(+ (term i_1) (term i_2)))
   (--> (Mult i_1 i_2) ,(* (term i_1) (term i_2)))
   (--> (Amb i_1 i_2) i_1)
   (--> (Amb i_1 i_2) i_2)))

(define →r (compatible-closure r A e))
(define ↦r (context-closure r A E))

(define rb
  (reduction-relation
   B
   #:domain e
   (--> (If True e_1 e_2) e_1)
   (--> (If False e_1 e_2) e_2)   
   (--> (Div i_1 i_2) ,(floor (/ (term i_1) (term i_2)))
        (side-condition (not (zero? (term i_2)))))
   ))

#;
(define rb
  (reduction-relation
   B
   #:domain e
   (--> (If 0 e_1 e_2) e_1)
   (--> (If i e_1 e_2) e_2
        (side-condition (not (zero? (term i)))))
   (--> True 0)
   (--> False 1)   
   ; Div
   ))

(define ↦rb
  (context-closure (union-reduction-relations
                    rb
                    (extend-reduction-relation r B))
                   B E))

#;
(define ↦r
  (reduction-relation
   A
   #:domain e
   (--> (in-hole E (Succ i))
        (in-hole E (in-hole E ,(+ (term i) 1))))
   (--> (in-hole E (Pred i)) (in-hole E ,(- (term i) 1)))
   ;...
   ))

(test-->>  →r (term (Succ 4)) (term 5))
(test-->>∃ →r (term (Amb 4 5)) (term 5))
(test-->>  →r (term (Amb 4 5)) (term 5) (term 4))

(redex-check
 A e
 (redex-match A (i ...)
              (apply-reduction-relation* →r (term e))))

(define-judgment-form B
  #:mode (⊢ I O)
  #:contract (⊢ e t)
  [-------
   (⊢ e Bool)]
  [-------
   (⊢ e Int)])

#;
(define-judgment-form B
  #:mode (⊢ I O)
  #:contract (⊢ e t)
  [-------
   (⊢ i Int)]
  [--------
   (⊢ True Bool)]
  [--------
   (⊢ False Bool)]
  [(⊢ e Int)
   -------
   (⊢ (Succ e) Int)]
  [(⊢ e Int)
   -------
   (⊢ (Pred e) Int)]
  [(⊢ e_1 Int) (⊢ e_2 Int)
   -------
   (⊢ (Plus e_1 e_2) Int)]
  [(⊢ e_1 Int) (⊢ e_2 Int)
   -------
   (⊢ (Mult e_1 e_2) Int)]
  [(⊢ e_1 Int) (⊢ e_2 Int)
   -------
   (⊢ (Div e_1 e_2) Int)]
  [(⊢ e_1 Bool) (⊢ e_2 Bool) (⊢ e_3 Bool)
   ------
   (⊢ (If e_1 e_2 e_3) Bool)]
  [(⊢ e_1 Bool) (⊢ e_2 Int) (⊢ e_3 Int)
   ------
   (⊢ (If e_1 e_2 e_3) Int)])


  
  
(redex-check
 B #:satisfying (⊢ e t)
 (redex-match B (v ...)
              (apply-reduction-relation* ↦rb (term e))))


  
#|
(define-metafunction A
  ⇓ : e -> i
  [(⇓ i) i]
  [(⇓ (Plus e_0 e_1))
   ,(+ (term i_0) (term i_1))
   (where i_0 (⇓ e_0))
   (where i_1 (⇓ e_1))])

(build-derivations (⇓ (Plus 3 4)))
|#


(define-judgment-form A
  #:mode (⇓ I O)
  #:contract (⇓ e i)
  [-------
   (⇓ i i)]

  [(⇓ e_0 i_0) (⇓ e_1 i_1)
   ------------------------
   (⇓ (Plus e_0 e_1) ,(+ (term i_0) (term i_1)))]

  [(⇓ e_0 i_0)
   ------------------------
   (⇓ (Succ e_0) ,(+ (term i_0) 1))]

  [(⇓ e_0 i_0)
   ------------------------
   (⇓ (Amb e_0 e_1) i_0)]

  [(⇓ e_1 i_1)
   ------------------------
   (⇓ (Amb e_0 e_1) i_1)])

(judgment-holds (⇓ 5 5))
(judgment-holds (⇓ 5 i) i)
}|

@bold{Tue Sep 22 15:22:19 EDT 2015}

As announced in class, I've postponed the deadline for @secref{PS2} by one week.



@bold{Thu Sep 17 17:04:03 EDT 2015}

Today's code:

@verbatim|{
#lang racket
(require redex)

(define-language A
  (e ::=
     i
     (Succ e)
     (Pred e)
     (Plus e e)
     (Mult e e)
     (Amb e e))
  (i j ::= integer)
  (E ::=
     hole
     (Succ E)
     (Pred E)
     (Plus e E)
     (Plus E i)
     (Mult E e)
     (Mult i E)
     (Amb E e)
     (Amb i E)))

(define r
  (reduction-relation
   A
   #:domain e
   (--> (Succ i) ,(+ (term i) 1))
   (--> (Pred i) ,(- (term i) 1))
   (--> (Plus i_1 i_2) ,(+ (term i_1) (term i_2)))
   (--> (Mult i_1 i_2) ,(* (term i_1) (term i_2)))
   (--> (Amb i_1 i_2) i_1)
   (--> (Amb i_1 i_2) i_2)))

(define →r (compatible-closure r A e))
(define ↦r (context-closure r A E))

#;
(define ↦r
  (reduction-relation
   A
   #:domain e
   (--> (in-hole E (Succ i))
        (in-hole E (in-hole E ,(+ (term i) 1))))
   (--> (in-hole E (Pred i)) (in-hole E ,(- (term i) 1)))
   ;...
   ))

(test-->>  →r (term (Succ 4)) (term 5))
(test-->>∃ →r (term (Amb 4 5)) (term 5))
(test-->>  →r (term (Amb 4 5)) (term 5) (term 4))

(redex-check
 A e
 (redex-match A (i ...)
              (apply-reduction-relation* →r (term e))))

#|
(define-metafunction A
  ⇓ : e -> i
  [(⇓ i) i]
  [(⇓ (Plus e_0 e_1))
   ,(+ (term i_0) (term i_1))
   (where i_0 (⇓ e_0))
   (where i_1 (⇓ e_1))])

(build-derivations (⇓ (Plus 3 4)))
|#


(define-judgment-form A
  #:mode (⇓ I O)
  #:contract (⇓ e i)
  [-------
   (⇓ i i)]

  [(⇓ e_0 i_0) (⇓ e_1 i_1)
   ------------------------
   (⇓ (Plus e_0 e_1) ,(+ (term i_0) (term i_1)))]

  [(⇓ e_0 i_0)
   ------------------------
   (⇓ (Succ e_0) ,(+ (term i_0) 1))]

  [(⇓ e_0 i_0)
   ------------------------
   (⇓ (Amb e_0 e_1) i_0)]

  [(⇓ e_1 i_1)
   ------------------------
   (⇓ (Amb e_0 e_1) i_1)])

(judgment-holds (⇓ 5 5))
(judgment-holds (⇓ 5 i) i)
}|

@bold{Thu Sep 17 14:42:58 EDT 2015}

Teams:

@verbatim|{
pair23: KonstantinosX (kostasx@cs), jameslitton (litton@cs) 
pair24: Umar-Farooqui (umar.a.farooqui@g), davdar (darais@cs)
pair25: Uyas (li3shuo1@g), vcepeda (vcepeda@cs)
pair26: meethu24 (meethu24@g), aconz2 (andrewconsroe@g)
pair27: Isweet (isweet@t), wir963 (wir963@g)
pair28: osuciu (osuciu@cs), mckaz (milod@u)
pair29: rstevens70 (stevens.rock@g), iobender (matthewbender77@g)
pair30: angel6392 (aplane@u), bbengfort (benjamin@b)
pair31: ashbondu (ashwin.lg341@g), xmaumd (xinyuanma521@g)

key: cs = cs.umd.edu, g = gmail.com, 
     t = terpmail.umd.edu, u = umd.edu,
     b = bengfort.com
}|

Missing: Segev, Yoav; Sekniqi, Kevin.

@bold{Tue Sep 15 20:28:40 EDT 2015}

Here is the code from today:

@verbatim|{
#lang racket
(require redex)

(define-language A
  (e ::=
     i
     (Succ e)
     (Pred e)
     (Plus e e)
     (Mult e e)
     (Amb e e))
  (i j ::= number))

(define r
  (reduction-relation
   A
   #:domain e
   (--> (Succ i) ,(+ (term i) 1))
   (--> (Pred i) ,(- (term i) 1))
   (--> (Plus i_1 i_2) ,(+ (term i_1) (term i_2)))
   (--> (Mult i_1 i_2) ,(* (term i_1) (term i_2)))
   (--> (Amb i_1 i_2) i_1)
   (--> (Amb i_1 i_2) i_2)))

(define ->r (compatible-closure r A e))
}|

@bold{Wed Sep  9 14:02:41 EDT 2015}

Small update: I won't have the marked-up essays back by tomorrow, but
I will try to have them done over the weekend.

As promised, here are some notes on OCaml basics:

@url{https://www.cs.umd.edu/class/spring2015/cmsc330/lectures/06-ocaml.pdf}

@bold{Tue Sep  1 02:52:10 EDT 2015}

There will be no lecture September 1 or September 3 because I am
attending the International Conference on Functional Programming.

However THERE IS AN ASSIGNMENT, which is due by midnight on Thursday,
9/3.  Please see @secref{PS1}.


@bold{Tue Aug 25 14:07:08 EDT 2015}

Welcome to CMSC631!  This ``blog'' is where course announcements will be
made; be sure to check it regularly.  -- David
