#lang scribble/manual

@(define (gh p) (link (format "https://github.com/cmsc631/~a" p) (format "~a" p)))

@title{Blog}

@bold{Thu Mar 27 17:44:05 EDT 2014}

Here is the OCaml code for the machine semantics of the higher-order,
imperative language we designed in class today:

@verbatim|{
type iexp = 
  | E_Ref of iexp
  | E_Deref of iexp
  | E_Update of iexp * iexp
  | E_Seq of iexp * iexp
  | E_Int of int
  | E_Bool of bool
  | E_Var of string
  | E_Pred of iexp
  | E_Succ of iexp
  | E_Plus of iexp * iexp
  | E_Mult of iexp * iexp
  | E_Div of iexp * iexp
  | E_If of iexp * iexp * iexp
  | E_App of iexp * iexp
  | E_Fun of string * iexp

type ans =
    A_Val of value * sto
  | A_Err of string
and sto = (int * value) list
and value =
  | V_Ref of int 
  | V_Int of int
  | V_Bool of bool
  | V_Fun of string * iexp * env
and env = (string * value) list

type cont =
  | K_Mt
  | K_Ref of cont
  | K_Deref of cont
  | K_UpdateL of iexp * env * cont
  | K_UpdateR of value * cont
  | K_SeqL of iexp * env * cont
  | K_SeqR of value * cont
  | K_AppL of iexp * env * cont
  | K_AppR of value * cont
  | K_Pred of cont
  | K_Succ of cont
  | K_PlusL of iexp * env * cont
  | K_PlusR of value * cont
  | K_MultL of iexp * env * cont
  | K_MultR of value * cont
  | K_DivL of iexp * env * cont
  | K_DivR of value * cont
  | K_If of iexp * iexp * env * cont
 

exception Unbound of string
exception Illegal_Ref
exception Not_Implemented

let rec lookup (r : env) (x : string) : value =
  match r with
      [] -> raise (Unbound x)
    | (y,v)::r -> if (x = y) then v else lookup r x

let rec allocate (s : sto) : int =
  match s with
    | [] -> 0
    | ((l,v)::s) ->
	(allocate s) + 1

let rec sto_lookup (s : sto) (i : int) : value =
  match s with
    | [] -> raise Illegal_Ref
    | (j,v)::s ->
	if (j=i) then v else sto_lookup s i

let rec sto_update (s : sto) (i : int) (v : value) : sto =
  match s with
    | [] -> raise Illegal_Ref
    | (j,v1)::s ->
	if (j=i) then (j,v)::s else (j,v1)::(sto_update s i v)


let rec ev (e : iexp) (r : env) (s : sto) (k : cont) : ans =
  match e with
    | E_Ref e -> ev e r s (K_Ref k)
    | E_Deref e -> ev e r s (K_Deref k)
    | E_Update (e1, e2) -> ev e1 r s (K_UpdateL (e2, r, k))
    | E_Seq (e1, e2) -> ev e1 r s (K_SeqL (e2, r, k))
    | E_Int i -> co k (V_Int i) s
    | E_Bool b -> co k (V_Bool b) s
    | E_Var x -> co k (lookup r x) s
    | E_Pred e -> ev e r s (K_Pred k)
    | E_Succ e -> ev e r s (K_Succ k)
    | E_Plus (e1, e2) -> ev e1 r s (K_PlusL (e2, r, k))
    | E_Mult (e1, e2) -> ev e1 r s (K_MultL (e2, r, k))
    | E_Div (e1, e2) -> ev e1 r s (K_DivL (e2, r, k))
    | E_If (e1, e2, e3) -> ev e1 r s (K_If (e2, e3, r, k))
    | E_App (e1, e2) -> ev e1 r s (K_AppL (e2, r, k))
    | E_Fun (x, e) -> co k (V_Fun (x, e, r)) s
and co (k : cont) (v : value) (s : sto) : ans =
  match k with
    | K_Mt -> A_Val (v, s)
    | K_Ref k -> 
	let l = allocate s in
	  co k (V_Ref l) ((l,v)::s)
    | K_Deref k ->
	(match v with
	  | V_Ref i -> co k (sto_lookup s i) s
	  | _ -> A_Err "not a reference")
    | K_UpdateL (e, r, k) -> ev e r s (K_UpdateR (v, k))
    | K_UpdateR (v1, k) -> 
	(match v1 with
	  | V_Ref i -> co k v1 (sto_update s i v)
	  | _ -> A_Err "not a reference")
    | K_SeqL (e, r, k) -> ev e r s (K_SeqR (v, k))	
    | K_SeqR (v1, k) -> co k v1 s
    | K_AppL (e, r, k) -> ev e r s (K_AppR (v, k))
    | K_AppR (f, k) ->
	(match f with
	   | V_Fun (x, e, r) -> ev e ((x,v)::r) s k
	   | _ -> A_Err "not a function")
    | K_Pred k ->
	(match v with
	   | V_Int i -> co k (V_Int (i-1)) s
	   | _ -> A_Err "not an integer")
    | K_Succ k ->
	(match v with
	   | V_Int i -> co k (V_Int (i+1)) s
	   | _ -> A_Err "not an integer")
    | K_PlusL (e, r, k) -> ev e r s (K_PlusR (v, k))
    | K_PlusR (u, k) -> 
	(match (u, v) with
	   | (V_Int i, V_Int j) -> co k (V_Int (i+j)) s
	   | _ -> A_Err "not an integer")
    | K_MultL (e, r, k) -> ev e r s (K_MultR (v, k))
    | K_MultR (u, k) -> 
	(match (u, v) with
	   | (V_Int i, V_Int j) -> co k (V_Int (i*j)) s
	   | _ -> A_Err "not an integer")
    | K_DivL (e, r, k) -> ev e r s (K_DivR (v, k))
    | K_DivR (u, k) -> 
	(match (u, v) with
	   | (V_Int i, V_Int 0) -> A_Err "div by zero"
	   | (V_Int i, V_Int j) -> co k (V_Int (i/j)) s
	   | _ -> A_Err "not an integer")
    | K_If (e2, e3, r, k) ->
	(match v with
	   | V_Bool true -> ev e2 r s k
	   | V_Bool false -> ev e3 r s k
	   | _ -> A_Err "not a boolean")

let eval e = ev e [] [] K_Mt

let two = E_Fun ("s", E_Fun ("z", E_App (E_Var "s", E_App (E_Var "s", E_Var "z"))))
let succ = E_Fun ("x", (E_Succ (E_Var "x")))
let do_256 = E_App (two, (E_App (two, E_App (two, two))))
let v1 = eval (E_App (E_App (do_256, succ), E_Int 0))

let succ_bang = E_Fun("b", E_Update (E_Var "b", E_Succ (E_Deref (E_Var "b"))))
let v2 = eval (E_App (E_App (do_256, succ_bang), (E_Ref (E_Int 0))))
}|

@bold{Thu Mar 27 15:18:06 EDT 2014}

@secref{RP3} is up.

@bold{Wed Mar 12 10:44:32 EDT 2014}

@emph{La Technique}

Creative scholarship (@emph{a.k.a.} research) shares much in common
with any other creative discipline such as programming, playing music,
wood working, and cooking food.  To be a master of one of these
disciplines requires being a master of @emph{technique}.  Only after
mastering technique do you have a chance of being creative.

With that in mind, I thought I'd share
@link["http://www.nytimes.com/2011/10/19/dining/jacques-pepin-demonstrates-cooking-techniques.html"]{an
article} on one of my favorite technicians from another discipline:
Jacques Pépin.  Read it and think of how it relates to your own world.
``Repeat, repeat, repeat, repeat until it becomes part of yourself.''

@bold{Tue Mar 11 10:40:28 EDT 2014}

I've given some more detail about what should be included in the
project proposals (@secref{RP2}).  Sorry this is so late in going out.

@bold{Fri Mar  7 09:40:30 EST 2014}

Partners for project, with github usernames:

@itemlist[
@item{@gh{proj01}: Andrew Ruef (@tt{awruef}), David Hogarty (@tt{flyingsymbols})}
@item{@gh{proj02}: Casey Mihaloe (@tt{cmihaloe}), John Morgan (@tt{johnjosephmorgan})}
@item{@gh{proj03}: Becca MacKenzie (@tt{rmacnz})}
@item{@gh{proj04}: Zebao Gao (@tt{gaozebao}), Emily Hand (@tt{ehand})}
@item{@gh{proj05}: Xiao Wang (@tt{wangxiao1254}), Kartik Nayak (@tt{kartik1507})}
@item{@gh{proj06}: Phil Nguyen (@tt{philnguyen}), Jon Fetter-Deges (@tt{jonfetterdegges})}
@item{@gh{proj07}: Garrett Katz (@tt{garrettkatz}), Tommy Pensyl (@tt{tpensyl})}
@item{@gh{proj08}: Javran Chang (@tt{Javran})}
@item{@gh{proj09}: Rohit Ramesh (@tt{rohit507})}
@item{@gh{proj10}: Saeed Seddighin (@tt{saeedseddighin}), Hossein Esfandiari (@tt{esfandiari})}
@item{@gh{proj11}: Alex Yu (@tt{YuRHere})}
]

@bold{Thu Mar  6 16:48:58 EST 2014}

Here's the code we wrote in class today.

We started from a typed version of the code from Tuesday and added
throw and catch to language:

@codeblock[#:keep-lang-line? #t]{
#lang typed/racket
(define-type Exp
  (U vbl num bool ife app lam prim1 prim2 throw catch))
(struct: vbl ([name : Symbol]))
(struct: num ([val : Integer]))
(struct: bool ([val : Boolean]))
(struct: ife ([e1 : Exp] [e2 : Exp] [e3 : Exp]))
(struct: app ([e1 : Exp] [e2 : Exp]))
(struct: lam ([x : Symbol] [e : Exp]))
(struct: prim1 ([op : Op1] [e : Exp]))
(struct: prim2 ([op : Op2] [e1 : Exp] [e2 : Exp]))
(struct: throw ([e : Exp]))
(struct: catch ([e : Exp]))

(struct: closure ([e : Exp] [ρ : Env] [x : Symbol]))

(define-type Op1 (U 'succ))
(define-type Op2 (U 'plus))
(define-type Val (U Integer Boolean closure))
(define-type Env (Listof (List Symbol Val)))

(: lookup (Env Symbol -> Val))
(define (lookup ρ x)
  (cond [(empty? ρ) (error "unbound variable")]
        [(eq? x (first (first ρ)))
         (second (first ρ))]
        [else
         (lookup (rest ρ) x)]))

(: extend (Env Symbol Val -> Env))
(define (extend ρ x v)
  (cons (list x v) ρ))

(: apply-function (Val Val (Val -> Val) -> Val))
(define (apply-function f v κ)
  (match f
    [(closure e ρ x)
     (eval e (extend ρ x v) κ)]
    [_ (error "type error: not a function")]))

(: ev (Exp Env -> Val))
(define (ev e ρ)
  (eval e ρ (λ: ([v : Val]) v)))

(: eval (Exp Env (Val -> Val) -> Val))
(define (eval e ρ κ)
  (match e
    [(vbl x) (κ (lookup ρ x))]
    [(num v) (κ v)]
    [(bool v) (κ v)]
    [(catch e)
     (κ (eval e ρ (λ: ([v : Val]) v)))]
    [(throw e)
     (eval e ρ (λ: ([v : Val]) v))]
    [(ife test then else)
     (eval test ρ
           (λ: ([v : Val])
             (if v
                 (eval then ρ κ)
                 (eval else ρ κ))))]
    [(app fun arg)
     (eval fun ρ
           (λ: ([f : Val])
             (eval arg ρ
                   (λ: ([v : Val])
                     (apply-function f v κ)))))]
    [(lam x e)
     (κ (closure e ρ x))]
    [(prim1 op e)
     (eval e ρ
           (λ: ([v : Val])             
             (case op
               [(succ)
                (if (integer? v)
                    (κ (add1 v))
                    (error "type error: expected integer"))])))]
                       
    [(prim2 op e1 e2)
     (eval e1 ρ
           (λ: ([v1 : Val])
             (eval e2 ρ
                   (λ: ([v2 : Val])
                     (case op
                       [(plus)
                        (if (and (integer? v1) (integer? v2))
                            (κ (+ v1 v2))
                            (error "type error: expected integers"))])))))]))
}

We observed a couple things: this code is in continuation passing
style, which means all non-total functions are called in tail position
with trivial arguments---with the exception of @code{catch}.  It's
possible to transform the program again by introducing another
explicit continuation argument.  The two would represent the local
context (up to the nearest enclosing @code{catch}) and the outer
context surrounding the nearest enclosing catch.

We then punted on exceptions and defunctionalized the code:

@codeblock[#:keep-lang-line? #f]{
#lang typed/racket
(define-type Cont
  (U evk ifk appk1 appk2 prim1k prim2k1 prim2k2))
(struct: evk ())
(struct: ifk ([e2 : Exp] [e3 : Exp] [ρ : Env] [κ : Cont]))
(struct: appk1 ([e2 : Exp] [ρ : Env] [κ : Cont]))
(struct: appk2 ([f : Val] [κ : Cont]))
(struct: prim1k ([op : Op1] [κ : Cont]))
(struct: prim2k1 ([op : Op2] [e2 : Exp] [ρ : Env] [κ : Cont]))
(struct: prim2k2 ([op : Op2] [v1 : Val] [κ : Cont]))

(: ev (Exp Env -> Val))
(define (ev e ρ)  
  (eval e ρ (evk)))

(: eval (Exp Env Cont -> Val))
(define (eval e ρ κ)
  (match e
    [(vbl x) (continue κ (lookup ρ x))]
    [(num v) (continue κ v)]
    [(bool v) (continue κ v)]    
    [(ife e1 e2 e3)
     (eval e1 ρ (ifk e2 e3 ρ κ))]
    [(app e1 e2)
     (eval e1 ρ (appk1 e2 ρ κ))]           
    [(lam x e)
     (continue κ (closure e ρ x))]      
    [(prim1 op e)
     (eval e ρ (prim1k op κ))]
    [(prim2 op e1 e2)
     (eval e1 ρ (prim2k1 op e2 ρ κ))]))

(: apply-function (Val Val Cont -> Val))
(define (apply-function f v κ)
  (match f
    [(closure e ρ x)
     (eval e (extend ρ x v) κ)]
    [_ (error "type error: not a function")]))

(: continue (Cont Val -> Val))
(define (continue κ v)
  (match κ
    [(evk) v]
    [(ifk e2 e3 ρ κ)
     (if v
         (eval e2 ρ κ)
         (eval e3 ρ κ))]
    [(appk1 e2 ρ κ)
     (eval e2 ρ (appk2 v κ))]
    [(appk2 f κ)
     (apply-function f v κ)]
    [(prim1k op κ)
     (case op
       [(succ) 
        (if (integer? v)
            (continue κ (add1 v))
            (error "type error: expected integer"))])]
    [(prim2k1 op e2 ρ κ)
     (let ((v1 v))
       (eval e2 ρ (prim2k2 op v1 κ)))]
    [(prim2k2 op v1 κ)
     (let ((v2 v))
       (case op
         [(plus)
          (if (and (integer? v1) (integer? v2))
              (continue κ (+ v1 v2))
              (error "type error: expected integers"))]))]))
}

This code consists of three mutually recursive, first-order functions
in which all calls are tail calls (and could be implemented as gotos).
So this derived interpreter is a first-order state transition system,
which is known as an abstract machine.  By the correctness of our
program transformations, we know it is a correct low-level
implementation of the language.

@bold{Wed Mar  5 10:30:27 EST 2014}

Here's the code we wrote in class yesterday.

We started with a data represtation of programs:

@codeblock[#:keep-lang-line? #t]{
#lang racket
;; An Exp is one of:
;; - (var Symbol)
;; - (num Number)
;; - (bool Boolean)
;; - (ife Exp Exp Exp)
;; - (app Exp Exp)
;; - (lam Symbol Exp)
;; - (prim1 Op Exp)
;; - (prim2 Op Exp Exp)
(struct vbl (name))
(struct num (val))
(struct bool (val))
(struct ife (test then else))
(struct app (fun arg))
(struct lam (var exp))
(struct prim1 (op arg))
(struct prim2 (op arg1 arg2))
}

We wrote some operations on environments:
@codeblock[#:keep-lang-line? #f]{
#lang racket
;; Env Symbol -> Val
(define (lookup ρ x)
  (cond [(empty? ρ) (error "unbound variable")]
        [(eq? x (first (first ρ)))
         (second (first ρ))]
        [else
         (lookup (rest ρ) x)]))

;; Env Symbol Val -> Env
(define (extend ρ x v)
  (cons (list x v) ρ))
}

Then we wrote an evaluator that represented functions as (Racket) functions:

@codeblock[#:keep-lang-line? #f]{
#lang racket
;; Exp Env -> Val
(define (ev e ρ)
  (match e
    [(vbl x) (lookup ρ x)]
    [(num v) v]
    [(bool v) v]
    [(ife test then else)
     (if (ev test ρ)
         (ev then ρ)
         (ev else ρ))]
    [(app fun arg)
     (let ((f (ev fun ρ))
           (v (ev arg ρ)))
       (f a))]
    [(lam x e)
     (λ (v)
       (ev e (extend ρ x v)))]
    [(prim1 op e)
     (let ((v (ev e ρ)))
       (case op 
         [(succ) (add1 v)]))]
    [(prim2 op e1 e2)
     (let ((v1 (ev e1 ρ))
           (v2 (ev e2 ρ)))
       (case op
         [(plus) (+ v1 v2)]))]))
}

We then talked about how to eliminate the use of functions by
representing them with a data structure.

@codeblock[#:keep-lang-line? #f]{
#lang racket
(struct closure (e ρ x))

;; Exp Env -> Val
(define (ev e ρ)
  (match e
    ...
    [(app fun arg)
     (let ((f (ev fun ρ))
           (v (ev arg ρ)))
       (match f
         [(closure e ρ x)
          (ev e (extend ρ x v))]))]
    [(lam x e)
     (closure e ρ x)]
    ...))
}

Then we discussed an explicit control evaluator:

@codeblock[#:keep-lang-line? #f]{
#lang racket
;; Exp Env -> Val
(define (ev e ρ)
  ;; Exp Env (Val -> Val) -> Val
  (define (eval e ρ κ)
    (match e
      [(vbl x) (κ (lookup ρ x))]
      [(num v) (κ v)]
      [(bool v) (κ v)]
      [(exn e)
       (eval e ρ
             (λ (v) v))]
      [(ife test then else)
       (eval test ρ
             (λ (v)           
               (if v
                   (eval then ρ κ)
                   (eval else ρ κ))))]
      [(app fun arg)
       (eval fun ρ
             (λ (f)
               (eval arg ρ
                     (λ (v)
                       (match f
                         [(closure e ρ x)
                          (eval e (extend ρ x v) κ)])))))]
      [(lam x e)
       (κ (closure e ρ x))]      
      [(prim1 op e)
       (eval e ρ
             (λ (v)
               (case op
                 [(succ) (κ (add1 v))])))]
      [(prim2 op e1 e2)
       (eval e1 ρ
             (λ (v1)
               (eval e2 ρ
                     (λ (v2)
                       (case op
                         [(plus)
                          (κ (+ v1 v2))])))))]))
  (eval e ρ (λ (v) v)))
}

@bold{Thu Feb 27 17:14:13 EST 2014}

Slides from Aseem's talk are @link["papers/wysteria-631.pdf"]{here}.

Slides from Matt's talk are @link["papers/2014.02.27-dvh631-ic-lecture.pdf"]{here}.

@bold{Thu Feb 27 15:24:46 EST 2014}

I wrote several problems for @secref{PS3} but decided I didn't like
them.  So instead there is just a single problem for @secref{PS3},
which you should complete with your current partner.  It is due March
6 at midnight.

Partners for projects are due by March 6 as well.  First draft of
proposals are due March 11.  You will get feedback by March 13.

@bold{Fri Feb 21 15:47:57 EST 2014}

A Piazza forum has been set up for the
@link["https://piazza.com/umd/spring2014/cmsc631/home"]{course}.
@link["https://piazza.com/umd/spring2014/cmsc631"]{Sign up} to
participate in discussions.

You may discuss any topic from class, but it may be useful in
particular to pitch project ideas and find like-minded partners to
work with on the research project.

@bold{Fri Feb 21 12:02:20 EST 2014}

The @link["notes.pdf"]{notes} have been updated to include material on
symbolic execution and type inference.  I made a bad mistake in the
way unification was presented in class, so be sure to read the notes.

@bold{Thu Feb 20 17:16:59 EST 2014}

Some ideas for @secref{Potential_projects} have been posted.

@bold{Fri Feb 14 00:14:13 EST 2014}

Be sure to fire up DrRacket at some point today.

@bold{Thu Feb 13 13:37:28 EST 2014}

A helpful hint on problem 5: first, develop the reduction relation
with pencil and paper as a set of inference rules; then translate the
rules into a relation in Redex using @tt{define-judgment-form}; then
lift this judgment into a @tt{reduction-relation} using the
@tt{judgment-holds} condition.

Here is a potentially helpful analogy.  It formalizes the language of
natural numbers and defines the ``@tt{>=}'' relation.  It then makes a
reduction relation out of this relation so that @tt{N --> N'} if
@tt{N} is greater than or equal to @tt{N'}.  It visualizes an example
for the number 3.

@codeblock[#:keep-lang-line? #t]|{
#lang racket
(require redex)

(define-language Natural
  (N ::= Z (S N)))

(define-judgment-form Natural
  #:mode (>= I O)
  [(>= N N)]
  [(>= N_0 N_1)
   ---------------
   (>= (S N_0) N_1)])

(define ->>= 
  (reduction-relation 
   Natural
   (--> N_0 N_1
        (judgment-holds (>= N_0 N_1)))))

(traces ->>= (term (S (S (S Z)))))
}|

@bold{Thu Feb 13 12:12:41 EST 2014}

@bold{Snow day!}  There's no class today, as the university is closed.
However, I still expect @secref{PS2} to be submitted tonight by
midnight (except for the memo portion).

Since I heard no objections, I have posted the redacted, marked-up
@link["papers/memos-marked_Redacted.pdf"]{memos}.

@bold{Tue Feb 11 18:47:04 EST 2014}

The @link["notes.pdf"]{notes} have been updated with content on typing
and abstract interpretation with types.

@bold{Tue Feb 11 15:11:07 EST 2014}

Since the memos were handed back late (in class today), I am extending
the deadline for the revised memo part of @secref{PS2} to next Tuesday at
midnight.  The rest of PS2 is still due at the posted time (this
Thursday at midnight).

@bold{Fri Feb  7 16:48:15 EST 2014}

Rohit writes, there are two ways to get a better OCaml repl:

@itemlist[
@item{@link["http://utopia.knoware.nl/~hlub/rlwrap/#rlwrap"]{rlwrap}:
which wraps any program so that it has history on lines from stdin.}

@item{@link["https://github.com/diml/utop"]{utop}: 
which is a full ocaml repl with many more features than the stadard.}
]

@bold{Thu Feb  6 21:24:24 EST 2014}

The
@link["http://www.cs.umd.edu/class/spring2014/cmsc631/notes.pdf"]{notes}
have been updated with a section on Redex.

@bold{Thu Feb  6 18:41:50 EST 2014}

CMSC 330 has a good
@link["http://www.cs.umd.edu/class/spring2013/cmsc330/lectures/ocaml.pdf"]{slide
deck} on programming in OCaml.

The Emacs mode I used in class today is called
@link["http://tuareg.forge.ocamlcore.org/"]{Tuareg}.

If you find other resources useful, send them to me and I will post
them here.

@bold{Thu Feb  6 18:22:22 EST 2014}

Question on PS2: ``Do you anticipate us writing a lexer and parser for
our interpreter?''

Answer: No.  An interpreter that operates on abstract syntax is all
that is required.

@bold{Wed Feb  5 01:48:11 EST 2014}

The
@link["http://www.cs.umd.edu/class/spring2014/cmsc631/notes.pdf"]{course
notes} have been revised to fix some errors and include discussion of
contexts, evaluation contexts, and standard reduction.

@bold{Tue Feb  4 20:30:40 EST 2014}

Becca caught a couple of errors in the natural semantics section (1.3)
of the notes where I had written the inference rules ``@emph{e evalsto
i} implies @emph{Pred(i) evalsto i-1}'' and similarly for @emph{Succ}
(where ``@emph{evalsto}'' is the natural semantics evaluation
relation).  The rule should be ``@emph{e evalsto i} implies
@emph{Pred(e) evalsto i-1}.''  It will be corrected in the next
iteration of the notes.

Thanks!

@bold{Tue Feb  4 12:16:08 EST 2014}

@secref{PS2} is up.  Several papers on the @secref{Schedule}, but the
precise list and order is still subject to change.

@bold{Fri Jan 31 18:41:56 EST 2014}

For what it's worth, here are the results of the PL popularity contest
from PS1:

@itemlist[
  @item{Java: 4}
  @item{Python: 3}
  @item{C++: 2}
  @item{Haskell: 2}
  @item{Racket: 2}
  @item{Ruby: 1}
  @item{OCaml: 1}
]

@bold{Fri Jan 31 17:06:50 EST 2014}

Teams for PS2:

@verbatim{
@gh{pair01}: jonfetterdegges, Javran
@gh{pair02}: ehand, gaozebao
@gh{pair03}: garrettkatz, flyingsymbols
@gh{pair04}: johnjosephmorgan, cmihaloe
@gh{pair05}: philnguyen, kartik1507
@gh{pair06}: rohit507, tpensyl
@gh{pair07}: wangxiao1254, awruef
@gh{pair08}: YuRHere, rmacnz
}

If you don't see your Github username, that means either you did not
submit PS1 or I've made a mistake.  In either case, email me ASAP or
you won't be able to submit PS2.

For each pair, a private repository has been created for you.  Use
this repository for your work.  If some other system is more useful
for collaborating, feel free to use it, but you must commit your work
to the repository by midnight of the due date for it to be graded.

The actual content of PS2 will be up shortly.

@bold{Thu Jan 30 15:14:35 EST 2014}

I've put together a short set of
@link["http://www.cs.umd.edu/class/spring2014/cmsc631/notes.pdf"]{course
notes} that cover the material for the first lecture or so.  These
notes will be updated after every lecture, if not more often.  The
@link["https://github.com/cmsc631/notes"]{source code} for the notes
are on Github, issues and pull requests are welcome and appreciated.

@bold{Thu Jan 30 11:45:48 EST 2014}

I've scheduled weekly office hours for Wed 1:30--3:30pm 3439 AVW.  You
can also make an appointment at any time via email.

@bold{Mon Jan 27 14:27:59 EST 2014}

Class is cancelled for Tuesday, Jan 28 due to a PI meeting I need to
attend.  Please read the course web page and be sure to start on the
first problem set which is due Thursday, Jan 30.  See you for the
first lecture on Thursday!

@bold{Sat Jan 18 15:09:27 EST 2014}

Welcome to CMSC631!  This ``blog'' is where course announcements will be
made; be sure to check it regularly.  -- David
