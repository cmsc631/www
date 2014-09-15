#lang scribble/manual

@(define (gh p) (link (format "https://github.com/cmsc631/~a" p) (format "~a" p)))

@title{Blog}

@bold{Mon Sep 15 10:21:54 EDT 2014}

Here is the code from Thursday.

OCaml:

@verbatim|{
type bt = 
  | Leaf
  | Node of int * bt * bt

let rec sum (bt : bt) : int =
  match bt with
    | Leaf -> 0
    | Node (i, l, r) -> i + sum l + sum r


sum Leaf;;
sum (Node (4, Node (2, Leaf, Leaf), Node (7, Leaf, Leaf)));;

(* Now abstracting over the type of elements *)

type 'a bt =
  | Leaf
  | Node of 'a * 'a bt * 'a bt

let rec sum (bt : int bt) : int =
  match bt with
    | Leaf -> 0
    | Node (i, l, r) -> i + sum l + sum r


sum Leaf;;
sum (Node (4, Node (2, Leaf, Leaf), Node (7, Leaf, Leaf)));;

let rec inorder (bt : 'a bt) : 'a list =
  match bt with
    | Leaf -> []
    | Node (x, l, r) -> inorder l @ [x] @ inorder r

inorder Leaf;;
inorder (Node (4, Node (2, Leaf, Leaf), Node (7, Leaf, Leaf)));;

(* Evaluator for A lang *)

type expr = 
  | Int of int
  | Pred of expr
  | Succ of expr
  | Mult of expr * expr
  | Plus of expr * expr

let rec eval (e : expr) : int =
  match e with
      Int i -> i
    | Pred e -> eval e - 1
    | Succ e -> eval e + 1
    | Plus (e1, e2) -> eval e1 + eval e2
    | Mult (e1, e2) -> eval e1 * eval e2

(* Works fine because eval relation but also a function. Doesn't work as well
   when it is relation but not a function. *)

type expr = 
  | Int of int
  | Pred of expr
  | Succ of expr
  | Mult of expr * expr
  | Plus of expr * expr
  | Amb of expr * expr (* Ambiguous choice *)

let rec eval (e : expr) : int =
  match e with
    | Int i -> i
    | Pred e -> eval e - 1
    | Succ e -> eval e + 1
    | Plus (e1, e2) -> eval e1 + eval e2
    | Mult (e1, e2) -> eval e1 * eval e2
    | Amb (e1, e2) -> eval e1 
    | Amb (e1, e2) -> eval e2 (* unreachable code *) 

(* We didn't see the rest in class, but here's how we could model 
   an evaluation relation... *)

(* Could model relation as a function returning a set. *)
(* For simplicty, let's model a set as a list. *)

(* We need a helper function to construct the 
   Cartesian product of two lists *)

let cartesian (l : 'a list) (l' : 'b list) : ('a * 'b) list =
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)

let rec eval (e : expr) : int list =
  match e with
    | Int i -> [i]
    | Pred e -> List.map (fun v -> v - 1) (eval e)
    | Succ e -> List.map (fun v -> v + 1) (eval e)
    | Plus (e1, e2) ->
        List.map (fun (v1, v2) -> v1 + v2) (cartesian (eval e1) (eval e2))
    | Mult (e1, e2) ->
        List.map (fun (v1, v2) -> v1 * v2) (cartesian (eval e1) (eval e2))
    | Amb (e1, e2) ->
        eval e1 @ eval e2

eval (Amb (Int 2, Int 3));;
eval (Mult (Int 4, (Amb (Int 2, Int 3))));;
}|

Racket/Redex:

@verbatim|{
#lang racket
;; bt =
;;   | (leaf)
;;   | (node int bt bt)

(struct leaf ())
(struct node (i l r))

(define (sum bt)
  (match bt
    [(leaf) 0]
    [(node i l r)
     (sum i (sum l) (sum r))]))

(define (inorder bt)
  (match bt
    [(leaf) empty]
    [(node i l r)
     (append (inorder l)
             (list i)
             (inorder r))]))


(require redex)

(define-language A
  (e ::= 
     integer 
     (Plus e e)
     (Mult e e)
     (Succ e)
     (Pred e)
     (Amb e e))
  (i ::= integer)
  (E ::= hole
     (Plus E e)
     (Plus i E)
     (Mult E e)
     (Mult i E)
     (Succ E)
     (Pred E)))

(define-judgment-form A
  #:mode (⇓ I O)
  #:contract (⇓ e i)
  [--------
   (⇓ i i)]
  
  [(⇓ e i)
   --------------
   (⇓ (Succ e) (unquote (+ (term i) 1)))]
  
  [(⇓ e i)
   --------------
   (⇓ (Pred e) (unquote (- (term i) 1)))]
  
  [(⇓ e_1 i_1) (⇓ e_2 i_2)
   -----------------------
   (⇓ (Plus e_1 e_2) 
      (unquote (+ (term i_1) (term i_2))))]
  
  [(⇓ e_1 i_1) (⇓ e_2 i_2)
   -----------------------
   (⇓ (Mult e_1 e_2) 
      (unquote (* (term i_1) (term i_2))))]
  
  [(⇓ e_1 i_1)
   -----------------------
   (⇓ (Amb e_1 e_2) i_1)]
  
  [(⇓ e_2 i_2)
   -----------------------
   (⇓ (Amb e_1 e_2) i_2)])

(define a
  (reduction-relation 
   A #:domain e
   (--> (Plus i_1 i_2) ,(+ (term i_1) (term i_2)))
   (--> (Mult i_1 i_2) ,(* (term i_1) (term i_2)))
   (--> (Pred i_1) ,(- (term i_1) 1))
   (--> (Succ i_1) ,(+ (term i_1) 1))
   (--> (Amb e_1 e_2) e_1)
   (--> (Amb e_1 e_2) e_2)))

(define -->_a
  (compatible-closure a A e))

(define -std->_a
  (context-closure a A E))

   
;; Some examples:
#|
(judgment-holds 
   (⇓ (Amb (Succ 5) (Pred 4)) 6))

(judgment-holds 
   (⇓ (Amb (Succ 5) (Pred 4)) i))

(apply-reduction-relation -->_a
                          (Mult (Succ 4) (Pred 4)))
(apply-reduction-relation* -->_a
                           (Mult (Succ 4) (Pred 4)))
(traces -->_a
        (Mult (Succ 4) (Pred 4)))           

(traces -std->_a
        (Mult (Succ 4) (Pred 4)))
|#
}|



@bold{Thu Sep 11 09:25:26 EDT 2014}

I've updated the partner assignments (by editing the blog post below).
I've added real names for convenience.  Everyone's team should have
writeable private repos.  Please veryify @bold{before} @secref{PS2}'s
due date and let me know if there's a problem.

@bold{Tue Sep  9 14:35:05 EDT 2014}

@secref{PS2} will require you to do some programming in OCaml.
CMSC 330 has a good
@link["http://www.cs.umd.edu/class/spring2013/cmsc330/lectures/ocaml.pdf"]{slide
deck} on programming in OCaml.

There's an Emacs mode I use called
@link["http://tuareg.forge.ocamlcore.org/"]{Tuareg}.


@bold{Tue Sep  9 10:15:27 EDT 2014}

I received almost all of the PS1 submissions.  There was one student
who didn't submit anything and a couple students who failed to provide
some of the requested deliverables.  If you are one of these students,
you should have received email.

I've created the initial partner assignments and repositories on
github:

@itemlist[
@item{pair10: labichn (Nicholas Labich)}
@item{pair11: leofanxiong (Xiong Fan), decaturk (Kevin DeCatur)}
@item{pair12: jreeseue (Josh Reese), alexwzk (Zikai Wen)}
@item{pair13: bjkwon90 (Bum Jun Kwon), andrewpachulski (Andrew Pachulski)}
@item{pair14: aidmony, ERJIALI (Erjia Li)}
@item{pair15: eldr4d (Nikolas Kofinas), ENOTTY (Joman Chu)}
@item{pair16: reflect9 (Yi Qian), HosseinT (Hossein Torkashvand)}
@item{pair17: ugur-koc (Ugur Koc)}
]

For each pair, a private repository has been created for you.  Use
this repository for your work.  If some other system is more useful
for collaborating, feel free to use it, but you must commit your work
to the repository by midnight of the due date for it to be graded.

The results of the language beauty contest are as follows:

@itemlist[
@item{C: 3}
@item{Python: 3}
@item{Ruby: 2}
@item{Agda: 1}
@item{C++: 1}
@item{Java: 1}
@item{JavaScript: 1}
@item{Perl: 1}
@item{Racket: 1}
]

I will commit marked up copies of your essay to your pair's repository
by Thursday, September 11, giving you a week to make edits for
@secref{PS2}.

@bold{Wed Aug 27 19:30:43 EDT 2014}

Welcome to CMSC 631, Program Analysis and Understanding.

I wanted to make you aware a few things before class starts:

@itemlist[#:style 'ordered
@item{The course web page is located at
@link["http://ter.ps/cmsc631f14"]{http://ter.ps/cmsc631f14}.  Please
read it this week.}

@item{There is @bold{no lecture} on 9/2 or 9/4---I have to travel for a
conference.  However, there is an assignment due on 9/4.}]

Looking forward to seeing you soon!


@bold{Mon Aug 25 15:03:22 EDT 2014}

Welcome to CMSC631!  This ``blog'' is where course announcements will be
made; be sure to check it regularly.  -- David


@;{

@bold{Fri May 16 17:05:23 EDT 2014}

Details of @secref{RP5} are up.

@bold{Tue May 13 15:26:28 EDT 2014}

Changes to end of semester plan: the deadline for the last homework is
extended to May 21, midnight; there will not a be a final.  Final
write-ups of your project are still due May 21 (more details will be
posted shortly).

@bold{Mon May 12 10:21:18 EDT 2014}

Javran asked: ``How can I "compose" two reduction relations together?
Say I have two reduction relations @code{-->_m^1} and @code{-->gc^1},
whose domain and codomain are all @code{ς}, I want to apply
@code{-->_m^1} and then @code{-->gc^1}.''

Here is small example that provides a solution to Javran's problem:

@codeblock{
#lang racket
(require redex/reduction-semantics)
(define-language L
  (n ::= number))

(define -->_1_2
  (reduction-relation
   L
   #:domain n
   [--> n_1 n_3
        (where (n_a ... n_2 n_b ...)
               ,(apply-reduction-relation -->_1 (term n_1)))
        (where (n_c ... n_3 n_d ...)
               ,(apply-reduction-relation -->_2 (term n_2)))]))

(define -->_1
  (reduction-relation
   L #:domain n
   [--> n ,(add1 (term n))]))

(define -->_2
  (reduction-relation
   L #:domain n
   [--> n ,(sqr (term n))]))
}

(You could even go a step further and write a function that consumes
two reduction relations and produces their sequential composition.)



@bold{Sat May 10 10:38:55 EDT 2014}

Fixed a small holdover bug in problem 2 of the revised practice exam
pointed out by Becca.

@bold{Thu May  8 19:19:55 EDT 2014}

A new practice exam is available @link["exam/exam.v2.pdf"]{here}.

@bold{Thu May  8 15:39:30 EDT 2014}

There is another problem with the exam.  Problem 4 asks you to prove
progress and preservation, but progress clearly does not hold.

Since there have been several problems with the practice exam, I will
provide a new practice exam later today.  (It will be a close variant
of the current practice exam.)

@bold{Thu May  8 09:16:15 EDT 2014}

There is a typo on the practice exam: where it says
@emph{heterogeneous} is should say @emph{homogeneous}.  The
@link["exam/exam.pdf"]{PDF} has been updated.  (Thanks to Phil for spotting.)

@bold{Wed May  7 17:41:45 EDT 2014}

Becca has made a bold conjecture:

@verbatim{
For problem 3 on the practice exam, I don't think it's possible to produce
an example of an ill-typed program.
}

Is she correct?  If she's correct, how could provide evidence
supporting her conjecture?

(By the way, it may be that she is correct, which would make problem 3
a ``trick question,'' which I promise not to give on the real exam.)

@bold{Wed May  7 14:23:48 EDT 2014}

Emily has pointed out a bug in the abstract machine code previously
posted on the blog, and part of the current assignment, in the code
for @code{-->_m*}.

You can trigger the bug by running @code{(viz* '(App (Lam x x) (Lam y
y)))}, which complains about no matching clauses for:
@codeblock{
(alloc (ev (App (Lam x x) (Lam y y)) () () Mt))
}

First, let's formulate a failing test case:
@codeblock{
(require rackunit)
(check-not-exn (λ () (ev* '(App (Lam x x) (Lam y y)))))
}

The issue, as indicated in the error message, is that we don't have a
case in @code{alloc} for the particular state that requested space
allocated.  If you look at @code{alloc}, you see that is a
metafunction for the language @code{L}.  But this is a bug in
@code{-->_m*}, i.e. an @code{L*} relation.  So it's not surprising
that we're missing a case; remember the step from @code{-->_m} to
@code{-->_m*} involved adding a new place where we allocate; when we
encounter an application, instead of pushing on the argument on the
stack, we allocate a pointer to the new stack frame.  This is the case
not considered in the original @code{alloc} function.

The fix is to introduce an allocation function suited for
@code{-->_m*}, which handles this case.  It can be an extension of the
original:
@codeblock{
(define-metafunction/extension alloc L*
  alloc* : ς -> a
  [(alloc* (ev (App e_0 e_1) ρ ([a ↦ _] ...) κ))
   ,(+ 1 (apply max -1
                (filter integer?
                        (term (a ...)))))])
}

After replacing calls to @code{alloc} with @code{alloc*} in
@code{-->_m*}, our test passes.

Note: I spotted two more errors in looking into this.  1) The original
@code{alloc} code has more cases than needed.  See if you can spot
why.  2) The definition of @code{ev^} uses
@code{apply-reduction-relation}, but it should use
@code{apply-reduction-relation*}.

@bold{Tue May  6 15:13:13 EDT 2014}

The practice exam is @link["exam/exam.pdf"]{here}.

@bold{Tue Apr 22 09:36:46 EDT 2014}

The final problem set (@secref{PS4}) is posted.  It is due at
midnight on the last day of class, which is plenty of time, but plan
ahead and work early since you have to balance delivering on your
research projects.

@bold{Thu Apr 17 14:35:05 EDT 2014}

The details of the project presentation (@secref{RP4}) are posted.

Here is the randomly assigned schedule for talks:

@itemlist[
@item{5/1: @gh{proj03}, @gh{proj07}, @gh{proj02}}
@item{5/6: @gh{proj10}, @gh{proj09}, @gh{proj04}}
@item{5/8: @gh{proj08}, @gh{proj05}, @gh{proj06}}
@item{5/13: @gh{proj01}}]


@bold{Thu Apr 10 17:40:10 EDT 2014}

Here is the code for the PCF model, the SPCF model, and a helper
substitution metafunction we saw in class today.

@codeblock|{
#lang racket
(provide v err-abort -->v δ δf PCF PCF-source)
(require redex/reduction-semantics
         "subst.rkt")
(require redex)

(define-language PCF-source
  ;; Types
  (T ::= nat (T ... -> T) _)
  ;; Terms
  (M ::= X V (M M ...) (μ (X : T) S) (if0 M M M) (err T string))
  ;; Values
  (V ::= N O (λ ([X : T] ...) M))
  ;; Simple terms
  (S ::= V X)
  ;; Naturals
  (N ::= natural)
  ;; Primitive operations
  (O ::= add1 sub1 * + quotient pos?)
  ;; Variables
  (X ::= variable-not-otherwise-mentioned)
  ;; Type environments
  (Γ ::= ((X T) ...)))

(define-extended-language PCF PCF-source
  ;; Labels
  (L ::= any)
  ;; Terms
  (M ::= .... (@ L M M ...) (err L T string))  
  ;; Evaluation contexts
  (E ::= hole (@ L V ... E M ...) (if0 E M M)))

(define v
  (reduction-relation
   PCF #:domain M
   (--> (@ L (λ ([X : T] ..._1) M) V ..._1)
   (subst (X V) ... M)
   β)
   (--> (μ (X : T) S)
        (subst (X (μ (X : T) S)) S)
        μ)
   (--> (@ L O V ...) M
   (judgment-holds (δ O L (V ...) M))
   δ)
   (--> (if0 0 M_0 M_1) M_0 if0-t)
   (--> (if0 N M_0 M_1) M_1
   (judgment-holds (nonzero? N))
   if0-f)))

(define-judgment-form PCF
  #:mode (nonzero? I)
  #:contract (nonzero? N)
  [(nonzero? N)
   (where (side-condition N (not (zero? (term N)))) N)])

(define err-abort
  (reduction-relation
   PCF #:domain M
   (--> (in-hole E (err L T string))
   (err L T string)
   (where #t (not-mt? E))
   err-abort)))

(define -->v
  (union-reduction-relations (context-closure v PCF E) err-abort))

(define-metafunction PCF
  not-mt? : E -> #t or #f
  [(not-mt? hole) #f]
  [(not-mt? E) #t])

(define-judgment-form PCF
  #:mode (δ I I I O)
  ;; Using this contract will make v non-reusable.
  ;#:contract (δ O (V ...) M)
  [(δ O L (N_0 ...) M)
   (where M (δf O L (N_0 ...)))])

(define-metafunction PCF
  δf : O L (V ...) -> M
  [(δf add1 L (N))           ,(add1 (term N))]
  [(δf sub1 L (N))           ,(max 0 (sub1 (term N)))]
  [(δf * L (N_0 N_1))        ,(* (term N_0) (term N_1))]
  [(δf + L (N_0 N_1))        ,(+ (term N_0) (term N_1))]
  [(δf pos? L (0))            1]
  [(δf pos? L (N))            0]
  [(δf quotient L (N_0 0))    (err L nat "Divide by zero")]
  [(δf quotient L (N_0 N_1)) ,(quotient (term N_0) (term N_1))])
}|

The SPCF model:

@codeblock|{
#lang racket
(provide s sv -->sv havoc δ^ SPCF)
(require redex/reduction-semantics
         "pcf.rkt")

(define-extended-language SPCF PCF
  ;; Values
  (V .... (• T)))

(define s
  (reduction-relation
   SPCF #:domain M
   (--> (@ L (• (T_0 ..._1 -> T)) V ..._1) (• T) β•) ;; Good stuff
   (--> (@ L (• (T_0 ..._1 T T_1 ... -> T_o))
           V_0 ..._1 V V_1 ...)
        (havoc T V)
        havoc)
   (--> (@ L O V ...) M
   (judgment-holds (δ^ O L (V ...) M))
   δ^)
   (--> (if0 (• nat) M_0 M_1) M_0 if•-t)
   (--> (if0 (• nat) M_0 M_1) M_1 if•-f)))

(define-metafunction SPCF
  havoc : T M -> M
  ;; (begin M (loop))
  [(havoc nat M) (@ 'Λ (λ ([x : nat]) (μ (x : nat) x)) M)]
  [(havoc (T_0 ... -> T_1) M)
   (havoc T_1 (@ 'Λ M (• T_0) ...))])

(define sv
  (union-reduction-relations s (extend-reduction-relation v SPCF)))

(define-metafunction SPCF
  not-zero? : any -> #t or #f
  [(not-zero? 0) #f]
  [(not-zero? any) #t])

(define-metafunction SPCF
  not-div? : any -> #t or #f
  [(not-div? div) #f]
  [(not-div? any) #t])

(define -->sv
  (union-reduction-relations (context-closure sv SPCF E)
                             (extend-reduction-relation err-abort SPCF)))

(define-judgment-form SPCF
  #:mode (δ^ I I I O)
  #:contract (δ^ O L (V ...) M)
  [(δ^ quotient L (any (• nat)) (• nat))]
  [(δ^ quotient L (any (• nat)) (err L nat "Divide by zero"))]
  [(δ^ quotient L ((• nat) 0)   (err L nat "Divide by zero"))]
  [(δ^ quotient L ((• nat) N)   (• nat))
   (side-condition (not-zero? N))]
  [(δ^ O L (any_0 ... (• nat) any_1 ...) (• nat))
   (side-condition (not-div? O))])


;; Example
#;
(require redex)
#;
(traces -->sv
          '(@ ME 
              (• ((nat -> nat) -> nat))
              (λ ([x : nat])
                (@ ME quotient 5 x))))
}|

The substitution function:

@codeblock|{
#lang racket
(provide subst)
(require redex/reduction-semantics)

;; Subst
(define-language L (X variable) (T any))
(define-metafunction L
  subst : (X any) ... any -> any
  [(subst (X_1 any_1) (X_2 any_2) ... any_3)
   (subst-1 X_1 any_1 (subst (X_2 any_2) ... any_3))]
  [(subst any_3) any_3])

(define-metafunction L
  subst-1 : X any any -> any
  ;; 1. X_1 bound, so don't continue in λ body
  [(subst-1 X_1 any_1 (λ ([X_2 : T_2] ... [X_1 : T_1] [X_3 : T_3] ...) any_2))
   (λ ([X_2 : T_2] ... [X_1 : T_1] [X_3 : T_3] ...) any_2)
   (side-condition (not (member (term X_1) (term (X_2 ...)))))]  
  [(subst-1 X any_1 (μ (X : T) any_2))
   (μ (X : T) any_2)]
  
  ;; 2. general purpose capture avoiding case
  [(subst-1 X_1 any_1 (λ ([X_2 : T_2] ...) any_2))
   (λ ([X_new : T_2] ...)
     (subst-1 X_1 any_1 (subst-vars (X_2 X_new) ... any_2)))
   (where (X_new ...)
     ,(variables-not-in (term (X_1 any_1 any_2))
     			           (term (X_2 ...))))]
  [(subst-1 X_1 any_1 (μ (X : T) any_2))
   (μ (X_new : T)
      (subst-1 X_1 any_1 (subst-vars (X X_new) any_2)))
   (where (X_new)
          ,(variables-not-in (term (X_1 any_1 any_2))
                             (term (X))))]
  
  ;; 3. replace X_1 with e_1
  [(subst-1 X_1 any_1 X_1) any_1]
  ;; 4. X_1 and X_2 are different, so don't replace
  [(subst-1 X_1 any_1 X_2) X_2]
  ;; the last cases cover all other expressions
  [(subst-1 X_1 any_1 (any_2 ...))
   ((subst-1 X_1 any_1 any_2) ...)]
  [(subst-1 X_1 any_1 any_2) any_2])

(define-metafunction L
  subst-vars : (X any) ... any -> any
  [(subst-vars (X_1 any_1) X_1) any_1]
  [(subst-vars (X_1 any_1) (any_2 ...))
   ((subst-vars (X_1 any_1) any_2) ...)]
  [(subst-vars (X_1 any_1) any_2) any_2]
  [(subst-vars (X_1 any_1) (X_2 any_2) ... any_3)
   (subst-vars (X_1 any_1) (subst-vars (X_2 any_2) ... any_3))]
  [(subst-vars any) any])

}|

@bold{Wed Apr  9 14:57:11 EDT 2014}

Here is the code for the ``abstract'' abstract machine we developed in
class, originally written by Phil Nguyen and rewritten by me.  I've
changed the code to more closely resemble the presentation I made on
the board, which is the presentation I now prefer.  I also
reformulated the 0CFA-like abstraction we inlined in class as an
@code{alloc^} function.  You can read the original work in
@link["http://dl.acm.org/citation.cfm?id=1995400"]{@emph{Communications
of the ACM} volume 54, issue 9}.

@codeblock{
#lang racket
(require redex)

(define-language L
  ;; Expressions
  [e x (App e e) (Lam x e)]
  [x variable-not-otherwise-mentioned]  
  [v (Clos x e ρ)]  
  ;; Finite functions
  [fin ([any ↦ any] ...)]  
  ;; Machine states
  [ς (ev e ρ σ κ)
     (co κ v σ)
     (ans v σ)]  
  ;; Environments
  [ρ ([x ↦ a] ...)]  
  ;; Stores
  [σ ([a ↦ s] ...)]    
  ;; Storables
  [s v]  
  ;; Continuations
  [κ Mt (AppL e ρ κ) (AppR v κ)]  
  ;; Addresses
  [(a b c) any])

;; Abstract machine in eval/apply form
(define -->_m
  (reduction-relation
   L
   #:domain ς   
   ;; Eval transitions
   [--> (ev x ρ σ κ) (co κ v σ)
        Var
        (where v (lookup σ (lookup ρ x)))]
   [--> (ev (App e_0 e_1) ρ σ κ) 
        (ev e_0 ρ σ (AppL e_1 ρ κ))
        AppL]
   [--> (ev (Lam x e) ρ σ κ)
        (co κ (Clos x e ρ) σ)
        Lam]
   ;; Continue transitions
   [--> (co Mt v σ) (ans v σ) Halt]
   [--> (co (AppL e ρ κ) v σ)
        (ev e ρ σ (AppR v κ))
        AppR]
   [--> (name ς (co (AppR (Clos x e ρ) κ) v σ))
        (ev e (ext ρ x a) (ext σ a v) κ)
        β
        (where a (alloc ς))]))

(define-metafunction L
  inj : e -> ς
  [(inj e) (ev e () () Mt)])

(define (ev e)
  (apply-reduction-relation* -->_m (term (inj ,e))))

;; Visualize concete machine
(define (viz e)
  (traces -->_m (term (inj ,e))))

(define-metafunction L
  lookup : fin any -> any
  [(lookup (_ ... [any_k ↦ any_v] _ ...) any_k) any_v])

(define-metafunction L
  ext : fin any any -> fin
  [(ext (any ...) any_k any_v) (any ... [any_k ↦ any_v])])

(define-metafunction L
  alloc : ς -> a
  [(alloc (ev x ρ ([a ↦ _] ...) κ))
   ,(+ 1 (apply max -1 
                (filter integer?
                        (term (a ...)))))]
  [(alloc (co κ v ([a ↦ _] ...)))
   ,(+ 1 (apply max -1 
                (filter integer?
                        (term (a ...)))))])

(define-extended-language L* L
  [κ Mt (AppL e ρ a) (AppR v a)]
  [s .... κ])

;; Abstract machine in eval/apply form
;; with heap-allocated stack frames
(define -->_m*
  (reduction-relation
   L*
   #:domain ς    
   ;; Eval transitions
   [--> (ev x ρ σ κ) (co κ v σ)
        Var
        (where v (lookup σ (lookup ρ x)))]
   [--> (name ς (ev (App e_0 e_1) ρ σ κ))
        (ev e_0 ρ (ext σ a κ) (AppL e_1 ρ a))
        (where a (alloc ς))
        AppL]
   [--> (ev (Lam x e) ρ σ κ)
        (co κ (Clos x e ρ) σ)
        Lam]
   ;; Continue transitions
   [--> (co Mt v σ) (ans v σ) Halt]
   [--> (co (AppL e ρ a) v σ)
        (ev e ρ σ (AppR v a))
        AppR]
   [--> (co (AppR (Clos x e ρ) b) v σ)
        (ev e (ext ρ x a) (ext σ a v) κ)
        β
        (where a (alloc ς))
        (where κ (lookup σ b))]))


(define (ev* e)
  (apply-reduction-relation* -->_m* (term (inj ,e))))

;; Visualize machine with stack allocated
(define (viz* e)
  (traces -->_m* (term (inj ,e))))

(define-extended-language L^ L*
  [σ ([a ↦ (s ...)] ...)])

(define-metafunction L^
  ⊔ : σ a s -> σ
  [(⊔ (name σ (any_1 ... [a ↦ (_ ... s_i _ ...)] any_2 ...)) a s_i)
   σ]
  [(⊔ (any_1 ... [a ↦ (s ...)] any_2 ...) a s_i)
   (any_1 ... [a ↦ (s ... s_i)] any_2 ...)]
  [(⊔ (any ...) a s) (any ... [a ↦ {s}])])


;; Swap these out for different abstractions
#;
;; constant allocation
(define-metafunction L^
  alloc^ : ς -> a
  [(alloc^ _) 0])

;; 0CFA-like abstraction
(define-metafunction L^
  alloc^ : ς -> a
  [(alloc^ (ev e ρ σ κ)) e]
  [(alloc^ (co (AppR (Clos x e ρ) b) v σ)) x])
  

;; Approximating Abstract machine in eval/apply form
;; with heap-allocated stack frames
(define -->_m^
  (reduction-relation
   L^
   #:domain ς    
   ;; Eval transitions
   [--> (ev x ρ σ κ) (co κ v σ)
        Var
        (where (_ ... v _ ...) (lookup σ (lookup ρ x)))]
   [--> (name ς (ev (App e_0 e_1) ρ σ κ))
        (ev e_0 ρ (⊔ σ a κ) (AppL e_1 ρ a))
        (where a (alloc^ ς))
        AppL]
   [--> (ev (Lam x e) ρ σ κ)
        (co κ (Clos x e ρ) σ)
        Lam]
   ;; Continue transitions
   [--> (co Mt v σ) (ans v σ) Halt]
   [--> (co (AppL e ρ a) v σ)
        (ev e ρ σ (AppR v a))
        AppR]
   [--> (name ς (co (AppR (Clos x e ρ) b) v σ))
        (ev e (ext ρ x a) (⊔ σ a v) κ)
        β
        (where a (alloc^ ς))
        (where (_ ... κ _ ...) (lookup σ b))]))

(define (ev^ e)
  (apply-reduction-relation -->_m^ (term (inj ,e))))

;; visualize approximating machine
(define (viz^ e)
  (traces -->_m^ (term (inj ,e))))


;; Some examples
; (viz^ '(App (Lam x x) (Lam y y)))
; (viz^ '(App (Lam x (App x x)) (Lam y (App y y))))
; (viz^ '(App (Lam f (App (App f f) (Lam x x))) (Lam y y)))
}

@bold{Fri Mar 28 09:20:51 EDT 2014}

There's an excellent article on scientific writing that's been
republished in Scientific American:
@link["https://www.americanscientist.org/issues/issue.aspx?id=877&y=0&no&content=true&page=4&css=print"]{The
Science of Scientific Writing}.  Please read it and try to apply it in
your own writing.


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
}