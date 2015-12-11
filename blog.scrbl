#lang scribble/manual

@(define (gh p) (link (format "https://github.com/cmsc631/~a" p) (format "~a" p)))

@title{Blog}

@bold{Fri Dec 11 11:18:08 EST 2015}

We will not NOT meet during the final exam period.  Focus on the final
problem set and research report.

@bold{Tue Dec  8 14:40:28 EST 2015}

Here are the abstracts for today's presentations.

@itemlist[

@item{Run-time static type checking, Consroe and Kazerounia

Type systems have historically fallen into two disjoint
classifications: static and dynamic. Static typing requires total
knowledge of a program in order to verify the program will not get
stuck when running. The knowledge gained from a static type system is
useful for language implementations as well as for the author of a
program. A dynamically typed program on the other hand starts
executing with no prior checks. This approach affords a programmer
many more sound programs which would otherwise produce a static type
error.

We seek to unify the power of each. Our approach will allow a program
to begin running as a dynamically typed program with the ability to
define new types at run-time. A program can then perform a static type
check of a subsection of the program. Furthermore, we provide the user
with the ability recover in the event that the “static section” of
their code should fail to type check. This approach will provide the
programmer with the flexibility of a dynamic type system and the
guarantees of a static type system.}

@item{Faceted Values and Dynamic Information Flow Control,
Xirogiannopoulos and Bengfort

It has become increasingly popular to create rich web applications
through the plug-and-play integration of web services by embedding
publisher specific JavaScript code into an application's native code
base. Typically, a plug-and-play workflow relies on the inclusion of
snippets of third party code in the client application, customized
with confidential API keys or site-specifc information. Modern
browsers provide techniques like sandboxing and cross-domain exclusion
attempt to protect confidential information from cross site scripting
(XSS) and injection attacks. However, once third party code is
embedded, it is treated as a native part of the code base and is
executed with the same privileges. As a result, this "outsider code"
is a vector for malicious behavior and the leakage of confidential
information through a variety of channels within the code itself.

The Information Flow Control of programs is a systematic approach to
understanding how information (data) stored in a program's variables,
gets propagated within the code itself. Information Flow Control
methods detect the way data is passed by specifying a security level
for each variable and provide monitoring to prevent the propagation
(flow) of sensitive information to public observers. In this talk, we
will introduce the basic concepts and primary contributions of
Information Flow Control and enumerate many of the existing techniques
proposed by the academic community. We will then turn to focus on a
specific technique: dynamic information flow control using faceted
values, developed by Austin and Flanagan.
}

@item{Debugging the Debugger: Assessing the usability of debugging
tools for novice programmers, Malu and Plane

Jakob Nielsen’s heuristics have been widely used and have become a
benchmark for evaluating any user interface. One of his classic
usability heuristics is “Visibility of System Status,” which means
giving relevant feedback as and when needed to keep the user informed
about the system. For novice programmers, coding and debugging can get
intimidating as they cannot see what’s happening behind the scenes. To
address this problem there has been some recent work to make
programming languages visual. However, for debugging the techniques
and tools still remain old. From a 2002 survey, it was found that
software bugs cost companies ~60 billion USD annually. Hence the need
for designing better debugging tools to easily understand and identify
errors and remove them thus leading to better productivity of
programmers and also reducing the maintenance cost is needed. But the
process of debugging can be particularly challenging for novice
programmers who are just starting out and may need training and
learning to understand what those errors mean and how can they be
removed. In our work, we explore the Eclipse’s in-built debugger to
see how novice programmers approach problem solving.

We combine Jakob Nielsen’s 10 heuristics and Sadowski et. al.’s heuristics to evaluate programming languages and create our new set of 10 heuristics for our context. We then conduct a user study with 10 novice programmers by giving them 3 tasks to evaluate Eclipse’s in-built debugger. Participants use this new set of heuristics to evaluate the tool and talk about their experience in a semi-structured interview. Because of the exploratory nature of this project, we use a grounded theory approach to understand problems, patterns and trends in debugging approach for novice programmers. We hypothesize that existing tools like the Eclipse’s in-built debugger is not intuitive for first time programmers and we expect results from our work will help in providing guidelines to build better tools that would not only help the productivity of programmers but also reduce the cost associated with it.
}

@item{Hacking Big Data and Machine Learning: An Analysis of Integrity
and Influence, Stevens and Suciu

Governments and businesses thrive on the output of data analytics for
improving their competitive edge in areas such as consumer
satisfaction, threat intelligence, decision making, and product
efficiency. These entities inherently trust the output of their data
processing algorithms with the belief that the results represent an
extrapolated trend from a massive selection of unnormalized
input. Public information is often deemed safe for ingestion into data
stores because of this core belief. This insecure practice introduces
a new vector of attack for miscreants in which they now control an
arbitrary subset of the input for machine learning algorithms. This
research will introduce a survey of novel attacks in which an
adversary can influence or control the outcome of machine learning and
data analytics algorithms given an arbitrarily small input. In an
effort to counter these attacks, we will propose approaches to defend
against our novel attacks.

}


]

Here are the abstracts for Thursday:

@itemlist[

@item{Survey and Model of "Static Analysis of String Manipulations in Critical Embedded C Programs", Farooqui and Lakshmanaswamy


Programming in C with strings, and more generally with buffers is a
source of major vulnerabilities, for the reasons that the compiler
doesn’t inherently provide a protection against writing to the
unallocated regions of memory and the dynamic errors can elude
detection from developers and testers. The motivation of this research
project is to survey and implement a static analysis algorithm of
string manipulation in program source code. In this regard, the scope
of the C language is confined only to the syntax related to performing
the string manipulation and doesn’t cover allocation of memory
dynamically. The analysis is based on the theory of abstract
interpretation and relies on the abstraction of stores that retain the
length of string buffers.

We implemented the paper in two folds. In the first approach, we used
C Intermediate Language (CIL) for the abstraction of the set of stores
and performed value analysis (using a plugin of an open source tool
called Frama-C) to obtain the abstract values stored. Based on these
values, we determine whether the destination has enough space to hold
the contents of the source, and accordingly emit the warning of the
string buffer flow. In the second approach, we implemented the static
analysis for the presence of the string overflows without relying on
the CIL and value analysis. For the small subset of C language which
deals with character arrays and string copy, we parse the source code
to detect the expression initializing the character arrays and
character pointers. The value analysis is performed by keeping a track
of the variables and the size allocated and in the case where the
destination string cannot hold the source string; we raise the string
buffer overflow exception. We tested both the approaches with some
example source code and achieved promising results.}

@item{Symbolic Execution for Cryptocurrencies, Sweet and Robinson


For our project, we wrote a symbolic executor for Ethereum Virtual
Machine (EVM) bytecode. Ethereum is a cryptocurrency platform with a
quasi-Turing complete programming language that leverages the
blockchain-paradigm popularized by Bitcoin. In addition to a permanent
record of all transactions, the blockchain also contains smart
contracts, which are user-written computer protocols to enforce
agreements. Smart contracts are intended to eliminate the need for a
trusted third party by allowing code to act as a truly independent 3rd
party.

However, it is difficult to verify that smart contracts in Ethereum do
what they claim to do because they are included on the blockchain in
the form of EVM bytecode. Even honest contract writers may include
exploitable errors in their smart contracts. Unlike other computer
systems whose bugs indirectly cost money, ethereum smart contracts can
directly transfer large amounts of money irrevocably making bugs even
more costly. To provide a level of verification for smart contracts,
we have developed a symbolic executor for EVM bytecode that can detect
the existence of three common bugs in Ethereum smart contracts, the
"blockhash bug", the "callstack bug" and the “out of gas bug”.

}

@item{Survey and Model: Logical Types for Untyped Languages, Li

My project is to do a survey on the paper Logical types for untyped
languages by Sam Tobin-Hochstadt and Matthias Felleisen in ICFP
10. The paper is about the improvement of the type system in
Racket. The problem of the old Racket type system is not being able to
typecheck an expression effectively when there is a combination of
logic predicates in the expression. For example, in (if (or (number?
x) (string? x)) (f x) 0), there is an if and an or in the expression,
so the information obtained from the test propositions should be
passed on to the if expression in the typecheck. To propagate this
information, the paper proposed a proposition environment. All the
information from the tests can be formulated as another language based
on the propositions, and the typecheck is done by the reduction on
this proposition environment language, which is the most part of the
paper. The authors described their method as a new framework of
occurrence typing. In my talk I will introduce this method by
presenting a number of examples.

}

@item{Mechanized Gradual Typing by Abstract Interpretation, Darais


Gradual type systems have been suggested and recently popularized in
an attempt to bridge the gap between dynamically and statically typed
programming languages. The effort is motivated by a desire to overcome
the _limitations_ of both paradigms: statically typed languages
restrict programmer expression, and dynamically typed languages make
no promises about program execution. On the other hand, gradual type
systems are simultaneously motivated by combining the _benefits_ of
both paradigms: uninhibited programmer expression combined with
optional lightweight program verification.

Although gradual type systems have seen great success, both for
researchers and practitioners in industry, advancement of the field is
crippled by the added complexity that comes with designing a new
gradually typed programming language. The design of gradual languages
has only managed to catch up with the state of the art in type systems
developed circa 1980.

To guide the design of new gradual type systems, a unifying framework
has been recently proposed called Abstracting Gradual Types
(AGT). Using AGT, both static and dynamic semantics of gradual type
systems are derived systematically through abstract interpretation of
a programming language with precise types.

In this presentation I will discuss _formal verification_ of gradual
type systems derived in AGT style. This poses a number of unique
challenges, and the key to overcoming them lies in the recently
developed framework of Constructive Galois connections: a variant of
classical Galois connections which carry computational content and are
amenable to formal verification.}

@item{Dynamic Type Systems, Litton

In any given programming language, the type system confers upon the
user a means to impose constraints on the source code in the hope of
providing certain guarantees on the output. A programming language
that disallows dereferencing an integer through a typecast, for
example, ameliorates errors and makes the code easier to reason about
and optimize. Programmers typically choose a language where the
constraints and guarantees provide a good trade-off for their
domain. Unfortunately, the domain is not fixed for the lifetime of the
program, and developers must live with the lowest common denominator
of available type systems.

C, for instance, allows developers to dereference an integer as a
pointer to some other type. This is a necessity in certain layers of
systems architecture, but is otherwise a hazard. Newer languages, such
as Rust, seek to accomodate this disparity by allowing developers to
drop into "unsafe" sections of code where the type system is
relaxed. We have developed a simple system to explore generalizing
this idea to user defined and runtime modifiable type systems embedded
within a larger program. In our talk we will present our results, both
in terms of the limits of the generalization, its applications, and
our implementation.}

]


@bold{Tue Dec  8 10:54:19 EST 2015}

Please be sure to fill out the course evaluation,
@url{https://www.CourseEvalUM.umd.edu}.

@bold{Tue Dec  1 15:16:19 EST 2015}

I've posted the schedule for final presentations on the syllabus.

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
