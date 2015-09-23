#lang scribble/manual

@(define (gh p) (link (format "https://github.com/cmsc631/~a" p) (format "~a" p)))

@title{Blog}

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
