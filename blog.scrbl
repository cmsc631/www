#lang scribble/manual

@(define (gh p) (link (format "https://github.com/cmsc631/~a" p) (format "~a" p)))

@title{Blog}

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

key: cs = cs.umd.edu, g = gmail.com, 
     t = terpmail.umd.edu, u = umd.edu,
     b = bengfort.com
}|

Missing: Ma, Xinyuan; Segev, Yoav; Sekniqi, Kevin; Shivapuram Lakshmanaswamy.

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
